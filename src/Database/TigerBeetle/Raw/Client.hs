{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Database.TigerBeetle.Raw.Client
  ( module Database.TigerBeetle.Raw.Client
    -- * Types
  , ClientInitError (..)
  , FFI.makeCompletionCallback
  )
where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, putTMVar, takeTMVar)
import Control.Concurrent.STM.TQueue (TQueue, tryReadTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVar, writeTVar)
import Control.Exception (assert)
import Control.Monad (forM_, void, when)
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor (($>))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (isJust)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word
import Database.TigerBeetle.Address
import Database.TigerBeetle.ClusterId (ClusterId)
import Database.TigerBeetle.Internal.FFI.Client
  ( TBClient
  , TBClientStatus (..)
  , TBCompletionCallback
  , TBCompletionContext
  , TBInitStatus
  , TBOperation (..)
  , TBPacket (..)
  , TBPacketStatus (..)
  )
import Database.TigerBeetle.Internal.FFI.Client qualified as FFI
import Database.TigerBeetle.Raw.Response (DecodeResponseError, TBResponse, decodeResponse)
import Foreign (Storable (..))
import Foreign.C.Types (CChar)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import GHC.Natural (Natural)
import System.Timeout (timeout)

-- | Whether to start an echo server or a standard server
data ClientKind = Echo | Standard
  deriving (Eq, Ord, Show)

data RequestError
  = ClientError TBClientStatus
  | PacketError TBPacketStatus
  | PacketDataParseError DecodeResponseError
  | ClientShutdownDuringRequest
  | RequestTimeoutError TBOperation ByteString
  deriving (Eq, Show)

-- | Context for a single request
data RequestContext = RequestContext
  { contextId :: Word64
  -- ^ Unique identifier for this request
  , resultVar :: TMVar (Either RequestError TBResponse)
  -- ^ Where to put the result
  }

-- | State maintained for the client
data ClientState = ClientState
  { csActiveRequests :: TVar (IntMap RequestContext)
  , csNextRequestId :: TVar Word64
  , csFreeRequestIds :: TQueue Word64 -- Pool of reusable IDs
  , csIsShutdown :: TVar Bool
  , csTimeoutMillis :: Natural -- Timeout in milliseconds
  }

newtype ClientHandle = ClientHandle {tvar :: TVar ClientState}

withAddressPtr :: Address -> ((Ptr CChar, Int) -> IO a) -> IO a
withAddressPtr = BS.useAsCStringLen . TE.encodeUtf8 . getAddress

data ClientInitError
  = Unexpected
  | OutOfMemory
  | AddressInvalid
  | AddressLimitExceeded
  | SystemResources
  | NetworkSubsystem
  deriving (Eq, Show)

-- | Create a 'Client' initialization error from a 'TBInitStatus'.
--
-- Asserts that @err@ is not 'FFI.Success', throws an exception at
-- runtime.
toClientInitError :: TBInitStatus -> ClientInitError
toClientInitError err = assert (err /= FFI.Success) $
  case err of
    FFI.Unexpected -> Unexpected
    FFI.OutOfMemory -> OutOfMemory
    FFI.AddressInvalid -> AddressInvalid
    FFI.AddressLimitExceeded -> AddressLimitExceeded
    FFI.SystemResources -> SystemResources
    FFI.NetworkSubsystem -> NetworkSubsystem
    FFI.Success -> error "toClientInitError: Success is not an error"

type ClientPtr = ForeignPtr FFI.TBClient

-- TODO: Add user function as finalizer parameter since we can run
-- arbitrary IO actions here.
clientFinalizer :: Ptr TBClient -> IO ()
clientFinalizer clientPtr = do
  result <- FFI.tbClientDeinit clientPtr
  case result of
    FFI.ClientOk -> pure ()
    FFI.ClientInvalid ->
      error $ "tbClientFinalizer (invalid clientPtr): " ++ show clientPtr

initClientPtr :: IO ClientPtr
initClientPtr = do
  clientPtr <- mallocForeignPtr
  pure clientPtr

deinitClient :: ClientPtr -> IO TBClientStatus
deinitClient clientPtr = do
  withForeignPtr clientPtr $ \rawPtr -> do
    FFI.tbClientDeinit rawPtr

validateClientInit :: ClientPtr -> TBInitStatus -> IO (Either ClientInitError ClientPtr)
validateClientInit clientPtr = \case
  FFI.Success -> pure $ Right clientPtr
  initError -> pure . Left . toClientInitError $ initError

-- | Call @tb_client_init_echo@ and return a valid 'Client' upon success.
--
-- Used to connect to the @libtb_client@ library and test the FFI.
--
-- The finalizer on 'Client' will call @tb_client_deinit@.
initClientEcho
  :: ClusterId
  -> Address
  -> TBCompletionContext
  -> FunPtr TBCompletionCallback
  -> IO (Either ClientInitError ClientPtr)
initClientEcho clusterId address completionCtx completionCallback = do
  clientPtr <- initClientPtr
  initStatus <- withForeignPtr clientPtr $ \cp -> do
    withAddressPtr address $ \(addrPtr, addrLen) -> do
      FFI.tbClientInitEcho
        cp
        clusterId
        addrPtr
        (fromIntegral addrLen)
        completionCtx
        completionCallback
  validateClientInit clientPtr initStatus

initCallback :: TBCompletionCallback -> IO (FunPtr TBCompletionCallback)
initCallback = FFI.makeCompletionCallback

-- | Call @tb_client_init@ and return a valid 'Client' upon success.
--
-- The finalizer on 'Client' will call @tb_client_deinit@.
initClient
  :: ClusterId
  -> Address
  -> TBCompletionContext
  -> FunPtr TBCompletionCallback
  -> IO (Either ClientInitError ClientPtr)
initClient clusterId address completionCtx completionCallback = do
  clientPtr <- initClientPtr
  initStatus <- withForeignPtr clientPtr $ \cp -> do
    withAddressPtr address $ \(addrPtr, addrLen) -> do
      FFI.tbClientInit
        cp
        clusterId
        addrPtr
        (fromIntegral addrLen)
        completionCtx
        completionCallback
  validateClientInit clientPtr initStatus

-- | Initializes the completion callback
setupCompletionCallback :: ClientState -> TBCompletionCallback
setupCompletionCallback state = \ctx packetPtr _timestamp resultPtr resultLen -> do
  -- Extract the packet information
  packet <- peek packetPtr

  -- Convert the uintptr_t context back to our RequestContext ID
  let requestIdW64 :: Word64 = fromIntegral ctx
      requestIdInt :: Int = fromIntegral ctx

  -- Look up the request context and recycle the ID
  mContext <- atomically $ do
    reqs <- readTVar state.csActiveRequests
    let mCtx = IM.lookup requestIdInt reqs
    when (isJust mCtx) $ do
      modifyTVar' state.csActiveRequests (IM.delete requestIdInt)
      -- Return the ID to the free list for recycling
      writeTQueue state.csFreeRequestIds requestIdW64
    return mCtx

  -- Process the result
  case mContext of
    Just context -> do
      result <-
        if resultPtr == nullPtr
          then pure . Left . PacketError $ packet.tbPacketStatus
          else do
            -- Convert the C result to a Haskell value
            bytes <- BS.packCStringLen (castPtr resultPtr, fromIntegral resultLen)
            pure . first PacketDataParseError $
              decodeResponse (BS.fromStrict bytes) packet.tbPacketOperation

      -- Deliver the result
      atomically $ putTMVar context.resultVar result
    Nothing ->
      -- TODO: Come up with a better way to log this
      putStrLn "Warning: Received callback for unknown request context"

finalizeClient :: ClientPtr -> ClientState -> IO ()
finalizeClient clientPtr state = do
  -- Signal that no new incoming request should proceed
  atomically $ writeTVar state.csIsShutdown True

  -- N.B. race-condition: Once we release from this transaction
  -- completionCallbacks may fire and overwrite our shutdown error in the
  -- request resultVar. Conceivably this is fine, the below block just ensures
  -- that all submitRequest invocations that are blocked on the request context
  -- resultVar are allowed to proceed. This should effectively flush all active
  -- requests since the isShutdown flag should prevent new requests from coming
  -- in.
  --
  -- Process interleaving shouldn't cause any active request entries after
  -- this block (due to using a stale isShutdown value) because the flag is
  -- read in the same atomically block as active requests are written to in
  -- submitRequest function
  atomically $ do
    activeReqs <- readTVar state.csActiveRequests
    -- Iterate through all the request vars and put a client shutdown result
    -- so that instances of `submitRequest` that are waiting are unblocked
    forM_ (IM.elems activeReqs) $ \context ->
      putTMVar context.resultVar (Left ClientShutdownDuringRequest)

  -- De-initialize the client
  withForeignPtr clientPtr $ \rawClient ->
    void $ FFI.tbClientDeinit rawClient

submitRequest
  :: ClientPtr
  -> ClientState
  -> TBOperation
  -> ByteString
  -- ^ Request data
  -> IO (Either RequestError TBResponse)
submitRequest clientPtr state operation reqData = do
  -- Scaffold request state
  res <-
    atomically $
      readTVar state.csIsShutdown >>= \case
        True -> pure (Left ClientShutdownDuringRequest)
        False -> Right <$> provisionRequestContext state

  case res of
    Left e -> pure $ Left e
    Right context -> withPacketPtrs reqData \packetPtr contextPtr (dataPtr, dataSize) -> do
      -- TODO: confirm that this is the correct way to assign a context id to a packet
      poke contextPtr context.contextId
      let packet =
            TBPacket
              { tbPacketUserData = castPtr contextPtr
              , tbPacketData = castPtr dataPtr
              , tbPacketDataSize = fromIntegral dataSize
              , tbPacketUserTag = 0
              , tbPacketOperation = operation
              , tbPacketStatus = Ok
              , tbPacketOpaque = V.empty
              }
      poke packetPtr packet

      -- Submit the request
      submitStatus <- withForeignPtr clientPtr $ \rawClient ->
        FFI.tbClientSubmit rawClient packetPtr

      case submitStatus of
        ClientOk -> do
          -- Wait for the result with a timeout
          let timeoutMicros = fromIntegral state.csTimeoutMillis * 1000 -- Convert ms to μs
          result <- timeout timeoutMicros $ atomically $ takeTMVar context.resultVar
          case result of
            Just r -> pure r
            Nothing -> cleanupRequest state context $> Left (RequestTimeoutError operation reqData)
        errorStatus -> cleanupRequest state context $> Left (ClientError errorStatus)
 where
  cleanupRequest :: ClientState -> RequestContext -> IO ()
  cleanupRequest s ctx = atomically $ do
    -- Remove from active requests
    modifyTVar' s.csActiveRequests $ IM.delete (fromIntegral ctx.contextId)
    -- Recycle the ID
    writeTQueue s.csFreeRequestIds (fromIntegral ctx.contextId)

  withPacketPtrs :: ByteString -> (Ptr TBPacket -> Ptr Word64 -> (Ptr CChar, Int) -> IO a) -> IO a
  withPacketPtrs bytes action =
    alloca \packetPtr ->
      alloca (BS.useAsCStringLen bytes . action packetPtr)

  provisionRequestContext :: ClientState -> STM RequestContext
  provisionRequestContext s = do
    -- Try to reuse an ID from the free list first
    mFreeId <- tryReadTQueue s.csFreeRequestIds
    reqId <- case mFreeId of
      Just freeId -> pure freeId
      Nothing -> do
        -- No free IDs, allocate a new one
        curId <- readTVar state.csNextRequestId
        writeTVar s.csNextRequestId (curId + 1)
        return curId

    -- Create context for this request
    resVar <- newEmptyTMVar
    let context = RequestContext reqId resVar

    -- Register the request
    modifyTVar' s.csActiveRequests $ IM.insert (fromIntegral reqId) context
    pure context

clientCallBack
  :: TBCompletionContext
  -> Ptr TBPacket
  -> Word64
  -> Ptr Word8
  -> Word32
  -> IO ()
clientCallBack _ _ timestamp _ _ = do
  putStrLn $ "clientCallBack: " ++ show timestamp
