{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Database.TigerBeetle.Raw.Client where

import Database.TigerBeetle.Internal.FFI.Client
import Database.TigerBeetle.Internal.FFI.Client.ClusterId (ClusterId)
import Data.Text (Text)
import Foreign.Marshal.Alloc (alloca)
import Foreign (Storable (..))
import Control.Exception (finally)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import GHC.Natural (Natural)
import Data.Word
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, newEmptyTMVar, takeTMVar)
import Control.Concurrent.STM.TVar (TVar, readTVar, modifyTVar', newTVarIO, writeTVar, stateTVar)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue, newTQueueIO, tryReadTQueue)
import Control.Concurrent.STM (readTVarIO, atomically)
import Data.Maybe (isJust)
import Control.Monad (when, void, forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Database.TigerBeetle.Raw.Response (TBResponse, decodeResponse, DecodeResponseError)
import Data.Bifunctor
import Data.Text.Encoding qualified as TE
import qualified Data.Vector as V
import Data.Traversable (for)
import System.Timeout (timeout)

-- | Whether to start an echo server or a standard server
data ClientKind = Echo | Standard
  deriving (Eq, Ord, Show)

-- | Configuration for creating a TigerBeetle client
data ClientConfig = ClientConfig
  { clientKind :: ClientKind  -- ^ Normal or Echo client
  , clientTimeoutMillis :: Natural  -- ^ Operation timeout in milliseconds
  }

-- | Default client configuration
defaultConfig :: ClientConfig
defaultConfig = ClientConfig 
  { clientKind = Standard
  , clientTimeoutMillis = 5000  -- 5 seconds default timeout
  }

data RequestError = 
    ClientError TBClientStatus
  | PacketError TBPacketStatus
  | PacketDataParseError DecodeResponseError
  | ClientShutdownDuringRequest
  | RequestTimeoutError TBOperation ByteString
  deriving (Eq, Show)

-- | Context for a single request
data RequestContext = RequestContext
  { contextId :: Word64  -- ^ Unique identifier for this request
  , resultVar :: TMVar (Either RequestError TBResponse)  -- ^ Where to put the result
  }

-- | State maintained for the client
data ClientState = ClientState
  { csClientPtr :: Ptr TBClient
  , csActiveRequests :: TVar (IntMap RequestContext)
  , csNextRequestId :: TVar Word64
  , csFreeRequestIds :: TQueue Word64  -- Pool of reusable IDs
  , csIsShutdown :: TVar Bool
  , csTimeoutMillis :: Natural  -- Timeout in milliseconds
  }

newtype ClientHandle = ClientHandle { tvar :: TVar ClientState }

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
        result <- if resultPtr == nullPtr
                  then pure . Left . PacketError $ packet.tbPacketStatus
                  else do
                    -- Convert the C result to a Haskell value
                    bytes <- BS.packCStringLen (castPtr resultPtr, fromIntegral resultLen)
                    pure . first PacketDataParseError
                         $ decodeResponse (BS.fromStrict bytes) packet.tbPacketOperation 
        
        -- Deliver the result
        atomically $ putTMVar context.resultVar result
        
      Nothing ->
        -- TODO: Come up with a better way to log this
        putStrLn "Warning: Received callback for unknown request context"

withClient
  :: ClientConfig
  -> ClusterId
  -> Text
  -> (ClientState -> IO a)
  -> IO (Either TBInitStatus a)
withClient cfg clusterId address action = 
  alloca $ \clientPtr -> do
    clientState <- ClientState clientPtr
      <$> newTVarIO IM.empty
      <*> newTVarIO 1
      <*> newTQueueIO
      <*> newTVarIO False
      <*> pure cfg.clientTimeoutMillis

    -- Initialize the completion callback
    callback <- makeCompletionCallback $ setupCompletionCallback clientState


    BS.useAsCStringLen (TE.encodeUtf8 address) $ \(addressPtr, addressLen) -> do
      let initFn = case cfg.clientKind of
                     Standard -> tbClientInit
                     Echo -> tbClientInitEcho

      -- Initialize the client
      initStatus <- initFn
        clientPtr
        clusterId
        addressPtr 
        (fromIntegral addressLen)
        0
        callback

      case initStatus of
        Success -> finally
            (Right <$> action clientState)
            (finalizeClient clientState)
        _ -> pure $ Left initStatus

finalizeClient :: ClientState -> IO ()
finalizeClient state = do
  atomically $ do
    -- Signal that no new incoming request should proceed
    writeTVar state.csIsShutdown True
    -- Eliminate the active requests feed so that the completion callback
    -- stops processing responses
    activeReqs <- stateTVar state.csActiveRequests (, IM.empty)
    -- Iterate through all the request vars and put a client shutdown result 
    -- so that instances of `submitRequest` that are waiting are unblocked
    forM_ (IM.elems activeReqs) $ \context ->
      putTMVar context.resultVar (Left ClientShutdownDuringRequest)
  
  -- Deinitialize the client
  void $ tbClientDeinit state.csClientPtr
  
  
  -- Fail any pending requests

submitRequest 
  :: ClientState
  -> TBOperation
  -> ByteString  -- ^ Request data
  -> IO (Either RequestError TBResponse)
submitRequest state operation reqData = do
  -- Scaffold request state
  res <- atomically $ readTVar state.csIsShutdown >>= \case
    True -> pure (Left ClientShutdownDuringRequest)
    False -> do
      -- Try to reuse an ID from the free list first
      mFreeId <- tryReadTQueue state.csFreeRequestIds
      reqId <- case mFreeId of
        Just freeId -> pure freeId
        Nothing -> do
          -- No free IDs, allocate a new one
          curId <- readTVar state.csNextRequestId
          writeTVar state.csNextRequestId (curId + 1)
          return curId

      -- Create context for this request
      resVar <- newEmptyTMVar
      let context = RequestContext reqId resVar
  
      -- Register the request
      modifyTVar' state.csActiveRequests $ IM.insert (fromIntegral reqId) context
      pure $ Right context

  case res of
    Left e -> pure $ Left e 
    Right context -> do
      -- Allocate and initialize the packet
      alloca \packetPtr ->
        alloca \contextPtr -> do
          -- TODO: confirm that this is the correct way to assign a context id to a packet
          poke contextPtr context.contextId
          -- Fill in the packet
          BS.useAsCStringLen reqData $ \(dataPtr, dataSize) -> do
            let packet = TBPacket
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
            submitStatus <- tbClientSubmit state.csClientPtr packetPtr
      
            case submitStatus of
              ClientOk -> do
                -- Wait for the result with a timeout
                let timeoutMicros = fromIntegral state.csTimeoutMillis * 1000  -- Convert ms to μs
                result <- timeout timeoutMicros $ atomically $ takeTMVar context.resultVar            
                case result of
                  Just r -> pure r
                  Nothing -> atomically $ do
                      -- Remove from active requests
                      modifyTVar' state.csActiveRequests $ IM.delete (fromIntegral context.contextId)
                      -- Recycle the ID
                      writeTQueue state.csFreeRequestIds (fromIntegral context.contextId)

                      pure . Left $ RequestTimeoutError operation reqData          

              _ -> do
                -- Remove the request from the map and return ID to free list
                atomically $ do
                  -- Remove from active requests
                  modifyTVar' state.csActiveRequests $ IM.delete (fromIntegral context.contextId)
                  -- Recycle the ID
                  writeTQueue state.csFreeRequestIds (fromIntegral context.contextId)
                  pure $ Left $ ClientError submitStatus
