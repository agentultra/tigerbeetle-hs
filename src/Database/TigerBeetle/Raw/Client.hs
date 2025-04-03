{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE TupleSections #-}
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
import Control.Concurrent.STM.TMVar (TMVar, putTMVar)
import Control.Concurrent.STM.TVar (TVar, readTVar, modifyTVar', newTVarIO, writeTVar, stateTVar)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue, newTQueueIO)
import Control.Concurrent.STM (readTVarIO, atomically)
import Data.Maybe (isJust)
import Control.Monad (when, void, forM_)
import qualified Data.ByteString as BS
import Database.TigerBeetle.Raw.Response (TBResponse, decodeResponse, DecodeResponseError)
import Data.Bifunctor
import qualified Data.Text.Encoding as TE

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
    PacketError TBPacketStatus
  | PacketDataParseError DecodeResponseError
  | ClientShutdownDuringRequest
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
setupCompletionCallback :: ClientHandle -> TBCompletionCallback
setupCompletionCallback handle = \ctx packetPtr _timestamp resultPtr resultLen -> do
    -- Get current client state
    state <- readTVarIO handle.tvar
    
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
  -> (ClientHandle -> IO a)
  -> IO (Either TBInitStatus a)
withClient cfg clusterId address action = 
  alloca $ \clientPtr -> do
    clientHandle <- fmap ClientHandle $ newTVarIO =<< ClientState clientPtr
      <$> newTVarIO IM.empty
      <*> newTVarIO 1
      <*> newTQueueIO
      <*> newTVarIO False
      <*> pure cfg.clientTimeoutMillis

    -- Initialize the completion callback
    callback <- makeCompletionCallback $ setupCompletionCallback clientHandle


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
            (Right <$> action clientHandle)
            (finalizeClient clientHandle)
        _ -> pure $ Left initStatus

finalizeClient :: ClientHandle -> IO ()
finalizeClient handle = do
  state <- atomically $ do
    s <- readTVar handle.tvar
    writeTVar s.csIsShutdown True
    pure s
  
  -- Deinitialize the client
  void $ tbClientDeinit state.csClientPtr
  
  -- Signal any remaining active requests and return IDs to the free list
  activeReqs <- atomically $ stateTVar state.csActiveRequests (, IM.empty)
  
  -- Fail any pending requests
  forM_ (IM.elems activeReqs) $ \context ->
    atomically $ putTMVar context.resultVar (Left ClientShutdownDuringRequest)
