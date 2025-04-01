{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Database.TigerBeetle.Raw.Client where

import Database.TigerBeetle.Internal.FFI.Client
import Database.TigerBeetle.Internal.FFI.Client.ClusterId (ClusterId)
import Data.Text (Text)
import Foreign.Marshal.Alloc (alloca)
import qualified Data.Text.Foreign as T
import Foreign (sizeOf, Storable (..))
import Control.Exception (finally)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import GHC.Natural (Natural)
import Data.Word
import Control.Concurrent.STM.TMVar (TMVar, putTMVar)
import Control.Concurrent.STM.TVar (TVar, readTVar, modifyTVar')
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import Control.Concurrent.STM (readTVarIO, atomically)
import Data.Maybe (isJust)
import Control.Monad (when)
import qualified Data.ByteString as BS

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

data Result = Result

-- | Context for a single request
data RequestContext = RequestContext
  { contextId :: Word64  -- ^ Unique identifier for this request
  , resultVar :: TMVar (Either TBPacketStatus Result)  -- ^ Where to put the result
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
                  then pure $ Left packet.tbPacketStatus
                  else do
                    -- Convert the C result to a Haskell value
                    bytes <- BS.packCStringLen (castPtr resultPtr, fromIntegral resultLen)
                    pure $ Right (packet.tbPacketOperation, bytes)
        
        -- Deliver the result
        atomically $ putTMVar context.resultVar (Right Result)
        
      Nothing ->
        -- This could happen during shutdown or if there's a bug
        putStrLn "Warning: Received callback for unknown request context"

data WithClientOps =
  WithClientOps
    { onInitFailure :: TBInitStatus -> IO ()
    , onDeinit :: TBClientStatus -> IO ()
    , useSubmit :: (TBPacket -> IO TBClientStatus) -> IO ()
    }

withClient
  :: ClientKind
  -> ClusterId
  -> Text
  -> WithClientOps
  -> TBCompletionCallback  
  -> IO ()
withClient kind clusterId address ops completionCb = 
  alloca $ \clientPtr ->
    T.withCString address $ \addressPtr -> do
      -- FIXME: need to understand how completion context should be initialized
      let completionContext = 0
      cb <- makeCompletionCallback completionCb
      initStatus <- initFn clientPtr clusterId addressPtr (fromIntegral $ sizeOf addressPtr) completionContext cb
      finally 
        (runClient clientPtr initStatus)
        (freeClient clientPtr)
  where
    initFn = case kind of
                Echo -> tbClientInitEcho
                Standard -> tbClientInit

    runClient :: Ptr TBClient -> TBInitStatus -> IO ()
    runClient clientPtr = \case 
      Success -> ops.useSubmit \packet ->
        alloca \packetPtr -> do
          poke packetPtr packet
          clientSubmit clientPtr packetPtr
      other -> ops.onInitFailure other

    freeClient :: Ptr TBClient -> IO ()
    freeClient clientPtr = do
      clientStatus <- clientDeinit clientPtr
      ops.onDeinit clientStatus
