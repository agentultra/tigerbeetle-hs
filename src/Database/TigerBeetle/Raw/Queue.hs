{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Database.TigerBeetle.Raw.Queue where

import Database.TigerBeetle.Internal.FFI.Client
import Database.TigerBeetle.Internal.FFI.Client.ClusterId (ClusterId)
import Data.Text (Text)
import Foreign.Marshal.Alloc (alloca)
import qualified Data.Text.Foreign as T
import Foreign (sizeOf, Storable (poke))
import Control.Exception (finally)
import Foreign.Ptr (Ptr)

data WithClientOps =
  WithClientOps
    { onInitFailure :: TBInitStatus -> IO ()
    , onDeinit :: TBClientStatus -> IO ()
    , useSubmit :: (TBPacket -> IO TBClientStatus) -> IO ()
    }

data ClientKind = Echo | Standard
  deriving (Eq, Ord, Show)

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

-- TODO: implement something like this
-- | Initialize a client with a TQueue for responses
-- initClient ::
--   -- | Cluster ID
--   [Word8] ->
--   -- | Address
--   Text ->
--   -- | Response queue
--   TQueue Response ->
--   IO (Either InitStatus Client)
-- initClient clusterId addr responseQueue =
--   alloca $ \out_client -> do
--     -- Create the callback function that will write to our TQueue
--     let callback :: CompletionCallback
--         callback _ctx _client packetPtr _reserved dataPtr cbDataSize = do
--           packet <- peek packetPtr
--           print packet
--           -- TODO: Figure out the actual semantics for the callback function here
--           dataCopy <- peekArray (fromIntegral cbDataSize) dataPtr
--           atomically $
--             writeTQueue
--               responseQueue
--               Response
--                 { responsePacket = packet,
--                   responseData = dataCopy,
--                   responseStatus = packet.tbPacketStatus
--                 }

--     callbackPtr <- makeCompletionCallback callback

--     -- Initialize the client
--     withArray clusterId $ \cluster_id_16 -> do
--       print cluster_id_16
--       T.withCString addr $ \address_ptr -> do
--         clientInitStatus <- tb_client_init out_client cluster_id_16 address_ptr (fromIntegral $ sizeOf address_ptr) 0 callbackPtr
--         case clientInitStatus of
--           Success -> Right <$> peek out_client
--           other -> pure $ Left other
