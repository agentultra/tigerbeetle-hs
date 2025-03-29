module Database.TigerBeetle.Raw.Queue where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue)
import Data.Text (Text)
import Data.Text.Foreign qualified as T
import Data.Word (Word8)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Storable
import Database.TigerBeetle.Internal.FFI

-- | Response type for handling callbacks
data Response = Response
  { responsePacket :: TBPacket,
    responseData :: [Word8],
    responseStatus :: PacketStatus
  }

-- | Initialize a client with a TQueue for responses
initClient ::
  -- | Cluster ID
  [Word8] ->
  -- | Address
  Text ->
  -- | Response queue
  TQueue Response ->
  IO (Either InitStatus Client)
initClient clusterId addr responseQueue =
  alloca $ \out_client -> do
    -- Create the callback function that will write to our TQueue
    let callback :: CompletionCallback
        callback _ctx _client packetPtr _reserved dataPtr cbDataSize = do
          packet <- peek packetPtr
          print packet
          -- TODO: Figure out the actual semantics for the callback function here
          dataCopy <- peekArray (fromIntegral cbDataSize) dataPtr
          atomically $
            writeTQueue
              responseQueue
              Response
                { responsePacket = packet,
                  responseData = dataCopy,
                  responseStatus = packet.tbPacketStatus
                }

    callbackPtr <- makeCompletionCallback callback

    -- Initialize the client
    withArray clusterId $ \cluster_id_16 -> do
      print cluster_id_16
      T.withCString addr $ \address_ptr -> do
        clientInitStatus <- tb_client_init out_client cluster_id_16 address_ptr (fromIntegral $ sizeOf address_ptr) 0 callbackPtr
        case clientInitStatus of
          Success -> Right <$> peek out_client
          other -> pure $ Left other
