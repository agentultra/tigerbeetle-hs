module Database.TigerBeetle.Raw.Client where

import Foreign.Ptr
import Database.TigerBeetle.Internal.FFI.Client (TBPacket)

data ClientResponse = ClientOk
  deriving (Eq, Show)

sendRequest :: Ptr TBPacket -> IO ClientResponse
sendRequest _ = pure ClientOk
