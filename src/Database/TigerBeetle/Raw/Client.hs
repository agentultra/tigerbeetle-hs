module Database.TigerBeetle.Raw.Client where

import Database.TigerBeetle.Internal.FFI
import Foreign.Ptr

data ClientResponse = ClientOk
  deriving (Eq, Show)

sendRequest :: Ptr TBPacket -> IO ClientResponse
sendRequest _ = pure ClientOk
