module Database.TigerBeetle.Timestamp where

import Data.Word

newtype Timestamp = Timestamp {getTimestamp :: Word64}
  deriving (Eq, Show)
