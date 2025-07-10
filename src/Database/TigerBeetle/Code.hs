module Database.TigerBeetle.Code where

import Data.Word

newtype Code = Code { getCode :: Word16 }
  deriving (Eq, Show)
