module Database.TigerBeetle.ClusterId where

import Data.WideWord

newtype ClusterId = ClusterId {wideword :: Word128}
  deriving newtype (Eq, Show)
