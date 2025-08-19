module Database.TigerBeetle.ClusterId where

import Data.WideWord

-- | Identifies the cluster of the Tigerbeetle instance
--
-- Represented as a 128-bit unsigned integer.
newtype ClusterId = ClusterId {getClusterId :: Word128}
  deriving newtype (Eq, Show)
