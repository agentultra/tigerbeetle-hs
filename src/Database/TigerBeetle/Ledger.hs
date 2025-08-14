module Database.TigerBeetle.Ledger where

import Data.Word

-- | Partitions the set of accounts that can transact with each other
newtype LedgerId = LedgerId { getLedgerId :: Word32 }
  deriving (Eq, Show)
