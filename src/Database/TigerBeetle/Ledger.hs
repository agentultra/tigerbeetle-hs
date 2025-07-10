module Database.TigerBeetle.Ledger where

import Data.Word

newtype LedgerId = LedgerId { getLedgerId :: Word32 }
  deriving (Eq, Show)
