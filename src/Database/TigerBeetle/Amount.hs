module Database.TigerBeetle.Amount where

import Data.WideWord

-- | An amount of currency in the base unit of that currency
--
-- Amounts in Tigerbeetle are represented as 128-bit unsigned
-- integers.
newtype Amount = Amount { getAmount :: Word128 }
  deriving (Eq, Show)
