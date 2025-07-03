module Database.TigerBeetle.Amount where

import Data.WideWord

newtype Amount = Amount { getAmount :: Word128 }
