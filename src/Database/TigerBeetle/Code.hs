module Database.TigerBeetle.Code where

import Data.Word

-- | Classifies an account
--
-- eg: @100, 101, 102@ are all internal accounts of receivables,
-- settlements, and holds respectively.
--
-- Represented as an unsigned 16-bit integer.
newtype Code = Code {getCode :: Word16}
  deriving (Eq, Show)
