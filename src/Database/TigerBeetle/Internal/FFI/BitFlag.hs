{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Internal.FFI.BitFlag where

import Data.Bits
import Data.Set (Set)
import Data.Set qualified as S

flagsToBitmask :: forall a b. (Enum a, FiniteBits b, Num b) => Set a -> b
flagsToBitmask = S.foldr (\flag acc -> acc .|. fromIntegral (fromEnum flag)) 0

safeFlagsToBitmask :: forall a b. (Bounded a, Enum a, FiniteBits b, Num b) => Set a -> Maybe b
safeFlagsToBitmask s =
  let
    enoughBitsForFlags = S.size s <= finiteBitSize @b 0
    wordIsBigEnough = fromEnum (maxBound @a) <= 2 ^ finiteBitSize @b 0
  in
    if enoughBitsForFlags && wordIsBigEnough
      then Just $ flagsToBitmask s
      else Nothing

bitmaskToFlags :: forall b a. (Ord a, Enum a, FiniteBits b, Num b) => b -> Set a
bitmaskToFlags bitmask =
  S.fromList $
    [ flag | flag <- enumFrom (toEnum 0)
    -- Note: an alternate implementation could look like this, which makes the C style
    -- bit flag more obvious
    --
    -- (bitmask .&. fromIntegral @Int @b (fromEnum flag)) /= 0
    , testBit bitmask . countTrailingZeros . fromIntegral @Int @b $ fromEnum flag
    ]

safeBitmaskToFlags
  :: forall b a
   . (Ord a, Bounded a, Enum a, FiniteBits b, Num b)
  => b
  -> Maybe (Set a)
safeBitmaskToFlags bitmask =
  if fromEnum (maxBound @a) <= 2 ^ finiteBitSize @b 0
    then Just $ bitmaskToFlags bitmask
    else Nothing
