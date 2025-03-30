{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.TigerBeetle.Internal.FFI.Client.ClusterId where

import Data.WideWord
import Foreign.Ptr (Ptr, plusPtr)
import Data.Word
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign (Storable(..), Bits (..))

newtype ClusterId = ClusterId { wideword :: Word128 }
  deriving newtype (Eq, Show)

withClusterIdPointer :: ClusterId -> (Ptr Word8 -> IO a) -> IO a
withClusterIdPointer clusterId f = allocaBytes 16 $ \clusterIdPtr -> do
  pokeClusterId clusterIdPtr clusterId.wideword
  f clusterIdPtr

pokeClusterId :: Ptr Word8 -> Word128 -> IO ()
pokeClusterId ptr Word128{..} = 
  pokeWord64LE ptr word128Lo64 >>
  pokeWord64LE (plusPtr ptr 8) word128Hi64

pokeWord64LE :: Ptr Word8 -> Word64 -> IO ()
pokeWord64LE ptr w = do
  pokeByteOff ptr 1 (fromIntegral (shiftR w 8) :: Word8)
  pokeByteOff ptr 2 (fromIntegral (shiftR w 16) :: Word8)
  pokeByteOff ptr 3 (fromIntegral (shiftR w 24) :: Word8)
  pokeByteOff ptr 4 (fromIntegral (shiftR w 32) :: Word8)
  pokeByteOff ptr 5 (fromIntegral (shiftR w 40) :: Word8)
  pokeByteOff ptr 6 (fromIntegral (shiftR w 48) :: Word8)
  pokeByteOff ptr 7 (fromIntegral (shiftR w 56) :: Word8)
