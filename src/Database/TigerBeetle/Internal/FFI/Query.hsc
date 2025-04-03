{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Internal.FFI.Query where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.WideWord
import Foreign.Ptr
import Foreign.Storable
import Data.Set (Set)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.TigerBeetle.Internal.FFI.BitFlag (flagsToBitmask, bitmaskToFlags)

#include "tb_client.h"

data TBQueryFilterFlags = 
      Reversed
    deriving (Eq, Ord, Show)

instance Enum TBQueryFilterFlags where
    fromEnum Reversed = #const TB_QUERY_FILTER_REVERSED

    toEnum (#const TB_QUERY_FILTER_REVERSED) = Reversed
    toEnum unmatched = error $ "QueryFilterFlags.toEnum: Cannot match " ++ show unmatched

marshallTBQueryFilterFlags :: Set TBQueryFilterFlags -> Word32 
marshallTBQueryFilterFlags = flagsToBitmask

unmarshallTBQueryFilterFlags :: Word32 -> Set TBQueryFilterFlags 
unmarshallTBQueryFilterFlags = bitmaskToFlags

data TBQueryFilter = TBQueryFilter
    { tbQueryFilterUserData128   :: Word128
    , tbQueryFilterUserData64    :: Word64
    , tbQueryFilterUserData32    :: Word32
    , tbQueryFilterLedger        :: Word32
    , tbQueryFilterCode          :: Word16
    , tbQueryFilterReserved      :: Vector Word8
    , tbQueryFilterTimestampMin  :: Word64
    , tbQueryFilterTimestampMax  :: Word64
    , tbQueryFilterLimit         :: Word32
    , tbQueryFilterFlags         :: Set TBQueryFilterFlags
    }
    deriving (Show, Eq)

instance Storable TBQueryFilter where
    sizeOf _ = #{size tb_query_filter_t}

    alignment _ = #{alignment tb_query_filter_t}

    peek ptr = do
      let reservedPtr = #{ptr tb_query_filter_t, reserved} ptr
      tbQueryFilterUserData128   <- #{peek tb_query_filter_t, user_data_128} ptr
      tbQueryFilterUserData64    <- #{peek tb_query_filter_t, user_data_64} ptr
      tbQueryFilterUserData32    <- #{peek tb_query_filter_t, user_data_32} ptr
      tbQueryFilterLedger        <- #{peek tb_query_filter_t, ledger} ptr
      tbQueryFilterCode          <- #{peek tb_query_filter_t, code} ptr
      tbQueryFilterReserved      <- V.generateM 6 (\i -> peekByteOff reservedPtr i)
      tbQueryFilterTimestampMin  <- #{peek tb_query_filter_t, timestamp_min} ptr
      tbQueryFilterTimestampMax  <- #{peek tb_query_filter_t, timestamp_max} ptr
      tbQueryFilterLimit         <- #{peek tb_query_filter_t, limit} ptr
      tbQueryFilterFlags         <- unmarshallTBQueryFilterFlags <$> #{peek tb_query_filter_t, flags} ptr
      pure TBQueryFilter{..}

    poke ptr queryFilter = do
        #{poke tb_query_filter_t, user_data_128} ptr queryFilter.tbQueryFilterUserData128
        #{poke tb_query_filter_t, user_data_64} ptr queryFilter.tbQueryFilterUserData64
        #{poke tb_query_filter_t, user_data_32} ptr queryFilter.tbQueryFilterUserData32
        #{poke tb_query_filter_t, ledger} ptr queryFilter.tbQueryFilterLedger
        #{poke tb_query_filter_t, code} ptr queryFilter.tbQueryFilterCode
        let reservedPtr = #{ptr tb_query_filter_t, reserved} ptr
        V.iforM_ queryFilter.tbQueryFilterReserved $ \i val -> pokeByteOff reservedPtr i val
        #{poke tb_query_filter_t, timestamp_min} ptr queryFilter.tbQueryFilterTimestampMin
        #{poke tb_query_filter_t, timestamp_max} ptr queryFilter.tbQueryFilterTimestampMax
        #{poke tb_query_filter_t, limit} ptr queryFilter.tbQueryFilterLimit
        #{poke tb_query_filter_t, flags} ptr (marshallTBQueryFilterFlags queryFilter.tbQueryFilterFlags)

instance Binary TBQueryFilter where
  put queryfilter = do
    put $ queryfilter.tbQueryFilterUserData128
    put $ queryfilter.tbQueryFilterUserData64
    putWord32le $ queryfilter.tbQueryFilterUserData32
    putWord32le $ queryfilter.tbQueryFilterLedger
    putWord16le $ queryfilter.tbQueryFilterCode
    V.mapM_ putWord8 $ queryfilter.tbQueryFilterReserved
    putWord64le $ queryfilter.tbQueryFilterTimestampMin
    putWord64le $ queryfilter.tbQueryFilterTimestampMax
    putWord32le $ queryfilter.tbQueryFilterLimit
    putWord32le . marshallTBQueryFilterFlags $ queryfilter.tbQueryFilterFlags
    
  get = do
    tbQueryFilterUserData128 <- get
    tbQueryFilterUserData64 <- get
    tbQueryFilterUserData32 <- getWord32le
    tbQueryFilterLedger <- getWord32le
    tbQueryFilterCode <- getWord16le
    tbQueryFilterReserved <- V.replicateM 6 getWord8
    tbQueryFilterTimestampMin <- getWord64le
    tbQueryFilterTimestampMax <- getWord64le
    tbQueryFilterLimit <- getWord32le
    tbQueryFilterFlags <- unmarshallTBQueryFilterFlags <$> getWord32le
    pure TBQueryFilter{..}
