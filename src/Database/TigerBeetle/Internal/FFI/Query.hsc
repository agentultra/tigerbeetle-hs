{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Internal.FFI.Query where

import Data.Word
import Data.WideWord
import Foreign.Ptr
import Foreign.Storable
import Data.Vector (Vector)
import Data.Vector qualified as V

#include "tb_client.h"

data QueryFilterFlags = 
      Reversed
    deriving (Eq, Ord, Show)

instance Enum QueryFilterFlags where
    fromEnum Reversed = #const TB_QUERY_FILTER_REVERSED

    toEnum (#const TB_QUERY_FILTER_REVERSED) = Reversed
    toEnum unmatched = error $ "QueryFilterFlags.toEnum: Cannot match " ++ show unmatched

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
    , tbQueryFilterFlags         :: Word32
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
      tbQueryFilterFlags         <- #{peek tb_query_filter_t, flags} ptr
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
        #{poke tb_query_filter_t, flags} ptr queryFilter.tbQueryFilterFlags
