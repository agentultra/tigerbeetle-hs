{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Internal.FFI.Client where

import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Data.Vector (Vector)
import Data.Vector qualified as V

#include "tb_client.h"

data TBClient = TBClient
    { tbClientOpaque :: Vector Word64
    }
    deriving (Show, Eq)

instance Storable TBClient where
    sizeOf _ = #{size tb_client_t}

    alignment _ = #{alignment tb_client_t}

    peek ptr = do
      let opaquePtr = #{ptr tb_client_t, opaque} ptr
      tbClientOpaque <- V.generateM 4 (\i -> peekByteOff opaquePtr (i * 8))
      pure TBClient{..}

    poke ptr client = do
      let opaquePtr = #{ptr tb_client_t, opaque} ptr
      V.iforM_ client.tbClientOpaque $ \i val -> pokeByteOff opaquePtr (i * 8) val

data TBOperation =
      Pulse
    | CreateAccounts
    | CreateTransfers
    | LookupAccounts
    | LookupTransfers
    | GetAccountTransfers
    | GetAccountBalances
    | QueryAccounts
    | QueryTransfers
    | GetEvents
    deriving (Eq, Show)

instance Enum TBOperation where
    fromEnum Pulse               = #const TB_OPERATION_PULSE
    fromEnum CreateAccounts      = #const TB_OPERATION_CREATE_ACCOUNTS
    fromEnum CreateTransfers     = #const TB_OPERATION_CREATE_TRANSFERS
    fromEnum LookupAccounts      = #const TB_OPERATION_LOOKUP_ACCOUNTS
    fromEnum LookupTransfers     = #const TB_OPERATION_LOOKUP_TRANSFERS
    fromEnum GetAccountTransfers = #const TB_OPERATION_GET_ACCOUNT_TRANSFERS
    fromEnum GetAccountBalances  = #const TB_OPERATION_GET_ACCOUNT_BALANCES
    fromEnum QueryAccounts       = #const TB_OPERATION_QUERY_ACCOUNTS
    fromEnum QueryTransfers      = #const TB_OPERATION_QUERY_TRANSFERS
    fromEnum GetEvents           = #const TB_OPERATION_GET_EVENTS

    toEnum (#const TB_OPERATION_PULSE)                 = Pulse
    toEnum (#const TB_OPERATION_CREATE_ACCOUNTS)       = CreateAccounts
    toEnum (#const TB_OPERATION_CREATE_TRANSFERS)      = CreateTransfers
    toEnum (#const TB_OPERATION_LOOKUP_ACCOUNTS)       = LookupAccounts
    toEnum (#const TB_OPERATION_LOOKUP_TRANSFERS)      = LookupTransfers
    toEnum (#const TB_OPERATION_GET_ACCOUNT_TRANSFERS) = GetAccountTransfers
    toEnum (#const TB_OPERATION_GET_ACCOUNT_BALANCES)  = GetAccountBalances
    toEnum (#const TB_OPERATION_QUERY_ACCOUNTS)        = QueryAccounts
    toEnum (#const TB_OPERATION_QUERY_TRANSFERS)       = QueryTransfers
    toEnum (#const TB_OPERATION_GET_EVENTS)            = GetEvents
    toEnum unmatched = error $ "TBOperation.toEnum: Cannot match " ++ show unmatched
