{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Internal.FFI.Transfer where

import Data.Word
import Data.WideWord
import Foreign.Storable

#include "tb_client.h"

data TransferFlags = 
      Linked 
    | Pending 
    | PostPendingTransfer 
    | VoidPendingTransfer 
    | BalancingDebit 
    | BalancingCredit 
    | ClosingDebit 
    | ClosingCredit 
    | Imported 
    deriving (Eq, Show)

instance Enum TransferFlags where
    fromEnum Linked              = #const TB_TRANSFER_LINKED
    fromEnum Pending = #const TB_TRANSFER_PENDING
    fromEnum PostPendingTransfer = #const TB_TRANSFER_POST_PENDING_TRANSFER
    fromEnum VoidPendingTransfer = #const TB_TRANSFER_VOID_PENDING_TRANSFER
    fromEnum BalancingDebit      = #const TB_TRANSFER_BALANCING_DEBIT
    fromEnum BalancingCredit     = #const TB_TRANSFER_BALANCING_CREDIT
    fromEnum ClosingDebit        = #const TB_TRANSFER_CLOSING_DEBIT
    fromEnum ClosingCredit       = #const TB_TRANSFER_CLOSING_CREDIT
    fromEnum Imported            = #const TB_TRANSFER_IMPORTED

    toEnum (#const TB_TRANSFER_LINKED) = Linked
    toEnum (#const TB_TRANSFER_PENDING) = Pending
    toEnum (#const TB_TRANSFER_POST_PENDING_TRANSFER) = PostPendingTransfer
    toEnum (#const TB_TRANSFER_VOID_PENDING_TRANSFER) = VoidPendingTransfer
    toEnum (#const TB_TRANSFER_BALANCING_DEBIT) = BalancingDebit
    toEnum (#const TB_TRANSFER_BALANCING_CREDIT) = BalancingCredit
    toEnum (#const TB_TRANSFER_CLOSING_DEBIT) = ClosingDebit
    toEnum (#const TB_TRANSFER_CLOSING_CREDIT) = ClosingCredit
    toEnum (#const TB_TRANSFER_IMPORTED) = Imported
    toEnum unmatched = error $ "TransferFlags.toEnum: Cannot match " ++ show unmatched

data TBTransfer
  = TBTransfer
  { tbTransferId :: Word128
  , tbTransferDebitAccountId :: Word128
  , tbTransferCreditAccountId :: Word128
  , tbTransferAmount :: Word128
  , tbTransferPendingId :: Word128
  , tbTransferUserData128 :: Word128
  , tbTransferUserData64 :: Word64
  , tbTransferUserData32 :: Word32
  , tbTransferTimeout :: Word32
  , tbTransferLedger :: Word32
  , tbTransferCode :: Word16
  , tbTransferFlags :: Word16
  , tbTransferTimestamp :: Word64
  }
  deriving (Eq, Show)

instance Storable TBTransfer where
    sizeOf _ = #{size tb_transfer_t}

    alignment _ = #{alignment tb_transfer_t}

    peek ptr
      = TBTransfer
        <$> #{peek tb_transfer_t, id} ptr
        <*> #{peek tb_transfer_t, debit_account_id} ptr
        <*> #{peek tb_transfer_t, credit_account_id} ptr
        <*> #{peek tb_transfer_t, amount} ptr
        <*> #{peek tb_transfer_t, pending_id} ptr
        <*> #{peek tb_transfer_t, user_data_128} ptr
        <*> #{peek tb_transfer_t, user_data_64} ptr
        <*> #{peek tb_transfer_t, user_data_32} ptr
        <*> #{peek tb_transfer_t, timeout} ptr
        <*> #{peek tb_transfer_t, ledger} ptr
        <*> #{peek tb_transfer_t, code} ptr
        <*> #{peek tb_transfer_t, flags} ptr
        <*> #{peek tb_transfer_t, timestamp} ptr

    poke ptr transfer = do
        #{poke tb_transfer_t, id} ptr transfer.tbTransferId
        #{poke tb_transfer_t, debit_account_id} ptr transfer.tbTransferDebitAccountId
        #{poke tb_transfer_t, credit_account_id} ptr transfer.tbTransferCreditAccountId
        #{poke tb_transfer_t, amount} ptr transfer.tbTransferAmount
        #{poke tb_transfer_t, pending_id} ptr transfer.tbTransferPendingId
        #{poke tb_transfer_t, user_data_128} ptr transfer.tbTransferUserData128
        #{poke tb_transfer_t, user_data_64} ptr transfer.tbTransferUserData64
        #{poke tb_transfer_t, user_data_32} ptr transfer.tbTransferUserData32
        #{poke tb_transfer_t, timeout} ptr transfer.tbTransferTimeout
        #{poke tb_transfer_t, ledger} ptr transfer.tbTransferLedger
        #{poke tb_transfer_t, code} ptr transfer.tbTransferCode
        #{poke tb_transfer_t, flags} ptr transfer.tbTransferFlags
        #{poke tb_transfer_t, timestamp} ptr transfer.tbTransferTimestamp
