{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Internal.FFI.Transfer where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.WideWord
import Data.Set (Set)
import Foreign.Storable
import Database.TigerBeetle.Internal.FFI.BitFlag (flagsToBitmask, bitmaskToFlags)

#include "tb_client.h"

data TBTransferFlags =
      Linked
    | Pending
    | PostPendingTransfer
    | VoidPendingTransfer
    | BalancingDebit
    | BalancingCredit
    | ClosingDebit
    | ClosingCredit
    | Imported
    deriving (Eq, Ord, Show)

instance Enum TBTransferFlags where
    fromEnum Linked              = #const TB_TRANSFER_LINKED
    fromEnum Pending             = #const TB_TRANSFER_PENDING
    fromEnum PostPendingTransfer = #const TB_TRANSFER_POST_PENDING_TRANSFER
    fromEnum VoidPendingTransfer = #const TB_TRANSFER_VOID_PENDING_TRANSFER
    fromEnum BalancingDebit      = #const TB_TRANSFER_BALANCING_DEBIT
    fromEnum BalancingCredit     = #const TB_TRANSFER_BALANCING_CREDIT
    fromEnum ClosingDebit        = #const TB_TRANSFER_CLOSING_DEBIT
    fromEnum ClosingCredit       = #const TB_TRANSFER_CLOSING_CREDIT
    fromEnum Imported            = #const TB_TRANSFER_IMPORTED

    toEnum (#const TB_TRANSFER_LINKED) = Linked
    toEnum (#const TB_TRANSFER_PENDING) = PostPendingTransfer
    toEnum (#const TB_TRANSFER_POST_PENDING_TRANSFER) = PostPendingTransfer
    toEnum (#const TB_TRANSFER_VOID_PENDING_TRANSFER) = VoidPendingTransfer
    toEnum (#const TB_TRANSFER_BALANCING_DEBIT) = BalancingDebit
    toEnum (#const TB_TRANSFER_BALANCING_CREDIT) = BalancingCredit
    toEnum (#const TB_TRANSFER_CLOSING_DEBIT) = ClosingDebit
    toEnum (#const TB_TRANSFER_CLOSING_CREDIT) = ClosingCredit
    toEnum (#const TB_TRANSFER_IMPORTED) = Imported
    toEnum unmatched = error $ "TransferFlags.toEnum: Cannot match " ++ show unmatched

marshallTBTransferFlags :: Set TBTransferFlags -> Word16
marshallTBTransferFlags = flagsToBitmask

unmarshallTBTransferFlags :: Word16 -> Set TBTransferFlags
unmarshallTBTransferFlags = bitmaskToFlags

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
  , tbTransferFlags :: Set TBTransferFlags
  , tbTransferTimestamp :: Word64
  }
  deriving (Eq, Show)

instance Storable TBTransfer where
    sizeOf _ = #{size tb_transfer_t}

    alignment _ = #{alignment tb_transfer_t}

    peek ptr = do
        tbTransferId <- #{peek tb_transfer_t, id} ptr
        tbTransferDebitAccountId <- #{peek tb_transfer_t, debit_account_id} ptr
        tbTransferCreditAccountId <- #{peek tb_transfer_t, credit_account_id} ptr
        tbTransferAmount <- #{peek tb_transfer_t, amount} ptr
        tbTransferPendingId <- #{peek tb_transfer_t, pending_id} ptr
        tbTransferUserData128 <- #{peek tb_transfer_t, user_data_128} ptr
        tbTransferUserData64 <- #{peek tb_transfer_t, user_data_64} ptr
        tbTransferUserData32 <- #{peek tb_transfer_t, user_data_32} ptr
        tbTransferTimeout <- #{peek tb_transfer_t, timeout} ptr
        tbTransferLedger <- #{peek tb_transfer_t, ledger} ptr
        tbTransferCode <- #{peek tb_transfer_t, code} ptr
        tbTransferFlags <- unmarshallTBTransferFlags <$> #{peek tb_transfer_t, flags} ptr
        tbTransferTimestamp <- #{peek tb_transfer_t, timestamp} ptr
        pure TBTransfer{..}

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
        #{poke tb_transfer_t, flags} ptr (marshallTBTransferFlags transfer.tbTransferFlags)
        #{poke tb_transfer_t, timestamp} ptr transfer.tbTransferTimestamp

instance Binary TBTransfer where
  put transfer = do
    put $ transfer.tbTransferId
    put $ transfer.tbTransferDebitAccountId
    put $ transfer.tbTransferCreditAccountId
    put $ transfer.tbTransferAmount
    put $ transfer.tbTransferPendingId
    put $ transfer.tbTransferUserData128
    put $ transfer.tbTransferUserData64
    putWord32le $ transfer.tbTransferUserData32
    putWord32le $ transfer.tbTransferTimeout
    putWord32le $ transfer.tbTransferLedger
    putWord16le $ transfer.tbTransferCode
    putWord16le . marshallTBTransferFlags $ transfer.tbTransferFlags
    putWord64le $ transfer.tbTransferTimestamp

  get = do
    tbTransferId <- get
    tbTransferDebitAccountId <- get
    tbTransferCreditAccountId <- get
    tbTransferAmount <- get
    tbTransferPendingId <- get
    tbTransferUserData128 <- get
    tbTransferUserData64 <- get
    tbTransferUserData32 <- getWord32le
    tbTransferTimeout <- getWord32le
    tbTransferLedger <- getWord32le
    tbTransferCode <- getWord16le
    tbTransferFlags <- unmarshallTBTransferFlags <$> getWord16le
    tbTransferTimestamp <- getWord64le
    return TBTransfer{..}

data TBCreateTransferResult =
      Ok
    | LinkedEventFailed
    | LinkedEventChainOpen
    | ImportedEventExpected
    | ImportedEventNotExpected
    | TimestampMustBeZero
    | ImportedEventTimestampOutOfRange
    | ImportedEventTimestampMustNotAdvance
    | ReservedFlag
    | IdMustNotBeZero
    | IdMustNotBeIntMax
    | ExistsWithDifferentFlags
    | ExistsWithDifferentPendingId
    | ExistsWithDifferentTimeout
    | ExistsWithDifferentDebitAccountId
    | ExistsWithDifferentCreditAccountId
    | ExistsWithDifferentAmount
    | ExistsWithDifferentUserData128
    | ExistsWithDifferentUserData64
    | ExistsWithDifferentUserData32
    | ExistsWithDifferentLedger
    | ExistsWithDifferentCode
    | Exists
    | IdAlreadyFailed
    | FlagsAreMutuallyExclusive
    | DebitAccountIdMustNotBeZero
    | DebitAccountIdMustNotBeIntMax
    | CreditAccountIdMustNotBeZero
    | CreditAccountIdMustNotBeIntMax
    | AccountsMustBeDifferent
    | PendingIdMustBeZero
    | PendingIdMustNotBeZero
    | PendingIdMustNotBeIntMax
    | PendingIdMustBeDifferent
    | TimeoutReservedForPendingTransfer
    | ClosingTransferMustBePending
    | LedgerMustNotBeZero
    | CodeMustNotBeZero
    | DebitAccountNotFound
    | CreditAccountNotFound
    | AccountsMustHaveTheSameLedger
    | TransferMustHaveTheSameLedgerAsAccounts
    | PendingTransferNotFound
    | PendingTransferNotPending
    | PendingTransferHasDifferentDebitAccountId
    | PendingTransferHasDifferentCreditAccountId
    | PendingTransferHasDifferentLedger
    | PendingTransferHasDifferentCode
    | ExceedsPendingTransferAmount
    | PendingTransferHasDifferentAmount
    | PendingTransferAlreadyPosted
    | PendingTransferAlreadyVoided
    | PendingTransferExpired
    | ImportedEventTimestampMustNotRegress
    | ImportedEventTimestampMustPostdateDebitAccount
    | ImportedEventTimestampMustPostdateCreditAccount
    | ImportedEventTimeoutMustBeZero
    | DebitAccountAlreadyClosed
    | CreditAccountAlreadyClosed
    | OverflowsDebitsPending
    | OverflowsCreditsPending
    | OverflowsDebitsPosted
    | OverflowsCreditsPosted
    | OverflowsDebits
    | OverflowsCredits
    | OverflowsTimeout
    | ExceedsCredits
    | ExceedsDebits
    deriving (Show, Eq)

instance Enum TBCreateTransferResult where
    fromEnum Ok                                              = #const TB_CREATE_TRANSFER_OK
    fromEnum LinkedEventFailed                               = #const TB_CREATE_TRANSFER_LINKED_EVENT_FAILED
    fromEnum LinkedEventChainOpen                            = #const TB_CREATE_TRANSFER_LINKED_EVENT_CHAIN_OPEN
    fromEnum ImportedEventExpected                           = #const TB_CREATE_TRANSFER_IMPORTED_EVENT_EXPECTED
    fromEnum ImportedEventNotExpected                        = #const TB_CREATE_TRANSFER_IMPORTED_EVENT_NOT_EXPECTED
    fromEnum TimestampMustBeZero                             = #const TB_CREATE_TRANSFER_TIMESTAMP_MUST_BE_ZERO
    fromEnum ImportedEventTimestampOutOfRange                = #const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_OUT_OF_RANGE
    fromEnum ImportedEventTimestampMustNotAdvance            = #const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_ADVANCE
    fromEnum ReservedFlag                                    = #const TB_CREATE_TRANSFER_RESERVED_FLAG
    fromEnum IdMustNotBeZero                                 = #const TB_CREATE_TRANSFER_ID_MUST_NOT_BE_ZERO
    fromEnum IdMustNotBeIntMax                               = #const TB_CREATE_TRANSFER_ID_MUST_NOT_BE_INT_MAX
    fromEnum ExistsWithDifferentFlags                        = #const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_FLAGS
    fromEnum ExistsWithDifferentPendingId                    = #const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_PENDING_ID
    fromEnum ExistsWithDifferentTimeout                      = #const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_TIMEOUT
    fromEnum ExistsWithDifferentDebitAccountId               = #const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_DEBIT_ACCOUNT_ID
    fromEnum ExistsWithDifferentCreditAccountId              = #const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_CREDIT_ACCOUNT_ID
    fromEnum ExistsWithDifferentAmount                       = #const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_AMOUNT
    fromEnum ExistsWithDifferentUserData128                  = #const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_128
    fromEnum ExistsWithDifferentUserData64                   = #const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_64
    fromEnum ExistsWithDifferentUserData32                   = #const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_32
    fromEnum ExistsWithDifferentLedger                       = #const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_LEDGER
    fromEnum ExistsWithDifferentCode                         = #const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_CODE
    fromEnum Exists                                          = #const TB_CREATE_TRANSFER_EXISTS
    fromEnum IdAlreadyFailed                                 = #const TB_CREATE_TRANSFER_ID_ALREADY_FAILED
    fromEnum FlagsAreMutuallyExclusive                       = #const TB_CREATE_TRANSFER_FLAGS_ARE_MUTUALLY_EXCLUSIVE
    fromEnum DebitAccountIdMustNotBeZero                     = #const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_ID_MUST_NOT_BE_ZERO
    fromEnum DebitAccountIdMustNotBeIntMax                   = #const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_ID_MUST_NOT_BE_INT_MAX
    fromEnum CreditAccountIdMustNotBeZero                    = #const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_ID_MUST_NOT_BE_ZERO
    fromEnum CreditAccountIdMustNotBeIntMax                  = #const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_ID_MUST_NOT_BE_INT_MAX
    fromEnum AccountsMustBeDifferent                         = #const TB_CREATE_TRANSFER_ACCOUNTS_MUST_BE_DIFFERENT
    fromEnum PendingIdMustBeZero                             = #const TB_CREATE_TRANSFER_PENDING_ID_MUST_BE_ZERO
    fromEnum PendingIdMustNotBeZero                          = #const TB_CREATE_TRANSFER_PENDING_ID_MUST_NOT_BE_ZERO
    fromEnum PendingIdMustNotBeIntMax                        = #const TB_CREATE_TRANSFER_PENDING_ID_MUST_NOT_BE_INT_MAX
    fromEnum PendingIdMustBeDifferent                        = #const TB_CREATE_TRANSFER_PENDING_ID_MUST_BE_DIFFERENT
    fromEnum TimeoutReservedForPendingTransfer               = #const TB_CREATE_TRANSFER_TIMEOUT_RESERVED_FOR_PENDING_TRANSFER
    fromEnum ClosingTransferMustBePending                    = #const TB_CREATE_TRANSFER_CLOSING_TRANSFER_MUST_BE_PENDING
    fromEnum LedgerMustNotBeZero                             = #const TB_CREATE_TRANSFER_LEDGER_MUST_NOT_BE_ZERO
    fromEnum CodeMustNotBeZero                               = #const TB_CREATE_TRANSFER_CODE_MUST_NOT_BE_ZERO
    fromEnum DebitAccountNotFound                            = #const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_NOT_FOUND
    fromEnum CreditAccountNotFound                           = #const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_NOT_FOUND
    fromEnum AccountsMustHaveTheSameLedger                   = #const TB_CREATE_TRANSFER_ACCOUNTS_MUST_HAVE_THE_SAME_LEDGER
    fromEnum TransferMustHaveTheSameLedgerAsAccounts         = #const TB_CREATE_TRANSFER_TRANSFER_MUST_HAVE_THE_SAME_LEDGER_AS_ACCOUNTS
    fromEnum PendingTransferNotFound                         = #const TB_CREATE_TRANSFER_PENDING_TRANSFER_NOT_FOUND
    fromEnum PendingTransferNotPending                       = #const TB_CREATE_TRANSFER_PENDING_TRANSFER_NOT_PENDING
    fromEnum PendingTransferHasDifferentDebitAccountId       = #const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_DEBIT_ACCOUNT_ID
    fromEnum PendingTransferHasDifferentCreditAccountId      = #const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_CREDIT_ACCOUNT_ID
    fromEnum PendingTransferHasDifferentLedger               = #const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_LEDGER
    fromEnum PendingTransferHasDifferentCode                 = #const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_CODE
    fromEnum ExceedsPendingTransferAmount                    = #const TB_CREATE_TRANSFER_EXCEEDS_PENDING_TRANSFER_AMOUNT
    fromEnum PendingTransferHasDifferentAmount               = #const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_AMOUNT
    fromEnum PendingTransferAlreadyPosted                    = #const TB_CREATE_TRANSFER_PENDING_TRANSFER_ALREADY_POSTED
    fromEnum PendingTransferAlreadyVoided                    = #const TB_CREATE_TRANSFER_PENDING_TRANSFER_ALREADY_VOIDED
    fromEnum PendingTransferExpired                          = #const TB_CREATE_TRANSFER_PENDING_TRANSFER_EXPIRED
    fromEnum ImportedEventTimestampMustNotRegress            = #const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_REGRESS
    fromEnum ImportedEventTimestampMustPostdateDebitAccount  = #const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_POSTDATE_DEBIT_ACCOUNT
    fromEnum ImportedEventTimestampMustPostdateCreditAccount = #const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_POSTDATE_CREDIT_ACCOUNT
    fromEnum ImportedEventTimeoutMustBeZero                  = #const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMEOUT_MUST_BE_ZERO
    fromEnum DebitAccountAlreadyClosed                       = #const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_ALREADY_CLOSED
    fromEnum CreditAccountAlreadyClosed                      = #const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_ALREADY_CLOSED
    fromEnum OverflowsDebitsPending                          = #const TB_CREATE_TRANSFER_OVERFLOWS_DEBITS_PENDING
    fromEnum OverflowsCreditsPending                         = #const TB_CREATE_TRANSFER_OVERFLOWS_CREDITS_PENDING
    fromEnum OverflowsDebitsPosted                           = #const TB_CREATE_TRANSFER_OVERFLOWS_DEBITS_POSTED
    fromEnum OverflowsCreditsPosted                          = #const TB_CREATE_TRANSFER_OVERFLOWS_CREDITS_POSTED
    fromEnum OverflowsDebits                                 = #const TB_CREATE_TRANSFER_OVERFLOWS_DEBITS
    fromEnum OverflowsCredits                                = #const TB_CREATE_TRANSFER_OVERFLOWS_CREDITS
    fromEnum OverflowsTimeout                                = #const TB_CREATE_TRANSFER_OVERFLOWS_TIMEOUT
    fromEnum ExceedsCredits                                  = #const TB_CREATE_TRANSFER_EXCEEDS_CREDITS
    fromEnum ExceedsDebits                                   = #const TB_CREATE_TRANSFER_EXCEEDS_DEBITS

    toEnum (#const TB_CREATE_TRANSFER_OK)                                                    = Ok
    toEnum (#const TB_CREATE_TRANSFER_LINKED_EVENT_FAILED)                                   = LinkedEventFailed
    toEnum (#const TB_CREATE_TRANSFER_LINKED_EVENT_CHAIN_OPEN)                               = LinkedEventChainOpen
    toEnum (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_EXPECTED)                               = ImportedEventExpected
    toEnum (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_NOT_EXPECTED)                           = ImportedEventNotExpected
    toEnum (#const TB_CREATE_TRANSFER_TIMESTAMP_MUST_BE_ZERO)                                = TimestampMustBeZero
    toEnum (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_OUT_OF_RANGE)                 = ImportedEventTimestampOutOfRange
    toEnum (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_ADVANCE)             = ImportedEventTimestampMustNotAdvance
    toEnum (#const TB_CREATE_TRANSFER_RESERVED_FLAG)                                         = ReservedFlag
    toEnum (#const TB_CREATE_TRANSFER_ID_MUST_NOT_BE_ZERO)                                   = IdMustNotBeZero
    toEnum (#const TB_CREATE_TRANSFER_ID_MUST_NOT_BE_INT_MAX)                                = IdMustNotBeIntMax
    toEnum (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_FLAGS)                           = ExistsWithDifferentFlags
    toEnum (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_PENDING_ID)                      = ExistsWithDifferentPendingId
    toEnum (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_TIMEOUT)                         = ExistsWithDifferentTimeout
    toEnum (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_DEBIT_ACCOUNT_ID)                = ExistsWithDifferentDebitAccountId
    toEnum (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_CREDIT_ACCOUNT_ID)               = ExistsWithDifferentCreditAccountId
    toEnum (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_AMOUNT)                          = ExistsWithDifferentAmount
    toEnum (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_128)                   = ExistsWithDifferentUserData128
    toEnum (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_64)                    = ExistsWithDifferentUserData64
    toEnum (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_32)                    = ExistsWithDifferentUserData32
    toEnum (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_LEDGER)                          = ExistsWithDifferentLedger
    toEnum (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_CODE)                            = ExistsWithDifferentCode
    toEnum (#const TB_CREATE_TRANSFER_EXISTS)                                                = Exists
    toEnum (#const TB_CREATE_TRANSFER_ID_ALREADY_FAILED)                                     = IdAlreadyFailed
    toEnum (#const TB_CREATE_TRANSFER_FLAGS_ARE_MUTUALLY_EXCLUSIVE)                          = FlagsAreMutuallyExclusive
    toEnum (#const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_ID_MUST_NOT_BE_ZERO)                     = DebitAccountIdMustNotBeZero
    toEnum (#const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_ID_MUST_NOT_BE_INT_MAX)                  = DebitAccountIdMustNotBeIntMax
    toEnum (#const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_ID_MUST_NOT_BE_ZERO)                    = CreditAccountIdMustNotBeZero
    toEnum (#const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_ID_MUST_NOT_BE_INT_MAX)                 = CreditAccountIdMustNotBeIntMax
    toEnum (#const TB_CREATE_TRANSFER_ACCOUNTS_MUST_BE_DIFFERENT)                            = AccountsMustBeDifferent
    toEnum (#const TB_CREATE_TRANSFER_PENDING_ID_MUST_BE_ZERO)                               = PendingIdMustBeZero
    toEnum (#const TB_CREATE_TRANSFER_PENDING_ID_MUST_NOT_BE_ZERO)                           = PendingIdMustNotBeZero
    toEnum (#const TB_CREATE_TRANSFER_PENDING_ID_MUST_NOT_BE_INT_MAX)                        = PendingIdMustNotBeIntMax
    toEnum (#const TB_CREATE_TRANSFER_PENDING_ID_MUST_BE_DIFFERENT)                          = PendingIdMustBeDifferent
    toEnum (#const TB_CREATE_TRANSFER_TIMEOUT_RESERVED_FOR_PENDING_TRANSFER)                 = TimeoutReservedForPendingTransfer
    toEnum (#const TB_CREATE_TRANSFER_CLOSING_TRANSFER_MUST_BE_PENDING)                      = ClosingTransferMustBePending
    toEnum (#const TB_CREATE_TRANSFER_LEDGER_MUST_NOT_BE_ZERO)                               = LedgerMustNotBeZero
    toEnum (#const TB_CREATE_TRANSFER_CODE_MUST_NOT_BE_ZERO)                                 = CodeMustNotBeZero
    toEnum (#const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_NOT_FOUND)                               = DebitAccountNotFound
    toEnum (#const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_NOT_FOUND)                              = CreditAccountNotFound
    toEnum (#const TB_CREATE_TRANSFER_ACCOUNTS_MUST_HAVE_THE_SAME_LEDGER)                    = AccountsMustHaveTheSameLedger
    toEnum (#const TB_CREATE_TRANSFER_TRANSFER_MUST_HAVE_THE_SAME_LEDGER_AS_ACCOUNTS)        = TransferMustHaveTheSameLedgerAsAccounts
    toEnum (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_NOT_FOUND)                            = PendingTransferNotFound
    toEnum (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_NOT_PENDING)                          = PendingTransferNotPending
    toEnum (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_DEBIT_ACCOUNT_ID)       = PendingTransferHasDifferentDebitAccountId
    toEnum (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_CREDIT_ACCOUNT_ID)      = PendingTransferHasDifferentCreditAccountId
    toEnum (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_LEDGER)                 = PendingTransferHasDifferentLedger
    toEnum (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_CODE)                   = PendingTransferHasDifferentCode
    toEnum (#const TB_CREATE_TRANSFER_EXCEEDS_PENDING_TRANSFER_AMOUNT)                       = ExceedsPendingTransferAmount
    toEnum (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_AMOUNT)                 = PendingTransferHasDifferentAmount
    toEnum (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_ALREADY_POSTED)                       = PendingTransferAlreadyPosted
    toEnum (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_ALREADY_VOIDED)                       = PendingTransferAlreadyVoided
    toEnum (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_EXPIRED)                              = PendingTransferExpired
    toEnum (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_REGRESS)             = ImportedEventTimestampMustNotRegress
    toEnum (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_POSTDATE_DEBIT_ACCOUNT)  = ImportedEventTimestampMustPostdateDebitAccount
    toEnum (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_POSTDATE_CREDIT_ACCOUNT) = ImportedEventTimestampMustPostdateCreditAccount
    toEnum (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMEOUT_MUST_BE_ZERO)                   = ImportedEventTimeoutMustBeZero
    toEnum (#const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_ALREADY_CLOSED)                          = DebitAccountAlreadyClosed
    toEnum (#const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_ALREADY_CLOSED)                         = CreditAccountAlreadyClosed
    toEnum (#const TB_CREATE_TRANSFER_OVERFLOWS_DEBITS_PENDING)                              = OverflowsDebitsPending
    toEnum (#const TB_CREATE_TRANSFER_OVERFLOWS_CREDITS_PENDING)                             = OverflowsCreditsPending
    toEnum (#const TB_CREATE_TRANSFER_OVERFLOWS_DEBITS_POSTED)                               = OverflowsDebitsPosted
    toEnum (#const TB_CREATE_TRANSFER_OVERFLOWS_CREDITS_POSTED)                              = OverflowsCreditsPosted
    toEnum (#const TB_CREATE_TRANSFER_OVERFLOWS_DEBITS)                                      = OverflowsDebits
    toEnum (#const TB_CREATE_TRANSFER_OVERFLOWS_CREDITS)                                     = OverflowsCredits
    toEnum (#const TB_CREATE_TRANSFER_OVERFLOWS_TIMEOUT)                                     = OverflowsTimeout
    toEnum (#const TB_CREATE_TRANSFER_EXCEEDS_CREDITS)                                       = ExceedsCredits
    toEnum (#const TB_CREATE_TRANSFER_EXCEEDS_DEBITS)                                        = ExceedsDebits
    toEnum unmatched                                                                         = error $ "CreateTransfersResult.toEnum: Cannot match " ++ show unmatched

instance Binary TBCreateTransferResult where
  put = putWord32le . marshallTBCreateTransferResult
  get = unmarshallTBCreateTransferResult <$> getWord32le

marshallTBCreateTransferResult :: TBCreateTransferResult -> Word32
marshallTBCreateTransferResult = fromIntegral . fromEnum

unmarshallTBCreateTransferResult :: Word32 -> TBCreateTransferResult
unmarshallTBCreateTransferResult = toEnum . fromIntegral

data TBCreateTransfersResult = TBCreateTransfersResult
    { tbCreateTransfersResultIndex :: Word32
    , tbCreateTransfersResultResult :: TBCreateTransferResult
    }
    deriving (Show, Eq)

instance Storable TBCreateTransfersResult  where
    sizeOf _ = #{size tb_create_transfers_result_t}

    alignment _ = #{alignment tb_create_transfers_result_t}

    peek ptr = do
      tbCreateTransfersResultIndex  <- #{peek tb_create_transfers_result_t, index} ptr
      tbCreateTransfersResultResult <- unmarshallTBCreateTransferResult <$> #{peek tb_create_transfers_result_t, result} ptr
      pure TBCreateTransfersResult{..}

    poke ptr createTransfersResult = do
        #{poke tb_create_transfers_result_t, index} ptr createTransfersResult.tbCreateTransfersResultIndex
        #{poke tb_create_transfers_result_t, result} ptr (marshallTBCreateTransferResult createTransfersResult.tbCreateTransfersResultResult)

instance Binary TBCreateTransfersResult where
  put result = do
    putWord32le result.tbCreateTransfersResultIndex
    put result.tbCreateTransfersResultResult

  get = do
    tbCreateTransfersResultIndex <- getWord32le
    tbCreateTransfersResultResult <- get
    return TBCreateTransfersResult{..}
