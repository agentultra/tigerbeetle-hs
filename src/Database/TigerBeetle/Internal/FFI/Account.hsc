{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Internal.FFI.Account where

import Data.Word
import Data.WideWord
import Foreign.Storable

#include "tb_client.h"

data AccountFlags = 
      Linked
    | DebitsMustNotExceedCredits
    | CreditsMustNotExceedDebits
    | History
    | Imported
    | Closed
    deriving (Eq, Ord, Show)

instance Enum AccountFlags where
    fromEnum Linked                     = #const TB_ACCOUNT_LINKED
    fromEnum DebitsMustNotExceedCredits = #const TB_ACCOUNT_DEBITS_MUST_NOT_EXCEED_CREDITS
    fromEnum CreditsMustNotExceedDebits = #const TB_ACCOUNT_CREDITS_MUST_NOT_EXCEED_DEBITS
    fromEnum History                    = #const TB_ACCOUNT_HISTORY
    fromEnum Imported                   = #const TB_ACCOUNT_IMPORTED
    fromEnum Closed                     = #const TB_ACCOUNT_CLOSED

    toEnum (#const TB_ACCOUNT_LINKED)                          = Linked
    toEnum (#const TB_ACCOUNT_DEBITS_MUST_NOT_EXCEED_CREDITS)  = DebitsMustNotExceedCredits 
    toEnum (#const TB_ACCOUNT_CREDITS_MUST_NOT_EXCEED_DEBITS)  = CreditsMustNotExceedDebits
    toEnum (#const TB_ACCOUNT_HISTORY)                         = History
    toEnum (#const TB_ACCOUNT_IMPORTED)                        = Imported
    toEnum (#const TB_ACCOUNT_CLOSED)                          = Closed
    toEnum unmatched = error $ "AccountFlags.toEnum: Cannot match " ++ show unmatched

data TBAccount
  = TBAccount
  { tbAccountId             :: Word128
  , tbAccountDebitsPending  :: Word128
  , tbAccountDebitsPosted   :: Word128
  , tbAccountCreditsPending :: Word128
  , tbAccountCreditsPosted  :: Word128
  , tbAccountUserData128    :: Word128
  , tbAccountUserData64     :: Word64
  , tbAccountUserData32     :: Word32
  , tbAccountReserved       :: Word32
  , tbAccountLedger         :: Word32
  , tbAccountCode           :: Word16
  , tbAccountFlags          :: Word16
  , tbAccountTimestamp      :: Word64
  }
  deriving (Eq, Show)

instance Storable TBAccount where
    sizeOf _ = #{size tb_account_t}

    alignment _ = #{alignment tb_account_t}

    peek ptr = do
      tbAccountId             <- #{peek tb_account_t, id} ptr
      tbAccountDebitsPending  <- #{peek tb_account_t, debits_pending} ptr
      tbAccountDebitsPosted   <- #{peek tb_account_t, debits_posted} ptr
      tbAccountCreditsPending <- #{peek tb_account_t, credits_pending} ptr
      tbAccountCreditsPosted  <- #{peek tb_account_t, credits_posted} ptr
      tbAccountUserData128    <- #{peek tb_account_t, user_data_128} ptr
      tbAccountUserData64     <- #{peek tb_account_t, user_data_64} ptr
      tbAccountUserData32     <- #{peek tb_account_t, user_data_32} ptr
      tbAccountReserved       <- #{peek tb_account_t, reserved} ptr
      tbAccountLedger         <- #{peek tb_account_t, ledger} ptr
      tbAccountCode           <- #{peek tb_account_t, code} ptr
      tbAccountFlags          <- #{peek tb_account_t, flags} ptr
      tbAccountTimestamp      <- #{peek tb_account_t, timestamp} ptr
      pure TBAccount{..}

    poke ptr account = do
        #{poke tb_account_t, id} ptr account.tbAccountId
        #{poke tb_account_t, debits_pending} ptr account.tbAccountDebitsPending
        #{poke tb_account_t, debits_posted} ptr account.tbAccountDebitsPosted
        #{poke tb_account_t, credits_pending} ptr account.tbAccountCreditsPending
        #{poke tb_account_t, credits_posted} ptr account.tbAccountCreditsPosted
        #{poke tb_account_t, user_data_128} ptr account.tbAccountUserData128
        #{poke tb_account_t, user_data_64} ptr account.tbAccountUserData64
        #{poke tb_account_t, user_data_32} ptr account.tbAccountUserData32
        #{poke tb_account_t, reserved} ptr account.tbAccountReserved
        #{poke tb_account_t, ledger} ptr account.tbAccountLedger
        #{poke tb_account_t, code} ptr account.tbAccountCode
        #{poke tb_account_t, flags} ptr account.tbAccountFlags
        #{poke tb_account_t, timestamp} ptr account.tbAccountTimestamp

data CreateAccountResult = 
      Ok
    | LinkedEventFailed
    | LinkedEventChainOpen
    | ImportedEventExpected
    | ImportedEventNotExpected
    | TimestampMustBeZero
    | ImportedEventTimestampOutOfRange
    | ImportedEventTimestampMustNotAdvance
    | ReservedField
    | ReservedFlag
    | IdMustNotBeZero
    | IdMustNotBeIntMax
    | ExistsWithDifferentFlags
    | ExistsWithDifferentUserData128
    | ExistsWithDifferentUserData64
    | ExistsWithDifferentUserData32
    | ExistsWithDifferentLedger
    | ExistsWithDifferentCode
    | Exists
    | FlagsAreMutuallyExclusive
    | DebitsPendingMustBeZero
    | DebitsPostedMustBeZero
    | CreditsPendingMustBeZero
    | CreditsPostedMustBeZero
    | LedgerMustNotBeZero
    | CodeMustNotBeZero
    | ImportedEventTimestampMustNotRegress
    deriving (Eq, Show)

instance Enum CreateAccountResult where
    fromEnum Ok                                   = #const TB_CREATE_ACCOUNT_OK
    fromEnum LinkedEventFailed                    = #const TB_CREATE_ACCOUNT_LINKED_EVENT_FAILED
    fromEnum LinkedEventChainOpen                 = #const TB_CREATE_ACCOUNT_LINKED_EVENT_CHAIN_OPEN
    fromEnum ImportedEventExpected                = #const TB_CREATE_ACCOUNT_IMPORTED_EVENT_EXPECTED
    fromEnum ImportedEventNotExpected             = #const TB_CREATE_ACCOUNT_IMPORTED_EVENT_NOT_EXPECTED
    fromEnum TimestampMustBeZero                  = #const TB_CREATE_ACCOUNT_TIMESTAMP_MUST_BE_ZERO
    fromEnum ImportedEventTimestampOutOfRange     = #const TB_CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_OUT_OF_RANGE
    fromEnum ImportedEventTimestampMustNotAdvance = #const TB_CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_ADVANCE
    fromEnum ReservedField                        = #const TB_CREATE_ACCOUNT_RESERVED_FIELD
    fromEnum ReservedFlag                         = #const TB_CREATE_ACCOUNT_RESERVED_FLAG
    fromEnum IdMustNotBeZero                      = #const TB_CREATE_ACCOUNT_ID_MUST_NOT_BE_ZERO
    fromEnum IdMustNotBeIntMax                    = #const TB_CREATE_ACCOUNT_ID_MUST_NOT_BE_INT_MAX
    fromEnum ExistsWithDifferentFlags             = #const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_FLAGS
    fromEnum ExistsWithDifferentUserData128       = #const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_128
    fromEnum ExistsWithDifferentUserData64        = #const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_64
    fromEnum ExistsWithDifferentUserData32        = #const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_32
    fromEnum ExistsWithDifferentLedger            = #const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_LEDGER
    fromEnum ExistsWithDifferentCode              = #const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_CODE
    fromEnum Exists                               = #const TB_CREATE_ACCOUNT_EXISTS
    fromEnum FlagsAreMutuallyExclusive            = #const TB_CREATE_ACCOUNT_FLAGS_ARE_MUTUALLY_EXCLUSIVE
    fromEnum DebitsPendingMustBeZero              = #const TB_CREATE_ACCOUNT_DEBITS_PENDING_MUST_BE_ZERO
    fromEnum DebitsPostedMustBeZero               = #const TB_CREATE_ACCOUNT_DEBITS_POSTED_MUST_BE_ZERO
    fromEnum CreditsPendingMustBeZero             = #const TB_CREATE_ACCOUNT_CREDITS_PENDING_MUST_BE_ZERO
    fromEnum CreditsPostedMustBeZero              = #const TB_CREATE_ACCOUNT_CREDITS_POSTED_MUST_BE_ZERO
    fromEnum LedgerMustNotBeZero                  = #const TB_CREATE_ACCOUNT_LEDGER_MUST_NOT_BE_ZERO
    fromEnum CodeMustNotBeZero                    = #const TB_CREATE_ACCOUNT_CODE_MUST_NOT_BE_ZERO
    fromEnum ImportedEventTimestampMustNotRegress = #const TB_CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_REGRESS

    toEnum (#const TB_CREATE_ACCOUNT_OK)                                        = Ok
    toEnum (#const TB_CREATE_ACCOUNT_LINKED_EVENT_FAILED)                       = LinkedEventFailed
    toEnum (#const TB_CREATE_ACCOUNT_LINKED_EVENT_CHAIN_OPEN)                   = LinkedEventChainOpen
    toEnum (#const TB_CREATE_ACCOUNT_IMPORTED_EVENT_EXPECTED)                   = ImportedEventExpected
    toEnum (#const TB_CREATE_ACCOUNT_IMPORTED_EVENT_NOT_EXPECTED)               = ImportedEventNotExpected
    toEnum (#const TB_CREATE_ACCOUNT_TIMESTAMP_MUST_BE_ZERO)                    = TimestampMustBeZero
    toEnum (#const TB_CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_OUT_OF_RANGE)     = ImportedEventTimestampOutOfRange
    toEnum (#const TB_CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_ADVANCE) = ImportedEventTimestampMustNotAdvance
    toEnum (#const TB_CREATE_ACCOUNT_RESERVED_FIELD)                            = ReservedField
    toEnum (#const TB_CREATE_ACCOUNT_RESERVED_FLAG)                             = ReservedFlag
    toEnum (#const TB_CREATE_ACCOUNT_ID_MUST_NOT_BE_ZERO)                       = IdMustNotBeZero
    toEnum (#const TB_CREATE_ACCOUNT_ID_MUST_NOT_BE_INT_MAX)                    = IdMustNotBeIntMax
    toEnum (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_FLAGS)               = ExistsWithDifferentFlags
    toEnum (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_128)       = ExistsWithDifferentUserData128
    toEnum (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_64)        = ExistsWithDifferentUserData64
    toEnum (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_32)        = ExistsWithDifferentUserData32
    toEnum (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_LEDGER)              = ExistsWithDifferentLedger
    toEnum (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_CODE)                = ExistsWithDifferentCode
    toEnum (#const TB_CREATE_ACCOUNT_EXISTS)                                    = Exists
    toEnum (#const TB_CREATE_ACCOUNT_FLAGS_ARE_MUTUALLY_EXCLUSIVE)              = FlagsAreMutuallyExclusive
    toEnum (#const TB_CREATE_ACCOUNT_DEBITS_PENDING_MUST_BE_ZERO)               = DebitsPendingMustBeZero
    toEnum (#const TB_CREATE_ACCOUNT_DEBITS_POSTED_MUST_BE_ZERO)                = DebitsPostedMustBeZero
    toEnum (#const TB_CREATE_ACCOUNT_CREDITS_PENDING_MUST_BE_ZERO)              = CreditsPendingMustBeZero
    toEnum (#const TB_CREATE_ACCOUNT_CREDITS_POSTED_MUST_BE_ZERO)               = CreditsPostedMustBeZero
    toEnum (#const TB_CREATE_ACCOUNT_LEDGER_MUST_NOT_BE_ZERO)                   = LedgerMustNotBeZero
    toEnum (#const TB_CREATE_ACCOUNT_CODE_MUST_NOT_BE_ZERO)                     = CodeMustNotBeZero
    toEnum (#const TB_CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_REGRESS) = ImportedEventTimestampMustNotRegress
    toEnum unmatched                                                            = error $ "CreateAccountsResult.toEnum: Cannot match " ++ show unmatched

data TBCreateAccountsResult = TBCreateAccountsResult
    { tbCreateAccountsResultIndex :: Word32
    , tbCreateAccountsResultResult :: CreateAccountResult
    }
    deriving (Show, Eq)

instance Storable TBCreateAccountsResult  where
    sizeOf _ = #{size tb_create_accounts_result_t}

    alignment _ = #{alignment tb_create_accounts_result_t}

    peek ptr = do
      tbCreateAccountsResultIndex  <- #{peek tb_create_accounts_result_t, index} ptr
      tbCreateAccountsResultResult <- toEnum <$> #{peek tb_create_accounts_result_t, result} ptr
      pure TBCreateAccountsResult{..}

    poke ptr createAccountsResult = do
        #{poke tb_create_accounts_result_t, index} ptr createAccountsResult.tbCreateAccountsResultIndex
        #{poke tb_create_accounts_result_t, result} ptr (fromEnum createAccountsResult.tbCreateAccountsResultResult)
