{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

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

    peek ptr
      = TBAccount
      <$> #{peek tb_account_t, id} ptr
      <*> #{peek tb_account_t, debits_pending} ptr
      <*> #{peek tb_account_t, debits_posted} ptr
      <*> #{peek tb_account_t, credits_pending} ptr
      <*> #{peek tb_account_t, credits_posted} ptr
      <*> #{peek tb_account_t, user_data_128} ptr
      <*> #{peek tb_account_t, user_data_64} ptr
      <*> #{peek tb_account_t, user_data_32} ptr
      <*> #{peek tb_account_t, reserved} ptr
      <*> #{peek tb_account_t, ledger} ptr
      <*> #{peek tb_account_t, code} ptr
      <*> #{peek tb_account_t, flags} ptr
      <*> #{peek tb_account_t, timestamp} ptr

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
