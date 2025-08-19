{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Response
  ( Response (..)
  -- * Internal functions
  , toResponse
  )
where

import Data.Set qualified as Set
import Database.TigerBeetle.Account
import Database.TigerBeetle.Code
import Database.TigerBeetle.Internal.FFI.Account qualified as FFA
import Database.TigerBeetle.Internal.FFI.Transfer qualified as FFT
import Database.TigerBeetle.Ledger
import Database.TigerBeetle.Raw.Response qualified as Raw
import Database.TigerBeetle.Response.Account (CreateAccountsResult)
import Database.TigerBeetle.Response.Account qualified as RA
import Database.TigerBeetle.Response.Transfer (CreateTransfersResult)
import Database.TigerBeetle.Response.Transfer qualified as TA
import Database.TigerBeetle.Timestamp
import Database.TigerBeetle.Transfer (Transfer (..), TransferId (..))
import Database.TigerBeetle.Transfer qualified as T

-- | The responses from each of the database commands.
data Response
  = CreateAccountResultResponse [CreateAccountsResult]
  | CreateTransferResultResponse [CreateTransfersResult]
  | LookupAccountsResponse [Account]
  | LookupTransfersResponse [Transfer]
  | GetAccountTransfersResponse [Transfer]
  | GetAccountBalancesResponse [AccountBalance]
  | QueryAccountsResponse [Account]
  | QueryTransfersResponse [Transfer]
  deriving (Eq, Show)

-- TODO: Move to an internal module
toResponse :: Raw.TBResponse -> Response
toResponse = \case
  Raw.CreateAccountResultResponse xs ->
    CreateAccountResultResponse $ map toCreateAccountsResult xs
  Raw.CreateTransferResultResponse xs ->
    CreateTransferResultResponse $ map toCreateTransfersResult xs
  Raw.LookupAccountsResponse xs ->
    LookupAccountsResponse $ map toAccount xs
  Raw.LookupTransfersResponse xs ->
    LookupTransfersResponse $ map toTransfer xs
  Raw.GetAccountTransfersResponse xs ->
    GetAccountTransfersResponse $ map toTransfer xs
  Raw.GetAccountBalancesResponse xs ->
    GetAccountBalancesResponse $ map toAccountBalance xs
  Raw.QueryAccountsResponse xs ->
    QueryAccountsResponse $ map toAccount xs
  Raw.QueryTransfersResponse xs ->
    QueryTransfersResponse $ map toTransfer xs

toAccount :: FFA.TBAccount -> Account
toAccount FFA.TBAccount {..}
  = Account
  { accountId             = AccountId tbAccountId
  , accountDebitsPending  = fromIntegral tbAccountDebitsPending
  , accountDebitsPosted   = fromIntegral tbAccountDebitsPosted
  , accountCreditsPending = fromIntegral tbAccountCreditsPending
  , accountCreditsPosted  = fromIntegral tbAccountCreditsPosted
  , accountLedger         = LedgerId tbAccountLedger
  , accountCode           = Code tbAccountCode
  , accountFlags          = Set.map toAccountFlag tbAccountFlags
  , accountTimestamp      = Timestamp tbAccountTimestamp
  }

toAccountFlag :: FFA.TBAccountFlags -> AccountFlags
toAccountFlag = \case
  FFA.Linked                     -> Linked
  FFA.DebitsMustNotExceedCredits -> DebitsMustNotExceedCredits
  FFA.CreditsMustNotExceedDebits -> CreditsMustNotExceedDebits
  FFA.History                    -> History
  FFA.Imported                   -> Imported
  FFA.Closed                     -> Closed

toCreateAccountsResult :: FFA.TBCreateAccountsResult -> CreateAccountsResult
toCreateAccountsResult FFA.TBCreateAccountsResult {..} =
  case tbCreateAccountsResultResult of
    FFA.Ok                                   -> RA.Ok
    FFA.LinkedEventFailed                    -> RA.LinkedEventFailed
    FFA.LinkedEventChainOpen                 -> RA.LinkedEventChainOpen
    FFA.ImportedEventExpected                -> RA.ImportedEventExpected
    FFA.ImportedEventNotExpected             -> RA.ImportedEventNotExpected
    FFA.TimestampMustBeZero                  -> RA.TimestampMustBeZero
    FFA.ImportedEventTimestampOutOfRange     -> RA.ImportedEventTimestampOutOfRange
    FFA.ImportedEventTimestampMustNotAdvance -> RA.ImportedEventTimestampMustNotAdvance
    FFA.ReservedField                        -> RA.ReservedField
    FFA.ReservedFlag                         -> RA.ReservedFlag
    FFA.IdMustNotBeZero                      -> RA.IdMustNotBeZero
    FFA.IdMustNotBeIntMax                    -> RA.IdMustNotBeIntMax
    FFA.ExistsWithDifferentFlags             -> RA.ExistsWithDifferentFlags
    FFA.ExistsWithDifferentUserData128       -> RA.ExistsWithDifferentUserData128
    FFA.ExistsWithDifferentUserData64        -> RA.ExistsWithDifferentUserData64
    FFA.ExistsWithDifferentUserData32        -> RA.ExistsWithDifferentUserData32
    FFA.ExistsWithDifferentLedger            -> RA.ExistsWithDifferentLedger
    FFA.ExistsWithDifferentCode              -> RA.ExistsWithDifferentCode
    FFA.Exists                               -> RA.Exists
    FFA.FlagsAreMutuallyExclusive            -> RA.FlagsAreMutuallyExclusive
    FFA.DebitsPendingMustBeZero              -> RA.DebitsPendingMustBeZero
    FFA.DebitsPostedMustBeZero               -> RA.DebitsPostedMustBeZero
    FFA.CreditsPendingMustBeZero             -> RA.CreditsPendingMustBeZero
    FFA.CreditsPostedMustBeZero              -> RA.CreditsPostedMustBeZero
    FFA.LedgerMustNotBeZero                  -> RA.LedgerMustNotBeZero
    FFA.CodeMustNotBeZero                    -> RA.CodeMustNotBeZero
    FFA.ImportedEventTimestampMustNotRegress -> RA.ImportedEventTimestampMustNotRegress

toCreateTransfersResult :: FFT.TBCreateTransfersResult -> CreateTransfersResult
toCreateTransfersResult FFT.TBCreateTransfersResult {..} =
  case tbCreateTransfersResultResult of
    FFT.Ok                                              -> TA.Ok
    FFT.LinkedEventFailed                               -> TA.LinkedEventFailed
    FFT.LinkedEventChainOpen                            -> TA.LinkedEventChainOpen
    FFT.ImportedEventExpected                           -> TA.ImportedEventExpected
    FFT.ImportedEventNotExpected                        -> TA.ImportedEventNotExpected
    FFT.TimestampMustBeZero                             -> TA.TimestampMustBeZero
    FFT.ImportedEventTimestampOutOfRange                -> TA.ImportedEventTimestampOutOfRange
    FFT.ImportedEventTimestampMustNotAdvance            -> TA.ImportedEventTimestampMustNotAdvance
    FFT.ReservedFlag                                    -> TA.ReservedFlag
    FFT.IdMustNotBeZero                                 -> TA.IdMustNotBeZero
    FFT.IdMustNotBeIntMax                               -> TA.IdMustNotBeIntMax
    FFT.ExistsWithDifferentFlags                        -> TA.ExistsWithDifferentFlags
    FFT.ExistsWithDifferentPendingId                    -> TA.ExistsWithDifferentPendingId
    FFT.ExistsWithDifferentTimeout                      -> TA.ExistsWithDifferentTimeout
    FFT.ExistsWithDifferentDebitAccountId               -> TA.ExistsWithDifferentDebitAccountId
    FFT.ExistsWithDifferentCreditAccountId              -> TA.ExistsWithDifferentCreditAccountId
    FFT.ExistsWithDifferentAmount                       -> TA.ExistsWithDifferentAmount
    FFT.ExistsWithDifferentUserData128                  -> TA.ExistsWithDifferentUserData128
    FFT.ExistsWithDifferentUserData64                   -> TA.ExistsWithDifferentUserData64
    FFT.ExistsWithDifferentUserData32                   -> TA.ExistsWithDifferentUserData32
    FFT.ExistsWithDifferentLedger                       -> TA.ExistsWithDifferentLedger
    FFT.ExistsWithDifferentCode                         -> TA.ExistsWithDifferentCode
    FFT.Exists                                          -> TA.Exists
    FFT.IdAlreadyFailed                                 -> TA.IdAlreadyFailed
    FFT.FlagsAreMutuallyExclusive                       -> TA.FlagsAreMutuallyExclusive
    FFT.DebitAccountIdMustNotBeZero                     -> TA.DebitAccountIdMustNotBeZero
    FFT.DebitAccountIdMustNotBeIntMax                   -> TA.DebitAccountIdMustNotBeIntMax
    FFT.CreditAccountIdMustNotBeZero                    -> TA.CreditAccountIdMustNotBeZero
    FFT.CreditAccountIdMustNotBeIntMax                  -> TA.CreditAccountIdMustNotBeIntMax
    FFT.AccountsMustBeDifferent                         -> TA.AccountsMustBeDifferent
    FFT.PendingIdMustBeZero                             -> TA.PendingIdMustBeZero
    FFT.PendingIdMustNotBeZero                          -> TA.PendingIdMustNotBeZero
    FFT.PendingIdMustNotBeIntMax                        -> TA.PendingIdMustNotBeIntMax
    FFT.PendingIdMustBeDifferent                        -> TA.PendingIdMustBeDifferent
    FFT.TimeoutReservedForPendingTransfer               -> TA.TimeoutReservedForPendingTransfer
    FFT.ClosingTransferMustBePending                    -> TA.ClosingTransferMustBePending
    FFT.LedgerMustNotBeZero                             -> TA.LedgerMustNotBeZero
    FFT.CodeMustNotBeZero                               -> TA.CodeMustNotBeZero
    FFT.DebitAccountNotFound                            -> TA.DebitAccountNotFound
    FFT.CreditAccountNotFound                           -> TA.CreditAccountNotFound
    FFT.AccountsMustHaveTheSameLedger                   -> TA.AccountsMustHaveTheSameLedger
    FFT.TransferMustHaveTheSameLedgerAsAccounts         -> TA.TransferMustHaveTheSameLedgerAsAccounts
    FFT.PendingTransferNotFound                         -> TA.PendingTransferNotFound
    FFT.PendingTransferNotPending                       -> TA.PendingTransferNotPending
    FFT.PendingTransferHasDifferentDebitAccountId       -> TA.PendingTransferHasDifferentDebitAccountId
    FFT.PendingTransferHasDifferentCreditAccountId      -> TA.PendingTransferHasDifferentCreditAccountId
    FFT.PendingTransferHasDifferentLedger               -> TA.PendingTransferHasDifferentLedger
    FFT.PendingTransferHasDifferentCode                 -> TA.PendingTransferHasDifferentCode
    FFT.ExceedsPendingTransferAmount                    -> TA.ExceedsPendingTransferAmount
    FFT.PendingTransferHasDifferentAmount               -> TA.PendingTransferHasDifferentAmount
    FFT.PendingTransferAlreadyPosted                    -> TA.PendingTransferAlreadyPosted
    FFT.PendingTransferAlreadyVoided                    -> TA.PendingTransferAlreadyVoided
    FFT.PendingTransferExpired                          -> TA.PendingTransferExpired
    FFT.ImportedEventTimestampMustNotRegress            -> TA.ImportedEventTimestampMustNotRegress
    FFT.ImportedEventTimestampMustPostdateDebitAccount  -> TA.ImportedEventTimestampMustPostdateDebitAccount
    FFT.ImportedEventTimestampMustPostdateCreditAccount -> TA.ImportedEventTimestampMustPostdateCreditAccount
    FFT.ImportedEventTimeoutMustBeZero                  -> TA.ImportedEventTimeoutMustBeZero
    FFT.DebitAccountAlreadyClosed                       -> TA.DebitAccountAlreadyClosed
    FFT.CreditAccountAlreadyClosed                      -> TA.CreditAccountAlreadyClosed
    FFT.OverflowsDebitsPending                          -> TA.OverflowsDebitsPending
    FFT.OverflowsCreditsPending                         -> TA.OverflowsCreditsPending
    FFT.OverflowsDebitsPosted                           -> TA.OverflowsDebitsPosted
    FFT.OverflowsCreditsPosted                          -> TA.OverflowsCreditsPosted
    FFT.OverflowsDebits                                 -> TA.OverflowsDebits
    FFT.OverflowsCredits                                -> TA.OverflowsCredits
    FFT.OverflowsTimeout                                -> TA.OverflowsTimeout
    FFT.ExceedsCredits                                  -> TA.ExceedsCredits
    FFT.ExceedsDebits                                   -> TA.ExceedsDebits

toTransfer :: FFT.TBTransfer -> Transfer
toTransfer FFT.TBTransfer {..}
  = Transfer
  { transferId = TransferId tbTransferId
  , transferDebitAccountId = AccountId tbTransferDebitAccountId
  , transferCreditAccountId = AccountId tbTransferCreditAccountId
  , transferAmount = fromIntegral tbTransferAmount
  , transferPendingId = TransferId tbTransferPendingId
  , transferTimeout = fromIntegral tbTransferTimeout
  , transferLedger = LedgerId tbTransferLedger
  , transferCode = Code tbTransferCode
  , transferFlags = Set.map toTransferFlag tbTransferFlags
  , transferTimestamp = Timestamp tbTransferTimestamp
  }

toTransferFlag :: FFT.TBTransferFlag -> T.TransferFlag
toTransferFlag = \case
  FFT.Linked -> T.Linked
  FFT.Pending -> T.Pending
  FFT.PostPendingTransfer -> T.PostPending
  FFT.VoidPendingTransfer -> T.VoidPending
  FFT.BalancingDebit -> T.BalancingDebit
  FFT.BalancingCredit -> T.BalancingCredit
  FFT.ClosingDebit -> T.ClosingDebit
  FFT.ClosingCredit -> T.ClosingCredit
  FFT.Imported -> T.Imported

toAccountBalance :: FFA.TBAccountBalance -> AccountBalance
toAccountBalance FFA.TBAccountBalance {..}
  = AccountBalance
  { accountBalanceDebitsPending  = fromIntegral tbAccountBalanceDebitsPending
  , accountBalanceDebitsPosted   = fromIntegral tbAccountBalanceDebitsPosted
  , accountBalanceCreditsPending = fromIntegral tbAccountBalanceCreditsPending
  , accountBalanceCreditsPosted  = fromIntegral tbAccountBalanceCreditsPosted
  , accountBalanceTimestamp      = Timestamp tbAccountBalanceTimestamp
  }
