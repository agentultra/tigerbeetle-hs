module Database.TigerBeetle.Response.Account where

data CreateAccountsResult
  = Ok
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
