module Database.TigerBeetle.Account
  ( -- * View Types
    AccountId (..)
  , AccountCode (..)
  , Account (..)
  , AccountFlag (..)
  , AccountFlags (..)
  , AccountBalance (..)
  , AccountTransfers (..)
    -- * Command Parameter Types
  , CreateAccount (..)
    -- * Query Parameter Types
  , AccountBalances (..)
  , AccountQuery (..)
  , AccountQueryFlag (..)
  )
where

import Data.Set (Set)
import Data.Word
import Data.WideWord
import Database.TigerBeetle.Code
import Database.TigerBeetle.Ledger
import Database.TigerBeetle.Timestamp

-- | Identify an 'Account' in the Tigerbeetle database
newtype AccountId = AccountId { getAccountId :: Word128 }
  deriving (Eq, Show)

-- | Classifies an account
--
-- Use these to distinguish "settlement" accounts from "customer"
-- accounts, etc.
newtype AccountCode = AccountCode { getAccountCode :: Word16 }
  deriving (Eq, Show)

data AccountFlags
  = Linked
  | DebitsMustNotExceedCredits
  | CreditsMustNotExceedDebits
  | History
  | Imported
  | Closed
  deriving (Eq, Ord, Show)

-- | A TigerBeetle account is a summary of the ledger of events on the
-- account.
data Account
  = Account
  { accountId             :: AccountId
  , accountDebitsPending  :: Integer
  , accountDebitsPosted   :: Integer
  , accountCreditsPending :: Integer
  , accountCreditsPosted  :: Integer
  , accountLedger         :: LedgerId
  , accountCode           :: Code
  , accountFlags          :: Set AccountFlags
  , accountTimestamp      :: Timestamp
  }
  deriving (Eq, Show)

-- | The result from the account balance query
data AccountBalance = AccountBalance
    { accountBalanceDebitsPending  :: Integer
    , accountBalanceDebitsPosted   :: Integer
    , accountBalanceCreditsPending :: Integer
    , accountBalanceCreditsPosted  :: Integer
    , accountBalanceTimestamp      :: Timestamp
    }
    deriving (Show, Eq)

-- | 'Account' creation parameters to pass to the create account
-- command.
data CreateAccount = CreateAccount
  { createAccountId     :: AccountId
  , createAccountLedger :: LedgerId
  , createAccountCode   :: AccountCode
  }
  deriving (Eq, Show)

data AccountFlag = AccountCredits | AccountDebits | AccountReversed
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Parameters for the account balances query
data AccountBalances = AccountBalances
  { balancesAccountId :: AccountId
  , balancesFlags     :: Set AccountFlag
  , balancesLimit     :: Int
  }
  deriving (Eq, Show)

-- | Parameters for the account transfers query
data AccountTransfers = AccountTransfers
  { transfersAccountId :: AccountId
  , transfersFlags     :: Set AccountFlag
  , transfersLimit     :: Int
  }
  deriving (Eq, Show)

data AccountQueryFlag = AccountQueryReversed
  deriving (Eq, Ord, Show)

-- | Parameters for the accounts query
data AccountQuery = AccountQuery
  { accountQueryLedger       :: LedgerId
  , accountQueryCode         :: AccountCode
  , accountQueryTimestampMin :: Timestamp
  , accountQueryTimestampMax :: Timestamp
  , accountQueryLimit        :: Int
  , accountQueryFlags        :: Set AccountQueryFlag
  }
  deriving (Eq, Show)
