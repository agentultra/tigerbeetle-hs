module Database.TigerBeetle.Account where

import Data.Set (Set)
import Data.Word
import Data.WideWord
import Database.TigerBeetle.Code
import Database.TigerBeetle.Ledger
import Database.TigerBeetle.Timestamp

newtype AccountId = AccountId { getAccountId :: Word128 }
  deriving (Eq, Show)

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

data AccountBalance = AccountBalance
    { accountBalanceDebitsPending  :: Integer
    , accountBalanceDebitsPosted   :: Integer
    , accountBalanceCreditsPending :: Integer
    , accountBalanceCreditsPosted  :: Integer
    , accountBalanceTimestamp      :: Timestamp
    }
    deriving (Show, Eq)

data CreateAccount = CreateAccount
  { createAccountId     :: AccountId
  , createAccountLedger :: LedgerId
  , createAccountCode   :: AccountCode
  }
  deriving (Eq, Show)

data AccountFlag = AccountCredits | AccountDebits | AccountReversed
  deriving (Bounded, Enum, Eq, Ord, Show)

data AccountBalances = AccountBalances
  { balancesAccountId :: AccountId
  , balancesFlags     :: Set AccountFlag
  , balancesLimit     :: Int
  }
  deriving (Eq, Show)

data AccountTransfers = AccountTransfers
  { transfersAccountId :: AccountId
  , transfersFlags     :: Set AccountFlag
  , transfersLimit     :: Int
  }
  deriving (Eq, Show)

data AccountQueryFlag = AccountQueryReversed
  deriving (Eq, Ord, Show)

data AccountQuery = AccountQuery
  { accountQueryLedger       :: LedgerId
  , accountQueryCode         :: AccountCode
  , accountQueryTimestampMin :: Timestamp
  , accountQueryTimestampMax :: Timestamp
  , accountQueryLimit        :: Int
  , accountQueryFlags        :: Set AccountQueryFlag
  }
  deriving (Eq, Show)
