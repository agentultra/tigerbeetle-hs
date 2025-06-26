module Database.TigerBeetle.Account where

import Data.Set (Set)
import Data.Word
import Data.WideWord
import Database.TigerBeetle.Timestamp

newtype AccountId = AccountId { getAccountId :: Word128 }
  deriving (Eq, Show)

newtype AccountCode = AccountCode { getAccountCode :: Word16 }
  deriving (Eq, Show)

data CreateAccount = CreateAccount
  { createAccountId     :: AccountId
  , createAccountLedger :: Int
  , createAccountCode   :: Int
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
  { accountQueryLedger       :: Int
  , accountQueryCode         :: AccountCode
  , accountQueryTimestampMin :: Timestamp
  , accountQueryTimestampMax :: Timestamp
  , accountQueryLimit        :: Int
  , accountQueryFlags        :: Set AccountQueryFlag
  }
  deriving (Eq, Show)
