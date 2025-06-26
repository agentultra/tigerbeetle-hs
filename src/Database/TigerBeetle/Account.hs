module Database.TigerBeetle.Account where

import Data.Set (Set)
import Data.WideWord

newtype AccountId = AccountId { getAccountId :: Word128 }
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
