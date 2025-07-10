module Database.TigerBeetle.Transfer where

import Data.Set (Set)
import Data.Word
import Data.WideWord
import Database.TigerBeetle.Account
import Database.TigerBeetle.Amount
import Database.TigerBeetle.Timestamp

newtype TransferId = TransferId { getTransferId :: Word128 }
  deriving (Eq, Show)

newtype TransferCode = TransferCode { getTransferCode :: Word16 }
  deriving (Eq, Show)

data TransferFlag
  = Linked
  | Pending
  | PostPending
  | VoidPending
  | BalancingDebit
  | BalancingCredit
  | ClosingDebit
  | ClosingCredit
  | Imported
  deriving (Bounded, Enum, Eq, Show)

data CreateTransfer
  = CreateTransfer
  { createTransferId              :: TransferId
  , createTransferDebitAccountId  :: AccountId
  , createTransferCreditAccountId :: AccountId
  , createTransferAmount          :: Amount
  , createTransferLedger          :: Int
  , createTransferCode            :: TransferCode
  , createTransferFlags           :: Set TransferFlag
  }
  deriving (Eq, Show)

data TransferQueryFlag = Reversed
  deriving (Bounded, Enum, Eq, Ord, Show)

data TransferQuery
  = TransferQuery
  { queryTransferLedger       :: Int
  , queryTransferCode         :: Int
  , queryTransferTimestampMin :: Timestamp
  , queryTransferTimestampMax :: Timestamp
  , queryTransferLimit        :: Int
  , queryTransferFlags        :: Set TransferQueryFlag
  }
  deriving (Eq, Show)
