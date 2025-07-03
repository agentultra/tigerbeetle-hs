module Database.TigerBeetle.Transfer where

import Data.Set (Set)
import Data.WideWord
import Database.TigerBeetle.Account
import Database.TigerBeetle.Amount

newtype TransferId = TransferId { getTransferId :: Word128 }

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
  { createTransferId :: TransferId
  , createTransferDebitAccountId :: AccountId
  , createTransferCreditAccountId :: AccountId
  , createTransferAmount :: Amount
  , createTransferLedger :: Int
  , createTransferCode :: Int
  , createTransferFlags :: Set TransferFlag
  }
