module Database.TigerBeetle.Raw.Transfer where

import Database.TigerBeetle.Internal.FFI.Transfer

zeroTBTransfer :: IO TBTransfer
zeroTBTransfer = pure $ TBTransfer
  { tbTransferId = 0
  , tbTransferDebitAccountId = 0
  , tbTransferCreditAccountId = 0
  , tbTransferAmount = 0
  , tbTransferPendingId = 0
  , tbTransferUserData128 = 0
  , tbTransferUserData64 = 0
  , tbTransferUserData32 = 0
  , tbTransferTimeout = 0
  , tbTransferLedger = 0
  , tbTransferCode = 0
  , tbTransferFlags = mempty
  , tbTransferTimestamp = 0
  }
