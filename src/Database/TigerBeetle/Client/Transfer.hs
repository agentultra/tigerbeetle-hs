{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Client.Transfer where

import Control.Monad.IO.Class
import Data.Set qualified as Set
import Database.TigerBeetle.Account
import Database.TigerBeetle.Amount
import Database.TigerBeetle.Raw.Transfer qualified as Raw
import Database.TigerBeetle.Transfer
import Database.TigerBeetle.Internal.FFI.Client
import Foreign.ForeignPtr

createTransfer :: [CreateTransfer] -> IO (ForeignPtr TBPacket)
createTransfer transfers = do
  tbTransfers <- liftIO $ mapM createTBTransfer transfers
  tbPacketPtr <- liftIO $ Raw.createTransfersPacket tbTransfers
  liftIO $ newForeignPtr_ tbPacketPtr
  where
    createTBTransfer :: CreateTransfer -> IO Raw.TBTransfer
    createTBTransfer CreateTransfer {..} = do
      tbTransfer <- Raw.zeroTBTransfer
      pure $
        tbTransfer
          { Raw.tbTransferId = getTransferId createTransferId
          , Raw.tbTransferDebitAccountId = getAccountId createTransferDebitAccountId
          , Raw.tbTransferCreditAccountId = getAccountId createTransferCreditAccountId
          , Raw.tbTransferAmount = getAmount createTransferAmount
          , Raw.tbTransferPendingId = 0
          , Raw.tbTransferUserData128 = 0
          , Raw.tbTransferUserData64 = 0
          , Raw.tbTransferUserData32 = 0
          , Raw.tbTransferTimeout = 100
          , Raw.tbTransferLedger = fromIntegral createTransferLedger
          , Raw.tbTransferCode = getTransferCode createTransferCode
          , Raw.tbTransferFlags = toRawTransferFlags `Set.map` createTransferFlags
          , Raw.tbTransferTimestamp = 0
          }
      where
        toRawTransferFlags :: TransferFlag -> Raw.TBTransferFlag
        toRawTransferFlags = \case
          Linked -> Raw.Linked
          Pending -> Raw.Pending
          PostPending -> Raw.PostPendingTransfer
          VoidPending -> Raw.VoidPendingTransfer
          BalancingDebit -> Raw.BalancingDebit
          BalancingCredit -> Raw.BalancingCredit
          ClosingDebit -> Raw.ClosingDebit
          ClosingCredit -> Raw.ClosingCredit
          Imported -> Raw.Imported

queryTransfers :: [TransferQuery] -> IO (ForeignPtr TBPacket)
queryTransfers transferQueries = do
  tbPacketPtr <- liftIO $ Raw.queryTransfersPacket transferQueries
  liftIO $ newForeignPtr_ tbPacketPtr
