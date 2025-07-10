{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Client.Sync where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Database.TigerBeetle.Account
import Database.TigerBeetle.Address
import Database.TigerBeetle.ClusterId
import Database.TigerBeetle.Internal.FFI.Client
import Database.TigerBeetle.Raw.Client qualified as Raw
import Database.TigerBeetle.Raw.Response
import Database.TigerBeetle.Client.Account qualified as Account
import Database.TigerBeetle.Client.Transfer qualified as Transfer
import Database.TigerBeetle.Transfer
import Foreign.ForeignPtr
import Foreign.Storable

data SyncState
  = SyncState
  { syncStateClientPtr :: Raw.ClientPtr
  , syncStateResultVar :: TVar (Maybe TBResponse)
  }

newtype SyncClientT m a = SyncClientT { getSyncClient :: ReaderT SyncState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader SyncState)

withClient :: MonadIO m => ClusterId -> Address -> SyncClientT m TBResponse -> m TBResponse
withClient clusterId address clientAction = do
  result <- liftIO $ newTVarIO Nothing
  cb <- liftIO $ Raw.makeCompletionCallback $ \_ tbPacketPtr _ resultDataPtr resultLen -> do
    tbPacket <- peek tbPacketPtr
    tbResponse <- decodeResponse tbPacket resultDataPtr $ fromIntegral resultLen
    liftIO . atomically $ writeTVar result (Just tbResponse)
  clientInitResult <- liftIO $ Raw.initClient clusterId address 0 cb
  case clientInitResult of
    Left err -> error $ show err
    Right clientPtr -> do
      let syncState
            = SyncState
            { syncStateClientPtr = clientPtr
            , syncStateResultVar = result
            }
      (`runReaderT` syncState) . getSyncClient $ clientAction

syncSubmit :: MonadIO m => (a -> IO (ForeignPtr TBPacket)) -> a -> SyncClientT m TBClientStatus
syncSubmit syncAction actionParam = do
  requestPacketPtr <- liftIO $ syncAction actionParam
  SyncState {..} <- ask
  liftIO $ withForeignPtr syncStateClientPtr $ \rawClient -> do
    withForeignPtr requestPacketPtr $ \rawPacket -> do
      tbClientSubmit rawClient rawPacket

createAccounts :: MonadIO m => [CreateAccount] -> SyncClientT m TBResponse
createAccounts createAccountParams = do
  status <- syncSubmit Account.createAccounts createAccountParams
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

lookupAccounts :: MonadIO m => [AccountId] -> SyncClientT m TBResponse
lookupAccounts ids = do
  status <- syncSubmit Account.lookupAccounts ids
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

getAccountBalances :: MonadIO m => [AccountBalances] -> SyncClientT m TBResponse
getAccountBalances balances = do
  status <- syncSubmit Account.getAccountBalances balances
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

getAccountTransfers :: MonadIO m => [AccountTransfers] -> SyncClientT m TBResponse
getAccountTransfers transfers = do
  status <- syncSubmit Account.getAccountTransfers transfers
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

queryAccounts :: MonadIO m => [AccountQuery] -> SyncClientT m TBResponse
queryAccounts accountQueries = do
  status <- syncSubmit Account.queryAccounts accountQueries
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

createTransfers :: MonadIO m => [CreateTransfer] -> SyncClientT m TBResponse
createTransfers transfers = do
  status <- syncSubmit Transfer.createTransfer transfers
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

queryTransfers :: MonadIO m => [TransferQuery] -> SyncClientT m TBResponse
queryTransfers transferQueries = do
  status <- syncSubmit Transfer.queryTransfers transferQueries
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

awaitResult :: MonadIO m => SyncClientT m TBResponse
awaitResult = do
  SyncState {..} <- ask
  mResult <- liftIO . atomically $ readTVar syncStateResultVar
  case mResult of
    Nothing  -> (liftIO $ threadDelay 2000) >> awaitResult
    Just pkt -> pure pkt
