{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Client.Sync
  ( -- * Connecting
    withClient
    -- * Commands
  , createAccounts
  , createTransfers
    -- * Queries
  , lookupAccounts
  , getAccountBalances
  , getAccountTransfers
  , queryAccounts
  , queryTransfers
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.TigerBeetle.Account
import Database.TigerBeetle.Address
import Database.TigerBeetle.ClusterId
import Database.TigerBeetle.Internal.FFI.Client
import Database.TigerBeetle.Raw.Account qualified as Raw
import Database.TigerBeetle.Raw.Client qualified as Raw
import Database.TigerBeetle.Raw.Response
import Database.TigerBeetle.Raw.Transfer qualified as Raw
import Database.TigerBeetle.Response
import Database.TigerBeetle.Transfer
import Foreign.Storable

data SyncState = SyncState
  { syncStateClientPtr :: Raw.ClientPtr
  , syncStateResultVar :: TVar (Maybe TBResponse)
  }

newtype SyncClientT m a = SyncClientT {getSyncClient :: ReaderT SyncState m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader SyncState)

-- | Initializes a TigerBeetle client connection to accept commands.
--
-- Commands are blocking and execute synchronously, each one awaiting
-- the response from the server.
withClient :: (MonadIO m) => ClusterId -> Address -> SyncClientT m Response -> m Response
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
      let syncState =
            SyncState
              { syncStateClientPtr = clientPtr
              , syncStateResultVar = result
              }
      (`runReaderT` syncState) . getSyncClient $ clientAction

createAccounts :: (MonadIO m) => [CreateAccount] -> SyncClientT m Response
createAccounts createAccountParams = do
  SyncState{..} <- ask
  status <- Raw.submit syncStateClientPtr Raw.createAccounts createAccountParams
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

lookupAccounts :: (MonadIO m) => [AccountId] -> SyncClientT m Response
lookupAccounts ids = do
  SyncState{..} <- ask
  status <- Raw.submit syncStateClientPtr Raw.lookupAccounts ids
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

getAccountBalances :: (MonadIO m) => [AccountBalances] -> SyncClientT m Response
getAccountBalances balances = do
  SyncState{..} <- ask
  status <- Raw.submit syncStateClientPtr Raw.getAccountBalances balances
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

getAccountTransfers :: (MonadIO m) => [AccountTransfers] -> SyncClientT m Response
getAccountTransfers transfers = do
  SyncState{..} <- ask
  status <- Raw.submit syncStateClientPtr Raw.getAccountTransfers transfers
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

-- | Query accounts by the intersection of some fields and time
-- ranges.
--
-- It is not possible to query more than 8189 accounts
-- atomically. When issuing multiple queries (eg: when paginating the
-- full result set) it can happen that other operations may be
-- interleaved leading to read skew.
--
-- Note that this can be worked around with a flag in more recent
-- versions of Tigerbeetle.
queryAccounts :: (MonadIO m) => [AccountQuery] -> SyncClientT m Response
queryAccounts accountQueries = do
  SyncState{..} <- ask
  status <- Raw.submit syncStateClientPtr Raw.queryAccounts accountQueries
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

createTransfers :: (MonadIO m) => [CreateTransfer] -> SyncClientT m Response
createTransfers transfers = do
  SyncState{..} <- ask
  status <- Raw.submit syncStateClientPtr Raw.createTransfers transfers
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

-- | Query transfers by the intersection of some fields and time
-- ranges.
--
-- It is not possible to query more than 8189 transfers
-- atomically. When issuing multiple queries (eg: when paginating the
-- full result set) it can happen that other operations may be
-- interleaved leading to read skew.
--
-- Note that this can be worked around with a flag in more recent
-- versions of Tigerbeetle.
queryTransfers :: (MonadIO m) => [TransferQuery] -> SyncClientT m Response
queryTransfers transferQueries = do
  SyncState{..} <- ask
  status <- Raw.submit syncStateClientPtr Raw.queryTransfers transferQueries
  case status of
    ClientOk -> awaitResult
    _ -> error $ show status

awaitResult :: (MonadIO m) => SyncClientT m Response
awaitResult = do
  SyncState{..} <- ask
  mResult <- liftIO (readTVarIO syncStateResultVar)
  case mResult of
    Nothing -> liftIO (threadDelay 2000) >> awaitResult
    Just pkt -> pure $ toResponse pkt
