module Database.TigerBeetle.Client.Async where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.TigerBeetle.Address
import Database.TigerBeetle.ClusterId
import Database.TigerBeetle.Raw.Client qualified as Raw

data AsyncState = AsyncState { asyncStateClientPtr :: Raw.ClientPtr }

newtype AsyncClientT m a = AsyncClientT { getAsyncClientT :: Reader AsyncState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AsyncState)

withClient :: MonadIO m => ClusterId -> Address -> AsyncClientT m TBResponse -> m TBResult
withClient = undefined

createAccounts :: MonadIO m => [CreateAccount] -> AsyncClientT m TBResponse
createAccounts = undefined

lookupAccounts :: MonadIO m => [AccountId] -> AsyncClientT m TBResponse
lookupAccounts = undefined

getAccountBalances :: MonadIO m => [AccountBalances] -> AsyncClientT m TBResponse
getAccountBalances = undefined

-- getAccountTransfers :: MonadIO m => [AccountTransfers] -> SyncClientT m TB= undefined
-- getAccountTransfers = undefined
--   SyncState {..} <- ask
--   status <- Raw.submit syncStateClientPtr Raw.getAccountTransfers transfers
--   case status of
--     ClientOk -> awaitResult
--     _ -> error $ show status

-- queryAccounts :: MonadIO m => [AccountQuery] -> SyncClientT m TBResponse
-- queryAccounts accountQueries = do
--   SyncState {..} <- ask
--   status <- Raw.submit syncStateClientPtr Raw.queryAccounts accountQueries
--   case status of
--     ClientOk -> awaitResult
--     _ -> error $ show status

-- createTransfers :: MonadIO m => [CreateTransfer] -> SyncClientT m TBResponse
-- createTransfers transfers = do
--   SyncState {..} <- ask
--   status <- Raw.submit syncStateClientPtr Raw.createTransfer transfers
--   case status of
--     ClientOk -> awaitResult
--     _ -> error $ show status

-- queryTransfers :: MonadIO m => [TransferQuery] -> SyncClientT m TBResponse
-- queryTransfers transferQueries = do
--   SyncState {..} <- ask
--   status <- Raw.submit syncStateClientPtr Raw.queryTransfers transferQueries
--   case status of
--     ClientOk -> awaitResult
--     _ -> error $ show status
