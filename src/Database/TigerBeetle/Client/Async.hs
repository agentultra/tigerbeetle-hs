{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Client.Async where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Word
import Database.TigerBeetle.Account
import Database.TigerBeetle.Address
import Database.TigerBeetle.ClusterId
import Database.TigerBeetle.Internal.FFI.Client
import Database.TigerBeetle.Raw.Account qualified as Raw
import Database.TigerBeetle.Raw.Response (TBResponse)
import Database.TigerBeetle.Raw.Response qualified as Raw
import Database.TigerBeetle.Raw.Client qualified as Raw
import Database.TigerBeetle.Raw.Transfer qualified as Raw
import Database.TigerBeetle.Transfer
import Foreign.C.Types
import Foreign.Storable

data AsyncState = AsyncState { asyncStateClientPtr :: Raw.ClientPtr }

newtype AsyncClientT m a = AsyncClientT { getAsyncClient :: ReaderT AsyncState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AsyncState)

newtype ThreadContext = ThreadContext { getThreadContext :: Word64 }

withClient :: MonadIO m => ClusterId -> Address -> ThreadContext -> (ThreadContext -> TBResponse -> IO ()) -> AsyncClientT m () -> m ()
withClient clusterId address (ThreadContext userCtxt) callback clientAction = do
  cb <- liftIO $ Raw.makeCompletionCallback $ \(CUIntPtr ctx) tbPacketPtr _ resultDataPtr resultLen -> do
    tbPacket <- peek tbPacketPtr
    tbResponse <- Raw.decodeResponse tbPacket resultDataPtr $ fromIntegral resultLen
    callback (ThreadContext ctx) tbResponse
  clientInitResult <- liftIO $ Raw.initClient clusterId address (CUIntPtr userCtxt) cb
  case clientInitResult of
    Left err -> error $ show err
    Right clientPtr -> do
      let asyncState = AsyncState { asyncStateClientPtr = clientPtr }
      (`runReaderT` asyncState) . getAsyncClient $ clientAction

createAccounts :: MonadIO m => [CreateAccount] -> AsyncClientT m ()
createAccounts createAccountParams = do
  AsyncState {..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.createAccounts createAccountParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status

lookupAccounts :: MonadIO m => [AccountId] -> AsyncClientT m ()
lookupAccounts lookupAccountParams = do
  AsyncState {..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.lookupAccounts lookupAccountParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status

getAccountBalances :: MonadIO m => [AccountBalances] -> AsyncClientT m ()
getAccountBalances accountBalanceParams = do
  AsyncState {..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.getAccountBalances accountBalanceParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status

getAccountTransfers :: MonadIO m => [AccountTransfers] -> AsyncClientT m ()
getAccountTransfers accountTransferParams = do
  AsyncState {..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.getAccountTransfers accountTransferParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status

queryAccounts :: MonadIO m => [AccountQuery] -> AsyncClientT m ()
queryAccounts queryAccountParams = do
  AsyncState {..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.queryAccounts queryAccountParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status

createTransfers :: MonadIO m => [CreateTransfer] -> AsyncClientT m ()
createTransfers transferParams = do
  AsyncState {..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.createTransfer transferParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status

queryTransfers :: MonadIO m => [TransferQuery] -> AsyncClientT m ()
queryTransfers transferQueryParams = do
  AsyncState {..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.queryTransfers transferQueryParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status
