{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Client.Sync where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Database.TigerBeetle.Address
import Database.TigerBeetle.ClusterId
import Database.TigerBeetle.Raw.Client qualified as Raw
import Database.TigerBeetle.Raw.Response
import Database.TigerBeetle.Client.Account qualified as Account
import Database.TigerBeetle.Internal.FFI.Client
import Foreign.ForeignPtr
import Foreign.Storable

data SyncState
  = SyncState
  { syncStateClientPtr :: Raw.ClientPtr
  , syncStateResultVar :: TVar (Maybe (Either DecodeResponseError TBResponse))
  }

newtype SyncClientT m a = SyncClientT { getSyncClient :: ReaderT SyncState m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader SyncState)

withClient :: MonadIO m => ClusterId -> Address -> SyncClientT m TBResponse -> m TBResponse
withClient clusterId address clientAction = do
  result <- liftIO $ newTVarIO Nothing
  cb <- liftIO $ Raw.makeCompletionCallback $ \_ packetPtr _ _ _ -> do
    packet <- peek packetPtr
    tbResponse <- parseResults packet
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
  where
    parseResults :: MonadIO m => TBPacket -> m (Either DecodeResponseError TBResponse)
    parseResults pkt = do
      -- TODO: remove `Either` result type unless we catch the IO exception?
      result <- liftIO $ decodeResponse pkt
      pure $ Right result

createAccounts :: MonadIO m => [Account.CreateAccount] -> SyncClientT m TBResponse
createAccounts createAccountParams = do
  SyncState {..} <- ask
  requestPacketPtr <- liftIO $ Account.createAccounts createAccountParams
  status <- liftIO $ withForeignPtr syncStateClientPtr $ \rawClient -> do
    withForeignPtr requestPacketPtr $ \rawPacket -> do
      tbClientSubmit rawClient rawPacket
  case status of
    ClientOk -> do
      result <- awaitResult
      case result of
        Left err -> error $ "createAccounts result error: " ++ show err
        Right response -> pure response
    _ -> error $ show status

awaitResult :: MonadIO m => SyncClientT m (Either DecodeResponseError TBResponse)
awaitResult = do
  SyncState {..} <- ask
  mResult <- liftIO . atomically $ readTVar syncStateResultVar
  case mResult of
    Nothing  -> (liftIO $ threadDelay 2000) >> awaitResult
    Just pkt -> pure pkt
