{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Client.Async
  ( -- * Types
    ThreadContext (..)
    -- * Connecting
  , withClient
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

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Word
import Database.TigerBeetle.Account
import Database.TigerBeetle.Address
import Database.TigerBeetle.ClusterId
import Database.TigerBeetle.Internal.FFI.Client
import Database.TigerBeetle.Raw.Account qualified as Raw
import Database.TigerBeetle.Raw.Client qualified as Raw
import Database.TigerBeetle.Raw.Response qualified as Raw
import Database.TigerBeetle.Raw.Transfer qualified as Raw
import Database.TigerBeetle.Response
import Database.TigerBeetle.Transfer
import Foreign.C.Types
import Foreign.Storable

newtype AsyncState = AsyncState {asyncStateClientPtr :: Raw.ClientPtr}

newtype AsyncClientT m a = AsyncClientT {getAsyncClient :: ReaderT AsyncState m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AsyncState)

-- | A thread ID that will be associated with a given command.
newtype ThreadContext = ThreadContext {getThreadContext :: Word64}

-- | Initializes a TigerBeetle client connection to accept commands.
--
-- Provide a callback to receive the responses from the server.
--
-- @
--    withClient (ClusterId 0) (Address "3000") (ThreadContext 1) (\_ result -> print result) $ do
--      createAccounts [CreateAccount 0 0 100]
-- @
withClient
  :: (MonadIO m)
  => ClusterId
  -> Address
  -> ThreadContext
  -> (ThreadContext -> Response -> IO ())
  -> AsyncClientT m ()
  -> m ()
withClient clusterId address (ThreadContext userCtxt) callback clientAction = do
  cb <- liftIO $ Raw.makeCompletionCallback $ \(CUIntPtr ctx) tbPacketPtr _ resultDataPtr resultLen -> do
    tbPacket <- peek tbPacketPtr
    tbResponse <- Raw.decodeResponse tbPacket resultDataPtr $ fromIntegral resultLen
    callback (ThreadContext ctx) $ toResponse tbResponse
  clientInitResult <- liftIO $ Raw.initClient clusterId address (CUIntPtr userCtxt) cb
  case clientInitResult of
    Left err -> error $ show err
    Right clientPtr -> do
      let asyncState = AsyncState{asyncStateClientPtr = clientPtr}
      (`runReaderT` asyncState) . getAsyncClient $ clientAction

createAccounts :: (MonadIO m) => [CreateAccount] -> AsyncClientT m ()
createAccounts createAccountParams = do
  AsyncState{..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.createAccounts createAccountParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status

lookupAccounts :: (MonadIO m) => [AccountId] -> AsyncClientT m ()
lookupAccounts lookupAccountParams = do
  AsyncState{..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.lookupAccounts lookupAccountParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status

getAccountBalances :: (MonadIO m) => [AccountBalances] -> AsyncClientT m ()
getAccountBalances accountBalanceParams = do
  AsyncState{..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.getAccountBalances accountBalanceParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status

getAccountTransfers :: (MonadIO m) => [AccountTransfers] -> AsyncClientT m ()
getAccountTransfers accountTransferParams = do
  AsyncState{..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.getAccountTransfers accountTransferParams
  case status of
    ClientOk -> pure ()
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
queryAccounts :: (MonadIO m) => [AccountQuery] -> AsyncClientT m ()
queryAccounts queryAccountParams = do
  AsyncState{..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.queryAccounts queryAccountParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status

createTransfers :: (MonadIO m) => [CreateTransfer] -> AsyncClientT m ()
createTransfers transferParams = do
  AsyncState{..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.createTransfers transferParams
  case status of
    ClientOk -> pure ()
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
queryTransfers :: (MonadIO m) => [TransferQuery] -> AsyncClientT m ()
queryTransfers transferQueryParams = do
  AsyncState{..} <- ask
  status <- Raw.submit asyncStateClientPtr Raw.queryTransfers transferQueryParams
  case status of
    ClientOk -> pure ()
    _ -> error $ show status
