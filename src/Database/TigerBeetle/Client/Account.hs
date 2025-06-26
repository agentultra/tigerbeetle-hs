{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Client.Account where

import Control.Monad.IO.Class
import Database.TigerBeetle.Account
import Database.TigerBeetle.Internal.FFI.Client qualified as FFI
import Database.TigerBeetle.Raw.Account qualified as Raw
import Foreign.ForeignPtr

-- | Create a batch of TigerBeetle accounts.
createAccounts :: MonadIO m => [CreateAccount] -> m (ForeignPtr FFI.TBPacket)
createAccounts accts = do
  tbAccounts <- liftIO $ mapM createTBAccount accts
  tbPacketPtr <- liftIO $ Raw.createAccountsPacket tbAccounts
  liftIO $ newForeignPtr_ tbPacketPtr
 where
  createTBAccount :: CreateAccount -> IO Raw.TBAccount
  createTBAccount (CreateAccount{..}) = do
    tbAcct <- Raw.zeroTBAccount
    pure $
      tbAcct
        { Raw.tbAccountId = fromIntegral $ getAccountId createAccountId
        , Raw.tbAccountLedger = fromIntegral createAccountLedger
        , Raw.tbAccountCode = fromIntegral createAccountCode
        }

lookupAccounts :: MonadIO m => [AccountId] -> m (ForeignPtr FFI.TBPacket)
lookupAccounts ids = do
  tbPacketPtr <- liftIO . Raw.createLookupAccountsPacket $ map getAccountId ids
  liftIO $ newForeignPtr_ tbPacketPtr

getAccountBalances :: MonadIO m => [AccountBalances] -> m (ForeignPtr FFI.TBPacket)
getAccountBalances balances = do
  tbPacketPtr <- liftIO $ Raw.createGetAccountBalancesPacket balances
  liftIO $ newForeignPtr_ tbPacketPtr
