{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Client.Account where

import Control.Monad.IO.Class
import Data.WideWord
import Database.TigerBeetle.Internal.FFI.Client qualified as FFI
import Database.TigerBeetle.Raw.Account qualified as Raw
import Foreign.ForeignPtr

newtype AccountId = AccountId { getAccountId :: Word128 }
  deriving (Eq, Show)

data CreateAccount = CreateAccount
  { createAccountId     :: AccountId
  , createAccountLedger :: Int
  , createAccountCode   :: Int
  }
  deriving (Eq, Show)

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
