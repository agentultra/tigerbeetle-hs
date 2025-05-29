{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Client.Account where

import Control.Monad.IO.Class
import Database.TigerBeetle.Internal.FFI.Client qualified as FFI
import Database.TigerBeetle.Raw.Account qualified as Raw
import Foreign.ForeignPtr

data CreateAccount = CreateAccount
  { createAccountId :: Int
  , createAccountLedger :: Int
  }
  deriving (Eq, Show)

-- | Create a batch of TigerBeetle accounts.
createAccounts :: (MonadIO m) => [CreateAccount] -> m (ForeignPtr FFI.TBPacket)
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
        { Raw.tbAccountId = fromIntegral createAccountId
        , Raw.tbAccountLedger = fromIntegral createAccountLedger
        }
