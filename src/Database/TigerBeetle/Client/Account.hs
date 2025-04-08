{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Client.Account (
  createAccounts
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Database.TigerBeetle.Client
import Database.TigerBeetle.Internal.FFI.Client qualified as FFI
import Database.TigerBeetle.Raw.Account qualified as Raw

data CreateAccount = CreateAccount
  { createAccountId :: Int
  , createAccountLedger :: Int
  }
  deriving (Eq, Show)

-- | Create a batch of TigerBeetle accounts.
createAccounts :: (MonadIO m) => [CreateAccount] -> Client m ()
createAccounts accts = do
  tbAccounts <- liftIO $ mapM createTBAccount accts
  tbPacket <- liftIO $ Raw.createAccountsPacket tbAccounts
  resultStatus <- liftIO $ undefined tbPacket

  unless (resultStatus == FFI.ClientOk) $ throwError ClientError
 where
  createTBAccount :: CreateAccount -> IO Raw.TBAccount
  createTBAccount (CreateAccount{..}) = do
    tbAcct <- Raw.zeroTBAccount
    pure $
      tbAcct
        { Raw.tbAccountId = fromIntegral createAccountId
        , Raw.tbAccountLedger = fromIntegral createAccountLedger
        }
