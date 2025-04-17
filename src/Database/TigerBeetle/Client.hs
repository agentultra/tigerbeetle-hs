module Database.TigerBeetle.Client
  ( -- * Re-exports
      module Database.TigerBeetle.Address
  , module Database.TigerBeetle.ClusterId

    -- * Types
  , Client (..)
  , ClientError (..)
  , ClientState (..)
  , withClient
  )
where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Database.TigerBeetle.Address
import Database.TigerBeetle.ClusterId
import Database.TigerBeetle.Raw.Client qualified as Raw
import Foreign.Storable -- TODO: for testing/dev only, remove me!

newtype ClientRef = ClientRef {getRawClient :: Raw.ClientPtr}
  deriving (Show)

data ClientState = ClientState
  { clientRef :: ClientRef
  }
  deriving (Show)

data ClientError
  = ClientInitError Raw.ClientInitError -- TODO: for testing/dev only, remove me!
  | ClientError
  deriving (Eq, Show)

instance Exception ClientError

newtype Client m a = Client
  { runClient :: (ExceptT ClientError (StateT ClientState m)) a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError ClientError
    , MonadIO
    , MonadState ClientState
    )

-- TODO: make this actually do something useful
withClient
  :: MonadIO m
  => ClusterId
  -> Address
  -> Client m ()
  -> m ()
withClient clusterId address clientAction = do
  cb <- liftIO $ Raw.initCallback $ \_ packetPtr _ _ _ -> peek packetPtr >>= print
  initResult <- liftIO $ Raw.initClient clusterId address 0 cb
  case initResult of
    Left initErr -> error $ show initErr
    Right client -> do
      result <- (`evalStateT` ClientState (ClientRef client)) . runExceptT . runClient $ clientAction
      case result of
        Left clientActionError -> error $ show clientActionError
        Right _ -> do
          _ <- liftIO $ Raw.deinitClient client
          pure ()
