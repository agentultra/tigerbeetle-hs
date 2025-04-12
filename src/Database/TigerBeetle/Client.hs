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
import Control.Monad.Trans.Resource
import Database.TigerBeetle.Address
import Database.TigerBeetle.ClusterId
import Database.TigerBeetle.Raw.Client qualified as Raw

newtype ClientRef = ClientRef {getRawClient :: Raw.ClientPtr}
  deriving (Show)

data ClientState = ClientState
  { clientRef :: ClientRef
  }
  deriving (Show)

data ClientError = ClientError
  deriving (Eq, Show)

instance Exception ClientError

newtype Client m a = Client
  { runClient :: ResourceT (ExceptT ClientError (StateT ClientState m)) a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError ClientError
    , MonadIO
    , MonadResource
    , MonadState ClientState
    )

-- TODO: make this actually do something useful
withClient :: ClusterId -> Address -> IO ()
withClient clusterId address = do
  cb <- Raw.makeCompletionCallback Raw.clientCallBack
  initResult <- Raw.initClientEcho clusterId address 0 cb
  case initResult of
    Left err -> error $ "withClient: " ++ show err
    Right ref -> do
      let clientState =
            ClientState
              { clientRef = ClientRef ref
              }
      pure ()
