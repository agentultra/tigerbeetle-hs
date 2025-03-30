module Database.TigerBeetle.Client
  ( -- ^ Types
    Client (..)
  , ClientError (..)
  , ClientState (..)
  )
where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Resource

data ClientState
  = ClientState
  { completionContextCounter :: Int
  }
  deriving (Eq, Show)

data ClientError = ClientError
  deriving (Eq, Show)

instance Exception ClientError

newtype Client m e a
  = Client
  { runClient :: ResourceT (ExceptT ClientError (StateT ClientState m)) a
  }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadError ClientError
           , MonadIO
           , MonadResource
           , MonadState ClientState
           )
