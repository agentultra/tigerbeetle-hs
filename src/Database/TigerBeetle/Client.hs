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
import Database.TigerBeetle.Raw.Client qualified as Raw
import Data.Map.Strict (Map)

data ClientState
  = ClientState
  { completionContextCounter :: Int
  , responseMap :: Map Int Response -- ^ Map the request to the response by the completion context
  }
  deriving (Eq, Show)

data ClientError = ClientError
  deriving (Eq, Show)

instance Exception ClientError

newtype Client m a
  = Client
  { getClient :: ResourceT (ExceptT ClientError (StateT ClientState m)) a
  }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadError ClientError
           , MonadIO
           , MonadResource
           , MonadState ClientState
           )

runClient :: MonadIO m => Client m a -> ClientState -> m a
runClient client = runResourceT . runExceptT . (`evalStateT` client)

data ClusterId = ClusterId
  deriving (Eq, Show)

type ClusterAddress = String

data Response = Response
  deriving (Eq, Show)

-- So we want a program like this...
--
-- withClient clusterId address cb = do
--   createAccounts [CreateAccount "id-1" 0, CreateAccount "id-2", 0]
--   createTransfer ...
--   queryAccount "id-1"..
--
-- Where each action in the computation is a request to libtb_client
-- and when the callback completes with the response, the monad calls
-- the users' callback with the response.
--
-- And in this case...
--
-- withClientSTM clusterId address channel = do
--   createAccounts [...]
--   createTransfers [...]
--
-- Each action will need to update the Client monad state to map the
-- responses.  We want each action to submit the response to the STM
-- bounded queue/channel/whatever.

withClient
  :: MonadIO m
  => ClusterId
  -> ClusterAddress
  -> (Response -> m ())
  -> Client m a
withClient clusterId clusterAddress = do
  initStatus <- Raw.initClient clusterId clusterAddress completionCtx cb
  case initStatus of
    Success -> _
    other   -> _
