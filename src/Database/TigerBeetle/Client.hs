module Database.TigerBeetle.Client
  ( -- * Re-exports
      module Database.TigerBeetle.Address
  , module Database.TigerBeetle.ClusterId

    -- * Types
  , ClientError (..)
  , withClient
  )
where

import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception
import Data.IntMap.Strict qualified as IM
import Database.TigerBeetle.Address
import Database.TigerBeetle.ClusterId
import Database.TigerBeetle.Internal.FFI.Client qualified as FFI
import Database.TigerBeetle.Raw.Client qualified as Raw
import GHC.Natural (Natural)

newtype ClientRef = ClientRef {getRawClient :: Raw.ClientPtr}
  deriving (Show)

data ClientError
  = ClientInitError Raw.ClientInitError -- TODO: for testing/dev only, remove me!
  | ClientError
  deriving (Eq, Show)

instance Exception ClientError

type TimeoutMilliseconds = Natural

withClient
  :: TimeoutMilliseconds
  -> ClusterId
  -> Address
  -> (Raw.ClientState -> IO a)
  -> IO (Either Raw.ClientInitError a)
withClient timeout clusterId address action = do
  clientState <-
    Raw.ClientState
      <$> newTVarIO IM.empty
      <*> newTVarIO 1
      <*> newTQueueIO
      <*> newTVarIO False
      <*> pure timeout

  -- Initialize the completion callback
  callback <- FFI.makeCompletionCallback $ Raw.setupCompletionCallback clientState

  initResult <- Raw.initClient clusterId address 0 callback

  case initResult of
    Right client ->
      finally
      (Right <$> action clientState)
      (Raw.finalizeClient client clientState)
    Left initError -> pure $ Left initError
