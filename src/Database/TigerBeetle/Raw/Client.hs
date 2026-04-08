{-# LANGUAGE LambdaCase #-}

module Database.TigerBeetle.Raw.Client
  ( -- * Types
    ClientInitError (..)
  , ClientPtr
  , TBRegisterLogCallbackStatus (..)

    -- * Functions
  , initClient
  , submit
  , FFI.makeCompletionCallback
  , registerLoggingCallback
  )
where

import Control.Exception (assert)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as TE
import Data.Word
import Database.TigerBeetle.Address
import Database.TigerBeetle.ClusterId (ClusterId)
import Database.TigerBeetle.Internal.FFI.Client
  ( TBClientStatus (..)
  , TBCompletionCallback
  , TBCompletionContext
  , TBInitStatus
  , TBLoggingCallback
  , TBOperation (..)
  , TBPacket (..)
  , TBPacketStatus (..)
  , TBRegisterLogCallbackStatus (..)
  )
import Database.TigerBeetle.Internal.FFI.Client qualified as FFI
import Database.TigerBeetle.Raw.Response (DecodeResponseError)
import Foreign.C.Types (CChar, CInt)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)

-- | Whether to start an echo server or a standard server
data ClientKind = Echo | Standard
  deriving (Eq, Ord, Show)

data RequestError
  = ClientError TBClientStatus
  | PacketError TBPacketStatus
  | PacketDataParseError DecodeResponseError
  | ClientShutdownDuringRequest
  | RequestTimeoutError TBOperation ByteString
  deriving (Eq, Show)

withAddressPtr :: Address -> ((Ptr CChar, Int) -> IO a) -> IO a
withAddressPtr = BS.useAsCStringLen . TE.encodeUtf8 . getAddress

data ClientInitError
  = Unexpected
  | OutOfMemory
  | AddressInvalid
  | AddressLimitExceeded
  | SystemResources
  | NetworkSubsystem
  deriving (Eq, Show)

-- | Create a 'Client' initialization error from a 'TBInitStatus'.
--
-- Asserts that @err@ is not 'FFI.Success', throws an exception at
-- runtime.
toClientInitError :: TBInitStatus -> ClientInitError
toClientInitError err = assert (err /= FFI.Success) $
  case err of
    FFI.Unexpected -> Unexpected
    FFI.OutOfMemory -> OutOfMemory
    FFI.AddressInvalid -> AddressInvalid
    FFI.AddressLimitExceeded -> AddressLimitExceeded
    FFI.SystemResources -> SystemResources
    FFI.NetworkSubsystem -> NetworkSubsystem
    FFI.Success -> error "toClientInitError: Success is not an error"

type ClientPtr = ForeignPtr FFI.TBClient

initClientPtr :: IO ClientPtr
initClientPtr = do
  mallocForeignPtr

validateClientInit :: ClientPtr -> TBInitStatus -> IO (Either ClientInitError ClientPtr)
validateClientInit clientPtr = \case
  FFI.Success -> pure $ Right clientPtr
  initError -> pure . Left . toClientInitError $ initError

-- | Call @tb_client_init@ and return a valid 'Client' upon success.
--
-- The finalizer on 'Client' will call @tb_client_deinit@.
initClient
  :: ClusterId
  -> Address
  -> TBCompletionContext
  -> FunPtr TBCompletionCallback
  -> IO (Either ClientInitError ClientPtr)
initClient clusterId address completionCtx completionCallback = do
  clientPtr <- initClientPtr
  initStatus <- withForeignPtr clientPtr $ \cp -> do
    withAddressPtr address $ \(addrPtr, addrLen) -> do
      FFI.tbClientInit
        cp
        clusterId
        addrPtr
        (fromIntegral addrLen)
        completionCtx
        completionCallback
  validateClientInit clientPtr initStatus

-- | Call @tb_client_submit@
submit :: (MonadIO m) => ClientPtr -> (a -> IO (ForeignPtr TBPacket)) -> a -> m TBClientStatus
submit clientPtr action param = do
  requestPacketPtr <- liftIO $ action param
  liftIO $ withForeignPtr clientPtr $ \rawClient -> do
    withForeignPtr requestPacketPtr $ \rawPacket -> do
      FFI.tbClientSubmit rawClient rawPacket

registerLoggingCallback
  :: (MonadIO m)
  => (CInt -> Ptr Word8 -> Word32 -> IO ())
  -> Bool
  -> m TBRegisterLogCallbackStatus
registerLoggingCallback callback debug = do
  callbackPtr <- liftIO $ FFI.makeLoggingCallback callback
  liftIO $ FFI.tbRegisterLogCallback callbackPtr debug
