{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Raw.Response where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Word
import Database.TigerBeetle.Internal.FFI.Account
import Database.TigerBeetle.Internal.FFI.Client (TBOperation (..), TBPacket (..))
import Database.TigerBeetle.Internal.FFI.Transfer hiding (Ok)
import Database.TigerBeetle.Internal.FFI.Transfer qualified as Transfer
import Database.TigerBeetle.Raw.Account (zeroTBAccount, zeroTBAccountBalance)
import Database.TigerBeetle.Raw.Transfer (zeroTBTransfer)
import Foreign.Ptr
import Foreign.Storable

data TBResponse
  = CreateAccountResultResponse [TBCreateAccountsResult]
  | CreateTransferResultResponse [TBCreateTransfersResult]
  | LookupAccountsResponse [TBAccount]
  | LookupTransfersResponse [TBTransfer]
  | GetAccountTransfersResponse [TBTransfer]
  | GetAccountBalancesResponse [TBAccountBalance]
  | QueryAccountsResponse [TBAccount]
  | QueryTransfersResponse [TBTransfer]
  deriving (Eq, Show)

data TBResponseParseError = TBResponseParseError
  { operation :: TBOperation
  , rawBytes :: ByteString
  , parseError :: Text
  }
  deriving (Eq, Show)

data DecodeResponseError
  = DecodeParseError TBResponseParseError
  | DisallowedOperation
  deriving (Eq, Show)

decodeResponse :: TBPacket -> Ptr Word8 -> Int -> IO TBResponse
decodeResponse packet resultData resultLen = case packet.tbPacketOperation of
  CreateAccounts -> do
    let numResults = resultLen `div` sizeOf (TBCreateAccountsResult 0 Ok)
    result <- (`traverse` [0 .. numResults - 1]) $ \ix -> do
      peekElemOff (castPtr @Word8 @TBCreateAccountsResult resultData) ix
    pure $ CreateAccountResultResponse result
  LookupAccounts -> do
    tbAccount <- zeroTBAccount
    let numResults = resultLen `div` sizeOf tbAccount
    result <- (`traverse` [0 .. numResults - 1]) $ \ix -> do
      peekElemOff (castPtr @Word8 @TBAccount resultData) ix
    pure $ LookupAccountsResponse result
  GetAccountBalances -> do
    tbAccountBalance <- zeroTBAccountBalance
    let numResults = resultLen `div` sizeOf tbAccountBalance
    result <- (`traverse` [0 .. numResults - 1]) $ \ix -> do
      peekElemOff (castPtr @Word8 @TBAccountBalance resultData) ix
    pure $ GetAccountBalancesResponse result
  GetAccountTransfers -> do
    tbAccountTransfer <- zeroTBTransfer
    let numResults = resultLen `div` sizeOf tbAccountTransfer
    result <- (`traverse` [0 .. numResults - 1]) $ \ix -> do
      peekElemOff (castPtr @Word8 @TBTransfer resultData) ix
    pure $ GetAccountTransfersResponse result
  QueryAccounts -> do
    tbAccount <- zeroTBAccount
    let numResults = resultLen `div` sizeOf tbAccount
    result <- (`traverse` [0 .. numResults - 1]) $ \ix -> do
      peekElemOff (castPtr @Word8 @TBAccount resultData) ix
    pure $ QueryAccountsResponse result
  CreateTransfers -> do
    let numResults = resultLen `div` sizeOf (TBCreateTransfersResult 0 Transfer.Ok)
    result <- (`traverse` [0 .. numResults - 1]) $ \ix -> do
      peekElemOff (castPtr @Word8 @TBCreateTransfersResult resultData) ix
    pure $ CreateTransferResultResponse result
  QueryTransfers -> do
    tbTransfer <- zeroTBTransfer
    let numResults = resultLen `div` sizeOf tbTransfer
    result <- (`traverse` [0 .. numResults - 1]) $ \ix -> do
      peekElemOff (castPtr @Word8 @TBTransfer resultData) ix
    pure $ QueryTransfersResponse result
  _ -> undefined
