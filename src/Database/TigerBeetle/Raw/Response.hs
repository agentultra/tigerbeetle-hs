{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Raw.Response where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Database.TigerBeetle.Internal.FFI.Account
import Database.TigerBeetle.Internal.FFI.Client (TBOperation (..), TBPacket (..))
import Database.TigerBeetle.Internal.FFI.Transfer
import Foreign.Ptr
import Foreign.Storable

data TBResponse
  = CreateAccountResultResponse [TBCreateAccountsResult]
  | CreateTranferResultResponse [TBCreateTransfersResult]
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

decodeResponse :: TBPacket -> IO TBResponse
decodeResponse packet = case packet.tbPacketOperation of
  CreateAccounts -> do
    result <- peek @TBCreateAccountsResult . castPtr @() @TBCreateAccountsResult $ packet.tbPacketData
    pure $ CreateAccountResultResponse [result]
  _ -> undefined
