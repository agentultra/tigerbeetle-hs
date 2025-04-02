{-# LANGUAGE LambdaCase #-}
module Database.TigerBeetle.Raw.Response where

import Database.TigerBeetle.Internal.FFI.Account
import Database.TigerBeetle.Internal.FFI.Transfer
import Database.TigerBeetle.Internal.FFI.Client (TBOperation(..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)

data TBResponse = 
    CreateAccountResultResponse [TBCreateAccountsResult]
  | CreateTranferResultResponse [TBCreateTransfersResult]
  | LookupAccountsResponse [TBAccount]
  | LookupTransfersResponse [TBTransfer]
  | GetAccountTransfersResponse [TBTransfer]
  | GetAccountBalancesResponse [TBAccountBalance]
  | QueryAccountsResponse [TBAccount]
  | QueryTransfersResponse [TBTransfer]
  | GetEventsResponse
  | PulseResponseSuccess
  deriving (Eq, Show)

data TBResponseParseError = TBResponseParseError
  { operation :: TBOperation
  , rawBytes :: ByteString
  , parseError :: Text
  }
  deriving (Eq, Show)

decodeResponse :: ByteString -> TBOperation -> Either TBResponseParseError TBResponse
decodeResponse bytes = \case
  Pulse -> if BS.length bytes == 0
    then Right PulseResponseSuccess
    else Left $ TBResponseParseError Pulse bytes "Unsuccesful pulse indicated by non-zero bytes respone"
  _ -> undefined
  -- CreateAccounts ->
  -- CreateTransfers ->
  -- LookupAccounts ->
  -- LookupTransfers ->
  -- GetAccountTransfers ->
  -- GetAccountBalances ->
  -- QueryAccounts ->
  -- QueryTransfers ->
  -- GetEvents ->
