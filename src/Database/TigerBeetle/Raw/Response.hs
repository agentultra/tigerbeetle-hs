module Database.TigerBeetle.Raw.Response where

import Database.TigerBeetle.Internal.FFI.Account
import Database.TigerBeetle.Internal.FFI.Transfer
import Database.TigerBeetle.Internal.FFI.Client (TBOperation(..))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Binary (decodeOrFail)
import Data.Binary.Get (ByteOffset)
import Data.Bifunctor
import qualified Data.Text as T
import Data.Foldable (Foldable(..))

data TBResponse = 
    CreateAccountResultResponse [TBCreateAccountsResult]
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

data DecodeResponseError = 
    DecodeParseError TBResponseParseError
  | DisallowedOperation  
  deriving (Eq, Show)

decodeResponse :: ByteString -> TBOperation -> Either DecodeResponseError TBResponse
decodeResponse bytes op = let
    mkError offset msg = DecodeParseError
      TBResponseParseError
        { operation = op
        , rawBytes = bytes 
        , parseError = mkParseError offset msg
        }
  in case op of
      CreateAccounts -> bimap
        (\(_,o,m) -> mkError o m)
        (\(_,_,res) -> CreateAccountResultResponse res)
        (decodeOrFail bytes)
      LookupAccounts -> bimap
        (\(_,o,m) -> mkError o m)
        (\(_,_,res) -> LookupAccountsResponse res)
        (decodeOrFail bytes)
      LookupTransfers -> bimap
        (\(_,o,m) -> mkError o m)
        (\(_,_,res) -> LookupTransfersResponse res)
        (decodeOrFail bytes)
      GetAccountTransfers -> bimap
        (\(_,o,m) -> mkError o m)
        (\(_,_,res) -> GetAccountTransfersResponse res)
        (decodeOrFail bytes)
      GetAccountBalances -> bimap
        (\(_,o,m) -> mkError o m)
        (\(_,_,res) -> GetAccountBalancesResponse res)
        (decodeOrFail bytes)
      QueryAccounts -> bimap
        (\(_,o,m) -> mkError o m)
        (\(_,_,res) -> QueryAccountsResponse res)
        (decodeOrFail bytes)
      QueryTransfers -> bimap
        (\(_,o,m) -> mkError o m)
        (\(_,_,res) -> QueryTransfersResponse res)
        (decodeOrFail bytes)
      _ -> Left DisallowedOperation
  where 
    mkParseError :: ByteOffset -> String -> Text
    mkParseError offset msg = fold
      [ "Failed at offset "
      , T.pack . show $ offset 
      , ", with message: "
      , T.pack msg
      ]
