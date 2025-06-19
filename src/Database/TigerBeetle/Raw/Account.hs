{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Raw.Account
  ( module Database.TigerBeetle.Raw.Account
  , TBAccount (..)
  )
where

import Control.Monad
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.WideWord
import Database.TigerBeetle.Internal.FFI.Account (TBAccount (..))
import Database.TigerBeetle.Internal.FFI.Client
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

zeroTBAccount :: IO TBAccount
zeroTBAccount =
  pure $
    TBAccount
      { tbAccountId = 0
      , tbAccountDebitsPending = 0
      , tbAccountDebitsPosted = 0
      , tbAccountCreditsPending = 0
      , tbAccountCreditsPosted = 0
      , tbAccountUserData128 = 0
      , tbAccountUserData64 = 0
      , tbAccountUserData32 = 0
      , tbAccountReserved = 0
      , tbAccountLedger = 0
      , tbAccountCode = 0
      , tbAccountFlags = S.empty
      , tbAccountTimestamp = 0
      }

createAccountsPacket :: [TBAccount] -> IO (Ptr TBPacket)
createAccountsPacket accounts = do
  (accountData, accountDataSize) <- pack accounts
  packetPtr <- malloc
  poke packetPtr $
    TBPacket
      { tbPacketUserData = nullPtr
      , tbPacketData = castPtr @TBAccount @() accountData
      , tbPacketDataSize = fromIntegral accountDataSize
      , tbPacketUserTag = 0
      , tbPacketOperation = CreateAccounts
      , tbPacketStatus = Ok
      , tbPacketOpaque = V.empty
      }
  pure packetPtr
 where
  pack :: [TBAccount] -> IO (Ptr TBAccount, Int)
  pack accts@(a:_) = do
    let dataSize = sizeOf a * length accts
    tbaccounts <- mallocBytes dataSize
    forM_ (zip [0 ..] accts) $ \(ix, acct) -> do
      pokeElemOff tbaccounts ix acct
    pure (tbaccounts, dataSize)
  pack [] = error "Cannot pack an empty list of accounts"

createLookupAccountsPacket :: [Word128] -> IO (Ptr TBPacket)
createLookupAccountsPacket ids = do
  (accountIdData, accountIdDataSize) <- pack ids
  packetPtr <- malloc
  poke packetPtr $
    TBPacket
      { tbPacketUserData = nullPtr
      , tbPacketData = castPtr @Word128 @() accountIdData
      , tbPacketDataSize = fromIntegral accountIdDataSize
      , tbPacketUserTag = 0
      , tbPacketOperation = LookupAccounts
      , tbPacketStatus = Ok
      , tbPacketOpaque = V.empty
      }
  pure packetPtr
  where
    pack :: [Word128] -> IO (Ptr Word128, Int)
    pack acctIds@(a:_) = do
      let dataSize = sizeOf a * length acctIds
      tbAccountIds <- mallocBytes dataSize
      forM_ (zip [0 ..] acctIds) $ \(ix, acctId) -> do
        pokeElemOff tbAccountIds ix acctId
      pure (tbAccountIds, dataSize)
    pack [] = error "Cannot pack an empty list of account ids"
