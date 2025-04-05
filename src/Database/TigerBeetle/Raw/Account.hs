{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Raw.Account
  ( module Database.TigerBeetle.Raw.Account
  , TBAccount (..)
  )
where

import Control.Monad
import Data.Set qualified as S
import Data.Vector qualified as V
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
  accountData <- pack accounts
  packetPtr <- malloc
  poke packetPtr $
    TBPacket
      { tbPacketUserData = nullPtr
      , tbPacketData = castPtr @TBAccount @() accountData
      , tbPacketDataSize = fromIntegral $ sizeOf accountData
      , tbPacketUserTag = 0
      , tbPacketOperation = CreateAccounts
      , tbPacketStatus = Ok
      , tbPacketOpaque = V.empty
      }
  pure packetPtr
 where
  pack :: [TBAccount] -> IO (Ptr TBAccount)
  pack accts = do
    tbaccounts <- malloc @TBAccount
    forM_ (zip [0 ..] accts) $ \(offset, acct) -> do
      pokeElemOff tbaccounts offset acct
    pure tbaccounts
