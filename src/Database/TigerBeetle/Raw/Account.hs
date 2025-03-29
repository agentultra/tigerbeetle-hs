{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Raw.Account where

import Control.Monad
import Database.TigerBeetle.Client.Account (Account(..))
import Database.TigerBeetle.Internal.FFI
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Data.Vector qualified as V

createAccounts :: [Account] -> IO (Ptr TBPacket)
createAccounts accounts = do
  accountData <- pack accounts
  packetPtr <- newPacket
  poke packetPtr
    $ TBPacket
    { tbPacketUserData         = nullPtr
    , tbPacketData             = castPtr @TbAccount @() accountData
    , tbPacketDataSize         = fromIntegral $ sizeOf accountData
    , tbPacketUserTag          = 0
    , tbPacketOperation        = fromIntegral $ fromEnum CreateAccounts
    , tbPacketStatus           = Ok
    , tbPacketOpaque           = V.empty
    }
  pure packetPtr
  where
    pack :: [Account] -> IO (Ptr TbAccount)
    pack accts = do
      tbaccounts <- malloc @TbAccount
      forM_ (zip [0..] accts) $ \(offset, acct) -> do
        tbAccount <- packAccount acct
        pokeElemOff tbaccounts offset tbAccount
      pure tbaccounts

    packAccount :: Account -> IO TbAccount
    packAccount Account {..}
      = pure $ TbAccount
      { tbAccountId = id_
      , tbAccountDebitsPending = debitsPending
      , tbAccountDebitsPosted = debitsPosted
      , tbAccountCreditsPending = creditsPending
      , tbAccountCreditsPosted = creditsPosted
      , tbAccountUserData128 = userData128
      , tbAccountUserData64 = userData64
      , tbAccountUserData32 = userData32
      , tbAccountReserved = reserved
      , tbAccountLedger = ledger
      , tbAccountCode = code
      , tbAccountFlags = flags
      , tbAccountTimestamp = timestamp
      }
