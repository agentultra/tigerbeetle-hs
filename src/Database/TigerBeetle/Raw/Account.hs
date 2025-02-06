{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Raw.Account where

import Control.Monad
import Data.Enum.Storable (fromPlain)
import Database.TigerBeetle.Client.Account (Account)
import Database.TigerBeetle.Internal.FFI
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

createAccounts :: [Account] -> IO Packet
createAccounts accounts = do
  accountData <- pack accounts
  pure
    $ Packet
    { next         = nullPtr
    , userData     = nullPtr -- TODO: This should be a generated
                             -- id from client state
    , operation    = fromIntegral $ fromEnum CreateAccounts
    , status       = fromPlain Ok
    , dataSize     = fromIntegral $ sizeOf accountData
    , packetData   = castPtr @TbAccount @() accountData
    , batchAllowed = 0
    , batchNext    = nullPtr
    , batchTail    = nullPtr
    , batchSize    = 0
    , reserved     = []
  }
  where
    pack :: [Account] -> IO (Ptr TbAccount)
    pack accts = do
      tbaccounts <- malloc @TbAccount
      forM_ (zip [0..] accts) $ \(offset, acct) -> do
        tbAccount <- packAccount acct
        pokeElemOff tbaccounts offset tbAccount
      pure tbaccounts

    packAccount :: Account -> IO TbAccount
    packAccount = undefined
