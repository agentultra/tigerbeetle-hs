{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Raw.Transfer
  ( module Database.TigerBeetle.Raw.Transfer
  , TBTransfer (..)
  , TBTransferFlag (..)
  )
where

import Control.Monad
import Data.Vector qualified as V
import Database.TigerBeetle.Internal.FFI.Client
  ( TBOperation (..)
  , TBPacket (..)
  )
import Database.TigerBeetle.Internal.FFI.Client qualified as Client
import Database.TigerBeetle.Internal.FFI.Transfer
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

zeroTBTransfer :: IO TBTransfer
zeroTBTransfer = pure $ TBTransfer
  { tbTransferId = 0
  , tbTransferDebitAccountId = 0
  , tbTransferCreditAccountId = 0
  , tbTransferAmount = 0
  , tbTransferPendingId = 0
  , tbTransferUserData128 = 0
  , tbTransferUserData64 = 0
  , tbTransferUserData32 = 0
  , tbTransferTimeout = 0
  , tbTransferLedger = 0
  , tbTransferCode = 0
  , tbTransferFlags = mempty
  , tbTransferTimestamp = 0
  }

createTransfersPacket :: [TBTransfer] -> IO (Ptr TBPacket)
createTransfersPacket transfers = do
  (transferData, transferDataSize) <- pack transfers
  packetPtr <- malloc
  poke packetPtr $
    TBPacket
      { tbPacketUserData = nullPtr
      , tbPacketData = castPtr @TBTransfer @() transferData
      , tbPacketDataSize = fromIntegral transferDataSize
      , tbPacketUserTag = 0
      , tbPacketOperation = CreateTransfers
      , tbPacketStatus = Client.Ok
      , tbPacketOpaque = V.empty
      }
  pure packetPtr
  where
    pack :: [TBTransfer] -> IO (Ptr TBTransfer, Int)
    pack ts@(a:_) = do
      let dataSize = sizeOf a * length ts
      tbtransfers <- mallocBytes dataSize
      forM_ (zip [0 ..] ts) $ \(ix, transfer) -> do
        pokeElemOff tbtransfers ix transfer
      pure (tbtransfers, dataSize)
    pack [] = error "Cannot pack an empty list of transfers"
