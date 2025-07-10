{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Raw.Transfer
  ( module Database.TigerBeetle.Raw.Transfer
  , TBTransfer (..)
  , FFI.TBTransferFlag (..)
  )
where

import Control.Monad
import Data.Set qualified as S
import Data.Vector qualified as V
import Database.TigerBeetle.Internal.FFI.Client
  ( TBOperation (..)
  , TBPacket (..)
  )
import Database.TigerBeetle.Internal.FFI.Client qualified as Client
import Database.TigerBeetle.Internal.FFI.Query
  ( TBQueryFilter (..),
    TBQueryFilterFlags
  )
import Database.TigerBeetle.Internal.FFI.Query qualified as Raw
import Database.TigerBeetle.Internal.FFI.Transfer (TBTransfer (..))
import Database.TigerBeetle.Internal.FFI.Transfer qualified as FFI
import Database.TigerBeetle.Timestamp
import Database.TigerBeetle.Transfer
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

queryTransfersPacket :: [TransferQuery] -> IO (Ptr TBPacket)
queryTransfersPacket transferQueries = do
  (transferFilterData, transferFilterDataSize) <- pack transferQueries
  packetPtr <- malloc
  poke packetPtr $
    TBPacket
      { tbPacketUserData = nullPtr
      , tbPacketData = castPtr @TBQueryFilter @() transferFilterData
      , tbPacketDataSize = fromIntegral transferFilterDataSize
      , tbPacketUserTag = 0
      , tbPacketOperation = QueryTransfers
      , tbPacketStatus = Client.Ok
      , tbPacketOpaque = V.empty
      }
  pure packetPtr
  where
    pack :: [TransferQuery] -> IO (Ptr TBQueryFilter, Int)
    pack queries = do
      let zeroQueryFilter
            = TBQueryFilter
            { tbQueryFilterUserData128  = 0
            , tbQueryFilterUserData64   = 0
            , tbQueryFilterUserData32   = 0
            , tbQueryFilterLedger       = 0
            , tbQueryFilterCode         = 0
            , tbQueryFilterReserved     = mempty
            , tbQueryFilterTimestampMin = 0
            , tbQueryFilterTimestampMax = 0
            , tbQueryFilterLimit        = 0
            , tbQueryFilterFlags        = mempty
            }
          dataSize = sizeOf zeroQueryFilter * length queries
      tbAccountFilters <- mallocBytes dataSize
      forM_ (zip [0 ..] queries) $ \(ix, query) -> do
        let acctFilter
              = TBQueryFilter
              { tbQueryFilterUserData128  = 0
              , tbQueryFilterUserData64   = 0
              , tbQueryFilterUserData32   = 0
              , tbQueryFilterLedger       = fromIntegral query.queryTransferLedger -- TODO: refactor
              , tbQueryFilterCode         = fromIntegral query.queryTransferCode   -- TODO: refactor
              , tbQueryFilterReserved     = mempty
              , tbQueryFilterTimestampMin = getTimestamp query.queryTransferTimestampMin
              , tbQueryFilterTimestampMax = getTimestamp query.queryTransferTimestampMax
              , tbQueryFilterLimit        = fromIntegral query.queryTransferLimit
              , tbQueryFilterFlags        = toTBQueryFilterFlag `S.map` query.queryTransferFlags
              }
        pokeElemOff tbAccountFilters ix acctFilter
      pure (tbAccountFilters, dataSize)

    toTBQueryFilterFlag :: TransferQueryFlag -> TBQueryFilterFlags
    toTBQueryFilterFlag = \case
      Reversed -> Raw.Reversed
