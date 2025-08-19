{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Raw.Account
  ( module Database.TigerBeetle.Raw.Account
  , TBAccount (..)
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.WideWord
import Database.TigerBeetle.Account
import Database.TigerBeetle.Internal.FFI.Account
  ( TBAccount (..)
  , TBAccountBalance (..)
  , TBAccountFilter (..)
  , TBAccountFilterFlags (..)
  )
import Database.TigerBeetle.Internal.FFI.Client
import Database.TigerBeetle.Internal.FFI.Query
  ( TBQueryFilter (..),
    TBQueryFilterFlags
  )
import Database.TigerBeetle.Internal.FFI.Query qualified as Q
import Database.TigerBeetle.Ledger
import Database.TigerBeetle.Timestamp
import Foreign.ForeignPtr
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

zeroTBAccountBalance :: IO TBAccountBalance
zeroTBAccountBalance =
  pure $
    TBAccountBalance
    { tbAccountBalanceDebitsPending  = 0
    , tbAccountBalanceDebitsPosted   = 0
    , tbAccountBalanceCreditsPending = 0
    , tbAccountBalanceCreditsPosted  = 0
    , tbAccountBalanceTimestamp      = 0
    , tbAccountBalanceReserved       = mempty
    }

-- | Create a 'TBPacket' for the @TB_OPERATION_CREATE_ACCOUNTS@ operation.
createAccounts :: MonadIO m => [CreateAccount] -> m (ForeignPtr TBPacket)
createAccounts accts = do
  tbAccounts <- liftIO $ mapM createTBAccount accts
  tbPacketPtr <- liftIO $ createAccountsPacket tbAccounts
  liftIO $ newForeignPtr_ tbPacketPtr
 where
  createTBAccount :: CreateAccount -> IO TBAccount
  createTBAccount (CreateAccount{..}) = do
    tbAcct <- zeroTBAccount
    pure $
      tbAcct
        { tbAccountId = fromIntegral $ getAccountId createAccountId
        , tbAccountLedger = getLedgerId createAccountLedger
        , tbAccountCode = getAccountCode createAccountCode
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

-- | Create a 'TBPacket' for the @TB_OPERATION_LOOKUP_ACCOUNTS@ operation.
lookupAccounts :: MonadIO m => [AccountId] -> m (ForeignPtr TBPacket)
lookupAccounts ids = do
  tbPacketPtr <- liftIO . createLookupAccountsPacket $ map getAccountId ids
  liftIO $ newForeignPtr_ tbPacketPtr

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

toTBAccountFilterFlag :: AccountFlag -> TBAccountFilterFlags
toTBAccountFilterFlag = \case
  AccountDebits -> Debits
  AccountCredits -> Credits
  AccountReversed -> Reversed

-- | Create a 'TBPacket' for the @TB_OPERATION_GET_ACCOUNT_BALANCES@ operation.
getAccountBalances :: MonadIO m => [AccountBalances] -> m (ForeignPtr TBPacket)
getAccountBalances balances = do
  tbPacketPtr <- liftIO $ createGetAccountBalancesPacket balances
  liftIO $ newForeignPtr_ tbPacketPtr

createGetAccountBalancesPacket :: [AccountBalances] -> IO (Ptr TBPacket)
createGetAccountBalancesPacket accountBalances = do
  (accountFilterData, accountFilterDataSize) <- pack accountBalances
  packetPtr <- malloc
  poke packetPtr $
    TBPacket
      { tbPacketUserData = nullPtr
      , tbPacketData = castPtr @TBAccountFilter @() accountFilterData
      , tbPacketDataSize = fromIntegral accountFilterDataSize
      , tbPacketUserTag = 0
      , tbPacketOperation = GetAccountBalances
      , tbPacketStatus = Ok
      , tbPacketOpaque = V.empty
      }
  pure packetPtr
  where
    pack :: [AccountBalances] -> IO (Ptr TBAccountFilter, Int)
    pack balanceFilters = do
      let zeroAcctFilter
            = TBAccountFilter
            { tbAccountFilterAccountId = 0
            , tbAccountFilterUserData128 = 0
            , tbAccountFilterUserData64 = 0
            , tbAccountFilterUserData32 = 0
            , tbAccountFilterCode = 0
            , tbAccountFilterReserved = mempty
            , tbAccountFilterTimestampMin = 0
            , tbAccountFilterTimestampMax = 0
            , tbAccountFilterLimit = 0
            , tbAccountFilterFlags = mempty
            }
          dataSize = sizeOf zeroAcctFilter * length balanceFilters
      tbAccountFilters <- mallocBytes dataSize
      forM_ (zip [0 ..] balanceFilters) $ \(ix, balanceFilter) -> do
        let acctFilter
              = TBAccountFilter
              { tbAccountFilterAccountId = getAccountId balanceFilter.balancesAccountId
              , tbAccountFilterUserData128 = 0
              , tbAccountFilterUserData64 = 0
              , tbAccountFilterUserData32 = 0
              , tbAccountFilterCode = 0
              , tbAccountFilterReserved = mempty
              , tbAccountFilterTimestampMin = 0
              , tbAccountFilterTimestampMax = 0
              , tbAccountFilterLimit = fromIntegral balanceFilter.balancesLimit
              , tbAccountFilterFlags = toTBAccountFilterFlag `S.map` balanceFilter.balancesFlags
              }
        pokeElemOff tbAccountFilters ix acctFilter
      pure (tbAccountFilters, dataSize)

-- | Create a 'TBPacket' for the @TB_OPERATION_GET_ACCOUNT_TRANSFERS@ operation.
getAccountTransfers :: MonadIO m => [AccountTransfers] -> m (ForeignPtr TBPacket)
getAccountTransfers transfers = do
  tbPacketPtr <- liftIO $ createGetAccountTransfersPacket transfers
  liftIO $ newForeignPtr_ tbPacketPtr

createGetAccountTransfersPacket :: [AccountTransfers] -> IO (Ptr TBPacket)
createGetAccountTransfersPacket accountTransfers = do
  (accountFilterData, accountFilterDataSize) <- pack accountTransfers
  packetPtr <- malloc
  poke packetPtr $
    TBPacket
      { tbPacketUserData = nullPtr
      , tbPacketData = castPtr @TBAccountFilter @() accountFilterData
      , tbPacketDataSize = fromIntegral accountFilterDataSize
      , tbPacketUserTag = 0
      , tbPacketOperation = GetAccountTransfers
      , tbPacketStatus = Ok
      , tbPacketOpaque = V.empty
      }
  pure packetPtr
  where
    pack :: [AccountTransfers] -> IO (Ptr TBAccountFilter, Int)
    pack transfers = do
      let zeroAcctFilter
            = TBAccountFilter
            { tbAccountFilterAccountId = 0
            , tbAccountFilterUserData128 = 0
            , tbAccountFilterUserData64 = 0
            , tbAccountFilterUserData32 = 0
            , tbAccountFilterCode = 0
            , tbAccountFilterReserved = mempty
            , tbAccountFilterTimestampMin = 0
            , tbAccountFilterTimestampMax = 0
            , tbAccountFilterLimit = 0
            , tbAccountFilterFlags = mempty
            }
          dataSize = sizeOf zeroAcctFilter * length transfers
      tbAccountFilters <- mallocBytes dataSize
      forM_ (zip [0 ..] transfers) $ \(ix, transfer) -> do
        let acctFilter
              = TBAccountFilter
              { tbAccountFilterAccountId = getAccountId transfer.transfersAccountId
              , tbAccountFilterUserData128 = 0
              , tbAccountFilterUserData64 = 0
              , tbAccountFilterUserData32 = 0
              , tbAccountFilterCode = 0
              , tbAccountFilterReserved = mempty
              , tbAccountFilterTimestampMin = 0
              , tbAccountFilterTimestampMax = 0
              , tbAccountFilterLimit = fromIntegral transfer.transfersLimit
              , tbAccountFilterFlags = toTBAccountFilterFlag `S.map` transfer.transfersFlags
              }
        pokeElemOff tbAccountFilters ix acctFilter
      pure (tbAccountFilters, dataSize)

-- | Create a 'TBPacket' for the @TB_OPERATION_QUERY_ACCOUNTS@ operation.
queryAccounts :: MonadIO m => [AccountQuery] -> m (ForeignPtr TBPacket)
queryAccounts queries = do
  tbPacketPtr <- liftIO $ queryAccountsPacket queries
  liftIO $ newForeignPtr_ tbPacketPtr

queryAccountsPacket :: [AccountQuery] -> IO (Ptr TBPacket)
queryAccountsPacket accountQueries = do
  (accountFilterData, accountFilterDataSize) <- pack accountQueries
  packetPtr <- malloc
  poke packetPtr $
    TBPacket
      { tbPacketUserData = nullPtr
      , tbPacketData = castPtr @TBQueryFilter @() accountFilterData
      , tbPacketDataSize = fromIntegral accountFilterDataSize
      , tbPacketUserTag = 0
      , tbPacketOperation = QueryAccounts
      , tbPacketStatus = Ok
      , tbPacketOpaque = V.empty
      }
  pure packetPtr
  where
    pack :: [AccountQuery] -> IO (Ptr TBQueryFilter, Int)
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
              , tbQueryFilterLedger       = getLedgerId query.accountQueryLedger
              , tbQueryFilterCode         = getAccountCode query.accountQueryCode
              , tbQueryFilterReserved     = mempty
              , tbQueryFilterTimestampMin = getTimestamp query.accountQueryTimestampMin
              , tbQueryFilterTimestampMax = getTimestamp query.accountQueryTimestampMax
              , tbQueryFilterLimit        = fromIntegral query.accountQueryLimit
              , tbQueryFilterFlags        = toTBQueryFilterFlag `S.map` query.accountQueryFlags
              }
        pokeElemOff tbAccountFilters ix acctFilter
      pure (tbAccountFilters, dataSize)

    toTBQueryFilterFlag :: AccountQueryFlag -> TBQueryFilterFlags
    toTBQueryFilterFlag = \case
      AccountQueryReversed -> Q.Reversed
