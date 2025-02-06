{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Database.TigerBeetle.Internal.FFI where

import Control.Monad
import Data.Enum.Storable (T)
import Data.Word
import Data.WideWord
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.Storable

#include "tb_client.h"

data Operation
    = Pulse
    | CreateAccounts
    | CreateTransfers
    | LookupAccounts
    | LookupTransfers
    | GetAccountTransfers
    | GetAccountBalances
    | QueryAccounts
    | QueryTransfers
    deriving (Eq, Show)

instance Enum Operation where
    fromEnum Pulse               = #const TB_OPERATION_PULSE
    fromEnum CreateAccounts      = #const TB_OPERATION_CREATE_ACCOUNTS
    fromEnum CreateTransfers     = #const TB_OPERATION_CREATE_TRANSFERS
    fromEnum LookupAccounts      = #const TB_OPERATION_LOOKUP_ACCOUNTS
    fromEnum LookupTransfers     = #const TB_OPERATION_LOOKUP_TRANSFERS
    fromEnum GetAccountTransfers = #const TB_OPERATION_GET_ACCOUNT_TRANSFERS
    fromEnum GetAccountBalances  = #const TB_OPERATION_GET_ACCOUNT_BALANCES
    fromEnum QueryAccounts       = #const TB_OPERATION_QUERY_ACCOUNTS
    fromEnum QueryTransfers      = #const TB_OPERATION_QUERY_TRANSFERS

    toEnum (#const TB_OPERATION_PULSE)                 = Pulse
    toEnum (#const TB_OPERATION_CREATE_ACCOUNTS)       = CreateAccounts
    toEnum (#const TB_OPERATION_CREATE_TRANSFERS)      = CreateTransfers
    toEnum (#const TB_OPERATION_LOOKUP_ACCOUNTS)       = LookupAccounts
    toEnum (#const TB_OPERATION_LOOKUP_TRANSFERS)      = LookupTransfers
    toEnum (#const TB_OPERATION_GET_ACCOUNT_TRANSFERS) = GetAccountTransfers
    toEnum (#const TB_OPERATION_GET_ACCOUNT_BALANCES)  = GetAccountBalances
    toEnum (#const TB_OPERATION_QUERY_ACCOUNTS)        = QueryAccounts
    toEnum (#const TB_OPERATION_QUERY_TRANSFERS)       = QueryTransfers
    toEnum unmatched = error $ "Operation.toEnum: Cannot match " ++ show unmatched

-- Status type
data Status
    = Success
    | Unexpected
    | OutOfMemory
    | AddressInvalid
    | AddressLimitExceeded
    | SystemResources
    | NetworkSubsystem
    deriving (Eq, Ord, Show)

instance Enum Status where
    fromEnum Success              = #const TB_STATUS_SUCCESS
    fromEnum Unexpected           = #const TB_STATUS_UNEXPECTED
    fromEnum OutOfMemory          = #const TB_STATUS_OUT_OF_MEMORY
    fromEnum AddressInvalid       = #const TB_STATUS_ADDRESS_INVALID
    fromEnum AddressLimitExceeded = #const TB_STATUS_ADDRESS_LIMIT_EXCEEDED
    fromEnum SystemResources      = #const TB_STATUS_SYSTEM_RESOURCES
    fromEnum NetworkSubsystem     = #const TB_STATUS_NETWORK_SUBSYSTEM

    toEnum (#const TB_STATUS_SUCCESS)                = Success
    toEnum (#const TB_STATUS_UNEXPECTED)             = Unexpected
    toEnum (#const TB_STATUS_OUT_OF_MEMORY)          = OutOfMemory
    toEnum (#const TB_STATUS_ADDRESS_INVALID)        = AddressInvalid
    toEnum (#const TB_STATUS_ADDRESS_LIMIT_EXCEEDED) = AddressLimitExceeded
    toEnum (#const TB_STATUS_SYSTEM_RESOURCES)       = SystemResources
    toEnum (#const TB_STATUS_NETWORK_SUBSYSTEM)      = NetworkSubsystem
    toEnum unmatched = error $ "Status.toEnum: Cannot match " ++ show unmatched

-- Packet status type
data PacketStatus
    = Ok
    | TooMuchData
    | ClientEvicted
    | ClientReleaseTooLow
    | ClientReleaseTooHigh
    | ClientShutdown
    | InvalidOperation
    | InvalidDataSize
    deriving (Eq, Ord, Show)

instance Enum PacketStatus where
    fromEnum Ok                   = #const TB_PACKET_OK
    fromEnum TooMuchData          = #const TB_PACKET_TOO_MUCH_DATA
    fromEnum ClientEvicted        = #const TB_PACKET_CLIENT_EVICTED
    fromEnum ClientReleaseTooLow  = #const TB_PACKET_CLIENT_RELEASE_TOO_LOW
    fromEnum ClientReleaseTooHigh = #const TB_PACKET_CLIENT_RELEASE_TOO_HIGH
    fromEnum ClientShutdown       = #const TB_PACKET_CLIENT_SHUTDOWN
    fromEnum InvalidOperation     = #const TB_PACKET_INVALID_OPERATION
    fromEnum InvalidDataSize      = #const TB_PACKET_INVALID_DATA_SIZE

    toEnum (#const TB_PACKET_OK)                      = Ok
    toEnum (#const TB_PACKET_TOO_MUCH_DATA)           = TooMuchData
    toEnum (#const TB_PACKET_CLIENT_EVICTED)          = ClientEvicted
    toEnum (#const TB_PACKET_CLIENT_RELEASE_TOO_LOW)  = ClientReleaseTooLow
    toEnum (#const TB_PACKET_CLIENT_RELEASE_TOO_HIGH) = ClientReleaseTooHigh
    toEnum (#const TB_PACKET_CLIENT_SHUTDOWN)         = ClientShutdown
    toEnum (#const TB_PACKET_INVALID_OPERATION)       = InvalidOperation
    toEnum (#const TB_PACKET_INVALID_DATA_SIZE)       = InvalidDataSize
    toEnum unmatched = error $ "PacketStatus.toEnum: Cannot match " ++ show unmatched

unmarshallPacketStatus :: PacketStatus -> Word8
unmarshallPacketStatus = fromIntegral . fromEnum

marshallPacketStatus :: Word8 -> PacketStatus
marshallPacketStatus = toEnum . fromIntegral

-- | Represents the tb_packet_t structure from TigerBeetle
data Packet = Packet
    { next        :: Ptr Packet      -- ^ next pointer
    , userData    :: Ptr ()          -- ^ user_data void pointer
    , operation   :: Word8           -- ^ operation field
    , status      :: T Word8 PacketStatus    -- ^ status field
    , dataSize    :: Word32          -- ^ data_size field
    , packetData  :: Ptr ()          -- ^ data void pointer
    , batchNext   :: Ptr Packet      -- ^ batch_next pointer
    , batchTail   :: Ptr Packet      -- ^ batch_tail pointer
    , batchSize   :: Word32          -- ^ batch_size field
    , batchAllowed:: Word8           -- ^ batch_allowed field
    , reserved    :: [Word8]         -- ^ reserved array [7]
    } deriving (Show)

instance Storable Packet where
    sizeOf _ = #{size tb_packet_t}

    alignment _ = #{alignment tb_packet_t}

    peek ptr = do
      pReserved <- forM [0..6] $ \i ->
        peekByteOff (#{ptr tb_packet_t, reserved} ptr) i
      Packet
        <$> #{peek tb_packet_t, next} ptr
        <*> #{peek tb_packet_t, user_data} ptr
        <*> #{peek tb_packet_t, operation} ptr
        <*> #{peek tb_packet_t, status} ptr
        <*> #{peek tb_packet_t, data_size} ptr
        <*> #{peek tb_packet_t, data} ptr
        <*> #{peek tb_packet_t, batch_next} ptr
        <*> #{peek tb_packet_t, batch_tail} ptr
        <*> #{peek tb_packet_t, batch_size} ptr
        <*> #{peek tb_packet_t, batch_allowed} ptr
        <*> pure pReserved

    poke ptr packet = do
        #{poke tb_packet_t, next} ptr packet.next
        #{poke tb_packet_t, user_data} ptr packet.userData
        #{poke tb_packet_t, operation} ptr packet.operation
        #{poke tb_packet_t, status} ptr packet.status
        #{poke tb_packet_t, data_size} ptr packet.dataSize
        #{poke tb_packet_t, data} ptr packet.packetData
        #{poke tb_packet_t, batch_next} ptr packet.batchNext
        #{poke tb_packet_t, batch_tail} ptr packet.batchTail
        #{poke tb_packet_t, batch_size} ptr packet.batchSize
        #{poke tb_packet_t, batch_allowed} ptr packet.batchAllowed
        let reservedPtr = #{ptr tb_packet_t, reserved} ptr
        forM_ (zip [0..] packet.reserved) $ \(i, value) ->
            pokeByteOff reservedPtr i value

data TbAccount
  = TbAccount
  { accountId        :: Word128
  , debitsPending    :: Word128
  , debitsPosted     :: Word128
  , creditsPending   :: Word128
  , creditsPosted    :: Word128
  , userData128      :: Word128
  , userData64       :: Word64
  , userData32       :: Word32
  , accountReserved  :: Word32
  , ledger           :: Word32
  , code             :: Word16
  , flags            :: Word16
  , accountTimestamp :: Word64
  }
  deriving (Eq, Show)

instance Storable TbAccount where
    sizeOf _ = #{size tb_account_t}

    alignment _ = #{alignment tb_account_t}

    peek ptr
      = TbAccount
      <$> #{peek tb_account_t, id} ptr
      <*> #{peek tb_account_t, debits_pending} ptr
      <*> #{peek tb_account_t, debits_posted} ptr
      <*> #{peek tb_account_t, credits_pending} ptr
      <*> #{peek tb_account_t, credits_posted} ptr
      <*> #{peek tb_account_t, user_data_128} ptr
      <*> #{peek tb_account_t, user_data_64} ptr
      <*> #{peek tb_account_t, user_data_32} ptr
      <*> #{peek tb_account_t, reserved} ptr
      <*> #{peek tb_account_t, ledger} ptr
      <*> #{peek tb_account_t, code} ptr
      <*> #{peek tb_account_t, flags} ptr
      <*> #{peek tb_account_t, timestamp} ptr

    poke ptr account = do
        #{poke tb_account_t, id} ptr account.accountId
        #{poke tb_account_t, debits_pending} ptr account.debitsPending
        #{poke tb_account_t, debits_posted} ptr account.debitsPosted
        #{poke tb_account_t, credits_pending} ptr account.creditsPending
        #{poke tb_account_t, credits_posted} ptr account.creditsPosted
        #{poke tb_account_t, user_data_128} ptr account.userData128
        #{poke tb_account_t, user_data_64} ptr account.userData64
        #{poke tb_account_t, user_data_32} ptr account.userData32
        #{poke tb_account_t, reserved} ptr account.accountReserved
        #{poke tb_account_t, ledger} ptr account.ledger
        #{poke tb_account_t, code} ptr account.code
        #{poke tb_account_t, flags} ptr account.flags
        #{poke tb_account_t, timestamp} ptr account.accountTimestamp

-- Helper functions for packet creation and manipulation
allocaPacket :: (Ptr Packet -> IO a) -> IO a
allocaPacket = allocaBytes #{size tb_packet_t}

newPacket :: IO (Ptr Packet)
newPacket = callocBytes #{size tb_packet_t}

freePacket :: Ptr Packet -> IO ()
freePacket = free

type Client = Ptr ()
type CallbackContext = CUIntPtr

-- Completion callback type
type CompletionCallback =
    CallbackContext ->  -- Context
    Client ->           -- Client
    Ptr Packet ->       -- Packet
    Word32 ->           -- Reserved
    Ptr Word8 ->        -- Data
    CUInt ->            -- Data size
    IO ()

-- Foreign imports

foreign import ccall safe "tb_client.h tb_client_init"
    tb_client_init_f :: Ptr Client               -- out_client
                  -> Ptr Word8                 -- cluster_id
                  -> CString                   -- address_ptr
                  -> Word32                    -- address_len
                  -> CallbackContext           -- on_completion_ctx
                  -> FunPtr CompletionCallback -- on_completion
                  -> IO CInt
tb_client_init
  :: Ptr Client
  -> Ptr Word8
  -> CString
  -> Word32
  -> CallbackContext
  -> FunPtr CompletionCallback
  -> IO Status
tb_client_init client clusterId addr addrLen ctx cb =
    toEnum . fromIntegral <$> tb_client_init_f client clusterId addr addrLen ctx cb

foreign import ccall safe "tb_client.h tb_client_init_echo"
    tb_client_init_echo_f :: Ptr Client             -- out_client
                       -> Ptr Word8                 -- cluster_id
                       -> CString                   -- address_ptr
                       -> Word32                    -- address_len
                       -> CallbackContext           -- on_completion_ctx
                       -> FunPtr CompletionCallback -- on_completion
                       -> IO CInt

tb_client_init_echo
  :: Ptr Client
  -> Ptr Word8
  -> CString
  -> Word32
  -> CallbackContext
  -> FunPtr CompletionCallback
  -> IO Status
tb_client_init_echo client clusterId addr addrLen ctx cb =
    toEnum . fromIntegral <$> tb_client_init_echo_f client clusterId addr addrLen ctx cb

foreign import ccall safe "tb_client.h tb_client_completion_context"
    tb_client_completion_context :: Client -> IO CUIntPtr

foreign import ccall safe "tb_client.h tb_client_submit"
    tb_client_submit :: Client -> Ptr Packet -> IO ()

foreign import ccall safe "tb_client.h tb_client_deinit"
    tb_client_deinit :: Client -> IO ()

foreign import ccall "wrapper"
    makeCompletionCallback :: CompletionCallback -> IO (FunPtr CompletionCallback)
