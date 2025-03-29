{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Internal.FFI where

import Data.Word
import Data.WideWord
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.Storable
import Data.Vector (Vector)
import Data.Vector qualified as V

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
data InitStatus
    = Success
    | Unexpected
    | OutOfMemory
    | AddressInvalid
    | AddressLimitExceeded
    | SystemResources
    | NetworkSubsystem
    deriving (Eq, Ord, Show)

instance Enum InitStatus where
    fromEnum Success              = #const TB_INIT_SUCCESS
    fromEnum Unexpected           = #const TB_INIT_UNEXPECTED
    fromEnum OutOfMemory          = #const TB_INIT_OUT_OF_MEMORY
    fromEnum AddressInvalid       = #const TB_INIT_ADDRESS_INVALID
    fromEnum AddressLimitExceeded = #const TB_INIT_ADDRESS_LIMIT_EXCEEDED
    fromEnum SystemResources      = #const TB_INIT_SYSTEM_RESOURCES
    fromEnum NetworkSubsystem     = #const TB_INIT_NETWORK_SUBSYSTEM

    toEnum (#const TB_INIT_SUCCESS)                = Success
    toEnum (#const TB_INIT_UNEXPECTED)             = Unexpected
    toEnum (#const TB_INIT_OUT_OF_MEMORY)          = OutOfMemory
    toEnum (#const TB_INIT_ADDRESS_INVALID)        = AddressInvalid
    toEnum (#const TB_INIT_ADDRESS_LIMIT_EXCEEDED) = AddressLimitExceeded
    toEnum (#const TB_INIT_SYSTEM_RESOURCES)       = SystemResources
    toEnum (#const TB_INIT_NETWORK_SUBSYSTEM)      = NetworkSubsystem
    toEnum unmatched = error $ "InitStatus.toEnum: Cannot match " ++ show unmatched

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

-- | Represents the tb_packet_t structure from TigerBeetle
data TBPacket = TBPacket
    { tbPacketUserData    :: Ptr ()          -- ^ user_data void pointer
    , tbPacketData        :: Ptr ()          -- ^ data void pointer
    , tbPacketDataSize    :: Word32          -- ^ data_size field
    , tbPacketUserTag     :: Word16          -- ^ user_tag field
    , tbPacketOperation   :: Word8           -- ^ operation field
    , tbPacketStatus      :: PacketStatus    -- ^ status field
    , tbPacketOpaque      :: Vector Word8    -- ^ opaque array [32]
    } deriving (Show)

instance Storable TBPacket where
    sizeOf _ = #{size tb_packet_t}

    alignment _ = #{alignment tb_packet_t}

    peek ptr = do
      let opaquePtr = #{ptr tb_packet_t, opaque} ptr
      tbPacketUserData  <- #{peek tb_packet_t, user_data} ptr
      tbPacketData      <- #{peek tb_packet_t, data} ptr
      tbPacketDataSize  <- #{peek tb_packet_t, data_size} ptr
      tbPacketUserTag   <- #{peek tb_packet_t, user_tag} ptr
      tbPacketOperation <- #{peek tb_packet_t, operation} ptr
      tbPacketStatus    <- toEnum <$> #{peek tb_packet_t, status} ptr
      tbPacketOpaque    <- V.generateM 31 (peekByteOff opaquePtr)
      pure TBPacket{..}

    poke ptr packet = do
        #{poke tb_packet_t, user_data} ptr packet.tbPacketUserData
        #{poke tb_packet_t, data} ptr packet.tbPacketData
        #{poke tb_packet_t, data_size} ptr packet.tbPacketDataSize
        #{poke tb_packet_t, user_tag} ptr packet.tbPacketUserTag
        #{poke tb_packet_t, operation} ptr packet.tbPacketOperation
        #{poke tb_packet_t, status} ptr (fromEnum packet.tbPacketStatus)
        let opaquePtr = #{ptr tb_packet_t, opaque} ptr
        V.iforM_ packet.tbPacketOpaque (pokeByteOff opaquePtr)

data TbAccount
  = TbAccount
  { tbAccountId             :: Word128
  , tbAccountDebitsPending  :: Word128
  , tbAccountDebitsPosted   :: Word128
  , tbAccountCreditsPending :: Word128
  , tbAccountCreditsPosted  :: Word128
  , tbAccountUserData128    :: Word128
  , tbAccountUserData64     :: Word64
  , tbAccountUserData32     :: Word32
  , tbAccountReserved       :: Word32
  , tbAccountLedger         :: Word32
  , tbAccountCode           :: Word16
  , tbAccountFlags          :: Word16
  , tbAccountTimestamp      :: Word64
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
        #{poke tb_account_t, id} ptr account.tbAccountId
        #{poke tb_account_t, debits_pending} ptr account.tbAccountDebitsPending
        #{poke tb_account_t, debits_posted} ptr account.tbAccountDebitsPosted
        #{poke tb_account_t, credits_pending} ptr account.tbAccountCreditsPending
        #{poke tb_account_t, credits_posted} ptr account.tbAccountCreditsPosted
        #{poke tb_account_t, user_data_128} ptr account.tbAccountUserData128
        #{poke tb_account_t, user_data_64} ptr account.tbAccountUserData64
        #{poke tb_account_t, user_data_32} ptr account.tbAccountUserData32
        #{poke tb_account_t, reserved} ptr account.tbAccountReserved
        #{poke tb_account_t, ledger} ptr account.tbAccountLedger
        #{poke tb_account_t, code} ptr account.tbAccountCode
        #{poke tb_account_t, flags} ptr account.tbAccountFlags
        #{poke tb_account_t, timestamp} ptr account.tbAccountTimestamp

-- Helper functions for packet creation and manipulation
allocaPacket :: (Ptr TBPacket -> IO a) -> IO a
allocaPacket = allocaBytes #{size tb_packet_t}

newPacket :: IO (Ptr TBPacket)
newPacket = callocBytes #{size tb_packet_t}

freePacket :: Ptr TBPacket -> IO ()
freePacket = free

type Client = Ptr ()
type CallbackContext = CUIntPtr

-- Completion callback type
type CompletionCallback =
    CallbackContext ->  -- Context
    Client ->           -- Client
    Ptr TBPacket ->       -- Packet
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
  -> IO InitStatus
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
  -> IO InitStatus
tb_client_init_echo client clusterId addr addrLen ctx cb =
    toEnum . fromIntegral <$> tb_client_init_echo_f client clusterId addr addrLen ctx cb

foreign import ccall safe "tb_client.h tb_client_completion_context"
    tb_client_completion_context :: Client -> IO CUIntPtr

foreign import ccall safe "tb_client.h tb_client_submit"
    tb_client_submit :: Client -> Ptr TBPacket -> IO ()

foreign import ccall safe "tb_client.h tb_client_deinit"
    tb_client_deinit :: Client -> IO ()

foreign import ccall "wrapper"
    makeCompletionCallback :: CompletionCallback -> IO (FunPtr CompletionCallback)

newtype ClientId = ClientId { getClientId :: Ptr Word8 }

newClientId :: Word128 -> IO ClientId
newClientId (Word128 hi lo) = do
  idPtr <- callocBytes 16
  poke idPtr hi
  poke idPtr lo
  pure $ ClientId $ castPtr @Word64 @Word8 idPtr
