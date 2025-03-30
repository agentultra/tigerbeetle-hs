{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Database.TigerBeetle.Internal.FFI.Client where

import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.TigerBeetle.Internal.FFI.Client.ClusterId

#include "tb_client.h"

data TBClient = TBClient
    { tbClientOpaque :: Vector Word64
    }
    deriving (Show, Eq)

instance Storable TBClient where
    sizeOf _ = #{size tb_client_t}

    alignment _ = #{alignment tb_client_t}

    peek ptr = do
      let opaquePtr = #{ptr tb_client_t, opaque} ptr
      tbClientOpaque <- V.generateM 4 (\i -> peekByteOff opaquePtr (i * 8))
      pure TBClient{..}

    poke ptr client = do
      let opaquePtr = #{ptr tb_client_t, opaque} ptr
      V.iforM_ client.tbClientOpaque $ \i val -> pokeByteOff opaquePtr (i * 8) val

data TBInitStatus =
      Success
    | Unexpected
    | OutOfMemory
    | AddressInvalid
    | AddressLimitExceeded
    | SystemResources
    | NetworkSubsystem
    deriving (Eq, Show)

instance Enum TBInitStatus where
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
    toEnum unmatched = error $ "TBInitStatus.toEnum: Cannot match " ++ show unmatched

data TBClientStatus =
      ClientOk
    | ClientInvalid
    deriving (Eq, Show)

instance Enum TBClientStatus where
    fromEnum ClientOk      = #const TB_CLIENT_OK
    fromEnum ClientInvalid = #const TB_CLIENT_INVALID

    toEnum (#const TB_CLIENT_OK)      = ClientOk
    toEnum (#const TB_CLIENT_INVALID) = ClientInvalid
    toEnum unmatched = error $ "TBClientStatus.toEnum: Cannot match " ++ show unmatched

data TBOperation =
      Pulse
    | CreateAccounts
    | CreateTransfers
    | LookupAccounts
    | LookupTransfers
    | GetAccountTransfers
    | GetAccountBalances
    | QueryAccounts
    | QueryTransfers
    | GetEvents
    deriving (Eq, Show)

instance Enum TBOperation where
    fromEnum Pulse               = #const TB_OPERATION_PULSE
    fromEnum CreateAccounts      = #const TB_OPERATION_CREATE_ACCOUNTS
    fromEnum CreateTransfers     = #const TB_OPERATION_CREATE_TRANSFERS
    fromEnum LookupAccounts      = #const TB_OPERATION_LOOKUP_ACCOUNTS
    fromEnum LookupTransfers     = #const TB_OPERATION_LOOKUP_TRANSFERS
    fromEnum GetAccountTransfers = #const TB_OPERATION_GET_ACCOUNT_TRANSFERS
    fromEnum GetAccountBalances  = #const TB_OPERATION_GET_ACCOUNT_BALANCES
    fromEnum QueryAccounts       = #const TB_OPERATION_QUERY_ACCOUNTS
    fromEnum QueryTransfers      = #const TB_OPERATION_QUERY_TRANSFERS
    fromEnum GetEvents           = #const TB_OPERATION_GET_EVENTS

    toEnum (#const TB_OPERATION_PULSE)                 = Pulse
    toEnum (#const TB_OPERATION_CREATE_ACCOUNTS)       = CreateAccounts
    toEnum (#const TB_OPERATION_CREATE_TRANSFERS)      = CreateTransfers
    toEnum (#const TB_OPERATION_LOOKUP_ACCOUNTS)       = LookupAccounts
    toEnum (#const TB_OPERATION_LOOKUP_TRANSFERS)      = LookupTransfers
    toEnum (#const TB_OPERATION_GET_ACCOUNT_TRANSFERS) = GetAccountTransfers
    toEnum (#const TB_OPERATION_GET_ACCOUNT_BALANCES)  = GetAccountBalances
    toEnum (#const TB_OPERATION_QUERY_ACCOUNTS)        = QueryAccounts
    toEnum (#const TB_OPERATION_QUERY_TRANSFERS)       = QueryTransfers
    toEnum (#const TB_OPERATION_GET_EVENTS)            = GetEvents
    toEnum unmatched = error $ "TBOperation.toEnum: Cannot match " ++ show unmatched


data TBPacketStatus =
      Ok
    | TooMuchData
    | ClientEvicted
    | ClientReleaseTooLow
    | ClientReleaseTooHigh
    | ClientShutdown
    | InvalidOperation
    | InvalidDataSize
    deriving (Eq, Show)

instance Enum TBPacketStatus where
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
    toEnum unmatched = error $ "TBPacketStatus.toEnum: Cannot match " ++ show unmatched

data TBPacket = TBPacket
    { tbPacketUserData   :: Ptr ()
    , tbPacketData       :: Ptr ()
    , tbPacketDataSize   :: Word32
    , tbPacketUserTag    :: Word16
    , tbPacketOperation  :: TBOperation
    , tbPacketStatus     :: TBPacketStatus
    , tbPacketOpaque     :: V.Vector Word8
    }
    deriving (Show, Eq)

instance Storable TBPacket where
    sizeOf _ = #{size tb_packet_t}

    alignment _ = #{alignment tb_packet_t}

    peek ptr = do
      tbPacketUserData   <- #{peek tb_packet_t, user_data} ptr
      tbPacketData       <- #{peek tb_packet_t, data} ptr
      tbPacketDataSize   <- #{peek tb_packet_t, data_size} ptr
      tbPacketUserTag    <- #{peek tb_packet_t, user_tag} ptr
      tbPacketOperation  <- toEnum <$> #{peek tb_packet_t, operation} ptr
      tbPacketStatus     <- toEnum <$> #{peek tb_packet_t, status} ptr
      let opaquePtr = #{ptr tb_packet_t, opaque} ptr
      tbPacketOpaque     <- V.generateM 32 (\i -> peekByteOff opaquePtr i)
      pure TBPacket{..}

    poke ptr packet = do
        #{poke tb_packet_t, user_data} ptr packet.tbPacketUserData
        #{poke tb_packet_t, data} ptr packet.tbPacketData
        #{poke tb_packet_t, data_size} ptr packet.tbPacketDataSize
        #{poke tb_packet_t, user_tag} ptr packet.tbPacketUserTag
        #{poke tb_packet_t, operation} ptr (fromEnum packet.tbPacketOperation)
        #{poke tb_packet_t, status} ptr (fromEnum packet.tbPacketStatus)
        let opaquePtr = #{ptr tb_packet_t, opaque} ptr
        V.iforM_ packet.tbPacketOpaque $ \i val -> pokeByteOff opaquePtr i val

type TBCompletionContext = CUIntPtr

-- TODO: add comments explaining what these represent, asked a question in TB slack 
type TBCompletionCallback =
  TBCompletionContext -> 
  Ptr TBPacket ->
  Word64 ->
  Ptr Word8 ->
  Word32 ->
  IO ()

foreign import ccall "wrapper"
    makeCompletionCallback :: TBCompletionCallback -> IO (FunPtr TBCompletionCallback)

foreign import ccall "tb_client.h tb_client_init"
    c_tb_client_init
      :: Ptr TBClient
      -> Ptr Word8
      -> CString
      -> Word32
      -> CUIntPtr 
      -> FunPtr TBCompletionCallback 
      -> IO Word32

tbClientInit
  :: Ptr TBClient
  -> ClusterId
  -> CString
  -> Word32
  -> TBCompletionContext
  -> FunPtr TBCompletionCallback
  -> IO TBInitStatus
tbClientInit client clusterId addr addrLen ctx cb =
    withClusterIdPointer clusterId $ \clusterIdPtr ->
       toEnum . fromIntegral <$> c_tb_client_init client clusterIdPtr addr addrLen ctx cb

foreign import ccall "tb_client.h tb_client_init_echo"
    c_tb_client_init_echo
      :: Ptr TBClient
      -> Ptr Word8
      -> CString
      -> Word32
      -> TBCompletionContext 
      -> FunPtr TBCompletionCallback  
      -> IO Word32

tbClientInitEcho
  :: Ptr TBClient
  -> ClusterId
  -> CString
  -> Word32
  -> TBCompletionContext
  -> FunPtr TBCompletionCallback
  -> IO TBInitStatus
tbClientInitEcho client clusterId addr addrLen ctx cb =
    withClusterIdPointer clusterId $ \clusterIdPtr ->
       toEnum . fromIntegral <$> c_tb_client_init_echo client clusterIdPtr addr addrLen ctx cb
