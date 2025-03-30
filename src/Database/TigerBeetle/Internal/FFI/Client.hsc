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
import Data.Vector (Vector)
import Data.Vector qualified as V

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
