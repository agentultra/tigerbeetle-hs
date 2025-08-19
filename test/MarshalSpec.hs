{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module MarshalSpec where

import Control.Monad.IO.Class
import Database.TigerBeetle.Internal.FFI.Account
import Database.TigerBeetle.Internal.FFI.Client
import Database.TigerBeetle.Raw.Account
import Foreign.Storable
import Test.Hspec

spec :: Spec
spec =
  describe "createAccountsPacket" $ do
    it "packet should have dataSize of sizeOf(TbAccount)" $ do
      zeroAccount <- liftIO $ zeroTBAccount
      packetPtr <-
        liftIO $
          createAccountsPacket
            [ zeroAccount{tbAccountId = 1}
            ]
      packet <- peek packetPtr
      packet.tbPacketDataSize `shouldBe` fromIntegral (sizeOf @TBAccount zeroAccount)
