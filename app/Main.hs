{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Database.TigerBeetle.Client
import Database.TigerBeetle.Client.Account
import Database.TigerBeetle.Client.Sync qualified as Sync

main :: IO ()
main = do
  result <- Sync.withClient (ClusterId 0) (Address "3000") $ do
    -- This should return an error from the server.. neither id nor ledger can be zero
    Sync.createAccounts [CreateAccount (AccountId 9) 9 1]
  print result
