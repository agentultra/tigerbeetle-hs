{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Database.TigerBeetle.Client
import Database.TigerBeetle.Client.Account
import Database.TigerBeetle.Client.Sync qualified as Sync

main :: IO ()
main = do
  result <- Sync.withClient (ClusterId 0) (Address "3000") $ do
    Sync.createAccounts [CreateAccount 1 1]
  print result
