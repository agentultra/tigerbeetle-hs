{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Database.TigerBeetle.Account
import Database.TigerBeetle.Client
import Database.TigerBeetle.Client.Async qualified as Async
import Database.TigerBeetle.Response

main :: IO ()
main = do
  result <- newTVarIO Nothing

  let completionCallback _ response = do
        atomically $ writeTVar result (Just response)

  Async.withClient (ClusterId 0) (Address "3000") (Async.ThreadContext 0) completionCallback $ do
    Async.createAccounts [CreateAccount (AccountId 9) (LedgerId 9) (AccountCode 1)]

  await result

await :: TVar (Maybe Response) -> IO ()
await result = do
  r <- readTVarIO result
  case r of
    Nothing -> threadDelay 3000 >> await result
    Just yay -> print yay
