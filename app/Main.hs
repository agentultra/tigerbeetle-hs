{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad
import Database.TigerBeetle.Client
import Database.TigerBeetle.Internal.FFI.Client
import Database.TigerBeetle.Raw.Account
import Database.TigerBeetle.Raw.Client
import Foreign.ForeignPtr
import Foreign.Storable

main :: IO ()
main = do
  putStrLn "BEGIN!"
  resultVar <- newTVarIO Nothing
  cb <- initCallback $ \_ pPtr _ _ _ -> do
    p <- peek pPtr
    atomically $ writeTVar resultVar (Just p)
  (Right client) <- initClient (ClusterId 0) (Address "3000") 0 cb
  status <- withForeignPtr client $ \rawClient -> do
    acct <- zeroTBAccount
    acctPkt <- createAccountsPacket [acct]
    tbClientSubmit rawClient acctPkt
  print status
  loop client resultVar

loop :: ForeignPtr TBClient -> TVar (Maybe TBPacket) -> IO ()
loop client resultVar = do
  threadDelay 20000
  mResult <- atomically $ readTVar resultVar
  case mResult of
    Nothing -> loop client resultVar
    Just pkt -> do
      print pkt
      deinitResult <- withForeignPtr client $ \raw -> tbClientDeinit raw
      print deinitResult
