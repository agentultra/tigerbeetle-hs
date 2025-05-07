{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.TigerBeetle.Client

main :: IO ()
main = do
  results <- withClient 3000 (ClusterId 0) (Address "3000") $ \_ -> putStrLn "Hello, world!"
  print results

-- q <- newTQueueIO
-- print "have empty queue"
-- res <- initClient [0..15] "hello world" q
-- print res
-- forever $ do
--   print "looping"
--   threadDelay 2
