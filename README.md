# tigerbeetle-hs #

An unofficial community Haskell client for the financial transaction
database, [Tigerbeetle](https://tigerbeetle.com/).

The current release supports Tigerbeetle `0.16.33`.

This project will aim to support the upstream release cycle and
version policies in future releases.

The aim of this client library is to provide a high-level,
Haskell-friendly API as well as access to lower level bindings in case
your project needs them.

It is not a full-featured application framework.

It is a good starting point for building a framework.  Or a snappy
tool.

## Sync Client

The synchronous client interface blocks and awaits the result of each
command sent to the server.  This is useful mainly for prototyping,
basic scripting, and exploring the API `ghci`.

Example usage:

``` haskell
module Main where

import Database.TigerBeetle.Account
import Database.TigerBeetle.Client
import qualified Database.TigerBeetle.Client.Sync qualified as Sync

main :: IO ()
main = do
  withClient (ClusterId 0) (Address "3000") $ do
    result <- createAccounts [CreateAccount (AccountId 0) (LedgerId 0) (AccountCode 100)]
    print result
```

It is important to note that each action, such as `createAccounts`,
will block and await the response from the server.

## Async Client

The asynchronous client interface is what most application developers
should use.  It is minimal and requires you to write your own loop and
manage requests.

Example usage:

``` haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Database.TigerBeetle.Account
import Database.TigerBeetle.Client
import qualified Database.TigerBeetle.Client.Async as Async
import Database.TigerBeetle.Response

main :: IO ()
main = do
  result <- newTVarIO Nothing

  -- When we initialize a thread, this context will associate responses from the
  -- server to the thread that issued the command.
  let mainContext = Async.ThreadContext 0

  let completionCallback _ response = do
        atomically $ writeTVar result (Just response)

  -- In this example we're awaiting all responses on the main thread but we
  -- could assemble our main loop to run more work in parallel across more
  -- threads as needed.
  Async.withClient (ClusterId 0) (Address "3000") mainContext completionCallback $ do
    Async.createAccounts [CreateAccount (AccountId 9) (LedgerId 9) (AccountCode 1)]

  -- For example, instead of awaiting the result we could feed results from
  -- child threads over an STM channel or queue.
  --
  -- For this example though we await the result on our TVar.
  await result

await :: TVar (Maybe Response) -> IO ()
await result = do
  r <- readTVarIO result
  case r of
    Nothing -> threadDelay 3000 >> await result
    Just yay -> print yay
```

This API enables you to build the event loop suitable for your
application.  Be aware that your callback will need to match responses
to commands on your own.

Future versions of this library may include a framework based on `STM`
that will give you a common setup based on this API.

## Commands

The essential commands are available:

- Create Accounts
- Create Transfers

And the essential queries:

- Lookup Accounts
- Lookup Transfers
- Get Account Balances
- Get Account Transfers
- Query Accounts
- Query Transfers

What's not supported (yet):

- _User data_: The Tigerbeetle client has fields for user metadata.
  We're planning on supporting this in a Haskell friendly way in the
  future.

## Reporting Issues

Please feel free to report issues or suggest improvements via Github
Issues.

Include as much detail and context as you can.

## Contributing

We welcome contributions from all interested hackers and developers of
all skill levels.

Interested in contributing? Check out [CONTRIBUTING](CONTRIBUTING.md).
