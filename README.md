# tigerbeetle-hs #

An unofficial community Haskell client for the financial transaction
database, [Tigerbeetle](https://tigerbeetle.com/).

The current release supports Tigerbeetle `0.16.67`.

Note that at this stage the API is subject to change. Consider this
experimental.  We will aim to make major version releases have a
stable API.

This project will aim to support the upstream release cycle and
version policies in future releases.

The aim of this client library is to provide a high-level,
Haskell-friendly API as well as access to lower level bindings in case
your project needs them.

It is not a full-featured application framework.

It is a good starting point for building a framework.  Or a snappy
tool.

## Prerequisites

You will need to compile and install the Tigerbeetle C client library
with `pkg-config`.

Due to [this
bug](https://github.com/tigerbeetle/tigerbeetle/issues/2865) in
Tigerbeetle, depending on the version of Zig you compile with, you may
need to patch the shared object with with `patchelf`:

``` bash
patchelf --add-needed libm.so.6 /path/to/libtb_client.so
```

This may be resolved when compiling with a newer version of Zig which
will correctly mark certain symbols in the object file's symbol table
as `HIDDEN` preventing the link error you may encounter.

## Sync Client

The synchronous client interface blocks and awaits the result of each
command sent to the server.  This is useful mainly for prototyping,
basic scripting, and exploring the API in `ghci`.

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

## Raw

The modules in `Database.TigerBeetle.Raw` are meant to be wrappers for
the FFI calls.  If you want to build your own higher-level client
library you should be able to use code from this layer as a starting
point that is one level above raw FFI code.

## Reporting Issues

Please feel free to report issues or suggest improvements via Github
Issues.

Include as much detail and context as you can.

## Contributing

We welcome contributions from all interested hackers and developers of
all skill levels.

Interested in contributing? Check out [CONTRIBUTING](CONTRIBUTING.md).
