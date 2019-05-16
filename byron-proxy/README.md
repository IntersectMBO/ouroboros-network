# Byron proxy

This is a library and an executable.

The library offers:
- Definitions of `cardano-sl` `Logic` `Diffusion` based upon a special
  configuration type `ByronProxyConfig` and backed by the `DB` and `Pool`
  types (next bullet points).
- A database abstraction over `ImmutableDB` from `ouroboros-consensus`, which
  includes an index header hashes, because the Byron interface requires looking
  up blocks using header hashes alone (rather than slot indices). An
  SQLite index implementation is included.
- A type called `Pool` to facilitate the Byron relay system for atomic data
  like transactions and update proposals. These are stored for a given amount
  of time and can be looked up by a small key.
- A chain sync client and server, using the `ouroboros-network` protocol
  definition, and serving whole `cardano-sl` `Block`s.

The executable is capable of
- Downloading a chain from either a Byron peer or a new chain sync peer. Only
  one peer can be used because no concurrent writes on the database are
  supported (it's the `ImmutableDB`, it cannot handle forks).
- Serving a chain using the new chain sync server.

## How to run it

```
$ byron-proxy --help
Usage: byron-proxy [--database-path FILEPATH] [--index-path FILEPATH]
                   [--logger-config FILEPATH] [--configuration-file FILEPATH]
                   [--configuration-key TEXT] [--system-start TIMESTAMP]
                   [--configuration-seed INTEGER] --server-host ARG
                   --server-port ARG ([--remote-host ARG] [--remote-port ARG] |
                   [--topology FILEPATH] [--kademlia FILEPATH]
                   [--node-id NODE_ID] [--default-port PORT]
                   [--policies FILEPATH] [--address IP:PORT] [--listen IP:PORT])
  Store and forward blocks from a Byron or Shelley server
```

## Validator Demo

This package also provides a `validator` executable, which is designed to be a
client of `byron-proxy`. It uses the chain sync protocol to pull blocks and runs
them through the `cardano-ledger` validation code.

To run this as a demo you should launch a `byron-proxy` using
```
stack exec -- byron-proxy --database-path db-byron-proxy-demo-server \
\ --index-path index-byron-proxy-demo-server \
\ --configuration-file ../cardano-sl/lib/configuration.yaml \
\ --configuration-key mainnet_full --server-host 127.0.0.1 --server-port 7777 \
\ --topology ./topology.yaml
```
, which requires a `cardano-sl` clone in the same directory as your
`ouroboros-network` clone and a `topology.yaml` file, containing
```
wallet:
  relays: [[{ host: relays.cardano-mainnet.iohk.io }]]
```
You should see this syncing blocks from mainnet.

Then to run the validator, run
```
stack exec -- validator --server-host 127.0.0.1 --server-port 7777
```
, which requires you to have `mainnet-genesis.json` in the current directory.
You can run `cp ../cardano-ledger/mainnet-genesis.json .`, if you have a clone
of `cardano-ledger` in the same directory as the `ouroboros-network` clone.
