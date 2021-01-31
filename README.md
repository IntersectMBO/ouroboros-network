[![Build Status](https://badge.buildkite.com/3c5e581fd69202ceddd64e91351846c41baa285aaca835cdd9.svg?style=flat-square&branch=master)](https://buildkite.com/input-output-hk/ouroboros-network)

# Ouroboros-Network

* io-sim - `IOSim` simulator monad which supports asynchronous exceptions,
  `STM` transactions and `async` interface, timers.
* io-sim-classes - type classes, all of them have instance for both `IOSim` and
  `IO`.
* `typed-protocols` - session type framework with support of [protocol pipelining](https://en.wikipedia.org/wiki/Protocol_pipelining)
  * See 45min Haskell eXchange 2019 [talk](https://skillsmatter.com/skillscasts/14633-45-minute-talk-by-duncan-coutts) by @dcoutts.
  * See three 50min Monadic Party 2019 workshop talks by @coot: [Part 1](https://www.youtube.com/watch?v=j8gza2L61nM), [Part 2](https://www.youtube.com/watch?v=oV6KSl1srL8), [Part 3](https://www.youtube.com/watch?v=nOIQCRPwmPA).
* `ouroboros-network`- ouroboros network package which implements protocols
  which to run ouroboros family of protocols, multiplexing layer.
* The [`byron-proxy`](https://github.com/input-output-hk/cardano-byron-proxy) is a network protocol proxy between Byron and Shelley.
  It now lives in a separate repository.

## Ouroboros-Network Documentation

We have two documents which describe various levels of the networking layer of
the Cardano Shelley implementation:

* [Introduction to the design of Data Diffusion and Networking of Cardano Shelley](https://hydra.iohk.io/job/Cardano/ouroboros-network/native.network-docs.x86_64-linux/latest/download/1)

  This document explains the technical requirements and key constraints for the networking
  layer of the _Cardano Shelley_ implementation of _Ouroboros Praos_.  This is
  a design document.

* [The Shelley Networking Protocol](https://hydra.iohk.io/job/Cardano/ouroboros-network/native.network-docs.x86_64-linux/latest/download/2)

  This document is a technical specification of the networking protocol.  It
  includes serialisation formats, necessary details of multiplexer and
  technical specifications of mini-protocols used by either _node-to-node_ and
  _node-to-client_ flavours of the protocol.

* [Haddock documentation](https://input-output-hk.github.io/ouroboros-network/)

## Ouroboros-Network API

The API consists of three layers:

• mini-protocol api's, which are GADTs for each mini-protocol under `Ouroboros.Network.Protocol`; this hides heavy type machinery of session types.  One only needs the typed `Peer` type  when one is using `runPeer` or `runPeerPipelined` function and each protocol exposes a function to create it (e.g. `Ouroboros.Network.Protocol.ChainSync.Client.chainSyncClientPeer`)

• callback `ptcl -> channel -> m ()` where `ptcl` is enumeration for each mini-protocol, this is either `NodeToNodeProtocols` or `NodeToClientProtocols`.  The callback is wrapped in `OuroborosApplication` GADT which allows to differentiate the initiator / responder (or client / server) callbacks.

• versioning which is a map from version numbers to the above callbacks and version data (the tricky part here is that version data type can be different between different versions; there is a simple way of building this map using a semigroup). You can use `simpleSingletonVersion` if your application does not depend on negotiated version data.  However, `Ouroboros.Network.NodeToNode` and `Ouroboros.Network.NodeToClient` expose `V1` api which hides versioning from the caller.

## Demo applications

* [demo-chain-sync](https://github.com/input-output-hk/ouroboros-network/wiki/Ouroboros-Network-Demo)
* [cardano-ping](https://github.com/input-output-hk/ouroboros-network/blob/master/network-mux/demo/cardano-ping.hs)
* [mux-demo](https://github.com/input-output-hk/ouroboros-network/blob/master/network-mux/demo/mux-demo.hs)
* [demo-ping-pong](https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-network-framework/demo/ping-pong.hs)
* [named-pipe-demo](https://github.com/input-output-hk/ouroboros-network/blob/master/Win32-network/demo/named-pipe-demo.hs) (Windows only)
* [demo-ntp-client](https://github.com/input-output-hk/ouroboros-network/blob/master/ntp-client/demo/Main.hs)

<details>
<summary>Instructions</summary>
To run a demo type:

```
cabal run <DEMO_NAME> --
```

After `--` you will need to pass arguments, when a demo is run without
arguments it will specify what arguments it needs.
</details>

# Ouroboros-Consensus

Consensus layer of the family Ouroboros blockchain protocols.

## Ouroboros-Consensus Documentation

The `ouroboros-consensus/docs` folder contains documentation about the
consensus layer. Start with the
[README.md](https://github.com/input-output-hk/ouroboros-network/ouroboros-consensus/docs/Contributing.md).

* [The Cardano Consensus and Storage Layer](https://hydra.iohk.io/job/Cardano/ouroboros-network/native.consensus-docs.x86_64-linux/latest/download/1)

  This technical report explains the design of the consensus and storage layer.

## Formatting

The consensus team uses `stylish-haskell` >= 0.11.0.0 to format its code. This
is enforced by CI.

Either enable editor integration or run the following command to manually
format all of the consensus code (but not the network code):

```bash
stylish-haskell -i `git ls-files -- 'ouroboros-consensus*/*.hs' | grep -v Setup.hs`
```

Alternatively, call the script used by CI itself:
https://github.com/input-output-hk/ouroboros-network/blob/master/scripts/buildkite/check-stylish.sh

```bash
./scripts/buildkite/check-stylish.sh
```

When using Nix, you can use the following command, which will build and use
the right version of `stylish-haskell`.

```bash
nix-shell --run ./scripts/buildkite/check-stylish.sh
```
