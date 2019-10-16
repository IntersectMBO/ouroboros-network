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
  It now lives in a seaprate repository.

## Ouroboros-Network API

The API consisists of three layers:

• mini-protocol api's, which are GADTs for each mini-protocol under `Ouroboros.Network.Protocol`; this hides heavy type machinery of session types.  One only needs the typed `Peer` type  when one is using `runPeer` or `runPeerPipelined` function and each protocol exposes a function to create it (e.g. `Ouroboros.Network.Protocol.ChainSync.Client.chainSyncClientPeer`)

• callback `ptcl -> channel -> m ()` where `ptcl` is enumeration for each mini-protoicol, this is either `NodeToNodeProtocols` or `NodeToClientProtocols`.  The callback is wrapped in `OuroborosApplication` GADT which allows to differentiate the initiator / responder (or client / server) callbacks.

• versioning which is a map from version numbers to the above callbacks and version data (the tricky part here is that version data type can be different between different versions; there is a simple way of building this map using a semigroup). You can use `simpleSingletonVersion` if your application does not depend on negotated version data.  However, `Ouroboros.Network.NodeToNode` and `Ouroboros.Network.NodeToClient` expose `V1` api which hides versioning from the caller.

## Demo application

You can run a demo application, check
[chain-sync-demo](https://github.com/input-output-hk/ouroboros-network/wiki/Ouroboros-Network-Demo)
wiki page.

## Tests

### Typed Protocols test suite
```
cabal new-run pkg:typed-protocols:tests
```
or with `nix`
```
nix-build -A nix-tools.tests.ouroboros-network
```
### IOSim test suite
```
cabal new-run pkg:io-sim:tests
```
or with `nix`
```
nix-build -A nix-tools.tests.io-sim
```
### Ouroboros-Network test suite
```
cabal new-run pkg:ouroboros-network:tests
```
or with `nix`
```
nix-build -A nix-tools.tests.ouroboros-network
```
### Ouroboros-Consensus test suite
```
cabal new-run pkg:ouroboros-consensus:tests
```
or with `nix`
```
nix-build -A nix-tools.tests.ouroboros-consensus
```

# Ouroboros-Consensus

Consensus layer of the family Ouroboros blockchain protocols.

## Tests

### Consensus test suite
```
cabal new-run pkg:ouroboros-consensus:test-consensus
```
or with `nix`
```
nix-build -A nix-tools.tests.ouroboros-consensus.test-consensus
```
### Storage test suite
```
cabal new-run pkg:ouroboros-consensus:test-storage
```
or with `nix`
```
nix-build -A nix-tools.tests.ouroboros-consensus.test-storage
```
