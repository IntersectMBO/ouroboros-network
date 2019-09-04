[![Build Status](https://badge.buildkite.com/3c5e581fd69202ceddd64e91351846c41baa285aaca835cdd9.svg?style=flat-square&branch=master)](https://buildkite.com/input-output-hk/ouroboros-network)

# Ouroboros-Network

* io-sim - `IOSim` simulator monad which supports asynchronous exceptions,
  `STM` transactions and `async` interface, timers. 
* io-sim-classes - type classes, all of them have instance for both `IOSim` and
  `IO`.
* `typed-protocols` - session type framework with support of [protocol pipelining](https://en.wikipedia.org/wiki/Protocol_pipelining)
* `ouroboros-network`- ouroboros network package which implements protocols
  which to run ouroboros family of protocols, multiplexing layer.
* The [`byron-proxy`](https://github.com/input-output-hk/cardano-byron-proxy) is a network protocol proxy between Byron and Shelley.
  It now lives in a seaprate repository.

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
