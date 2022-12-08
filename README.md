# Introduction

This repository contains the core components of the network & consensus code
for  the Cardano node.  It is a dependency when building the node from the
cardano-node repository.

# Ouroboros-Network

* `network-mux` - implementation of a general network muliplexer.
* `ouroboros-network-api` - shared API between `network` and `consensus` components.
* `ouroboros-network-framework` - low level network components, e.g. snockets,
  connection manager, inbound governor, handshake mini-protocol, network
  simulator. 
* `ouroboros-network-protocols` - implementation of all /node-to-node/
  & /node-to-client/ protocols.  Also contains a testing library which is
  implements various applications for testing purposes.
* `ouroboros-network`- top level integration of all network components also
  defines `node-to-node` and `node-to-client` API.  It contains the implementation
  of outbound governor.
* `ouroboros-network-mock` & `ouroboros-network-testing` - shared testing code.
* `ntp-client` - an `ntp` client (used by `cardano-wallet`).

Libraries:

* `monoidal-synchronisation` - a small standalone package which contains
  synchronisation primitives.


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

- [Official Cardano Documentation](https://docs.cardano.org/en/latest/)

  Specifically the section "Explore Cardano" is helpful, since it talks about the [Cardano Architecture](https://docs.cardano.org/explore-cardano/cardano-architecture), [Cardano Design Rationale](https://docs.cardano.org/explore-cardano/cardano-design-rationale), the [Cardano Network](https://docs.cardano.org/explore-cardano/cardano-network/about-the-cardano-network), etc.
  Although the Cardano documentation is less detailed, it is a good place to start and refresh some more basic concepts about Cardano.

## Ouroboros-Network API

The API consists of three layers:

• mini-protocol api's, which are GADTs for each mini-protocol under `Ouroboros.Network.Protocol` (defined in `ouroboros-network-protocols` package); this hides heavy type machinery of session types.  One only needs the typed `Peer` type  when one is using `runPeer` or `runPeerPipelined` function and each protocol exposes a function to create it (e.g. `Ouroboros.Network.Protocol.ChainSync.Client.chainSyncClientPeer`)

• callback `ptcl -> channel -> m ()` where `ptcl` is enumeration for each mini-protocol, this is either `NodeToNodeProtocols` or `NodeToClientProtocols`.  The callback is wrapped in `OuroborosApplication` GADT which allows to differentiate the initiator / responder (or client / server) callbacks.

• versioning which is a map from version numbers to the above callbacks and version data (the tricky part here is that version data type can be different between different versions; there is a simple way of building this map using a semigroup). You can use `simpleSingletonVersion` if your application does not depend on negotiated version data.  However, `Ouroboros.Network.NodeToNode` and `Ouroboros.Network.NodeToClient` expose `V1` api which hides versioning from the caller.

## Demo applications

* [demo-chain-sync](https://github.com/input-output-hk/ouroboros-network/wiki/Ouroboros-Network-Demo)
* [cardano-ping](https://github.com/input-output-hk/ouroboros-network/wiki/cardano-ping)
* [mux-demo](https://github.com/input-output-hk/ouroboros-network/blob/master/network-mux/demo/mux-demo.hs)
* [demo-ping-pong](https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-network-framework/demo/ping-pong.hs)
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
[README.md](https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/README.md).

* [The Cardano Consensus and Storage Layer](https://hydra.iohk.io/job/Cardano/ouroboros-network/native.consensus-docs.x86_64-linux/latest/download/1)

  This technical report explains the design of the consensus and storage layer.

## Formatting

The consensus team uses `stylish-haskell` >= 0.11.0.0 to format its code. This
is enforced by CI.

Either enable editor integration or call the script used by CI itself:

```bash
./scripts/ci/check-stylish.sh
```

When using Nix, you can use the following command, which will build and use
the right version of `stylish-haskell`.

```bash
nix-shell --run ./scripts/ci/check-stylish.sh
```
