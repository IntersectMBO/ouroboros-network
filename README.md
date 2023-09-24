# [Ouroboros-Network][ouroboros-network]

[![Haskell CI](https://img.shields.io/github/actions/workflow/status/input-output-hk/ouroboros-network/build.yml?branch=master&label=Build&style=for-the-badge)](https://github.com/input-output-hk/ouroboros-network/actions/workflows/build.yml)
[![Nightly CI](https://img.shields.io/github/actions/workflow/status/input-output-hk/ouroboros-network/nightly.yml?branch=master&label=Nightly&style=for-the-badge)](https://github.com/input-output-hk/ouroboros-network/actions/workflows/nightly.yml)
[![Haddocks](https://img.shields.io/github/actions/workflow/status/input-output-hk/ouroboros-network/github-page.yml?branch=master&label=Haddocks&style=for-the-badge)](https://github.com/input-output-hk/ouroboros-network/actions/workflows/github-page.yml)
[![handbook](https://img.shields.io/badge/policy-Cardano%20Engineering%20Handbook-informational?style=for-the-badge)](https://input-output-hk.github.io/cardano-engineering-handbook)

This repository contains the core components of the network code for the Cardano
node. It is a dependency when building the node from the cardano-node
repository.

The following graph shows the dependency tree.  The top-level package is
`ouroboros-consensus-diffusion` which is part of [ouroboros-consensus]

```mermaid
  flowchart TD
    A[network-mux]                   --> B[network / Win32-network]
    M[cardano-ping]                  --> A
    D[ouroboros-network-framework]   --> A
    D                                --> E[ouroboros-network-api]
    E                                --> H[typed-protocols]
    G                                --> H[typed-protocols]
    G                                --> E
    F[ouroboros-network]             --> D
    F                                --> G[ouroboros-network-protocols]
    I[ouroboros-consensus-diffusion] --> F
    J[cardano-client]                --> F
    K[ntp-client]                    --> B

   click A "https://input-output-hk.github.io/ouroboros-network/network-mux/" _blank
   click M "https://input-output-hk.github.io/ouroboros-network/cardano-ping/" _blank
   click D "https://input-output-hk.github.io/ouroboros-network/ouroboros-network-framework/" _blank
   click E "https://input-output-hk.github.io/ouroboros-network/ouroboros-network-api/" _blank
   click F "https://input-output-hk.github.io/ouroboros-network/ouroboros-network/" _blank
   click G "https://input-output-hk.github.io/ouroboros-network/ouroboros-network-protocols/" _blank
   click I "https://github.com/input-output-hk/ouroboros-consensus/" _blank
   click J "https://input-output-hk.github.io/ouroboros-network/cardano-client/" _blank
   click K "https://input-output-hk.github.io/ouroboros-network/ntp-client/" _blank
   click H "https://github.com/input-output-hk/typed-protocols/" _blank
```

* `network-mux` - implementation of a general network multiplexer.
* `ouroboros-network-api` - shared API between `network` and `consensus` components.
* `ouroboros-network-framework` - low-level network components, e.g. snockets,
  connection manager, inbound governor, handshake mini-protocol, network
  simulator. 
* `ouroboros-network-protocols` - implementation of all /node-to-node/
  & /node-to-client/ protocols.  Also contains a testing library which is
  implementing various applications for testing purposes.
* `ouroboros-network`- top-level integration of all network components also
  defines `node-to-node` and `node-to-client` API.  It contains the implementation
  of the outbound governor.
* `ouroboros-network-mock` & `ouroboros-network-testing` - shared testing code.
* `ntp-client` - an `ntp` client (used by `cardano-wallet`).
* `cardano-ping` - a library which implements the core functionality of
  `cardano-cli ping` command.
* `cardano-client` - a subscription for `node-to-client` which wants to connect
  to a `cardano-node`.

Libraries:

* `monoidal-synchronisation` - a small standalone package which contains
  synchronisation primitives.


## Ouroboros-Network Documentation

We have two documents which describe various levels of the networking layer of
the Cardano Shelley implementation:

* [Introduction to the design of Data Diffusion and Networking of Cardano Shelley](https://input-output-hk.github.io/ouroboros-network/pdfs/network-design)

  This document explains the technical requirements and key constraints for the networking
  layer of the _Cardano Shelley_ implementation of _Ouroboros Praos_.  This is
  a design document.

* [The Shelley Networking Protocol](https://input-output-hk.github.io/ouroboros-network/pdfs/network-spec)

  This document is a technical specification of the networking protocol.  It
  includes serialisation formats, necessary details of the multiplexer and
  technical specifications of mini-protocols used by either _node-to-node_ and
  _node-to-client_ flavours of the protocol.

* [Haddock documentation][ouroboros-network]

- [Official Cardano Documentation](https://docs.cardano.org/en/latest/)

  Specifically the section "Explore Cardano" is helpful, since it talks about the [Cardano Architecture](https://docs.cardano.org/explore-cardano/cardano-architecture), [Cardano Design Rationale](https://docs.cardano.org/explore-cardano/cardano-design-rationale), the [Cardano Network](https://docs.cardano.org/explore-cardano/cardano-network/about-the-cardano-network), etc.
  Although the Cardano documentation is less detailed, it is a good place to start and refresh some more basic concepts about Cardano.

### Work progress

On a biweekly basis, we publish updates on [cardano-updates].
We are also tracking our current work in the [Ouroboros Network Project][ouroboros-network-project].
Our quarterly goals are published in the [Cardano Node Product Backlog][cardano-node-backlog].

### For Contributors

The contributing guide is available [here][contributing-guide].
The style guide is available [here][style-guide].
The code of conduct is available [here][code-of-conduct].

## Ouroboros-Network API

The API consists of three layers:

• mini-protocol APIs, which are GADTs for each mini-protocol under `Ouroboros.Network.Protocol` (defined in `ouroboros-network-protocols` package); this hides heavy type machinery of session types.  One only needs the [`Peer`] or [`PeerPipelined`] type  when one is using [`runPeer`] or [`runPeerPipelined`] function and each protocol exposes a function to create it (e.g. [`chainSyncClientPeer`].  There is also API which allows to run a [`Peer`] or [`PipelinedPeer`] with limits (i.e. per state timeouts & per message size limits).

• callback [`MiniProtocolCb`].  The callback is wrapped in `OuroborosApplication` GADT which allows to differentiate the initiator/responder (or client/server) callbacks.

• versioning which is a map from version numbers to the above callbacks and version data (the tricky part here is that version data type can be different between different versions; there is a simple way of building this map using a semigroup). You can use [`simpleSingletonVersion`] if your application does not depend on negotiated version data.  However, [`Ouroboros.Network.NodeToNode`] and [`Ouroboros.Network.NodeToClient`] expose API which hides versioning from the caller.


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

[ouroboros-consensus]: https://github.com/input-output-hk/ouroboros-consensus
[ouroboros-network]: https://input-output-hk.github.io/ouroboros-network
[cardano-updates]: https://input-output-hk.github.com/cardano-updates
[ouroboros-network-project]: https://github.com/orgs/input-output-hk/projects/19/views/23
[cardno-node-backlog]: https://github.com/orgs/input-output-hk/projects/39/views/30
[contributing-guide]: ./CONTRIBUTING.md
[code-of-conduct]: ./CODE_OF_CONDUCT.md
[style-guide]: ./docs/StyleGuide.md
[`MiniProtocolCb`]: https://input-output-hk.github.io/ouroboros-network/ouroboros-network-framework/Ouroboros-Network-Mux.html#t:MiniProtocolCb
[`Peer`]: https://input-output-hk.github.io/typed-protocols/typed-protocols/Network-TypedProtocol-Core.html#t:Peer
[`PeerPipelines`]: https://input-output-hk.github.io/typed-protocols/typed-protocols/Network-TypedProtocol-Pipelined.html#t:PeerPipelined
[`runPeer`]: https://input-output-hk.github.io/ouroboros-network/ouroboros-network-framework/Ouroboros-Network-Driver.html#v:runPeer
[`runPipelinedPeer`]: https://input-output-hk.github.io/ouroboros-network/ouroboros-network-framework/Ouroboros-Network-Driver.html#v:runPipelinedPeer
[`chainSyncClientPeer`]: https://input-output-hk.github.io/ouroboros-network/ouroboros-network-protocols/Ouroboros-Network-Protocol-ChainSync-Client.html#v:chainSyncClientPeer
[`OuroborosApplication`]: https://input-output-hk.github.io/ouroboros-network/ouroboros-network-framework/Ouroboros-Network-Mux.html#t:OuroborosApplication
[`simpleSingletonVersion`]: https://input-output-hk.github.io/ouroboros-network/ouroboros-network-framework/Ouroboros-Network-Protocol-Handshake-Version.html#v:simpleSingletonVersions
[`Ouroboros.Network.NodeToNode`]: https://input-output-hk.github.io/ouroboros-network/ouroboros-network/Ouroboros-Network-NodeToNode.html
[`Ouroboros.Network.NodeToClient`]: https://input-output-hk.github.io/ouroboros-network/ouroboros-network/Ouroboros-Network-NodeToClient.html
