# Revision history for ouroboros-network

## next release

### Breaking changes

### Non-breaking changes

* Fix `KnownPeers.insert` function semantic bug where it could easily misused,
  overwriting values.
* Made (light) peer sharing results advertisable unless already known
* Peer sharing is now delayed for 5minutes for newly established peers.
* `policyPeerShareRetryTime` to 900s

## 0.10.2.0 -- 2023-12-14

## 0.10.1.0 -- 2023-11-29

### Non-breaking changes

* Fixed a bug in `outbound-governor`: PR #4748.  In rare cases the Outbound
  Governor could lose track of a connection, and thus not being able to
  reconnect to a remote peer.

## 0.10.1.0 -- 2023-11-29

### Non-breaking changes

* Fix random selection of peers to peer share with.
* Fix asynchronous demotions to `Cold` (`CoolingToCold`) not being noticed, hence
  making peers stay in the `inProgressDemotedToCold` set forever.
* Fixed bug where peers with `DoNotAdvertisePeer` flag were being shared
* Fixed peer sharing pool of peers to be shared being confused with the pool
  of peers to request to.

## 0.10.0.1 -- 2023-11-16

### Non-breaking changes

* Use `io-sim-1.3.0.0`.

## 0.10.0.0 -- 2023-11-02

### Breaking changes

* Make chainsync idle timeout configurable.

### Non-breaking changes

* Updated types to accommodate `PeerSharing` data type changes.
* Fixed PeerSharing IPv6 enc/decoding
* Introduce NodeToNodeVersion 13
* Updated types to accommodate `PeerSharing` data type changes:
  * `PeerSharingPrivate` got removed and hence, handshake is now symmetric,
  fixing issue [#4642](https://github.com/input-output-hk/ouroboros-network/issues/4642)
  * This implies that newer peer sharing node versions will see older
  version's `PeerSharingPrivate` as `PeerSharingEnabled`. So older version
  node's should not rely on `PeerSharingPrivate` semantics from newer version
  honest nodes.

* Changed encoding of IPv6 addresses sent over `PeerSharing` mini-protocol.

## 0.9.2.0 -- 2023-10-26

### Breaking changes

* Refactors `PeerSelection.RootPeersDNS` module, enabling more sharing between
  modules and providing just better module organisation overall.
    * Tweaks exports and imports
    * Shares semaphores with `withPeerSelectionActions` and `ledgerPeersThread`

### Non-breaking changes

* Updated KeepAlive client to collect a rtt sample for the first packet.
* Less aggresive churning of established and known peers.
* Added peer sharing to wireshark dissector.
* Added ledger peers to diffusion simulation
* Fixed diffusion tests.
* `demo-chain-sync`: added option parser, added new options.
* Lifted `chainGenerator` to be `Infinite`.
* Strengthened precondition in `pickPeers` to check that the peers to be picked
  from are a subset of the `PeerSelectionState` used to analyze them.
  - This is non-breaking because it reflects the current reality of how peers
    are chosen from `PeerSelectionState`.
* Restructured `txSubmissionOutbound` to prevent an impossible case and
  eliminate the associated error.

* Increase keyed timeout on a testnet test that was failing.

* Adds new constructor to `PeerStatus`: `PeerCooling`. This is in order to
  fix a possible race where a node would be asynchronously demoted to cold
  state, put into the cold set and wrongly forgotten, while its connection
  was not yet cleaned up. This could lead to the peer being added to the known
  set and promoted leading to a `TrConnectionExists` trace in the logs, which
  is disallowed. With this being said, `PeerCold` status is now an
  intermediate status that means the peer is cold but its connection still
  lingers. This extra information warns the governor not to move the peer to
  the cold set just yet.

  Yes it can mean that the governor could promote the peer to hot, however it
  will promptly fail since no mux will be running anyway.

## 0.9.1.0 -- 2023-08-22

### Breaking changes

* Removes `updatePeerSharing` from `KnownPeers` module API

### Non-breaking changes

* Disable light peer sharing if peer sharing is not enabled.
* Do not use light peer sharing in `node-to-client` case (which leads to a memory leak).
* Split `test` component into `io-tests` and `sim-tests`.


## 0.9.0.0 -- 2023-08-09

### Breaking changes

* The counters logged by `PeerSelectionCounters` for local root peers are now
  the number of warm and hot peers per group (before the first one was the
  target of the given group).

* Introduced big ledger peers to the outbound governor.  The breaking changes include:
  - Added new targets to `PeerSelectionTargets` data type

  - Added `requestBigLedgerPeers` to `PeerSelectionActions`.

  - `establishPeerConnection` and `ativatePeerConnection` receive an
    `IsLedgerPeer` argument (which is then passed to mini-protocols via
    `ExtendedInitiatorContext`.

  - The `PeerSelectionState` contains new fields to support big ledger peers.

  - Modified `PickPolicy` type, it is now parametrised by monad `m` rather than
    by an `stm` monad.  With this change the type alias can be used in
   `pickPeers` type signature.

  - `TraceLedgerPeers` renamed some constructors:
      - `PickedPeer  -> PickedLedgerPeer`
      - `PickedPeers -> PickedLedgerPeers`;
    added new ones:
      - `PickedBigLedgerPeer`
      - `PickedBigLedgerPeers`;
    and `FetchingNewLedgerState` constructor has a new field: number of big
    ledger peers.

* Propagated changes from `ouroboros-network-framework` related to the
  introduction of initiator and responder contexts to `RunMiniProtocol` data
  type. These changes include breaking changes to the following APIs:

  - `Ouroboros.Network.Diffusion` is using: `OuroborosBundleWithExpandedCtx`
    for node-to-node applications, `OuroborosApplicationWithMinimalCtx` for
    node-to-client responders.
  - `Ouroboros.Network.NodeToNode` exports `MinimalInitiatorContext` and
    `ExpandedInitiatorContext` data types.
  - `Ouroboros.Network.NodeToClient` exports `MinimalInitiatorContext` and
    `ResponderContext` data types.
  - `Ouroboros.Network.NodeToNode.NodeToNodeProtocols`,
    `Ouroboros.Network.NodeToNode.nodeToNodeProtocols`,
    `Ouroboros.Network.NodeToNode.versionedNodeToClientProtocols`,
    `Ouroboros.Network.NodeToNode.withServer`  were modified.
  - `Ouroboros.Network.NodeToClient.NodeToClientProtocols`,
    `Ouroboros.Network.NodeToClient.nodeToClientProtocols`,
    `Ouroboros.Network.NodeToClient.versionedNodeToClientProtocols`,
    `Ouroboros.Network.NodeToClient.withServer`,
    `Ouroboros.Network.NodeToClient.ipSubscriptionWorker`,
    `Ouroboros.Network.NodeToClient.dnsSubscriptionWorker` were modified.

### Non-breaking changes

* Fixed a small memory leak in `PeerMetrics` (#4633).

* The counters logged by `PeerSelectionCounters` for local root peers are now
  the number of warm and hot peers per group (before the first one was the
  target of the given group).
* Added `getNumberOfPeers` destructor of `NumberOfPeers`.
* Added `NotEnoughBigLedgerPeers` and `NotEnoughLedgerPeers` constructors of
  `TraceLedgerPeers`; Renamed `FallingBackToBootstrapPeers` as
  `FallingBackToPublicRootPeers`.

## 0.8.2.0

### Breaking changes

* light peer sharing
  * Added `TraceKnownInboundConnection` constructor to `TracePeerSelection`
  * Added `readNewInboundConnection` field to `PeerSelectionActions` record.
* The constructor `FetchDeclineChainNoIntersection` was renamed to
  `FetchDeclineChainIntersectionTooDeep` (#4541)
* Include Warm Valency for Local Root Peers
* `TraceLedgerPeers` renamed some constructors:
    - `PickedPeer  -> PickedLedgerPeer`
    - `PickedPeers -> PickedLedgerPeers`;
  added new ones:
    - `PickedBigLedgerPeer`
    - `PickedBigLedgerPeers`;
  and `FetchingNewLedgerState` constructor has a new field: number of big
  ledger peers.

### Non-breaking changes

* Support for decoding Handshake Query Reply in wireshark dissector.
* Support for decoding CBOR payload in wireshark dissector.
* Limit concurrency used by dns resolution.  We only resolve up to 8 dns names
  concurrently for public / ledger peers and up to 2 for local root peers.
  This will affect how quickly node connects to ledger peers when it starts.
* Improved memory footprint of peer metrics (#4620)

## 0.8.1.1

### Non-breaking changes

* Initialise local root peers results `TVar` (#4584).

## 0.8.1.0

### Non-breaking changes

* Do not wrap `ExitCode` in `DiffusionError` wrapper.

## 0.8.0.1

### Non-breaking changes

* Export `Ouroboros.Network.Diffusion.Failiure` constructors.

## 0.8.0.0

### Breaking changes

* Changed how DNS for local root peers works
  - Change TraceLocalRootPeersTrace to include TraceLocalRootDNSMap constructor;
  - Change TraceLocalRootGroups constructor type;
  - Change localRootPeersProvider type signature;
  - Updated tests to reflect the above changes.

## 0.7.0.1

### Non-breaking changes

* Updated to use `ouroboros-network-api-0.5.0.0`.

## 0.7.0.0

### Breaking changes

* Added `DiffusionError` constructor of `Ouroboros.Network.Diffusion.Failure` which kind is now `Type`.

### Non-breaking changes

* Compatible with `ouroboros-network-framework-0.6.0.0` and
  `ouroboros-network-api-0.4.0.0`

## 0.6.0.0

### Breaking changes

* Use `io-classes-1.1`.

### Non-breaking changes

* `ghc-9.4` and `ghc-9.6` compatiblity.

## 0.5.0.0 -- 2023-04-19

### Breaking

- Integration of latest `cardano-ledger` and `cardano-base`.

- Add Peer Sharing feature:

  Peer Sharing is a new miniprotocol where nodes can share peer addresses, in order to
  discover new peers. It is only used if enabled. It should be disabled by default. Please
  read this design plan to understand the rationale and how Peer Sharing works with more
  detail:
  https://github.com/input-output-hk/ouroboros-network/wiki/Peer-Sharing-Implementation-Plan

  This new feature includes the following changes:

  - Peer Sharing - MiniProtocol
  - Refactor KnownPeers and EstablishedPeers
  - Refactor old "gossip" Peer Selection mechanism.
  - Changes to Handshake
    - Add new NodeToNode version
    - Add PeerSharing to RunNodeArgs and NodeToNodeVersionData
    - Add versionData (agreedOptions) to ConnectionHandler
    - Add versionData to PeerConnectionHandle
    - New CDDL tests

### Non breaking

- Fix interop problems between NonP2P and P2P nodes (PR #4465)
- Refactors requestPublicRootPeers to include PeerAdvertise
  - Public Roots Peer Advertise value is now used
- Implement ChainDB to fix header-body split in Diffusion Tests
- Fix DNS issue #4491

## 0.4.0.1 -- 2023-02-24

### Non-Breaking

* Fixed bugs in peer state actions & testing framework (PR #4385)

## 0.4.0.0 -- 2023-01-25

* Packages where reorganised:
   * `ouroboros-network-api`: a common api for `network` & `consensus` packages
   * `ouroboros-network-mock`: a mock chain which is used for testing purposes
   * `ouroboros-network-protocols`: implementation & tests of all mini-protocols.
      It contains two public libraries:
         * `ouroboros-network-protocols`
         * `ouroboros-network-protocols:testlib`
* Moved the `ipv6` cabal flag to `ouroboros-network-framework` package.
* Build with `ghc-9.2`.

## 0.3.0.2

### Non-breaking changes

- Fix interop problems between NonP2P and P2P nodes (PR #4465)

## 0.3.0.0 -- YYYY-MM-DD

*

## 0.2.0.0 -- YYYY-MM-DD

*

## 0.1.0.0 -- 2018-09-20

* Initial experiments and prototyping
