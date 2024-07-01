# Revision history for ouroboros-network

## next version

### Breaking changes

* moved `accBigPoolStake` and `reRelativeStake` to ouroboros-networking-api
  in order to expose functionality of creating snapshots of big ledger peers,
  eg. for Genesis consensus mode.

### Non-Breaking changes

* Refactored signature of `LedgerPeers.ledgerPeersThread` for concision
  and use of previously created records for shunting related values around.

## 0.16.1.1 -- 2024-06-28

### Breaking changes

### Non-Breaking changes

- Increase the target number of active peers during bulk sync to account for hot
  trusted localroot peers.

## 0.16.1.0 -- 2024-06-07

### Breaking changes

### Non-Breaking changes

* Don't include peers that are failing in peershare responses.
- Bump io-sim and io-classes
- Refactor testnet

## 0.16.0.0 -- 2024-05-07

### Breaking changes

* Added `daUpdateOutboundConnectionsState :: OutboundConnectionsState -> STM m ()`
  to `Diffusion.Common.Applications`. This callback is to be provided by
  consensus and is propagated all the way to the peer selection governor.

## 0.15.0.0 -- 2024-05-07

### Breaking changes

* Added `dtTraceChurnCounters` to `Ouroboros.Network.Diffusion.P2P.TracersExtra`.
* Added `PeerSelectionView` and `PeerSelectionCounters` (now a pattern synonym)
  which provides sets or sizes of active / established / known sets, and added
  `PeerSelectionCountersHWC` which provides sizes of hot / warm / cold sets.
  The counters cover more groups including: all peers, big ledger peers,
  bootstrap peers, local roots and shared peers.
* `emptyPeerSelectionState` doesn't take targets of local roots.
* Added `AssociationMode` and `LedgerStateJudgement` to `DebugPeerSelectionState`.
  Both should be exposed through `EKG` counters by the node.
* Removed `TraceKnownInboundConnection` and replaced it with
  `TracePickInboundPeers` message in the peer selection tracer.
* Adapted to changes in `ouroboros-network-framework`, in particular the
  outbound governor is using `PublicInboundGovernorState` to implemented light
  peer sharing.

### Non-Breaking changes

* Improved Churn governor by synchronizing according to the counters instead
  of relying on `threadDelay`.
* Added `TraceChurnAction` and `TraceChurnTimeout` trace points of `TracePeerSelection`.
* Added `HasCallStack` to functions which call `pickPeers`.
* Update the bigledger retry state in case of an exception
* Reset public root retry state when transition between `LedgerStateJudgements`.
* Reduce public root retry timer.
* Don't classify a config file with publicRoot/bootstrapPeers IP addresss only
  as a DNS error.
* Renamed `fuzzRnd` to `stdGen` in `PeerSelectionState`
* split `stdGen` in `PeerSelection.Governor.wakeupAction`

## 0.14.0.0 -- 2024-04-04

### Breaking changes

* `newPeerSharingAPI` requires `PublicPeerSelectionState` variable to be passed to it.
* `Diffusion.Arguments` requires `PublicPeerSelectionState`; the integration
  code should make sure both `newPeerSharingAPI` and diffusion receives the
  same mutable variable.
* `TracePeerShareRequest` also includes the number of requests peers.

## 0.13.1.0 -- 2024-03-20

### Non-Breaking changes

* Honour policyPeerShareActivationDelay timeout when peersharing
* Increase timeout to 120s for 'any Cold async demotion' test

## 0.13.0.0 -- 2024-03-14

### Breaking changes

* Added `PeerSharingAPI` with all the things necessary to run peer sharing.

### Non-Breaking changes

* Fix `LedgerStateJudgement` redundant tracing
* Refactored `computePeerSharingPeers` and moved it to
  `Ouroboros.Network.Peersharing`
* Fix 'any Cold async demotion' test
* Let light peer sharing depend on the configured peer sharing flag
* Split churning of non-active peers into an established step and a known step.
* When peer sharing ask for more peers than needed, but only add as many unique
  peers as desired.

## 0.12.0.0 -- 2024-02-21

### Breaking changes

* Moved `LedgerConsensusInterface` type to `ouroboros-network-api`.
* Preserved `PeerAdvertise` information when connecting to peers.
* Added `daReadUseBootstrapPeers` to `ArgumentsExtra`.

* Added `PeerTrustable` to Local Root Peers

* Added new trace constructors for `TracePeerSelection`
* Updated type of constructor in `TraceLedgerPeers`
* Updated type of constructor in `TraceLocalRootPeers`
* Added `TraceDebugState` message to `TracePeerSelection` for tracing
  peer selection upon getting a USR1 sig.
* Changed withPeerSelectionActions and withLedgerPeers signatures

* Removed `computePeers` callback in `daApplicationInitiatorAndResponderMode`.
* Changed `peerSharingServer` to require `PeerSharingAPI`.

### Non-breaking changes

* Limit the rate at which one can discover peers through peersharing.
* Created `PublicRootPeers` and adds `BootstrapPeers` and big ledger peers to
  `PublicRootPeers` abstraction.

* Adjusted `PeerSelectionActions` `requestPublicRootPeers` function to
  provide either only ledger peers or bootstrap peers according to the
  current ledger state. The same for `requestBigLedgerPeers`.

* Added `readLedgerStateJudgement` to `PeerSelectionActions`
* Added `ledgerStateJudgement` to `PeerSelectionState`
* Added `bootstrapPeersFlag` to `PeerSelectionState`
* Added `hasOnlyBootstrapPeers` to `PeerSelectionState`

* Simplified `KnownPeerInfo` by removing `IsLedgerPeer`, `PeerTrustable` and
  `IsBootstrapPeer`

* Preserved `PeerAdvertise` information when connecting to peers.

* Added new monitoring tasks to monitor the bootstrap peers flag, the ledger
  state judgement value and act accordingly (`monitorBootstrapPeersFlag`,
  `monitorLedgerStateJudgement` and `waitForOnlyBootstrapPeers`) .

* Updated other monitoring tasks to consider a possible sensitive state that
  involves the bootstrap peers flag and the ledger state judgement value.

* Improved tracing when peersharing
* set knownSuccessfulConnection for incomming peers
* Don't use minPeerShareTime with GuardedSkip

* `PeerSharingController` is now private and `requestPeers` is exported

* Fix hot demotion by having blockfetch give chainsync a chance to exit
  cleanly before killing it.

* Disable mean reward for new peers

* Fix `targetPeers` monitoring action to use the correct set of local peers
  when in sensitive mode.

* Forget non-established bootstrap peers when transitioning from
  `TooOld` state to `YoungEnough`

* Implemented Churn for bootstrap peers

* Coalesced various diffusion configuration parameters in a new Configuration module which were scattered around previously

## 0.11.0.0 -- 2024-01-22

### Breaking changes

* Renamed `ReconnectDelay` to `RepromoteDelay` - the dalay is used after
  demotion to `cold` as well as `warm` state.  A `ReconnectDelay` type alias is
  still provided but deprecated.

* Changed pipelining parameters in `MiniProtocolParameters` from `Word32` to
  `Word16` to match the types elsewhere.

### Non-breaking changes

* The internal `Guarded` type changed.  It is provided with pattern synonyms
  which hide both `Min` and `FirstToFinish`.
* Adds 'unit_reconnect' testnet test
* When churning split restoring known peers and established peers targets into
  two separate steps.
* Fix `KnownPeers.insert` function semantic bug where it could easily misused,
  overwriting values.
* Made (light) peer sharing results advertisable unless already known
* Peer sharing is now delayed for 5minutes for newly established peers.
* `policyPeerShareRetryTime` to 900s

## 0.10.2.2 -- 2023-12-15

### Non-breaking changes

* Use `checked-strict-vars-0.2.0.0`.
* ghc-9.8 support.


## 0.10.2.1 -- 2023-12-14

This release is based directly on top of `ouroboros-network-0.10.1.0`.

### Non-breaking changes

* Fixed a bug in `outbound-governor`: PR #4748.  In rare cases the Outbound
  Governor could lose track of a connection, and thus not being able to
  reconnect to a remote peer.

## 0.10.2.0 -- 2023-12-14

Deprecated release.

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
  fixing issue [#4642](https://github.com/intersectmbo/ouroboros-network/issues/4642)
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
  https://github.com/intersectmbo/ouroboros-network/wiki/Peer-Sharing-Implementation-Plan

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

### Non-breaking

- Fix interop problems between NonP2P and P2P nodes (PR #4465)
- Refactors requestPublicRootPeers to include PeerAdvertise
  - Public Roots Peer Advertise value is now used
- Implement ChainDB to fix header-body split in Diffusion Tests
- Fix DNS issue #4491

## 0.4.0.1 -- 2023-02-24

### Non-breaking

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
