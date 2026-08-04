# Revision history for cardano-diffusion

<!-- scriv-insert-here -->

<a id='changelog-1.1.1.0'></a>
## 1.1.1.0 -- 2026-08-05

### Non-Breaking

- `cardano-diffusion:ping` - export `HashType` from `Cardano.Network.Ping`.

<a id='changelog-1.1.0.0'></a>
## 1.1.0.0 -- 2026-07-28

### Breaking

- Added `NodeToNodeV_16` and updated `NodeToNodeVersionData` with a new `PerasSupport` field that is used only for `NodeToNodeV_16` version and later. This field indicates whether the node supports Peras protocols, and is negotiated during the version handshake.
  - CDDL specification for `NodeToNodeVersionData` has been updated in a backward-compatible way.

- `Cardano.Network.NodeToNode`: Added support for certificate and vote diffusion mini-protocols for Ouroboros Peras.
  - New fields in `NodeToNodeProtocols`.
  - Protocol limits `perasCertDiffusionProtocolLimits`/`perasCertVoteProtocolLimits`.
  - Mini-protocol numbers `perasCertDiffusionMiniProtocolNum`/`perasVoteDiffusionMiniProtocolNum`.

- `cardano-ping` has been deprecated and it is recommended to switch to
  `cardano-diffusion:ping` library.  The API has changed.  The logger is
  instantiated by the top level `pingClients` function.  It supports
  connecting to multiple nodes at once and supports SRV records. Note that
  `pingClient` is now an internal, not-exported function. The new API suports
  querying tip over node-to-node and node-to-client protocols.  An
  optparse-applicative parser for `PingOpts` is provided.
  - minimal node-to-node version support is raised from `NodeToNodeV_7` to `NodeToNodeV_14`.
  - minimal node-to-client version support is raised from `NodeToClientV_9` to `NodeToClientV_16`.
  - `pingOptsJson` is now a `LogFormat` rather than a `Bool`.
  - logger is now an internal funcionality, no need to initialise it on the user side.

- Added `nodeTo{Client,Node}VersionDataCodec` to
  `Cardano.Network.NodeTo{Client,Node}` modules.
- Removed `nodeTo{Client,Node}CodecCBORTerm`, use the above binding instead.

- `ChurnMode` is now a newtype wrapper around `FetchMode`.

- Rename `getBlockHash` to `getImmutableBlockPoint` to better reflect that it queries the immutable DB and returns a `Point`.
- Change the return type of `getBlockHash` (renamed to `getImmutableBlockPoint`) from `Maybe (Point RawBlockHash)` to `Either GetImmutableBlockPointError (Point RawBlockHash)`, replacing an opaque `Nothing` with structured error information.

- Upgraded to `contra-tracer ^>=0.2.1`. The `Tracer` data constructor is no
  longer exported; use `mkTracer` instead.
- Capped `QuickCheck < 2.18`.

- Bumped `trace-dispatcher` to `^>=2.13`.

### Non-Breaking

- `Cardano.Network.OrphanInstances`: Added support for `NodeToNodeV_16`.

- Using tracers defined in `Test.Ouroboros.Network.Utils` rather than providing ones own.
- Added number of transitions in the tx-submission sim-net test.
- Replaced `classify` with `label` in `sim-net` tests which provides nicer output.
- Provide `DiffSimResult` type alias for diffusion simulation result type.
- Using `PrettyShow` in tests.

- Re-exported `Ouroboros.Network.Diffusion.Topology` from `Cardano.Network.Diffusion.Topology`.

- `ToJSON` instance of `CardanoLocalRootConfig` was changed, instead of
  a generic `extraFlags` field we use `peerTrustable` field name for the
 `PeerTrustable` flag.

- Improved haddocks of `NodeToNodeVersion` and `NodeToClientVersion`.

- Add test for testing V2 tx-submission when faced with option to download TXs in different
  orders from different peers.

- When a node is started and syncing begins using bootstrap peers, only two outbound active connections
  should be established to reduce the load on bootstray relays. Prior to this change, until the first
  churn cycle 15 minutes into operation, a full set of active peers would be connected to.

- Minor tweaks to tracing irregularities

- Rename the peer-selection target signal helper from `selectEnvTargets` to `selectGovTargets`.

- Update test suite for tx-submission v2 without a central decision thread.

- Exported `PraosFetchMode` from `Cardano.Network.LedgerPeerConsensusInterface`
- Added `PrettyShow` instances for
    - `NodeToNodeVersion`
    - `NodeToNodeVersionData`
    - `NodeToClientVersion`
    - `NodeToClientVersionData`

- Added `pingOptsParser` to `cardano-diffusion:ping`.
- Added `cardano-diffusion:cardano-ping` command.

- Removed the `QuickCheck < 2.18` upper bound, allowing QuickCheck 2.18+.
- Added `cardano-base:testlib >=0.1.5.0` dependency.

- Added `pingClient` to `cardano-diffusion:ping`
- `cardano-diffusion:ping` now exports also
  - `Stage`
  - `ResolvedSRVOrFilePath`
  - `PingClientException`

- `cardano-diffusion:ping` changes
  - added `pingClients'` to `cardano-diffusion:ping`,
  - exported `IP` and `mkAddress` constructors of the `Address` type.

- Revert aeson lower bound

- Internal adaptations to `ouroboros-network` changes regarding `TTL` newtype wrapper.

- cardano-diffusion:ping - more concise formatting of `NodeToNodeVersionData`
- cardano-diffusion:ping - quiet mode: don't show rtt information
- cardano-diffusion:ping - `--short-hash` option added

<a id='changelog-1.0.0.0'></a>
## 1.0.0.0 -- 2026-03-06

### Breaking

- Adapted to removal of `ChurnCounters` and `dtTraceChurnCounters` in
  `ouroboros-network`.

- `Ouroboros.Network.Diffusion.Tracers` changed in `ouroboros-network` package,
  see it's changelog for details.

- Change the type of `LedgerPeersConsensusInterface.getBlockHash` to accept a `Point` instead of only a `SlotNo`.

- Integration with `typed-protocols-1.2.0.0`, `NFData` constraints are required in public API (e.g. `NodeTo{Node,Client}.connectTo`, etc.).

- Replace `SomeHashableBlock` with the `RawBlockHash` type in `LedgerPeerConsensusInterface`

- Added trace-dispatcher LogFormatting and MetaTrace instances
  for Churn's TraceChurnMode tracer

- Removed ToJSON UseBootstrapPeers oprhan instance from OrphanInstances module

- Removed ExtraTrace from Monitor module, and moved its data constructors
  to ToExtraTrace data instance of SupportsPeerSelectionState instance for
  ExtraPeers type. Similarly, ExtraPeerSelectionSetsWithSizes was moved to
  the instance as well.
- removed cardanoPeerSelectionStateToCounters
- Adjusted various types to integrate the changes related to the introduction
  of SupportsPeerSelectionState class
  - CardanoTracers, CardanoTraceLocalRootPeers, CardanoPeerSelectionCounters,
  - peerChurnGovernor, cardanoPeerSelectionGovernorArgs

- Removed orphan ToJSON ExtraTrace instance

### Non-Breaking

- Update dependencies.

- Exported `PublicRootPeers (..)` from `Cardano.Network.PeerSelection` module.

- Added property that verifies target changes stay within acceptable bounds.
- Updated `diffusionSimulation` to run with either the Cardano churn and Ouroboros churn.

- Fix an incomplete case match warning.

- Integrate TVar to collect duplicate tx's in the mempool writer

- Added SupportsPeerSelectionState instance for ExtraPeers

### Non-breaking

- Added ToJSON UseBootstraPeers instance to Bootstrap module

<a id='changelog-0.1.0.0'></a>
## 0.1.0.0 -- 2026-01-20

### Breaking

- Initial release of the `cardano-diffusion` package, which is based on
  `ouroboros-network:cardano-diffusion` with the following modifications:
  - Removed `Cardano.Network.Types` module. `LedgerStateJudgement` is available
    from the `cardano-diffusion:api` package in
    `Cardano.Network.LedgerStateJudgement` module. `NumberOfBigLedgerPeers` is
    available from `cardano-diffusion` in `Cardano.Network.PeerSelection` module.
  - Added `Cardano.Network.PeerSelection` module, which exports most of the
    Cardano-related `PeerSelection` APIs - you can simplify your imports with it.
    It might be a good idea to import this module qualified.

- `Cardano.Network.FetchMode` exports `ConsensusMode` and
  `LedgerStateJudgement` as these are arguments of `mkReadFetchMode`.

- `Cardano.Network.Types` module was removed.  `LedgerStateJudgement` is
   available from the `cardano-diffusion:api` package in
   `Cardano.Network.LedgerStateJudgement` module. `NumberOfBigLedgerPeers` is
   available from `cardano-diffusion` in `Cardano.Network.PeerSelection` module.

- `Cardano.Network.PeerSelection.PeerSelectionActions.requestPublicRootPeers`
   was renamed as `requestPublicRootPeersImpl` to avoid a name clash with
   `PeerSelectionActions{requestPublicRootPeers}`.

- `Cardano.Network.LedgerPeerConsensusInterface` re-exports `FetchMode`,
  `LedgerStateJudgement` and `OutboundConnectionsState` since these types are
  appear in `LedgerPeerConsensusInterface` record.

- `Cardano.Network.PeerSelection.Churn` exports `PeerChurnArgs` for the
   completeness sake.

- `diffusionSimulation`: removed tracer argument, no longer polymorphic in
  monad - using `IOSim` only.  `diffusionSimulationM` is available but not
  exported.

cardano-diffusion:
- added `getBlockHash` to `LedgerPeersConsensusInterface`

- Moved `timeLimitsChainSync` from `Ouroboros.Network.Protocol.ChainSync.Codec` to `Cardano.Network.Protocol.ChainSync.Codec.TimeLimits`.
- Added parameter `PeerTrustable` to `timeLimitsChainSync`.
- Changed timeout behavior: No timeout when peer is trusted and ChainSync state is `StNext StMustReply`.

### Non-Breaking

- Testing improvements in net-sim.

- fixed false positive in `prop_diffusion_target_active_below` testnet test
- improved approach in general to target-chasing tests in diffusion testnet
  and PeerSelection mock environment tests.

- Adapted tests to changes in the `Ouroboros.Network.TxSubmission.Mempool.Simple` API

- Patched so that it compiles to wasm

- Added export list to `Cardano.Network.PeerSelection.PublicRootPeers`, also
  re-exported `Ouroboros.Network.PeerSelection.PublicRootPeers` module.
- Fixed a false positive test failure in peer selection test: `prop_governor_target_established_above`

cardano-diffusion:
- moved `jobVerifyPeerSnapshot` from ouroboros-network

- Enforce a minimum churn of established peers based on churned active peers.
- Enforce a minimum churn of known peers based on churned established peers.

- Added a property test to verify that the node never connects to peers behind a firewall.
- Added a round-trip property test to verify that topology file decoding and encoding are correct.

- Update dependencies.

- Fixed `prop_governor_target_established_local` tests by accounting for ongoing promotions.

- Ensure timeout to enter sensitive state when bootstrap peers
  are enabled is always respected.
- Added test for timeout

- Added property tests to validate the timeout behavior of the `timeLimitsChainSync` function.

* Update test-lib for new args for TracePromoteColdFailed and TracePromoteColdBigLedgerPeerFailed.

- Compatibility with both `QuickCheck` < 2.15 and >= 2.16
<!-- scriv-end-here -->
