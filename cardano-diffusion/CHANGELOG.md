# Revision history for cardano-diffusion

<!-- scriv-insert-here -->

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
