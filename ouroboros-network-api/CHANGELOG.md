# Revision history for ouroboros-network-api

## next release

### Breaking changes

* Simplify type of `headerForgeUTCTime` in `BlockFetchConsensusInterface`, and
  remove the supporting type `FromConsensus`.

### Non-breaking changes

## 0.16.0.0 -- 2025-07-21

### Breaking changes

* Added `encodeLedgerPeerSnapshot` and `decodeLedgerPeerSnapshot`; removed
  `{To,From}CBOR` instances for `LedgerPeerSnapshot`.

## 0.15.0.0 -- 2025-06-28

### Breaking changes

* Added `NodeToClientV_21`.

### Non-breaking changes

* `IsLedgerPeer` added to `Ouroboros.Network.LedgerPeers.Types` module.
* Added `ProtocolTimeLimitsWithRnd` to `Ouroboros.Network.Protocol.Limits`

## 0.14.1.0 -- 2025-07-17

### Breaking changes

### Non-breaking changes

* Allow/restore capability for peer sharing in `InitiatorOnly` mode

## 0.14.0.0 - 2025-05-13

### Breaking changes

* Removed `NodeToNodeV_13`

### Non-breaking changes

## 0.13.0.0 -- 2025-02-25

### Breaking changes

* Added `NodeToClientV_20`.
- Moved `Ouroboros.Network.ConsensusMode` to `Cardano.Network.ConsensusMode`
- Moved `Ouroboros.Network.PeerSelection.Bootstrap` to `Cardano.Network.PeerSelection.Bootstrap`
- Moved `Ouroboros.Network.PeerSelection.LocalRootPeers` to `Cardano.Network.PeerSelection.LocalRootPeers`
- Moved `Ouroboros.Network.PeerSelection.PeerTrustable` to `Cardano.Network.PeerSelection.PeerTrustable`
- Created `Cardano.Network.Types` and moved Cardano specific types such as
  `LedgerStateJudgement` and `NumberOfBigLedgerPeers` from
  `Ouroboros.Network.PeerSelection.LedgerPeers/Type` to there.
- Removed `lpGetLedgerStateJudgment` from `LedgerConsensusInterface` and added
  `extraAPI` type parameter to it.
* Removed `DomainAccessPoint` type
* Added `RelayAccessSRVDomain` tag to `RelayAccessPoint`
* Removed `RelayAccessPointCoded` type
* Bumped LedgerPeerSnapshot version to 2 to directly
  leverage JSON and CBOR instances for `RelayAccessPoint`

## 0.12.0.0 -- 2025-01-02

### Breaking changes

* Removed deprecated APIs
  * `getLegacyTipBlockNo`
  * `legacyTip`
  * `toLegacyTip`
* Dropped all node-to-client versions < `NodeToClientV_16`.

### Non-breaking changes

## 0.11.0.0 -- 2024-10-17

### Breaking changes

* Renamed:
  * `accBigPoolStake` -> `accumulateBigLedgerStake`
     and `reRelativeStake` -> `recomputeRelativeStake`
* Using `typed-protocols-0.3.0.0`.
* Added `NodeToClientV_19`.
* Removed `blockForgeUTCTime` from `BlockFetchConsensusInterface`.

### Non-breaking changes

* Added `ConsensusMode` which must be passed to start diffusion in the
  appropriate mode
* added `compareLedgerPeerSnapshotApproximate` function which compares
  two snapshots for approximate equality wrt stake distribution and
  fully qualified domain names.
* Added `MinBigLedgerPeersForTrustedState` type of values indicating
  the minimum number of active big ledger peers needed to signal
  trusted state when finishing syncing in Genesis mode.

## 0.10.0.0 -- 2024-10-11

### Breaking changes

* Removed `NodeToNodeV_12` and older as these are unable
  to cross the hard fork boundary.
* Removed `WhetherReceivingTentativeBlocks` used to
  distinguish whether a node version is pipelining-enabled,
  used in older `NodeToNodeVersion`
* Added `NodeToNodeV_14` to identify nodes supporting Chang+1 HF
* Added `NodeToClientV_18`

### Non-breaking changes

## 0.9.0.1 -- 2024-08-27

### Breaking changes

### Non-breaking changes

* bump for bad ref in chap for 0.9.0.0

## 0.9.0.0 -- 2024-08-22

### Breaking changes

* Added `NodeToClientV_17` to support new local state queries

### Non-Breaking changes

## 0.8.0.0 -- 2024-08-07

### Breaking changes

* `LedgerPeersKind` was transplanted here from o-network because this
  functionality needs to be exposed in support of Genesis work and
  generation of a big ledger peer snapshot.

### Non-Breaking changes

* Transplanted `accBigPoolStake` and `reRelativeStake` from ouroboros-network
  `LedgerPeers` module to expose functionality that facilitates serializing
  of big ledger peers via LocalStateQuery miniprotocol.
* Introduced `LedgerPeerSnapshot` type for values of big ledger peers obtained
  from the ledger at a particular volatile tip.
  * New type supports CBOR & JSON for serialisation purposes.
  * Ledger peer snapshot is versioned in case changes need to be made to the
    encoding format in the future.

* Added `Measure` and `BoundedMeasure` instances for `SizeInBytes`.

* Make it build with ghc-9.10

* Added Monoid and Semigroup instances for `SizeInBytes`

## 0.7.3.0 -- 2024-06-07

### Breaking changes

### Non-Breaking changes

- Bump io-sim and io-classes
* Added `ShowProxy SlotNo` instance
* Added `AnchoredSeq.splitAtMeasure`
* Added `AnchoredFragment.splitAtSlot`

## 0.7.2.0 -- 2024-05-07

### Breaking changes

### Non-Breaking changes

* Added `OutboundConnectionsState` data type

## 0.7.1.0 -- 2024-03-14

### Breaking changes

### Non-Breaking changes

* Added `Generic` and `NFData` instance derivations for `NodeToClientVersion`
  data type

## 0.7.0.0 -- 2024-02-21

### Breaking changes

* Changed `LedgerConsensusInterface` type:
    `LedgerConsensusInterface` now has to fill 3 STM actions:
        * `lpGetLatestSlot :: STM m (WithOrigin SlotNo)`
        * `lpGetLedgerStateJudgment :: STM m LedgerStateJudgement`
        * `lpGetLedgerPeers :: STM m [(PoolStake, NonEmpty RelayAccessPoint)]`

* Added `PeerTrustable` flag
* Added `UseBootstrapPeers`, this data type is now a flag
  to enable/disable bootstrap peers usage.
* Added `UseLedgerPeers` flag with an indirection layer to `UseLedgerAfter`

### Non-breaking changes

## 0.6.3.0 -- 2024-01-22

### Non-breaking changes

- CI requires me to add an entry here even though I merely updated a comment on
  `NodeToClientV_16` mention new `ImmutableTip` argument in LocalStateQuery
  mini protocol

## 0.6.2.0 -- 2023-12-14

### Non-breaking changes

* Refactored `NodeToNodeVersionData` decoder.

## 0.6.1.0 -- 2023-11-29

### Non-breaking changes

* Fixed handshake codec: disabled `PeerSharing` for `node-to-node` versions 11 and 12.
* Disable `PeerSharing` with `InitiatorOnly` nodes, since they do not run
  peer sharing server side and can not reply to requests.
* Fixed `Acceptable` instance of `NodeToNodeVersionData` to only negotiate
  `PeerSharing` if diffusion mode is `InitiatorAndResponder`
* ghc-9.8 support.

## 0.6.0.1 -- 2023-11-16

### Non-breaking changes

* Use `io-sim-1.3.0.0`.

## 0.6.0.0 -- 2023-11-02

### Breaking changes

* Remove `PeerSharingPrivate` option from the `PeerSharing` data type.
* Rename `NoPeerSharing` and `PeerSharingPublic` to `PeerSharingDisabled` and
  `PeerSharingEnabled`, respectively.
* Add new `NodeToNodeV_13` that encodes and decodes the updated `PeerSharing` flag data
  type.
* Move remote address codec to 'src/Ouroboros/Network/NodeToNode/Version.hs'.
* Make remote address codec receive 'NodeToNodeVersion'.

### Non-breaking changes

* Restructured `decodeTerm` to prevent an impossible case and eliminate the
  associated `error`.

## 0.5.1.1 -- 2023-10-26

### Non-breaking changes

* Depend on `type-protocols-0.1.1.0`.

## 0.5.1.0 -- 2023-08-09

### Breaking changes

### Non-breaking changes

* Added `IsBigLedgerPeer` type to
  `Ouroboros.Network.PeerSelection.LedgerPeers.Type`.

## 0.5.0.0 -- 2023-05-15

* Swapped `NodeToClientV_15` with `NodeToClientV_16`, e.g. handshake query
  comes with `V_15` and Conway with `V_16`.
* Swapped `NodeToNodeV_11` with `NodeToNodeV_12`, e.g. handshake query moved to
  `V_11` and Conway moved to `V_12`.  `V_11` also comes with handshake support
  for peer sharing.

## 0.4.0.0 -- 2023-05-08

Depracated release, use `0.5` instead.

### Breakin changes

* Added `NodeToNodeV_12` and `NodeToClientV_16` which support handshake query.
* Added `query` flag to `NodeToClientVersionData` and `NodeToNodeVersionData`.
* Introduced `HandshakeCallbacks` record.

### Non-breaking changes

* Added `Querable` type class.

## 0.3.0.0 -- 2023-04-28

* Removed `encoddedTipSize` and `encodedPointSize`.
* `HeaderHash` is kind polymorphic.

## 0.2.0.0 -- 2023-04-19

### Breaking

- Integration of latest `cardano-ledger` and `cardano-base`.
- Peer Sharing integration
  - New PeerAdvertise data type file
  - New PeerSharing data type file
- Remove foldr in favour of toOldestFirst

### Non-breaking

## 0.1.0.0 -- 2022-11-17

* Initial release
