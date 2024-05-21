# Revision history for ouroboros-network-api

## next release

### Breaking changes

### Non-Breaking changes

- Bump io-sim and io-classes
* Added `ShowProxy SlotNo` instance

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
