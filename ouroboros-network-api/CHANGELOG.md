# Revision history for ouroboros-network-api

## next version

### Breaking changes

### Non-breaking changes

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
