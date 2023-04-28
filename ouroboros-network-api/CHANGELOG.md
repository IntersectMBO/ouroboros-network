# Revision history for ouroboros-network-api

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

### Non breaking

## 0.1.0.0 -- 2022-11-17

* Initial release
