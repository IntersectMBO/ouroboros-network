# Revision history for ouroboros-network

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

## 0.3.0.0 -- YYYY-MM-DD

*

## 0.2.0.0 -- YYYY-MM-DD

*

## 0.1.0.0 -- 2018-09-20

* Initial experiments and prototyping
