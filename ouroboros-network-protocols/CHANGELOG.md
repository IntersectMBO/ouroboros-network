# Revision history for ouroboros-network-protocols

## next release

### Breaking changes

* Add side-effect argument to `SendMsgRequestNextPipelined` to be invoked
  promptly upon `MsgAwaitReply`.

* Change the arguments of both `SendMsgRequestNext` constructors to have the
  same shape (and limited expressivity) as does `SendMsgRequestNextPipelined`
  (since we never use the extra expressivity).

### Non-breaking changes

## 0.7.0.0 -- 2024-01-22

### Breaking changes

* Pipeline depth type changed from `Word32` to `Word16`.

* In LocalStateQuery, changed the argument of `MsgAcquire` and `MsgReAcquire`
  from `Maybe point` to a new ADT `Target point`. It still allows the client to
  acquire either the volatile tip or a specific point, but now also allows them
  to instead acquire the immutable tip.

### Non-breaking changes

* ghc-9.8 support.

## 0.6.1.0 -- 2023-12-14

### Non-breaking changes

* Testlib depends on `cardano-slotting`'s `testlib` at version
  `0.1.2.0` and uses its instances.

* Use `io-sim-1.3.1.0`.

## 0.6.0.1 -- 2023-11-16

### Non-breaking changes

* Use `io-sim-1.3.0.0`.

## 0.6.0.0 -- 2023-11-02

### Breaking changes

* Make chainsync idle timeout configurable.

### Non-breaking changes

* Make it possible for KeepAlive client to collect a rtt sample for the first packet.

## 0.5.3.0 -- 2023-10-26

### Non-breaking changes

* Improved cdd specs by using `any` (PR #4638)
* Added a 3673s timeout to chainsync's StIdle state.
* Added a 97s timeout to keepalive's StClient state.

* Added a test to check that Peer Sharing values after handshake are symmetric
  relative to the initiator and responder side.
* Added cddl specs and tests for `NodeToNodeV_13` and handshake

* Refactored cddl tests for `PeerSharing` to take into account versioning.

## 0.5.2.0 -- 2023-09-08

### Non-breaking changes

* Use `io-classes-1.2`.
* Fixed a momory leak in `GHC-9.2`.

## 0.5.1.0 -- 2023-07-17

### Breaking changes

* Provide `Any` type in `Test.Data.CDDL` module.
* Definition of `TxId` has changed, it's now a newtype wrapper for
  `Any` type, which indicates that `ouroboros-network` does not specify what
  `TxId` or `Tx` types are.

## 0.5.0.3 -- 2023-05-26

* `ghc-9.6` compatibility

## 0.5.0.2 -- 2023-05-15

## Non-breaking changes

* Updated to use `ouroboros-network-api-0.5.0.0`.

## 0.5.0.1 -- 2023-05-08

## Non-breaking changes

* Updated to use `ouroboros-network-api-0.4.0.0`.

## 0.5.0.0 -- 2023-04-28

### Breaking changes

* `io-classes-1.1` support.

### Non-breaking changes

* `ghc-9.4` and `ghc-9.6` compatibility.

## 0.4.0.0 -- 2023-04-19

- Release

## 0.3.0.0 -- 2023-02-24

### Breaking

* Modified type `chain-sync` `Client`: `points` method now returns `Either` (PR #4385)

### Non-Breaking

* Expanded documentation about CDDL (PR #4351)

## 0.1.0.0 -- 2022-11-17

* Initial release
