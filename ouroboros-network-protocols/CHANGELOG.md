# Revision history for ouroboros-network-protocols

## next version

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
