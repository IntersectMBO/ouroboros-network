# Revision history for ouroboros-network-mock

## next release

## 0.1.1.2 -- 2024-08-07

### Breaking changes

### Non-breaking changes

* Make it build with ghc-9.10

## 0.1.1.1 -- 2024-01-22

### Non-breaking changes

* ghc-9.8 support.

## 0.1.1.0 -- 2023-11-02

### Non-breaking changes

* Clarified `successorBlock`'s contract (specifying that behavior is undefined
  when the provided point isn't on the provided chain) and leveraged it to
  prevent a call to `error`.

## 0.1.0.2 -- 2023-10-26

### Non-breaking changes

* Fixed cabal warnings.

## 0.1.0.1 -- 2023-04-28

###  Non-breaking changes

* `ghc-9.4` and `ghc-9.6` compatibility.

## 0.1.0.0 -- 2022-11-17

* Initial release
