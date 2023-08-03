# Revision history for mux

## next

### Breaking

### Non-breaking

* `asserts` cabal flag was removed, one can use `ghc-options` directly

## 0.4.0.0 -- 2023-04-28

### Breaking

* Use `io-classes-1.1`.
* Renamed `MuxTraceShutdown` as `MuxTraceStopping`.
* Fixed a typo now the mux stopping exception carries message: `Mux stopping`.


## 0.3.0.0 -- 2023-01-25

* Provide a `MakeBearer` newtype: a function to constructs a `MuxBearer`.
* Fix NodeToNodeV10 support
* Fix invalid Haddock markup
* Support `ghc-9.2`

## 0.2.0.0 -- 2022-11-11

* Bump versions of packages
* Platform independent TCP info trace
