# Revision history for ouroboros-network-testing

## next version

### Breaking changes

### Non breaking changes

* type signature of `prop_shrink_valid` is more admissible
* added `singletonTimedScript`
* added `Ouroboros.Network.Testing.Data.Script.shrinkScriptWith`

## 0.3.0.0

### Breaking changes

* `io-classes-1.1` support.

### Non-breaking changes

* `ghc-9.4` and `ghc-9.6` compatibility.

## 0.2.0.1

* Release a version compatible with `ghc-9.2`

## 0.2.0.0

/only the most recent changes/

### Breaking changes

* `keydTimeout` does not ignore tail (PR #4086)
* Added `Delay` constructor to `ScriptDelay`

### Non-breaking changes

* Added `Ouroboros.Network.Testing.Data.Signal.fromEventsWith` (PR #4086)
* Added `NonFailingAbsBearerInfo` with its arbitrary instances and
  `toNonfailingAbsBearerInfo` (PR #3862).
* Added `stepScriptOrFinish` and `stepScriptOrFinishSTM`

## 0.1.0.0 -- 2019-03-07

* Initial version
