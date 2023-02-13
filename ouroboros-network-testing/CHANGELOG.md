# Revision history for ouroboros-network-testing

## 0.2.0.1

* Release a version compatible with `ghc-9.2`

## 0.2.0.0

/only the most recent changes/

### Breaking changes

* `keydTimeout` does not ignore tail (PR #4086)
* Added `Delay` constructor to `ScriptDelay`

### Non breaking changes

* Added `Ouroboros.Network.Testing.Data.Signal.fromEventsWith` (PR #4086)
* Added `NonFailingAbsBearerInfo` with its arbitrary instances and
  `toNonfailingAbsBearerInfo` (PR #3862).
* Added `stepScriptOrFinish` and `stepScriptOrFinishSTM`

## 0.1.0.0 -- 2019-03-07

* Initial version
