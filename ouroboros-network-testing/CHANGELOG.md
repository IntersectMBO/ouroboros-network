# ouroboros-network-testing changelog

<!-- scriv-end-here -->

## 0.8.3.0 -- 10.09.2025

### Breaking changes

### Non-breaking changes
* `renderRanges`: print a range using math notation for open/closed intervals.
* Pretty print `WithName` using `Show` instance.
* Pretty print `WithTime` using `Show` instance.
* Added `DistinctList` and `DistinctNEList` quickcheck modifiers.

## 0.8.2.0 -- 28.06.2025

### Non-breaking changes

* Support `io-classes-1.8`.
* Improved `Arbitrary` instances for `Delay` and `SmallDelay`.
* Improved `Show` instances for `WithName` and `WithTime`.

## 0.8.1.0 -- 2025-02-25

### Non-breaking changes

* Added `Test.Ouroboros.Network.Data.Signal.keydLinger'`.

## 0.8.0.0 -- 2025-01-02

### Breaking changes

* Renamed modules from `Ouroboros.Network.Testing.*` to `Test.Ouroboros.Network.*`
* Added `IOError` field to `ErrorInterval :: AbsAttenuation` constructor.

### Non-Breaking changes

* Added `AbsIOError` quickcheck generator for `IOError`s.

## 0.7.0.0 -- 2024-08-07

### Breaking changes

* Improvements and bug fixes to Signal API

### Non-Breaking changes

* Make it build with ghc-9.10

## 0.6.2.0 -- 2024-06-07

### Breaking changes

### Non-Breaking changes

- Bump io-sim and io-classes
* Added `isSubsetProperty` and `disjoinSetsProperty` to `Ouroboros.Network.Testing.Utils`.

## 0.6.1.0 -- 2024-05-07

### Breaking changes

### Non-Breaking changes

## 0.6.0.0 -- 2024-02-21

### Breaking changes

* Fixed Script strict API functions. Now functions with an apostrophe `'` are
  properly strict variants of the non-apostrophe functions.

### Non-breaking changes

## 0.5.0.0 -- 2024-01-22

### Breaking changes

* Adds `eventually` and `eventsToListWithId` functions to Signal API

### Non-breaking changes

* ghc-9.8 support

## 0.4.1.0 -- 2023-12-14

### Non-breaking changes

* Use `io-sim-1.3.1.0`

## 0.4.0.1 -- 2023-11-16

### Non-breaking changes

* Use `io-sim-1.3.0.0`.

## 0.4.0.0 -- 2023-10-26

### Breaking changes

- Changed `prop_shrink_valid` to use `ShrinkCarefully`
- Changed `prop_shrink_nonempty` to use `ShrinkCarefully` (formerlly
  `prop_shrinkCarefully` was defined in `ouroboros-network:sim-test`)

### Non-breaking changes

- Added 'keyedTimeoutTruncated' to Signal API

## 0.3.1.0 -- 2023-08-09

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
