# cardano-client changelog

<!-- scriv-insert-here -->
<!-- scriv-end-here -->

## 0.6.1.0 -- 2025-09-10

* Support `ouroboros-network-0.22`

## 0.6.0.0 -- 2025-06-28

### Breaking changes

* SubscriptionTracers: mux tracer was split into two parts.

### Non-breaking changes

* Updated to `io-classes-1.8`

## 0.5.2.0 -- 2025-05-23

### Non-breaking changes

* Fixed how asynchronous exceptions are handled.

## 0.5.1.1 -- 2025-05-13

### Breaking changes

### Non-breaking changes

* bump dependencies

## 0.5.1.0 -- 2025-03-25

### Non-breaking changes

* Export `Decision` type.

## 0.5.0.0 -- 2025-01-02

### Breaking changes

* Addapted to `network-mux` changes in https://github.com/IntersectMBO/ouroboros-network/pull/4997

## 0.4.0.0 -- 2024-10-17

### Breaking changes

* Reimplementation of `subscribe` without relaying on non-p2p stack.  Its
  arguments have changed.  Note that the `NodeToClientProtocols` and
  `OuroborosApplicationWithMinimalCtx` specify `Void` as return type of the
  responder side.
* The default reconnect delay was increased from `0.025s` to `5s`.

## 0.3.1.6 -- 2024-10-11

### Breaking changes

### Non-breaking changes

* bump for version bounds

## 0.3.1.5 -- 2024-08-27

### Breaking changes

### Non-breaking changes

* bump for bad ref in chap for 0.3.1.4

## 0.3.1.4 -- 2024-08-22

### Breaking changes

### Non-breaking changes

* version bump for build depends

## 0.3.1.3 -- 2024-08-07

### Breaking changes

### Non-breaking changes

* Make it build with ghc-9.10

## 0.3.1.2

### Non-breaking changes

* Updated bounds

## 0.3.1.1

### Non-breaking changes

* Updated bounds

## 0.3.1.0

### Non-breaking changes

* ghc-9.8 support.

## 0.3.0.2

### Non-breaking changes

* Updated package bounds.

## 0.3.0.1 -- 2023-10-26

### Non-breaking changes

* Updated bounds.

## 0.3.0.0 -- 2023-08-09

### Breaking changes

* Due to changes in `RunMiniProtocol`, `OuroborosApplication` and
  `NodeToClientProtocols` data types the API provided in this package has
  breaking changes.  For more details see recent changes in
  [`ouroboros-network-framework`][onf-changelog].

## 0.2.0.0 -- 2023-04-25

### Breaking changes

* Make `cardano-client` independent of `ouroboros-consensus-diffusion`.  Look
  at haddocks how to upgrade.

## 0.1.0.2 -- 2023-01-25

* Update dependencies after repository restructure

## 0.1.0.0 -- 2020-05-18

* Initial release

[onf-changelog]: https://github.com/intersectmbo/ouroboros-network/blob/master/ouroboros-network-framework/CHANGELOG.md
