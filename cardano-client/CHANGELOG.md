# Revision history for cardano-client

## next version

## 0.3.1.4 -- 2024-08-22

### Breaking changes

### Non-breaking changes

* version bump for build depends

## 0.3.1.3 -- 2024-08

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
