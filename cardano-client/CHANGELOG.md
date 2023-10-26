# Revision history for cardano-client

## next version

### Breaking changes

### Non-breaking changes

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

[onf-changelog]: https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-network-framework/CHANGELOG.md
