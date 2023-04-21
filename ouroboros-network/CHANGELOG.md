# Revision history for ouroboros-network

## next version

### Non breaking

- Fix interop problems between NonP2P and P2P nodes (PR #4465)

### Breaking

- Add CBORCodec Tuple and refactor code to use it

## 0.4.0.1 -- 2023-02-24

### Non-Breaking

* Fixed bugs in peer state actions & testing framework (PR #4385)

## 0.4.0.0 -- 2023-01-25

* Packages where reorganised:
   * `ouroboros-network-api`: a common api for `network` & `consensus` packages
   * `ouroboros-network-mock`: a mock chain which is used for testing purposes
   * `ouroboros-network-protocols`: implementation & tests of all mini-protocols.
      It contains two public libraries:
         * `ouroboros-network-protocols`
         * `ouroboros-network-protocols:testlib`
* Moved the `ipv6` cabal flag to `ouroboros-network-framework` package.
* Build with `ghc-9.2`.

## 0.3.0.0 -- YYYY-MM-DD

*

## 0.2.0.0 -- YYYY-MM-DD

*

## 0.1.0.0 -- 2018-09-20

* Initial experiments and prototyping
