# Revision history for ntp-client

## next release

## 0.0.1.6 -- 2024-08-07

### Breaking changes

### Non-breaking changes

* Make it build with ghc-9.10

## 0.0.1.5 -- 2024-06-07

### Breaking changes

### Non-breaking changes

- Bump `Win32-network` package version

## 0.0.1.4 -- 2024-01-22

### Non-breaking changes

* ghc-9.8 support.

## 0.0.1.3 -- 2023-11-02

### Non-breaking changes

* Created a new `CompletedNtpStatus` to represent the subset of `NtpStatus`
  produced by `ntpQuery` and used it to eliminate an impossible case in
  `ntpClientThread` and its associated error.

## 0.0.1.2 -- 2023-10-26

* fixed cabal warnings

## 0.0.1.1 -- 2023-04-28

### Non-breaking changes

* `ghc-9.4` and `ghc-9.6` compatiblity.

## 0.0.1.0 -- 2022-12-13

* Initial release
