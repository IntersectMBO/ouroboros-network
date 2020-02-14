# ntp-client

A library for approximating the time-offset between the local clock
of the client and a number of NTP servers.
This library is not a replacement for a full-feature ntp-client.
The purpose of this library is to detect when the local clock is off by more than several seconds.
In this case an application, like for example a wallet, may report the problem
and ask the user to take proper actions to sync the local clock.

## tests

To run the test suite:
```
cabal new-test ntp-client-test
```
or
```
nix-build -A haskellPackages.ntp-client.checks
```

## demo app
```
nix-build -A haskellPackages.ntp-client.components.exes.demo-ntp-client
```
