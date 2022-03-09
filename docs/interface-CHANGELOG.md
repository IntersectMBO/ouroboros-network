# Interface Change Log

This change log file is written for the benefit of Cardano Node development
team.  See [consensus
CHANGELOG](../ouroboros-consensus/docs/interface-CHANGELOG.md) file for how
this changelog is supposed to be used.


## Circa 2022-03-08


### Added

- on Linux when `network-mux` is compiled with `tracetcpinfo` cabal flag, mux
  will log `MuxTraceTCPInfo`.  For performance reasons this is disabled by
  default.
