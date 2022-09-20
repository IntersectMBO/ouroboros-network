# Interface Change Log

This change log file is written for the benefit of Cardano Node development
team.  See [consensus
CHANGELOG](../ouroboros-consensus/docs/interface-CHANGELOG.md) file for how
this changelog is supposed to be used.

## Circa 2022-09-20

- 'InitializationTracer' type renamed as 'DiffusionTracer'.
- The 'dtDiffusionInitializationTracer' record field of
  'Ouoroboros.Network.Diffusion.Tracers' record renamed as 'dtDiffusionTracer'.

## Circa 2022-05-19

- removed `node-to-client` versions `1` to `8`.  The lowest supported version is
  `NodeToClientV_9` introduced before the Alonzo hard-fork.

## Circa 2022-05-16

- `io-classes`, `io-sim` and `strict-stm` were moved to
  https://github.com/input-output-hk/io-sim

- `typed-protocols`, `typed-protocols-cborg` and `typed-protocols` were moved
  to https://github.com/input-output-hk/typed-protocols


## Circa 2022-04-06

- removed `node-to-node` versions `1` to `6`.  The lowest supported version is
  `NodeToNodeV_7` introduced before the Alonzo hard fark.

## Circa 2022-03-08


### Added

- on Linux when `network-mux` is compiled with `tracetcpinfo` cabal flag, mux
  will log `MuxTraceTCPInfo`.  For performance reasons this is disabled by
  default.
