# Interface Change Log

This change log file is written for the benefit of Cardano Node development
team.  See [consensus
CHANGELOG](../ouroboros-consensus/docs/interface-CHANGELOG.md) file for how
this changelog is supposed to be used.

## Circa 2022-05-05

- redesign of `typed-protocols`.  Each pipelining client now tracks unsatisfied
  transitions using a type level fifo. For example in the context of a simple
  [`ping-poing` mini-protocol](https://input-output-hk.github.io/ouroboros-network/typed-protocols-examples/Network-TypedProtocol-PingPong-Type.html#t:PingPong)
  if a pipelining clients sends two `MsgPing`, the queue will contain two yet
  unsatisfied transitions `StBusy -> StIdle`.  They can be taken out of the
  queue in a fifo order using one of the `Collect`, `CollectSTM`, `CollectDone`
  primitives which usually are packaged.  In particular all pipelining clients
  will need to be updated.

- `Data.Type.Nat` was moved from `typed-protocols` to `ouroboros-network` package.

- Pipeliend decisions are still using `Nat` singletons; one needs
  `Data.Type.Nat.queueDepthNat` or `Data.Type.Nat.queueFDepthNat` to transform
  one of the `Queue` singletons to a `Nat`.

## Circa 2022-04-06

- removed `node-to-node` versions `1` to `6`.  The lowest supported version is
  `NodeToNodeV_7` introduced before the Alonzo hard fark.

## Circa 2022-03-08


### Added

- on Linux when `network-mux` is compiled with `tracetcpinfo` cabal flag, mux
  will log `MuxTraceTCPInfo`.  For performance reasons this is disabled by
  default.
