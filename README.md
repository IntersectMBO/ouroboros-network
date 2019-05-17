[![Build Status](https://badge.buildkite.com/3c5e581fd69202ceddd64e91351846c41baa285aaca835cdd9.svg?style=flat-square)](https://buildkite.com/input-output-hk/ouroboros-network)

# Ouroboros-Network

* io-sim - `IOSim` simulator monad which supports asynchrounous exceptions,
  `STM` transactions and `async` interface, timers. 
* io-sim-classes - type classes, all of them have instance for both `IOSim` and
  `IO`.
* `typed-protocols` - session type framework with support of [protocol pipelineing](https://en.wikipedia.org/wiki/Protocol_pipelining)
* `ouroboros-network`- ouroboros network package which implements protocols
  which to run ouroboros family of protocols, multiplexing layer.
* `byron-proxy` - proxy between Byron and Shelley nodes.

# Ouroboros-Consensus

Consensus layer ofr the family Ouroboros blockain protocols.
