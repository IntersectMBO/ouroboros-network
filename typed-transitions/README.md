# Chain exchange protocol via type transitions

## Overview

The unidirectional chain producer / consumer protocol is expressed at the type
level, and is used to eliminate possible miscommunication between either side.
A chain producer and chain consumer pair of the appropriate types will never
deadlock (both await each-other), nor interject (both yield to one-another
concurrently), and (assuming a well-behaved encoder/decoder) will never send an
unexpected message when they are run in separate threads or processes.

- `Protocol/Core.hs` defines the infrastructure for expressing a protocol in
  types, and for using it in a model where every state belongs to one of two
  agents (such as client/server).
- `Protocol/Chain/Type.hs` defines the blockchain exchange protocol using the
  definitions from `Protocol/Core.hs`.
- `Protocol/Chain/StreamProducer.hs` defines the *producer* side of the protocol
  from `Protocol/Chain/Type.hs`, given a definition of a source of blocks.
- `Protocol/Chain/StreamConsumer.hs` defines the *consumer* side of the protocol
  from `Protocol/Chain/Type/hs`, given a definition of a sink of blocks.

## Why use it

Better factoring / modularity, and higher assurance / type safety.

- The protocol is known to GHC.
  Some properties which we would need to quickcheck can instead be proved at
  compile time. Absence of deadlock and interjection is assured.
- Effects and simulation infrastructure come in only at the periphery.
  The core producer and consumer definitions are pure, rather than
  *potentially* pure depending on the choice of a monad and its instances of
  various classes which presuppose effects.
  Any well-typed application of the protocol will work correctly unless there
  is a problem with the effects which back it, and there is a clear demarcation
  between those effects and the application itself.
- It is amenable to, but not dependent upon, existing work on simulation.
  The protocol applications themselves (stream-based producer and consumer)
  make no mention of simulation, nor of IO or any effects, but the streaming
  datatypes which back them can be chosen at simulation or production
  implementations.

# Where to go from here / how to use it

All that's missing are `Sim`/`MonadClass` implementations of `BlockStream` and
`ConsumerStream`. With these, we can run the `streamProducer` and
`streamConsumer` across a `SimSTM` `TVar` channel
(`Protocol/Chain/Channel.Sim.hs`), using `MonadConc` for concurrency.
