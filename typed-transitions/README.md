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

## Factoring through a protocol

The `BlockStream m t` and `ConsumerStream m t` types are complementary, as
witnessed by `Protocol.Chain.Direct.direct`, which feeds the consumer with
blocks from the producer.

```Haskell
direct
  :: ( Monad m )
  => BlockStream m t
  -> ConsumerStream m t
  -> m t
```

The `BlockStream m t` models the producer side of the protocol, and the
`ConsumerStream m t` models the consumer side. Linking them `direct`ly, we get
consumer and producer in the same thread, which isn't even a triviality: it may
be useful for a wallet application.

A typical use case, however, puts the producer (`BlockStream m t`) and consumer
(`ConsumerStream m t`) in separate processes, probably on different machines.
This typed-transitions approach allows us to factor the `direct` link through
a sequence of transitions. Complementary sides of the `TrChainExchange` protocol
can be derived:

```Haskell
streamProducer
  :: ( Monad m )
  => BlockStream m t
  -> Peer ChainExchange TrChainExchange ('Yielding 'StInit) ('Yielding ('StBusy 'Next)) m t

streamConsumer
  :: ( Monad m )
  => ConsumerStream m t
  -> Peer ChainExchange TrChainExchange ('Awaiting 'StInit) ('Yielding 'StInit) m t
```

As the type indicates, these add no side-effects; the derived `Peer`s will use
only the side-effects already defined in the `BlockStream m t` or
`ConsumerStream m t`.

## Integration with `Sim`/`MonadClass` from `ouroboros-network`

Using existing work in `ouroboros-network` is only a matter of creating
`BlockStream m t` and `ConsumerStream m t` where `m` is constrained by the
various `MonadClass`es. This has been done in the
`Protocol.Chain.Sim.*` modules in the `typed-transitions` package.
