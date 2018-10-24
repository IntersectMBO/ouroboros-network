# Chain exchange protocol via type transitions

## Overview

The unidirectional chain producer / consumer protocol is expressed at the type
level, and is used to eliminate possible miscommunication between either side.
A chain producer and chain consumer pair of the appropriate types will never
deadlock (both await each-other), nor interject (both yield to one-another
concurrently), and (assuming a well-behaved encoder/decoder) will never send an
unexpected message when they are run in separate threads or processes.

- `Protocol.Core` defines the infrastructure for expressing a protocol in
  types, and for using it in a model where every state belongs to one of two
  agents (such as client/server) or is a terminal state (end of protocol).
- `Protocol.Channel` defines the infrastructure for running two sides of
  a protocol in separate threads or processes, communicating by way of
  side-effects. `STM`, pipes, sockets; any reliable and ordered mechanism gives
  rise to a `Channel`, which can be wired to one side of the protocol by
  `useChannel`.

Modules specific to chain exchange:

- `Protocol.Chain.Type` defines the blockchain exchange protocol using the
  definitions from `Protocol.Core`.
- `Protocol.Chain.ProducerStream` defines a type for a producer, and shows how
  a value of this type defines a producer side of the protocol from
  `Protocol.Chain.Type`.
- `Protocol.Chain.ConsumerStream` defines a type for a consumer, and shows how
  a value of this type defines a consumer side of the protocol from
  `Protocol.Chain.Type`.
- `Protocol.Chain.Direct` shows that the producer and consumer streams are
  complementary. They can be directly linked, without factoring through the
  protocol from `Protocol.Chain.Type`. Linking a producer and consumer by way of
  `direct` should be the same as factoring them through the protocol by way of
  `streamProducer`, `streamConsumer`, and `connect`. This can be verified by
  random testing.

Applications (these modules are in `ouroboros-network`):

- `Protocol.Chain.Sim.Consumer` defines a `ConsumerStream` which attemps to
  download every chain, never attempting to improve the read pointer, and
  updates a `TVar` from `MonadSTM` with its chain every time a new header is
  received.
- `Protocol.Chain.Sim.Producer` defines a `ProducerStream` which serves headers
  according to a `TVar` from `MonadSTM`.
- `Protocol.Chain.Sim.Node` shows how chain selection, and the producer and
  consumer streams defines in the previous two modules, can be integrated to
  make a node and a network of nodes. This is used in
  `Test.Node.prop_consensus` to check that such a network always reaches
  consensus in a connected component.

## Why use it

Better factoring / modularity, and higher assurance / type safety.

The protocol is a part of the statics of the program. In theory this makes it
easier to communicate. Anyone who wishes to implement the protocol, whether in
Haskell or otherwise, has the spec written explicitly.
Some properties which we would need to quickcheck can instead be proved at
compile time. Absence of deadlock and interjection is assured.

Effects and simulation infrastructure come in only at the periphery.
The producer and consumer sides of the protocol are pure, rather than
*potentially* pure depending on the choice of a monad and its instances of
various classes which presuppose effects.
Any well-typed application of the protocol will work correctly unless there
is a problem with the effects which back it, and there is a clear demarcation
between those effects and the application itself.

It is amenable to, but not dependent upon, existing work on simulation.
The protocol applications themselves (stream-based producer and consumer)
make no mention of simulation, nor of IO or any effects, but the streaming
datatypes which back them can be chosen at simulation or production
implementations.

## Factoring through a protocol

Perhaps the greatest appeal of the `typed-transitions` approach is the
separation of

  1. The definition of a producer (`StreamProducer`)
  2. The definition of a consumer (`StreamConsumer`)
  3. The definition of the protocol (`ChainExchange`, `TrChainExchange`)

For each of these things, there is a type (given in parens), and none of these
make any mention of side-effects nor concurrency. This allows us to speak of
an *arbitrary* producer or consumer, which will be useful in testing and
simulation: given a *particular* producer or consumer, which will run against
*any* consumer or producer, we can choose that consumer or producer such that
when we run it directly (without concurrency or the protocol in-between) we
should expect a certain result. In this way we can verify properties of a
particular producer or consumer with fewer confounding variables.

It's also possible to experimentally verify the soundness of the protocol: if
this producer and this consumer, when directly linked, gives this result, then
that same producer and consumer, when linked *through the protocol*, must give
the same result. If we can generate random producers and consumers, then we
can quickcheck this. We can do similar automated testing for the introduction
of an effectful channel to carry the protocol transitions.

Compare to the `consumerSideProtocol1` and `producerSideProtocol1`, which are
defined in such a way that they *must* be linked using side-effects and
channels carrying messages in the protocol. These are inherently effectful
things (they give `m ()`), whereas the `StreamConsumer` and `StreamProducer`
can compute and return values, which in some cases obviates the need to use
probes: instead of running each node in a simulation network forever, and
checking some mutable probe, we can run each node *until it finishes*, and
check the results of every node. See `prop_consensus` for an example of this.

### Comparison of `Streams` and `Handlers`

In `ProtocolInterfaces` from `ouroboros-network` the `ConsumerHandlers` and
`ProducerHandlers` callback records are defined. Implementations of either side
of the protocol will use these to do effects, but these handlers are less
capable of expressing *decision making* in the protocol.
`consumerSideProtocol`, for instance, never uses its `ConsumerHandlers` to
affect control flow; it requests next, deals with the result, and repeats.

`consumerStream` is far more general. It uses a `ConsumerStream` value to
decide whether to improve the read pointer, download headers, or wait for the
next update. That same `ConsumerStream` also acts as `ConsumerHandlers` by
dealing with the data received. With this approach, implementing some kind of
policy for controlling a download is natural: simply put this logic into the
`ConsumerStream`. It's capable of blocking, deciding what to ask for, and
even terminating the protocol.
