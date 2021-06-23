# Simulator Monad Class Hierarchy

This package provides a monad class hierarchy which is an interface for both the
[io-sim](https://hackage.haskell.org/package/io-sim) and
[IO](https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-IO.html#t:IO)
monads.  It was developed with the following constraints in mind:

* be a drop in replacement for `IO` monad;
* `IO` instances does not alter its original semantics, providing a shallow
  bindings to `async`, `base`, `stm` and `exception` packages;
* provide zero cost abstractions.

There are a few departures from this principles, usually visible in type
signature, which we discuss in this document.  When using `IO`, for most of the
interfaces, `GHC` can optimise away the provided abstractions with `-o1`
optimisation level.

## Exception Class Hierarchy

This package provides an alternative class hierarchy giving access to
exceptions api.  The `exception` package class hierarchy is also supported by
`io-sim`, so you can also use either one.

 The `MonadThrow` defined in this package allows to work with exceptions
without having explicit access to `catch` or `mask`.  It only provides access
to `throwIO`, `bracket`, `bracket_` and `finally`.  `MonadCatch` class provides
api which allows to work with exceptions, e.g. `catch` or `bracketOnError`, and
`MonadMask` gives access to low level `mask` and friends.   This division makes
code review process somewhat easier.  Using only `MonadThrow` constraint the
reviewer can be sure that no low level exception api is used, which usually
requires more care, and still allows to do resource handling right.

## Time and Timer APIs

We follow the tradition of splitting time into two units of measures: as unit
of time differences, which has monoidal nature and as a unit of time which is
a G-set for the former.  We use
[DiffTime](https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Clock.html#t:DiffTime)
for the former and a newtype wrapper `Time` for the later (provided for this
package).  `DiffTime` is used consistently across all the type classes which is
one of the few departures from the `base` interface.  One example is
[threadDelay](https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadTimer.html#v:threadDela)
(provided by `MonadDelay`) which is using `DiffTime` (being in seconds) rather
than passing microseconds as an `Int` - as it is done by `base` package.
Provided `threadDelay` function is safely against overflows, this is especially
important on `32`-bit architectures (with the original `base`
approach on 32-architectures, the maximal delay is slightly more than `30`
minutes).

`MonadTimer` class provides a unified interface to `GHC` event manager api as
defined in
[GHC.Event](https://hackage.haskell.org/package/base/docs/GHC-Event.html).  We
expose instances also for architectures which do not provide this `GHC`
interface, like `Windows` or `GHCJS`.

A good example of usage of this interface is an implementation of platform
independent (Windows!) and reliable implementation of
[timeout](https://github.com/input-output-hk/ouroboros-network/blob/master/network-mux/src/Network/Mux/Timeout.hs#L225)
function (which lives outside of this package).  Note that since it is using
only type classes constraints from this package it also works in
[IOSim](https://hackage.haskell.org/package/io-sim/docs/Control-Monad-IOSim.html#t:IOSim)
monad.

## Software Transactional Memory API

We provide two interfaces to `stm` api: lazy and strict one.  The strict
interface provides a novel way of testing / eliminating space-leaks which might
lurk in `stm` shared mutable variables.  This together with an interface build
on top of
[ghc-heap](https://gitlab.haskell.org/ghc/ghc/-/tree/master/libraries/ghc-heap)
was successfully used to eliminate such bugs in a large system.  We strongly
recommend to use `Control.Monad.Class.MonadSTM.Strict`.  It exposes the
[MonadSTM](https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadSTM.html#t:MonadSTM)
interface and gives access to
[StrictTVar](https://hackage.haskell.org/package/io-sim-classes/docs/Control-Monad-Class-MonadSTM-Strict.html#t:StrictTVar)'s
in place of non-strict
[TVar](https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadSTM.html#t:TVar)'s.

## Threads API

We draw a line between `base` api and `async` api.  The former one is provided
by
[MonadFork](https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadFork.html#t:MonadFork)
the latter by
[MonadAsync](https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadFork.html#t:MonadAsync).
Both are shallow abstractions around APIs exposed by the `base` and `async`
packages.

## Some other APIs

* [MonadEventlog](https://hackage.haskell.org/package/io-sim-classes/docs/Control-Monad-Class-MonadEventlog.html#t:MonadEventlog):
  provides an API to the
  [Debug.Trace](https://hackage.haskell.org/package/base/docs/Debug-Trace.html)
  eventlog interface.
* [MonadST](https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadST.html#t:MonadST): provides a way to lift `ST`-computations.
* [MonadSay](https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadSay.html#t:MonadSay): dummy debugging interface
