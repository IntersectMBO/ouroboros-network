# Simulator Monad

A pure simulator monad built on top of the `ST` monad which supports:

  * synchronous and asynchronous exceptions; including: throwing, catching and
    masking synchronous and asynchronous exceptions;
  * concurrency (using simulated threads), with interfaces shaped by the
    `base` and `async` libraries;
  * software transactional memory (`STM`);
  * simulated time;
  * timeouts;
  * dynamically typed traces and event log tracing;
  * lifting any `ST` computations;
  * deadlock detection.

`io-sim` is a drop-in replacement for the `IO` monad.  It was designed to write easily
testable Haskell networking code.  Using
[io-classes](https://hackage.haskell.org/package/io-classes) library
one can write code that can run in both: real `IO` and the `SimM` monad.  One
of the design goals was to keep the api as close as possible to `base`,
`exceptions`, `async` and `stm` packages.

As a design choice `IOSim` does not support `MVar`s by default, but they can be
simulated using `stm` interface.

`io-sim` supports both `io-classes` class hierarchy and `base`
/ `exceptions` class hierarchies (they diverge in some detail).


The package contains thorough tests, including tests of `STM` against the original
specification (as described in [Composable Memory
Transactions](https://research.microsoft.com/en-us/um/people/simonpj/papers/stm/stm.pdf)
and its `GHC` implementation.  This can be seen in both ways: as a check that
our implementation matches the specification and the `GHC` implementation, but also
the other way around: that `GHC`s `STM` implementation meets the specification.
