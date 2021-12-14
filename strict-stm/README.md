# Strict Software Transaction Memory

The `strict-stm` package provides a strict interface to software transaction
memory.  It builds on top of `io-classes` and thus it provides the interface
for both [STM](https://hackage.haskell.org/package/stm) as well as
[io-sim](https://github.com/input-output-hk/ouroboros-network/tree/master/io-classes).

# Novel testing / space-leak elimination approach

The strict interface provides a novel way of testing / eliminating space-leaks
which might lurk in `stm` shared mutable variables.  This together with an
interface build on top of
[ghc-heap](https://gitlab.haskell.org/ghc/ghc/-/tree/master/libraries/ghc-heap)
was successfully used to eliminate such bugs in a large system.  We strongly
recommend to use `Control.Monad.Class.MonadSTM.Strict`.  It exposes the
[MonadSTM](https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadSTM.html#t:MonadSTM)
interface and gives access to
[StrictTVar](https://hackage.haskell.org/package/io-sim-classes/docs/Control-Monad-Class-MonadSTM-Strict.html#t:StrictTVar)'s
in place of non-strict
[TVar](https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadSTM.html#t:TVar)'s.
