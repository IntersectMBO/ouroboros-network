{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types    #-}

-- | See 'OnlyForTracer'
--
-- Intended for qualified import.
--
-- > import           Ouroboros.Network.Tracers.OnlyForTracer (OnlyForTracer)
-- > import qualified Ouroboros.Network.Tracers.OnlyForTracer as OnlyForTracer
module Ouroboros.Network.Tracers.OnlyForTracer
  ( OnlyForTracer (..)
  , PolyRun (..)
  , contramap
  , mkPolyRun
  , mkRun
    -- * Convenience combinators
  , contramapM
  , lift
  , mkTracerWith
  , mkTracerWithPolyRun
  , pure
  , traceWith
  ) where

import           Prelude hiding (Applicative (..))
import qualified Prelude

import           Control.Tracer (Tracer (..))
import qualified Control.Tracer as Tracer

-- | An opaque box holding data that can only be accessed by a 'Tracer'
--
-- Note: In sufficient monads, the intended use can be subverted in order to
-- leak the contained data outside of tracer invocations, but that should be
-- caught and avoided during code review.
newtype OnlyForTracer a = UnsafeOnlyForTracer {
    -- | This is only safe to use _inside_ of another 'OnlyForTracer'
    -- computation
    unsafeRunOnlyForTracer :: a
  }
  deriving (Functor)
  -- DO NOT DERIVE Foldable or Traversable -- those violate the intended use

instance Prelude.Applicative OnlyForTracer where
  pure = UnsafeOnlyForTracer
  UnsafeOnlyForTracer f <*> UnsafeOnlyForTracer a = UnsafeOnlyForTracer (f a)

-- | The only safe use of 'UnsafeOnlyForTracer'
pure :: a -> OnlyForTracer a
pure = Prelude.pure

-- | The only way to eliminate the 'OnlyForTracer' structure is to let it
-- influence a 'Tracer'
contramap :: OnlyForTracer (a -> b) -> Tracer m b -> Tracer m a
contramap = Tracer.contramap . unsafeRunOnlyForTracer

-- | A exported wrapper only used to avoid impredicative polymorphism
--
-- TODO Once we switch to ghc-9.2, check to see if we should eliminate this
-- wrapper, since ghc-9.2 will have improved impredicative polymorphism
-- support.
newtype PolyRun = PolyRun { run :: forall a. OnlyForTracer a -> a }

-- | A polymorphic version of 'mkRun'
mkPolyRun :: OnlyForTracer PolyRun
mkPolyRun = pure $ PolyRun unsafeRunOnlyForTracer

-- | Since 'OnlyForTracer' is inescapable (excepting the note on its Haddock),
-- it's sound for any 'OnlyForTracer' computation to trivially eliminate the
-- 'OnlyForTracer' layer
--
-- Note: compare this to a hypothetical constant of type @M (M a -> a)@ for
-- some specific monad @M@.
mkRun :: OnlyForTracer (OnlyForTracer a -> a)
mkRun = fmap run mkPolyRun

{-------------------------------------------------------------------------------
  Convenient aliases
-------------------------------------------------------------------------------}

-- We define these aliases without using any @unsafe*@ functions, so that we
-- can't accidentally break the established invariants.

lift :: Tracer m a -> Tracer m (OnlyForTracer a)
lift = contramap mkRun

contramapM :: Monad m
           => OnlyForTracer (a -> m b)
           -> Tracer m b
           -> Tracer m a
contramapM kOft bTr =
    contramap
      (fmap (\kRun a -> kRun kOft a) mkRun)
      (Tracer.contramapM id bTr)

traceWith :: Tracer m a -> OnlyForTracer a -> m ()
traceWith = Tracer.traceWith . lift

mkTracerWith :: OnlyForTracer a -> (a -> Tracer m b) -> Tracer m b
mkTracerWith aOft mk =
    contramap
      (fmap (\aRun b -> (aRun, b)) mkRun)
      (Tracer $ \(aRun, b) -> Tracer.traceWith (mk (aRun aOft)) b)

mkTracerWithPolyRun :: (PolyRun -> Tracer m a) -> Tracer m a
mkTracerWithPolyRun = mkTracerWith mkPolyRun
