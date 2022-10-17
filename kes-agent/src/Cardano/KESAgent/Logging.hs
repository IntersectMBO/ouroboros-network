{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
module Cardano.KESAgent.Logging
( Tracer (..)
, Contravariant (..)
, (>$<), (>$$<), ($<)
, nullTracer
, LogFormattable (..)
)
where

import Data.Functor.Contravariant

newtype Tracer m a = Tracer { traceWith :: a -> m () }

instance Contravariant (Tracer m) where
  contramap f (Tracer t) = Tracer (t . f)

nullTracer :: Applicative m => Tracer m a
nullTracer = Tracer (const $ pure ())

class LogFormattable a where
  logFormat :: a -> String

instance LogFormattable String where
  logFormat = id
