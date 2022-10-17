{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}

-- | Minimal contravariant logging, as per contravariant logging, as per
-- https://www.youtube.com/watch?v=qzOQOmmkKEM
module Cardano.KESAgent.Logging
( Tracer (..)
, Contravariant (..)
, (>$<), (>$$<), ($<)
, nullTracer
, LogFormattable (..)
)
where

import Data.Functor.Contravariant

-- | A \"logger\", a.k.a. 'Tracer', is just a newtype over a generalized
-- monadic logging function @a -> m ()@, where @a@ is the log message to be
-- traced, and @m@ is the monad in which the tracing happens. (Note that @m@
-- does not technically have to be a 'Monad', though in practical use cases, it
-- usually will be).
newtype Tracer m a = Tracer { traceWith :: a -> m () }

-- | This instance is useful, because it allows us to manipulate the /input/ to
-- the tracer in much the same way as a regular 'Functor' allows us to
-- manipulate /outputs/.
instance Contravariant (Tracer m) where
  contramap f (Tracer t) = Tracer (t . f)

-- | The do-nothing tracer; this just throws away all traces.
nullTracer :: Applicative m => Tracer m a
nullTracer = Tracer (const $ pure ())

class LogFormattable a where
  logFormat :: a -> String

instance LogFormattable String where
  logFormat = id
