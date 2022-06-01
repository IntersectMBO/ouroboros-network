{-# LANGUAGE PatternSynonyms #-}

-- | Utility functions for enclosing a code segment with tracing events.
module Ouroboros.Consensus.Util.Enclose (
    Enclosing
  , Enclosing' (..)
  , encloseWith
  , pattern FallingEdge
  ) where

import           Control.Tracer (Tracer, traceWith)

data Enclosing' a =
    -- | Preceding a specific code segment.
    RisingEdge
    -- | Succeeding a specific code segment, with extra information.
  | FallingEdgeWith !a
  deriving (Show, Eq, Ord)

type Enclosing = Enclosing' ()

pattern FallingEdge :: Enclosing' ()
pattern FallingEdge = FallingEdgeWith ()

{-# COMPLETE RisingEdge, FallingEdge #-}

-- | Enclose an action using the given 'Tracer'.
encloseWith ::
     Applicative m
  => Tracer m Enclosing
  -> m a
  -> m a
encloseWith tracer action =
    traceWith tracer RisingEdge *> action <* traceWith tracer FallingEdge
