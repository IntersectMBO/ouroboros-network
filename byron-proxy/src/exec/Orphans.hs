{-# LANGUAGE FlexibleInstances #-}

module Orphans where

import Control.Monad.Trans.Resource (ResourceT)

import qualified Control.Monad.Class.MonadThrow as NonStandard
import qualified Control.Monad.Catch as Standard

-- Orphan, forced upon me because of the IO sim stuff.
-- Required because we use ResourceT in the chain sync server, and `runPeer`
-- demands this non-standard `MonadThrow`. That could be fixed by returning
-- the failure reason rather than throwing it...

instance NonStandard.MonadThrow (ResourceT IO) where
  throwM = Standard.throwM
  -- There's a default definition fo this which requires
  -- NonStandard.MonadCatch (ResourceT IO). To avoid having to give those,
  -- we'll just use the standard definition.
  -- NB: it's weird huh? This implementation uses the _standard_ MonadMask
  -- constraint, but the non-standard one is not defined.
  bracket = Standard.bracket
