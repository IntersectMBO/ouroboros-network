{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.KESAgent.Processes.Agent.Context
where

import Cardano.KESAgent.Processes.Agent.Monad

-- | For convenience: the typeclasses that are required for typical agent
-- actions.
type AgentContext m c =
  ( MonadAgent m
  , AgentCrypto c
  )
