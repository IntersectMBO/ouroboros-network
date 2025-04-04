{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.KESAgent.Processes.Agent.Context
where

import Cardano.Crypto.KES.Class (KESAlgorithm (..))
import Cardano.KESAgent.Serialization.DirectCodec
import Data.SerDoc.Class (HasInfo (..))

import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.Processes.Agent.Monad

type AgentContext m c =
  ( MonadAgent m
  , AgentCrypto c
  , HasInfo (DirectCodec m) (VerKeyKES (KES c))
  , HasInfo (DirectCodec m) (SignKeyKES (KES c))
  )
