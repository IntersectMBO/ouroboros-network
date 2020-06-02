{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Cardano.Block (
    CardanoBlock
  ) where

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
import           Ouroboros.Consensus.HardFork.Combinator.Unary

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyLedgerConfig (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraos, TPraosCrypto)

{-------------------------------------------------------------------------------
  The block type of the Cardano block chain, including hard-forks
-------------------------------------------------------------------------------}

-- | Temporary: use the 'HardFork' combinator with a single block
-- ('DegenFork'), 'ShelleyBlock', as it is easier to generate transactions for
-- Shelley than for Byron. This makes sure we exercise all parts of the
-- 'HardFork' combinator in the ThreadNet tests.
--
-- We parameterise over crypto ('TPraosStandardCrypto' for the real block,
-- 'TPraosMockCrypto' for the tests), because we can only generate
-- transactions in the tests when using mock crypto.
type CardanoBlock c = DegenFork (ShelleyBlock c)

instance TPraosCrypto c => HasPartialConsensusConfig (TPraos c)
  -- Use defaults

instance TPraosCrypto c => HasPartialLedgerConfig (ShelleyBlock c)
  -- Use defaults

instance TPraosCrypto c => NoHardForks (ShelleyBlock c) where
  getEraParams               = shelleyLedgerEraParams . configLedger
  toPartialLedgerConfig    _ = id
  toPartialConsensusConfig _ = id

instance TPraosCrypto c => SingleEraBlock (ShelleyBlock c) where
  -- Don't transition
  singleEraTransition _cfg _st = Nothing

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Shelley"
    }

instance TPraosCrypto c => Condense (CardanoBlock c) where
  condense = condense . aux . unDBlk
    where
      aux :: HardForkBlock '[ShelleyBlock c] -> ShelleyBlock c
      aux = project' (Proxy @(I (ShelleyBlock c)))
