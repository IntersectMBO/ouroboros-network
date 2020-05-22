{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.Block (
    CardanoBlock
  ) where

import qualified Cardano.Crypto.Hash.Class as CardanoCrypto
import qualified Shelley.Spec.Ledger.BlockChain as Shelley

import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
import           Ouroboros.Consensus.HardFork.Combinator.Unary
                     (FromRawHash (..), projBlock)

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyHash (..), ShelleyLedgerConfig (..))
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

instance TPraosCrypto c => SingleEraBlock (ShelleyBlock c) where
  singleEraParams _ = shelleyLedgerEraParams

  -- Don't transition
  singleEraTransition _cfg _st = Nothing

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Shelley"
    }

  getRawHash _ = CardanoCrypto.getHash . Shelley.unHashHeader . unShelleyHash

instance TPraosCrypto c => FromRawHash (ShelleyBlock c) where
  fromRawHash _ = ShelleyHash . Shelley.HashHeader . CardanoCrypto.UnsafeHash

instance TPraosCrypto c => Condense (CardanoBlock c) where
  condense = condense . projBlock . unDBlk
