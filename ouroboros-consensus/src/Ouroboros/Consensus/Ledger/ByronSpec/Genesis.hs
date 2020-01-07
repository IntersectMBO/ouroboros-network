{-# LANGUAGE RecordWildCards #-}

-- | Genesis config for the spec
--
-- Intended for qualified import
--
-- > import           Ouroboros.Consensus.Ledger.ByronSpec.Genesis (ByronSpecGenesis)
-- > import qualified Ouroboros.Consensus.Ledger.ByronSpec.Genesis as Genesis
module Ouroboros.Consensus.Ledger.ByronSpec.Genesis (
    ByronSpecGenesis(..)
  , setPBftThreshold
    -- * Conversions
  , toChainEnv
  , fromChainEnv
  ) where

import           Data.Set (Set)

import qualified Cardano.Spec.Chain.STS.Rule.Chain as Spec
import qualified Control.State.Transition as Spec
import qualified Ledger.Core as Spec
import qualified Ledger.Update as Spec
import qualified Ledger.UTxO as Spec

import           Ouroboros.Consensus.Ledger.ByronSpec.Orphans ()

{-------------------------------------------------------------------------------
  Genesis config
-------------------------------------------------------------------------------}

-- | The equivalent of the genesis config for the abstract ledger
data ByronSpecGenesis = ByronSpecGenesis {
      byronSpecGenesisDelegators    :: Set Spec.VKeyGenesis
    , byronSpecGenesisInitUtxo      :: Spec.UTxO
    , byronSpecGenesisInitPParams   :: Spec.PParams
    , byronSpecGenesisSecurityParam :: Spec.BlockCount
    }
  deriving (Show)

setPBftThreshold :: Double -> ByronSpecGenesis -> ByronSpecGenesis
setPBftThreshold threshold genesis = genesis {
      byronSpecGenesisInitPParams = (byronSpecGenesisInitPParams genesis) {
          Spec._bkSgnCntT = Spec.BkSgnCntT threshold
        }
    }

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | Derive CHAIN rule environment
toChainEnv :: ByronSpecGenesis -> Spec.Environment Spec.CHAIN
toChainEnv ByronSpecGenesis{..} = disableConsensusChecks (
      Spec.Slot 0 -- current slot
    , byronSpecGenesisInitUtxo
    , byronSpecGenesisDelegators
    , byronSpecGenesisInitPParams
    , byronSpecGenesisSecurityParam
    )
  where
    -- We are only interested in updating the /ledger state/, not the /consensus
    -- chain state/. Unfortunately, the Byron spec does not make that
    -- distinction, and so when we call the CHAIN rule, we might get some errors
    -- here that the implementation does not report (because it would only find
    -- them when we update the chain state). There are at least two possible
    -- proper solutions for this:
    --
    -- 1. Modify the spec so that we /do/ have the separation. Note that if we
    --    did, we would not use the chain state part of the spec, since the
    --    chain state part of the dual ledger is determined entirely by the
    --    concrete Byron block.
    -- 2. Turn 'applyExtLedger' and related types into a type class of their
    --    own, so that we can override it specifically for the dual ledger.
    --
    -- Either way, we are only testing the /ledger/ part of the two blocks here,
    -- not the consensus part. For now we just override some parameters in the
    -- environment to work around the problem and make sure that none of the
    -- consensus checks in the spec can fail.
    disableConsensusChecks :: Spec.Environment Spec.CHAIN
                           -> Spec.Environment Spec.CHAIN
    disableConsensusChecks ( _currentSlot
                           , utx0
                           , delegators
                           , pparams
                           , k
                           ) = (
          -- Disable 'SlotInTheFuture' failure
          Spec.Slot maxBound
        , utx0
        , delegators
          -- Disable 'TooManyIssuedBlocks' failure
        , pparams { Spec._bkSgnCntT = Spec.BkSgnCntT 1 }
        , k
        )

-- | Construct genesis config from CHAIN environment
--
-- This doens't make an awful lot of sense, but the abstract spec doesn't /have/
-- a concept of a genesis config, and instead the CHAIN environment fulfills
-- that role. In order to be able to reuse the test generators, we therefore
-- also define a translation in the opposite direction.
fromChainEnv :: Spec.Environment Spec.CHAIN -> ByronSpecGenesis
fromChainEnv ( _currentSlot
             , byronSpecGenesisInitUtxo
             , byronSpecGenesisDelegators
             , byronSpecGenesisInitPParams
             , byronSpecGenesisSecurityParam
             ) = ByronSpecGenesis{..}
