{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Ouroboros.Consensus.HardFork.Combinator.Translation (
    -- * Translate from one era to the next
    TranslateEraLedgerState(..)
  , TranslateEraLedgerView(..)
  , TranslateEraConsensusState(..)
  , EraTranslation(..)
  , trivialEraTranslation
  ) where

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..), RequiringBoth (..))

{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

newtype TranslateEraLedgerState blk blk' = TranslateEraLedgerState {
      translateLedgerStateWith :: EpochNo
                               -> LedgerState blk
                               -> LedgerState blk'
    }

newtype TranslateEraLedgerView blk blk' = TranslateEraLedgerView {
      translateLedgerViewWith :: EpochNo
                              -> LedgerView (BlockProtocol blk)
                              -> LedgerView (BlockProtocol blk')
    }

newtype TranslateEraConsensusState blk blk' = TranslateEraConsensusState {
      translateConsensusStateWith :: EpochNo
                                  -> ConsensusState (BlockProtocol blk)
                                  -> ConsensusState (BlockProtocol blk')
    }

data EraTranslation xs = EraTranslation {
      translateLedgerState    :: InPairs (RequiringBoth WrapLedgerConfig    TranslateEraLedgerState)    xs
    , translateLedgerView     :: InPairs (RequiringBoth WrapLedgerConfig    TranslateEraLedgerView)     xs
    , translateConsensusState :: InPairs (RequiringBoth WrapConsensusConfig TranslateEraConsensusState) xs
    }
  deriving NoUnexpectedThunks
       via OnlyCheckIsWHNF "EraTranslation" (EraTranslation xs)

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation = EraTranslation {
      translateLedgerState    = PNil
    , translateLedgerView     = PNil
    , translateConsensusState = PNil
    }
