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

import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))

{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

newtype TranslateEraLedgerState blk blk' = TranslateEraLedgerState {
      translateLedgerStateWith :: LedgerConfig blk
                               -> LedgerConfig blk'
                               -> EpochNo
                               -> LedgerState blk
                               -> LedgerState blk'
    }

newtype TranslateEraLedgerView blk blk' = TranslateEraLedgerView {
      translateLedgerViewWith :: LedgerConfig blk
                              -> LedgerConfig blk'
                              -> EpochNo
                              -> LedgerView (BlockProtocol blk)
                              -> LedgerView (BlockProtocol blk')
    }

newtype TranslateEraConsensusState blk blk' = TranslateEraConsensusState {
      translateConsensusStateWith :: ConsensusConfig (BlockProtocol blk)
                                  -> ConsensusConfig (BlockProtocol blk')
                                  -> EpochNo
                                  -> ConsensusState (BlockProtocol blk)
                                  -> ConsensusState (BlockProtocol blk')
    }

data EraTranslation xs = EraTranslation {
      translateLedgerState    :: InPairs TranslateEraLedgerState xs
    , translateLedgerView     :: InPairs TranslateEraLedgerView xs
    , translateConsensusState :: InPairs TranslateEraConsensusState xs
    }
  deriving NoUnexpectedThunks
       via OnlyCheckIsWHNF "EraTranslation" (EraTranslation xs)

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation = EraTranslation {
      translateLedgerState    = PNil
    , translateLedgerView     = PNil
    , translateConsensusState = PNil
    }
