{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ouroboros.Consensus.Protocol.Translate (TranslateProto (..)) where

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked (Ticked)

-- | Translate across protocols
class TranslateProto protoFrom protoTo
  where
  translateConsensusConfig ::
    ConsensusConfig protoFrom -> ConsensusConfig protoTo
  translateLedgerView ::
    LedgerView protoFrom -> LedgerView protoTo
  -- | Translate the ticked ledger view.
  --
  -- In general, this function is trivially derivable from
  -- 'translateLedgerView', but since there's no general @ a -> Ticked a @
  -- class available, we include this funcion here.
  translateTickedLedgerView ::
    Ticked (LedgerView protoFrom) -> Ticked (LedgerView protoTo)
  translateChainDepState ::
    ChainDepState protoFrom -> ChainDepState protoTo

-- | Degenerate instance - we may always translate from a protocol to itself.
instance TranslateProto singleProto singleProto
  where
  translateConsensusConfig = id
  translateLedgerView = id
  translateTickedLedgerView = id
  translateChainDepState = id
