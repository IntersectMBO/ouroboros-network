{-# LANGUAGE FlexibleInstances #-}

-- | In tests we want to be able to map a block to the core node that produced
-- it.
module Test.Util.HasCreator (
    HasCreator(..)
  ) where

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.Praos

class HasCreator b where
    getCreator :: b -> CoreNodeId

instance HasCreator (SimpleBftBlock c BftMockCrypto) where
    getCreator = CoreNodeId
               . verKeyIdFromSigned
               . bftSignature
               . simpleBftExt
               . simpleHeaderExt
               . simpleHeader

instance HasCreator (SimplePBftBlock c PBftMockCrypto) where
    getCreator = CoreNodeId
               . verKeyIdFromSigned
               . pbftSignature
               . simplePBftExt
               . simpleHeaderExt
               . simpleHeader

instance HasCreator (SimplePraosBlock c PraosMockCrypto) where
    getCreator = praosCreator
               . praosExtraFields
               . simplePraosExt
               . simpleHeaderExt
               . simpleHeader

instance HasCreator (SimplePraosRuleBlock c) where
    getCreator = simplePraosRuleExt
               . simpleHeaderExt
               . simpleHeader
