{-# LANGUAGE FlexibleInstances #-}

-- | In tests we want to be able to map a block to the core node that produced
-- it.
module Test.Util.HasCreator (
    HasCreator(..)
  ) where

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.HardFork
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.HardFork
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.Praos

class HasCreator b where
    getCreator :: NodeConfig (BlockProtocol b) -> b -> CoreNodeId

instance HasCreator (SimpleBftBlock c BftMockCrypto) where
    getCreator _ = CoreNodeId
                 . verKeyIdFromSigned
                 . bftSignature
                 . simpleBftExt
                 . simpleHeaderExt
                 . simpleHeader

instance HasCreator (SimplePBftBlock c PBftMockCrypto) where
    getCreator _ = CoreNodeId
                 . verKeyIdFromSigned
                 . pbftSignature
                 . simplePBftExt
                 . simpleHeaderExt
                 . simpleHeader

instance HasCreator (SimplePraosBlock c PraosMockCrypto) where
    getCreator _ = praosCreator
                 . praosExtraFields
                 . simplePraosExt
                 . simpleHeaderExt
                 . simpleHeader

instance HasCreator (SimplePraosRuleBlock c) where
    getCreator _ = simplePraosRuleExt
                 . simpleHeaderExt
                 . simpleHeader

instance
  HasCreator
    ( Forked
        (SimplePBftBlock c PBftMockCrypto)
        (SimplePraosBlock c PraosMockCrypto)
    ) where
  getCreator nodeConfig =
    forked
      (getCreator (nodeConfigBeforeFork nodeConfig))
      (getCreator (nodeConfigAfterFork nodeConfig))
