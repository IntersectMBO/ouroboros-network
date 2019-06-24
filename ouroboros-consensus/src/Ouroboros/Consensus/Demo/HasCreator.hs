{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

-- | In tests and the demo we want to be able to map a block to the core
-- node that produced it
module Ouroboros.Consensus.Demo.HasCreator (
    HasCreator(..)
  ) where

import qualified Data.Bimap as Bimap
import           Data.Maybe (fromMaybe)

import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Delegation as CC.Delegation
import qualified Cardano.Crypto as Cardano

import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Demo.Ledger.Byron.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
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

instance HasCreator (ByronBlock ByronDemoConfig) where
    getCreator (EncNodeConfig _ ByronDemoConfig{..}) (ByronBlock b) =
        fromMaybe (error "getCreator: unknown key") $
          Bimap.lookup key pbftCoreNodes
      where
        key :: Cardano.VerificationKey
        key = CC.Delegation.issuerVK
            . CC.Block.delegationCertificate
            . CC.Block.headerSignature
            . CC.Block.blockHeader
            $ b
