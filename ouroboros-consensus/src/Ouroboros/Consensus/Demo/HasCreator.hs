{-# LANGUAGE FlexibleInstances #-}

-- | In tests and the demo we want to be able to map a block to the core
-- node that produced it
module Ouroboros.Consensus.Demo.HasCreator (
    HasCreator(..)
  ) where

import           Ouroboros.Consensus.Node (CoreNodeId (..))

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
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

{-
instance HasCreator (ByronBlock ByronDemo.Config) where
    getCreator (EncNodeConfig _ ByronDemo.Config{..}) (ByronBlock b) =
        fromMaybe (error "getCreator: unknown key") $ Bimap.lookup key pbftCoreNodes
     where
       key :: Cardano.VerificationKey
       key = Cardano.pskIssuerVK
             . Cardano.psigPsk
             . Cardano.Block.unBlockSignature
             . Cardano.Block.headerSignature
             . Cardano.Block.blockHeader
             $ b
-}
