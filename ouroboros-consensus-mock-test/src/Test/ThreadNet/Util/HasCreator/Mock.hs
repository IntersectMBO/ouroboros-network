{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.Util.HasCreator.Mock (

  ) where

import           Cardano.Crypto.DSIGN
import           Data.Word (Word64)

import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.PBFT

import           Test.ThreadNet.Util.HasCreator


instance HasCreator (SimpleBftBlock c BftMockCrypto) where
    getCreator = coreNodeId
               . bftSignature
               . simpleBftExt
               . simpleHeaderExt
               . simpleHeader
      where
        coreNodeId :: SignedDSIGN MockDSIGN a -> CoreNodeId
        coreNodeId = CoreNodeId . verKeyIdFromSigned

instance HasCreator (SimplePBftBlock c PBftMockCrypto) where
    getCreator = coreNodeId
               . pbftSignature
               . simplePBftExt
               . simpleHeaderExt
               . simpleHeader
      where
        coreNodeId :: SignedDSIGN MockDSIGN a -> CoreNodeId
        coreNodeId = CoreNodeId . verKeyIdFromSigned

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

-- | Get the id of the signer from a signature. Used for testing.
verKeyIdFromSigned :: SignedDSIGN MockDSIGN a -> Word64
verKeyIdFromSigned (SignedDSIGN (SigMockDSIGN _ i)) = i
