{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.Mock.Node.Abstract (
    RunMockBlock(..)
  , constructMockProtocolMagicId
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Data.Hashable (hash)
import           GHC.Stack

import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Protocol.Abstract

-- | Protocol specific functionality required to run consensus with mock blocks
class MockProtocolSpecific c ext => RunMockBlock c ext where
  mockProtocolMagicId
    :: BlockConfig (SimpleBlock c ext)
    -> ProtocolMagicId
  mockEncodeConsensusState
    :: TopLevelConfig (SimpleBlock c ext)
    -> ConsensusState (BlockProtocol (SimpleBlock c ext)) -> Encoding
  mockDecodeConsensusState
    :: TopLevelConfig (SimpleBlock c ext)
    -> Decoder s (ConsensusState (BlockProtocol (SimpleBlock c ext)))

-- | Construct protocol magic ID depending on where in the code this is called
--
-- The sole purpose of this is to make sure that these mock protocols have
-- different IDs from each other and from regular protocols.
constructMockProtocolMagicId :: HasCallStack => ProtocolMagicId
constructMockProtocolMagicId =
    ProtocolMagicId $ fromIntegral $ hash (prettyCallStack callStack)
