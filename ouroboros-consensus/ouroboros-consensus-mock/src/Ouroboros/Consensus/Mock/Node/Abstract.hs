{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node.Abstract (
    CodecConfig(..)
  , RunMockBlock(..)
  , constructMockProtocolMagicId
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Data.Hashable (hash)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           GHC.Stack

import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Protocol.Abstract


-- | Protocol specific functionality required to run consensus with mock blocks
class MockProtocolSpecific c ext => RunMockBlock c ext where
  mockProtocolMagicId
    :: BlockConfig (SimpleBlock c ext)
    -> ProtocolMagicId
  mockEncodeConsensusState
    :: CodecConfig (SimpleBlock c ext)
    -> ConsensusState (BlockProtocol (SimpleBlock c ext)) -> Encoding
  mockDecodeConsensusState
    :: CodecConfig (SimpleBlock c ext)
    -> Decoder s (ConsensusState (BlockProtocol (SimpleBlock c ext)))

-- | Construct protocol magic ID depending on where in the code this is called
--
-- The sole purpose of this is to make sure that these mock protocols have
-- different IDs from each other and from regular protocols.
constructMockProtocolMagicId :: HasCallStack => ProtocolMagicId
constructMockProtocolMagicId =
    ProtocolMagicId $ fromIntegral $ hash (prettyCallStack callStack)

instance RunMockBlock c ext
      => ConfigSupportsNode (SimpleBlock c ext) where
  getSystemStart = const $ SystemStart dummyDate
    where
      --  This doesn't matter much
      dummyDate = UTCTime (fromGregorian 2019 8 13) 0
  getNetworkMagic    = const $ NetworkMagic 0x0000ffff
  getProtocolMagicId = mockProtocolMagicId
