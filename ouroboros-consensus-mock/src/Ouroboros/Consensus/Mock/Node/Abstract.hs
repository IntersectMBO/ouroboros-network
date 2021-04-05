{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node.Abstract (
    CodecConfig (..)
  , RunMockBlock (..)
  , constructMockNetworkMagic
  ) where

import           Data.Hashable (hash)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           GHC.Stack

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.Storage.Serialisation

-- | Protocol specific functionality required to run consensus with mock blocks
class ( MockProtocolSpecific c ext
      , EncodeDisk (SimpleBlock c ext) (ChainDepState (BlockProtocol (SimpleBlock c ext)))
      , DecodeDisk (SimpleBlock c ext) (ChainDepState (BlockProtocol (SimpleBlock c ext)))
      ) => RunMockBlock c ext where
  mockNetworkMagic
    :: BlockConfig (SimpleBlock c ext)
    -> NetworkMagic

-- | Construct protocol magic ID depending on where in the code this is called
--
-- The sole purpose of this is to make sure that these mock protocols have
-- different IDs from each other and from regular protocols.
constructMockNetworkMagic :: HasCallStack => NetworkMagic
constructMockNetworkMagic =
    NetworkMagic $ fromIntegral $ hash (prettyCallStack callStack)

instance RunMockBlock c ext
      => ConfigSupportsNode (SimpleBlock c ext) where
  getSystemStart = const $ SystemStart dummyDate
    where
      --  This doesn't matter much
      dummyDate = UTCTime (fromGregorian 2019 8 13) 0

  getNetworkMagic = mockNetworkMagic
