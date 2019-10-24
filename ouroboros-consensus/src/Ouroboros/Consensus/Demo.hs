{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

-- | Instantiations of the protocol stack used in demos
module Ouroboros.Consensus.Demo (
    -- * Support for running the demos
    defaultSecurityParam
  , defaultDemoPraosParams
  , defaultDemoPBftParams
  , runDemo
  ) where

import           Data.Reflection (give)

import qualified Cardano.Chain.Genesis as Cardano.Genesis

import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

{-------------------------------------------------------------------------------
  Support for running the demos
-------------------------------------------------------------------------------}

defaultSecurityParam :: SecurityParam
defaultSecurityParam = SecurityParam 5

defaultDemoPraosParams :: PraosParams
defaultDemoPraosParams = PraosParams {
      praosSecurityParam = defaultSecurityParam
    , praosSlotsPerEpoch = 3
    , praosLeaderF       = 0.5
    , praosLifetimeKES   = 1000000
    }

defaultDemoPBftParams :: PBftParams
defaultDemoPBftParams = PBftParams {
      pbftSecurityParam      = defaultSecurityParam
    , pbftNumNodes           = nn
    , pbftSignatureThreshold = (1.0 / fromIntegral nn) + 0.1
    }
  where
    nn = 3

{-------------------------------------------------------------------------------
  Evidence that we can run all the supported protocols
-------------------------------------------------------------------------------}

runDemo :: Protocol blk -> Dict (RunDemo blk)
runDemo ProtocolMockBFT{}        = Dict
runDemo ProtocolMockPraos{}      = Dict
runDemo ProtocolLeaderSchedule{} = Dict
runDemo ProtocolMockPBFT{}       = Dict
runDemo ProtocolRealPBFT{}       = give (Dummy.dummyEpochSlots)
                                 $ give (Cardano.Genesis.gdProtocolMagicId Dummy.dummyGenesisData)
                                 $ Dict
