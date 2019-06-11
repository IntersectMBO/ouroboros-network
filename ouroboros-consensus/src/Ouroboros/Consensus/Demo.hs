{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Instantiations of the protocol stack used in tests and demos
module Ouroboros.Consensus.Demo (
    -- * Abstract over protocols
    DemoProtocol(..)
  , protocolInfo
  , runDemo
    -- * Protocols used for the various demos
  , DemoBFT
  , DemoPraos
  , DemoLeaderSchedule
  , DemoMockPBFT
  , DemoRealPBFT
    -- * Support for running the demos
  , defaultSecurityParam
  , defaultDemoPraosParams
  , defaultDemoPBftParams
  , enumCoreNodes
  ) where

import           Data.Coerce
import           Data.Reflection (give)

import qualified Cardano.Chain.Block as Cardano.Block
import qualified Cardano.Chain.Genesis as Cardano.Genesis
import qualified Cardano.Crypto as Cardano

import           Ouroboros.Consensus.Demo.Ledger.Byron
import           Ouroboros.Consensus.Demo.Ledger.Mock ()
import           Ouroboros.Consensus.Demo.Ledger.Mock.BFT
import           Ouroboros.Consensus.Demo.Ledger.Mock.PBFT
import           Ouroboros.Consensus.Demo.Ledger.Mock.Praos
import           Ouroboros.Consensus.Demo.Ledger.Mock.PraosRule
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

{-------------------------------------------------------------------------------
  Protocols used for the various demos
-------------------------------------------------------------------------------}

type DemoBFT            = Bft BftMockCrypto
type DemoPraos          = ExtNodeConfig AddrDist (Praos PraosMockCrypto)
type DemoLeaderSchedule = WithLeaderSchedule (Praos PraosCryptoUnused)
type DemoMockPBFT       = ExtNodeConfig (PBftLedgerView PBftMockCrypto) (PBft PBftMockCrypto)
type DemoRealPBFT       = ExtNodeConfig ByronDemoConfig (PBft PBftCardanoCrypto)

{-------------------------------------------------------------------------------
  Abstract over the various protocols
-------------------------------------------------------------------------------}

-- | Consensus protocol to use
data DemoProtocol blk where
  -- | Run BFT against the mock ledger
  DemoBFT
    :: SecurityParam
    -> DemoProtocol (SimpleBftBlock SimpleMockCrypto BftMockCrypto)

  -- | Run Praos against the mock ledger
  DemoPraos
    :: PraosParams
    -> DemoProtocol (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)

  -- | Run Praos against the mock ledger but with an explicit leader schedule
  DemoLeaderSchedule
    :: PraosParams
    -> LeaderSchedule
    -> DemoProtocol (SimplePraosRuleBlock SimpleMockCrypto)

  -- | Run PBFT against the mock ledger
  DemoMockPBFT
    :: PBftParams
    -> DemoProtocol (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)

  -- | Run PBFT against the real ledger
  DemoRealPBFT
    :: PBftParams
    -> Cardano.Genesis.Config
    -> DemoProtocol (ByronBlock ByronDemoConfig)

-- | Info needed to run the selected protocol
protocolInfo :: NumCoreNodes
             -> CoreNodeId
             -> DemoProtocol blk
             -> ProtocolInfo blk
protocolInfo nodes nid demoProtocol =
    case demoProtocol of
      DemoBFT            params          -> protocolInfoBft       nodes nid params
      DemoPraos          params          -> protocolInfoPraos     nodes nid params
      DemoLeaderSchedule params schedule -> protocolInfoPraosRule nodes nid params schedule
      DemoMockPBFT       params          -> protocolInfoMockPBFT  nodes nid params
      DemoRealPBFT       params gc       -> protocolInfoByron     nodes nid params gc

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
    , pbftSignatureWindow    = fromIntegral $ nn * 10
    , pbftSignatureThreshold = (1.0 / fromIntegral nn) + 0.1
    }
  where
    nn = 3

enumCoreNodes :: NumCoreNodes -> [CoreNodeId]
enumCoreNodes (NumCoreNodes numNodes) = [ CoreNodeId n
                                        | n <- [0 .. numNodes - 1]
                                        ]

{-------------------------------------------------------------------------------
  Evidence that we can run all the supported demos
-------------------------------------------------------------------------------}

runDemo :: DemoProtocol blk -> Dict (RunDemo blk)
runDemo DemoBFT{}            = Dict
runDemo DemoPraos{}          = Dict
runDemo DemoLeaderSchedule{} = Dict
runDemo DemoMockPBFT{}       = Dict
runDemo DemoRealPBFT{}       = give (Dummy.dummyEpochSlots)
                             $ give (Cardano.Genesis.gdProtocolMagicId Dummy.dummyGenesisData)
                             $ give (coerce @_ @Cardano.Block.HeaderHash Dummy.dummyGenesisHash)
                             $ Dict
