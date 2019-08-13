{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ouroboros.Consensus.Node.ProtocolInfo.Mock.HardFork (
    protocolInfoMockHardFork
  , SimpleForkedBlock
  ) where

import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.HardFork
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.PBFT
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.Praos
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.HardFork
import           Ouroboros.Consensus.Protocol.PBFT hiding (pbftParams)
import           Ouroboros.Consensus.Protocol.Praos hiding (praosParams)

type SimpleForkedBlock
  = Forked
      (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
      (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)

protocolInfoMockHardFork
  :: NumCoreNodes
  -> CoreNodeId
  -> PBftParams
  -> PraosParams
  -> ProtocolInfo SimpleForkedBlock
protocolInfoMockHardFork numCoreNodes nid pbftParams praosParams =
  ProtocolInfo
    { pInfoConfig = ForkedNodeConfig
        { nodeConfigBeforeFork = pInfoConfig pInfoPbft
        , nodeConfigAfterFork = pInfoConfig pInfoPraos
        }
    , pInfoInitLedger = ExtLedgerState
      { ledgerState = ForkedLedgerState . BeforeFork . ledgerState $
          pInfoInitLedger pInfoPbft
      , ouroborosChainState = BeforeFork . ouroborosChainState $
          pInfoInitLedger pInfoPbft
      }
    , pInfoInitState = BeforeFork $ pInfoInitState pInfoPbft
    }
  where
    pInfoPbft = protocolInfoMockPBFT numCoreNodes nid pbftParams
    pInfoPraos = protocolInfoPraos numCoreNodes nid praosParams
