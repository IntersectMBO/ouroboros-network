{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
-- | ProtocolInfo
module Ouroboros.Consensus.Node.ProtocolInfo (
    -- * ProtocolInfo
    module X
    -- * Data required to run a protocol
  , protocolInfo
  ) where

import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.Ledger.Dual.Byron (protocolInfoDualByron)
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract as X
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron as X
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.BFT as X
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.PBFT as X
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.Praos as X
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.PraosRule as X
import           Ouroboros.Consensus.Protocol

import           Ouroboros.Consensus.Ledger.OddChain
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Cardano.Crypto.DSIGN
import           Ouroboros.Network.Block (ChainHash(GenesisHash), pattern BlockPoint, pattern GenesisPoint)

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the selected protocol
protocolInfo :: Protocol blk -> ProtocolInfo blk
protocolInfo (ProtocolMockBFT nodes nid params slotLengths) =
    protocolInfoBft nodes nid params slotLengths

protocolInfo (ProtocolMockPraos nodes nid params) =
    protocolInfoPraos nodes nid params

protocolInfo (ProtocolLeaderSchedule nodes nid params schedule) =
    protocolInfoPraosRule nodes nid params schedule

protocolInfo (ProtocolMockPBFT params nid) =
    protocolInfoMockPBFT params nid

protocolInfo (ProtocolRealPBFT gc mthr prv swv mplc) =
    protocolInfoByron gc mthr prv swv mplc

protocolInfo (ProtocolDualPBFT abstractEnv params mLeader) =
    protocolInfoDualByron abstractEnv params mLeader

protocolInfo (ProtocolOdd numCoreNodes nid securityParam slotLengths numSlotsPerEpoch startTime currentSlot ) =
  ProtocolInfo
  { pInfoConfig = ExtNodeConfig
                  { extNodeConfig =
                    OddConfig
                    { slotsPerEpoch = numSlotsPerEpoch
                    , cfgNodeStartTime = startTime
                    }

                  , extNodeConfigP
                      = BftNodeConfig
                        { bftParams   =
                            BftParams
                            { bftNumNodes      = numCoreNodes
                            , bftSecurityParam = securityParam
                            , bftSlotLengths   = slotLengths
                            }
                        , bftNodeId   = CoreId nid
                        , bftSignKey  = signKey nid
                        , bftVerKeys  =
                            Map.fromList [ (CoreId n, verKey n)
                                         | n <- enumCoreNodes numCoreNodes
                                         ]
                        }
                  }
  , pInfoInitState  = () -- :: NodeState  (BlockProtocol b)
  , pInfoInitLedger = ExtLedgerState
                      { ledgerState = LedgerState
                                      { stLastApplied = GenesisPoint
                                      , phase = Increase 0
                                      }
                      , headerState = genesisHeaderState ()
                      }
  }
  where
    signKey :: CoreNodeId -> SignKeyDSIGN MockDSIGN
    signKey (CoreNodeId n) = SignKeyMockDSIGN (fromIntegral n)

    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN (fromIntegral n)
