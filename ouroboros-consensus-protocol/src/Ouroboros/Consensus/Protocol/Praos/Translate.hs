{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Protocol.Praos.Translate () where

import           Cardano.Crypto.DSIGN (VerKeyDSIGN)
import           Cardano.Crypto.VRF (VerKeyVRF)
import qualified Cardano.Ledger.Chain as SL
import           Cardano.Ledger.Crypto (ADDRHASH, HASH)
import qualified Cardano.Ledger.PoolDistr as SL
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as SL
import qualified Cardano.Protocol.TPraos.Rules.Tickn as SL
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Protocol.Praos (ConsensusConfig (..),
                     Praos, PraosParams (..), PraosState (..),
                     Ticked (TickedPraosLedgerView))
import           Ouroboros.Consensus.Protocol.Praos.Views (LedgerView (..))
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import           Ouroboros.Consensus.Protocol.TPraos (TPraos, TPraosParams (..),
                     TPraosState (tpraosStateChainDepState, tpraosStateLastSlot))
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Protocol.Translate (TranslateProto (..))

{-------------------------------------------------------------------------------
  Translation from transitional Praos
-------------------------------------------------------------------------------}

-- | We can translate between TPraos and Praos, provided:
--
-- - They share the same HASH algorithm
-- - They share the same ADDRHASH algorithm
-- - They share the same DSIGN verification keys
-- - They share the same VRF verification keys
instance
  ( HASH c1 ~ HASH c2,
    ADDRHASH c1 ~ ADDRHASH c2,
    VerKeyDSIGN c1 ~ VerKeyDSIGN c2,
    VerKeyVRF c1 ~ VerKeyVRF c2
  ) =>
  TranslateProto (TPraos c1) (Praos c2)
  where
  translateConsensusConfig TPraosConfig {tpraosParams, tpraosEpochInfo} =
    PraosConfig
      { praosParams =
          PraosParams
            { praosSlotsPerKESPeriod = tpraosSlotsPerKESPeriod tpraosParams,
              praosLeaderF = tpraosLeaderF tpraosParams,
              praosSecurityParam = tpraosSecurityParam tpraosParams,
              praosMaxKESEvo = tpraosMaxKESEvo tpraosParams,
              praosQuorum = tpraosQuorum tpraosParams,
              praosMaxMajorPV = tpraosMaxMajorPV tpraosParams,
              praosMaxLovelaceSupply = tpraosMaxLovelaceSupply tpraosParams,
              praosNetworkId = tpraosNetworkId tpraosParams,
              praosSystemStart = tpraosSystemStart tpraosParams
            },
        praosEpochInfo = tpraosEpochInfo
      }

  translateTickedLedgerView (TPraos.TickedPraosLedgerView lv) =
      TickedPraosLedgerView $ translateLedgerView lv
    where
      translateLedgerView SL.LedgerView {SL.lvPoolDistr, SL.lvChainChecks} =
        Views.LedgerView
          { Views.lvPoolDistr = coercePoolDistr lvPoolDistr,
            lvMaxHeaderSize = SL.ccMaxBHSize lvChainChecks,
            lvMaxBodySize = SL.ccMaxBBSize lvChainChecks,
            lvProtocolVersion = SL.ccProtocolVersion lvChainChecks
          }
        where
          coercePoolDistr :: SL.PoolDistr c1 -> SL.PoolDistr c2
          coercePoolDistr (SL.PoolDistr m) =
            SL.PoolDistr
              . Map.mapKeysMonotonic coerce
              . Map.map coerceIndividualPoolStake
              $ m
          coerceIndividualPoolStake :: SL.IndividualPoolStake c1 -> SL.IndividualPoolStake c2
          coerceIndividualPoolStake (SL.IndividualPoolStake stake vrf) =
            SL.IndividualPoolStake stake $ coerce vrf

  translateChainDepState tpState =
    PraosState
      { praosStateLastSlot = tpraosStateLastSlot tpState,
        praosStateOCertCounters = Map.mapKeysMonotonic coerce certCounters,
        praosStateEvolvingNonce = evolvingNonce,
        praosStateCandidateNonce = candidateNonce,
        praosStateEpochNonce = SL.ticknStateEpochNonce csTickn,
        praosStateLabNonce = csLabNonce,
        praosStateLastEpochBlockNonce = SL.ticknStatePrevHashNonce csTickn
      }
    where
      SL.ChainDepState {SL.csProtocol, SL.csTickn, SL.csLabNonce} =
        tpraosStateChainDepState tpState
      SL.PrtclState certCounters evolvingNonce candidateNonce =
        csProtocol


{-------------------------------------------------------------------------------
  Translation between Praos using different cryptos
-------------------------------------------------------------------------------}

-- FIXME this instance overlaps with the "degenerate instance" defined in Translate module
instance
  ( HASH c1 ~ HASH c2,
    ADDRHASH c1 ~ ADDRHASH c2,
    VerKeyDSIGN c1 ~ VerKeyDSIGN c2
  ) =>
  TranslateProto (Praos c1) (Praos c2)
  where
  translateConsensusConfig = id

  translateTickedLedgerView (TickedPraosLedgerView LedgerView{lvPoolDistr, lvMaxHeaderSize, lvMaxBodySize, lvProtocolVersion }) =
      TickedPraosLedgerView LedgerView { lvPoolDistr = translatedPoolDistr lvPoolDistr, lvMaxBodySize, lvMaxHeaderSize, lvProtocolVersion}

  translateChainDepState = id

translatedPoolDistr ::   forall c1 c2.  (VerKeyVRF c1 ~ VerKeyVRF c2  ) => SL.PoolDistr c1 -> SL.PoolDistr c2
translatedPoolDistr poolDistr = coercePoolDistr poolDistr
  where
      coercePoolDistr :: SL.PoolDistr c1 -> SL.PoolDistr c2
      coercePoolDistr (SL.PoolDistr m) =
            SL.PoolDistr
              . Map.mapKeysMonotonic coerce
              . Map.map coerceIndividualPoolStake
              $ m
      coerceIndividualPoolStake :: SL.IndividualPoolStake c1 -> SL.IndividualPoolStake c2
      coerceIndividualPoolStake (SL.IndividualPoolStake stake vrf) =
            SL.IndividualPoolStake stake $ coerce vrf
