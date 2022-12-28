{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.Shelley.Node.Praos (
    ProtocolParamsBabbage (..)
  , praosBlockForging
  , praosSharedBlockForging
  ) where

import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.OCert as Absolute
import qualified Cardano.Protocol.TPraos.OCert as SL
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config (configConsensus)
import           Ouroboros.Consensus.Mempool.TxLimits
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.Praos (Praos, PraosParams (..),
                     praosCheckCanForge)
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (PraosCanBeLeader (praosCanBeLeaderOpCert))
import           Ouroboros.Consensus.Shelley.Eras (BabbageEra, EraCrypto,
                     ShelleyBasedEra (shelleyBasedEraName))
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible, forgeShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node
                     (ShelleyLeaderCredentials (..))
import           Ouroboros.Consensus.Shelley.Node.Common (ShelleyEraWithCrypto)
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.Util.IOLike (IOLike)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

-- | Create a 'BlockForging' record for a single era.
praosBlockForging ::
  forall m era c.
  ( ShelleyCompatible (Praos c) era,
    c ~ EraCrypto era,
    TxLimits (ShelleyBlock (Praos c) era),
    IOLike m
  ) =>
  PraosParams ->
  TxLimits.Overrides (ShelleyBlock (Praos c) era) ->
  ShelleyLeaderCredentials (EraCrypto era) ->
  m (BlockForging m (ShelleyBlock (Praos c) era))
praosBlockForging praosParams maxTxCapacityOverrides credentials = do
    hotKey <- HotKey.mkHotKey @m @c initSignKey startPeriod praosMaxKESEvo
    pure $ praosSharedBlockForging hotKey slotToPeriod credentials maxTxCapacityOverrides
  where
    PraosParams {praosMaxKESEvo, praosSlotsPerKESPeriod} = praosParams

    ShelleyLeaderCredentials {
        shelleyLeaderCredentialsInitSignKey = initSignKey
      , shelleyLeaderCredentialsCanBeLeader = canBeLeader
      } = credentials

    startPeriod :: Absolute.KESPeriod
    startPeriod = SL.ocertKESPeriod $ praosCanBeLeaderOpCert canBeLeader

    slotToPeriod :: SlotNo -> Absolute.KESPeriod
    slotToPeriod (SlotNo slot) =
      SL.KESPeriod $ fromIntegral $ slot `div` praosSlotsPerKESPeriod

-- | Create a 'BlockForging' record safely using the given 'Hotkey'.
--
-- The name of the era (separated by a @_@) will be appended to each
-- 'forgeLabel'.
praosSharedBlockForging ::
  forall m c era.
  ( ShelleyEraWithCrypto c (Praos c) era,
    IOLike m
  )
  => HotKey.HotKey c m
  -> (SlotNo -> Absolute.KESPeriod)
  -> ShelleyLeaderCredentials c
  -> TxLimits.Overrides (ShelleyBlock (Praos c) era)
  -> BlockForging m     (ShelleyBlock (Praos c) era)
praosSharedBlockForging
  hotKey
  slotToPeriod
  ShelleyLeaderCredentials
    { shelleyLeaderCredentialsCanBeLeader = canBeLeader,
      shelleyLeaderCredentialsLabel = label
    }
  maxTxCapacityOverrides = do
      BlockForging
        { forgeLabel = label <> "_" <> shelleyBasedEraName (Proxy @era),
          canBeLeader = canBeLeader,
          updateForgeState = \_ curSlot _ ->
            forgeStateUpdateInfoFromUpdateInfo
              <$> HotKey.evolve hotKey (slotToPeriod curSlot),
          checkCanForge = \cfg curSlot _tickedChainDepState _isLeader ->
            praosCheckCanForge
              (configConsensus cfg)
              curSlot,
          forgeBlock = \cfg ->
            forgeShelleyBlock
              hotKey
              canBeLeader
              cfg
              maxTxCapacityOverrides
        }

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Parameters needed to run Babbage
data ProtocolParamsBabbage c = ProtocolParamsBabbage
  { babbageProtVer :: SL.ProtVer,
    babbageMaxTxCapacityOverrides :: TxLimits.Overrides (ShelleyBlock (Praos c) (BabbageEra c))
  }

