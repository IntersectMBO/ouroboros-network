{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Node configuration common to all (era, protocol) combinations deriving from
-- Shelley.
module Ouroboros.Consensus.Shelley.Node.Common (
    ProtocolParamsShelleyBased (..)
  , ShelleyEraWithCrypto
  , ShelleyLeaderCredentials (..)
  , shelleyBlockIssuerVKey
  , translateShelleyLeaderCredentials
  ) where

import qualified Cardano.Ledger.Keys as SL
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Ledger.Slot
import           Data.Text (Text)
import           Ouroboros.Consensus.Block (CannotForge, ForgeStateInfo,
                     ForgeStateUpdateError)
import           Ouroboros.Consensus.Config (maxRollbacks)
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Mempool (TxLimits)
import           Ouroboros.Consensus.Node.InitStorage
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (PraosCanBeLeader (praosCanBeLeaderColdVerKey), translateCanBeLeader)
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible, shelleyNetworkMagic,
                     shelleyStorageConfigSecurityParam,
                     shelleyStorageConfigSlotsPerKESPeriod, shelleySystemStart,
                     verifyBlockIntegrity)
import           Ouroboros.Consensus.Shelley.Protocol.Abstract
                     (ProtocolHeaderSupportsProtocol (CannotForgeError))
import           Ouroboros.Consensus.Storage.ImmutableDB
import Ouroboros.Consensus.Protocol.Praos.Crypto (CanConvertVRF, VRF, KES)
import Cardano.Ledger.Crypto (DSIGN)

{-------------------------------------------------------------------------------
  Credentials
-------------------------------------------------------------------------------}

data ShelleyLeaderCredentials c = ShelleyLeaderCredentials
  { -- | The unevolved signing KES key (at evolution 0).
    --
    -- Note that this is not inside 'ShelleyCanBeLeader' since it gets evolved
    -- automatically, whereas 'ShelleyCanBeLeader' does not change.
    shelleyLeaderCredentialsInitSignKey :: SL.SignKeyKES c,
    shelleyLeaderCredentialsCanBeLeader :: PraosCanBeLeader c,
    -- | Identifier for this set of credentials.
    --
    -- Useful when the node is running with multiple sets of credentials.
    shelleyLeaderCredentialsLabel       :: Text
  }

translateShelleyLeaderCredentials ::
  (CanConvertVRF (VRF c1) (VRF c2), DSIGN c1 ~ DSIGN c2, KES c1 ~ KES c2)
  => ShelleyLeaderCredentials c1 -> ShelleyLeaderCredentials c2
translateShelleyLeaderCredentials
   ShelleyLeaderCredentials
        { shelleyLeaderCredentialsInitSignKey
        , shelleyLeaderCredentialsCanBeLeader
        , shelleyLeaderCredentialsLabel
        } =  ShelleyLeaderCredentials
            { shelleyLeaderCredentialsInitSignKey
            , shelleyLeaderCredentialsCanBeLeader = translateCanBeLeader shelleyLeaderCredentialsCanBeLeader
            , shelleyLeaderCredentialsLabel
            }

shelleyBlockIssuerVKey ::
  ShelleyLeaderCredentials c -> SL.VKey 'SL.BlockIssuer c
shelleyBlockIssuerVKey =
  praosCanBeLeaderColdVerKey . shelleyLeaderCredentialsCanBeLeader

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

type instance CannotForge (ShelleyBlock proto era) = CannotForgeError proto

type instance ForgeStateInfo (ShelleyBlock proto era) = HotKey.KESInfo

type instance ForgeStateUpdateError (ShelleyBlock proto era) = HotKey.KESEvolutionError

-- | Needed in '*SharedBlockForging' because we can't partially apply
-- equality constraints.
class
  (ShelleyCompatible proto era, TxLimits (ShelleyBlock proto era), EraCrypto era ~ c) =>
  ShelleyEraWithCrypto c proto era

instance
  (ShelleyCompatible proto era, TxLimits (ShelleyBlock proto era), EraCrypto era ~ c) =>
  ShelleyEraWithCrypto c proto era

{-------------------------------------------------------------------------------
  ConfigSupportsNode instance
-------------------------------------------------------------------------------}

instance ConfigSupportsNode (ShelleyBlock proto era) where
  getSystemStart = shelleySystemStart
  getNetworkMagic = shelleyNetworkMagic

{-------------------------------------------------------------------------------
  NodeInitStorage instance
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => NodeInitStorage (ShelleyBlock proto era) where
  -- We fix the chunk size to @10k@ so that we have the same chunk size as
  -- Byron. Consequently, a Shelley net will have the same chunk size as the
  -- Byron-to-Shelley net with the same @k@.
  nodeImmutableDbChunkInfo =
    simpleChunkInfo
      . EpochSize
      . (* 10)
      . maxRollbacks
      . shelleyStorageConfigSecurityParam

  nodeCheckIntegrity cfg =
    verifyBlockIntegrity (shelleyStorageConfigSlotsPerKESPeriod cfg)

{-------------------------------------------------------------------------------
  Protocol parameters
-------------------------------------------------------------------------------}

-- | Parameters common to all Shelley-based ledgers.
--
-- When running a chain with multiple Shelley-based eras, in addition to the
-- per-era protocol parameters, one value of 'ProtocolParamsShelleyBased' will
-- be needed, which is shared among all Shelley-based eras.
--
-- The @era@ parameter determines from which era the genesis config will be
-- used.
data ProtocolParamsShelleyBased era = ProtocolParamsShelleyBased
  { shelleyBasedGenesis           :: SL.ShelleyGenesis (EraCrypto era),
    -- | The initial nonce, typically derived from the hash of Genesis
    -- config JSON file.
    --
    -- WARNING: chains using different values of this parameter will be
    -- mutually incompatible.
    shelleyBasedInitialNonce      :: SL.Nonce,
    shelleyBasedLeaderCredentials :: [ShelleyLeaderCredentials (EraCrypto era)]
  }
