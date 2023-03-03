{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Various things common to iterations of the Praos protocol.
module Ouroboros.Consensus.Protocol.Praos.Common (
    MaxMajorProtVer (..)
  , PraosCanBeLeader (..)
  , PraosChainSelectView (..)
    -- * node support
  , PraosNonces (..)
  , PraosProtocolSupportsNode (..)
  ) where

import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Ledger.BaseTypes (Nonce, Version)
import           Cardano.Ledger.Crypto (Crypto, VRF)
import           Cardano.Ledger.Keys (KeyHash, KeyRole (BlockIssuer))
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.OCert as OCert
import           Cardano.Slotting.Block (BlockNo)
import           Cardano.Slotting.Slot (SlotNo)
import           Data.Function (on)
import           Data.Map.Strict (Map)
import           Data.Ord (Down (Down))
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Protocol.Abstract

-- | The maximum major protocol version.
--
-- Must be at least the current major protocol version. For Cardano mainnet, the
-- Shelley era has major protocol verison __2__.
newtype MaxMajorProtVer = MaxMajorProtVer
  { getMaxMajorProtVer :: Version
  }
  deriving (Eq, Show, Generic)
  deriving newtype NoThunks

-- | View of the ledger tip for chain selection.
--
-- We order between chains as follows:
--
-- 1. By chain length, with longer chains always preferred.
-- 2. If the tip of each chain was issued by the same agent, then we prefer
--    the chain whose tip has the highest ocert issue number.
-- 3. By a VRF value from the chain tip, with lower values preferred. See
--    @pTieBreakVRFValue@ for which one is used.
data PraosChainSelectView c = PraosChainSelectView
  { csvChainLength :: BlockNo,
    csvSlotNo      :: SlotNo,
    csvIssuer      :: SL.VKey 'SL.BlockIssuer c,
    csvIssueNo     :: Word64,
    csvTieBreakVRF :: VRF.OutputVRF (VRF c)
  }
  deriving (Show, Eq, Generic, NoThunks)

instance Crypto c => Ord (PraosChainSelectView c) where
  compare =
    mconcat
      [ compare `on` csvChainLength,
        whenSame csvIssuer (compare `on` csvIssueNo),
        compare `on` Down . csvTieBreakVRF
      ]
    where
      -- When the @a@s are equal, use the given comparison function,
      -- otherwise, no preference.
      whenSame ::
        Eq a =>
        (view -> a) ->
        (view -> view -> Ordering) ->
        (view -> view -> Ordering)
      whenSame f comp v1 v2
        | f v1 == f v2 =
            comp v1 v2
        | otherwise =
            EQ

data PraosCanBeLeader c = PraosCanBeLeader
  { -- | Certificate delegating rights from the stake pool cold key (or
    -- genesis stakeholder delegate cold key) to the online KES key.
    praosCanBeLeaderOpCert     :: !(OCert.OCert c),
    -- | Stake pool cold key or genesis stakeholder delegate cold key.
    praosCanBeLeaderColdVerKey :: !(SL.VKey 'SL.BlockIssuer c),
    praosCanBeLeaderSignKeyVRF :: !(SL.SignKeyVRF c)
  }
  deriving (Generic)

instance Crypto c => NoThunks (PraosCanBeLeader c)

-- | See 'PraosProtocolSupportsNode'
data PraosNonces = PraosNonces {
    candidateNonce   :: !Nonce
  , epochNonce       :: !Nonce
  , evolvingNonce    :: !Nonce
    -- | Nonce constructed from the hash of the Last Applied Block
  , labNonce         :: !Nonce
    -- | Nonce corresponding to the LAB nonce of the last block of the previous
    -- epoch
  , previousLabNonce :: !Nonce
  }

-- | The node has Praos-aware code that inspects nonces in order to support
-- some Cardano API queries that are crucial to the user exprience
--
-- The interface being used for that has grown and needs review, but we're
-- adding to it here under time pressure. See
-- <https://github.com/input-output-hk/cardano-node/issues/3864>
class ConsensusProtocol p => PraosProtocolSupportsNode p where
  type PraosProtocolSupportsNodeCrypto p

  getPraosNonces :: proxy p -> ChainDepState p -> PraosNonces

  getOpCertCounters :: proxy p -> ChainDepState p -> Map (KeyHash 'BlockIssuer (PraosProtocolSupportsNodeCrypto p)) Word64
