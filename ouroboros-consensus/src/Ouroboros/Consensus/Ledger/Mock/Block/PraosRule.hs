{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Test the Praos chain selection rule (with explicit leader schedule)
module Ouroboros.Consensus.Ledger.Mock.Block.PraosRule (
    SimplePraosRuleBlock
  , SimplePraosRuleExt(..)
  , SimplePraosRuleHeader
  , PraosCryptoUnused
  ) where

import           Codec.Serialise (Serialise (..))
import           GHC.Generics (Generic)

import           Cardano.Crypto.Hash
import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Ledger.Mock.Forge
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util.Condense
import qualified Ouroboros.Consensus.Util.SlotBounded as SB

{-------------------------------------------------------------------------------
  Instantiate @ext@
-------------------------------------------------------------------------------}

-- | Simple block extended with the fields required for Praos
--
-- @c@ is crypto used for the block itself
-- With an explicit leader schedule we need no crypto for the consensus protocol.
--
-- This is an example of a block which is /not/ an instance of 'SignedBlock'.
type SimplePraosRuleBlock c = SimpleBlock c SimplePraosRuleExt

-- | Header for Proas
type SimplePraosRuleHeader c = SimpleHeader c SimplePraosRuleExt

-- | Required extension
--
-- The 'WithLeaderSchedule' doesn't require /anything/ in the block header.
-- We add the 'CoreNodeId' just so that we can check that the schedule matches
-- the chain.
newtype SimplePraosRuleExt = SimplePraosRuleExt {
      simplePraosRuleExt :: CoreNodeId
    }
  deriving (Generic, Condense, Show, Eq, NoUnexpectedThunks)

type instance BlockProtocol (SimplePraosRuleBlock c) =
   WithLeaderSchedule (Praos () PraosCryptoUnused)

-- | Sanity check that block and header type synonyms agree
_simplePraosRuleHeader :: SimplePraosRuleBlock c -> SimplePraosRuleHeader c
_simplePraosRuleHeader = simpleHeader

{-------------------------------------------------------------------------------
  Evidence that 'SimpleBlock' can support Praos with an explicit leader schedule
-------------------------------------------------------------------------------}

instance SimpleCrypto c
      => ForgeExt (WithLeaderSchedule p) c SimplePraosRuleExt where
  forgeExt cfg () SimpleBlock{..} = do
      let ext = SimplePraosRuleExt $ lsNodeConfigNodeId cfg
      return SimpleBlock {
          simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
        , simpleBody   = simpleBody
        }
    where
      SimpleHeader{..} = simpleHeader

instance SimpleCrypto c
      => SupportedBlock (SimpleBlock c SimplePraosRuleExt)

instance SimpleCrypto c
      => ProtocolLedgerView (SimplePraosRuleBlock c) where
  protocolLedgerView _ _ = ()
  anachronisticProtocolLedgerView _ _ _ = Right $ SB.unbounded ()

{-------------------------------------------------------------------------------
  We don't need crypto for this protocol
-------------------------------------------------------------------------------}

data PraosCryptoUnused

instance PraosCrypto PraosCryptoUnused where
  type PraosKES  PraosCryptoUnused = NeverKES
  type PraosVRF  PraosCryptoUnused = NeverVRF
  type PraosHash PraosCryptoUnused = NeverHash

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise SimplePraosRuleExt
