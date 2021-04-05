{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test the Praos chain selection rule (with explicit leader schedule)
module Ouroboros.Consensus.Mock.Ledger.Block.PraosRule (
    PraosCryptoUnused
  , SimplePraosRuleBlock
  , SimplePraosRuleExt (..)
  , SimplePraosRuleHeader
  , forgePraosRuleExt
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Crypto.Hash
import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.Forge
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.Storage.Serialisation

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
  deriving stock    (Generic, Show, Eq)
  deriving newtype  (Condense)
  deriving anyclass (NoThunks)

type instance BlockProtocol (SimplePraosRuleBlock c) =
    WithLeaderSchedule (Praos PraosCryptoUnused)

-- | Sanity check that block and header type synonyms agree
_simplePraosRuleHeader :: SimplePraosRuleBlock c -> SimplePraosRuleHeader c
_simplePraosRuleHeader = simpleHeader

{-------------------------------------------------------------------------------
  Customization of the generic infrastructure
-------------------------------------------------------------------------------}

instance SimpleCrypto c => MockProtocolSpecific c SimplePraosRuleExt where
  type MockLedgerConfig c SimplePraosRuleExt = ()

{-------------------------------------------------------------------------------
  Evidence that 'SimpleBlock' can support Praos with an explicit leader schedule
-------------------------------------------------------------------------------}

instance SimpleCrypto c => RunMockBlock c SimplePraosRuleExt where
  mockNetworkMagic = const constructMockNetworkMagic

instance
  ( SimpleCrypto c
  ) => BlockSupportsProtocol (SimpleBlock c SimplePraosRuleExt) where
  validateView _ _ = ()

instance
  ( SimpleCrypto c
  ) => LedgerSupportsProtocol (SimplePraosRuleBlock c) where
  protocolLedgerView   _ _ = TickedTrivial
  ledgerViewForecastAt _   = trivialForecast

{-------------------------------------------------------------------------------
  We don't need crypto for this protocol
-------------------------------------------------------------------------------}

data PraosCryptoUnused

instance PraosCrypto PraosCryptoUnused where
  type PraosKES  PraosCryptoUnused = NeverKES
  type PraosVRF  PraosCryptoUnused = NeverVRF
  type PraosHash PraosCryptoUnused = NeverHash

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}


type instance CannotForge (SimplePraosRuleBlock c) = Void

type instance ForgeStateInfo (SimplePraosRuleBlock c) = ()

type instance ForgeStateUpdateError (SimplePraosRuleBlock c) = Void

forgePraosRuleExt :: SimpleCrypto c => ForgeExt c SimplePraosRuleExt
forgePraosRuleExt = ForgeExt $ \cfg _ SimpleBlock{..} ->
    let ext = SimplePraosRuleExt $ wlsConfigNodeId (configConsensus cfg)
        SimpleHeader{..} = simpleHeader
    in SimpleBlock {
        simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
      , simpleBody   = simpleBody
      }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise SimplePraosRuleExt

instance EncodeDisk (SimplePraosRuleBlock c) ()
  -- Default instance

instance DecodeDisk (SimplePraosRuleBlock c) ()
  -- Default instance
