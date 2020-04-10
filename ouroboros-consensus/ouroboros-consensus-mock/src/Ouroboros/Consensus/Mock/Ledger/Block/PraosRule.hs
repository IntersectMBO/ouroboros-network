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
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Util.Condense

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
  deriving anyclass (NoUnexpectedThunks)

type instance NodeState     (SimplePraosRuleBlock c) = ()
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
  forgeExt cfg _updateState () SimpleBlock{..} = do
      let ext = SimplePraosRuleExt $ wlsConfigNodeId (configConsensus cfg)
      return SimpleBlock {
          simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
        , simpleBody   = simpleBody
        }
    where
      SimpleHeader{..} = simpleHeader

  mockProtocolMagicId      = const constructMockProtocolMagicId
  mockEncodeConsensusState = const encode
  mockDecodeConsensusState = const decode

instance SimpleCrypto c
      => BlockSupportsProtocol (SimpleBlock c SimplePraosRuleExt) where
  validateView _ _ = ()

instance SimpleCrypto c => LedgerSupportsProtocol (SimplePraosRuleBlock c) where
  protocolLedgerView _ _ = ()
  ledgerViewForecastAt_ _ _ = Just . trivialForecast

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
