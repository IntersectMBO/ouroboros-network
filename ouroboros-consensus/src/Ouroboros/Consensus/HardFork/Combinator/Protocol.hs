{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Protocol (
    -- * Re-exports to keep 'Protocol.State' an internal module
    HardForkConsensusState
  , HardForkValidationErr(..)
    -- * Re-exports to keep 'Protocol.LedgerView' an internal module
  , HardForkLedgerView
  , HardForkEraLedgerView(..)
  , mkHardForkEraLedgerView
  ) where

import           Data.SOP.Strict

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
                     (HardForkSelectView (..))
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
                     (HardForkEraLedgerView (..), HardForkLedgerView,
                     mkHardForkEraLedgerView)
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.State
                     (HardForkConsensusState, HardForkValidationErr)
import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.State as ProtocolState
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

instance CanHardFork xs => ConsensusProtocol (HardForkProtocol xs) where
  type ConsensusState (HardForkProtocol xs) = HardForkConsensusState xs
  type ValidationErr  (HardForkProtocol xs) = HardForkValidationErr  xs
  type LedgerView     (HardForkProtocol xs) = HardForkLedgerView     xs
  type IsLeader       (HardForkProtocol xs) = OneEraIsLeader         xs
  type ValidateView   (HardForkProtocol xs) = OneEraValidateView     xs

  -- Operations on the state

  checkIsLeader          = ProtocolState.check
  updateConsensusState   = ProtocolState.update
  rewindConsensusState _ = ProtocolState.rewind

  --
  -- Straight-forward extensions
  --

  -- We can be a leader if we can be a leader in /any/ era
  checkIfCanBeLeader =
        or
      . hcollapse
      . hcmap proxySingle (K . aux)
      . getPerEraConsensusConfig
      . hardForkConsensusConfigPerEra
    where
      aux :: forall blk. SingleEraBlock blk
          => WrapPartialConsensusConfig blk -> Bool
      aux = partialCheckIfCanBeLeader (Proxy @(BlockProtocol blk))
          . unwrapPartialConsensusConfig

  -- Security parameter must be equal across /all/ eras
  protocolSecurityParam = hardForkConsensusConfigK

  -- Extract 'ChainSelConfig'
  chainSelConfig =
        PerEraChainSelConfig
      . hcmap proxySingle aux
      . getPerEraConsensusConfig
      . hardForkConsensusConfigPerEra
    where
      aux :: forall blk. SingleEraBlock blk
          => WrapPartialConsensusConfig blk
          -> WrapChainSelConfig blk
      aux = WrapChainSelConfig
          . partialChainSelConfig (Proxy @(BlockProtocol blk))
          . unwrapPartialConsensusConfig

{-------------------------------------------------------------------------------
  BlockSupportsProtocol
-------------------------------------------------------------------------------}

instance CanHardFork xs => BlockSupportsProtocol (HardForkBlock xs) where
  validateView HardForkBlockConfig{..} =
        OneEraValidateView
      . hczipWith proxySingle (WrapValidateView .: validateView) cfgs
      . getOneEraHeader
      . getHardForkHeader
    where
      cfgs = getPerEraBlockConfig hardForkBlockConfigPerEra

  selectView HardForkBlockConfig{..} hdr =
        HardForkSelectView (blockNo hdr)
      . OneEraSelectView
      . hczipWith proxySingle (WrapSelectView .: selectView) cfgs
      . getOneEraHeader
      $ getHardForkHeader hdr
    where
      cfgs = getPerEraBlockConfig hardForkBlockConfigPerEra
