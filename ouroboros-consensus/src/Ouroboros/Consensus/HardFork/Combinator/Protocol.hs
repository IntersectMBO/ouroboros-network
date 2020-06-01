{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Protocol (
    -- * Re-exports to keep 'Protocol.State' an internal module
    HardForkConsensusState
  , HardForkIsLeader
  , HardForkCanBeLeader
  , HardForkValidationErr(..)
    -- * Re-exports to keep 'Protocol.LedgerView' an internal module
  , HardForkLedgerView
  , HardForkEraLedgerView(..)
  , mkHardForkEraLedgerView
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Functor.Product
import           Data.SOP.Strict
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
                     (HardForkSelectView (..))
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
                     (HardForkEraLedgerView (..), HardForkLedgerView,
                     mkHardForkEraLedgerView)
import           Ouroboros.Consensus.HardFork.Combinator.State (HardForkState,
                     Translate (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..), RequiringBoth (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

type HardForkConsensusState xs = HardForkState WrapConsensusState xs

instance CanHardFork xs => ConsensusProtocol (HardForkProtocol xs) where
  type ConsensusState (HardForkProtocol xs) = HardForkConsensusState xs
  type ValidationErr  (HardForkProtocol xs) = HardForkValidationErr  xs
  type LedgerView     (HardForkProtocol xs) = HardForkLedgerView     xs
  type CanBeLeader    (HardForkProtocol xs) = HardForkCanBeLeader    xs
  type CannotLead     (HardForkProtocol xs) = HardForkCannotLead     xs
  type IsLeader       (HardForkProtocol xs) = HardForkIsLeader       xs
  type ValidateView   (HardForkProtocol xs) = OneEraValidateView     xs

  -- Operations on the state

  checkIsLeader          = check
  updateConsensusState   = update
  rewindConsensusState _ = rewind

  --
  -- Straight-forward extensions
  --

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

{-------------------------------------------------------------------------------
  Leader check

  NOTE: The precondition to `align` is satisfied: the consensus state will never
  be ahead (but possibly behind) the ledger state, which we tick first.
-------------------------------------------------------------------------------}

-- | We are a leader if we have a proof from one of the eras
type HardForkIsLeader xs = OneEraIsLeader xs

-- | If we fail to lead, it's because one of the eras tried but failed
type HardForkCannotLead xs = OneEraCannotLead xs

-- | CanBeLeader instance for 'HardForkProtocol'
--
-- We allow an optional 'CanBeLeader' proof /per era/, to make it possible to
-- configure a node to, say, be able to produce Shelley but not Byron blocks.
-- However, we do not allow /all/ eras to be empty (since, in that case, we
-- /cannot/ be a leader).
type HardForkCanBeLeader xs = OptNP 'False WrapCanBeLeader xs

check :: forall m xs. (MonadRandom m, CanHardFork xs)
      => ConsensusConfig (HardForkProtocol xs)
      -> HardForkCanBeLeader xs
      -> Ticked (HardForkLedgerView xs)
      -> HardForkConsensusState xs
      -> m (LeaderCheck (HardForkProtocol xs))
check cfg@HardForkConsensusConfig{..} canBeLeader (Ticked slot ledgerView) =
      fmap distrib
    . hsequence'
    . State.tip
    . State.align
        (translateConsensus ei cfg)
        (hczipWith
           proxySingle
           (fn_2 .: checkOne ei slot)
           cfgs
           (fromOptNP canBeLeader))
        ledgerView
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    ei   = State.epochInfoLedgerView hardForkConsensusConfigShape ledgerView

    distrib :: NS WrapLeaderCheck xs -> LeaderCheck (HardForkProtocol xs)
    distrib = hcollapse . hzipWith3 inj injections injections

    inj :: Injection WrapIsLeader   xs blk
        -> Injection WrapCannotLead xs blk
        -> WrapLeaderCheck blk
        -> K (LeaderCheck (HardForkProtocol xs)) blk
    inj injIsLeader injCannotLead (WrapLeaderCheck leaderCheck) = K $
        case leaderCheck of
          NotLeader    -> NotLeader
          IsLeader   p -> IsLeader $ OneEraIsLeader . unK $
                            apFn injIsLeader (WrapIsLeader p)
          CannotLead e -> CannotLead $ OneEraCannotLead . unK $
                            apFn injCannotLead (WrapCannotLead e)

checkOne :: (MonadRandom m, SingleEraBlock blk)
         => EpochInfo Identity
         -> SlotNo
         -> WrapPartialConsensusConfig  blk
         -> (Maybe :.: WrapCanBeLeader) blk
         -> HardForkEraLedgerView       blk
         -> WrapConsensusState          blk
         -> (m :.: WrapLeaderCheck)     blk
checkOne ei slot cfg (Comp mCanBeLeader)
         HardForkEraLedgerView{..}
         (WrapConsensusState consensusState) = Comp $ WrapLeaderCheck <$>
     case mCanBeLeader of
       Nothing ->
         return NotLeader
       Just canBeLeader ->
         checkIsLeader
           (completeConsensusConfig' ei cfg)
           (unwrapCanBeLeader canBeLeader)
           (Ticked slot hardForkEraLedgerView)
           consensusState

{-------------------------------------------------------------------------------
  Rolling forward and backward
-------------------------------------------------------------------------------}

data HardForkValidationErr xs =
    -- | Validation error from one of the eras
    HardForkValidationErrFromEra (OneEraValidationErr xs)

    -- | We tried to apply a block from the wrong era
  | HardForkValidationErrWrongEra (MismatchEraInfo xs)
  deriving (Generic)

rewind :: (CanHardFork xs, Serialise (HeaderHash hdr))
       => SecurityParam
       -> Point hdr
       -> HardForkConsensusState xs
       -> Maybe (HardForkConsensusState xs)
rewind k p =
        -- Using just the 'SlotNo' is okay: no EBBs near transition
        State.retractToSlot (pointSlot p)
    >=> (hsequence' . hcmap proxySingle rewindOne)
  where
    rewindOne :: forall blk. SingleEraBlock     blk
              => WrapConsensusState             blk
              -> (Maybe :.: WrapConsensusState) blk
    rewindOne (WrapConsensusState st) = Comp $
        WrapConsensusState <$>
          rewindConsensusState (Proxy @(BlockProtocol blk)) k p st

update :: forall xs. CanHardFork xs
       => ConsensusConfig (HardForkProtocol xs)
       -> Ticked (HardForkLedgerView xs)
       -> OneEraValidateView xs
       -> HardForkConsensusState xs
       -> Except (HardForkValidationErr xs) (HardForkConsensusState xs)
update cfg@HardForkConsensusConfig{..}
       (Ticked slot ledgerView)
       (OneEraValidateView view)
       consensusState =
    case State.match view ledgerView of
      Left mismatch ->
        throwError $ HardForkValidationErrWrongEra . MismatchEraInfo $
          Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
      Right matched ->
           hsequence'
         . State.tickAllPast hardForkConsensusConfigK
         . State.align
            (translateConsensus ei cfg)
            (hczipWith proxySingle (fn_2 .: updateEra ei slot) cfgs injections)
            matched
         $ consensusState
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    ei   = State.epochInfoLedgerView hardForkConsensusConfigShape ledgerView

updateEra :: forall xs blk. SingleEraBlock blk
          => EpochInfo Identity
          -> SlotNo
          -> WrapPartialConsensusConfig                                 blk
          -> Injection WrapValidationErr xs                             blk
          -> Product WrapValidateView HardForkEraLedgerView             blk
          -> WrapConsensusState                                         blk
          -> (Except (HardForkValidationErr xs) :.: WrapConsensusState) blk
updateEra ei slot cfg injectErr
          (Pair (WrapValidateView view) HardForkEraLedgerView{..})
          (WrapConsensusState consensusState) = Comp $
    withExcept (injectValidationErr injectErr) $
      fmap WrapConsensusState $
        updateConsensusState
          (completeConsensusConfig' ei cfg)
          (Ticked slot hardForkEraLedgerView)
          view
          consensusState

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => State.Current HardForkEraLedgerView blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

translateConsensus :: CanHardFork xs
                   => EpochInfo Identity
                   -> ConsensusConfig (HardForkProtocol xs)
                   -> InPairs (Translate WrapConsensusState) xs
translateConsensus ei HardForkConsensusConfig{..} =
    InPairs.requiringBoth
      (getPerEraConsensusConfig hardForkConsensusConfigPerEra)
      (InPairs.hcmap
        proxySingle
        (RequireBoth . aux)
        (translateConsensusState hardForkEraTranslation))
  where
    aux :: forall blk blk'. (SingleEraBlock blk, SingleEraBlock blk')
        => TranslateEraConsensusState blk blk'
        -> WrapPartialConsensusConfig blk
        -> WrapPartialConsensusConfig blk'
        -> Translate WrapConsensusState blk blk'
    aux f pcfg pcfg' = Translate $ \epoch (WrapConsensusState st) ->
        WrapConsensusState $
          translateConsensusStateWith f cfg cfg' epoch st
      where
        cfg  :: ConsensusConfig (BlockProtocol blk)
        cfg' :: ConsensusConfig (BlockProtocol blk')
        cfg  = completeConsensusConfig' ei pcfg
        cfg' = completeConsensusConfig' ei pcfg'

injectValidationErr :: Injection WrapValidationErr xs blk
                    -> ValidationErr (BlockProtocol blk)
                    -> HardForkValidationErr xs
injectValidationErr inj =
      HardForkValidationErrFromEra
    . OneEraValidationErr
    . unK
    . apFn inj
    . WrapValidationErr

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving instance CanHardFork xs => Eq                 (HardForkValidationErr xs)
deriving instance CanHardFork xs => Show               (HardForkValidationErr xs)
deriving instance CanHardFork xs => NoUnexpectedThunks (HardForkValidationErr xs)
