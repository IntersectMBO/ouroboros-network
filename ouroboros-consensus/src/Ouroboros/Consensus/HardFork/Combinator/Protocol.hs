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

{-# OPTIONS_GHC -Wno-orphans -fno-show-valid-hole-fits #-}

module Ouroboros.Consensus.HardFork.Combinator.Protocol (
    -- * Re-exports to keep 'Protocol.State' an internal module
    HardForkChainDepState
  , HardForkIsLeader
  , HardForkCanBeLeader
  , HardForkValidationErr(..)
    -- * Re-exports to keep 'Protocol.LedgerView' an internal module
  , HardForkLedgerView_(..)
  , HardForkLedgerView
  ) where

import           Codec.Serialise (Serialise)
import           Control.Exception (assert)
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Functor.Product
import           Data.SOP.Strict
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
                     (HardForkSelectView (..))
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
                     (HardForkLedgerView, HardForkLedgerView_ (..))
import           Ouroboros.Consensus.HardFork.Combinator.State (HardForkState,
                     Translate (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

type HardForkChainDepState xs = HardForkState WrapChainDepState xs

instance CanHardFork xs => ConsensusProtocol (HardForkProtocol xs) where
  type ChainDepState (HardForkProtocol xs) = HardForkChainDepState xs
  type ValidationErr (HardForkProtocol xs) = HardForkValidationErr xs
  type LedgerView    (HardForkProtocol xs) = HardForkLedgerView    xs
  type CanBeLeader   (HardForkProtocol xs) = HardForkCanBeLeader   xs
  type CannotLead    (HardForkProtocol xs) = HardForkCannotLead    xs
  type IsLeader      (HardForkProtocol xs) = HardForkIsLeader      xs
  type ValidateView  (HardForkProtocol xs) = OneEraValidateView    xs

  -- Operations on the state

  tickChainDepState     = tick
  checkIsLeader         = check
  updateChainDepState   = update
  rewindChainDepState _ = rewind

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
  Ticking the chain dependent state
-------------------------------------------------------------------------------}

tick :: CanHardFork xs
     => ConsensusConfig (HardForkProtocol xs)
     -> Ticked (HardForkLedgerView xs)
     -> HardForkChainDepState xs
     -> Ticked (HardForkChainDepState xs)
tick cfg@HardForkConsensusConfig{..} (Ticked slot ledgerView) =
      Ticked slot
    . State.align
        (translateConsensus ei cfg)
        (hcmap proxySingle (fn_2 . tickOne) cfgs)
        (hardForkLedgerViewPerEra ledgerView)
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    ei   = State.epochInfoLedgerView hardForkConsensusConfigShape ledgerView

    tickOne :: SingleEraBlock             blk
            => WrapPartialConsensusConfig blk
            -> WrapLedgerView             blk
            -> WrapChainDepState          blk
            -> WrapChainDepState          blk
    tickOne cfg' ledgerView' chainDepState' =
        WrapChainDepState . tickedState $
          tickChainDepState
            (completeConsensusConfig' ei cfg')
            (Ticked slot (unwrapLedgerView ledgerView'))
            (unwrapChainDepState chainDepState')

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

check :: forall m xs. (MonadRandom m, CanHardFork xs, HasCallStack)
      => ConsensusConfig (HardForkProtocol xs)
      -> HardForkCanBeLeader xs
      -> ChainIndepState (HardForkProtocol xs)
      -> Ticked (HardForkLedgerView xs)
      -> Ticked (ChainDepState (HardForkProtocol xs))
      -> m (LeaderCheck (HardForkProtocol xs))
check HardForkConsensusConfig{..}
      canBeLeader
      chainIndepState
      (Ticked slot ledgerView)
      (Ticked slot' chainDepState) = assert (slot == slot') $
    case Match.matchNS
           (State.tip chainDepState)
           (State.tip (hardForkLedgerViewPerEra ledgerView)) of
      Left mismatch ->
        -- This shouldn't happen: 'checkIsLeader' aligned the two telescopes
        let mismatch' :: MismatchEraInfo xs
            mismatch' = MismatchEraInfo $
                          Match.bihcmap
                            proxySingle
                            chainDepStateInfo
                            ledgerInfo'
                            mismatch
        in error $ "check: unexpected mismatch: " ++ show mismatch'
      Right aligned ->
        (fmap distrib . hsequence') $
            hcpure proxySingle (fn_4 checkOne)
          `hap`
            cfgs
          `hap`
            (fromOptNP canBeLeader)
          `hap`
            (getPerEraChainIndepState chainIndepState)
          `hap`
            aligned
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    ei   = State.epochInfoLedgerView hardForkConsensusConfigShape ledgerView

    checkOne :: SingleEraBlock                           blk
             => WrapPartialConsensusConfig               blk
             -> (Maybe :.: WrapCanBeLeader)              blk
             -> WrapChainIndepState                      blk
             -> Product WrapChainDepState WrapLedgerView blk
             -> (m :.: WrapLeaderCheck)                  blk
    checkOne cfg'
             (Comp mCanBeLeader)
             chainIndepState'
             (Pair chainDepState' ledgerView') = Comp . fmap WrapLeaderCheck $
         case mCanBeLeader of
           Nothing ->
             return NotLeader
           Just canBeLeader' ->
             checkIsLeader
               (completeConsensusConfig' ei cfg')
               (unwrapCanBeLeader canBeLeader')
               (unwrapChainIndepState chainIndepState')
               (Ticked slot (unwrapLedgerView ledgerView'))
               (Ticked slot (unwrapChainDepState chainDepState'))

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
       -> HardForkChainDepState xs
       -> Maybe (HardForkChainDepState xs)
rewind k p =
        -- Using just the 'SlotNo' is okay: no EBBs near transition
        State.retractToSlot (pointSlot p)
    >=> (hsequence' . hcmap proxySingle rewindOne)
  where
    rewindOne :: forall blk. SingleEraBlock     blk
              => WrapChainDepState              blk
              -> (Maybe :.: WrapChainDepState) blk
    rewindOne (WrapChainDepState st) = Comp $
        WrapChainDepState <$>
          rewindChainDepState (Proxy @(BlockProtocol blk)) k p st

update :: forall xs. CanHardFork xs
       => ConsensusConfig (HardForkProtocol xs)
       -> OneEraValidateView xs
       -> Ticked (HardForkLedgerView xs)
       -> Ticked (HardForkChainDepState xs)
       -> Except (HardForkValidationErr xs) (HardForkChainDepState xs)
update cfg@HardForkConsensusConfig{..}
       (OneEraValidateView view)
       (Ticked slot ledgerView)
       (Ticked _slot chainDepState) =
    case State.match view (hardForkLedgerViewPerEra ledgerView) of
      Left mismatch ->
        throwError $ HardForkValidationErrWrongEra . MismatchEraInfo $
          Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
      Right matched ->
           hsequence'
         . State.tickAllPast hardForkConsensusConfigK
         . State.align
            (translateConsensus ei cfg)
            (hczipWith proxySingle (fn_2 .: updateEra ei slot) cfgs errInjections)
            matched
         $ chainDepState
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    ei   = State.epochInfoLedgerView hardForkConsensusConfigShape ledgerView

    errInjections :: NP (Injection WrapValidationErr xs) xs
    errInjections = injections

updateEra :: forall xs blk. SingleEraBlock blk
          => EpochInfo Identity
          -> SlotNo
          -> WrapPartialConsensusConfig blk
          -> Injection WrapValidationErr xs blk
          -> Product WrapValidateView WrapLedgerView blk
          -> WrapChainDepState blk
          -> (Except (HardForkValidationErr xs) :.: WrapChainDepState) blk
updateEra ei slot cfg injectErr
          (Pair (WrapValidateView view) ledgerView)
          (WrapChainDepState chainDepState) = Comp $
    withExcept (injectValidationErr injectErr) $
      fmap WrapChainDepState $
        updateChainDepState
          (completeConsensusConfig' ei cfg)
          view
          (Ticked slot (unwrapLedgerView ledgerView))
          (Ticked slot chainDepState)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall f blk. SingleEraBlock blk
           => State.Current f blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

ledgerInfo' :: forall blk. SingleEraBlock blk
            => WrapLedgerView blk -> LedgerEraInfo blk
ledgerInfo' _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

chainDepStateInfo :: forall blk. SingleEraBlock blk
                  => WrapChainDepState blk -> SingleEraInfo blk
chainDepStateInfo _ = singleEraInfo (Proxy @blk)

translateConsensus :: forall xs. CanHardFork xs
                   => EpochInfo Identity
                   -> ConsensusConfig (HardForkProtocol xs)
                   -> InPairs (Translate WrapChainDepState) xs
translateConsensus ei HardForkConsensusConfig{..} =
    InPairs.requiringBoth cfgs $
       translateChainDepState hardForkEraTranslation
  where
    pcfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    cfgs  = hcmap proxySingle (completeConsensusConfig'' ei) pcfgs

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
