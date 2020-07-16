{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Protocol (
    -- * Re-exports to keep 'Protocol.State' an internal module
    HardForkChainDepState
  , HardForkIsLeader
  , HardForkCanBeLeader
  , HardForkValidationErr(..)
    -- * Re-exports to keep 'Protocol.LedgerView' an internal module
  , HardForkLedgerView_(..)
  , HardForkLedgerView
    -- * Type family instances
  , Ticked(..)
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
import           Data.Functor.Product
import           Data.SOP.Strict
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Abstract
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
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
                     (HardForkLedgerView, HardForkLedgerView_ (..),
                     Ticked (..))
import           Ouroboros.Consensus.HardFork.Combinator.State (HardForkState,
                     HardForkState_, Translate (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

{-------------------------------------------------------------------------------
  ChainSelection
-------------------------------------------------------------------------------}

type HardForkSelectView xs = WithBlockNo OneEraSelectView xs

mkHardForkSelectView ::
     BlockNo
  -> NS WrapSelectView xs
  -> HardForkSelectView xs
mkHardForkSelectView bno view = WithBlockNo bno (OneEraSelectView view)

-- | Chain selection across eras
instance CanHardFork xs => ChainSelection (HardForkProtocol xs) where
  type ChainSelConfig (HardForkProtocol xs) = PerEraChainSelConfig xs
  type SelectView     (HardForkProtocol xs) = HardForkSelectView   xs

  -- We leave 'preferCandidate' at the default

  compareCandidates _ (PerEraChainSelConfig cfgs) l r =
       acrossEraSelection
         cfgs
         hardForkChainSel
         (mapWithBlockNo getOneEraSelectView l)
         (mapWithBlockNo getOneEraSelectView r)

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

type HardForkChainDepState xs = HardForkState WrapChainDepState xs

instance CanHardFork xs => ConsensusProtocol (HardForkProtocol xs) where
  type ChainDepState (HardForkProtocol xs) = HardForkChainDepState xs
  type ValidationErr (HardForkProtocol xs) = HardForkValidationErr xs
  type LedgerView    (HardForkProtocol xs) = HardForkLedgerView    xs
  type CanBeLeader   (HardForkProtocol xs) = HardForkCanBeLeader   xs
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
        mkHardForkSelectView (blockNo hdr)
      . hczipWith proxySingle (WrapSelectView .: selectView) cfgs
      . getOneEraHeader
      $ getHardForkHeader hdr
    where
      cfgs = getPerEraBlockConfig hardForkBlockConfigPerEra

{-------------------------------------------------------------------------------
  Ticking the chain dependent state
-------------------------------------------------------------------------------}

data instance Ticked (HardForkChainDepState xs) =
    TickedHardForkChainDepState {
        tickedHardForkChainDepStatePerEra ::
             HardForkState_ WrapChainDepState (Ticked :.: WrapChainDepState) xs

        -- 'EpochInfo' constructed from the ticked 'LedgerView'
      , tickedHardForkChainDepStateEpochInfo :: EpochInfo Identity
      }

tick :: CanHardFork xs
     => ConsensusConfig (HardForkProtocol xs)
     -> Ticked (HardForkLedgerView xs)
     -> SlotNo
     -> HardForkChainDepState xs
     -> Ticked (HardForkChainDepState xs)
tick cfg@HardForkConsensusConfig{..}
     (TickedHardForkLedgerView transition ledgerView)
     slot
     chainDepState = TickedHardForkChainDepState {
      tickedHardForkChainDepStateEpochInfo = ei
    , tickedHardForkChainDepStatePerEra =
         State.align
           (translateConsensus ei cfg)
           (hcmap proxySingle (fn_2 . tickOne) cfgs)
           ledgerView
           chainDepState
    }
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    ei   = State.epochInfoPrecomputedTransitionInfo
             hardForkConsensusConfigShape
             transition
             ledgerView

    tickOne :: SingleEraBlock                 blk
            => WrapPartialConsensusConfig     blk
            -> (Ticked :.: WrapLedgerView)    blk
            -> WrapChainDepState              blk
            -> (Ticked :.: WrapChainDepState) blk
    tickOne cfg' (Comp ledgerView') chainDepState' = Comp $
        WrapTickedChainDepState $
          tickChainDepState
            (completeConsensusConfig' ei cfg')
            (unwrapTickedLedgerView ledgerView')
            slot
            (unwrapChainDepState chainDepState')

{-------------------------------------------------------------------------------
  Leader check

  NOTE: The precondition to `align` is satisfied: the consensus state will never
  be ahead (but possibly behind) the ledger state, which we tick first.
-------------------------------------------------------------------------------}

-- | We are a leader if we have a proof from one of the eras
type HardForkIsLeader xs = OneEraIsLeader xs

-- | CanBeLeader instance for 'HardForkProtocol'
--
-- We allow an optional 'CanBeLeader' proof /per era/, to make it possible to
-- configure a node to, say, be able to produce Shelley but not Byron blocks.
-- However, we do not allow /all/ eras to be empty (since, in that case, we
-- /cannot/ be a leader).
type HardForkCanBeLeader xs = OptNP 'False WrapCanBeLeader xs

check :: forall xs. (CanHardFork xs, HasCallStack)
      => ConsensusConfig (HardForkProtocol xs)
      -> HardForkCanBeLeader xs
      -> SlotNo
      -> Ticked (ChainDepState (HardForkProtocol xs))
      -> Maybe (HardForkIsLeader xs)
check HardForkConsensusConfig{..}
      canBeLeader
      slot
      (TickedHardForkChainDepState chainDepState ei) =
    distrib $
        hczipWith3
          proxySingle
          checkOne
          cfgs
          (fromOptNP canBeLeader)
          (State.tip chainDepState)
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    checkOne :: SingleEraBlock                 blk
             => WrapPartialConsensusConfig     blk
             -> (Maybe :.: WrapCanBeLeader)    blk
             -> (Ticked :.: WrapChainDepState) blk
             -> (Maybe :.: WrapIsLeader)       blk
    checkOne cfg'
             (Comp mCanBeLeader)
             (Comp chainDepState') = Comp $
        case mCanBeLeader of
          Nothing           -> Nothing
          Just canBeLeader' -> WrapIsLeader <$>
            checkIsLeader
              (completeConsensusConfig' ei cfg')
              (unwrapCanBeLeader canBeLeader')
              slot
              (unwrapTickedChainDepState chainDepState')

    distrib :: NS (Maybe :.: WrapIsLeader) xs
            -> Maybe (HardForkIsLeader xs)
    distrib = hcollapse . hzipWith inj injections
      where
        inj :: Injection WrapIsLeader xs blk
            -> (Maybe :.: WrapIsLeader) blk
            -> K (Maybe (HardForkIsLeader xs)) blk
        inj injIsLeader (Comp mIsLeader) = K $
            OneEraIsLeader . unK . apFn injIsLeader <$> mIsLeader

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
       -> SlotNo
       -> Ticked (HardForkChainDepState xs)
       -> Except (HardForkValidationErr xs) (HardForkChainDepState xs)
update HardForkConsensusConfig{..}
       (OneEraValidateView view)
       slot
       (TickedHardForkChainDepState chainDepState ei) =
    case State.match view chainDepState of
      Left mismatch ->
        throwError $ HardForkValidationErrWrongEra . MismatchEraInfo $
          Match.bihcmap
            proxySingle
            singleEraInfo
            (LedgerEraInfo . chainDepStateInfo . State.currentState)
            mismatch
      Right matched ->
           hsequence'
         . State.tickAllPast hardForkConsensusConfigK
         . hczipWith3 proxySingle (updateEra ei slot) cfgs errInjections
         $ matched
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    errInjections :: NP (Injection WrapValidationErr xs) xs
    errInjections = injections

updateEra :: forall xs blk. SingleEraBlock blk
          => EpochInfo Identity
          -> SlotNo
          -> WrapPartialConsensusConfig blk
          -> Injection WrapValidationErr xs blk
          -> Product WrapValidateView (Ticked :.: WrapChainDepState) blk
          -> (Except (HardForkValidationErr xs) :.: WrapChainDepState) blk
updateEra ei slot cfg injectErr
          (Pair view (Comp chainDepState)) = Comp $
    withExcept (injectValidationErr injectErr) $
      fmap WrapChainDepState $
        updateChainDepState
          (completeConsensusConfig' ei cfg)
          (unwrapValidateView view)
          slot
          (unwrapTickedChainDepState chainDepState)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

chainDepStateInfo :: forall blk. SingleEraBlock blk
                  => (Ticked :.: WrapChainDepState) blk -> SingleEraInfo blk
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
