{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.State (HardForkState(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
module Ouroboros.Consensus.HardFork.Combinator.State (
    module X
    -- * Support for defining instances
    --  , getTip
    -- * Serialisation support
  , recover
    -- * EpochInfo
  , epochInfoLedger
  , epochInfoPrecomputedTransitionInfo
  , mostRecentTransitionInfo
  , reconstructSummaryLedger
    -- * Ledger specific functionality
  , extendToSlot
  , ConsensusHardForkLedgerState (..)
  ) where

import           Prelude hiding (sequence)

import           Control.Monad (guard)
import           Data.Functor.Product
import           Data.Proxy
import           Data.SOP.Strict hiding (shape)

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract as Abstract
import           Ouroboros.Consensus.Ledger.SupportsUTxOHD
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Counting (getExactly)

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (InPairs,
                     Requiring (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Extend (..), ScanNext (..), Telescope)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

import           Ouroboros.Consensus.HardFork.Combinator.State.Infra as X
import           Ouroboros.Consensus.HardFork.Combinator.State.Instances as X ()
import           Ouroboros.Consensus.HardFork.Combinator.State.Types as X
import Ouroboros.Consensus.Util.SOP
import Ouroboros.Consensus.Util.Singletons
import Type.Reflection

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}
-- type instance HeaderHash (Flip2 LedgerState wt mk blk) = HeaderHash (LedgerState blk wt mk)

-- instance GetTip (Flip2 LedgerState wt mk blk) => GetTip (LedgerState blk wt mk) where
--   getTip = castPoint . Abstract.getTip . Flip2

-- getTip :: forall (wt :: SwitchLedgerTables) (mk :: MapKind) x xs xs'.
--           ( CanHardFork xs
--           , xs ~ (x ': xs')
--           , All (Compose GetTip (Flip2 LedgerState wt mk)) xs
--           )
--        => (forall blk. ( GetTip (LedgerState blk wt mk)
--                        , SingleEraBlock blk
--                        ) => LedgerState blk wt mk -> Point blk
--           )
--        -> HardForkState (Flip2 LedgerState wt mk) xs -> Point (HardForkBlock xs)
-- getTip getLedgerTip = hcollapse
--                       . hcmap proxySingle (K . fun)
--                       . tip
--   where
--     fun :: forall x. ( SingleEraBlock x
--                        , Compose GetTip (Flip2 LedgerState wt mk) x
--                        ) => Flip2 LedgerState wt mk x -> Point (HardForkBlock xs)

--     fun = injPoint . getLedgerTip . unFlip2

--     injPoint :: forall x. SingleEraBlock x
--              => Point x -> Point (HardForkBlock (x ': xs'))
--     injPoint GenesisPoint     = GenesisPoint
--     injPoint (BlockPoint s h) = BlockPoint s $ OneEraHash $
--                                   toShortRawHash (Proxy @x) h

{-------------------------------------------------------------------------------
  Recovery
-------------------------------------------------------------------------------}

-- | Recover 'HardForkState' from partial information
--
-- The primary goal of this is to make sure that for the /current/ state we
-- really only need to store the underlying @f@. It is not strictly essential
-- that this is possible but it helps with the unary hardfork case, and it may
-- in general help with binary compatibility.
recover :: forall f xs. CanHardFork xs
        => Telescope (K Past) f xs -> HardForkState f xs
recover =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty {} ->
          HardForkState
        . Telescope.bihmap
            (\(Pair _ past) -> past)
            recoverCurrent
        . Telescope.scanl
            (InPairs.hpure $ ScanNext $ const $ K . pastEnd . unK)
            (K History.initBound)
  where
    recoverCurrent :: Product (K History.Bound) f blk -> Current f blk
    recoverCurrent (Pair (K prevEnd) st) = Current {
          currentStart = prevEnd
        , currentState = st
        }

{-------------------------------------------------------------------------------
  Reconstruct EpochInfo
-------------------------------------------------------------------------------}

-- class (SingleEraBlock blk, GetTip (LedgerState blk wt mk)) => SEBandGetTip wt mk blk
-- instance (SingleEraBlock blk, GetTip (LedgerState blk wt mk)) => SEBandGetTip wt mk blk

-- pseb :: Proxy wt -> Proxy mk -> Proxy (SEBandGetTip wt mk)
-- pseb _ _ = Proxy

mostRecentTransitionInfo :: forall xs.
     All SingleEraBlock xs
  => HardForkLedgerConfig xs
  -> HardForkState LedgerState xs
  -> TransitionInfo
mostRecentTransitionInfo HardForkLedgerConfig{..} st =
    hcollapse $
      hczipWith3
        proxySingle
        getTransition
        cfgs
        (getExactly (History.getShape hardForkLedgerConfigShape))
        (Telescope.tip (getHardForkState st))
  where
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

    getTransition :: forall blk. SingleEraBlock blk
                  => WrapPartialLedgerConfig    blk
                  -> K History.EraParams        blk
                  -> Current LedgerState        blk
                  -> K TransitionInfo           blk
    getTransition cfg (K eraParams) Current{..} = K $
     case singleEraTransition' cfg eraParams currentStart currentState of
          Nothing -> TransitionUnknown (ledgerTipSlot currentState)
          Just e  -> TransitionKnown e

reconstructSummaryLedger :: All SingleEraBlock xs
                         => HardForkLedgerConfig xs
                         -> HardForkState LedgerState xs
                         -> History.Summary xs
reconstructSummaryLedger cfg@HardForkLedgerConfig{..} st =
    reconstructSummary
      hardForkLedgerConfigShape
      (mostRecentTransitionInfo cfg st)
      st

-- | Construct 'EpochInfo' from the ledger state
--
-- NOTE: The resulting 'EpochInfo' is a snapshot only, with a limited range.
-- It should not be stored.
epochInfoLedger :: All SingleEraBlock xs
                => HardForkLedgerConfig xs
                -> HardForkState LedgerState xs
                -> EpochInfo (Except PastHorizonException)
epochInfoLedger cfg st =
    History.summaryToEpochInfo $
      reconstructSummaryLedger cfg st

-- | Construct 'EpochInfo' given precomputed 'TransitionInfo'
--
-- The transition and state arguments are acquired either from a ticked ledger
-- state or a ticked ledger view.
epochInfoPrecomputedTransitionInfo ::
     History.Shape xs
  -> TransitionInfo
  -> HardForkState f xs
  -> EpochInfo (Except PastHorizonException)
epochInfoPrecomputedTransitionInfo shape transition st =
    History.summaryToEpochInfo $
      reconstructSummary shape transition st

{-------------------------------------------------------------------------------
  Extending
-------------------------------------------------------------------------------}

newtype HardForkConsensusLedgerState wt mk x = HardForkConsensusLedgerState {
    unHardForkConsensusLedgerState :: ConsensusLedgerState (LedgerState x) wt mk
  }

-- | Extend the telescope until the specified slot is within the era at the tip
extendToSlot :: forall xs wt.
                ( CanHardFork xs
                , TableStuff (LedgerTablesGADT (LedgerTables' (LedgerState (HardForkBlock xs))) wt)
                , LedgerTablesCanHardFork xs
                )
             => HardForkLedgerConfig xs
             -> SlotNo
             -> ConsensusLedgerState (LedgerState (HardForkBlock xs)) wt EmptyMK
             -> ConsensusLedgerState (LedgerState (HardForkBlock xs)) wt DiffMK
extendToSlot ledgerCfg@HardForkLedgerConfig{..} slot ledgerSt@(ConsensusLedgerState (HardForkLedgerState hst@(HardForkState st)) _) =
      undefined . unI
    . Telescope.extend
        ( undefined -- $ InPairs.hcimap proxySingle (\idx f -> Require $ \(K t)
          --                        -> Extend  $ \cur
          --                        -> I $ howExtend f idx t cur)
          -- translate
        )
        (hczipWith
           proxySingle
           (fn .: whenExtend
           )
           pcfgs
           (getExactly (History.getShape hardForkLedgerConfigShape)))
    $ (hcmap proxySingle (\(Current x y) -> Current x $ y `ConsensusHardForkLedgerState` polyEmptyLedgerTables)
    $ st :: Telescope (K Past) (Current (ConsensusHardForkLedgerState xs wt DiffMK)) xs)
  where
    pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
    ei    = epochInfoLedger ledgerCfg hst

    -- convertToDiff :: forall blk. SingleEraBlock blk
    --               => Current LedgerState blk
    --               -> Current LedgerState blk
    -- convertToDiff c = c { currentState = Flip2
    --                             . flip withLedgerTables polyEmptyLedgerTables
    --                             . unFlip2
    --                             . currentState
    --                             $ c }

    -- Return the end of this era if we should transition to the next
    whenExtend :: SingleEraBlock              blk
               => WrapPartialLedgerConfig     blk
               -> K History.EraParams         blk
               -> Current (ConsensusHardForkLedgerState xs wt DiffMK) blk
               -> (Maybe :.: K History.Bound) blk
    whenExtend pcfg (K eraParams) cur = Comp $ K <$> do
        transition <- singleEraTransition'
                        pcfg
                        eraParams
                        (currentStart cur)
                        (chfls $ currentState cur)
        let endBound = History.mkUpperBound
                         eraParams
                         (currentStart cur)
                         transition
        guard (slot >= History.boundSlot endBound)
        return endBound

    howExtend :: ( SingI wt
                 , Typeable wt
                 , StowableLedgerTables (LedgerTablesGADT (LedgerTables' (LedgerState blk')) wt)
                 , TableStuff (LedgerTablesGADT (LedgerTables' (LedgerState blk')) wt)
                 , TableStuff (LedgerTablesGADT (LedgerTables' (LedgerState blk)) wt)
                 )
              => TranslateLedgerState blk blk'
              -> Index xs blk'
              -> History.Bound
              -> Current (ConsensusHardForkLedgerState xs wt DiffMK) blk
              -> (K Past blk, Current (ConsensusHardForkLedgerState xs wt DiffMK) blk')
    howExtend f idx currentEnd cur@(Current start (ConsensusHardForkLedgerState state tables)) = (
          K Past {
              pastStart    = start
            , pastEnd      = currentEnd
            }
        , Current {
              currentStart = currentEnd
            , currentState =
              let ConsensusLedgerState state' tables' = translateLedgerStateWith f (History.boundEpoch currentEnd) $ ConsensusLedgerState state polyEmptyLedgerTables in
              ConsensusHardForkLedgerState state' $ zipLedgerTables rawPrependDiffs tables $ fmapTables (applyInjectLedgerTables (projectNP idx hardForkInjectLedgerTablesKeysMK)) tables'
            }
        )

    translate :: InPairs TranslateLedgerState xs
    translate = InPairs.requiringBoth cfgs $
                  translateLedgerState hardForkEraTranslation

-- {--------------------------------------------------------------------------------

-- --------------------------------------------------------------------------------}
