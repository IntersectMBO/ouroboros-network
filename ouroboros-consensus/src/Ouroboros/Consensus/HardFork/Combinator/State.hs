{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

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

mostRecentTransitionInfo :: forall xs wt mk.
  ( All SingleEraBlock xs
  , IsSwitchLedgerTables wt
  )
  => HardForkLedgerConfig xs
  -> HardForkState (Flip2 LedgerState wt mk) xs
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

    getTransition :: forall blk. SingleEraBlock        blk
                  => WrapPartialLedgerConfig           blk
                  -> K History.EraParams               blk
                  -> Current (Flip2 LedgerState wt mk) blk
                  -> K TransitionInfo                  blk
    getTransition cfg (K eraParams) Current{..} = K $
      let state = rf (Proxy @(TableStuff (LedgerState blk)))
                     (Proxy @wt)
                  $ forgetLedgerTables $ unFlip2 currentState in
        case singleEraTransition' cfg eraParams currentStart state of
          Nothing -> TransitionUnknown (rf (Proxy @(FlipGetTip LedgerState blk EmptyMK))
                                           (Proxy @wt)
                                         $ ledgerTipSlot state)
          Just e  -> TransitionKnown e

reconstructSummaryLedger :: (All SingleEraBlock xs, IsSwitchLedgerTables wt)
                         => HardForkLedgerConfig xs
                         -> HardForkState (Flip2 LedgerState wt mk) xs
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
epochInfoLedger :: (All SingleEraBlock xs, IsSwitchLedgerTables wt)
                => HardForkLedgerConfig xs
                -> HardForkState (Flip2 LedgerState wt mk) xs
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

-- | Extend the telescope until the specified slot is within the era at the tip
extendToSlot :: forall xs wt.
                ( CanHardFork xs
                , IsSwitchLedgerTables wt
                )
             => HardForkLedgerConfig xs
             -> SlotNo
             -> HardForkState (Flip2 LedgerState wt EmptyMK) xs -> HardForkState (Flip2 LedgerState wt DiffMK) xs
extendToSlot ledgerCfg@HardForkLedgerConfig{..} slot ledgerSt@(HardForkState st) =
      HardForkState    . unI
    . Telescope.extend
        ( InPairs.hcmap proxySingle (\f -> Require $ \(K t)
                                        -> Extend  $ \cur
                                        -> I $ howExtend' f t cur)
        $ translate
        )
        (hczipWith
           proxySingle
           (fn .: whenExtend)
           pcfgs
           (getExactly (History.getShape hardForkLedgerConfigShape)))
    -- In order to make this an automorphism, as required by 'Telescope.extend',
    -- we have to promote the input to @DiffMK@ albeit it being empty.
    $ hcmap
        proxySingle
        convertToDiff
    $ st
  where
    pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
    ei    = epochInfoLedger ledgerCfg ledgerSt

    convertToDiff :: forall blk. SingleEraBlock blk
                  => Current (Flip2 LedgerState wt EmptyMK) blk
                  -> Current (Flip2 LedgerState wt DiffMK) blk
    convertToDiff c = c { currentState = Flip2
                                . rf (Proxy @(TableStuff (LedgerState blk))) (Proxy @wt)
                                  (flip withLedgerTables polyEmptyLedgerTables)
                                . unFlip2
                                . currentState
                                $ c }

    -- Return the end of this era if we should transition to the next
    whenExtend :: SingleEraBlock                    blk
               => WrapPartialLedgerConfig           blk
               -> K History.EraParams               blk
               -> Current (Flip2 LedgerState wt DiffMK) blk
               -> (Maybe :.: K History.Bound)       blk
    whenExtend pcfg (K eraParams) cur = Comp $ K <$> do
        transition <- singleEraTransition'
                        pcfg
                        eraParams
                        (currentStart cur)
                        (unFlip2 $ currentState cur)
        let endBound = History.mkUpperBound
                         eraParams
                         (currentStart cur)
                         transition
        guard (slot >= History.boundSlot endBound)
        return endBound

    howExtend' :: forall blk blk'.
                  ( LedgerSupportsUTxOHD LedgerState blk
                  , LedgerSupportsUTxOHD LedgerState blk'
                  )
              => TranslateLedgerState blk blk'
              -> History.Bound
              -> Current (Flip2 LedgerState wt DiffMK) blk
              -> (K Past blk, Current (Flip2 LedgerState wt DiffMK) blk')
    howExtend' = rf (Proxy @(And (TableStuff (LedgerState blk'))
                                 (TableStuff (LedgerState blk))))
                    (Proxy @wt)
                    howExtend

    howExtend :: ( TableStuff (LedgerState blk) wt
                 , TableStuff (LedgerState blk') wt
                 )
              => TranslateLedgerState blk blk'
              -> History.Bound
              -> Current (Flip2 LedgerState wt DiffMK) blk
              -> (K Past blk, Current (Flip2 LedgerState wt DiffMK) blk')
    howExtend f currentEnd cur = (
          K Past {
              pastStart    = currentStart cur
            , pastEnd      = currentEnd
            }
        , Current {
              currentStart = currentEnd
            , currentState = Flip2
                             -- we need to bring back the diffs provided by
                             -- previous translations. Note that if there is
                             -- only one translation or if the previous
                             -- translations don't add any new tables this will
                             -- just be a no-op. See the haddock for
                             -- 'translateLedgerTablesWith' for more
                             -- information).
                             . prependLedgerTablesDiffsRaw ( translateLedgerTablesWith f
                                             . projectLedgerTables
                                             . unFlip2
                                             . currentState
                                             $ cur
                                             )
                             . translateLedgerStateWith f (History.boundEpoch currentEnd)
                             . forgetLedgerTables
                             . unFlip2
                             . currentState
                             $ cur
            }
        )

    translate :: InPairs TranslateLedgerState xs
    translate = InPairs.requiringBoth cfgs $
                  translateLedgerState hardForkEraTranslation
