{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Test.Consensus.HardFork.Forecast (
    tests
    -- Quell ghc warning
  , LedgerView (..)
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Data.Either (isRight)
import           Data.Foldable (toList)
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.SOP.Strict
import           Data.Word
import           GHC.Stack
import           Ouroboros.Consensus.Util.Counting

import           Test.QuickCheck hiding (elements)
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (elements)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
                     (AnnForecast (..), mkHardForkForecast)
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope (..))
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraEnd (..),
                     EraParams (..), EraSummary (..), Summary (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HardFork.History.Util
import           Ouroboros.Consensus.Util (Some (..), repeatedly, splits)
import           Ouroboros.Consensus.Util.SOP

import           Test.Consensus.HardFork.Infra
import           Test.Util.QuickCheck

tests :: TestTree
tests = testGroup "Forecast" [
      testGroup "Sanity" [
          testProperty "generator" $ checkGenerator prop_validTestSetup
        , testProperty "shrinker"  $ checkShrinker  prop_validTestSetup
        ]
    , testProperty "forecast" $ prop_forecast
    ]

{-------------------------------------------------------------------------------
  Mock chain and ledger
-------------------------------------------------------------------------------}

newtype Chain era = Chain { getBlocks :: [Block] }
  deriving (Show)

data Block = Block SlotNo Scheduled
  deriving (Show)

type Scheduled = Map SlotNo LedgerUpdate

newtype LedgerUpdate = IncreaseValueBy Word64
  deriving (Show, Eq, Num)

{-------------------------------------------------------------------------------
  Ledger state
-------------------------------------------------------------------------------}

type LedgerValue = Word64

data LedgerState = LedgerState {
      ledgerValue     :: LedgerValue
    , ledgerScheduled :: Scheduled
    , ledgerTip       :: WithOrigin SlotNo
    }
  deriving (Show)

data instance Ticked LedgerState = TickedLedgerState {
      tickedValue     :: LedgerValue
    , tickedScheduled :: Scheduled
    }
  deriving (Show)

newtype LedgerView = LedgerView LedgerValue

newtype instance Ticked LedgerView = TickedView LedgerValue

initLedgerState :: LedgerState
initLedgerState = LedgerState {
      ledgerValue     = 0
    , ledgerScheduled = Map.empty
    , ledgerTip       = Origin
    }

tickLedgerState :: SlotNo -> LedgerState -> Ticked LedgerState
tickLedgerState sno = advanceTo sno . tickToTip

-- | "Tick to tip" just translates @LedgerState@ to @Ticked LedgerState@
-- without actually changing anything: after all, the ledger already /is/
-- at its tip.
tickToTip :: LedgerState -> Ticked LedgerState
tickToTip LedgerState{..} = TickedLedgerState {
      tickedScheduled = ledgerScheduled
    , tickedValue     = ledgerValue
    }

-- | Advance the ticked ledger state to the given 'SlotNo'
advanceTo :: SlotNo -> Ticked LedgerState -> Ticked LedgerState
advanceTo sno TickedLedgerState{..} = TickedLedgerState {
      tickedScheduled = notYet
    , tickedValue     = repeatedly
                          applyLedgerUpdate
                          (Map.elems toApply)
                          tickedValue
    }
  where
    toApply, notYet :: Scheduled
    (toApply, notYet) = Map.partitionWithKey
                          (\sno' _ -> sno' <= sno)
                          tickedScheduled

applyLedgerUpdate :: LedgerUpdate -> LedgerValue -> LedgerValue
applyLedgerUpdate (IncreaseValueBy d) = (+ d)

-- | Advance ledger state to the next slot (without a block)
stepLedgerState :: LedgerState -> LedgerState
stepLedgerState ledgerState = LedgerState {
      ledgerScheduled = tickedScheduled
    , ledgerValue     = tickedValue
    , ledgerTip       = NotOrigin nextSlot
    }
  where
    nextSlot :: SlotNo
    nextSlot = case ledgerTip ledgerState of
                 Origin      -> SlotNo 0
                 NotOrigin s -> succ s

    TickedLedgerState{..} = tickLedgerState nextSlot ledgerState

applyBlock :: Block -> Ticked LedgerState -> LedgerState
applyBlock (Block slot blockScheduled) TickedLedgerState{..} = LedgerState {
      ledgerScheduled = Map.unionWith combineUpdates
                          tickedScheduled
                          notYet
    , ledgerValue     = maybe id applyLedgerUpdate mToApply $ tickedValue
    , ledgerTip       = NotOrigin slot
    }
  where
    combineUpdates :: LedgerUpdate -> LedgerUpdate -> LedgerUpdate
    combineUpdates (IncreaseValueBy x) (IncreaseValueBy y) =
        IncreaseValueBy (x + y)

    -- Immediately apply any changes scheduled for the block's own slot
    mToApply :: Maybe LedgerUpdate
    notYet   :: Scheduled
    (mToApply, notYet) = Map.updateLookupWithKey
                           (\_ _ -> Nothing)
                           slot
                           blockScheduled

{-------------------------------------------------------------------------------
  Moving between eras
-------------------------------------------------------------------------------}

-- A value of @x@ in era @n@ corresponds to a value of @3x@ in era @n+1@
--
-- This means that the HFC translation functions have some work to do.
translateToNextEra :: LedgerState -> LedgerState
translateToNextEra LedgerState{..} = LedgerState{
      ledgerTip       = ledgerTip
    , ledgerValue     = inflate ledgerValue
    , ledgerScheduled = inflate <$> ledgerScheduled
    }

inflate :: Num a => a -> a
inflate x = x * 3

{-------------------------------------------------------------------------------
  Forecasting within an era
-------------------------------------------------------------------------------}

withinEraForecast :: MaxLookahead -> LedgerState -> Forecast LedgerView
withinEraForecast maxLookAhead st = Forecast{
      forecastAt  = ledgerTip st
    , forecastFor = go
    }
  where
    go :: SlotNo -> Except OutsideForecastRange (Ticked LedgerView)
    go for = do
        when (for >= upperBound) $
          throwError OutsideForecastRange {
                  outsideForecastAt     = ledgerTip st
                , outsideForecastMaxFor = upperBound
                , outsideForecastFor    = for
                }

        return $ TickedView . tickedValue $ tickLedgerState for st
      where
        -- Exclusive upper bound
        upperBound :: SlotNo
        upperBound = case ledgerTip st of
                       Origin      -> addSlots maxLookAhead (SlotNo 0)
                       NotOrigin s -> addSlots maxLookAhead (succ s)

{-------------------------------------------------------------------------------
  Forecasting across eras
-------------------------------------------------------------------------------}

-- | Translations between eras
translations :: forall xs.
     TestSetup xs
  -> InPairs (TranslateForecast (K LedgerState) (K LedgerView)) xs
translations TestSetup{..} =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty{} -> go testLookahead
  where
    go :: Exactly (x ': xs') MaxLookahead
       -> InPairs (TranslateForecast (K LedgerState) (K LedgerView)) (x ': xs')
    go (ExactlyCons _ ExactlyNil) =
        InPairs.PNil
    go (ExactlyCons this rest@(ExactlyCons next _)) =
        InPairs.PCons (tr this next) (go rest)

    tr :: MaxLookahead -- ^ Look-ahead in the current era
       -> MaxLookahead -- ^ Look-ahead in the next era
       -> TranslateForecast (K LedgerState) (K LedgerView) era era'
    tr thisLookahead nextLookahead =
        TranslateForecast $ \transition sno (K st) ->
          assert (sno >= boundSlot transition) $ do
            let tip :: WithOrigin SlotNo
                tip = ledgerTip st

                -- (Exclusive) upper bound for the forecast
                bound :: SlotNo
                bound = crossEraForecastBound
                          tip
                          (boundSlot transition)
                          thisLookahead
                          nextLookahead

            when (sno >= bound) $
              throwError $ OutsideForecastRange {
                  outsideForecastAt     = tip
                , outsideForecastMaxFor = bound
                , outsideForecastFor    = sno
                }

            -- We set things up so that we don't have to be too careful with
            -- the ordering of the operations here: @3x + 3y = 3(x + y)@.
            return $ TickedK $ TickedView $
                inflate $ tickedValue $ tickLedgerState sno st

acrossErasForecast :: forall xs.
     TestSetup xs
  -> Map (WithOrigin SlotNo) LedgerState
  -> Forecast LedgerView
acrossErasForecast setup@TestSetup{..} ledgerStates =
    mapForecast aux $
      mkHardForkForecast (translations setup) (HardForkState (go testEras))
  where
    TestForecastParams{..} = testForecastParams

    aux :: Ticked (HardForkLedgerView_ (K LedgerView) xs)
        -> Ticked LedgerView
    aux = hcollapse
        . hmap (K . getTickedK . unComp)
        . tickedHardForkLedgerViewPerEra

    go :: NonEmpty xs' TestEra
       -> Telescope (K Past) (Current (AnnForecast (K LedgerState) (K LedgerView))) xs'
    go (NonEmptyOne era) =
        assert (testEraContains testForecastAt era) $
        TZ $ Current {
            currentStart = eraStart (testEraSummary era)
          , currentState = AnnForecast {
                annForecast      = mapForecast TickedK $
                                     withinEraForecast
                                       (testEraMaxLookahead era)
                                       st
              , annForecastState = K st
              , annForecastTip   = testForecastAt
              , annForecastEnd   = Nothing
              }
          }
        where
          st :: LedgerState
          st = ledgerStates `mapAt` testForecastAt
    go (NonEmptyCons era eras) =
        if testEraContains testForecastAt era then
          TZ $ Current {
              currentStart = start
            , currentState = AnnForecast {
                  annForecast      = mapForecast TickedK $
                                       withinEraForecast
                                         (testEraMaxLookahead era)
                                         st
                , annForecastState = K st
                , annForecastTip   = testForecastAt
                , annForecastEnd   = Just end
                }
            }
        else
          TS (K (Past start end)) (go eras)
      where
        st :: LedgerState
        st = ledgerStates `mapAt` testForecastAt

        start, end :: Bound
        start = eraStart (testEraSummary era)
        end   = case eraEnd (testEraSummary era) of
                  EraUnbounded -> error "past eras cannot be unbounded"
                  EraEnd e     -> e

{-------------------------------------------------------------------------------
  Forecast validity
-------------------------------------------------------------------------------}

correctForecastOf :: Ticked LedgerView -> LedgerState -> Property
TickedView forecast `correctForecastOf` actual =
      counterexample ("forecast: " ++ show forecast)
    $ counterexample ("actual: " ++ show actual)
    $ forecast === ledgerValue actual

{-------------------------------------------------------------------------------
  Sanity checks
-------------------------------------------------------------------------------}

prop_validTestSetup :: Some TestSetup -> Property
prop_validTestSetup (Some setup@TestSetup{..}) = conjoin [
        counterexample "strictlyIncreasing" $
          strictlyIncreasing $ map (\(Block s _) -> s) $ concat $ blocksPerEra setup
      , counterexample "obeysMaxLookahead" $
          conjoin $ map checkLookahead (toList testEras)
      , counterexample "validForecastParams" $
          validForecastParams setup === Right ()
      ]
  where
    checkLookahead :: TestEra -> Property
    checkLookahead TestEra{..} = conjoin [
          slotChange `ge` addSlots testEraMaxLookahead slotBlock
        | (Block slotBlock scheduled) <- testEraBlocks
        , (slotChange, _update) <- Map.toList scheduled
        ]

prop_forecast :: Bool -> Some TestSetup -> Property
prop_forecast useWithinEra (Some setup@TestSetup{..}) =
      tabulate "(useWithinEra, isWithinEra, within range)" [intercalate "/" [
          show useWithinEra
        , show isWithinEra
        , show (isRight mForecastLedger)
        ]]
    $ counterexample ("ledgerStates: " ++ show ledgerStates)
    $ counterexample ("markTransitions: " ++ show (markTransitions setup))
    $ case mForecastLedger of
        Left _outOfRange ->
          -- Ideally we would check that these out of ranges are justified.
          property True
        Right forecastLedger ->
          forecastLedger `correctForecastOf` actualLedger
  where
    TestForecastParams{..} = testForecastParams

    ledgerStates :: Map (WithOrigin SlotNo) LedgerState
    ledgerStates = interpretChain setup

    forecast :: Forecast LedgerView
    forecast
      | useWithinEra =
          withinEraForecast
            (slotMaxLookahead setup testForecastAt)
            (ledgerStates `mapAt` testForecastAt)
      | otherwise =
          acrossErasForecast setup ledgerStates

    for :: SlotNo
    for | useWithinEra = testForecastWithinEra
        | otherwise    = testForecastAcrossEras

    isWithinEra :: Bool
    isWithinEra = slotSameEra setup testForecastAt (NotOrigin for)

    mForecastLedger :: Either OutsideForecastRange (Ticked LedgerView)
    mForecastLedger = runExcept $ forecastFor forecast for

    actualLedger :: LedgerState
    actualLedger = ledgerStates `mapAt` NotOrigin for

{-------------------------------------------------------------------------------
  Valued derived from the 'TestSetup'
-------------------------------------------------------------------------------}

-- | Mark era transitions
--
-- This is an auxiliary type used in 'interpretChain'. It records the start of
-- end of the current era (equals start of the next)
data EraTransition = EraTransition SlotNo
  deriving (Show)

markTransitions :: TestSetup xs -> [Either Block EraTransition]
markTransitions =
    concatMap (either (map Left) ((:[]) . Right)) . go . toList . testEras
  where
    go :: [TestEra] -> [Either [Block] EraTransition]
    go []        = []
    go [e]       = [Left (testEraBlocks e)]
    go (e:e':es) = Left (testEraBlocks e)
                 : Right (EraTransition (boundSlot (eraStart (testEraSummary e'))))
                 : go (e' : es)

-- | The ledger state at every 'SlotNo'
interpretChain :: TestSetup xs -> Map (WithOrigin SlotNo) LedgerState
interpretChain setup@TestSetup{..} =
    Map.fromList $
        (Origin, initLedgerState)
      : go startSlot initLedgerState (markTransitions setup)
  where
    -- The 'endSlot' is the max 'SlotNo' we might need a ledger state for
    startSlot, endSlot :: SlotNo
    startSlot = SlotNo 0
    endSlot   = max (testForecastWithinEra  testForecastParams)
                    (testForecastAcrossEras testForecastParams)

    go :: SlotNo       -- Next expected slot
       -> LedgerState  -- Previous state
       -> [Either Block EraTransition]
       -> [(WithOrigin SlotNo, LedgerState)]
    go curSlot prevLedger [] =
        pad curSlot prevLedger
    go curSlot prevLedger xs@(Left blk@(Block s _):xs')
      | s > curSlot = (NotOrigin curSlot, stepped) : go (succ curSlot) stepped xs
      | otherwise   = (NotOrigin curSlot, applied) : go (succ curSlot) applied xs'
      where
        stepped = stepLedgerState prevLedger
        ticked  = tickLedgerState curSlot prevLedger
        applied = applyBlock blk ticked
    -- Applying the transition itself does not advance the slot
    -- (there might be a block in the very first slot in the next era)
    go curSlot prevLedger xs@(Right (EraTransition s):xs')
      | s > curSlot = (NotOrigin curSlot, stepped) : go (succ curSlot) stepped xs
      | otherwise   =                                go       curSlot  doubled xs'
      where
        stepped = stepLedgerState    prevLedger
        doubled = translateToNextEra prevLedger

    -- After we have applied the final block, keep ticking the ledger state
    -- until we have reached the required 'SlotNo'
    pad :: SlotNo -> LedgerState -> [(WithOrigin SlotNo, LedgerState)]
    pad curSlot prevLedger
      | curSlot > endSlot = []
      | otherwise         = (NotOrigin curSlot, stepped) : pad (succ curSlot) stepped
      where
        stepped = stepLedgerState prevLedger

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data TestEra = TestEra {
      -- | Era summary (the 'EraParams' and bounds)
      --
      -- NOTE: The 'EraParams' (including associated safe zone) are independent
      -- from the lookahead, which is a property of the ledger ("how far into
      -- the future can we look and still know the ledger state"). The safe
      -- zones of the 'EraParams' only provide guarantees about when we can
      -- expect era transitions.
      testEraSummary      :: EraSummary

      -- | The maximum look ahead
      --
      -- The HFC itself does not impose any restrictions on the relation between
      -- the max lookahead of various eras. If the max lookahead in era B is
      -- smaller than the max lookahead in era A, this " merely " poses a
      -- problem for the translation function.
    , testEraMaxLookahead :: MaxLookahead

      -- | Blocks on the chain in this era
    , testEraBlocks       :: [Block]
    }
  deriving (Show)

-- | The parameters for the forecast we construct
--
-- The forecast is constructed in a single era. The HFC combinator is
-- responsible for extending it across eras (that's precisely what we're
-- testing in this module, of course).
data TestForecastParams = TestForecastParams {
      -- | Anchor of the forecast
      testForecastAt         :: WithOrigin SlotNo

      -- | An arbitrary slot number within the forecast's era
      --
      -- This is used as a sanity check to make sure that within-era
      -- forecasting works as expected.
      --
      -- Must be at or after 'testForecastAt'.
    , testForecastWithinEra  :: SlotNo

      -- | An arbitrary slot after (or equal to) 'testForecastAt'
      --
      -- This is used to test the general case (across eras).
      -- Invariant: ahead of testForecastAt but not ahead by more than one era.
    , testForecastAcrossEras :: SlotNo
    }
  deriving (Show)

data TestSetup xs = (SListI xs, IsNonEmpty xs) => TestSetup {
      -- | The maximum lookahead in each era
      --
      -- We record this separately because the chain might terminate early
      -- (we might not have reached all eras yet), but these parameters /are/
      -- known for all eras (similar to how the HFC wants to know the era
      -- parameters for all eras)
      testLookahead      :: Exactly xs MaxLookahead

      -- | The test eras themselves
    , testEras           :: NonEmpty xs TestEra

      -- | The forecast we're constructing
    , testForecastParams :: TestForecastParams
    }

type MaxLookahead = Word64

deriving instance Show (TestSetup xs)
deriving instance Show (Some TestSetup)

{-------------------------------------------------------------------------------
  Invariant
-------------------------------------------------------------------------------}

validForecastParams :: TestSetup xs -> Either String ()
validForecastParams setup@TestSetup{..} = runExcept $ do
    unless (testForecastAt <= NotOrigin testForecastWithinEra) $
      throwError $ mconcat [
          "'testForecastWithinEra' == "
        , show testForecastWithinEra
        , " not after 'testForecastAt' == "
        , show testForecastAt
        ]

    unless (testForecastAt <= NotOrigin testForecastAcrossEras) $
      throwError $ mconcat [
          "'testForecastAcrossEras' == "
        , show testForecastAcrossEras
        , " not after 'testForecastAt' == "
        , show testForecastAt
        ]

    era <- case slotEra' setup testForecastAt of
             Just era -> return era
             Nothing  -> throwError $ mconcat [
                 "No era known for 'testForecastAt' == "
               , show testForecastAt
               ]

    unless (testEraContains (NotOrigin testForecastWithinEra) era) $
      throwError $ mconcat [
          "'testForecastWithinEra' == "
        , show testForecastWithinEra
        , " not in same era as 'testForecastAt' == "
        , show testForecastAt
        ]

    case slotEra' setup (NotOrigin testForecastAcrossEras) of
      Just _  -> return ()
      Nothing -> throwError $ mconcat [
          "No era known for 'testForecastAcrossEras' == "
        , show testForecastAcrossEras
        ]

    -- It would be nice to check that the "across eras" isn't ahead by more than
    -- one era (but that's a little tricky to do right now so we omit this
    -- check).
  where
    TestForecastParams{..} = testForecastParams

{-------------------------------------------------------------------------------
  Query 'TestEra'
-------------------------------------------------------------------------------}

testEraContains :: WithOrigin SlotNo -> TestEra -> Bool
testEraContains mSlot TestEra{..} = and [
      boundSlot eraStart <= fromWithOrigin (SlotNo 0) mSlot
    , case (mSlot, eraEnd) of
        (NotOrigin s, EraEnd end) -> s < boundSlot end
        _otherwise                -> True
    ]
  where
    EraSummary{..} = testEraSummary

{-------------------------------------------------------------------------------
  Query the 'TestSetup'
-------------------------------------------------------------------------------}

blocksPerEra :: TestSetup xs -> [[Block]]
blocksPerEra = map testEraBlocks . toList . testEras

-- | Era containing the given slot, if any
slotEra' :: TestSetup xs -> WithOrigin SlotNo -> Maybe TestEra
slotEra' TestSetup{..} mSlot =
    listToMaybe $ filter (testEraContains mSlot) (toList testEras)

-- | Wrapper around 'slotEra' to be used when the era should exist
slotEra :: HasCallStack => TestSetup xs -> WithOrigin SlotNo -> TestEra
slotEra setup mSlot =
    case slotEra' setup mSlot of
      Nothing  -> error $ "slotEra: unknown slot " ++ show mSlot
      Just era -> era

-- | Maximum lookahead of the ledger in the era containing the slot
slotMaxLookahead :: TestSetup xs -> WithOrigin SlotNo -> MaxLookahead
slotMaxLookahead setup = testEraMaxLookahead . slotEra setup

-- | Check if two slots are in the same era
slotSameEra :: TestSetup xs -> WithOrigin SlotNo -> WithOrigin SlotNo -> Bool
slotSameEra setup otherSlot = testEraContains otherSlot . slotEra setup

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

instance Arbitrary (Some TestSetup) where
  arbitrary = chooseEras $ \ixs -> do
      ProofNonEmpty{} <- return $ isNonEmpty ixs
      summary   <- getSummary <$> genSummary ixs
      lookahead <- genMaxLookahead (eraIndices ixs) (nonEmptyWeaken summary)
      eras      <- sequence $
                      genTestEra <$> summary
                                 <*> exactlyWeakenNonEmpty lookahead
      forecast  <- genForecastParams (toList eras)
      return $ Some TestSetup{
          testLookahead      = lookahead
        , testEras           = eras
        , testForecastParams = forecast
        }
    where
      genMaxLookahead ::
           NP (K era) xs
        -> AtMost xs EraSummary
        -> Gen (Exactly xs MaxLookahead)
      genMaxLookahead Nil _ =
          return ExactlyNil
      genMaxLookahead (_ :* es) (AtMostCons s ss) = sized' $ \sz -> do
          l  <- choose (0, sz)

          -- Suppose an era lasts 10 slots
          --
          -- >           ~             ~
          -- > previous  ~  10 ... 19  ~  next
          -- >           ~             ~
          --
          -- If the maximum lookahead is 0, then if we are the last block
          -- of the previous era, we can't even forecast the ledger state for
          -- slot 10. If it's 1, we can forecast to 10; if it's 10, we can
          -- forecast to 19; but if it's 11, we can forecast to 20, which is
          -- the /next next/ era. We don't currently support this, and so
          -- we avoid generating this edge case.

          let l' = case eraEnd s of
                     EraUnbounded -> l
                     EraEnd end   -> min l $
                         countSlots
                           (boundSlot end)
                           (boundSlot (eraStart s))

          ls <- genMaxLookahead es ss
          return (ExactlyCons l' ls)
      genMaxLookahead (_ :* es) AtMostNil = sized' $ \sz -> do
          l  <- choose (0, sz)
          ls <- genMaxLookahead es AtMostNil
          return (ExactlyCons l ls)

      genTestEra :: EraSummary -> MaxLookahead -> Gen TestEra
      genTestEra summary@EraSummary{..} maxLookahead = sized' $ \sz -> do
          upperBound   <- case eraEnd of
            EraEnd bound -> return bound
            EraUnbounded -> mkUpperBound eraParams eraStart <$> choose (0, sz)
          mBlocks <- forM (enumIncExc (boundSlot eraStart) (boundSlot upperBound)) $ \slot -> do
            slotFilled <- arbitrary
            if slotFilled then do
              scheduled <- genScheduled maxLookahead slot
              return $ Just (Block slot scheduled)
            else
              return Nothing
          return TestEra {
              testEraSummary      = summary
            , testEraMaxLookahead = maxLookahead
            , testEraBlocks       = catMaybes mBlocks
            }

      genScheduled :: MaxLookahead -> SlotNo -> Gen Scheduled
      genScheduled maxLookahead slotBlock = do
          numChanges <- choose (0, 2)
          fmap Map.fromList $
            replicateM numChanges $ genChange maxLookahead slotBlock

      genChange :: MaxLookahead -> SlotNo -> Gen (SlotNo, LedgerUpdate)
      genChange maxLookahead slotBlock = sized' $ \sz -> do
          skip     <- choose (0, sz)
          increase <- choose (0, 2)
          -- If the maxLookahead is zero (no look ahead possible), the change
          -- is applied when we apply the block (i.e., in the same slot).
          let slotChange = addSlots (maxLookahead + skip) slotBlock
          return (slotChange, IncreaseValueBy increase)

      -- Construct an upper bound for an era, given number of epochs
      mkUpperBound :: EraParams -> Bound -> Word64 -> Bound
      mkUpperBound eraParams eraStart =
            History.mkUpperBound eraParams eraStart
          . flip addEpochs (boundEpoch eraStart)

      genForecastParams :: [TestEra] -> Gen TestForecastParams
      genForecastParams eras = sized' $ \sz -> do
          -- Pick an era for the forecast
          (isFirstEra, anchorEra) <- elements $ zip (True : repeat False) eras

          let anchorEraStart = eraStart $ testEraSummary anchorEra
              anchorEraEnd   = eraEnd   $ testEraSummary anchorEra

          -- Pick an anchor
          at <- oneof $ concat [
              [ fmap NotOrigin $ elements $
                  enumIncExc
                    (boundSlot anchorEraStart)
                    (boundSlot end)
              | EraEnd end <- [anchorEraEnd]
              ]

            , [ do upperBound <- choose (1, 1 + sz) -- upper bound is exclusive
                   fmap NotOrigin $ elements $
                     enumIncExc
                       (boundSlot anchorEraStart)
                       (addSlots upperBound (boundSlot anchorEraStart))
              | EraUnbounded <- [anchorEraEnd]
              ]

            , [ return Origin
              | isFirstEra
              ]
            ]

          -- Pick a slot within the same era
          -- (for within-era forecast sanity check)
          let at' = fromWithOrigin (SlotNo 0) at
          withinEra <- pickSlotBetween at' anchorEraEnd

          -- For any slot after the anchor
          let finalEra    = last eras
              finalEraEnd = eraEnd $ testEraSummary finalEra
          acrossEras <- pickSlotBetween at' finalEraEnd

          return TestForecastParams {
              testForecastAt         = at
            , testForecastWithinEra  = withinEra
            , testForecastAcrossEras = acrossEras
            }

      pickSlotBetween :: SlotNo -> EraEnd -> Gen SlotNo
      pickSlotBetween lo hi = sized' $ \sz -> oneof $ concat [
            [ elements $ enumIncExc lo (boundSlot end)
            | EraEnd end <- [hi]
            ]

          , [ do upperBound <- choose (1, 1 + sz) -- upper bound is exclusive
                 elements $ enumIncExc lo (addSlots upperBound lo)
            | EraUnbounded <- [hi]
            ]
          ]

  -- We make some effort towards a good shrinker, but there is a lot we could
  -- still do to improve it:
  --
  -- o We could drop some eras entirely
  -- o We could shift the era bounds
  -- o We could shrink the maximum lookahead in the individual eras
  --
  -- The tricky part is to do this without violating some of the invariants
  -- that we established in the generator:
  --
  -- o The era of the forecast anchor might not exist anymore
  -- o Due to reducing the bounds of an era, the within-era 'at' might not
  --   actually be within-era anymore
  -- o Due to a reduction in the max lookahead, a forecast might now exceed the
  --   maximum.
  -- o Due to shrinking an era's size, the lookahead might now exceed the
  --   era length (see comment above regarding forecasting across multiple eras)
  -- o By shrinking the anchor of the forecast, it might not be in the same era
  --   as the within-era 'at' anymore.
  shrink (Some setup@TestSetup{..}) = concat [
        -- Shrink the eras
        [ Some setup'
        | eras' <- shrinkEras testEras
        , let setup' = setup { testEras = eras' }
        ]

        -- Shrink the forecast params
      , [ Some setup'
        | params' <- shrinkForecastParams testForecastParams
        , let setup' = setup { testForecastParams = params' }
        ]
      ]
    where
      shrinkEras :: NonEmpty xs TestEra -> [NonEmpty xs TestEra]
      shrinkEras eras = concat [
            -- Shrink one era
            nonEmptyMapOne shrinkEra eras
          ]

      shrinkEra :: TestEra -> [TestEra]
      shrinkEra era@TestEra{..} = concat [
            -- Drop some blocks
            [ era'
            | blocks' <- shrinkList (const []) testEraBlocks
            , let era' = era { testEraBlocks = blocks' }
            ]

            -- Shrink blocks
            --
            -- We don't use shrinkList for this, because we need some context
          , [ era'
            | (xs, y, zs) <- splits testEraBlocks
            , let prev | null xs   = Nothing
                       | otherwise = Just (last xs)
            , y' <- shrinkBlock testEraSummary testEraMaxLookahead prev y
            , let era' = era { testEraBlocks = xs ++ [y'] ++ zs }
            ]
          ]

      shrinkBlock :: EraSummary -> MaxLookahead -> Maybe Block -> Block -> [Block]
      shrinkBlock summary maxLookahead mPrev (Block (SlotNo slot) scheduled) = concat [
            -- Move the block earlier into the era
            --
            -- NOTE: Moving a block _earlier_ into the chain can't violate
            -- the max-lookahead, as the distance between the block and the
            -- change can only _increase_
            [ Block slot' scheduled
            | slot' <- map SlotNo $ shrink slot
              -- Don't clash with the previous block
            , case mPrev of
                Just (Block prevSlot _) -> slot' > prevSlot
                Nothing                 -> True
              -- Don't move block out of this era
            , slot' >= boundSlot (eraStart summary)
            ]

            -- Shrink the block body
          , [ Block (SlotNo slot) scheduled'
            | scheduled' <- shrinkScheduled maxLookahead (SlotNo slot) scheduled
            ]
          ]

      shrinkScheduled :: MaxLookahead -> SlotNo -> Scheduled -> [Scheduled]
      shrinkScheduled maxLookahead slotBlock =
            map Map.fromList
          . shrinkList shrinkUpdate
          . Map.toList
        where
          shrinkUpdate :: (SlotNo, LedgerUpdate) -> [(SlotNo, LedgerUpdate)]
          shrinkUpdate (SlotNo slotUpdate, update@(IncreaseValueBy newLedgerValue)) = concat [
                -- Shrink the ledger value (complicated ledger values distract)
                [ (SlotNo slotUpdate, IncreaseValueBy newLedgerValue')
                | newLedgerValue' <- shrink newLedgerValue
                ]

                -- Try to do the update sooner
              , [ (slotUpdate', update)
                | slotUpdate' <- map SlotNo $ shrink slotUpdate
                  -- The earliest it can change is the very next slot
                , slotUpdate' > slotBlock
                  -- We must still obey the maxLookahead though
                , countSlots slotUpdate' slotBlock > maxLookahead
                ]
              ]

      shrinkForecastParams :: TestForecastParams -> [TestForecastParams]
      shrinkForecastParams params@TestForecastParams{..} = concat [
            [ params'
            | at' <- shrinkSlotNo' testForecastAt
            , slotSameEra setup at' (NotOrigin testForecastWithinEra)
            , let params' = params { testForecastAt = at' }
            ]

          , [ params'
            | withinEra' <- shrinkSlotNo testForecastWithinEra
            , NotOrigin withinEra' >= testForecastAt
            , let params' = params { testForecastWithinEra = withinEra' }
            ]

          , [ params'
            | acrossEras' <- shrinkSlotNo testForecastAcrossEras
            , NotOrigin acrossEras' >= testForecastAt
            , let params' = params { testForecastAcrossEras = acrossEras' }
            ]
          ]

      shrinkSlotNo' :: WithOrigin SlotNo -> [WithOrigin SlotNo]
      shrinkSlotNo' Origin        = []
      shrinkSlotNo' (NotOrigin s) = Origin : map NotOrigin (shrinkSlotNo s)

      shrinkSlotNo :: SlotNo -> [SlotNo]
      shrinkSlotNo (SlotNo s) = map SlotNo (shrink s)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Like 'enumFromTo', but with an exclusive upper bound
enumIncExc :: forall a. (Ord a, Enum a) => a -> a -> [a]
enumIncExc lo hi = go lo
  where
    go :: a -> [a]
    go x | x >= hi   = []
         | otherwise = x : go (succ x)

sized' :: (Word64 -> Gen a) -> Gen a
sized' f = sized (f . fromIntegral)

mapAt :: (HasCallStack, Show k, Show a, Ord k) => Map k a -> k -> a
m `mapAt` k =
    Map.findWithDefault
      (error $ concat [
           "at: key "
         , show k
         , " not found in "
         , show m
         ])
      k
      m
