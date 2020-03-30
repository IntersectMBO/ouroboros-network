{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

module Test.Consensus.HardFork.History (tests) where

import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.Functor.Const
import           Data.Functor.Identity
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Time
import           Data.Word

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime.SlotLength
import           Ouroboros.Consensus.HardFork.History (ShiftTime (..))
import qualified Ouroboros.Consensus.HardFork.History as HF
import           Ouroboros.Consensus.Util.Counting

import           Test.Util.Orphans.Arbitrary

tests :: TestTree
tests = testGroup "HardForkHistory" [
      testGroup "Summary" [
          testGroup "Sanity" [
              testProperty "generator" $ checkGenerator $ \ArbitrarySummary{..} ->
                checkInvariant HF.invariantSummary arbitrarySummary
            , testProperty "shrinker"  $ checkShrinker $ \ArbitrarySummary{..} ->
                checkInvariant HF.invariantSummary arbitrarySummary
            ]
        , testGroup "Conversions" [
              testProperty "roundtripWallclockSlot" roundtripWallclockSlot
            , testProperty "roundtripSlotWallclock" roundtripSlotWallclock
            , testProperty "roundtripSlotEpoch"     roundtripSlotEpoch
            , testProperty "roundtripEpochSlot"     roundtripEpochSlot
            , testProperty "reportsPastHorizon"     reportsPastHorizon
            ]
        ]
    , testGroup "Chain" [
          testGroup "Sanity" [
              testProperty "generator" $ checkGenerator $ \ArbitraryChain{..} ->
                checkInvariant HF.invariantShape arbitraryChainShape
            , testProperty "shrinker"  $ checkShrinker $ \ArbitraryChain{..} ->
                checkInvariant HF.invariantShape arbitraryChainShape
            ]
        , testGroup "Conversions" [
              testProperty "summarizeInvariant" summarizeInvariant
            , testProperty "eventSlotToEpoch"     eventSlotToEpoch
            , testProperty "eventEpochToSlot"     eventEpochToSlot
            , testProperty "eventSlotToWallclock" eventSlotToWallclock
            , testProperty "eventWallclockToSlot" eventWallclockToSlot
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Sanity checks

  Explicit 'forAll' here since we don't want to assume that the shrinker is
  correct here.
-------------------------------------------------------------------------------}

-- | Test the generator
checkGenerator :: (Arbitrary a, Show a) => (a -> Property) -> Property
checkGenerator p = forAll arbitrary $ p

-- | Test the shrinker
checkShrinker :: (Arbitrary a, Show a) => (a -> Property) -> Property
checkShrinker p = forAll arbitrary $ conjoin . map p . shrink

checkInvariant :: (a -> Except String ()) -> (a -> Property)
checkInvariant f a =
    case runExcept (f a) of
      Left err -> counterexample err $ property False
      Right () -> property True

{-------------------------------------------------------------------------------
  Tests using just 'Summary'
-------------------------------------------------------------------------------}

roundtripWallclockSlot :: ArbitrarySummary -> Property
roundtripWallclockSlot ArbitrarySummary{ arbitrarySummary = summary
                                       , beforeHorizonTime = time
                                       } = noPastHorizonException $ do
    (slot  ,  inSlot ) <- HF.wallclockToSlot summary time
    (time' , _slotLen) <- HF.slotToWallclock summary slot
    return $ addUTCTime inSlot time' === time

roundtripSlotWallclock :: ArbitrarySummary -> Property
roundtripSlotWallclock ArbitrarySummary{ arbitrarySummary  = summary
                                       , beforeHorizonSlot = slot
                                       } = noPastHorizonException $ do
    (time  , _slotLen) <- HF.slotToWallclock summary slot
    (slot' ,  inSlot ) <- HF.wallclockToSlot summary time
    return $ slot' === slot .&&. inSlot === 0

roundtripSlotEpoch :: ArbitrarySummary -> Property
roundtripSlotEpoch ArbitrarySummary{ arbitrarySummary  = summary
                                   , beforeHorizonSlot = slot
                                   } = noPastHorizonException $ do
    (epoch ,  inEpoch  ) <- HF.slotToEpoch summary slot
    (slot' , _epochSize) <- HF.epochToSlot summary epoch
    return $ HF.addSlots inEpoch slot' === slot

roundtripEpochSlot :: ArbitrarySummary -> Property
roundtripEpochSlot ArbitrarySummary{ arbitrarySummary   = summary
                                   , beforeHorizonEpoch = epoch
                                   } = noPastHorizonException $ do
    (slot  , _epochSize) <- HF.epochToSlot summary epoch
    (epoch',  inEpoch  ) <- HF.slotToEpoch summary slot
    return $ epoch' === epoch .&&. inEpoch === 0

reportsPastHorizon :: ArbitrarySummary -> Property
reportsPastHorizon ArbitrarySummary{..} = conjoin [
      isPastHorizonException $
        HF.wallclockToSlot arbitrarySummary pastHorizonTime
    , isPastHorizonException $
        HF.slotToWallclock arbitrarySummary pastHorizonSlot
    , isPastHorizonException $
        HF.slotToEpoch arbitrarySummary pastHorizonSlot
    , isPastHorizonException $
        HF.epochToSlot arbitrarySummary pastHorizonEpoch
    ]

noPastHorizonException :: Except HF.PastHorizonException Property -> Property
noPastHorizonException mProp =
    case runExcept mProp of
      Right prop -> prop
      Left  ex   -> counterexample ("Unexpected " ++ show ex) $ property False

isPastHorizonException :: Show a => Except HF.PastHorizonException a -> Property
isPastHorizonException ma =
    case runExcept ma of
      Right a -> counterexample ("Unexpected " ++ show a) $ property False
      Left _  -> property True

{-------------------------------------------------------------------------------
  Properties of summarize

  TODO: We should strengten these tests: at the moment, the summary is
  constructed from the /entire/ blockchain, and then applied to any of the
  events in the blockchain. That is good, but we should additionally construct
  the summary from a /prefix/ of the blockchain and then verify that we can
  still convert events /after/ that prefix (up to the safe zone).
-------------------------------------------------------------------------------}

-- | Check that 'summarize' establishes 'invariantSummary'
summarizeInvariant :: ArbitraryChain -> Property
summarizeInvariant chain =
    withArbitraryChainSummary chain $
      checkInvariant HF.invariantSummary

eventSlotToEpoch :: ArbitraryChain -> Property
eventSlotToEpoch chain@ArbitraryChain{..} =
    withArbitraryChainSummary chain $ \summary ->
      noPastHorizonException $ do
        (epoch, relSlot) <- HF.slotToEpoch summary eventAbsSlot
        return $ epoch === eventEpoch .&&. relSlot === eventRelSlot
  where
    (EventTime{..}, _, _) = arbitraryChainPre !! arbitraryEventIx

eventEpochToSlot :: ArbitraryChain -> Property
eventEpochToSlot chain@ArbitraryChain{..} =
    withArbitraryChainSummary chain $ \summary ->
      noPastHorizonException $ do
        (startOfEpoch, EpochSize epochSize) <- HF.epochToSlot summary eventEpoch
        return $ eventAbsSlot === HF.addSlots eventRelSlot startOfEpoch
            .&&. eventRelSlot < epochSize
  where
    (EventTime{..}, _, _) = arbitraryChainPre !! arbitraryEventIx

eventSlotToWallclock :: ArbitraryChain -> Property
eventSlotToWallclock chain@ArbitraryChain{..} =
    withArbitraryChainSummary chain $ \summary ->
      noPastHorizonException $ do
        (time, _slotLen) <- HF.slotToWallclock summary eventAbsSlot
        return $ time === eventTime
  where
    (EventTime{..}, _, _) = arbitraryChainPre !! arbitraryEventIx

eventWallclockToSlot :: ArbitraryChain -> Property
eventWallclockToSlot chain@ArbitraryChain{..} =
    withArbitraryChainSummary chain $ \summary ->
      noPastHorizonException $ do
        (slot, inSlot) <- HF.wallclockToSlot summary eventTime
        return $ slot === eventAbsSlot .&&. inSlot === 0
  where
    (EventTime{..}, _, _) = arbitraryChainPre !! arbitraryEventIx

{-------------------------------------------------------------------------------
  Arbitrary chain
-------------------------------------------------------------------------------}

data ArbitraryChain = forall xs. ArbitraryChain {
      arbitraryChainStart :: HF.SystemStart
    , arbitraryChainPre   :: PreChain
    , arbitraryChainEras  :: Eras     xs
    , arbitraryChainShape :: HF.Shape xs
    , arbitraryChain      :: Chain    xs

      -- Index into the 'PreChain' of events
      -- Guaranteed to be within bounds
    , arbitraryEventIx    :: Int
    }

deriving instance Show ArbitraryChain

withArbitraryChainSummary :: ArbitraryChain
                          -> (forall xs'. HF.Summary xs' -> a)
                          -> a
withArbitraryChainSummary ArbitraryChain{arbitraryChainEras = eras :: Eras xs, ..} f =
    f summary
  where
    transitions :: HF.Transitions xs
    transitions = chainTransitions eras arbitraryChain

    tip :: WithOrigin SlotNo
    tip = chainTip arbitraryChain

    summary :: HF.Summary xs
    summary = HF.summarize
                arbitraryChainStart
                tip
                arbitraryChainShape
                transitions

instance Arbitrary ArbitraryChain where
  arbitrary = chooseEras $ \eras -> do
      start    <- HF.SystemStart <$> arbitrary
      shape    <- genShape genParams (EpochNo 0) eras
      preChain <- genPreChain start eras shape `suchThat` (not . null)
      eventIx  <- choose (0, length preChain - 1)
      return ArbitraryChain {
          arbitraryChainStart   = start
        , arbitraryChainPre     = preChain
        , arbitraryChainEras    = eras
        , arbitraryChainShape   = shape
        , arbitraryChain        = fromPreChain eras preChain
        , arbitraryEventIx      = eventIx
        }
    where
      genParams :: Era -> EpochNo -> Gen (EpochNo, HF.EraParams)
      genParams _era startOfThis = do
          params      <- genEraParams      startOfThis
          startOfNext <- genStartOfNextEra startOfThis params
          return (startOfNext, params)

  shrink c@ArbitraryChain{..} = concat [
        -- Simplify the system start
        [ resetArbitraryChainStart c
        | HF.getSystemStart arbitraryChainStart /= dawnOfTime
        ]

        -- Pick an earlier event
      , [ c { arbitraryEventIx = eventIx' }
        | eventIx' <- shrink arbitraryEventIx
        ]

        -- Shrink the chain
        -- We shrink the pre-chain, then re-generate the chain
      , [ ArbitraryChain{ arbitraryChain    = fromPreChain
                                                arbitraryChainEras
                                                preChain'
                        , arbitraryChainPre = preChain'
                        , ..
                        }
        | preChain' <- init (L.inits arbitraryChainPre)
        , arbitraryEventIx < length preChain'
        ]
      ]

{-------------------------------------------------------------------------------
  Chain model: Events
-------------------------------------------------------------------------------}

-- | We don't model a chain as a list of blocks, but rather as a list of events
data Event f =
    -- | Nothing of interest happens, time just ticks
    Tick

    -- | A new hard fork transition is confirmed
    --
    -- "Confirmed" here is taken to mean "no longer subject to rollback",
    -- which is the concept that the hard fork history depends on.
  | Confirm (f EpochNo)

deriving instance Show a => Show (Event (Const a))
deriving instance Show (Event Identity)

-- | When did an event occur?
--
-- NOTE: We don't care about 'BlockNo' here. Our events don't record necessarily
-- whether a block is actually present in a given slot or not.
data EventTime = EventTime {
      eventAbsSlot :: SlotNo
    , eventEpoch   :: EpochNo
    , eventRelSlot :: Word64
    , eventTime    :: UTCTime
    , eventEra     :: Era
    }
  deriving (Show)

initTime :: HF.SystemStart -> Era -> EventTime
initTime (HF.SystemStart start) firstEra = EventTime {
      eventAbsSlot = SlotNo  0
    , eventEpoch   = EpochNo 0
    , eventRelSlot = 0
    , eventTime    = start
    , eventEra     = firstEra
    }

-- | Next time slot
--
-- We assume the era does not change (this is done separately in 'genChain')
stepTime :: HF.EraParams -> EventTime -> EventTime
stepTime HF.EraParams{..} EventTime{..} = EventTime{
      eventAbsSlot = succ eventAbsSlot
    , eventEpoch   = epoch'
    , eventRelSlot = relSlot'
    , eventTime    = addUTCTime (getSlotLength eraSlotLength) eventTime
    , eventEra     = eventEra
    }
  where
    epoch'   :: EpochNo
    relSlot' :: Word64
    (epoch', relSlot') =
        if succ eventRelSlot == unEpochSize eraEpochSize
          then (succ eventEpoch, 0)
          else (eventEpoch, succ eventRelSlot)

{-------------------------------------------------------------------------------
  Chain model
-----------------------------------------------------------------------------}

-- | Chain divided into eras
--
-- Like 'Summary', we might not have blocks in the chain for all eras.
newtype Chain xs = Chain (AtMost xs (Events Identity))
  deriving (Show)

-- | Slot at the tip of the chain
chainTip :: Chain xs -> WithOrigin SlotNo
chainTip (Chain xs) = tip . reverse . concat . toList $ xs
  where
    tip :: [(EventTime, HF.EraParams, Event Identity)] -> WithOrigin SlotNo
    tip []            = Origin
    tip ((t, _, _):_) = At (eventAbsSlot t)

-- | Find all confirmed transitions in the chain
chainTransitions :: Eras xs -> Chain xs -> HF.Transitions xs
chainTransitions = \(Eras eras) (Chain chain) -> HF.Transitions $
    shift eras (uncurry findTransition <$> exactlyZipAtMost eras chain)
  where
    -- After mapping 'findTransition', for each era on the chain we have
    -- 'Maybe' a transition point. Those transition points have structure that
    -- we must recover here:
    --
    -- * The last era cannot have a transition point (i)
    -- * Unless it is the last era, the last era /on chain/ may or may
    --   not have a transition point (ii)
    -- * All other eras on chain /must/ have a transition point (iii)
    --
    -- We must also shift the type-level indices: we find the transition points
    -- in the eras that they occur /in/, but they must be associated with the
    -- eras that they transition /to/.
    shift :: Exactly (x ': xs) Era
          -> AtMost  (x ': xs) (Maybe EpochNo)
          -> AtMost        xs  EpochNo
    shift _ AtMostNil =
        -- No more transitions on the chain
        AtMostNil
    shift (ExactlyCons era ExactlyNil) (AtMostCons transition AtMostNil) =
        -- case (i)
        case transition of
          Nothing -> AtMostNil
          Just t  -> error $ concat [
                         "Unexpected transition "
                       , show t
                       , " in final era "
                       , show era
                       ]
    shift (ExactlyCons _ ExactlyCons{}) (AtMostCons transition AtMostNil) =
        -- case (ii)
        case transition of
          Nothing -> AtMostNil
          Just t  -> AtMostCons t AtMostNil
    shift (ExactlyCons era eras@(ExactlyCons{})) (AtMostCons transition ts) =
        -- case (iii)
        case transition of
          Nothing -> error $ concat [
                         "Missing transition in era "
                       , show era
                       ]
          Just t  -> AtMostCons t (shift eras ts)

-- | Locate transition point in a list of events
findTransition :: Era -> Events Identity -> Maybe EpochNo
findTransition era = mustBeUnique . catMaybes . map isTransition
  where
    mustBeUnique :: [EpochNo] -> Maybe EpochNo
    mustBeUnique []  = Nothing
    mustBeUnique [e] = Just e
    mustBeUnique _   = error $ "multiple transition points in " ++ show era

    isTransition :: (EventTime, HF.EraParams, Event Identity) -> Maybe EpochNo
    isTransition (_, _, Confirm (Identity e)) = Just e
    isTransition (_, _, Tick)                 = Nothing

patchEvents :: Events (Const ()) -> Events Identity
patchEvents events = map go events
  where
    bounds :: Map Era (EpochNo, EpochNo)
    bounds = eventsEraBounds events

    go :: (EventTime, HF.EraParams, Event (Const ()))
       -> (EventTime, HF.EraParams, Event Identity)
    go (time, params, event) = (time, params, event')
      where
        event' :: Event Identity
        event' =
          case event of
            Tick               -> Tick
            Confirm (Const ()) -> Confirm . Identity $ snd $
                                    bounds Map.! eventEra time

fromPreChain :: Eras xs -> PreChain -> Chain xs
fromPreChain (Eras eras) preChain = Chain $
    snd <$> exactlyZipAtMost eras grouped
  where
    grouped :: [Events Identity]
    grouped = L.groupBy ((==) `on` (\(t, _, _) -> eventEra t)) preChain

{-------------------------------------------------------------------------------
  Pre-chain
-------------------------------------------------------------------------------}

type Events f = [(EventTime, HF.EraParams, Event f)]
type PreChain = Events Identity

-- | Inclusive lower bound and exclusive upper bound for each era
eventsEraBounds :: Events (Const ()) -> Map Era (EpochNo, EpochNo)
eventsEraBounds []    = Map.empty
eventsEraBounds chain =
      Map.fromList
    . map bounds
    . L.groupBy ((==) `on` (\(t, _, _) -> eventEra t))
    $ chain
  where
    bounds :: Events (Const ()) -> (Era, (EpochNo, EpochNo))
    bounds era = (
          (\(t, _, _) -> eventEra t) $ head era
        , (minimum epochs, succ $ maximum epochs) -- Upper bound is exclusive
        )
      where
        epochs :: [EpochNo]
        epochs = map (\(t, _, _) -> eventEpoch t) era

genPreChain :: HF.SystemStart -> Eras xs -> HF.Shape xs -> Gen PreChain
genPreChain start eras shape = do
    events <- patchEvents <$> genEvents start eras shape
    n      <- choose (0, length events) -- See comment in 'genEvents'
    return $ take n events

-- | Generate events
--
-- It is important that this generates events for all eras (only the number of
-- events in the final era don't matter). If we don't do that, the bounds
-- computed by 'eventsEraBounds' are incorrect, and 'patchEvents' will be
-- do the wrong thing.
--
-- Therefore, to generate a shorter chain, we must generate a sufficient
-- number of events, then patch them, and only then can we take a prefix.
genEvents :: HF.SystemStart
          -> Eras     xs
          -> HF.Shape xs
          -> Gen (Events (Const ()))
genEvents = \start (Eras eras) (HF.Shape shape) -> sized $ \sz -> do
    eventsInFinalEra <- choose (0, fromIntegral sz)
    go eventsInFinalEra
       (initTime start (exactlyHead eras))
       NotYet
       (exactlyZip eras shape)
  where
    -- We first construct a 'PreChain' where we decide the moments of the
    -- /announcements/ of the transition points, but not yet the transition
    -- points themselves. We then patch up the chain as a post-processing step.
    go :: Word64         -- ^ Number of events in final era
       -> EventTime      -- ^ Current time
       -> CanStartNewEra
       -> Exactly (x ': xs) (Era, HF.EraParams)
       -> Gen (Events (Const ()))
    go 0 _   _        _     = return []
    go n now canStart shape = do
        case shape of
          ExactlyCons _ shape'@(ExactlyCons{}) | canStartNow && eventRelSlot now == 0 -> do
            shouldStartNow <- arbitrary
            if shouldStartNow
              then go' n now NotYet    shape'
              else go' n now canStart' shape
          _otherwise ->
            go' n now canStart' shape
      where
        (canStart', canStartNow) = stepCanStartNewEra (eventEpoch now) canStart

    -- After having decided whether or not to start a new era
    go' :: Word64
        -> EventTime
        -> CanStartNewEra
        -> Exactly (x ': xs) (Era, HF.EraParams)
        -> Gen (Events (Const ()))
    go' n now canStart shape@(ExactlyCons (era, params@HF.EraParams{..}) next) = do
        (event, canStart') <- frequency $ concat [
            [ (2, return (Tick, canStart)) ]

            -- Avoid starting a count-down if another one is already in process
            -- The final era cannot contain any transitions
          , [ (1, return (Confirm (Const ()), CanStartIf eraSafeZone))
            | NotYet          <- [canStart]
            , ExactlyCons _ _ <- [next]
            ]
          ]
        ((now { eventEra = era }, params, event) :) <$>
          go n' (stepTime params now) canStart' shape
      where
        n' = if eraIsLast era
               then n - 1
               else n

data CanStartNewEra =
    -- | The announcement of the transition hasn't yet happened
    NotYet

    -- | The announcement has happened
    --
    -- We must observe the 'SafeZone'
  | CanStartIf HF.SafeZone

stepCanStartNewEra :: EpochNo -> CanStartNewEra -> (CanStartNewEra, Bool)
stepCanStartNewEra _            NotYet                = (NotYet, False)
stepCanStartNewEra currentEpoch (CanStartIf safeZone) =
    case HF.safeFromTip safeZone of
      0 -> (CanStartIf safeZone, checkEpoch (HF.safeBeforeEpoch safeZone))
      n -> (CanStartIf safeZone { HF.safeFromTip = n - 1 }, False)
  where
    checkEpoch :: HF.SafeBeforeEpoch -> Bool
    checkEpoch HF.NoLowerBound   = True
    checkEpoch (HF.LowerBound e) = currentEpoch >= e

{-------------------------------------------------------------------------------
  Arbitrary 'Summary'

  We should be able to show properties of the conversion functions independent
  of how the 'Summary' that they use is derived.
-------------------------------------------------------------------------------}

data ArbitrarySummary = forall xs. ArbitrarySummary {
      arbitrarySummaryStart :: HF.SystemStart
    , arbitrarySummary      :: HF.Summary xs
    , beforeHorizonTime     :: UTCTime
    , beforeHorizonSlot     :: SlotNo
    , beforeHorizonEpoch    :: EpochNo
    , pastHorizonTime       :: UTCTime
    , pastHorizonSlot       :: SlotNo
    , pastHorizonEpoch      :: EpochNo
    }

deriving instance Show ArbitrarySummary

instance Arbitrary ArbitrarySummary where
  arbitrary = chooseEras $ \is -> do
      start   <- HF.SystemStart <$> arbitrary
      summary <- genSummary genEraSummary (HF.initBound start) is

      let summaryStart, summaryEnd :: HF.Bound
          (summaryStart, summaryEnd) = summaryBounds summary

          summarySlots, summaryEpochs :: Word64
          summarySlots  = HF.countSlots
                            (HF.boundSlot summaryEnd)
                            (HF.boundSlot summaryStart)
          summaryEpochs = HF.countEpochs
                            (HF.boundEpoch summaryEnd)
                            (HF.boundEpoch summaryStart)

          summaryTimeSpan :: NominalDiffTime
          summaryTimeSpan = diffUTCTime
                              (HF.boundTime summaryEnd)
                              (HF.boundTime summaryStart)

          summaryTimeSpanSeconds :: Double
          summaryTimeSpanSeconds = realToFrac summaryTimeSpan

      -- Pick arbitrary values before the horizon

      beforeHorizonSlots   <- choose (0, summarySlots  - 1)
      beforeHorizonEpochs  <- choose (0, summaryEpochs - 1)
      beforeHorizonSeconds <- choose (0, summaryTimeSpanSeconds)
                                `suchThat` \x -> x /= summaryTimeSpanSeconds

      let beforeHorizonSlot  :: SlotNo
          beforeHorizonEpoch :: EpochNo
          beforeHorizonTime  :: UTCTime

          beforeHorizonSlot  = HF.addSlots
                                 beforeHorizonSlots
                                 (HF.boundSlot summaryStart)
          beforeHorizonEpoch = HF.addEpochs
                                 beforeHorizonEpochs
                                 (HF.boundEpoch summaryStart)
          beforeHorizonTime  = addUTCTime
                                 (realToFrac beforeHorizonSeconds)
                                 (HF.boundTime summaryStart)

      -- Pick arbitrary values past the horizon

      pastHorizonSlots   :: Word64 <- choose (0, 10)
      pastHorizonEpochs  :: Word64 <- choose (0, 10)
      pastHorizonSeconds :: Double <- choose (0, 10)

      let pastHorizonSlot  :: SlotNo
          pastHorizonEpoch :: EpochNo
          pastHorizonTime  :: UTCTime

          pastHorizonSlot  = HF.addSlots
                                pastHorizonSlots
                                (HF.boundSlot summaryEnd)
          pastHorizonEpoch = HF.addEpochs
                                pastHorizonEpochs
                                (HF.boundEpoch summaryEnd)
          pastHorizonTime  = addUTCTime
                                (realToFrac pastHorizonSeconds)
                                (HF.boundTime summaryEnd)

      return ArbitrarySummary{
            arbitrarySummaryStart = start
          , arbitrarySummary      = summary
          , beforeHorizonTime
          , beforeHorizonSlot
          , beforeHorizonEpoch
          , pastHorizonTime
          , pastHorizonSlot
          , pastHorizonEpoch
          }
    where
      genEraSummary :: Era -> HF.Bound -> Gen (HF.Bound, HF.EraSummary)
      genEraSummary _era lo = do
          params <- genEraParams (HF.boundEpoch lo)
          hi     <- genUpperBound lo params
          return (hi, HF.EraSummary lo hi params)

      genUpperBound :: HF.Bound -> HF.EraParams -> Gen HF.Bound
      genUpperBound lo params = do
          startOfNextEra <- genStartOfNextEra (HF.boundEpoch lo) params
          return $ HF.mkUpperBound params lo startOfNextEra

  shrink summary@ArbitrarySummary{..} = concat [
        -- Simplify the system start
        [ resetArbitrarySummaryStart summary
        | HF.getSystemStart arbitrarySummaryStart /= dawnOfTime
        ]

        -- Reduce before-horizon slot
      , [ summary { beforeHorizonSlot = SlotNo s }
        | s <- shrink (unSlotNo beforeHorizonSlot)
        ]

        -- Reduce before-horizon epoch
      , [ summary { beforeHorizonEpoch = EpochNo e }
        | e <- shrink (unEpochNo beforeHorizonEpoch)
        ]

        -- Reduce before-horizon time
      , [ summary { beforeHorizonTime = t }
        | t <- shrink beforeHorizonTime
        , t >= HF.getSystemStart arbitrarySummaryStart
        ]

        -- Drop an era /provided/ this doesn't cause of any of the before
        -- horizon values to become past horizon
      , [ ArbitrarySummary { arbitrarySummary = summary', .. }
        | Just (summary', lastEra) <- [summaryInit arbitrarySummary]
        , beforeHorizonSlot  < HF.boundSlot  (HF.eraStart lastEra)
        , beforeHorizonEpoch < HF.boundEpoch (HF.eraStart lastEra)
        , beforeHorizonTime  < HF.boundTime  (HF.eraStart lastEra)
        ]
      ]

-- | Generate 'EpochNo' for the start of the next era
genStartOfNextEra :: EpochNo ->  HF.EraParams -> Gen EpochNo
genStartOfNextEra startOfEra HF.EraParams{..} =
    case HF.safeBeforeEpoch eraSafeZone of
       HF.LowerBound e -> (\n -> HF.addEpochs n e         ) <$> choose (0, 10)
       HF.NoLowerBound -> (\n -> HF.addEpochs n startOfEra) <$> choose (1, 10)

-- | Generate era parameters
--
-- Need to know the start of the era to generate a valid 'HF.LowerBound'.
-- We do /not/ assume that the 'safeFromTip' must be less than an epoch.
genEraParams :: EpochNo -> Gen HF.EraParams
genEraParams startOfEra = do
    eraEpochSize  <- EpochSize         <$> choose (1, 10)
    eraSlotLength <- slotLengthFromSec <$> choose (1, 5)
    eraSafeZone   <- genSafeZone
    return HF.EraParams{..}
  where
    genSafeZone :: Gen HF.SafeZone
    genSafeZone = do
        safeFromTip     <- choose (1, 10)
        safeBeforeEpoch <- genSafeBeforeEpoch
        return HF.SafeZone{..}

    genSafeBeforeEpoch :: Gen HF.SafeBeforeEpoch
    genSafeBeforeEpoch = oneof [
          return HF.NoLowerBound
        , (\n -> HF.LowerBound (HF.addEpochs n startOfEra)) <$> choose (1, 5)
        ]

{-------------------------------------------------------------------------------
  Auxiliary: generate arbitrary type-level indices
-------------------------------------------------------------------------------}

data Era = Era {
       eraIndex  :: Word64
     , eraIsLast :: Bool
     }
  deriving (Show, Eq, Ord)

data Eras :: [*] -> * where
    -- We guarantee to have at least one era
    Eras :: Exactly (x ': xs) Era -> Eras (x ': xs)

deriving instance Show (Eras xs)

chooseEras :: (forall xs. Eras xs -> Gen a) -> Gen a
chooseEras k = oneof [
      k $ Eras $ exactlyOne  (Era 0 True)
    , k $ Eras $ ExactlyCons (Era 0 False) $ exactlyOne  (Era 1 True)
    , k $ Eras $ ExactlyCons (Era 0 False) $ ExactlyCons (Era 1 False) $ exactlyOne  (Era 2 True)
    , k $ Eras $ ExactlyCons (Era 0 False) $ ExactlyCons (Era 1 False) $ ExactlyCons (Era 2 False) $ exactlyOne (Era 3 True)
    ]

{-------------------------------------------------------------------------------
  Auxiliary: using 'Eras' to generate indexed types
-------------------------------------------------------------------------------}

genSummary :: forall m xs a. Monad m
           => (Era -> a -> m (a, HF.EraSummary))  -- ^ Step
           -> a                                   -- ^ Initial seed
           -> Eras xs -> m (HF.Summary xs)
genSummary f = \seed (Eras eras) ->
    HF.Summary <$> go seed eras
  where
    go :: forall xs'. a -> Exactly xs' Era -> m (AtMost xs' HF.EraSummary)
    go _  ExactlyNil        = return AtMostNil
    go a (ExactlyCons e es) = do
        (a', eraSummary) <- f e a
        summary          <- go a' es
        return $ AtMostCons eraSummary summary

genShape :: forall m xs a. Monad m
         => (Era -> a -> m (a, HF.EraParams))  -- ^ Step
         -> a                                  -- ^ Initial seed
         -> Eras xs -> m (HF.Shape xs)
genShape f = \seed (Eras eras) ->
    HF.Shape <$> go seed eras
  where
    go :: forall xs'. a -> Exactly xs' Era -> m (Exactly xs' HF.EraParams)
    go _  ExactlyNil        = return ExactlyNil
    go a (ExactlyCons e es) = do
        (a', eraParams) <- f e a
        shape           <- go a' es
        return $ ExactlyCons eraParams shape

{-------------------------------------------------------------------------------
  Shifting time
-------------------------------------------------------------------------------}

instance ShiftTime ArbitrarySummary where
  shiftTime delta ArbitrarySummary{..} = ArbitrarySummary {
      arbitrarySummaryStart = shiftTime delta arbitrarySummaryStart
    , arbitrarySummary      = shiftTime delta arbitrarySummary
    , beforeHorizonTime     = shiftTime delta beforeHorizonTime
    , beforeHorizonSlot
    , beforeHorizonEpoch
    , pastHorizonTime
    , pastHorizonSlot
    , pastHorizonEpoch
    }

instance ShiftTime ArbitraryChain where
  shiftTime delta ArbitraryChain{..} = ArbitraryChain {
        arbitraryChainStart = shiftTime delta arbitraryChainStart
      , arbitraryChain      = shiftTime delta arbitraryChain
      , arbitraryChainPre   = shiftTime delta arbitraryChainPre
      , arbitraryChainEras
      , arbitraryChainShape
      , arbitraryEventIx
      }

instance ShiftTime (Chain xs) where
  shiftTime delta (Chain chain) = Chain $ shiftTime delta <$> chain

instance ShiftTime (EventTime, HF.EraParams, Event f) where
  shiftTime delta (time, params, event) = (shiftTime delta time, params, event)

instance ShiftTime EventTime where
  shiftTime delta t = t { eventTime = shiftTime delta (eventTime t) }

-- | Reset the system start (during shrinking)
--
-- Since this brings the system start /back/, we don't care about the past
-- horizon values. 'EpochNo' and 'SlotNo' are also unaffected.
resetArbitrarySummaryStart :: ArbitrarySummary -> ArbitrarySummary
resetArbitrarySummaryStart summary@ArbitrarySummary{..} =
    shiftTime (negate ahead) summary
  where
    ahead :: NominalDiffTime
    ahead = diffUTCTime (HF.getSystemStart arbitrarySummaryStart) dawnOfTime

-- | Reset chain start
resetArbitraryChainStart :: ArbitraryChain -> ArbitraryChain
resetArbitraryChainStart chain@ArbitraryChain{..} =
    shiftTime (negate ahead) chain
  where
    ahead :: NominalDiffTime
    ahead = diffUTCTime (HF.getSystemStart arbitraryChainStart) dawnOfTime

{-------------------------------------------------------------------------------
  Additional functions on 'Summary' needed for the tests only
-------------------------------------------------------------------------------}

-- | Lift 'atMostInit' to 'Summary'
summaryInit :: HF.Summary xs -> Maybe (HF.Summary xs, HF.EraSummary)
summaryInit (HF.Summary xs) = first HF.Summary <$> atMostInit xs

-- | Outer bounds of the summary
--
-- We must always have at least one era but the 'Summary' type does not tell
-- us that. This function is therefore partial, but used only in the tests.
summaryBounds :: HF.Summary xs -> (HF.Bound, HF.Bound)
summaryBounds (HF.Summary summary) =
    case toList summary of
      [] -> error "summaryBounds: no eras"
      ss -> (HF.eraStart (head ss), HF.eraEnd (last ss))
