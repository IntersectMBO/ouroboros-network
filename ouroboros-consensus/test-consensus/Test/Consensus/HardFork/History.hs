{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}

module Test.Consensus.HardFork.History (tests) where

import           Control.Exception (throw)
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.Functor.Identity
import qualified Data.List as L
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.SOP.Strict hiding (shape, shift)
import           Data.Time
import           Data.Word

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Forecast
import qualified Ouroboros.Consensus.HardFork.History as HF
import           Ouroboros.Consensus.Util (nTimes)
import           Ouroboros.Consensus.Util.Counting
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope (..))

import           Test.Consensus.HardFork.Infra
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.QuickCheck

-- | Tests for 'summarize'
--
-- General approach:
--
-- * Generate a chain of events
-- * Each event records its own 'RelativeTime', 'SlotNo', and 'EpochNo'
-- * We then construct a 'HF.Summary' from a /prefix/ of this chain
-- * We then pick an arbitrary event from the (full) chain:
--   a. If that event is on the prefix of the chain, or within the safe zone, we
--      expect to be able to do any slot/epoch or slot/time conversion, and we
--      can easily verify the result by comparing it to the values the 'Event'
--      itself reports.
--   b. If the event is outside of safe zone, we expect the conversion to throw
--      a 'PastHorizonException'.
tests :: TestTree
tests = testGroup "HardForkHistory" [
      testGroup "Chain" [
          testGroup "Sanity" [
              testProperty "generator" $ checkGenerator $ \ArbitraryChain{..} ->
                let ArbitraryParams{..} = arbitraryParams in
                checkInvariant HF.invariantShape arbitraryChainShape
            , testProperty "shrinker"  $ checkShrinker $ \ArbitraryChain{..} ->
                let ArbitraryParams{..} = arbitraryParams in
                checkInvariant HF.invariantShape arbitraryChainShape
            ]
        , testGroup "Conversions" [
              testProperty "summarizeInvariant"   summarizeInvariant
            , testProperty "eventSlotToEpoch"     eventSlotToEpoch
            , testProperty "eventEpochToSlot"     eventEpochToSlot
            , testProperty "eventSlotToWallclock" eventSlotToWallclock
            , testProperty "eventWallclockToSlot" eventWallclockToSlot
            , testProperty "epochInfoSlotToEpoch" epochInfoSlotToEpoch
            , testProperty "epochInfoEpochToSlot" epochInfoEpochToSlot
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Dealing with the 'PastHorizonException'
-------------------------------------------------------------------------------}

isPastHorizonIf :: Show a
                => Bool -- ^ Are we expecting an exception?
                -> Either HF.PastHorizonException a
                -> (a -> Property)
                -> Property
isPastHorizonIf True  (Left _)  _ = property True
isPastHorizonIf False (Right a) p = p a
isPastHorizonIf False (Left ex) _ =
    counterexample ("Unexpected exception " ++ show ex) $
      property False
isPastHorizonIf True (Right a)  _ =
    counterexample ("Unexpected value " ++ show a
                ++ " (expected PastHorizonException)"
                   ) $
      property False

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
summarizeInvariant ArbitraryChain{..} =
    checkInvariant HF.invariantSummary arbitrarySummary

testSkeleton :: Show a
             => ArbitraryChain
             -> HF.Qry a
             -> (a -> Property)
             -> Property
testSkeleton ArbitraryChain{..} q =
      tabulate "arbitraryEventIx" [eventIxType arbitraryEventIx]
    . isPastHorizonIf
        (not $ eventIsPreHorizon arbitraryEventIx)
        (HF.runQuery q arbitrarySummary)

eventSlotToEpoch :: ArbitraryChain -> Property
eventSlotToEpoch chain@ArbitraryChain{..} =
    testSkeleton chain (HF.slotToEpoch eventTimeSlot) $
      \(epochNo, epochSlot, slotsLeft) -> conjoin [
          epochNo               === eventTimeEpochNo
        , epochSlot             === eventTimeEpochSlot
        , epochSlot + slotsLeft === (unEpochSize . HF.eraEpochSize $
                                       eventEraParams arbitraryEvent)
        ]
  where
    EventTime{..} = eventTime arbitraryEvent

eventEpochToSlot :: ArbitraryChain -> Property
eventEpochToSlot chain@ArbitraryChain{..} =
    testSkeleton chain (HF.epochToSlot eventTimeEpochNo) $
      \(startOfEpoch, epochSize) -> conjoin [
         eventTimeSlot === HF.addSlots eventTimeEpochSlot startOfEpoch
       , eventTimeEpochSlot `lt` unEpochSize epochSize
       ]
  where
    EventTime{..} = eventTime arbitraryEvent

eventSlotToWallclock :: ArbitraryChain -> Property
eventSlotToWallclock chain@ArbitraryChain{..} =
    testSkeleton chain (HF.slotToWallclock eventTimeSlot) $
      \(time, _slotLen) -> conjoin [
          time === eventTimeRelative
        ]
  where
    EventTime{..} = eventTime arbitraryEvent

eventWallclockToSlot :: ArbitraryChain -> Property
eventWallclockToSlot chain@ArbitraryChain{..} =
    testSkeleton chain (HF.wallclockToSlot time) $
      \(slot, inSlot, timeSpent) -> conjoin [
          slot               === eventTimeSlot
        , inSlot             === diff
        , inSlot + timeSpent === (getSlotLength . HF.eraSlotLength $
                                    eventEraParams arbitraryEvent)
        ]
  where
    EventTime{..} = eventTime arbitraryEvent

    time :: RelativeTime
    time = addRelTime diff eventTimeRelative

    diff :: NominalDiffTime
    diff = arbitraryDiffTime arbitraryParams

{-------------------------------------------------------------------------------
  Tests using EpochInfo

  NOTE: We have two degrees of freedom here: we can ask for an 'EpochInfo' for a
  particular slot, and then we can use that 'EpochInfo' for another slot. We
  don't try to be exhaustive here: we use the 'SlotNo' of the event that we
  choose for both.

  TODO: Given time, we should make these tests more thorough.
-------------------------------------------------------------------------------}

epochInfoSlotToEpoch :: ArbitraryChain -> Property
epochInfoSlotToEpoch chain@ArbitraryChain{..} =
        counterexample ("view: " ++ view)
      $ counterexample ("reconstructed: " ++ reconstructed)
      $ eventIsPreHorizon arbitraryEventIx
    ==> runIdentity (epochInfoEpoch epochInfo eventTimeSlot)
    === eventTimeEpochNo
  where
    EventTime{..}     = eventTime arbitraryEvent
    (epochInfo, view, reconstructed) = hardForkEpochInfo chain eventTimeSlot

epochInfoEpochToSlot :: ArbitraryChain -> Property
epochInfoEpochToSlot chain@ArbitraryChain{..} =
        counterexample ("view: " ++ view)
      $ counterexample ("reconstructed: " ++ reconstructed)
      $ eventIsPreHorizon arbitraryEventIx
    ==> let startOfEpoch = runIdentity (epochInfoFirst epochInfo eventTimeEpochNo)
        in counterexample ("startOfEpoch: " ++ show startOfEpoch) $
                 HF.addSlots eventTimeEpochSlot startOfEpoch
             === eventTimeSlot
  where
    EventTime{..} = eventTime arbitraryEvent
    (epochInfo, view, reconstructed) = hardForkEpochInfo chain eventTimeSlot

{-------------------------------------------------------------------------------
  Arbitrary chain
-------------------------------------------------------------------------------}

data ArbitraryParams xs = ArbitraryParams {
      arbitraryChainEvents :: [Event]
    , arbitraryChainEras   :: Eras     xs
    , arbitraryChainShape  :: HF.Shape xs

      -- | Index into the events
      --
      -- > 0 <= arbitraryEventIx < length arbitraryChainEvents
      --
      -- The tests will use 'arbitraryEventIx' instead.
    , arbitraryRawEventIx  :: Int

      -- | Split of the prechain
      --
      -- > 0 <= arbitraryChainSplit < length arbitraryChainEvents
    , arbitraryChainSplit  :: Int

      -- | Arbitrary 'DiffTime'
      --
      -- Let @s@ be the slot length of the selected event. Then
      --
      -- 0 <= arbitraryDiffTime < s
    , arbitraryDiffTime    :: NominalDiffTime
    }
  deriving (Show)

data ArbitraryChain = forall xs. (SListI xs, IsNonEmpty xs) => ArbitraryChain {
      -- | QuickCheck generated parameters
      --
      -- The rest of these values are derived
      arbitraryParams      :: ArbitraryParams xs

      -- | Chain derived from a prefix of the prechain
    , arbitraryChain       :: Chain xs

      -- | Transitions on the chain
    , arbitraryTransitions :: HF.Transitions xs

      -- | Summary of the chain
    , arbitrarySummary     :: HF.Summary xs

      -- | Active safe zone
    , arbitrarySafeZone    :: (Maybe EpochNo, HF.SafeZone)

      -- | Events after the chain, but within the safe zone
    , arbitraryInSafeZone  :: [Event]

      -- | Events after the chain, no longer within the safe zone
    , arbitraryPastHorizon :: [Event]

      -- | Event index into one of the three parts of the chain
    , arbitraryEventIx     :: EventIx

      -- | Arbitrary event
      --
      -- This is equal to both of
      --
      -- > arbitraryChainEvents !! arbitraryRawEventIx
    , arbitraryEvent       :: Event
    }

data EventIx =
    -- > 0 <= n < length (concat (toList arbitraryChain))
    EventOnChain Int

    -- > 0 <= n < length arbitrarySafeZone
    -- The 'Bool' indicates if this is the very last entry in the safe zone
  | EventInSafeZone Int Bool

    -- > 0 <= n < length arbitraryPastHorizon
  | EventPastHorizon Int
  deriving (Show)

eventIxType :: EventIx -> String
eventIxType (EventOnChain     _      ) = "on chain"
eventIxType (EventInSafeZone  _ False) = "in safe zone"
eventIxType (EventInSafeZone  _ True ) = "last in safe zone"
eventIxType (EventPastHorizon _      ) = "past horizon"

eventIsPreHorizon :: EventIx -> Bool
eventIsPreHorizon (EventOnChain     _  ) = True
eventIsPreHorizon (EventInSafeZone  _ _) = True
eventIsPreHorizon (EventPastHorizon _  ) = False

-- | Fill in the derived parts of the 'ArbitraryChain'
mkArbitraryChain :: forall xs. (SListI xs, IsNonEmpty xs)
                 => ArbitraryParams xs -> ArbitraryChain
mkArbitraryChain params@ArbitraryParams{..} = ArbitraryChain {
      arbitraryParams      = params
    , arbitraryChain       = chain
    , arbitraryTransitions = transitions
    , arbitrarySummary     = summary
    , arbitrarySafeZone    = safeZone
    , arbitraryInSafeZone  = inSafeZone
    , arbitraryPastHorizon = pastHorizon
    , arbitraryEventIx     = mkEventIx arbitraryRawEventIx
    , arbitraryEvent       = arbitraryChainEvents !! arbitraryRawEventIx
    }
  where
    (beforeSplit, afterSplit) = splitAt arbitraryChainSplit arbitraryChainEvents
    safeZone                  = activeSafeZone
                                  arbitraryChainShape
                                  chain
                                  transitions
    (inSafeZone, pastHorizon) = splitSafeZone
                                  (fst <$> chainTip chain)
                                  safeZone
                                  afterSplit

    chain :: Chain xs
    chain = fromEvents arbitraryChainEras beforeSplit

    transitions :: HF.Transitions xs
    transitions = chainTransitions arbitraryChainEras chain

    summary :: HF.Summary xs
    summary = HF.summarize
                (snd <$> chainTip chain)
                arbitraryChainShape
                transitions

    mkEventIx :: Int -> EventIx
    mkEventIx n
      | n   < length beforeSplit = EventOnChain     n
      | n'  < length inSafeZone  = EventInSafeZone  n' (n' + 1 == length inSafeZone)
      | n'' < length pastHorizon = EventPastHorizon n''
      | otherwise = error $ concat [
            "mkEventIx: index "
          , show n
          , " out of bounds "
          , show (length beforeSplit, length inSafeZone, length pastHorizon)
          , "\nparameters:  " ++ show params
          , "\nbeforeSplit: " ++ show beforeSplit
          , "\nafterSplit:  " ++ show afterSplit
          , "\nsafeZone:    " ++ show safeZone
          , "\ninSafeZone:  " ++ show inSafeZone
          , "\npastHorizon: " ++ show pastHorizon
          ]
      where
        n'  = n  - length beforeSplit
        n'' = n' - length inSafeZone

deriving instance Show ArbitraryChain

instance Arbitrary ArbitraryChain where
  arbitrary = chooseEras $ \eras -> do
      shape  <- HF.Shape <$> erasMapStateM genParams eras (EpochNo 0)
      events <- genEvents eras shape `suchThat` (not . null)
      split  <- choose (0, length events - 1)
      rawIx  <- choose (0, length events - 1)
      diff   <- genDiffTime $ HF.eraSlotLength (eventEraParams (events !! rawIx))
      return $ mkArbitraryChain $ ArbitraryParams {
          arbitraryChainEvents = events
        , arbitraryChainEras   = eras
        , arbitraryChainShape  = shape
        , arbitraryRawEventIx  = rawIx
        , arbitraryChainSplit  = split
        , arbitraryDiffTime    = diff
        }
    where
      genParams :: Era -> EpochNo -> Gen (HF.EraParams, EpochNo)
      genParams _era startOfThis = do
          params      <- genEraParams      startOfThis
          startOfNext <- genStartOfNextEra startOfThis params
          -- If startOfNext is 'Nothing', we used 'UnsafeUnbounded' for this
          -- era. This means we should not be generating any events for any
          -- succeeding eras, but to determine the /shape/ of the eras, and
          -- set subsequent lower bounds, we just need to make sure that we
          -- generate a valid shape: the next era must start after this one.
          return (params, fromMaybe (succ startOfThis) startOfNext)

      genDiffTime :: SlotLength -> Gen NominalDiffTime
      genDiffTime s = realToFrac <$> choose (0, s') `suchThat` (/= s')
        where
          s' :: Double
          s' = fromIntegral $ slotLengthToSec s

  shrink ArbitraryChain{..} = concat [
        -- Pick an earlier event
        [ mkArbitraryChain $ arbitraryParams { arbitraryRawEventIx = rawIx' }
        | rawIx' <- shrink arbitraryRawEventIx
        ]

        -- Pick an earlier split
      , [ mkArbitraryChain $ arbitraryParams { arbitraryChainSplit = split' }
        | split' <- shrink arbitraryChainSplit
        ]

        -- Shrink the chain by taking a prefix
        -- (The standard shrinker for lists does not make sense for chains)
      , [ mkArbitraryChain $ arbitraryParams { arbitraryChainEvents = events' }
        | events' <- init (L.inits arbitraryChainEvents)
        , arbitraryRawEventIx < length events'
        , arbitraryChainSplit < length events'
        ]
      ]
    where
      ArbitraryParams{..} = arbitraryParams

{-------------------------------------------------------------------------------
  Chain model: Events
-------------------------------------------------------------------------------}

-- | We don't model a chain as a list of blocks, but rather as a list of events
--
-- Unlike blocks, events are not subject to rollback.
data Event = Event {
      eventType      :: EventType
    , eventTime      :: EventTime
    , eventEra       :: Era
    , eventEraParams :: HF.EraParams
    }
  deriving (Show)

data EventType =
    -- | Nothing of interest happens, time just ticks
    Tick

    -- | A new hard fork transition is confirmed
    --
    -- "Confirmed" here is taken to mean "no longer subject to rollback",
    -- which is the concept that the hard fork history depends on.
  | Confirm EpochNo
  deriving (Show)

-- | When did an event occur?
--
-- NOTE: We don't care about 'BlockNo' here. Our events don't record necessarily
-- whether a block is actually present in a given slot or not.
data EventTime = EventTime {
      eventTimeSlot      :: SlotNo
    , eventTimeEpochNo   :: EpochNo
    , eventTimeEpochSlot :: Word64
    , eventTimeRelative  :: RelativeTime
    }
  deriving (Show)

initEventTime :: EventTime
initEventTime = EventTime {
      eventTimeSlot      = SlotNo  0
    , eventTimeEpochNo   = EpochNo 0
    , eventTimeEpochSlot = 0
    , eventTimeRelative  = RelativeTime 0
    }

-- | Next time slot
stepEventTime :: HF.EraParams -> EventTime -> EventTime
stepEventTime HF.EraParams{..} EventTime{..} = EventTime{
      eventTimeSlot      = succ eventTimeSlot
    , eventTimeEpochNo   = epoch'
    , eventTimeEpochSlot = relSlot'
    , eventTimeRelative  = addRelTime (getSlotLength eraSlotLength) $
                             eventTimeRelative
    }
  where
    epoch'   :: EpochNo
    relSlot' :: Word64
    (epoch', relSlot') =
        if succ eventTimeEpochSlot == unEpochSize eraEpochSize
          then (succ eventTimeEpochNo, 0)
          else (eventTimeEpochNo, succ eventTimeEpochSlot)

{-------------------------------------------------------------------------------
  Chain model
-----------------------------------------------------------------------------}

-- | Chain divided into eras
--
-- Like 'Summary', we might not have blocks in the chain for all eras.
-- The chain might be empty, but we must at least have one era.
newtype Chain xs = Chain (NonEmpty xs [Event])
  deriving (Show)

-- | Slot at the tip of the chain
chainTip :: Chain xs -> WithOrigin (EpochNo, SlotNo)
chainTip (Chain xs) = tip . reverse . concat . toList $ xs
  where
    tip :: [Event] -> WithOrigin (EpochNo, SlotNo)
    tip []    = Origin
    tip (e:_) = At (eventTimeEpochNo (eventTime e), eventTimeSlot (eventTime e))

-- | Find all confirmed transitions in the chain
chainTransitions :: Eras xs -> Chain xs -> HF.Transitions xs
chainTransitions = \(Eras eras) (Chain chain) -> HF.Transitions $
    shift eras (uncurry findTransition <$> exactlyZipFoldable eras chain)
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
    shift (K era :* Nil) (AtMostCons transition AtMostNil) =
        -- case (i)
        case transition of
          Nothing -> AtMostNil
          Just t  -> error $ concat [
                         "Unexpected transition "
                       , show t
                       , " in final era "
                       , show era
                       ]
    shift (_ :* (_ :* _)) (AtMostCons transition AtMostNil) =
        -- case (ii)
        case transition of
          Nothing -> AtMostNil
          Just t  -> AtMostCons t AtMostNil
    shift (K era :* eras@(_ :* _)) (AtMostCons transition ts) =
        -- case (iii)
        case transition of
          Nothing -> error $ concat [
                         "Missing transition in era "
                       , show era
                       ]
          Just t  -> AtMostCons t (shift eras ts)

-- | Locate transition point in a list of events
findTransition :: Era -> [Event] -> Maybe EpochNo
findTransition era =
    mustBeUnique . catMaybes . map (isTransition . eventType)
  where
    mustBeUnique :: [EpochNo] -> Maybe EpochNo
    mustBeUnique []  = Nothing
    mustBeUnique [e] = Just e
    mustBeUnique _   = error $ "multiple transition points in " ++ show era

    isTransition :: EventType -> Maybe EpochNo
    isTransition (Confirm e) = Just e
    isTransition Tick        = Nothing

fromEvents :: Eras xs -> [Event] -> Chain xs
fromEvents (Eras eras) events = Chain $
    fromMaybe (NonEmptyOne []) . atMostNonEmpty . fmap snd $
      exactlyZipFoldable eras grouped
  where
    grouped :: [[Event]]
    grouped = L.groupBy ((==) `on` eventEra) events

{-------------------------------------------------------------------------------
  Generate events
-------------------------------------------------------------------------------}

-- | Time used during event generation
data Time = forall x xs. Time {
      timeEvent   :: EventTime
    , timeNextEra :: Maybe EpochNo -- ^ Start of the epoch (if already decided)
    , timeEras    :: Exactly (x ': xs) (Era, HF.EraParams)
    }

stepTime :: EventType -> Time -> Time
stepTime typ Time{..} =
    case (typ, timeNextEra, exactlyTail timeEras) of
      (Tick, Nothing, _) ->
        Time timeEvent' Nothing timeEras
      (Tick, Just e, timeEras'@(_ :* _)) | reachedNextEra e ->
        Time timeEvent' Nothing timeEras'
      (Tick, Just e, Nil) | reachedNextEra e ->
        error "stepTime: unexpected confirmation in final era"
      (Tick, Just e, _) -> -- not (reachedNextEra e)
        Time timeEvent' (Just e) timeEras
      (Confirm _, Just _, _) ->
        error "stepTime: unexpected double confirmation"
      (Confirm e, Nothing, _) ->
        Time timeEvent' (Just e) timeEras
  where
    timeEvent' :: EventTime
    timeEvent' = stepEventTime (snd (exactlyHead timeEras)) timeEvent

    reachedNextEra :: EpochNo -> Bool
    reachedNextEra e = eventTimeEpochNo timeEvent' == e

genEvents :: Eras xs -> HF.Shape xs -> Gen [Event]
genEvents = \(Eras eras) (HF.Shape shape) -> sized $ \sz -> do
    go sz Time {
        timeEvent   = initEventTime
      , timeNextEra = Nothing
      , timeEras    = exactlyZip eras shape
      }
  where
    go :: Int -> Time -> Gen [Event]
    go 0 _             = return []
    go n time@Time{..} = do
        typ <- frequency $ concat [
            [(2, return Tick)]
          , case canTransition of
              Nothing        -> []
              Just pickStart -> [(1, Confirm <$> pickStart)]
          ]
        let event = Event {
                eventType      = typ
              , eventTime      = timeEvent
              , eventEra       = era
              , eventEraParams = eraParams
              }
        (event:) <$> go (n - 1) (stepTime typ time)
      where
        era       :: Era
        eraParams :: HF.EraParams
        (era, eraParams) = exactlyHead timeEras

        canTransition :: Maybe (Gen EpochNo)
        canTransition
          | Just _ <- timeNextEra =
              -- We already generated a transition
              Nothing
          | Nil <- exactlyTail timeEras =
              -- We are in the final era
              Nothing
          | Nothing <- mNextLo =
              -- This era is 'UnsafeUnbounded'
             Nothing
          | Just lo <- mNextLo =
              Just (pickStartOfNextEra lo)

        -- Lower bound on the start of the next era
        mNextLo :: Maybe EpochNo
        mNextLo =
            HF.maxMaybeEpoch
              (HF.safeBeforeEpoch (HF.eraSafeZone eraParams))
              (if eventTimeEpochSlot afterSafeZone == 0
                 then eventTimeEpochNo afterSafeZone
                 else eventTimeEpochNo afterSafeZone + 1)
          where
            -- The 'EventTime' of the first event after the safe zone
            -- (The @+ 1@ here is required because the first step is to skip
            -- over the 'Confirm' itself)
            afterSafeZone :: EventTime
            afterSafeZone = nTimes
                              (stepEventTime eraParams)
                              (HF.safeFromTip (HF.eraSafeZone eraParams) + 1)
                              timeEvent

        pickStartOfNextEra :: EpochNo -> Gen EpochNo
        pickStartOfNextEra lo = (\d -> HF.addEpochs d lo) <$> choose (0, 10)

{-------------------------------------------------------------------------------
  Safe zone
-------------------------------------------------------------------------------}

-- | The safe zone active at the end of the chain
--
-- If the transition to the next era is known, we specify the epoch number of
-- the start of the next era and the safe zone in that next era; otherwise we
-- give the safe zone in the current era.
activeSafeZone :: HF.Shape xs
               -> Chain xs
               -> HF.Transitions xs
               -> (Maybe EpochNo, HF.SafeZone)
activeSafeZone (HF.Shape shape) (Chain chain) (HF.Transitions transitions) =
    go shape chain transitions
  where
    go :: Exactly  (x ': xs) HF.EraParams
       -> NonEmpty (x ': xs) [Event]
       -> AtMost         xs  EpochNo
       -> (Maybe EpochNo, HF.SafeZone)
    -- No transition is yet known for the last era on the chain
    go (K ps :* _) (NonEmptyOne _) AtMostNil =
        (Nothing, HF.eraSafeZone ps)
    -- Transition /is/ known for the last era on the chain
    go (_ :* pss) (NonEmptyOne _) (AtMostCons t AtMostNil) =
        (Just t, HF.eraSafeZone (exactlyHead pss))
    -- Find the last era on chain
    go (_ :* pss) (NonEmptyCons _ ess) AtMostNil =
        -- We need to convince ghc there is another era
        case ess of
          NonEmptyCons{} -> go pss ess AtMostNil
          NonEmptyOne{}  -> go pss ess AtMostNil
    go (_ :* pss) (NonEmptyCons _ ess) (AtMostCons _ ts) =
        go pss ess ts

    -- Impossible cases

    -- If this is the final era on the chain, we might know the transition to
    -- the next era, but we certainly couldn't know the next transition
    go _ (NonEmptyOne _) (AtMostCons _ (AtMostCons{})) =
        error "activeSafeZone: impossible"

-- | Return the events within and outside of the safe zone
splitSafeZone :: WithOrigin EpochNo
                 -- ^ Epoch at the tip of the chain
                 -- (Needed because transitions only happen at epoch boundaries)
              -> (Maybe EpochNo, HF.SafeZone)
                 -- ^ Active safe zone (see 'activeSafeZone')
              -> [Event]
                 -- ^ Events after the end of the chain
              -> ([Event], [Event])
splitSafeZone tipEpoch = \(mTransition, safeZone) events ->
    let (definitelySafe, rest) =
           case mTransition of
             Nothing -> ([], events)
             Just t  -> span (beforeEpoch t) events
    in first (definitelySafe ++) $ go [] safeZone rest
  where
    beforeEpoch :: EpochNo -> Event -> Bool
    beforeEpoch t e = eventTimeEpochNo (eventTime e) < t

    go :: [Event]     -- Accumulated events in the safe zone
       -> HF.SafeZone -- Remaining safe zone
       -> [Event]     -- Remaining events to be processed
       -> ([Event], [Event])
    go acc _               []     = (reverse acc, [])
    go acc HF.SafeZone{..} (e:es)
        -- Interpret the 'SafeZone' parameters
      | eventTimeEpochNo (eventTime e) `before` safeBeforeEpoch =
          go (e:acc) (HF.SafeZone (pred' safeFromTip) safeBeforeEpoch) es
      | safeFromTip > 0 =
          go (e:acc) (HF.SafeZone (pred  safeFromTip) safeBeforeEpoch) es
      | otherwise =
          let (sameEpoch, rest) = span inLastEpoch (e:es)
          in (reverse acc ++ sameEpoch, rest)
      where
        lastEpoch :: EpochNo
        lastEpoch = case acc of
                      []   -> fromWithOrigin (EpochNo 0) tipEpoch
                      e':_ -> eventTimeEpochNo (eventTime e')

        inLastEpoch :: Event -> Bool
        inLastEpoch e' = eventTimeEpochNo (eventTime e') == lastEpoch

    before :: EpochNo -> HF.SafeBeforeEpoch -> Bool
    before _ HF.NoLowerBound    = False
    before _ HF.UnsafeUnbounded = True
    before e (HF.LowerBound e') = e < e'

    -- Example. Suppose
    --
    -- * 'safeFromTip'     == 2
    -- * 'safeBeforeEpoch' == 4
    -- * 'eraEpochSize'    == 5
    --
    -- This means that the next era cannot start until slot 4 * 5 = 20.
    -- If we are currently at slot 10, the safe zone extends to slot 20.
    -- If we are currently at slot 19, the safe zone extends to slot 19 + 2 = 21.
    --
    -- This means the 'safeFromTip' is decremented even if we haven't reached
    -- 'safeBeforeEpoch' yet, and will stay at 0 when it reaches 0.
    pred' :: Word64 -> Word64
    pred' 0 = 0
    pred' n = pred n

{-------------------------------------------------------------------------------
  Relation to the HardForkLedgerView
-------------------------------------------------------------------------------}

-- | Construct 'EpochInfo' through the forecast
--
-- We also 'Show' the 'HardForkLedgerView' and the reconstructed 'Summary',
-- for the benefit of 'counterexample'.
hardForkEpochInfo :: ArbitraryChain -> SlotNo -> (EpochInfo Identity, String, String)
hardForkEpochInfo ArbitraryChain{..} for =
    let forecast = mockHardForkLedgerView
                     arbitraryChainShape
                     arbitraryTransitions
                     arbitraryChain
    in case runExcept $ forecastFor forecast for of
         Left err -> (
             EpochInfo {
                 epochInfoSize_  = \_ -> throw err
               , epochInfoFirst_ = \_ -> throw err
               , epochInfoEpoch_ = \_ -> throw err
               }
           , "<out of range>"
           , "<out of range>"
           )
         Right view@HardForkLedgerView{..} ->
           let reconstructed = State.reconstructSummary
                                 arbitraryChainShape
                                 hardForkLedgerViewTransition
                                 hardForkLedgerViewPerEra
           in (
             HF.snapshotEpochInfo reconstructed
           , show view
           , show reconstructed
           )
  where
    ArbitraryParams{..} = arbitraryParams

mockHardForkLedgerView :: SListI xs
                       => HF.Shape xs
                       -> HF.Transitions xs
                       -> Chain xs
                       -> Forecast (HardForkLedgerView_ (K ()) xs)
mockHardForkLedgerView = \(HF.Shape pss) (HF.Transitions ts) (Chain ess) ->
    mkHardForkForecast
      (InPairs.hpure $ Translate $ \_epoch (K ()) -> K ())
      (mockState HF.initBound pss ts ess)
  where
    mockState :: HF.Bound
              -> Exactly  (x ': xs) HF.EraParams
              -> AtMost         xs  EpochNo
              -> NonEmpty (x ': xs) [Event]
              -> Telescope (Past (K ())) (Current (AnnForecast (K ()))) (x : xs)
    mockState start (K ps :* _) ts (NonEmptyOne es) =
        TZ $ Current start $ AnnForecast {
            annForecastEraParams = ps
          , annForecastNext      = atMostHead ts
          , annForecast          = Forecast {
                forecastAt  = tip es
              , forecastFor = \_for -> return $ K ()
              }
          }
    mockState start (K ps :* pss) (AtMostCons t ts) (NonEmptyCons _ ess) =
        TS (Past start end NoSnapshot) (mockState end pss ts ess)
      where
        end :: HF.Bound
        end = HF.mkUpperBound ps start t
    mockState _ _ AtMostNil (NonEmptyCons _ _) =
        error "mockState: next era without transition"

    tip :: [Event] -> WithOrigin SlotNo
    tip [] = Origin
    tip es = At $ eventTimeSlot $ eventTime (last es)
