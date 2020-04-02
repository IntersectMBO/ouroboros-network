{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}

module Test.Consensus.HardFork.History (tests) where

import           Data.Foldable (toList)
import           Data.Function (on)
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

import           Test.Consensus.HardFork.Infra
import           Test.Util.Orphans.Arbitrary

tests :: TestTree
tests = testGroup "HardForkHistory" [
      testGroup "Chain" [
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
    withArbitraryChainSummary chain $ \summary -> noPastHorizonException $ do
      (epochNo, epochSlot) <- HF.slotToEpoch summary eventTimeSlot
      return $ epochNo   === eventTimeEpochNo
          .&&. epochSlot === eventTimeEpochSlot
  where
    EventTime{..} = eventTime (arbitraryChainPre !! arbitraryEventIx)

eventEpochToSlot :: ArbitraryChain -> Property
eventEpochToSlot chain@ArbitraryChain{..} =
    withArbitraryChainSummary chain $ \summary -> noPastHorizonException $ do
      (startOfEpoch, epochSize) <- HF.epochToSlot summary eventTimeEpochNo
      return $ eventTimeSlot === HF.addSlots eventTimeEpochSlot startOfEpoch
          .&&. eventTimeEpochSlot < unEpochSize epochSize
  where
    EventTime{..} = eventTime (arbitraryChainPre !! arbitraryEventIx)

eventSlotToWallclock :: ArbitraryChain -> Property
eventSlotToWallclock chain@ArbitraryChain{..} =
    withArbitraryChainSummary chain $ \summary -> noPastHorizonException $ do
      (time, _slotLen) <- HF.slotToWallclock summary eventTimeSlot
      return $ time === eventTimeUTC
  where
    EventTime{..} = eventTime (arbitraryChainPre !! arbitraryEventIx)

eventWallclockToSlot :: ArbitraryChain -> Property
eventWallclockToSlot chain@ArbitraryChain{..} =
    withArbitraryChainSummary chain $ \summary -> noPastHorizonException $ do
      (slot, inSlot) <- HF.wallclockToSlot summary eventTimeUTC
      return $ slot === eventTimeSlot .&&. inSlot === 0
  where
    EventTime{..} = eventTime (arbitraryChainPre !! arbitraryEventIx)

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
withArbitraryChainSummary ArbitraryChain{..} f =
    f $ HF.summarize
          arbitraryChainStart
          (chainTip arbitraryChain)
          arbitraryChainShape
          (chainTransitions arbitraryChainEras arbitraryChain)

instance Arbitrary ArbitraryChain where
  arbitrary = chooseEras $ \eras -> do
      start    <- HF.SystemStart <$> arbitrary
      shape    <- HF.Shape <$> erasMapStateM genParams (EpochNo 0) eras
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
      genParams :: EpochNo -> Era -> Gen (EpochNo, HF.EraParams)
      genParams startOfThis _era = do
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
data Event e = Event {
      eventType      :: EventType e
    , eventTime      :: EventTime
    , eventEraParams :: HF.EraParams
    }
  deriving (Show, Functor)

data EventType e =
    -- | Nothing of interest happens, time just ticks
    Tick

    -- | A new hard fork transition is confirmed
    --
    -- "Confirmed" here is taken to mean "no longer subject to rollback",
    -- which is the concept that the hard fork history depends on.
    --
    -- For a fully constructed chain, we expect @e@ to be 'EpochNo': the epoch
    -- in which the transition will take place. During construction however we
    -- we set this to @()@, and fill in those blanks later.
  | Confirm e
  deriving (Show, Functor)

-- | When did an event occur?
--
-- NOTE: We don't care about 'BlockNo' here. Our events don't record necessarily
-- whether a block is actually present in a given slot or not.
data EventTime = EventTime {
      eventTimeSlot      :: SlotNo
    , eventTimeEpochNo   :: EpochNo
    , eventTimeEpochSlot :: Word64
    , eventTimeUTC       :: UTCTime
    , eventTimeEra       :: Era
    }
  deriving (Show)

eventEra :: Event f -> Era
eventEra = eventTimeEra . eventTime

initTime :: HF.SystemStart -> Era -> EventTime
initTime (HF.SystemStart start) firstEra = EventTime {
      eventTimeSlot      = SlotNo  0
    , eventTimeEpochNo   = EpochNo 0
    , eventTimeEpochSlot = 0
    , eventTimeUTC       = start
    , eventTimeEra       = firstEra
    }

-- | Next time slot
--
-- We assume the era does not change (this is done separately in 'genChain')
stepTime :: HF.EraParams -> EventTime -> EventTime
stepTime HF.EraParams{..} EventTime{..} = EventTime{
      eventTimeSlot      = succ eventTimeSlot
    , eventTimeEpochNo   = epoch'
    , eventTimeEpochSlot = relSlot'
    , eventTimeUTC       = addUTCTime (getSlotLength eraSlotLength) eventTimeUTC
    , eventTimeEra       = eventTimeEra
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
newtype Chain xs = Chain (AtMost xs [Event EpochNo])
  deriving (Show)

-- | Slot at the tip of the chain
chainTip :: Chain xs -> WithOrigin SlotNo
chainTip (Chain xs) = tip . reverse . concat . toList $ xs
  where
    tip :: [Event EpochNo] -> WithOrigin SlotNo
    tip []    = Origin
    tip (e:_) = At (eventTimeSlot (eventTime e))

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
findTransition :: Era -> [Event EpochNo] -> Maybe EpochNo
findTransition era =
    mustBeUnique . catMaybes . map (isTransition . eventType)
  where
    mustBeUnique :: [EpochNo] -> Maybe EpochNo
    mustBeUnique []  = Nothing
    mustBeUnique [e] = Just e
    mustBeUnique _   = error $ "multiple transition points in " ++ show era

    isTransition :: EventType a -> Maybe a
    isTransition (Confirm e) = Just e
    isTransition (Tick)      = Nothing

patchEvents :: [Event ()] -> [Event EpochNo]
patchEvents events = map (\e -> const (go e) <$> e) events
  where
    bounds :: Map Era (EpochNo, EpochNo)
    bounds = eventsEraBounds events

    go :: Event () -> EpochNo
    go = snd . (bounds Map.!) . eventEra

fromPreChain :: Eras xs -> PreChain -> Chain xs
fromPreChain (Eras eras) preChain = Chain $
    snd <$> exactlyZipAtMost eras grouped
  where
    grouped :: [[Event EpochNo]]
    grouped = L.groupBy ((==) `on` eventEra) preChain

{-------------------------------------------------------------------------------
  Pre-chain
-------------------------------------------------------------------------------}

type PreChain = [Event EpochNo]

-- | Inclusive lower bound and exclusive upper bound for each era
eventsEraBounds :: [Event ()] -> Map Era (EpochNo, EpochNo)
eventsEraBounds []    = Map.empty
eventsEraBounds chain =
      Map.fromList
    . map bounds
    . L.groupBy ((==) `on` eventEra)
    $ chain
  where
    bounds :: [Event ()] -> (Era, (EpochNo, EpochNo))
    bounds era = (
          eventEra $ head era
        , (minimum epochs, succ $ maximum epochs) -- Upper bound is exclusive
        )
      where
        epochs :: [EpochNo]
        epochs = map (eventTimeEpochNo . eventTime) era

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
          -> Gen [Event ()]
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
       -> Gen [Event ()]
    go 0 _   _        _     = return []
    go n now canStart shape = do
        case shape of
          ExactlyCons _ shape'@(ExactlyCons{})
            | canStartNow && eventTimeEpochSlot now == 0
            -> do
              shouldStartNow <- arbitrary
              if shouldStartNow
                then go' n now NotYet    shape'
                else go' n now canStart' shape
          _otherwise
            -> go' n now canStart' shape
      where
        (canStart', canStartNow) = stepCanStartNewEra
                                    (eventTimeEpochNo now)
                                    canStart

    -- After having decided whether or not to start a new era
    go' :: Word64
        -> EventTime
        -> CanStartNewEra
        -> Exactly (x ': xs) (Era, HF.EraParams)
        -> Gen [Event ()]
    go' n now canStart shape@(ExactlyCons (era, params@HF.EraParams{..}) next) = do
        (typ, canStart') <- frequency $ concat [
            [ (2, return (Tick, canStart)) ]

            -- Avoid starting a count-down if another one is already in process
            -- The final era cannot contain any transitions
          , [ (1, return (Confirm (), CanStartIf eraSafeZone))
            | NotYet          <- [canStart]
            , ExactlyCons _ _ <- [next]
            ]
          ]
        let event = Event {
                eventType      = typ
              , eventEraParams = params
              , eventTime      = now { eventTimeEra = era }
              }
        (event :) <$> go n' (stepTime params now) canStart' shape
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
  Shifting time
-------------------------------------------------------------------------------}

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

instance ShiftTime (Event f) where
  shiftTime delta e = e { eventTime = shiftTime delta (eventTime e) }

instance ShiftTime EventTime where
  shiftTime delta t = t { eventTimeUTC = shiftTime delta (eventTimeUTC t) }

-- | Reset chain start
resetArbitraryChainStart :: ArbitraryChain -> ArbitraryChain
resetArbitraryChainStart chain@ArbitraryChain{..} =
    shiftTime (negate ahead) chain
  where
    ahead :: NominalDiffTime
    ahead = diffUTCTime (HF.getSystemStart arbitraryChainStart) dawnOfTime
