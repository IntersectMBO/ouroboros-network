{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.Testnet (tests) where

import           Control.Monad.IOSim
import           Control.Monad.IOSim.Types (ThreadId)
import           Control.Monad.Class.MonadTime (Time (Time), diffTime, DiffTime)
import           Control.Tracer (Tracer (Tracer), contramap, nullTracer)

import           Data.Void (Void)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Dynamic (Typeable)
import           Data.Functor (void)
import           Data.List (intercalate)
import qualified Data.List.Trace as Trace
import           Data.Time (secondsToDiffTime)

import           System.Random (mkStdGen)
import           GHC.Exception.Type (SomeException)

import           Ouroboros.Network.Testing.Data.AbsBearerInfo
                     (AbsBearerInfo (..), attenuation, delay, toSduSize)
import           Ouroboros.Network.PeerSelection.Governor
                      (TracePeerSelection (..), DebugPeerSelection (..))
import           Ouroboros.Network.Testing.Data.Signal
                      (Events, Signal, eventsToList,
                      signalProperty)
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                      (TraceLocalRootPeers, TracePublicRootPeers)
import           Ouroboros.Network.PeerSelection.Types (PeerStatus(..))
import           Ouroboros.Network.Diffusion.P2P (TracersExtra(..))
import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace)
import           Ouroboros.Network.ConnectionManager.Types
import qualified Ouroboros.Network.Testing.Data.Signal as Signal
import qualified Ouroboros.Network.PeerSelection.Governor as Governor
import qualified Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers
import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import qualified Ouroboros.Network.Diffusion.P2P as Diff.P2P
import           Ouroboros.Network.Testing.Utils
                      (WithTime(..), WithName(..), tracerWithTime,
                      tracerWithName, sayTracer, splitWithNameTrace)

import           Simulation.Network.Snocket (BearerInfo (..))

import           Test.Ouroboros.Network.Testnet.Simulation.Node
                     (DiffusionScript (..), diffusionSimulation,
                     prop_diffusionScript_commandScript_valid,
                     prop_diffusionScript_fixupCommands,
                     DiffusionSimulationTrace (..))
import           Test.Ouroboros.Network.Diffusion.Node.NodeKernel
import           Test.QuickCheck (Property, counterexample, conjoin, classify, property)
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Testnet"
  [ testGroup "multinodeSim"
    [ testProperty "diffusionScript fixupCommands idempotent"
                   prop_diffusionScript_fixupCommands
    , testProperty "diffusionScript command script valid"
                   prop_diffusionScript_commandScript_valid
    , testProperty "diffusion no livelock"
                   prop_diffusion_nolivelock
    , testProperty "diffusion target established local"
                   prop_diffusion_target_established_local
    , testProperty "diffusion target active below"
                   prop_diffusion_target_active_below
    , testProperty "diffusion target active local above"
                   prop_diffusion_target_active_local_above
    ]
  ]


-- Warning: be careful with writing properties that rely
-- on trace events from multiple components environment.
-- These events typically occur in separate threads and
-- so are not casually ordered. It is ok to use them for
-- timeout/eventually properties, but not for properties
-- that check conditions synchronously.
--
data DiffusionTestTrace =
      DiffusionLocalRootPeerTrace (TraceLocalRootPeers NtNAddr SomeException)
    | DiffusionPublicRootPeerTrace TracePublicRootPeers
    | DiffusionPeerSelectionTrace (TracePeerSelection NtNAddr)
    | DiffusionDebugPeerSelectionTrace (DebugPeerSelection NtNAddr ())
    | DiffusionConnectionManagerTrace
        (ConnectionManagerTrace NtNAddr
          (ConnectionHandlerTrace NtNVersion NtNVersionData))
    | DiffusionDiffusionSimulationTrace DiffusionSimulationTrace
    deriving (Show)

tracersExtraWithTimeName
  :: NtNAddr
  -> Diff.P2P.TracersExtra NtNAddr NtNVersion NtNVersionData
                           NtCAddr NtCVersion NtCVersionData
                           SomeException (IOSim s)
tracersExtraWithTimeName ntnAddr =
  Diff.P2P.TracersExtra {
    dtTraceLocalRootPeersTracer           = contramap
                                             DiffusionLocalRootPeerTrace
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtTracePublicRootPeersTracer        = contramap
                                             DiffusionPublicRootPeerTrace
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtTracePeerSelectionTracer          = contramap
                                             DiffusionPeerSelectionTrace
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtDebugPeerSelectionInitiatorTracer = contramap
                                             ( DiffusionDebugPeerSelectionTrace
                                             . void
                                             )
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtDebugPeerSelectionInitiatorResponderTracer
        = contramap
           ( DiffusionDebugPeerSelectionTrace
           . void
           )
        . tracerWithName ntnAddr
        . tracerWithTime
        $ dynamicTracer
    , dtTracePeerSelectionCounters        = nullTracer
    , dtPeerSelectionActionsTracer        = nullTracer
    , dtConnectionManagerTracer           = contramap
                                             DiffusionConnectionManagerTrace
                                          . tracerWithName ntnAddr
                                          . tracerWithTime
                                          $ dynamicTracer
    , dtConnectionManagerTransitionTracer = nullTracer
    , dtServerTracer                      = nullTracer
    , dtInboundGovernorTracer             = nullTracer
    , dtInboundGovernorTransitionTracer   = nullTracer
    , dtLocalConnectionManagerTracer      = nullTracer
    , dtLocalServerTracer                 = nullTracer
    , dtLocalInboundGovernorTracer        = nullTracer
  }

tracerDiffusionSimWithTimeName :: NtNAddr -> Tracer (IOSim s) DiffusionSimulationTrace
tracerDiffusionSimWithTimeName ntnAddr =
   contramap DiffusionDiffusionSimulationTrace
 . tracerWithName ntnAddr
 . tracerWithTime
 $ dynamicTracer

-- | A variant of
-- 'Test.Ouroboros.Network.ConnectionHandler.Network.PeerSelection.prop_governor_nolivelock'
-- but for running on Diffusion. This test doesn't check for events occuring at the same
-- time but rather for events happening between an interval (usual 1s). This is because,
-- since Diffusion is much more complex and can run more than 1 node in parallel, time
-- might progress but very slowly almost like a livelock. We want to safeguard from such
-- cases.
--
prop_diffusion_nolivelock :: AbsBearerInfo
                         -> DiffusionScript
                         -> Property
prop_diffusion_nolivelock defaultBearerInfo diffScript@(DiffusionScript l) =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        trace :: [(Time, ThreadId, Maybe ThreadLabel, SimEventType)]
        trace = take 1000000
              . traceEvents
              $ runSimTrace sim

        lastTime :: Time
        lastTime = getTime (last trace)

     in classifySimulatedTime lastTime
      $ check_governor_nolivelock (secondsToDiffTime 0)
                                  trace
  where
    check_governor_nolivelock :: DiffTime
                              -> [(Time, ThreadId, Maybe ThreadLabel, SimEventType)]
                              -> Property
    check_governor_nolivelock dt trace =
      let trace' = (\(t, tid, tl, e) -> (t, (tid, tl, e)))
                 <$> trace

          numberOfEvents = 10000 * max (length l) 1

       in case tooManyEventsBeforeTimeAdvances numberOfEvents dt trace' of
            Nothing -> property True
            Just es ->
              counterexample
                ("over " ++ show numberOfEvents ++ " events in "
                ++ show dt ++ "\n" ++ "first " ++ show numberOfEvents
                ++ " events: " ++ (unlines . map show . take numberOfEvents $ es))
              $ property False

    tooManyEventsBeforeTimeAdvances :: Int
                                    -> DiffTime
                                    -> [(Time, e)]
                                    -> Maybe [(Time, e)]
    tooManyEventsBeforeTimeAdvances _         _  []     = Nothing
    tooManyEventsBeforeTimeAdvances threshold dt trace0 =
        go (groupByTime dt trace0)
      where
        groupByTime :: DiffTime -> [(Time, e)] -> [[(Time, e)]]
        groupByTime _ [] = []
        groupByTime dtime trace@((t, _):_) =
          let (tl, tr) = span (\(t', _) -> diffTime t' t <= dtime) trace
           in tl : groupByTime dtime tr

        go :: [[(Time, e)]] -> Maybe [(Time, e)]
        go []    = Nothing
        go (h:t)
          | countdown threshold h = go t
          | otherwise = Just h

        countdown 0 (_ : _) = False
        countdown _ []      = True
        countdown n (_ : es)  = countdown (n-1) es

-- | A variant of
-- 'Test.Ouroboros.Network.PeerSelection.prop_governor_target_established_local'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
-- We do not need separate above and below variants of this property since it
-- is not possible to exceed the target.
--
prop_diffusion_target_established_local :: AbsBearerInfo
                                        -> DiffusionScript
                                        -> Property
prop_diffusion_target_established_local defaultBearerInfo diffScript =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        events :: [Events DiffusionTestTrace]
        events = fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . Trace.toList
               . splitWithNameTrace
               . Trace.fromList ()
               . fmap snd
               . Trace.toList
               . fmap (\(WithTime t (WithName name b)) -> (t, WithName name (WithTime t b)))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.fromList (MainReturn (Time 0) () [])
               . fmap (\(t, tid, tl, te) -> SimEvent t tid tl te)
               . take 1000000
               . traceEvents
               $ runSimTrace sim

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_target_established_local ev
        )
      <$> events

  where
    verify_target_established_local :: Events DiffusionTestTrace -> Property
    verify_target_established_local events =
      let govLocalRootPeersSig :: Signal (Set NtNAddr)
          govLocalRootPeersSig =
            selectDiffusionPeerSelectionState
              ( LocalRootPeers.keysSet
              . Governor.localRootPeers)
              events

          govInProgressPromoteColdSig :: Signal (Set NtNAddr)
          govInProgressPromoteColdSig =
            selectDiffusionPeerSelectionState
              Governor.inProgressPromoteCold
              events

          govEstablishedPeersSig :: Signal (Set NtNAddr)
          govEstablishedPeersSig =
            selectDiffusionPeerSelectionState
              ( EstablishedPeers.toSet
              . Governor.establishedPeers)
              events

          govEstablishedFailuresSig :: Signal (Set NtNAddr)
          govEstablishedFailuresSig =
              Signal.keyedLinger
                180 -- 3 minutes  -- TODO: too eager to reconnect?
                (fromMaybe Set.empty)
            . Signal.fromEvents
            . Signal.selectEvents
                (\case TracePromoteColdFailed _ _ peer _ _ ->
                         Just (Set.singleton peer)
                       --TODO: what about TraceDemoteWarmDone ?
                       -- these are also not immediate candidates
                       -- why does the property not fail for not tracking these?
                       TraceDemoteAsynchronous status
                         | Set.null failures -> Nothing
                         | otherwise         -> Just failures
                         where
                           failures =
                             Map.keysSet (Map.filter (==PeerCold) status)
                       TracePromoteWarmFailed _ _ peer _ ->
                         Just (Set.singleton peer)
                       _ -> Nothing
                )
            . selectDiffusionPeerSelectionEvents
            $ events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoinedOrKilled (Set.singleton ())
                                                      Set.empty)
                                  (fromJoinedOrKilled Set.empty
                                                      (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          promotionOpportunities :: Signal (Set NtNAddr)
          promotionOpportunities =
            (\local established recentFailures inProgressPromoteCold isAlive ->
              if isAlive
              then local Set.\\ established
                         Set.\\ recentFailures
                         Set.\\ inProgressPromoteCold
              else Set.empty
            ) <$> govLocalRootPeersSig
              <*> govEstablishedPeersSig
              <*> govEstablishedFailuresSig
              <*> govInProgressPromoteColdSig
              <*> trIsNodeAlive

          promotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          promotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeout
              10 -- seconds
              id
              promotionOpportunities

       in counterexample
            ("\nSignal key: (local root peers, established peers, " ++
             "recent failures, is alive, opportunities, ignored too long)\n" ++
               intercalate "\n" (map show $ eventsToList events)
            )
        $ signalProperty 20 show
              (\(_,_,_,_,_,_, tooLong) -> Set.null tooLong)
              ((,,,,,,) <$> govLocalRootPeersSig
                      <*> govEstablishedPeersSig
                      <*> govEstablishedFailuresSig
                      <*> govInProgressPromoteColdSig
                      <*> trIsNodeAlive
                      <*> promotionOpportunities
                      <*> promotionOpportunitiesIgnoredTooLong
              )

-- | A variant of
-- 'Test.Ouroboros.Network.PeerSelection.prop_governor_target_active_below'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
-- We do not need separate above and below variants of this property since it
-- is not possible to exceed the target.
--
prop_diffusion_target_active_below :: AbsBearerInfo
                                   -> DiffusionScript
                                   -> Property
prop_diffusion_target_active_below defaultBearerInfo diffScript =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        events :: [Events DiffusionTestTrace]
        events = fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . Trace.toList
               . splitWithNameTrace
               . Trace.fromList ()
               . fmap snd
               . Signal.eventsToList
               . Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . Trace.toList
               . fmap (\(WithTime t (WithName name b)) -> (t, WithName name (WithTime t b)))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.fromList (MainReturn (Time 0) () [])
               . fmap (\(t, tid, tl, te) -> SimEvent t tid tl te)
               . take 1000000
               . traceEvents
               $ runSimTrace sim

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_target_active_below ev
        )
      <$> events

  where
    verify_target_active_below :: Events DiffusionTestTrace -> Property
    verify_target_active_below events =
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers NtNAddr)
          govLocalRootPeersSig =
            selectDiffusionPeerSelectionState Governor.localRootPeers events

          govEstablishedPeersSig :: Signal (Set NtNAddr)
          govEstablishedPeersSig =
            selectDiffusionPeerSelectionState
              (EstablishedPeers.toSet . Governor.establishedPeers)
              events

          govActivePeersSig :: Signal (Set NtNAddr)
          govActivePeersSig =
            selectDiffusionPeerSelectionState Governor.activePeers events

          govActiveFailuresSig :: Signal (Set NtNAddr)
          govActiveFailuresSig =
              Signal.keyedLinger
                180 -- 3 minutes  -- TODO: too eager to reconnect?
                (fromMaybe Set.empty)
            . Signal.fromEvents
            . Signal.selectEvents
                (\case TracePromoteWarmFailed _ _ peer _ ->
                         --TODO: the environment does not yet cause this to happen
                         -- it requires synchronous failure in the establish
                         -- action
                         Just (Set.singleton peer)
                       --TODO
                       TraceDemoteAsynchronous status
                         | Set.null failures -> Nothing
                         | otherwise         -> Just failures
                         where
                           failures = Map.keysSet (Map.filter (==PeerWarm) status)
                       _ -> Nothing
                )
            . selectDiffusionPeerSelectionEvents
            $ events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoinedOrKilled (Set.singleton ())
                                                      Set.empty)
                                  (fromJoinedOrKilled Set.empty
                                                      (Set.singleton()))
                                  (const False)
                                  trJoinKillSig

          promotionOpportunities :: Signal (Set NtNAddr)
          promotionOpportunities =
            (\local established active recentFailures isAlive ->
              if isAlive
              then Set.unions
                    [ -- There are no opportunities if we're at or above target
                      if Set.size groupActive >= target
                         then Set.empty
                         else groupEstablished Set.\\ active
                                               Set.\\ recentFailures
                    | (target, group) <- LocalRootPeers.toGroupSets local
                    , let groupActive      = group `Set.intersection` active
                          groupEstablished = group `Set.intersection` established
                    ]
              else Set.empty
            ) <$> govLocalRootPeersSig
              <*> govEstablishedPeersSig
              <*> govActivePeersSig
              <*> govActiveFailuresSig
              <*> trIsNodeAlive

          promotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          promotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeout
              10 -- seconds
              id
              promotionOpportunities

       in counterexample
            ("\nSignal key: (local, established peers, active peers, " ++
             "recent failures, opportunities, ignored too long)") $

          signalProperty 20 show
            (\toolong -> Set.null toolong)
            promotionOpportunitiesIgnoredTooLong

-- | A variant of
-- 'Test.Ouroboros.Network.PeerSelection.prop_governor_target_active_local_above'
-- but for running on Diffusion. This means it has to have in consideration the
-- the logs for all nodes running will all appear in the trace and the test
-- property should only be valid while a given node is up and running.
--
-- We do not need separate above and below variants of this property since it
-- is not possible to exceed the target.
--
prop_diffusion_target_active_local_above :: AbsBearerInfo
                                         -> DiffusionScript
                                         -> Property
prop_diffusion_target_active_local_above defaultBearerInfo diffScript =
    let sim :: forall s . IOSim s Void
        sim = diffusionSimulation (toBearerInfo defaultBearerInfo)
                                  diffScript
                                  tracersExtraWithTimeName
                                  tracerDiffusionSimWithTimeName

        events :: [Events DiffusionTestTrace]
        events = fmap ( Signal.eventsFromList
                      . fmap (\(WithName _ (WithTime t b)) -> (t, b))
                      )
               . Trace.toList
               . splitWithNameTrace
               . Trace.fromList ()
               . fmap snd
               . Signal.eventsToList
               . Signal.eventsFromListUpToTime (Time (10 * 60 * 60))
               . Trace.toList
               . fmap (\(WithTime t (WithName name b)) -> (t, WithName name (WithTime t b)))
               . withTimeNameTraceEvents
                  @DiffusionTestTrace
                  @NtNAddr
               . Trace.fromList (MainReturn (Time 0) () [])
               . fmap (\(t, tid, tl, te) -> SimEvent t tid tl te)
               . take 1000000
               . traceEvents
               $ runSimTrace sim

     in conjoin
      $ (\ev ->
        let evsList = eventsToList ev
            lastTime = fst
                     . last
                     $ evsList
         in classifySimulatedTime lastTime
          $ classifyNumberOfEvents (length evsList)
          $ verify_target_active_above ev
        )
      <$> events

  where
    verify_target_active_above :: Events DiffusionTestTrace -> Property
    verify_target_active_above events =
      let govLocalRootPeersSig :: Signal (LocalRootPeers.LocalRootPeers NtNAddr)
          govLocalRootPeersSig =
            selectDiffusionPeerSelectionState Governor.localRootPeers events

          govActivePeersSig :: Signal (Set NtNAddr)
          govActivePeersSig =
            selectDiffusionPeerSelectionState Governor.activePeers events

          trJoinKillSig :: Signal JoinedOrKilled
          trJoinKillSig =
              Signal.fromChangeEvents Killed -- Default to TrKillingNode
            . Signal.selectEvents
                (\case TrJoiningNetwork -> Just Joined
                       TrKillingNode    -> Just Killed
                       _                -> Nothing
                )
            . selectDiffusionSimulationTrace
            $ events

          -- Signal.keyedUntil receives 2 functions one that sets start of the
          -- set signal, one that ends it and another that stops all.
          --
          -- In this particular case we want a signal that is keyed beginning
          -- on a TrJoiningNetwork and ends on TrKillingNode, giving us a Signal
          -- with the periods when a node was alive.
          trIsNodeAlive :: Signal Bool
          trIsNodeAlive =
                not . Set.null
            <$> Signal.keyedUntil (fromJoinedOrKilled (Set.singleton ())
                                                      Set.empty)
                                  (fromJoinedOrKilled Set.empty
                                                      (Set.singleton ()))
                                  (const False)
                                  trJoinKillSig

          demotionOpportunities :: Signal (Set NtNAddr)
          demotionOpportunities =
            (\local active isAlive ->
              if isAlive
              then Set.unions
                    [ -- There are no opportunities if we're at or below target
                      if Set.size groupActive <= target
                         then Set.empty
                         else groupActive
                    | (target, group) <- LocalRootPeers.toGroupSets local
                    , let groupActive = group `Set.intersection` active
                    ]
              else Set.empty
            ) <$> govLocalRootPeersSig
              <*> govActivePeersSig
              <*> trIsNodeAlive

          demotionOpportunitiesIgnoredTooLong :: Signal (Set NtNAddr)
          demotionOpportunitiesIgnoredTooLong =
            Signal.keyedTimeout
              10 -- seconds
              id
              demotionOpportunities

       in counterexample
            ("\nSignal key: (local peers, active peers, " ++
             "demotion opportunities, ignored too long)") $

          signalProperty 20 show
            (\(_,_,_,_,toolong) -> Set.null toolong)
            ((,,,,) <$> (LocalRootPeers.toGroupSets <$> govLocalRootPeersSig)
                   <*> govActivePeersSig
                   <*> trIsNodeAlive
                   <*> demotionOpportunities
                   <*> demotionOpportunitiesIgnoredTooLong)

-- Utils
--

data JoinedOrKilled = Joined | Killed
  deriving (Eq, Show)

-- Similar to 'either' but for 'JoinedOrKilled'
fromJoinedOrKilled :: c -> c -> JoinedOrKilled -> c
fromJoinedOrKilled j _ Joined = j
fromJoinedOrKilled _ k Killed = k

getTime :: (Time, ThreadId, Maybe ThreadLabel, SimEventType) -> Time
getTime (t, _, _, _) = t

classifySimulatedTime :: Time -> Property -> Property
classifySimulatedTime lastTime =
        classify (lastTime <= Time (60 * 60)) "Simulated time <= 1H"
      . classify (lastTime >= Time (5 * 60 * 60)) "Simulated time >= 5H"
      . classify (lastTime >= Time (10 * 60 * 60)) "Simulated time >= 10H"
      . classify (lastTime >= Time (24 * 60 * 60)) "Simulated time >= 1 Day"

classifyNumberOfEvents :: Int -> Property -> Property
classifyNumberOfEvents nEvents =
        classify (nEvents <= 100) "Nº Events <= 100"
      . classify (nEvents >= 1000) "Nº Events >= 1000"
      . classify (nEvents >= 10000) "Nº Events >= 10000"
      . classify (nEvents >= 50000) "Nº Events >= 50000"

dynamicTracer :: (Typeable a, Show a) => Tracer (IOSim s) a
dynamicTracer = Tracer traceM <> sayTracer

withTimeNameTraceEvents :: forall b name r. (Typeable b, Typeable name)
                        => SimTrace r
                        -> Trace (SimResult r) (WithTime (WithName name b))
withTimeNameTraceEvents = traceSelectTraceEventsDynamic
                            @r
                            @(WithTime (WithName name b))

selectDiffusionPeerSelectionEvents :: Events DiffusionTestTrace
                                   -> Events (TracePeerSelection NtNAddr)
selectDiffusionPeerSelectionEvents = Signal.selectEvents
                    (\case DiffusionPeerSelectionTrace e -> Just e
                           _                             -> Nothing)

selectDiffusionSimulationTrace :: Events DiffusionTestTrace
                               -> Events DiffusionSimulationTrace
selectDiffusionSimulationTrace = Signal.selectEvents
                    (\case DiffusionDiffusionSimulationTrace e -> Just e
                           _                                   -> Nothing)

selectDiffusionPeerSelectionState :: Eq a
                                  => (Governor.PeerSelectionState NtNAddr () -> a)
                                  -> Events DiffusionTestTrace
                                  -> Signal a
selectDiffusionPeerSelectionState f =
    Signal.nub
  . fmap f
  -- TODO: #3182 Rng seed should come from quickcheck.
  . Signal.fromChangeEvents (Governor.emptyPeerSelectionState $ mkStdGen 42)
  . Signal.selectEvents
      (\case
        DiffusionDebugPeerSelectionTrace (TraceGovernorState _ _ st) -> Just st
        _                                                            -> Nothing)

toBearerInfo :: AbsBearerInfo -> BearerInfo
toBearerInfo abi =
    BearerInfo {
        biConnectionDelay      = delay (abiConnectionDelay abi),
        biInboundAttenuation   = attenuation (abiInboundAttenuation abi),
        biOutboundAttenuation  = attenuation (abiOutboundAttenuation abi),
        biInboundWriteFailure  = abiInboundWriteFailure abi,
        biOutboundWriteFailure = abiOutboundWriteFailure abi,
        biAcceptFailures       = Nothing, -- TODO
        biSDUSize              = toSduSize (abiSDUSize abi)
      }

