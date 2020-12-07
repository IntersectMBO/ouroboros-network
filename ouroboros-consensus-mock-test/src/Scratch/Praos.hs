{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-name-shadowing  #-}

module Scratch.Praos (
  _main,
  tests,
  -- * Example
  simulateExample,
  simulateExampleIO,
  ) where

-- the general definitions
import           Control.Exception (Exception, SomeException)
import           Control.Monad (foldM, when)
import           Control.Monad.ST.Lazy (ST, runST)
import           Data.Dynamic (fromDynamic)
import           Data.Foldable (forM_)
import           Data.Functor ((<&>))
import           Data.Functor.Contravariant (contramap)
import           Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy (Proxy (..))
import           Data.Semigroup (Min (..))
import qualified Data.Set as Set
import           Data.Traversable (forM)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Natural (Natural)
import           Quiet (Quiet (..))
import           System.Random (mkStdGen)
import           Test.QuickCheck

import           Control.Tracer (Tracer (..), condTracing, nullTracer, showTracing, traceWith)

import           Control.Monad.Class.MonadSay (say)
import           Control.Monad.IOSim (Failure, IOSim, STMSim, TraceEvent (..), Trace (..), runSim, runSimTraceST, traceM)

import           Cardano.Slotting.Slot (EpochSize (..), WithOrigin (..))

import           Ouroboros.Network.Block (BlockNo, pattern GenesisPoint, Point, StandardHash)
import           Ouroboros.Network.Protocol.Limits (longWait, shortWait)

import           Ouroboros.Consensus.BlockchainTime (SlotLength, getSlotLength, slotLengthFromMillisec)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode (getSystemStart)
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Mock.Ledger (BlockConfig)
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.Praos (MockPraosBlock, protocolInfoPraos)
import           Ouroboros.Consensus.Mock.Protocol.Praos (PraosParams (..))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (NodeToNodeVersion, supportedNodeToNodeVersions)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..), enumCoreNodes)
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike (IOLike, Time)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import           Ouroboros.Consensus.Util.Time (nominalDelay)

import           Test.ThreadNet.Util.Expectations (NumBlocks (..))
import           Test.ThreadNet.Util.NodeToNodeVersion (newestVersion)
import qualified Test.Util.HardFork.Future as HFF
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Slots (NumSlots (..))

import           Scratch.ThreadNet.MockNet (Arguments (Arguments), NetArguments (NetArguments), NodeArguments (NodeArguments), SomeNodeArguments (SomeNodeArguments))
import qualified Scratch.ThreadNet.MockNet as MockNet
import           Scratch.ThreadNet.Types
import           Scratch.Toolbox

----

import           Control.Concurrent.Async (AsyncCancelled (..))

----

import           Control.Monad.IOSim (Failure (..))

import           Test.Tasty
import           Test.Tasty.QuickCheck

{-------------------------------------------------------------------------------
  Example
-------------------------------------------------------------------------------}

-- | A fixed arbitrary plan with constant message delays
examplePlan :: NetPlan
examplePlan =
    mconcat $
      [ mempty
      , eventsNetPlan 0 $
          concat [mk 0 [], mk 1 [0], mk 2 [0, 1], mk 3 [1]]
      , eventsNetPlan 80 $
          [ PlannedDisconnect (connid 2 1 0)
          , PlannedDisconnect (connid 1 2 0)
          ]
      , eventsNetPlan 90 $
          [ PlannedConnect
              (connid 2 1 1)
              (mpConst $ ClientServerPair verySmallConstantDelay verySmallConstantDelay)
              ntnVersion
          , PlannedConnect
              (connid 1 2 1)
              (mpConst $ ClientServerPair verySmallConstantDelay verySmallConstantDelay)
              ntnVersion
          ]
      ]
  where
    connid i j k =
        ConnectionId (CoreNodeId i `ClientServerPair` CoreNodeId j) k

    (ntnVersion, _) = newestVersion (Proxy :: Proxy MockPraosBlock)

    verySmallConstantDelay :: Delays
    verySmallConstantDelay = Delays $ 3 NE.:| []

    keepAliveRng = mkStdGen 230412340023140132

    mk :: Word64 -> [Word64] -> [PlannedEvent]
    mk i is =
        PlannedNodeUp
          (NodeId cid 0)
          NodeUpData
            { keepAliveRng
            } :
        foldr
          (\elder acc ->
              PlannedConnect
                (ConnectionId (ClientServerPair cid elder) 0)
                ( mpConst $
                  ClientServerPair
                    verySmallConstantDelay
                    verySmallConstantDelay
                )
                ntnVersion
            : PlannedConnect
                (ConnectionId (ClientServerPair elder cid) 0)
                ( mpConst $
                  ClientServerPair
                    verySmallConstantDelay
                    verySmallConstantDelay
                )
                ntnVersion
            : acc
          )
          []
          elders
      where
        cid    = CoreNodeId i
        elders = map CoreNodeId is

{-------------------------------------------------------------------------------
  Praos Generator
-------------------------------------------------------------------------------}

data PraosTestSetup = PraosTestSetup
  { activeSlotCoeff :: Double
  , initialNonce    :: Natural
  }
  deriving (Show)

genPraosTestSetup :: Gen PraosTestSetup
genPraosTestSetup = do
    -- TODO magic number
    activeSlotCoeff <- elements      $  map (/ 1000) [100 .. 999]
    initialNonce    <- fromIntegral <$> choose (0, maxBound :: Word64)
    pure PraosTestSetup
      { activeSlotCoeff
      , initialNonce
      }

-- | @ceiling (10k/f)@
mkPraosEpochSize :: PraosTestSetup -> SecurityParam -> EpochSize
mkPraosEpochSize praosTestSetup (SecurityParam k) =
    -- TODO once ledger's stability window is parameterized, we can tighten
    -- this (and vary it?)
    EpochSize $ ceiling $ (10 / activeSlotCoeff) * fromIntegral k
  where
    PraosTestSetup
      { activeSlotCoeff
      } = praosTestSetup

mkPraosNodeArguments ::
     IOLike m
  => Map CoreNodeId ((Tracer m (SelectionEvent MockPraosBlock), Tracer (IOLike.STM m) (StmEvent MockPraosBlock)))
  -> PraosTestSetup
  -> NumCoreNodes
  -> SecurityParam
  -> HFF.Future
  -> CoreNodeId
  -> SomeNodeArguments m
mkPraosNodeArguments
  tracers
  praosTestSetup
  numCoreNodes
  k
  hffFuture
  coreNodeId
  = SomeNodeArguments $ NodeArguments
  { MockNet.protocolInfo    = protocolInfoPraos
      numCoreNodes
      (Just $ ceiling $ 3 * fromIntegral (maxRollbacks k) / activeSlotCoeff)
      coreNodeId
      PraosParams {
          praosLeaderF       = activeSlotCoeff
        , praosSecurityParam = k
        , praosSlotsPerEpoch = unEpochSize epochSize
        }
      (HardFork.defaultEraParams k slotLength)
      initialNonce
  , MockNet.vertexTracers   =
      usefulEventSubsetTracer
        `uncurry`
        (Map.findWithDefault (nullTracer, nullTracer) coreNodeId tracers)
  , MockNet.wrapForgeBlock   = MockNet.doNotWrapForgeBlock
  }
  where
    PraosTestSetup
      { activeSlotCoeff
      , initialNonce
      } = praosTestSetup

    (slotLength, epochSize) = case hffFuture of
      HFF.EraCons{}           -> error "exampleMockPraosArguments: EraCons"
      HFF.EraFinal slen esize -> (slen, esize)

{-------------------------------------------------------------------------------
  Net Generator
-------------------------------------------------------------------------------}

data NetTestSetup = NetTestSetup
  { commonK     :: SecurityParam
  , future      :: HFF.Future   -- TODO replace with HF.Summary?
  , mbEndOfDays :: Maybe IOLike.DiffTime
  , plan        :: NetPlan
    -- ^ No events planned after 'numSlots'
  }
  deriving (Show)

genDelays :: IOLike.DiffTime -> Gen Delays
genDelays maxDelay = do
    n <- choose (0, 999)   -- TODO magic number
    Delays <$> nePlus1 n genDelay
  where
    -- TODO add occasional large ones to simulate network partitions
    genDelay :: Gen IOLike.DiffTime
    genDelay =
        ((* maxDelay) . realToFrac) <$> choose (0, 1 :: Double)

    nePlus1 :: forall a. Int -> Gen a -> Gen (NE.NonEmpty a)
    nePlus1 n x = (NE.:|) <$> x <*> vectorOf n x

-- | The large number of delays are illegible
removeAllDelays :: NetPlan -> NetPlan
removeAllDelays (NetPlan m) =
    NetPlan $ fmap (fmap go) m
  where
    go ev = case ev of
      PlannedDisconnect{}                   -> ev
      PlannedNodeUp{}                       -> ev
      PlannedConnect connId _delays version ->
          PlannedConnect connId delays version
        where
          delays = mpConst $ csPairConst $ Delays $ 0 NE.:| []

genNodeUpData :: Gen NodeUpData
genNodeUpData = do
    keepAliveRng <- mkStdGen <$> arbitrary
    pure NodeUpData
      { keepAliveRng
      }

shortestTimeouts :: MpTuple (Maybe IOLike.DiffTime)
shortestTimeouts = MpTuple
  { mpBF = longWait
  , mpCS = Nothing
  , mpKA = Just 60   -- TODO: #2505 should be 10s.
  , mpTS = shortWait
  }

genPlan :: [NodeToNodeVersion] -> NumCoreNodes -> SlotLength -> Gen NetPlan
genPlan ntnVersions numCoreNodes slotLength = do
    initialJoinEvents <- forM coreNodeIds $ \coreNodeId -> do
      -- WLOG each node is connected bidirectionally to some previous nodes,
      -- which ensures the initial topology is connected.
      elders <- do
        let CoreNodeId i = coreNodeId
        if 0 == i then pure [] else
          flip suchThat (not . null) $
            sublistOf (enumCoreNodes (NumCoreNodes i))  -- NB excludes i

      let
        firstInstance = 0   -- TODO generate later instances too

        -- TODO should we /somehow/ align the occasional large delays so that
        -- the different mini protocols protocol channels and/or the
        -- client-server pair channels simulate contemporaneous network
        -- partitions?
        genDelays' :: Gen (MpTuple (ClientServerPair Delays))
        genDelays' =
            sequence $ fmap sequence $
                mpConst (\maxDelay -> csPairConst (genDelays maxDelay))
            <*> fmap f shortestTimeouts
          where
            -- TODO magic number
            --
            -- TODO should this depend on the active slot coefficient?
            dflt = 20 * nominalDelay (getSlotLength slotLength)

            -- TODO why must we divide by two? are these timeouts actually
            -- round-trip timeouts?
            f x = maybe id (min . (/2)) x dflt

      initialConnectionEvents <- foldM
        ( \acc elder -> do
          d1 <- genDelays'
          d2 <- genDelays'
          v1 <- elements ntnVersions   -- TODO fix one version per node pair?
          v2 <- elements ntnVersions
          pure $
            PlannedConnect
              (ConnectionId (ClientServerPair coreNodeId elder) firstInstance)
              d1
              v1 :
            PlannedConnect
              (ConnectionId (ClientServerPair elder coreNodeId) firstInstance)
              d2
              v2 :
            acc
        )
        []
        elders

      nodeUpData <- genNodeUpData

      pure $
        PlannedNodeUp (NodeId coreNodeId firstInstance) nodeUpData :
        initialConnectionEvents

    -- TODO dynamic connects and disconnects

    pure $
      -- all core nodes join and connect at the very beginning of the
      -- simulation
      eventsNetPlan 0 $ concat initialJoinEvents
  where
    coreNodeIds = enumCoreNodes numCoreNodes

genNetTestSetup ::
     [NodeToNodeVersion]
     -- ^ the set of versions supported by all nodes
  -> (SecurityParam -> EpochSize)
  -> Gen NetTestSetup
genNetTestSetup ntnVersions mkEpochSize = do
    -- TODO magic number
    commonK      <- SecurityParam          <$> choose (2, 8)
    -- TODO magic number
    numCoreNodes <- NumCoreNodes           <$> choose (2, 5)
    -- TODO magic number
    slotLength   <- slotLengthFromMillisec <$> choose (100, 10000)

    let epochSize = mkEpochSize commonK
    netPlan      <- genPlan ntnVersions numCoreNodes slotLength

    -- TODO magic number
    let tot = 1000

    pure NetTestSetup
      { commonK
      , future      = HFF.singleEraFuture slotLength epochSize
      , mbEndOfDays = Just tot
      , plan        = truncateNetPlan tot netPlan
      }

exampleMockPraosArguments :: forall m.
     IOLike m
  => Tracer m String
  -> (Tracer m (), Tracer m (ThrottlerState m))
  -> NetTestSetup
  -> PraosTestSetup
  -> Map CoreNodeId (Tracer m (SelectionEvent MockPraosBlock), Tracer (IOLike.STM m) (StmEvent MockPraosBlock))
  -> Tracer m PlannedEvent
  -> Arguments m
exampleMockPraosArguments dbgTracer throttlerTracers netTestSetup praosTestSetup tracers tracerPlannedEvent =
    Arguments
      netArguments
      (mkPraosNodeArguments tracers praosTestSetup (plannedNumCoreNodes plan))
  where
    NetTestSetup
      { commonK
      , future
      , plan
      , mbEndOfDays
      } = netTestSetup

    (enterObserving, endOfDays) = throttlerTracers

    netArguments = NetArguments
      { MockNet.commonK
      , MockNet.firstProperBlockNo   = 0
      , MockNet.future
      , MockNet.mbEndOfDays
      , MockNet.plan
      , MockNet.systemStart          =
          getSystemStart $
          (error "Mock getSystemStart is const" :: BlockConfig MockPraosBlock)
      , MockNet.tracerThrottlerEvent = Tracer $ \ev -> case ev of
          EndOfDays prevStat
            -> do
            traceWith endOfDays prevStat
            traceWith dbgTracer $
              replicate 200 '-' <> " CLOCK IS EXHAUSTED  " <> replicate 200 '-'
          FlushingNewMessages{}
            -> traceWith dbgTracer $ show ev
          FlushingNoneInflight{}
            -> traceWith dbgTracer $ show ev
          FlushingOnce{}
            -> traceWith dbgTracer $ show ev
          FlushingQuiescence flushingState
            -> do
            traceWith dbgTracer $ show ev
            case flushingState of
              FlushingState1{}    -> pure ()
              FlushingState2{}    -> traceWith enterObserving ()
              FlushingStateLast{} -> pure ()
          ForgeTransition Observing{}
            -> error "impossible!"   -- only the flusher transitions to this state
          ForgeTransition Flushing1{}
            -> pure ()
          ForgeTransition Extending{}
            -> pure ()
                 -- a forge exit transitions to this state when the leader did
                 -- not succeed (eg was suppressed)
          ForgeTransition Flushing2{}
            -> pure ()
          ForgeTransition LastFlush{}
            -> error "impossible!"   -- only the timeout transitions to this state
          ForgeTransition Shutdown{}
            -> error "impossible!"   -- only the flusher transitions to this state
      , MockNet.tracerPlannedEvent
      , MockNet.usherNetConfig       = error "unused"
      }

{-------------------------------------------------------------------------------
  Entry points
-------------------------------------------------------------------------------}

simulateExample :: IOLike.DiffTime -> Either Failure ()
simulateExample endOfDays =
    runSim $
    MockNet.simulateNet $
    exampleMockPraosArguments
      nullTracer
      (nullTracer, nullTracer)
      NetTestSetup
        { commonK    = SecurityParam 5
        , future     =
            HFF.singleEraFuture (slotLengthFromMillisec 1000) (EpochSize 100)
        , mbEndOfDays = Just endOfDays
        , plan       = truncateNetPlan endOfDays examplePlan
        }
      PraosTestSetup
        { activeSlotCoeff = 0.5
        , initialNonce    = 2635227082147483398
        }
      tracers
      nullTracer
  where
    tracers ::
         (Monad m, Monad (IOLike.STM m))
      => Map CoreNodeId
         ( Tracer m (SelectionEvent MockPraosBlock)
         , Tracer (IOLike.STM m) (StmEvent MockPraosBlock)
         )
    tracers =
        Map.fromList $
        [ (,) coreNodeId $
          flip (,) nullTracer $
          condTracing
            ( \case
              HarnessEvent (MpEvent _ _ _ (End (IOLike.ExitCaseException e)))
                | Just AsyncCancelled <- IOLike.fromException e -> False
              HarnessEvent{} -> True
              _              -> True
            ) $
          showTracing $
          contramap (\s -> condense coreNodeId <> " " <> s) $
          nullTracer
        | coreNodeId <-
            enumCoreNodes $ plannedNumCoreNodes $
            truncateNetPlan endOfDays examplePlan
        ]

simulateExampleIO :: IO ()
{-# INLINE simulateExampleIO #-}
simulateExampleIO = undefined {- do
    praosTestSetup <- generate $ genPraosTestSetup
    netTestSetup   <- generate $
      genNetTestSetup
        praosNtnVersions
        (mkPraosEpochSize praosTestSetup)

    let trace = runST $ mkTrace netTestSetup praosTestSetup
    (_finalNetData, _finalNodeDataMap, mbResult) <- processTraceIO go trace
    case removeAllDelays $ plan netTestSetup of
      NetPlan m ->
          sequence_ $
          [ print (s, ev)
          | (s, evs) <- Map.toAscList m, ev <- NE.toList evs
          ]
    print netTestSetup{plan = emptyNetPlan}
    print praosTestSetup

    forM_ mbResult $ \(totExt, m) -> do
      let set = Set.fromList $ Map.elems m
      putStrLn $ show totExt <> " reset slots"
      putStrLn $ case Set.maxView set of
        Just (x, rest)
          | Set.null rest
          -> "Final stats OK: "  <> show x
        _ -> "Final stats BAD: " <> show m
  where
    praosNtnVersions =
        Map.keys $ supportedNodeToNodeVersions (Proxy :: Proxy MockPraosBlock)

    go :: Show a => (Trace a -> IO x) -> Trace a -> IO x
    go  tr kont = case tr of
        Trace time tid tlbl ev tr'
          -> do
          hand time tid tlbl ev
          go tr' kont
        TraceMainReturn time a threads
          -> do
          putStrLn $ "TraceMainReturn " <> show (time, threads)
          kont (Just a)
        ev@(TraceMainException time e _threads)
          -> do
          print ev
          kont Nothing
          throwIO $ Unhandled time e
        ev@TraceDeadlock{}
          -> do
          print ev
          kont Nothing

    hand time tid tlbl = \case
        EventSay s
          -> putStrLn $ show (time, tid, tlbl) <> " Say " <> s
        EventThreadUnhandled e
          -> case IOLike.fromException e of
            Just AsyncCancelled -> pure ()
            _                   ->
                putStrLn $ show (time, tid, tlbl) <> " Unhandled " <> show e
        _ -> pure ()
-}

data Unhandled = Unhandled !Time SomeException
 deriving (Show)

instance Exception Unhandled

{-------------------------------------------------------------------------------
  Instrumented simulation
-------------------------------------------------------------------------------}

data Observation
  = ObserveEnterObserving
  | ObserveEndOfDays String

  | ObserveSpan NumSlots

  | ObserveImmutablePoint CoreNodeId (Point MockPraosBlock)
  | ObserveVolatileBlockNo CoreNodeId (WithOrigin BlockNo)

  | ObserveSwitch NumBlocks NumSlots

-- | NOTE this is the /lazy/ ST monad
mkTrace :: forall s.
     NetTestSetup
  -> PraosTestSetup
  -> ST s (Trace ())
{-# INLINE mkTrace #-}
mkTrace netTestSetup praosTestSetup = runSimTraceST $ do
    let updWorstSpan :: AnchoredFragmentStats -> IOSim s ()
        updWorstSpan afStats =
            when (maxRollbacks commonK == unNumBlocks afLength) $
            traceM $ ObserveSpan afSpan
          where
            AnchoredFragmentStats
              { afLength
              , afSpan
              } = afStats

    let tracers ::
          Map CoreNodeId
          ( Tracer (IOSim s) (SelectionEvent MockPraosBlock)
          , Tracer (STMSim s) (StmEvent MockPraosBlock)
          )
        tracers =
            Map.fromList $
            [ (,) coreNodeId $
              flip (,) nullTracer $
              Tracer $ \case
                AmLeader{}
                  -> pure ()
                DecidedToFetch{}
                  -> pure ()
                DownloadedBlock{}
                  -> pure ()
                ev@DownloadedBlockCS{}
                  -> say (show ev)
                DownloadedHeader{}
                  -> pure ()
                ExtendedSelection _p bno afStats _prev
                  -> do
                  traceM $ ObserveVolatileBlockNo coreNodeId bno
                  updWorstSpan afStats
                FinalSelection immPoint _volPoint
                  -> do
                  traceM $ ObserveImmutablePoint coreNodeId immPoint
                ForgedBlock _sl _p _bno _prev
                  -> pure ()
                HarnessEvent _ev
                  -> pure ()
                ev@Humoring{}
                  -> say (show ev)
                SwitchedSelection _p bno afStats _prev (oldSuf, newSuf)
                  -> do
                  traceM $ ObserveVolatileBlockNo coreNodeId bno
                  updWorstSpan afStats
                  do
                    let AnchoredFragmentStats
                          { afLength
                          } = oldSuf
                    let AnchoredFragmentStats
                          { afSpan
                          } = newSuf
                    traceM $ ObserveSwitch afLength afSpan
          | coreNodeId <- enumCoreNodes $ plannedNumCoreNodes (plan netTestSetup)
          ]

    let dbgTracer = Tracer say
        throttlerTracers =
            ( Tracer $ \() -> traceM ObserveEnterObserving
            , Tracer $ \st -> traceM $ ObserveEndOfDays $ case st of
                Observing{}       -> "Observing"
                Flushing1{}       -> "Flushing1"
                Extending Nothing -> "ExtendingNothing"
                Extending Just{}  -> "ExtendingJust"
                Flushing2{}       -> "Flushing2"
                LastFlush{}       -> error "impossible!"
                Shutdown{}        -> error "impossible!"
            )
    MockNet.simulateNet $ exampleMockPraosArguments
      dbgTracer
      throttlerTracers
      netTestSetup
      praosTestSetup
      tracers
      nullTracer
  where
    NetTestSetup
      { commonK
      } = netTestSetup

{-------------------------------------------------------------------------------
  QC Main
-------------------------------------------------------------------------------}

data TestSetup = TestSetup
  { netTestSetup   :: NetTestSetup
  , praosTestSetup :: PraosTestSetup
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
      praosTestSetup <- genPraosTestSetup

      netTestSetup   <- genNetTestSetup
        praosNtnVersions
        (mkPraosEpochSize praosTestSetup)

      pure TestSetup
        { netTestSetup
        , praosTestSetup
        }
    where
      praosNtnVersions =
          Map.keys $
          supportedNodeToNodeVersions (Proxy :: Proxy MockPraosBlock)

-- | Simulates via 'mkTrace' and then checks each of the following
--
-- o 'prop_FinalNodeDataMap'
prop_PraosSimulation :: TestSetup -> Property
prop_PraosSimulation testSetup =
    counterexample "==================== CONTEXT ====================" $
    contextShow "k" (maxRollbacks commonK) $
    contextShowOnFAIL
      "plan with the illegible delays removed"
      ( case removeAllDelays plan of
          NetPlan m ->
              [ (s, ev) | (s, evs) <- Map.toAscList m, ev <- NE.toList evs ]
      ) $
    contextShowOnFAIL
      "legible netTestSetup"
      netTestSetup{plan = emptyNetPlan} $
    contextShowOnFAIL "praosTestSetup" praosTestSetup $
    ioProperty $
    flip fmap (processTraceIO netTestSetup trace) $ \(_numEvents, result, mbError) ->
    let CumulativeObservations
          { numEnterObserving
          , tips
          , truncatedState
          , worstSpan
          , worstSwitchLength
          , worstSwitchSpan
          } = result

        minBno = foldMap (Just . Min . volTipBno) finalNodeDataMap

        finalNodeDataMap :: Map CoreNodeId (FinalNodeData MockPraosBlock)
        finalNodeDataMap =
            tips <&> \(Id volTipBno, Id immTipPoint) -> FinalNodeData
              { volTipBno
              , immTipPoint
              }

        -- Praos stability window
        w :: Int
        w =
            ceiling $
            3 * fromIntegral (maxRollbacks commonK) / activeSlotCoeff
    in
    contextShowOnFAIL "simulation result" result $
--    context "numEvents / endOfDays" (flip showMaybe mbEndOfDays $ \endOfDays -> showTens (numEvents `div` fromIntegral (unNumSlots numSlots))) $
    contextShow "numEnterObserving" (unId numEnterObserving) $
    context ("truncatedState, " <> show numCoreNodes) (maybe "aborted" id $ unId truncatedState) $
    context ("truncatedState, " <> show commonK) (maybe "aborted" id $ unId truncatedState) $
    context
      ("worstSpan / (k / f) * 100, " <> show numCoreNodes)
      ( showMaybe (showTens . (\x->x :: Int) . round . (100*)) $
        fmap
          (\x ->   fromIntegral (unNumSlots x)
                 / (fromIntegral (maxRollbacks commonK) / activeSlotCoeff)
          )
          (unId worstSpan)
      ) $
    contextShowOnFAIL ("minimum final bno, " <> show commonK) minBno $
    contextShow ("worstSwitchLength, " <> show commonK) worstSwitchLength $
    context
      ("worstSwitchSpan / stabilityWindow * 100, " <> show commonK)
      ( showTens $
        (round :: Double -> Int) $
        100 *
          fromIntegral (unNumSlots (unId worstSwitchSpan))
        / fromIntegral w
      ) $
    case mbError of
      Just e  -> testFAIL $ "io-sim failure! " <> show e
      Nothing ->
          prop_FinalNodeDataMap netTestSetup praosTestSetup finalNodeDataMap
  where
    TestSetup
      { netTestSetup
      , praosTestSetup
      } = testSetup
    NetTestSetup
      { commonK
      , plan
      } = netTestSetup
    PraosTestSetup
      { activeSlotCoeff
      } = praosTestSetup

    numCoreNodes = plannedNumCoreNodes plan

    trace = runST $ mkTrace netTestSetup praosTestSetup

data FinalNodeData blk = FinalNodeData
  { immTipPoint :: Point blk
    -- ^ The point of the node's final immutable tip
  , volTipBno   :: WithOrigin BlockNo
    -- ^ The bno of the node's final volatile tip
  }
  deriving (Eq, Generic, Ord)
  deriving Show via Quiet (FinalNodeData blk)

-- | Check that all nodes have the exact same 'FinalNodeData'.
prop_FinalNodeDataMap ::
     StandardHash blk
  => NetTestSetup
  -> PraosTestSetup
  -> Map CoreNodeId (FinalNodeData blk)
  -> Property
prop_FinalNodeDataMap _netTestSetup _praosTestSetup finalNodeDataMap =
    case Set.maxView $ Set.fromList $ Map.elems finalNodeDataMap of
      Nothing
        -> testFAIL "No final node data!"
      Just (_finalNodeData, rest)
        | not $ Set.null rest
        -> testFAIL "Differing final immutable tip points!"
        | otherwise
        ->
{-
        context
          "final BlockNo / (numSlots * f) * 100"
          (showOrigin
            (showTens . (\x->x :: Int) . round . (100*) . relativeSpeed)
            volTipBno
          ) $
-}
        testPASS
        where
{-
          FinalNodeData
            { volTipBno
            } = finalNodeData -}
  where
{-    NetTestSetup
      { mbEndOfDays
      } = netTestSetup
    PraosTestSetup
      { activeSlotCoeff
      } = praosTestSetup -}

{-
    relativeSpeed volTipBno =
           fromIntegral (unBlockNo volTipBno)
        / (fromIntegral (unNumSlots (fromJust mbEndOfDays)) * activeSlotCoeff)   -- TODO
-}

tests :: TestTree
tests = testGroup "Praos ThreadNet simulations" $
    [ testProperty "arbitrary TestSetup" $ \testSetup ->
        prop_PraosSimulation testSetup
    ]

_main :: IO ()
_main = defaultMain tests

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

_traceResultIO :: Trace a -> IO (Int, Either Failure a)
_traceResultIO = go1 0
  where
    go1 !acc thunk =
        (IOLike.try $ IOLike.evaluate thunk) >>= \case
          Left exn -> pure (acc, Left (FailureException exn))
          Right t  -> go2 (acc + 1) t

    go2 !acc = \case
      Trace _ _ _ (EventSay s) t | s /= s -> do
          putStrLn s
          go1 (acc + 1) t
      Trace _ _ _ _ t                -> go1 (acc + 1) t
      TraceMainReturn _ _ tids@(_:_) -> pure (acc, Left (FailureSloppyShutdown tids))
      TraceMainReturn _ x _          -> pure (acc, Right x)
      TraceMainException _ e _       -> pure (acc, Left (FailureException e))
      TraceDeadlock _ x              -> pure (acc, Left (FailureDeadlock x))

data CumulativeObservations f = CumulativeObservations
  { numEnterObserving :: f Word64
    -- ^ How many times the Throttler entered 'Observing', excluding the fact
    -- that the simulation begins there.

  , tips :: Map CoreNodeId (f (WithOrigin BlockNo), f (Point MockPraosBlock))

  , truncatedState    :: f (Maybe String)
    -- ^ What was the state at the end of days?

  , worstSpan         :: f (Maybe NumSlots)
    -- ^ How many slots it took some selected chain to span k blocks
    --
    -- 'Nothing' iff no such chain ever spanned k blocks

  , worstSwitchLength :: f NumBlocks
    -- ^ The length of the old suffix
  , worstSwitchSpan   :: f NumSlots
    -- ^ The span of the new suffix
  }

deriving instance Show (CumulativeObservations Id)
  
traverseCO ::
     Applicative i
  => (forall x. f x -> i (g x))
  -> CumulativeObservations f
  -> i (CumulativeObservations g)
traverseCO f x = do
    numEnterObserving <- f numEnterObserving

    tips <- traverse (\(x0, x1) -> (,) <$> f x0 <*> f x1) tips

    truncatedState <- f truncatedState

    worstSpan <- f worstSpan

    worstSwitchLength <- f worstSwitchLength
    worstSwitchSpan   <- f worstSwitchSpan

    pure CumulativeObservations
      { numEnterObserving
      , tips
      , truncatedState
      , worstSpan
      , worstSwitchLength
      , worstSwitchSpan
      }
  where
    CumulativeObservations
      { numEnterObserving
      , tips
      , truncatedState
      , worstSpan
      , worstSwitchLength
      , worstSwitchSpan
      } = x
    
newtype Id a = Id {unId :: a}
  deriving (Foldable, Functor, Generic, Traversable)
  deriving Show via Quiet (Id a)

processTraceIO :: NetTestSetup -> Trace () -> IO (Int, CumulativeObservations Id, Maybe Failure)
processTraceIO netTestSetup trace = do
    tips <- Map.fromList <$> sequence [ do
        bnoRef      <- newIORef Origin
        immPointRef <- newIORef GenesisPoint
        pure (coreNodeId, (bnoRef, immPointRef))
      | coreNodeId <- enumCoreNodes $ plannedNumCoreNodes (plan netTestSetup)
      ]

    numEnterObserving <- newIORef 0
    truncatedState    <- newIORef Nothing

    worstSpan         <- newIORef Nothing
    worstSwitchLength <- newIORef $ NumBlocks 0
    worstSwitchSpan   <- newIORef $ NumSlots 0

    go1
      0
      CumulativeObservations
        { numEnterObserving
        , truncatedState
        , tips
        , worstSpan
        , worstSwitchLength
        , worstSwitchSpan
        }
      trace
  where
    finish acc refsCO x = do
        result <- traverseCO (fmap Id . readIORef) refsCO
        pure (acc, result, x)

    go1 !acc refsCO thunk =
        (IOLike.try $ IOLike.evaluate thunk) >>= \case
          Left exn -> finish acc refsCO $ Just $ FailureException exn
          Right t  -> go2 (acc + 1) refsCO t

    go2 !acc refsCO = \case
        Trace _ _ _ ev t -> do
            go3 refsCO ev
            go1 (acc + 1) refsCO t
        TraceMainReturn _ _ tids@(_:_) ->
            finish acc refsCO $ Just $ FailureSloppyShutdown tids
        TraceMainReturn _ () _         -> finish acc refsCO Nothing
        TraceMainException _ e _       ->
            finish acc refsCO $ Just $ FailureException e
        TraceDeadlock _ x              ->
            finish acc refsCO $ Just $ FailureDeadlock x


    go3 refsCO = \case
        EventSay msg ->
            asTypeOf (pure ()) $
            putStrLn msg
        EventLog dyn -> forM_ (fromDynamic dyn) $ \case
            ObserveEnterObserving -> modifyIORef' numEnterObserving (+1)
            ObserveEndOfDays s    -> writeIORef truncatedState $ Just s

            ObserveSpan span ->
                modifyIORef' worstSpan $ Just . maybe span (max span)

            ObserveImmutablePoint coreNodeId immPoint ->
                forM_ (Map.lookup coreNodeId tips) $
                \(_volBnoRef, immPointRef) -> writeIORef immPointRef immPoint
            ObserveVolatileBlockNo coreNodeId volBno ->
                forM_ (Map.lookup coreNodeId tips) $
                \(volBnoRef, _immPointRef) -> writeIORef volBnoRef volBno

            ObserveSwitch numDropped numSpanned -> do
                modifyIORef' worstSwitchLength $ max numDropped
                modifyIORef' worstSwitchSpan   $ max numSpanned
        _ -> pure ()
      where
        CumulativeObservations
          { numEnterObserving
          , tips
          , truncatedState
          , worstSpan
          , worstSwitchLength
          , worstSwitchSpan
          } = refsCO

testPASS :: Property
testPASS = property True

testFAIL :: String -> Property
testFAIL s =
    counterexample "==================== FAILURE ====================" $
    counterexample s $
    property False

contextShow :: (Testable prop, Show a) => String -> a -> prop -> Property
contextShow key val = context key (show val)

-- | Include key and value in QC output for PASS and for FAIL
context :: Testable prop => String -> String -> prop -> Property
context key val =
    tabulate key [val] .
    contextOnFAIL key val

contextShowOnFAIL :: (Testable prop, Show a) => String -> a -> prop -> Property
contextShowOnFAIL key val = contextOnFAIL key (show val)

-- | Include key and value in QC output only for FAIL
contextOnFAIL :: Testable prop => String -> String -> prop -> Property
contextOnFAIL key val =
    counterexample (key <> ": " <> val)

_showOrigin :: (a -> String) -> WithOrigin a -> String
_showOrigin f = \case
    Origin -> "Origin"
    At x   -> f x

showMaybe :: (a -> String) -> Maybe a -> String
showMaybe f = \case
    Nothing -> "Nothing"
    Just x  -> f x

showTens :: (Integral a, Show a) => a -> String
showTens x =
    "in [" <> show (10 * y) <> ", " <> show (10 * (y + 1)) <> ")"
  where
    y = div x 10
