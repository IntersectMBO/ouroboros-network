{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Test.Ouroboros.Network.Diffusion.Policies where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.IOSim (runSimOrThrow)
import qualified Data.IntPSQ as Pq
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Network.Socket (SockAddr (..))
import           System.Random

import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.DeltaQ (SizeInBytes)
import           Ouroboros.Network.Diffusion.Policies
import           Ouroboros.Network.PeerSelection.Governor
import           Ouroboros.Network.PeerSelection.PeerMetric
import           Ouroboros.Network.PeerSelection.Types (PeerSource (..))

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Policies"
  [ testProperty "HotToWarm" prop_hotToWarm
  , testProperty "WarmToCold" prop_randomDemotion
  ]


newtype ArbitrarySockAddr = ArbitrarySockAddr SockAddr deriving (Eq, Ord, Show)

instance Arbitrary ArbitrarySockAddr where
    arbitrary = do
        ip <- arbitrary
        port <- arbitrary
        return $ ArbitrarySockAddr $
            SockAddrInet (fromIntegral (port :: Word16)) ip

data ArbitraryPeerInfo = ArbitraryPeerInfo {
    piFailCount :: !Int
  , piTepid     :: !Bool
  } deriving Show

instance Arbitrary ArbitraryPeerInfo where
    arbitrary = do
        tepid <- arbitrary
        failCnt <- oneof [ return 0
                         , choose (0, 10)
                         , choose (0, maxBound)
                         ]
        return $ ArbitraryPeerInfo failCnt tepid


data ArbitraryPolicyArguments = ArbitraryPolicyArguments {
    apaAvailable     :: Map SockAddr ArbitraryPeerInfo
  , apaPickNum       :: Int
  , apaHeaderMetric  :: SlotMetric SockAddr
  , apaFetchedMetric :: SlotMetric (SockAddr, SizeInBytes)
  , apaChurnMode     :: ChurnMode
  , apaDemotion      :: ArbitraryDemotion
  } deriving Show

data ArbitraryDemotion = ArbitraryWarmDemotion
                       | ArbitraryColdDemotion
                       deriving Show

instance Arbitrary ArbitraryDemotion where
    arbitrary = elements [ArbitraryWarmDemotion, ArbitraryColdDemotion]

newtype ArbitraryChurnMode = ArbitraryChurnMode ChurnMode deriving Show

instance Arbitrary ArbitraryChurnMode where
    arbitrary = ArbitraryChurnMode <$>
        elements [ChurnModeNormal, ChurnModeBulkSync]

instance Arbitrary ArbitraryPolicyArguments where
    arbitrary = do
        peer <- arbitrary
        peers_ <- arbitrary
        kpi <- arbitrary
        kpis <- arbitrary
        let available = Map.fromList $ zipWith fn (peer:peers_) (kpi:kpis)
            peers = Map.keys available
        pickNum <- oneof [ return 1
                         , return $ min 2 (Map.size available)
                         , choose (1, Map.size available)
                         ]
        hCnt <- choose (0, maxSamples)
        fCnt <- choose (0, maxSamples)
        let hSlotNo = take hCnt [1..maxSamples]
            fSlotNo = take fCnt [1..maxSamples]
        hm <- Pq.fromList <$> mapM (headerMetric peers) hSlotNo
        fm <- Pq.fromList <$> mapM (fetchedMetric peers) fSlotNo
        (ArbitraryChurnMode cm) <- arbitrary
        dm <- arbitrary

        return $ ArbitraryPolicyArguments available pickNum hm fm cm dm

     where
       maxSamples = 10

       fn :: ArbitrarySockAddr
          -> ArbitraryPeerInfo
          -> (SockAddr, ArbitraryPeerInfo)
       fn (ArbitrarySockAddr addr) kpi = (addr, kpi)

       headerMetric :: [SockAddr]
                    -> Int
                    -> Gen (Int, SlotNo, (SockAddr, Time))
       headerMetric peers slotNo = do
           peer <- elements peers
           return (slotNo, SlotNo $ fromIntegral slotNo, (peer, Time 0))

       fetchedMetric :: [SockAddr]
                    -> Int
                    -> Gen (Int, SlotNo, ((SockAddr, SizeInBytes), Time))
       fetchedMetric peers slotNo = do
           peer <- elements peers
           fetched <- choose (1, 0xffff)
           return (slotNo, SlotNo $ fromIntegral slotNo,
                   ((peer, fetched), Time 0))



prop_hotToWarm :: ArbitraryPolicyArguments
                 -> Int
                 -> Property
prop_hotToWarm args seed = runSimOrThrow $ prop_hotToWarmM args seed

-- Verify that there are no peers worse than the peers picked for demotion.
prop_hotToWarmM :: forall m.
                   ( MonadSTM m
                   , Monad (STM m)
                   )
                 => ArbitraryPolicyArguments
                 -> Int
                 -> m Property
prop_hotToWarmM ArbitraryPolicyArguments{..} seed = do
    let rng = mkStdGen seed
    rngVar <- newTVarIO rng
    cmVar <- newTVarIO apaChurnMode
    hVar <- newTVarIO apaHeaderMetric
    fVar <- newTVarIO apaFetchedMetric


    let policies = simplePeerSelectionPolicy
                        rngVar
                        (readTVar cmVar)
                        metrics
        metrics = PeerMetrics hVar fVar
    picked <- atomically $ policyPickHotPeersToDemote policies
                  (const PeerSourceLocalRoot)
                  peerConnectFailCount
                  peerIsTepid
                  (Map.keysSet apaAvailable)
                  apaPickNum
    noneWorse metrics picked

  where

    peerConnectFailCount p =
        maybe (error "peerConnectFailCount") piFailCount (Map.lookup p apaAvailable)

    peerIsTepid p =
        maybe (error "peerIsTepid") piTepid (Map.lookup p apaAvailable)

    noneWorse :: PeerMetrics m SockAddr
              -> Set SockAddr
              -> m Property
    noneWorse metrics pickedSet = do
        scores <- atomically $ case apaChurnMode of
                      ChurnModeNormal -> do
                          hup <- upstreamyness <$> getHeaderMetrics metrics
                          bup <- fetchynessBlocks <$> getFetchedMetrics metrics
                          return $ Map.unionWith (+) hup bup
                      ChurnModeBulkSync -> fetchynessBytes <$>
                          getFetchedMetrics metrics
        let (picked, notPicked) = Map.partitionWithKey fn scores
            maxPicked = maximum $ Map.elems picked
            minNotPicked = minimum $ Map.elems notPicked
        if Map.null notPicked || Map.null picked
           then return $ property True
           else return $
               counterexample (show maxPicked ++ " > " ++ show minNotPicked)
                 (maxPicked <= minNotPicked)
      where
        fn :: SockAddr -> a -> Bool
        fn peer _ = Set.member peer pickedSet


prop_randomDemotion :: ArbitraryPolicyArguments
                 -> Int
                 -> Property
prop_randomDemotion args seed = runSimOrThrow $ prop_randomDemotionM args seed


-- Verifies that Tepid (formely hot) or failing peers are more likely to get
-- demoted/forgotten.
prop_randomDemotionM :: forall m.
                        ( MonadSTM m
                        , Monad (STM m)
                        )
                     => ArbitraryPolicyArguments
                     -> Int
                     -> m Property
prop_randomDemotionM ArbitraryPolicyArguments{..} seed = do
    let rng = mkStdGen seed
    rngVar <- newTVarIO rng
    cmVar <- newTVarIO apaChurnMode
    hVar <- newTVarIO apaHeaderMetric
    fVar <- newTVarIO apaFetchedMetric


    let policies = simplePeerSelectionPolicy
                        rngVar
                        (readTVar cmVar)
                        metrics
        metrics = PeerMetrics hVar fVar
    doDemotion numberOfTries policies Map.empty


  where
    numberOfTries = 10000

    peerConnectFailCount p =
        maybe (error "peerConnectFailCount") piFailCount (Map.lookup p apaAvailable)

    peerIsTepid p =
        maybe (error "peerIsTepid") piTepid (Map.lookup p apaAvailable)

    doDemotion :: Int
               -> PeerSelectionPolicy SockAddr m
               -> Map SockAddr Int
               -> m Property
    doDemotion 0 _ countMap = do
        let (!nonTepids, !nonTepidSum, !tepids, !tepidSum) =
                foldl' byTepid (0,0,0,0) $ Map.toList countMap
            meanNonTepid = if nonTepids == 0
                              then 0 :: Double
                              else fromIntegral nonTepidSum /
                                     fromIntegral nonTepids
            meanTepid = if tepids == 0
                           then 0 :: Double
                           else fromIntegral tepidSum /
                                  fromIntegral tepids
        if apaPickNum == Map.size apaAvailable
           then return $ property True
           else if meanNonTepid /= 0 && meanTepid /= 0
           then return $ property $ meanNonTepid < meanTepid
           else return $ property True
      where
        kpiFilter :: ArbitraryPeerInfo -> Bool
        kpiFilter = case apaDemotion of
                     ArbitraryWarmDemotion -> piTepid
                     ArbitraryColdDemotion ->
                         (\kpi -> piFailCount kpi > 0)

        byTepid :: (Int, Int, Int, Int)
                -> (SockAddr, Int)
                -> (Int, Int, Int, Int)
        byTepid (!nonTepids, !nonTepidSum, !tepids, !tepidSum) (addr, cnt) =
            case Map.lookup addr apaAvailable of
                 Just kpi ->
                     if kpiFilter kpi
                        then ( nonTepids, nonTepidSum
                             , tepids + 1, tepidSum + cnt)
                        else ( nonTepids + 1, nonTepidSum + cnt
                             , tepids, tepidSum)
                 Nothing -> error "picked unknown addr"

    doDemotion !n policies countMap = do
        let policy = case apaDemotion of
                          ArbitraryWarmDemotion -> policyPickWarmPeersToDemote
                          ArbitraryColdDemotion -> policyPickColdPeersToForget
        picked <- atomically $ policy policies
                    (const PeerSourceLocalRoot)
                    peerConnectFailCount
                    peerIsTepid
                    (Map.keysSet apaAvailable)
                    apaPickNum
        if Set.size picked /= apaPickNum
           then return $ property False
           else do
               let countMap' = foldl' fn countMap picked
               doDemotion (n-1) policies countMap'
      where
        fn :: Map SockAddr Int -> SockAddr -> Map SockAddr Int
        fn m addr = Map.alter add addr m

        add :: Maybe Int -> Maybe Int
        add Nothing  = Just 1
        add (Just c) = Just $! c + 1
