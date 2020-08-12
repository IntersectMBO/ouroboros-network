{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Test.Ouroboros.Network.TipSample.Client (tests) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), contramap, nullTracer)

import           Control.Arrow (second, (***))

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.Serialise as Serialise (Serialise (..), DeserialiseFailure)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import           Data.Foldable (foldl')
import           Data.List (partition, sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import           System.Random (StdGen, mkStdGen)
import qualified System.Random as Random
import qualified Statistics.Quantile as Statistics

import           Cardano.Slotting.Slot (WithOrigin (..))

import           Network.TypedProtocol.Pipelined (N (..), Nat (Zero, Succ))

import           Ouroboros.Network.Block
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Mux (ControlMessage (..))
import           Ouroboros.Network.AnchoredFragment ( AnchoredFragment )
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.ChainFragment as CF

import           Ouroboros.Network.Protocol.TipSample.Type
import           Ouroboros.Network.Protocol.TipSample.Codec
import           Ouroboros.Network.Protocol.TipSample.Client
import           Ouroboros.Network.Protocol.TipSample.Server
import           Ouroboros.Network.TipSample.Client
import           Ouroboros.Network.TipSample.TipFragment ( Timed (..) )
import           Ouroboros.Network.TipSample.TipRegistry
import           Ouroboros.Network.Util.ShowProxy

import           Test.ChainGenerators hiding (tests)
import           Test.ChainFragment (TestHeaderChainFragment (..))
import           Ouroboros.Network.Testing.ConcreteBlock

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Ouroboros.Network.TipSample"
  [ testGroup "Client"
    [ testProperty "client and registry"   prop_tipSampleClient
    , testProperty "simpleTipSamplePolicy" prop_simpleTipSamplePolicy
    ]
  ]


data TestChainFragmentAndRange =
    TestChainFragmentAndRange (ChainFragment BlockHeader) Int Int Int
  deriving (Show, Eq)


data ClientId =
    Client0
  | Client1
  deriving (Show, Eq, Ord)


instance Arbitrary TestChainFragmentAndRange where
    arbitrary = do
        (prevhash, prevblock, prevslot) <- genChainAnchor
        n <- choose (3, 100)
        bodies <- vector n
        slots  <- mkSlots prevslot <$>
                    suchThat
                      (vectorOf n genSlotGap)
                      (\as -> length (filter (/= 0) as) >= 3)
        let chain = CF.mapChainFragment blockHeader
                  $ mkChainFragment prevhash prevblock (zip slots bodies)
        maxHistory <- choose (1, n)
        let Just (SlotNo maxSlot) = CF.headSlot chain
        maxSlotRange <- choose (1, fromIntegral maxSlot - 1)
        maxMultipleTipsRange <- choose (1, fromIntegral maxSlot - maxSlotRange)
        return $ TestChainFragmentAndRange chain maxHistory maxSlotRange maxMultipleTipsRange
      where
        mkSlots :: SlotNo -> [Int] -> [SlotNo]
        mkSlots (SlotNo prevslot) =
            map SlotNo . tail
          . scanl (\slot gap -> slot + fromIntegral gap) prevslot

    shrink (TestChainFragmentAndRange chain maxHistory maxSlotRange maxMultipleTipsRange)
        = if maxHistory > 1
            then [ TestChainFragmentAndRange chain (pred maxHistory) maxSlotRange maxMultipleTipsRange ]
            else []
          ++
          if maxSlotRange > 1
            then [ TestChainFragmentAndRange chain maxHistory (pred maxSlotRange) maxMultipleTipsRange ]
            else []
          ++
          if maxMultipleTipsRange > 1
            then [ TestChainFragmentAndRange chain maxHistory maxSlotRange (pred maxMultipleTipsRange) ]
            else []
          ++
          [ TestChainFragmentAndRange chain'
                                      (maxHistory `min` CF.length chain')
                                      maxSlotRange'
                                      maxMultipleTipsRange'
          | TestHeaderChainFragment chain' <- shrink (TestHeaderChainFragment chain)
          , CF.length chain' >= 3
          , let Just (maxSlotRange', maxMultipleTipsRange') =
                  fixRanges (CF.length chain') maxSlotRange maxMultipleTipsRange
          ]
      where
        -- invariant: @n + m < l@
        fixRanges :: Int -> Int -> Int -> Maybe (Int, Int)
        fixRanges l n m | n + m < l      = Just (n, m)
                        | n > 1 && m > 1 = fixRanges (pred n) (pred m) l
                        | n > 1          = fixRanges (pred n) m l
                        | m > 1          = fixRanges n (pred m) l
                        | otherwise      = Nothing



-- | Run two 'tipSampleClient''s against two servers  using 'Channels', returns
-- 'TipRegistry' results using 'getPeerResults' method.
--
tipSampleClientExperiment
    :: forall header m.
       ( MonadAsync         m
       , MonadDelay         m
       , MonadMonotonicTime m
       , MonadST            m
       , MonadSTM           m
       , MonadCatch         m
       , MonadThrow         m
       , HasHeader          header
       , ShowProxy          header
       , Show               header
       )
    => Tracer m (ClientId, TipSampleClientTrace header)
    -> AnchoredFragment header
    -> TipSamplePolicy
    -> Codec (TipSample (Tip header))
             Serialise.DeserialiseFailure
             m ByteString
    -> (Channel m ByteString, Channel m ByteString)
    -> (Channel m ByteString, Channel m ByteString)
    -> StdGen
    -> m [(NumberOfWins, NumberOfPeers, ClientId)]
tipSampleClientExperiment clientTracer af
                          tsp
                          codec
                          channels0
                          channels1
                          stdGen = do
    tipRegistry <-
      makeTipRegistry
        TipRegistryArguments {
            traChainOffset     = 0,
            traGetCurrentChain = pure af,
            traTracer          = nullTracer
          }
    let (stdGen0, stdGen1) = Random.split stdGen

    currentSlotNoVar <- newTVarM Origin

    tfc0 <- registerPeer tipRegistry Client0
    let client0 = makeTipSampleClient
                    ((Client0,) `contramap` clientTracer)
                    tfc0 currentSlotNoVar stdGen0

    tfc1 <- registerPeer tipRegistry Client1
    let client1 = makeTipSampleClient
                    ((Client1,) `contramap` clientTracer)
                    tfc1 currentSlotNoVar stdGen1

    _ <-
      (
        runPeer nullTracer codec (fst channels0) (tipSampleServerPeer (tipSampleServer af))
        `race_` 
        runPeer nullTracer codec (snd channels0) (tipSampleClientPeer client0)
      )
      `concurrently`
      (
        runPeer nullTracer codec (fst channels1) (tipSampleServerPeer (tipSampleServer af))
        `race_`
        runPeer nullTracer codec (snd channels1) (tipSampleClientPeer client1)
      )
      `concurrently`
      runTime currentSlotNoVar

    getPeerResults tipRegistry
  where
    -- we are gods, we are in control of time flow ;)
    runTime :: TVar m (WithOrigin SlotNo) -> m ()
    runTime var = go (AF.toOldestFirst af)
      where
        go [] = atomically $ do
          a <- readTVar var
          case a of
            Origin    -> writeTVar var (At $ SlotNo 1)
            At slotNo -> writeTVar var (At $ succ slotNo)
        go (h : hs) = do
          atomically $ writeTVar var (At $ blockSlot h)
          threadDelay 1
          go hs


    makeTipSampleClient :: Tracer m (TipSampleClientTrace header)
                        -> TipFragmentVar ClientId m header
                        -> TVar m (WithOrigin SlotNo)
                        -> StdGen
                        -> TipSampleClient (Tip header) m ()
    makeTipSampleClient tr (TipFragmentVar tsaModifyTipFragment) currentSlotNoVar stdGen' =
      let tsaGetCurrentSlotNo :: STM m SlotNo
          tsaGetCurrentSlotNo = do
            curSlotNo <- readTVar currentSlotNoVar
            case curSlotNo of
              Origin -> retry
              At slotNo -> pure slotNo

          -- We need to stop the client before it will request a tip which is
          -- outside of the chain.
          stopAction :: STM m ControlMessage
          stopAction = do
            mbCurrentSlot <- readTVar currentSlotNoVar
            let currentSlot =
                  case mbCurrentSlot of
                    Origin -> SlotNo 0
                    At slotNo -> slotNo
            -- check if the server will be able to find slots on the chain; It
            -- must contain slot 'tspSlotRange' away from the current slot plus
            -- 'tspMultipleTipsRange' elements on the rest of the chain.
            case CF.lookupBySlotFT
                    (AF.unanchorFragment af)
                    (currentSlot + fromIntegral (snd (tspSlotRange tsp))) of
              CF.Position _ _ cf'
                | fromIntegral (CF.length (CF.ChainFragment cf'))
                  >= snd (tspMultipleTipsRange tsp)
                ->
                pure Continue

              _ ->
                pure Terminate

      in tipSampleClient
               tsp
               TipSampleActions {
                   tsaModifyTipFragment,
                   tsaGetCurrentSlotNo,
                   tsaSlotTime = pure (const Nothing),
                   tsaTracer = tr
                 }
               stopAction
               stdGen'


-- | A 'TipSampleServer' which sends @'Tip' header@'s from the given chain.
--
tipSampleServer :: forall header m.
                   ( Monad m
                   , HasHeader header
                   , Show      header
                   )
                => AnchoredFragment header
                -> TipSampleServer (Tip header) m ()
tipSampleServer af = TipSampleServer $ pure (go (AF.unanchorFragment af))
  where
    go :: ChainFragment header -> ServerStIdle (Tip header) m ()
    go cf = ServerStIdle {
        handleFollowTip = handleFollowTip cf,
        handleDone
      }

    handleFollowTip :: forall (n :: N).
                       ChainFragment header
                    -> Nat (S n)
                    -> SlotNo
                    -> m (SendTips (S n) (Tip header) m ())
    handleFollowTip cf n0 slotNo =
        case CF.lookupBySlotFT cf slotNo of
          CF.Position _ header0 cf0 ->
            pure $ sendTips n0 header0 (CF.ChainFragment cf0)
          CF.OnLeft ->
            pure $ error "handleFollowTip: unexpected OnLeft"
          CF.OnRight ->
            pure $ error "handleFollowTip: unexpected OnRight"
          CF.Nowhere ->
            pure $ error "handleFollowTip: unexpected Nowhere"

      where
        sendTips :: forall (k :: N).
                    Nat (S k)
                 -> header
                 -> ChainFragment header
                 -> SendTips (S k) (Tip header) m ()
        sendTips n@(Succ Zero) header' cf' =
          SendLastTip n (getHeaderTip header') (go cf')
        sendTips n@(Succ n'@Succ{}) header' cf' =
          SendNextTip n (getHeaderTip header') $
            case cf' of
              CF.Empty -> error $ "tipSampleServer: chain invariant violation: "
                               ++ show (n, getHeaderTip header')
              header'' CF.:< cf'' ->
                pure $ sendTips n' header'' cf''


    getHeaderTip :: header -> Tip header
    getHeaderTip hdr = Tip (blockSlot hdr) (blockHash hdr) (blockNo hdr)

    handleDone :: m ()
    handleDone = pure ()


-- | Seed for 'StdGen'.
--
newtype Seed = Seed Int
  deriving Arbitrary via Large Int
  deriving Show


prop_tipSampleClient
    :: TestChainFragmentAndRange
    -> Seed
    -> ([Positive Integer], [Positive Integer])
    -- ^ delays applied to channels
    -> Property
prop_tipSampleClient (TestChainFragmentAndRange cf maxHistory maxSlotRange maxMultipleTipsRange)
                     (Seed seed)
                     (delays0, delays1) =
    (\tr -> validateResults tr $ traceInfo tr) $
    runSimTrace $ do

      delaysVar0 <- newTVarM $ (fromInteger . getPositive)
                             `map` (cycle $ case delays0 of {[] -> [Positive 0]; _ -> delays0})
      delaysVar1 <- newTVarM $ (fromInteger . getPositive)
                             `map` (cycle $ case delays1 of {[] -> [Positive 0]; _ -> delays1})
      channelPair0 <- second (modifyChannel delaysVar0) <$> createConnectedChannels
      channelPair1 <- second (modifyChannel delaysVar1) <$> createConnectedChannels

      tipSampleClientExperiment
        clientTracer
        (AF.mkAnchoredFragment AF.AnchorGenesis cf)
        policy
        codec
        channelPair0
        channelPair1
        (mkStdGen seed)
  where
    policy :: TipSamplePolicy
    policy = TipSamplePolicy {
          tspMaxHistory               = fromIntegral maxHistory,
          tspSlotRange                = (1, fromIntegral maxSlotRange),
          tspFollowSingleTipWeight    = 8,
          tspFollowMultipleTipsWeight = 2,
          tspMultipleTipsRange        = (2, fromIntegral maxMultipleTipsRange)
        }

    traceInfo :: Trace [(NumberOfWins, NumberOfPeers, ClientId)]
              -> Either Failure
                   ( [(NumberOfWins, NumberOfPeers, ClientId)]
                   , [(ClientId, TipSampleClientTrace BlockHeader)]
                   )
    traceInfo tr =
      let tr' = selectTraceEventsDynamic tr
      in case traceResult True tr of
        Left f   -> Left f
        Right rs -> Right (rs, tr')


    validateResults :: Trace a
                    -> Either Failure
                         ( [(NumberOfWins, NumberOfPeers, ClientId)]
                         , [(ClientId, TipSampleClientTrace BlockHeader)]
                         )
                    -> Property
    validateResults tr (Left failure) =
        counterexample
          (show failure ++ "\n" ++ (foldl' (\acc a -> acc ++ "\n" ++ show a) "" $ traceEvents tr))
          False
    validateResults _ (Right (results, trace)) =
        map (\(a, _, c) -> (a, c)) results === computeResults tts0 tts1
      where
        (tts0, tts1) = dataFromTrace *** dataFromTrace
                     $ partition ((Client0 ==) . fst)
                     $ trace

        dataFromTrace :: [(ClientId, TipSampleClientTrace BlockHeader)]
                      -> [Timed (Tip BlockHeader)]
        dataFromTrace = take maxHistory
                      . foldr go []
          where
            go (_, TipSampleClientProtocolValidationError {}) acc = acc
            go (_, TipSampleClientAddTip tt _)   acc = tt : acc
            go (_, TipSampleClientRollback tt _) acc = tt : acc

        -- we don't compute 'NumberOfPeers'; when there are only two peers it
        -- will not change the order of the results
        computeResults :: [Timed (Tip BlockHeader)]
                       -> [Timed (Tip BlockHeader)]
                       -> [(NumberOfWins, ClientId)]
        computeResults = go (NumberOfWins 0) (NumberOfWins 0)
          where
            go :: NumberOfWins
               -> NumberOfWins
               -> [Timed (Tip BlockHeader)]
               -> [Timed (Tip BlockHeader)]
               -> [(NumberOfWins, ClientId)]

            go !r0 !r1 [] [] =
                filter (\(n, _) -> n > NumberOfWins 0) $ sort [(r0, Client0), (r1, Client1)]

            go !r0 !r1 tf0@(tt0 : tf0') tf1@(tt1 : tf1') =
                case getTipSlotNo (timedData tt0) `compare` getTipSlotNo (timedData tt1) of
                  EQ -> if timedAt tt0 <= timedAt tt1
                          -- biased towards peers with smaller address
                          then go (succ r0) r1 tf0' tf1'
                          else go r0 (succ r1) tf0' tf1'
                  LT -> go r0 r1 tf0' tf1
                  GT -> go r0 r1 tf0  tf1'

            go !r0  r1 (_ : tf0') [] = go r0 r1 tf0' []

            go  r0 !r1 [] (_ : tf1') = go r0 r1 []  tf1'


    codec :: MonadST m
          => Codec (TipSample (Tip BlockHeader))
                   Serialise.DeserialiseFailure
                   m ByteString
    codec = codecTipSample encodeBlockHeaderTip decodeBlockHeaderTip
      where
        encodeBlockHeaderTip :: Tip BlockHeader -> CBOR.Encoding
        encodeBlockHeaderTip TipGenesis =
             CBOR.encodeListLen 1
          <> CBOR.encodeWord 0
        encodeBlockHeaderTip (Tip slot hash blockNo_) =
             CBOR.encodeListLen 4
          <> CBOR.encodeWord 1
          <> Serialise.encode slot
          <> Serialise.encode hash
          <> Serialise.encode blockNo_

        decodeBlockHeaderTip :: forall s. CBOR.Decoder s (Tip BlockHeader)
        decodeBlockHeaderTip = do
          l <- CBOR.decodeListLen
          t <- CBOR.decodeWord
          case (l, t) of
            (1, 0) -> pure TipGenesis
            (4, 1) -> Tip <$> Serialise.decode
                          <*> Serialise.decode
                          <*> Serialise.decode
            _      -> fail $ "decodeTip: failure " ++ show (l, t)


    -- modify receive side of clients using a random list of delays
    modifyChannel :: ( MonadDelay m
                     , MonadSTM   m
                     )
                  => TVar m [DiffTime]
                  -> Channel m ByteString
                  -> Channel m ByteString
    modifyChannel v =
      channelEffect
        (\_ -> pure ())
        (\_ -> do
          delay
            <- atomically $ do
              as <- readTVar v
              case as of
                a : as' -> writeTVar v as' $> a
                []      -> pure 0
          threadDelay delay)

    clientTracer :: forall s. Tracer (SimM s) (ClientId, TipSampleClientTrace BlockHeader)
    clientTracer = Tracer traceM


data TestSimplePolicyArgs = TestSimplePolicyArgs
    { tspaMaxHistory        :: Word
    , tspaNumberOfWarmPeers :: Word
    , tspaPeersPerSlot      :: Word
    , tspaSlotRange         :: Word
    }
  deriving Show

instance Arbitrary TestSimplePolicyArgs where
    arbitrary = do
      -- in the test 'tspaMaxHistory' also specifies how many experiments we
      -- run for each quickcheck.
      tspaMaxHistory <- choose (100, 300)
      -- we only test the range we are interested in
      tspaNumberOfWarmPeers <- choose (30, 70)
      tspaPeersPerSlot <- choose (5, tspaNumberOfWarmPeers)
      tspaSlotRange    <- choose (1, 2 * tspaNumberOfWarmPeers `div` tspaPeersPerSlot)
      pure $ TestSimplePolicyArgs {
          tspaMaxHistory,
          tspaNumberOfWarmPeers,
          tspaPeersPerSlot,
          tspaSlotRange
        }


newtype PeerId = PeerId Word
  deriving (Show, Eq, Ord)


prop_simpleTipSamplePolicy
    :: TestSimplePolicyArgs
    -> Seed
    -> Property
prop_simpleTipSamplePolicy
  TestSimplePolicyArgs {
      tspaMaxHistory,
      tspaNumberOfWarmPeers,
      tspaPeersPerSlot,
      tspaSlotRange
    }
  (Seed seed) =
    validate $ runGame $ PeerId `map` [0 .. pred tspaNumberOfWarmPeers]
  where
    validate :: Map SlotNo [PeerId]
             -> Property
    validate m =
      let v = Statistics.quantile
                Statistics.normalUnbiased
                1 3
                (Vector.fromList $ (realToFrac . length) `map` Map.elems m)

          -- We check that the first 3-quantile is larger than 75% of the
          -- expected mean. This means that less than 1/3 of all the slots, less
          -- than (0.75 * tspaPeersPerSlot) number of peers is fighting to win
          -- the slot.
          predicate = v >= 0.75 * realToFrac tspaPeersPerSlot
      in 
         label (mkLabel v)
       . counterexample (show m)
       -- we check that the 'predicate' is valid in at least 90% of cases
       . checkCoverage
       . cover 90.0 predicate "peers-per-slot"
       $ True

    mkLabel :: Double -> String
    mkLabel i = let l, mini, maxi :: Int
                    l = 10
                    mini = l * (round i `div` l)
                    maxi = mini + l
                in show mini ++ " - " ++ show maxi

    policy = simpleTipSamplePolicy
                tspaMaxHistory
                tspaNumberOfWarmPeers
                tspaPeersPerSlot
                tspaSlotRange

    runGame :: [PeerId] -> Map SlotNo [PeerId]
    runGame
        = snd
        . foldl'
            (\(g, acc) peerId ->
              case Random.split g of
                (g', g'') -> 
                  let !acc' = Map.unionWith (++)
                                (Map.fromList . map (,[peerId]) $ slots g')
                                acc
                  in (g'', acc'))
            (mkStdGen seed, Map.empty)

    -- list of slots produced by 'randomTipSampleDecision' run recursively
    slots :: StdGen -> [SlotNo]
    slots = go (SlotNo 0) []
      where
        go :: SlotNo -> [SlotNo] -> StdGen -> [SlotNo]
        go _currentSlot !acc _g
          | length acc >= fromIntegral tspaMaxHistory
          = reverse $ take (fromIntegral tspaMaxHistory) acc
        go currentSlot  !acc g =
          case randomTipSampleDecision policy g of
            (FollowSingleTip slotNo, g') ->
              let nextSlot = currentSlot + fromIntegral slotNo
              in go nextSlot (nextSlot : acc) g'
            (FollowMultipleTips slotNo range, g') ->
              let firstSlot = currentSlot + fromIntegral slotNo
                  lastSlot  = firstSlot + fromIntegral range
              in go lastSlot
                    ([lastSlot, pred lastSlot .. firstSlot] ++ acc)
                    g'
