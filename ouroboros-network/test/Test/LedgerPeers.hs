{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Test.LedgerPeers where

import           Control.Exception (SomeException (..))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim hiding (SimResult)
import           Control.Tracer (showTracing, Tracer (..), traceWith)
import           Data.List (foldl', intercalate, nub)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.IP as IP
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Data.Ratio
import           System.Random

import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Network.Socket (SockAddr)
import           Network.DNS (Domain)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Text.Printf

tests :: TestTree
tests = testGroup "LedgerPeers"
  [ testProperty "Pick 100%" prop_pick100
  , testProperty "Pick" prop_pick
  ]

newtype ArbitraryPortNumber = ArbitraryPortNumber { getArbitraryPortNumber :: PortNumber }

instance Arbitrary ArbitraryPortNumber where
    arbitrary = elements
              $ map (ArbitraryPortNumber . read . show)
              $ ([1000..1100] :: [Int])

newtype ArbitraryRelayAccessPoint = ArbitraryRelayAccessPoint RelayAccessPoint

instance Arbitrary ArbitraryRelayAccessPoint where
    arbitrary =
      ArbitraryRelayAccessPoint <$>
        oneof [ RelayAccessAddress (read "1.1.1.1")     . getArbitraryPortNumber <$> arbitrary
              , RelayAccessDomain  "relay.iohk.example" . getArbitraryPortNumber <$> arbitrary
              ]

data StakePool = StakePool {
      spStake :: !Word64
    , spRelay :: NonEmpty RelayAccessPoint
    } deriving Show



instance Arbitrary StakePool where
    arbitrary = do
        stake <- choose (0, 1000000)
        (ArbitraryRelayAccessPoint firstRelay) <- arbitrary
        moreRelays <- filter (/= firstRelay) . nub . map unAddr <$> arbitrary
        return $ StakePool stake (firstRelay :| moreRelays)
      where
        unAddr (ArbitraryRelayAccessPoint a) = a

    shrink sp@StakePool { spStake, spRelay } =
      [ sp { spStake = spStake' }
      | spStake' <- shrink spStake
      ]
      ++
      [ sp { spRelay = NonEmpty.fromList spRelay' }
      | spRelay'@(_ : _) <- shrinkList (const [])
                                       (NonEmpty.toList spRelay)
      ]

newtype LedgerPools = LedgerPools [(PoolStake, NonEmpty RelayAccessPoint)]
  deriving Show

instance Arbitrary LedgerPools where
    arbitrary = LedgerPools . calculateRelativeStake <$> arbitrary

      where
        calculateRelativeStake :: [StakePool]
                               -> [(PoolStake, NonEmpty RelayAccessPoint)]
        calculateRelativeStake sps =
            let totalStake = foldl' (\s p -> s + spStake p) 0 sps in
            map (\p -> ( PoolStake (fromIntegral (spStake p) % fromIntegral totalStake)
                       , spRelay p)) sps

-- | A pool with 100% stake should allways be picked.
prop_pick100 :: Word16
             -> Property
prop_pick100 seed =
    let rng = mkStdGen $ fromIntegral seed
        sps = [ (1, RelayAccessAddress (read "1.1.1.1") 1  :| [])
              , (0, RelayAccessAddress (read "0.0.0.0") 0  :| [])
              ]

        sim :: IOSim s [RelayAccessPoint]
        sim = withLedgerPeers
                rng verboseTracer
                (pure (UseLedgerAfter 0))
                interface
                (\_ -> pure Map.empty) -- we're not relying on domain name resolution in this simulation
                (\request _ -> do
                  threadDelay 1900 -- we need to invalidate ledger peer's cache
                  resp <- request (NumberOfPeers 1)
                  pure $ case resp of
                    Nothing          -> []
                    Just (peers, _)  -> [ RelayAccessAddress ip port
                                        | Just (ip, port) <- IP.fromSockAddr
                                                         <$> Set.toList peers
                                        ]
                )
          where
            interface = LedgerPeersConsensusInterface $ \_ -> pure (Just (Map.elems (accPoolStake sps)))

    in ioProperty $ do
        tr' <- evaluateTrace (runSimTrace sim)
        case tr' of
             SimException e trace -> do
                 return $ counterexample (intercalate "\n" $ show e : trace) False
             SimDeadLock trace -> do
                 return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
             SimReturn peers _trace -> do
                 -- printf "Log: %s\n" (intercalate "\n" _trace)
                 return $ peers === [ RelayAccessAddress (read "1.1.1.1") 1 ]

-- | Veify that given at least one peer we manage to pick `count` peers.
prop_pick :: LedgerPools
          -> Word16
          -> Word16
          -> Property
prop_pick (LedgerPools lps) count seed =
    let rng = mkStdGen $ fromIntegral seed

        sim :: IOSim s [RelayAccessPoint]
        sim = withLedgerPeers
                rng (verboseTracer) --  <> Tracer Debug.traceShowM)
                (pure (UseLedgerAfter 0))
                interface resolve
                (\request _ -> do
                  threadDelay 1900 -- we need to invalidate ledger peer's cache
                  resp <- request (NumberOfPeers count)
                  pure $ case resp of
                    Nothing          -> []
                    Just (peers, _)  -> [ reverseLookup (RelayAccessAddress ip port)
                                        | Just (ip, port) <- IP.fromSockAddr
                                                      `fmap` Set.toList peers
                                        ]
                )
          where
            interface :: LedgerPeersConsensusInterface (IOSim s)
            interface = LedgerPeersConsensusInterface $ \_ -> pure (Just (Map.elems (accPoolStake lps)))

            domainMap :: Map Domain (Set IP)
            domainMap = Map.fromList [("relay.iohk.example", Set.singleton (read "2.2.2.2"))]

            resolve :: [DomainAccessPoint]
                    -> IOSim s (Map DomainAccessPoint (Set SockAddr))
            resolve = \daps ->
              pure $ Map.fromList
                     [ (dap, addrs)
                     | dap@(DomainAccessPoint domain port) <- daps
                     , let addrs = Set.map (\ip -> IP.toSockAddr (ip, port))
                                 . fromMaybe Set.empty
                                 $ Map.lookup domain domainMap
                     ]

            reverseLookup :: RelayAccessPoint -> RelayAccessPoint
            reverseLookup ap@(RelayAccessAddress ip port)
              = case [ domain
                     | (domain, addrs) <- Map.assocs domainMap
                     , ip `Set.member` addrs
                     ] of
                  (domain : _) -> RelayAccessDomain domain port
                  _            -> ap
            reverseLookup ap = ap



    in ioProperty $ do
        tr' <- evaluateTrace (runSimTrace sim)
        case tr' of
             SimException e trace -> do
                 return $ counterexample (intercalate "\n" $ show e : trace) False
             SimDeadLock trace -> do
                 return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
             SimReturn peers trace -> do
                 let numOfPeers = length peers
                 if null lps
                    then return $ property $ null peers
                    else return $ counterexample (intercalate "\n" $
                                                    ( "Lenght missmatch "
                                                      ++ show (length peers)
                                                    )
                                                    : trace)
                                      (numOfPeers
                                        == fromIntegral count `min` numOfPeers)

prop :: Property
prop = prop_pick (LedgerPools [(PoolStake {unPoolStake = 1 % 1},RelayAccessAddress (read "1.1.1.1") 1016 :| [])]) 0 2

-- TODO: Belongs in iosim.
data SimResult a = SimReturn a [String]
                 | SimException SomeException [String]
                 | SimDeadLock [String]

-- Traverses a list of trace events and returns the result along with all log messages.
-- Incase of a pure exception, ie an assert, all tracers evaluated so far are returned.
evaluateTrace :: SimTrace a -> IO (SimResult a)
evaluateTrace = go []
  where
    go as tr = do
      r <- try (evaluate tr)
      case r of
        Right (SimTrace _ _ _ (EventSay s) tr') -> go (s : as) tr'
        Right (SimTrace _ _ _ _ tr' )           -> go as tr'
        Right (TraceMainReturn _ a _)           -> pure $ SimReturn a (reverse as)
        Right (TraceMainException _ e _)        -> pure $ SimException e (reverse as)
        Right (TraceDeadlock _ _)               -> pure $ SimDeadLock (reverse as)
        Left  (SomeException e)                 -> pure $ SimException (SomeException e) (reverse as)

data WithThreadAndTime a = WithThreadAndTime {
      wtatOccuredAt    :: !Time
    , wtatWithinThread :: !String
    , wtatEvent        :: !a
    }

instance (Show a) => Show (WithThreadAndTime a) where
    show WithThreadAndTime {wtatOccuredAt, wtatWithinThread, wtatEvent} =
        printf "%s: %s: %s" (show wtatOccuredAt) (show wtatWithinThread) (show wtatEvent)

verboseTracer :: forall a m.
                       ( MonadAsync m
                       , MonadSay m
                       , MonadMonotonicTime m
                       , Show a
                       )
               => Tracer m a
verboseTracer = threadAndTimeTracer $ showTracing $ Tracer say

threadAndTimeTracer :: forall a m.
                       ( MonadAsync m
                       , MonadMonotonicTime m
                       )
                    => Tracer m (WithThreadAndTime a) -> Tracer m a
threadAndTimeTracer tr = Tracer $ \s -> do
    !now <- getMonotonicTime
    !tid <- myThreadId
    traceWith tr $ WithThreadAndTime now (show tid) s
