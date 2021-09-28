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
import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim
import           Control.Tracer (showTracing, Tracer (..), traceWith)
import           Data.List (foldl', intercalate)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Word
import           Data.Ratio
import           System.Random

import           Ouroboros.Network.PeerSelection.LedgerPeers

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Text.Printf

tests :: TestTree
tests = testGroup "LedgerPeers"
  [ testProperty "Pick 100%" prop_pick100
  , testProperty "Pick" prop_pick
  ]

newtype ArbitraryRelayAccessPoint = ArbitraryRelayAccessPoint RelayAccessPoint

instance Arbitrary ArbitraryRelayAccessPoint where
    arbitrary =
      ArbitraryRelayAccessPoint <$>
        elements [ RelayAccessAddress (read "1.1.1.1")     1234
                 , RelayAccessDomain  "relay.iohk.example" 1234
                 ]

data StakePool = StakePool {
      spStake :: !Word64
    , spRelay :: NonEmpty RelayAccessPoint
    } deriving Show



instance Arbitrary StakePool where
    arbitrary = do
        stake <- choose (0, 1000000)
        (ArbitraryRelayAccessPoint firstRelay) <- arbitrary
        moreRelays <- map unAddr <$> arbitrary
        return $ StakePool stake (firstRelay :| moreRelays)
      where
        unAddr (ArbitraryRelayAccessPoint a) = a

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
        peerMap = accPoolStake sps
        tr = (runSimTrace $ pickPeers rng verboseTracer peerMap $ NumberOfPeers 1) in
    ioProperty $ do
        tr' <- evaluateTrace tr
        case tr' of
             SimException e trace -> do
                 return $ counterexample (intercalate "\n" $ show e : trace) False
             SimDeadLock trace -> do
                 return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
             SimReturn (_, peers) _trace -> do
                 -- printf "Log: %s\n" (intercalate "\n" _trace)
                 return $ peers === [ RelayAccessAddress (read "1.1.1.1") 1 ]

-- | Veify that given at least one peer we manage to pick `count` peers.
prop_pick :: LedgerPools
          -> Word16
          -> Word16
          -> Property
prop_pick (LedgerPools lps) count seed =
    let rng = mkStdGen $ fromIntegral seed
        peerMap = accPoolStake lps
        tr = runSimTrace (pickPeers rng verboseTracer peerMap $ NumberOfPeers count) in
    ioProperty $ do
        tr' <- evaluateTrace tr
        case tr' of
             SimException e trace -> do
                 return $ counterexample (intercalate "\n" $ show e : trace) False
             SimDeadLock trace -> do
                 return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
             SimReturn (_, peers) trace -> do
                 if null lps
                    then return $ property $ null peers
                    else return $ counterexample (intercalate "\n" $ "Lenght missmatch" : trace)
                                      (length peers == fromIntegral count)


-- TODO: Belongs in iosim.
data SimResult a = SimReturn a [String]
                 | SimException SomeException [String]
                 | SimDeadLock [String]

-- Traverses a list of trace events and returns the result along with all log messages.
-- Incase of a pure exception, ie an assert, all tracers evaluated so far are returned.
evaluateTrace :: Trace a -> IO (SimResult a)
evaluateTrace = go []
  where
    go as tr = do
      r <- try (evaluate tr)
      case r of
        Right (Trace _ _ _ (EventSay s) tr') -> go (s : as) tr'
        Right (Trace _ _ _ _ tr' )           -> go as tr'
        Right (TraceMainReturn _ a _)        -> pure $ SimReturn a (reverse as)
        Right (TraceMainException _ e _)     -> pure $ SimException e (reverse as)
        Right (TraceDeadlock _ _)            -> pure $ SimDeadLock (reverse as)
        Left  (SomeException e)              -> pure $ SimException (SomeException e) (reverse as)

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
