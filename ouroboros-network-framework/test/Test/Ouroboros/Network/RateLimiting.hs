{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.RateLimiting where


import           Control.Monad (when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), contramapM)
import           Data.List (scanl')

import           Ouroboros.Network.Server.RateLimiting

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Show.Functions ()


--
-- The list of all properties
--

tests :: TestTree
tests = testGroup "Ouroboros.Network.RateLimiting"
  [ testProperty "HardLimit" propRateLimit_NotExceedHardLimit
  , testProperty "SoftLimit" propRateLimit_SoftLimitDelay
  ]


--
-- Basic data types
--

-- | To emulate environment we generate 'Event's:  we either receive a new
-- connection, or one of the accepted connections is terminating.
--
-- 'DiffTime' - a delay between consecutive events.
--
data Event
      = IncomingConnection   DiffTime
      | ConnectionTerminated DiffTime
  deriving (Eq, Ord, Show)


data WithNumberOfConnections a = WithNumberOfConnections {
    message             :: a,
    numberOfConnections :: Int
  }



--
-- Generators
--


-- | A newtype wrapper for 'Arbitrary' instances.
--
newtype Arb a = Arb { getArb :: a }
  deriving (Eq, Ord, Show)


genEvent :: Gen Event
genEvent = frequency
    [ (6, IncomingConnection   . toDiffTime <$> arbitrary)
    , (4, ConnectionTerminated . toDiffTime <$> arbitrary)
    ]

toDiffTime :: NonNegative Int -> DiffTime
toDiffTime = fromIntegral . getNonNegative

fixEvents :: [Event] -> [Event]
fixEvents = go 0
  where
    go :: Int -> [Event] -> [Event]
    go _ []                                    = []
    go 0 (ConnectionTerminated _ : events)     =      go 0 events
    go n (ev@ConnectionTerminated {} : events) = ev : go (pred n) events
    go n (ev@IncomingConnection {}   : events) = ev : go (succ n) events

instance Arbitrary (Arb [Event]) where
    arbitrary = Arb . fixEvents <$> listOf genEvent

    shrink (Arb events) =
      [ Arb (fixEvents events')
      | events' <- shrinkList (const []) events
      ]


genAcceptedConnectionsLimit :: Int ->  Gen AcceptedConnectionsLimit
genAcceptedConnectionsLimit limit = do
    hardLimit <- resize limit arbitrary
    softLimit <- resize (fromIntegral hardLimit) arbitrary
    delay <- toDiffTime <$> arbitrary
    pure $ AcceptedConnectionsLimit {
        acceptedConnectionsHardLimit = hardLimit,
        acceptedConnectionsSoftLimit = softLimit,
        acceptedConnectionsDelay     = delay
      }

shrinkAcceptedConnectionsLimit :: AcceptedConnectionsLimit -> [AcceptedConnectionsLimit]
shrinkAcceptedConnectionsLimit (AcceptedConnectionsLimit hardLimit softLimit delay) =
  [ AcceptedConnectionsLimit hardLimit' softLimit delay
  | hardLimit' <- shrink hardLimit
  , hardLimit' >= softLimit
  ]
  ++
  [ AcceptedConnectionsLimit hardLimit softLimit' delay
  | softLimit' <- shrink softLimit
  , softLimit' >= 0
  ]
  ++
  [ AcceptedConnectionsLimit hardLimit softLimit delay'
  | delay' <- fromRational `map` shrink (toRational delay)
  , delay' >= 0 && delay' /= delay
  ]


instance Arbitrary (Arb ([Event], AcceptedConnectionsLimit)) where
    arbitrary = do
      Arb events <- arbitrary
      let limit = foldr (\case
                          IncomingConnection {}   -> succ
                          ConnectionTerminated {} -> pred)
                      0 events
      limits <-
        oneof [ genAcceptedConnectionsLimit (2 * limit)
              , genAcceptedConnectionsLimit (limit `div` 2)
              ]


      pure $ Arb (events, limits)

    shrink (Arb (events, limits)) =
      [ Arb (events', limits)
      | Arb events' <- shrink (Arb events)
      ]
      ++
      [ Arb (events, limits')
      | limits' <- shrinkAcceptedConnectionsLimit limits
      ]


--
-- Simulations
--

rateLimittingExperiment
    :: forall m.
       ( MonadAsync m
       , MonadTime  m
       , MonadDelay m
       )
    => Tracer m (WithNumberOfConnections AcceptConnectionsPolicyTrace)
    -> AcceptedConnectionsLimit
    -> [Event]
    -> m ()
rateLimittingExperiment tracer policy events0 = do
    v <- atomically $ newTVar 0

    let numberOfConnectionsSTM :: STM m Int
        numberOfConnectionsSTM = readTVar v

    runConnectionRateLimits (getTracer v) numberOfConnectionsSTM policy
      `race_`
      shedulingThread v events0
  where
    -- thread which schedules events
    shedulingThread :: TVar m Int -> [Event] -> m ()
    shedulingThread _v [] = pure ()
    shedulingThread v  (IncomingConnection delay : events) = do
      when (delay > 0)
        $ threadDelay delay
      atomically $ modifyTVar' v succ
      shedulingThread v events
    shedulingThread v  (ConnectionTerminated delay : events) = do
      when (delay > 0)
        $ threadDelay delay
      atomically $ modifyTVar' v pred
      shedulingThread v events

    -- tracer
    getTracer :: TVar m Int
              -> Tracer m AcceptConnectionsPolicyTrace
    getTracer v =
      contramapM
        (\msg -> WithNumberOfConnections msg <$> atomically (readTVar v))
        tracer


-- | Run 'rateLimittingExperiment' in 'IOSim', and return the trace.
--
runRateLimitExperiment :: AcceptedConnectionsLimit
                       -> [Event]
                       -> [WithNumberOfConnections AcceptConnectionsPolicyTrace]
runRateLimitExperiment policy events =
    selectTraceEventsDynamic
      $ runSimTrace
      $ rateLimittingExperiment (Tracer traceM) policy events


--
-- QuickCheck properties
--


-- | We never should exceed the hard limit.
--
propRateLimit_NotExceedHardLimit
    :: Arb ([Event], AcceptedConnectionsLimit)
    -> Property
propRateLimit_NotExceedHardLimit (Arb (events, policy)) =
      label (buckets $ numberOfTurnsAboveHardLimit policy events)
    $ isValid (runRateLimitExperiment policy events)
  where
    isValid :: [WithNumberOfConnections b]
            -> Bool
    isValid =
      all
        (\x -> numberOfConnections x
                <= fromIntegral (acceptedConnectionsHardLimit policy))


-- | When above soft limit and below the hard limit, the delay should be
-- between '0' and 'acceptedConnectionsDelay'.
--
propRateLimit_SoftLimitDelay
    :: Arb ([Event], AcceptedConnectionsLimit)
    -> Property
propRateLimit_SoftLimitDelay (Arb (events, policy)) =
      label (buckets $ numberOfTurnsAboveSoftLimit policy events)
    $ isValid $ runRateLimitExperiment policy events
  where
    isValid :: [WithNumberOfConnections AcceptConnectionsPolicyTrace]
            -> Bool
    isValid =
          all  (\case
                  WithNumberOfConnections (ServerTraceAcceptConnectionRateLimiting delay _) _ ->
                         0 < delay
                      &&
                         delay <= acceptedConnectionsDelay policy

                  WithNumberOfConnections ServerTraceAcceptConnectionHardLimit {} _ ->
                      False
                  WithNumberOfConnections ServerTraceAcceptConnectionResume {} _ ->
                      False)

        . filter (\x ->
                    numberOfConnections x > fromIntegral (acceptedConnectionsSoftLimit policy)
                 &&
                    numberOfConnections x < fromIntegral (acceptedConnectionsHardLimit policy))


--
-- QuickCheck labels
--

interpr :: Int -> Event -> Int -> Int
interpr hardLimit IncomingConnection {}   m = succ m `min` hardLimit
interpr _         ConnectionTerminated {} m = pred m `max` 0

numberOfTurnsAboveHardLimit :: AcceptedConnectionsLimit
                            -> [Event]
                            -> Int
numberOfTurnsAboveHardLimit AcceptedConnectionsLimit
                              {acceptedConnectionsHardLimit} =
      length
    . filter (>= hardLimit)
    . scanl' (flip (interpr hardLimit)) 0
  where
    hardLimit = fromIntegral acceptedConnectionsHardLimit


numberOfTurnsAboveSoftLimit :: AcceptedConnectionsLimit
                            -> [Event]
                            -> Int
numberOfTurnsAboveSoftLimit AcceptedConnectionsLimit
                              {acceptedConnectionsSoftLimit} =
      length
    . filter (>= softLimit)
    . scanl' (flip (interpr softLimit)) 0
  where
    softLimit = fromIntegral acceptedConnectionsSoftLimit


buckets :: Int -> String
buckets 0           = "0"
buckets i | i <= 5  = "1 - 5"
          | i <= 10 = "5 - 10"
buckets i =
    let _min = 10 * (i `div` 10)
        _max = _min + 10
    in show _min ++ " - " ++ show _max
