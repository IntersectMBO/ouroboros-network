{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TupleSections              #-}

-- 'TestAddress' 'Arbitrary' instance.
{-# OPTIONS_GHC -Wno-orphans                   #-}
-- 'ScheduleEntry' have partial fields.
{-# OPTIONS_GHC -Wno-partial-fields            #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-name-shadowing            #-}

module Test.Ouroboros.Network.ConnectionManager
  ( tests
  ) where

import           Prelude hiding (read)

import           Control.Exception (assert)
import           Control.Monad (forever, unless, when, (>=>))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), contramap, nullTracer)

import           GHC.Generics
import           GHC.Stack (HasCallStack)
import           GHC.IO.Exception

import           Data.Bifunctor (Bifunctor, bimap)
import           Data.Either (isLeft, isRight)
import           Data.Functor (void, ($>))
import           Data.Foldable (fold, foldl', maximumBy, traverse_, forM_)
import           Data.List (intercalate, sortOn)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (All (..), Any (..))
import qualified Data.Set as Set
import           Data.Void (Void)
import           Quiet

import           Network.Mux.Types

import           Test.QuickCheck hiding (shrinkMap)
import qualified Test.QuickCheck as QC
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.Snocket (Snocket (..), Accept (..), Accepted (..),
                   AddressFamily(TestFamily), TestAddress (..))
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.MuxMode
import           Ouroboros.Network.Server.RateLimiting
import qualified Ouroboros.Network.InboundGovernor.ControlChannel as ControlChannel

import           Ouroboros.Network.Testing.Utils (Delay (..),
                   genDelayWithPrecision)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.ConnectionManager"
  [ -- generators, shrinkers properties
    testGroup "generators"
    [ testProperty "ScheduleEntry generator"      prop_generator_ScheduleEntry
    , testProperty "ScheduleEntry shrinker"       prop_shrinker_ScheduleEntry
    , testProperty "RefinedSchedule generator"    prop_generator_RefinedSchedule
    , testProperty "RefinedSchedule shrinker"     prop_shrinker_RefinedSchedule
    , testProperty "ReuseSchedule generator"      prop_generator_ReuseSchedule
    , testProperty "RefinedScheduleMap generator" prop_generator_RefinedScheduleMap
    , testProperty "RefinedScheduleMap shrinker"  (withMaxSuccess 33 prop_shrinker_RefinedScheduleMap)
    , testProperty "fixupSchedule"                prop_fixupSchedule
    , testProperty "schedule"                     prop_schedule
    , testGroup "utils"
      [ testProperty "smallerList"                prop_smallerList
      , testProperty "smallerMap"                 prop_smallerMap
      ]
    ]
    -- connection manager simulation property
  , testGroup "simulations"
    [ testProperty "simulation"                     prop_connectionManagerSimulation
    , testProperty "overwritten"                    unit_overwritten
    , testProperty "timeoutExpired"                 unit_timeoutExpired
    ]
  ]


-- | Address type.  '0' indicates local address, the 'Arbitrary' generator only
-- returns (strictly) positive addresses.
--
type Addr = TestAddress Int


instance Arbitrary Addr where
    arbitrary =
      TestAddress <$>
        -- from one side we want a small address pool (this makes a greater
        -- chance of reusing a connection), but we also want to allow
        -- variability
        frequency [ (90, elements [1..3])
                  , (10, suchThat arbitrary (\a -> a > 3 && a < 5) )
                  ]
    shrink (TestAddress addr) =
          TestAddress . getPositive
      <$> shrink (Positive addr)

newtype ArbDataFlow = ArbDataFlow { getDataFlowType :: DataFlow }
  deriving stock Generic
  deriving Show via Quiet ArbDataFlow

instance Arbitrary ArbDataFlow where
    arbitrary = ArbDataFlow <$>
                  elements [ Unidirectional
                           , Duplex
                           ]
    shrink (ArbDataFlow Duplex)         = [ArbDataFlow Unidirectional]
    shrink (ArbDataFlow Unidirectional) = []


newtype Idx = Idx Int
  deriving (Eq, Ord, Show, Enum, Num)

-- | Schedule entry.  Both inbound and outbound mark the time while the
-- connection is used by either inbound / outbound side.  For inbound entries we
-- also include a schedule of remote promotions demotions.
--
-- In the test designed in this module we are not running server and inbound
-- protocol governor, we are testing the connection manager in isolation.  This
-- means that we cannot just generate two outbound schedules and run them on
-- two different nodes that connect to each other.  This scenario will be
-- covered in another test.
--
-- In a 'ScheduleMap' (list of schedule entries), a 'ScheduleInbound' should only
-- start when there is no overlapping 'ScheduleOutbound'.  Connection manager
-- behaviour is not specified in this case (it would accept the second
-- connection overwriting the state of the one that was already accepted), but
-- it would never happen in a real system (forbidden by TCP rules).
--
data ScheduleEntry extra =
    -- | Outbound connection which successful handshake negotiation.
    -- @
    --    seStart seConnDealy seHandshakeDelay seActiveDelay
    -- ───┼───────•───────────•────────────────┼────▶
    --                                              τ
    -- @
    -- or outbound connection where handshake timed out.
    -- @
    --    seStart seConnDealy                 seActiveDelay
    -- ───┼───────•───────────────────────────┼───▶
    --                                            τ
    -- @
    -- or outbound connections where connect fails:
    -- @
    --    seStart seConnDealy (failure)
    -- ───┼───────┼──────────────────────────────▶
    --                                           τ
    -- @
    --
    -- 'seStart' is a delay from when last connection was scheduler;
    -- 'seHandshakeDelay' and 'seActiveDelay' is a delay from 'seStart'
    --
    ScheduleOutbound {
        seIdx            :: Idx,
        seStart          :: DiffTime,
        seConnDelay      :: Either DiffTime DiffTime,
        -- ^ 'connect' delay, 'Left' indicate a connection error after given
        -- delay.
        --
        -- TODO: use 'Left' with non-positive delay as an error in
        -- 'openToConnect'.
        --
        seHandshakeDelay :: DiffTime,
        seActiveDelay    :: Either DiffTime DiffTime,
        seDataFlow       :: DataFlow,
        -- ^ delay since 'seStart' or 'seHandshakeDelay'.
        seExtra          :: extra
      }
    -- | Inbound connection which finished handshake negotiation.
    -- @
    --    seStart seHandshakeDelay  remotePromoteToWarm  remoteDemoteToCold  seActiveDelay
    -- ───┼───────•─────────────────•────────────────────•────────⋯⋯─────────┼───▶
    --                                                                           τ
    -- @
  | ScheduleInbound {
        seIdx               :: Idx,
        seStart             :: DiffTime,
        seHandshakeDelay    :: DiffTime,
        seActiveDelay       :: Either DiffTime DiffTime,
        seDataFlow          :: DataFlow,
        seRemoteTransitions :: [(DiffTime, DiffTime)],
        -- ^ (delay, length)
        seExtra             :: extra
      }
  deriving (Eq, Functor, Show)


isScheduleOutbound :: ScheduleEntry extra -> Bool
isScheduleOutbound ScheduleOutbound {} = True
isScheduleOutbound ScheduleInbound  {} = False

isScheduleInbound :: ScheduleEntry extra -> Bool
isScheduleInbound = not . isScheduleOutbound


genPositiveDelayWithPrecision :: Integer
                              -> Gen DiffTime
genPositiveDelayWithPrecision n =
    genDelayWithPrecision n `suchThat` (> 0)


-- | Precision used in 'genScheduleOutbound' and 'genScheduleInbound'.  The
-- higher the precision the smaller chance that events will happen in the same
-- slot time.
--
precision :: Integer
precision = 10 ^ (5 :: Int)


-- Generate 'ScheduleOutbound' which lifetime is limited by the size parameter.
-- Some small number of generated cases might be blocked on handshake.
--
genScheduleOutbound :: Int -> Gen (ScheduleEntry ())
genScheduleOutbound size = do
    seIdx   <- Idx <$> arbitrary
    seStart <- frequency
     [ ( 1
       , resize (size `div` 4) (genPositiveDelayWithPrecision precision)
       )
     , ( 9
       , resize (size + size `div` 2) (genPositiveDelayWithPrecision precision)
         `suchThat` (>= realToFrac (size - size `div` 16))
       )
     ]
    seConnDelay <- frequency
      [ (9, Right <$> resize 1 (genPositiveDelayWithPrecision precision))
      , (1, Left  <$> resize 1 (genPositiveDelayWithPrecision precision))
      ]
    maxActive <- resize size (genDelayWithPrecision precision)
                 `suchThat` (> 0.3)
    seHandshakeDelay
      <- (resize 5 $ genDelayWithPrecision precision)
         `suchThat` (    (< (maxActive / 3) `max` 0.1)
                      /\ (> 0)
                    )
    activeDelay <- resize (ceiling maxActive) (genDelayWithPrecision precision)
                   `suchThat` (\a -> a > 0 && a + seHandshakeDelay <= maxActive)
    seActiveDelay <- frequency
      [ (8, pure (Right activeDelay))
      , (2, pure (Left  activeDelay))
      ]
    seDataFlow <- elements [ Unidirectional, Duplex ]
    return ScheduleOutbound {
        seIdx,
        seStart,
        seConnDelay,
        seHandshakeDelay,
        seActiveDelay,
        seDataFlow,
        seExtra = ()
      }

genScheduleInbound :: Int -> Gen (ScheduleEntry ())
genScheduleInbound size = do
    seIdx   <- Idx <$> arbitrary
    seStart <-
      frequency

        [ (3,  resize 1   $ genPositiveDelayWithPrecision precision)
        , (16, resize 100 $ genPositiveDelayWithPrecision precision `suchThat` (> 60))
        -- , (1,  return 0)
        -- By excluding this case we exclude a possible race condition between
        -- `requestOutboundConnection` and `includeInboundConnection` (The
        -- @Overwritten@ transition).  It would introduce additional complexity
        -- in `fixupSchedule` function, for the following reason.  The call to
        -- `requestOutboundConnection` could be blocked until previous
        -- connection is held in `TerminatingState`, but
        -- `includeInboundConnection` would not.  To deal with this we would
        -- need to track the time when outbound connection actually starts in
        -- 'State' (used by 'fixupSchedule'.
        ]
    maxActive <- resize size (genDelayWithPrecision precision)
                 `suchThat` (> 1)
    seHandshakeDelay
      <- suchThat (resize 5 (genDelayWithPrecision precision))
                  (   (< (maxActive / 3))
                   /\ (> 0)
                  )
    activeDelay <- resize (round maxActive) (genDelayWithPrecision precision)
                   `suchThat` (\a -> a > 0 && a + seHandshakeDelay <= maxActive)
    seActiveDelay <- frequency
      [ (8, pure (Right activeDelay))
      , (2, pure (Left  activeDelay))
      ]
    seDataFlow <- frequency [ (1, pure Unidirectional)
                           , (2, pure Duplex)
                           ]
    let size' = size `div` 5
    seRemoteTransitions <-
          fixupRemoteTransitions activeDelay
      <$> listOf ((,) <$> resize size' (genDelayWithPrecision precision)
                      <*> resize size' (genDelayWithPrecision precision))
    return ScheduleInbound {
        seIdx,
        seStart,
        seHandshakeDelay,
        seActiveDelay,
        seRemoteTransitions,
        seDataFlow,
        seExtra = ()
      }


-- TODO: this generator needs to be tuned. We ought to have greater change for
-- generating edge cases:
-- - race conditions between inbound / outbound connections
-- - ScheduleInbound should be refined to contains information when remote
--   promotions / demotions should happen.
instance Arbitrary (ScheduleEntry ()) where
    arbitrary =
        frequency
        [ ( 6
          , genScheduleOutbound 20
          )
        , ( 2
          , genScheduleInbound 100
          )
        , ( 2
          , genScheduleInbound 300
          )
        ]

    shrink = go
      where
        shrinkDataFlow Duplex         = [Unidirectional]
        shrinkDataFlow Unidirectional = []

        go (sa@ScheduleOutbound { seStart
                                , seConnDelay
                                , seHandshakeDelay
                                , seActiveDelay
                                , seDataFlow
                                }) =
            [ sa { seStart = seStart' }
            | seStart' <- QC.shrinkMap getDelay Delay seStart
            , seStart' > 0
            ]
          ++
            [ sa { seConnDelay = seConnDelay' }
            | seConnDelay' <- QC.shrinkMap (both getDelay)
                                           (both Delay)
                                           seConnDelay
            , either id id seConnDelay' > 0
            ]
          ++
            [ sa { seActiveDelay = seActiveDelay'
                 }
            | seActiveDelay' <- QC.shrinkMap (both getDelay)
                                             (both Delay)
                                             seActiveDelay
            ]
          ++
            [ sa { seHandshakeDelay = seHandshakeDelay' }
            | seHandshakeDelay' <- QC.shrinkMap getDelay Delay seHandshakeDelay
            ]
          ++
            [ sa { seDataFlow = seDataFlow' }
            | seDataFlow' <- shrinkDataFlow seDataFlow
            ]
        go (sa@ScheduleInbound { seStart
                               , seHandshakeDelay
                               , seActiveDelay
                               , seDataFlow
                               , seRemoteTransitions
                               }) =
            [ sa { seStart = seStart' }
            | seStart' <- QC.shrinkMap getDelay Delay seStart
            , seStart' > 0
            ]
          ++
            [ sa { seActiveDelay = seActiveDelay'
                 , seRemoteTransitions =
                     fixupRemoteTransitions
                       (either id id seActiveDelay')
                       seRemoteTransitions
                 }
            | seActiveDelay' <- QC.shrinkMap (both getDelay)
                                             (both Delay)
                                             seActiveDelay
            ]
          ++
            [ sa { seHandshakeDelay = seHandshakeDelay'
                 }
            | seHandshakeDelay' <- QC.shrinkMap getDelay Delay seHandshakeDelay
            ]
          ++
            [ sa { seRemoteTransitions = seRemoteTransitions'' }
            | seRemoteTransitions' <- shrinkList (const [])
                                                 (bimap Delay Delay `map` seRemoteTransitions)
            , let seRemoteTransitions'' =
                      fixupRemoteTransitions (either id id seActiveDelay)
                    . map (both getDelay)
                    $ seRemoteTransitions'
            ]
          ++
            [ sa { seDataFlow = seDataFlow' }
            | seDataFlow' <- shrinkDataFlow seDataFlow
            ]


-- make sure that remote transition schedule is contained while the
-- inbound connection is active.
fixupRemoteTransitions :: DiffTime -> [(DiffTime, DiffTime)] -> [(DiffTime, DiffTime)]
fixupRemoteTransitions active = reverse . snd . foldl' f (0, [])
  where
    f as@(t, !acc) a@(d, l) | s <- t + d + l
                            , s <= active    = (s, a : acc)
                            | otherwise      = as


validScheduleEntry :: ScheduleEntry extra -> Bool
validScheduleEntry sa =
       -- seStart sa >= 0
       (isScheduleOutbound sa `implies` seStart sa >  0)
    && (isScheduleInbound  sa `implies` seStart sa >= 0)
    && seHandshakeDelay sa >= 0
    && either id id (seActiveDelay sa) >= 0
    && case sa of
        ScheduleInbound { seActiveDelay = Right d} ->
             foldl' (\acc (d, l) -> acc + d + l) 0 (seRemoteTransitions sa)
          <= d
        ScheduleInbound { seActiveDelay = Left _} -> True
        ScheduleOutbound {} -> True


prop_generator_ScheduleEntry :: ScheduleEntry () -> Property
prop_generator_ScheduleEntry s =
    label (case s of
            ScheduleOutbound {} -> "outbound"
            ScheduleInbound {}  -> "inbound"
          ) $
    label ("lifetime " ++ mkLabel 20 (round entryLifeTime)) $
    validScheduleEntry s
  where
    -- for outbound connections we include 'connect' time into life time
    entryLifeTime :: DiffTime
    entryLifeTime =
      case s of
        ScheduleOutbound { seConnDelay = Left  connDelay } -> connDelay
        ScheduleOutbound { seConnDelay = Right connDelay, seHandshakeDelay } ->
            connDelay + seHandshakeDelay + either id id (seActiveDelay s)
        ScheduleInbound { seHandshakeDelay } ->
            seHandshakeDelay + either id id (seActiveDelay s)


prop_shrinker_ScheduleEntry :: ScheduleEntry () -> Property
prop_shrinker_ScheduleEntry a =
      conjoin
    . map (\a' ->
                counterexample ("entry invariant violation for: " ++ show a')
                  (validScheduleEntry a')
            .&&. counterexample (show a' ++ " ≰ " ++ show a)
                  (smallerScheduleEntry a' a && a /= a')
          )
    . shrink
    $ a


-- | Provides 'QuickCheck' instance for a list of 'ScheduleEntry'-ies.  This
-- newtype models connection schedule between two fixed endpoints of a network.
--
newtype Schedule extra = Schedule { getSchedule :: [ScheduleEntry extra] }
  deriving (Eq, Show, Functor)

type RefinedSchedule = Schedule ScheduleInfo


-- | Generate an inbound connection which then is reused by a sequence of
-- outbound requests.
--
genReuseSchedule :: Int
                 -> Gen [ScheduleEntry ()]
genReuseSchedule size = do
    inbound       <- genScheduleInbound size
    NonNegative a <- resize 10 arbitrary
    outbounds     <- vectorOf a (genScheduleOutbound (size `div` a))
    return (inbound : outbounds)


prop_generator_ReuseSchedule :: Property
prop_generator_ReuseSchedule =
    forAll (fixupSchedule <$> genReuseSchedule 200)
      (\a ->
        label (  "reused "
              ++ mkLabel 10 ((100 * length (filter (siReused . seExtra)
                                           a))
                             `div` length a
                            )
              ) $

        label (  "not exists "
              ++ mkLabel 10 ((100 * length (filter (not . siExists . seExtra)
                                           a))
                              `div` length a
                            )
              ) $

        label (  "exists or reused "
              ++ mkLabel 10 ((100 * length (filter (    siExists . seExtra
                                                     \/ siReused . seExtra)
                                            a))
                              `div` length a
                            )
              ) $ True
      )


instance Arbitrary (Schedule ()) where
    arbitrary = do
      NonNegative n <- resize 10 arbitrary
      as <- concat
        <$> vectorOf n
              (frequency
                [ (2, resize 10 (listOf1 arbitrary))
                , (4, genReuseSchedule 20)
                , (4, genReuseSchedule 100)
                ])
      return (Schedule as)

    shrink (Schedule s) =
        [ Schedule s'
        | s' <- shrinkList' (map ($> ()) s)
        , s' /= s
        ]
      where
        -- much slower shrinker than 'shrinkList'
        shrinkList' :: [ ScheduleEntry () ]
                    -> [[ScheduleEntry ()]]
        shrinkList' []     = []
        shrinkList' (x:xs) =
                        [ xs ]
                     ++ [ x : xs' | xs' <- shrinkList' xs ]
                     ++ [ x': xs  | x'  <- shrink x ]


--
-- Fix and refine random @[ScheduleEntry any]@.
--

-- | Internal state of the 'fixupSchedule' function.
--
data State = State {
      -- | Time when last connection started.
      time            :: !Time

    , dataFlow        :: !(Maybe DataFlow)

    , handshakeUntil  :: !(Maybe Time)

      -- | Time when outbound connection started, when it will terminate and
      -- a boolean value which indicates if it errors or not.
    , outboundActive  :: !IsActive

      -- | Time when inbound connection started and when it will terminate.
    , inboundActive   :: !IsActive
    }
  deriving Show

data IsActive = NotActive
              | IsActive { iaStartTime :: !Time
                         , iaEndTime   :: !Time
                         , iaError     :: !Bool
                         }
 deriving (Show, Eq)

compareByEndTime :: IsActive -> IsActive -> Ordering
compareByEndTime NotActive NotActive        = EQ
compareByEndTime NotActive _                = LT
compareByEndTime _         NotActive        = GT
compareByEndTime IsActive { iaEndTime = a }
                 IsActive { iaEndTime = b } = a `compare` b


isUnidirectional :: State -> Bool
isUnidirectional = maybe False (Unidirectional ==) . dataFlow


-- @5s@ @TIME_WAIT@ timeout
testTimeWaitTimeout :: DiffTime
testTimeWaitTimeout = 5


data ScheduleInfo = ScheduleInfo {
    siExists         :: !Bool,
    siReused         :: !Bool,
    siForbidden      :: !Bool,
    siOverwrite      :: !Bool,
    siBlockHandshake :: !(Maybe Bool)
    -- ^ Only for outbound connections that are reused ('siReused' is 'True').
    -- Some of these connections will block on handshake that was already
    -- started by an inbound connection.  For 'siReused` which where already
    -- negotiated this is 'False'.
  }
  deriving (Show, Eq)

type RefinedScheduleEntry       = ScheduleEntry ScheduleInfo

-- | Refine & fix up a schedule.
--
-- For each address we analyse the sequence of schedule entries.  We keep
-- 'State' which measures time progress and keep track of existing inbound
-- / outbound connections.
--
fixupSchedule :: [       ScheduleEntry any]
              -> [RefinedScheduleEntry    ]
fixupSchedule =
      reindex
      -- get back the original order
    . sortOn seIdx
    . go initialState []
    . reindex
  where
    reindex :: [ScheduleEntry any] -> [ScheduleEntry any]
    reindex = map (\(seIdx, s) -> s { seIdx })
            . zip [0..]

    updateState :: Time -> DataFlow -> State -> State
    updateState time df State {
                          dataFlow,
                          outboundActive,
                          inboundActive,
                          handshakeUntil
                        } =
        State {
          time,
          dataFlow       = dataFlow',
          outboundActive = outboundActive',
          inboundActive  = inboundActive',
          handshakeUntil = handshakeUntil'
        }
      where
        -- Note: using '<' instead of '<=' is a matter when connections are
        -- considered terminated.  With '<' the @fixupSchedule'@ is nilpotent,
        -- which is a nice property to have for a fixup function.
        outboundActive' =
          (case outboundActive of
            IsActive { iaEndTime } | iaEndTime < time -> NotActive
            _                      -> outboundActive)
        inboundActive' =
          (case inboundActive of
            IsActive { iaEndTime } | iaEndTime < time -> NotActive
            _                      -> inboundActive)
        handshakeUntil' =
          (case handshakeUntil of
            Just a | a < time -> Nothing
            _                 -> handshakeUntil)

        dataFlow' =
          if | IsActive {} <- inboundActive'
             -> dataFlow
             | IsActive {} <- outboundActive'
             -> dataFlow
             | otherwise
             -> Just df


    initialState :: State
    initialState = State {
        time           = Time 0,
        dataFlow       = Nothing,
        handshakeUntil = Nothing,
        outboundActive = NotActive,
        inboundActive  = NotActive
      }

    -- this function assumes that all addr are equal!
    go :: State
       -> [RefinedScheduleEntry]
       -> [ScheduleEntry any]
       -> [RefinedScheduleEntry]
    go !_s !acc [] = reverse acc

    go !s  !acc (a@ScheduleOutbound
                     { seStart
                     , seConnDelay
                     , seHandshakeDelay
                     , seDataFlow
                     , seActiveDelay
                     }
                   : as) =
      let t :: Time
          t  = seStart `addTime` time s

          s' = updateState t seDataFlow s
          -- 'requestOutboundConnection' blocks for 'testTimeWaitTimeout' if
          -- there exists a connection in 'TerminatingState'. @t'@ is the
          -- effective time when the connection will start.
          -- Note: we use @s@ to compute @t'@ rather than @s'@, because
          -- 'updateState' does not takes the above into account.  This allows
          -- us to test more scenarios.
          --
          -- We use @t'@ to extend duration of outbound connection:
          --
          -- start   end (end + testTimeWaitTimeout)
          -- ├───────┼━━━┫
          --          ├───────┼━━━┫
          -- will effectively transform into:
          -- ├───────┼━━━┫
          --          ├──────────┼━━━┫
          -- (note the middle segment of the second connection is longer, as it
          -- first need to block until the TerminatingState timeout expires)
          --
          -- Note that if we have two outbound connections starting during
          -- 'TerminatingState' of some connection, one of them will error with
          -- 'ConnectionExists' error.  We assume that that it's the later one
          -- will error.  Hopefuly, this is good enough.
          --
          t' :: Time
          t' =
            case inboundActive s' of
              IsActive {}  -> t
              NotActive ->
                case maximumBy compareByEndTime
                               [ inboundActive s
                               , outboundActive s
                               ] of
                    NotActive -> t

                    -- the previous connection did not error, we add
                    -- 'testTimeWaitTimeout'
                    IsActive { iaError = False, iaEndTime } ->
                      t `max` (testTimeWaitTimeout `addTime` iaEndTime)

                    -- the previous connection errored, no need to add
                    -- 'testTimeWaitTimeout'
                    IsActive { iaError = True } -> t

          activeDelay :: DiffTime
          activeDelay = either id id seActiveDelay

      in -- Debug.traceShow (t, t', s, s') $
        case seConnDelay of

        -- no outbound nor inbound connection; 'connect' fails
        (Left connDelay) | NotActive <- outboundActive s'
                         , NotActive <- inboundActive s'
                         ->
          let outboundActive' = IsActive {
                                   iaStartTime = t,
                                   iaEndTime   = connDelay `addTime` t',
                                   iaError     = True
                                 }
              si = ScheduleInfo {
                    siExists         = False,
                    siReused         = False,
                    siForbidden      = False,
                    siOverwrite      = False,
                    siBlockHandshake = Nothing
                  }
              a'  = a { seExtra = si }
              s'' = s' { outboundActive = outboundActive' }
          in go s'' (a' : acc) as

        -- no outbound nor inbound connection; 'connect' succeeds
        (Right connDelay) | NotActive <- outboundActive s'
                          , NotActive <- inboundActive s'
                          ->
          let handshakeUntil' = seHandshakeDelay `addTime` connDelay `addTime` t'
              outboundActive' = IsActive {
                                    iaStartTime = t,
                                    iaEndTime   = activeDelay `addTime` handshakeUntil',
                                    iaError     = isLeft seConnDelay || isLeft seActiveDelay
                                  }

              s'' = s' { outboundActive  = outboundActive',
                         handshakeUntil = Just handshakeUntil'
                       }

              si = ScheduleInfo {
                    siExists         = False,
                    siReused         = False,
                    siForbidden      = False,
                    siOverwrite      = False,
                    siBlockHandshake = Nothing
                  }
              a' = a { seExtra = si }
          in go s'' (a' : acc) as

        -- if there exists outbound connection, we never call 'connect'
        _ | IsActive {} <- outboundActive s'
          ->
          let si = ScheduleInfo {
                    siExists         = True,
                    siReused         = False,
                    siForbidden      = False,
                    siOverwrite      = False,
                    siBlockHandshake = Nothing
                  }

              a'  = a { seDataFlow =
                         case dataFlow s' of
                           Just df -> df
                           Nothing -> error "fixupSchedule: invariant violation",
                        seExtra          = si,
                        seConnDelay      = Left 0,
                        seHandshakeDelay = 0,
                        seActiveDelay    = Right 0
                      }
          in go s' (a' : acc) as

        -- if we reuse an inbound connection, we never call 'connect'
        _ | IsActive { iaStartTime = inboundStartTime,
                       iaEndTime   = inboundEndTime,
                       iaError     = inboundError
                     }
              <- inboundActive s'
          ->
          let si = ScheduleInfo {
                     siExists = False,
                     siReused = True,
                     siForbidden = isUnidirectional s',
                     siOverwrite = False,
                     siBlockHandshake =
                       case handshakeUntil s of
                         Nothing -> Just True
                         Just h  -> Just (t <= h)
                   }

              outboundEndTime :: Time
              outboundEndTime =
                -- The outbound connection will start
                -- when ongoing handshake will finish.
                activeDelay
                `addTime`
                fromMaybe t (handshakeUntil s')

              s'' = s' { outboundActive =
                           if siForbidden si
                             then outboundActive s'
                             else IsActive
                                    { iaStartTime = t
                                    -- note: we can assume that
                                    -- @outboundEntTime < inboundEndTime@;
                                    -- otherwise we will ignore this schedule
                                    -- entry.
                                    , iaEndTime   = outboundEndTime
                                    , iaError     = isLeft seConnDelay
                                                 || isLeft seActiveDelay
                                    }
                       , inboundActive =
                           case seActiveDelay of
                             -- we can assume that:
                             -- 'outboundEndTime < inboundEndTime'
                             -- if this is not satisfied we will ignore such
                             -- cases.
                             Left _ | not (siForbidden si)
                                    -> IsActive
                                         { iaStartTime = inboundStartTime
                                         , iaEndTime   = outboundEndTime
                                         , iaError     = inboundError
                                                      || isLeft seActiveDelay
                                         }
                             _      -> inboundActive s'

                       }

              a' = a { seDataFlow =
                          case dataFlow s' of
                            Just df -> df
                            Nothing -> error "fixupSchedule: invariant violation"
                      , seConnDelay      = Right 0
                      , seHandshakeDelay = maybe 0 (\d -> (d `diffTime` t) `max` 0)
                                                   (handshakeUntil s)
                      , seActiveDelay    = if siForbidden si
                                             then Right 0
                                             else seActiveDelay
                      , seExtra          = si
                      }

              acc' = case seActiveDelay of
                Right _ -> acc
                Left  _ | siForbidden si -> acc
                        | otherwise      -> modifyInbound f acc
                  where
                    f s@ScheduleInbound { seHandshakeDelay = h, seActiveDelay = d }
                      = s { seActiveDelay = Left $ (outboundEndTime `diffTime` inboundStartTime - h)
                                                    `min` either id id d
                          }
                    f ScheduleOutbound {} = error "modifyInbound: invariant violation"

          in if |  siForbidden si
                -> go s'' (a' : acc') as

                |  isRight seActiveDelay
                ,  outboundEndTime < inboundEndTime
                -> go s'' (a' : acc') as

                |  isLeft seActiveDelay
                ,  outboundEndTime <= inboundEndTime
                -> go s'' (a' : acc') as

                |  otherwise
                -> go s         acc   as

    go !s !acc (a@ScheduleInbound {
                    seStart,
                    seHandshakeDelay,
                    seDataFlow,
                    seActiveDelay
                  } : as) =
      let t :: Time
          t  = seStart `addTime` time s

          s' = updateState t seDataFlow s

          -- when @t == time s@ the inbound connection will overwrite outbound
          -- connection
          hasOutbound :: Bool
          hasOutbound =
            case outboundActive s' of
              NotActive -> False
              -- after 'updateState', @_until@ is guaranteed to be past the
              -- current time @t@.  The condition here allows to only bypass
              -- the condition if the outbound connection was started at the
              -- same moment in time as the current time.  This way we allow to
              -- generate schedule which can do the `Overwritten` transition.
              IsActive { iaStartTime } -> iaStartTime < t

          hasInbound :: Bool
          hasInbound =
            case inboundActive s' of
              NotActive   -> False
              IsActive {} -> True

          activeDelay :: DiffTime
          activeDelay = either id id seActiveDelay

      in -- Debug.traceShow (t, s, s') $
         if hasInbound || hasOutbound
         -- ignore an inbound connection if:
         --
         -- - there is a running inbound connection or;
         -- - an outbound connection.  For outbound connection we allow to have
         --   inbound connection started at the same time as outbound one, this
         --   simulates race condition that the connection manager can resolve.
         then go s acc as
         else
           let handshakeUntil' = seHandshakeDelay `addTime` t
               siOverwrite =
                 case outboundActive s' of
                   NotActive                -> False
                   IsActive { iaStartTime } -> iaStartTime <= t

               a'  = a { seExtra = ScheduleInfo {
                           siExists         = False,
                           siReused         = False,
                           siForbidden      = False,
                           siOverwrite,
                           siBlockHandshake = Nothing
                         }
                       }

               s'' = s' { inboundActive  = IsActive {
                              iaStartTime = t,
                              iaEndTime   = activeDelay `addTime` handshakeUntil',
                              iaError     = isLeft seActiveDelay
                            },
                          outboundActive = NotActive,
                          handshakeUntil = Just handshakeUntil'
                        }

               acc' =
                 if siOverwrite
                   then modifyOutbound (\x -> x { seConnDelay = Left 0 }) acc
                   else acc

           in go s'' (a' : acc') as

    -- modify all 'ScheduleOutbound' until first that has non zero 'seStart'
    modifyOutbound :: (RefinedScheduleEntry -> RefinedScheduleEntry)
                   -> [RefinedScheduleEntry]
                   -> [RefinedScheduleEntry]
    modifyOutbound f as =
      case span ((== 0) . seStart /\ isScheduleOutbound) as of
        (as', x@ScheduleOutbound {} : xs) -> map f as'
                                          ++ f x : xs
        (as', xs)                         -> map f as'
                                          ++ xs


    -- modify last 'ScheduleInbound'
    modifyInbound :: (RefinedScheduleEntry -> RefinedScheduleEntry)
                  -> [RefinedScheduleEntry]
                  -> [RefinedScheduleEntry]
    modifyInbound f (a@ScheduleInbound {} : as) = f a : as
    modifyInbound f (a                    : as) =   a : modifyInbound f as
    modifyInbound _ []                          = []


-- | A basic property test for 'RefinedSchedule' with extended statistics.
--
prop_fixupSchedule :: Schedule ()
                   -> Property
prop_fixupSchedule (Schedule schedule) =
    let schedule' = fixupSchedule schedule in
    counterexample "non-indepotent" (fixupSchedule schedule' == schedule')
    .&&.
    counterexample ""
      (conjoin (map (\a -> counterexample (show a) $
                                if    siExists    (seExtra a)
                                   || siReused    (seExtra a)
                                   || siForbidden (seExtra a)
                                then
                                  case a of
                                    ScheduleOutbound {} -> True
                                    ScheduleInbound  {} -> False
                                else True
                             && if siOverwrite (seExtra a)
                                then
                                  case a of
                                    ScheduleOutbound {} -> False
                                    ScheduleInbound  {} -> True
                                else True
                    )
                    schedule'))


-- | Arbitrary instance used to generate a connection schedule between two
-- endpoints.
--
instance Arbitrary RefinedSchedule where
    arbitrary = do
      NonNegative n <- resize 10 arbitrary
      as <- concat
        <$> vectorOf n
              (frequency
                [ (2, resize 10 (listOf1 arbitrary))
                , (4, genReuseSchedule 20)
                , (4, genReuseSchedule 100)
                ])
      return (Schedule (fixupSchedule as))

    shrink (Schedule s) =
        [ Schedule s''
        | s' <- shrinkList' (map ($> ()) s)
        , let s'' = fixupSchedule s'
        , s'' /= s
        ]
      where
        -- much slower shrinker than 'shrinkList'
        shrinkList' :: [ ScheduleEntry () ]
                    -> [[ScheduleEntry ()]]
        shrinkList' []     = []
        shrinkList' (x:xs) =
                        [ xs ]
                     ++ [ x : xs' | xs' <- shrinkList' xs ]
                     ++ [ x': xs  | x'  <- shrink x ]


validRefinedSchedule :: RefinedSchedule -> Bool
validRefinedSchedule (Schedule s) =
    all (validScheduleEntry /\ validScheduleInfo) s
  where
    validScheduleInfo se =
           (siExists    `implies`    not siReused
                                  && seConnDelay se == Left 0)
        && (siReused    `implies` not siExists)
        && (siForbidden `implies` siReused)
        && (isJust siBlockHandshake
                        `implies` siReused)
        && (isScheduleInbound se
                        `implies`    not siExists
                                  && not siReused
                                  && not siForbidden)
        && (isScheduleOutbound se
                        `implies` not siOverwrite)
      where
        ScheduleInfo {..} = seExtra se


-- | Output statistics about generated schedules and check that they are valid.
--
prop_generator_RefinedSchedule :: RefinedSchedule -> Property
prop_generator_RefinedSchedule a@(Schedule schedule) =
    let outNo = length
              . filter (\a ->
                         case a of
                           ScheduleOutbound {seExtra} ->
                             (not . siExists /\ not . siForbidden) seExtra
                           ScheduleInbound {} -> False
                       )
              $ schedule
        inNo  = length
              . filter (\a ->
                         case a of
                           ScheduleOutbound {} -> False
                           ScheduleInbound {} -> True
                       )
              $ schedule
        cs = length (filter (isJust . siBlockHandshake . seExtra) schedule)
    in
    -- number of all connections
    label (concat
            [ "length "
            , mkLabel 25 (length schedule)
            ]) $

    -- number of all outbound connections
    label (concat
            [ "outbound "
            , mkLabel 25 outNo
            ]) $

    -- number of all inbound connections
    label (concat
            [ "inbound "
            , mkLabel 5 inNo
            ]) $

    -- % of outbound connection which error
    (label $ concat
           [ "connection error "
           , if outNo > 0
               then mkLabel 5
                      ((100 * length ( filter (   not . siExists . seExtra
                                               /\ not . siReused . seExtra
                                               /\ isLeft . seConnDelay)
                                        -- 'seConnDelay' is a partial function!
                                     . filter (\se -> case se of { ScheduleOutbound {} -> True; _ -> False })
                                     $ schedule))
                       `div` outNo
                      )
               else "0"
           , "%"
           ]) $

    -- % of outbound connections which are requested when there already exists
    -- an outbound connection
    (label $ concat
           [ "exists "
           , if outNo > 0
               then mkLabel 10 ((100 * length (filter (siExists . seExtra)
                                                      schedule))
                                `div` outNo
                               )
               else "0"
           , "%"
           ]) $

    -- % of outbound connections which will reuse an existing inbound connection
    (label $ concat
           [ "reused "
           , if outNo > 0
               then mkLabel 25 ((100 * length (filter (siReused . seExtra)
                                                      schedule))
                                `div` outNo
                               )
               else "0"
           , "%"
           ]) $

    -- % of all connections which:
    -- - are outbound and reuse an existing inbound connection
    -- - are blocked on ongoing handshake of the inbound connection
    (label $ concat
           [ "reuse-handshake blocking "
           , if cs > 0
               then mkLabel 25 ((100 * length (filter ((Just True ==) . siBlockHandshake . seExtra)
                                                      schedule))
                                `div` cs
                               )
               else "0"
           , "%"
           ]) $

    -- % of all connections which:
    -- - are outbound and reuse an existing inbound connection
    -- - are not blocked on ongoing handshake of the inbound connection
    (label $ concat
           [ "reuse-handshake non-blocking "
           , if cs > 0
               then mkLabel 25 ((100 * length (filter ((Just False ==) . siBlockHandshake . seExtra)
                                                      schedule))
                                `div` cs
                               )
               else "0"
           , "%"
           ]) $

    -- number of inbound connections which will overwrite an outbound connection
    label (concat
            [ "overwrite "
            ,  mkLabel 2 (length (filter (siOverwrite . seExtra)
                                         schedule))
            ]) $

    -- number of forbidden connections, i.e. outbound connections which turns
    -- out to be 'Unidirectional' and cannot be reused.
    label (concat
          [ "forbidden "
          ,  mkLabel 10 (length (filter (siForbidden . seExtra)
                                        schedule))
          ]) $

    validRefinedSchedule a


prop_shrinker_RefinedSchedule :: RefinedSchedule -> Property
prop_shrinker_RefinedSchedule a@(Schedule s) =
      conjoin
    . map (/= s)
    . map getSchedule
    . shrink
    $ a


-- | Connection schedule for multiple nodes.  Each map entry represents outbound
-- and inbound connection to / from that node (star network topology).
--
newtype ScheduleMap' addr extra =
        ScheduleMap { getScheduleMap :: Map addr (Schedule extra) }
  deriving (Eq, Functor, Show)


{-
prettyScheduleMap :: (Show addr, Show extra)
                  => ScheduleMap' addr extra -> String
prettyScheduleMap (ScheduleMap schedule) =
        concat
      . map (\(addr, schedule') ->
            concat
              [ show addr
              , "\n"
              , intercalate "\n" (map (('\t' :) . show) schedule')
              ])
      . Map.assocs
      . fmap getSchedule
      $ schedule
-}


instance Ord addr => Semigroup (ScheduleMap' addr extra) where
    ScheduleMap a <> ScheduleMap b = ScheduleMap (Map.unionWith f a b)
      where
        f (Schedule s) (Schedule s') = Schedule (s' ++ s)

instance Ord addr => Monoid (ScheduleMap' addr extra) where
    mempty = ScheduleMap Map.empty


type ScheduleMap          addr  = ScheduleMap'  addr ()
type RefinedScheduleMap   addr  = ScheduleMap'  addr ScheduleInfo


-- | Compute a linear schedule from 'RefinedScheduleMap'.
--
schedule :: RefinedScheduleMap addr
         -> [(Time, addr, RefinedScheduleEntry)]
schedule =
      sortOn (\(t, _, _) -> t)
    . concatMap (\(a, as) -> map (\(t, e) -> (t, a, e)) as)
    . Map.assocs
    . fmap (go (Time 0) . getSchedule)
    . getScheduleMap
  where
    go :: Time -> [RefinedScheduleEntry] -> [(Time, RefinedScheduleEntry)]
    go _t []       = []
    go  t (a : as) =
      let t' = seStart a `addTime` t
      in (t', a) : go t' as



prop_schedule :: RefinedScheduleMap Addr -> Bool
prop_schedule a =
      -- 'schedule' is ordered by time
         sortOn (\(t, _, _) -> t) s
      == s
    &&
      -- 'schedule' preserves index order
      all (\s' -> sortOn (seIdx . snd) s'
               == sortOn fst          s')
          sm
  where
    s  = schedule a
    sm = Map.fromListWith
           (flip (++))
           (map (\(t, addr, a) -> (addr, [(t, a)])) s)


-- | Only used to test 'fixupSchedule'
--
instance Arbitrary (ScheduleMap Int) where
    arbitrary = do
      Small n <- arbitrary
      as <- map Schedule <$> vectorOf n arbitrary
      return (ScheduleMap (Map.fromList (zip [0.. (n - 1)] as)))


    shrink = map ScheduleMap
           . shrinkMap (map Schedule . shrinkList shrink . getSchedule)
           . getScheduleMap


instance (Arbitrary addr, Ord addr) => Arbitrary (RefinedScheduleMap addr) where
    arbitrary = do
      addrs <- resize 20 orderedList
      as <- map Schedule <$>
            vectorOf (length addrs)
                     -- use @'Arbitrary' 'RefinedSchedule'@
                     (getSchedule <$> arbitrary)
      return (ScheduleMap (Map.fromList (zip addrs as)))

    shrink = map ScheduleMap
             -- use @'Arbitrary' 'RefinedSchedule'@
           . shrinkMap shrink
           . getScheduleMap


-- | Linearised schedule of inbound connections.
--
inboundSchedule :: RefinedScheduleMap addr
                -> [(Time, addr, RefinedScheduleEntry)]
inboundSchedule =
      filter (isScheduleInbound . (\(_, _, a) -> a))
    . schedule


--
-- TODO: USE 'schedule' in the 'prop_connectionManagerSimulation'
--


prop_generator_RefinedScheduleMap :: RefinedScheduleMap Addr -> Property
prop_generator_RefinedScheduleMap (ScheduleMap s) =
      label ("map-size " ++ mkLabel 2  (Map.size s))
    . label ("size "     ++ mkLabel 50 (sum (Map.map (length . getSchedule) s)))
    $ all validRefinedSchedule s


prop_shrinker_RefinedScheduleMap :: RefinedScheduleMap Addr -> Property
prop_shrinker_RefinedScheduleMap (a@(ScheduleMap s)) =
      conjoin
    . map (\(ScheduleMap s') ->
               all validRefinedSchedule s'
            &&
               s /= s'
          )
    . shrink
    $ a

--
-- Snocket implementation based on a fixed schedule.
--


data ConnState = UnconnectedState
               | ConnectedState
               | AcceptedState
               | ListeningState
               | ClosedState
  deriving (Eq, Show)

data Bound = Bound | NotBound

data FDState = FDState {
    fdLocalAddress    :: Addr,
    fdRemoteAddress   :: Maybe Addr,
    fdConnectionState :: ConnState,
    fdBound           :: Bound,
    fdScheduleEntry   :: Maybe RefinedScheduleEntry
  }

newtype FD m = FD { fdState :: StrictTVar m FDState }

-- | We only keep exceptions here which should not be handled by the test
-- runtime, i.e. for 'connect' and 'accept' we use 'IOException's.
--
data SnocketError =
      InvalidArgumentError
    | BindError
    | ListenError
  deriving Show

instance Exception SnocketError


-- | Test runtime error.  If triggered it indicates a bug in the test
-- environment.
--
data TestRunTimeError
    = PopScheduleOutboundError
    | NegativeDelayError
    | EventTimingError String
    | MismatchedScheduleEntry (Addr, Idx) (Addr, Idx)
  deriving Show

instance Exception TestRunTimeError


-- | Test failure.
data TestError
  -- | Unexpected timeout.
  = TimeoutError String
  | UnsupportedStateError String AbstractState
  deriving Show

instance Exception TestError


-- | A pure snocket.  Reading always blocks forever, writing is immediate.
--
-- This very roughly captures socket semantics, but it's good enough for the
-- the testing we do here.  In particular this does not rule out situations
-- which the kernel would forbid, e.g. the two connections with the same
-- four-tuples.
--
-- Note: we don't track all the connection in the system, but rather relay on
-- the event schedule.  If the execution environment (test runtime) is in sync
-- with the snocket, it will pass the right 'ScheduleEntry' to the test
-- runtime, both for outbound ('connect' call) and inbound ('accept' call).
-- The limiting factor of this strategy is that the execution environment must
-- execute events in a correct order.  We try to avoid events scheduled in the
-- same time slot with a fine grane delays generated by the generators (see
-- 'precision'), as we cannot guarantee that events in the same time slot will
-- be executed in the right order (especially in the presence of concurrency).
--
mkSnocket :: forall m.
             ( MonadDelay m
             , MonadMask  m
             , MonadMonotonicTime m
             , MonadSTM   m
             , MonadThrow (STM m)
             )
          => RefinedScheduleMap Addr
          -- ^ we need the schedule to know how much time 'connect' will take
          -- and weather it errors or not.
          -> m (Snocket m (FD m) Addr)
mkSnocket scheduleMap = do
    -- We keep track of outbound connections which will call 'connect' in
    -- a mutable TVar.
    let inboundSchedule :: Map Addr [RefinedScheduleEntry]
        inboundSchedule =
              filter (   isScheduleOutbound
                      /\ not . siExists . seExtra
                      /\ not . siReused . seExtra
                     )

            . getSchedule
          <$> (getScheduleMap scheduleMap)
    v <- newTVarIO inboundSchedule
    return $ Snocket {
        getLocalAddr,
        getRemoteAddr,
        addrFamily,
        open,
        openToConnect,
        connect = connect v,
        listen,
        accept,
        bind,
        close,
        toBearer
      }
  where
    pop :: StrictTVar m (Map Addr [RefinedScheduleEntry])
        -> Addr
        -> STM m RefinedScheduleEntry
    pop v addr = do
      m <- readTVar v
      case Map.lookup addr m of
        Nothing -> error "mkSnocket.pop: unknown address"
        Just [] -> throwSTM PopScheduleOutboundError
        Just (x : xs) -> writeTVar v (Map.insert addr xs m)
                      $> x

    getLocalAddr (FD v) =
      fdLocalAddress <$> atomically (readTVar v)

    getRemoteAddr (FD v) = do
      mbRemote <- fdRemoteAddress <$> atomically (readTVar v)
      case mbRemote of
        Nothing   -> throwIO InvalidArgumentError
        Just addr -> pure addr

    addrFamily _ = TestFamily

    open _ =
      FD <$>
        newTVarIO FDState {
            fdLocalAddress    = TestAddress 0,
            fdRemoteAddress   = Nothing,
            fdConnectionState = UnconnectedState,
            fdBound           = NotBound,
            fdScheduleEntry   = Nothing
          }

    openToConnect _ =
      FD <$>
        newTVarIO FDState {
            fdLocalAddress    = TestAddress 0,
            fdRemoteAddress   = Nothing,
            fdConnectionState = UnconnectedState,
            fdBound           = NotBound,
            fdScheduleEntry   = Nothing
          }

    connect v (FD fd) remoteAddr = do
        se <- atomically $ do
          fds@FDState { fdConnectionState } <- readTVar fd
          se <- pop v remoteAddr
          case fdConnectionState of
            UnconnectedState ->
              writeTVar fd fds { fdRemoteAddress   = Just remoteAddr,
                                 fdConnectionState = ConnectedState,
                                 fdScheduleEntry   = Just se
                               }
            _ -> throwSTM (ioe (concat
                            [ "unexpected state: "
                            , show fdConnectionState
                            , " "
                            , show (remoteAddr, seIdx se)
                            ]))
          return se
        case seConnDelay se of
          Left d  -> threadDelay d
                  >> throwIO (ioe (show (remoteAddr, seIdx se)))
          Right d -> threadDelay d
      where
        ioe :: String -> IOException
        ioe ioe_description =
              IOError { ioe_handle   = Nothing
                      , ioe_type        = OtherError
                      , ioe_location    = "connect"
                      , ioe_description
                      , ioe_errno       = Nothing
                      , ioe_filename    = Nothing
                      }


    bind (FD fd) localAddr =
      atomically $ do
        fds@FDState { fdBound } <- readTVar fd
        case fdBound of
          NotBound -> writeTVar fd fds { fdLocalAddress = localAddr
                                       , fdBound = Bound
                                       }
          Bound -> throwIO BindError

    accept :: FD m -> m (Accept m (FD m) Addr)
    accept (FD fd) = pure $ Accept $ go (inboundSchedule scheduleMap)
      where
        go :: [(Time, Addr, RefinedScheduleEntry)]
           -> m (Accepted (FD m) Addr, Accept m (FD m) Addr)
        go [] = pure (AcceptFailure (toException ioe), Accept $ go [])
          where
            ioe = IOError { ioe_handle      = Nothing
                          , ioe_type        = OtherError
                          , ioe_location    = "accept"
                          , ioe_description = ""
                          , ioe_errno       = Nothing
                          , ioe_filename    = Nothing
                          }


        go ((blockUntil, remoteAddr, se) : as) = do
          t <- getMonotonicTime
          threadDelay (blockUntil `diffTime` t)
          fd' <- atomically $ do
            FDState { fdLocalAddress = localAddr } <- readTVar fd
            newTVar FDState {
                        fdLocalAddress    = localAddr,
                        fdRemoteAddress   = Just remoteAddr,
                        fdConnectionState = AcceptedState,
                        fdBound           = Bound,
                        fdScheduleEntry   = Just se
                      }
          pure (Accepted (FD fd') remoteAddr, Accept $ go as)

    toBearer _ _ _ =
      return MuxBearer {
          write   = \_ _ -> getMonotonicTime,
          read    = \_ -> forever (threadDelay 3600),
          sduSize = SDUSize 1500
        }

    listen (FD fd) = atomically $ do
      fds@FDState{ fdConnectionState } <- readTVar fd
      case fdConnectionState of
        UnconnectedState ->
          writeTVar fd (fds { fdConnectionState = ListeningState })
        _ -> throwIO ListenError

    close (FD fd) =
      uninterruptibleMask_
        $ atomically
        $ modifyTVar fd (\fds -> fds { fdConnectionState = ClosedState })


--
-- ConnectionHandler
--

-- | Connection handle. Gives access to 'RefinedScheduleEntry' and thread id in
-- which the handler is running which allows to asynchronously kill the thread.
--
data Handle m = Handle { hScheduleEntry :: RefinedScheduleEntry
                       , hThreadId      :: ThreadId m
                       }

-- | Version use by the handshake.
--
data Version = Version DataFlow

-- | A connection handler.  It will block for 'seHandshakeDelay' before
-- notifying handshake negotiation and then block until the connection is closed.
--
-- We pass 'ScheduleEntry's for outbound and inbound connections via a mutable
-- variable which contains a map of lists of connections.  We pop from each list
-- when we connected, but we also need to pop when we tried to include
-- a connection and connection manager thrown 'ConnectionManagerError'.
--
mkConnectionHandler :: forall m handlerTrace.
                       ( MonadLabelledSTM m
                       , MonadCatch       m
                       , MonadFork        m
                       , MonadTimer       m
                       , MonadFail        m
                       )
                    => Snocket m (FD m) Addr
                    -> ConnectionHandler InitiatorResponderMode
                                         handlerTrace (FD m)
                                         Addr (Handle m)
                                         Void Version
                                         m
mkConnectionHandler snocket =
    ConnectionHandler $
      WithInitiatorResponderMode
        handler
        handler
  where
    handler :: ConnectionHandlerFn handlerTrace (FD m) Addr (Handle m) Void Version m
    handler fd promise _ ConnectionId { remoteAddress } _ =
      MaskedAction $ \unmask ->
        do threadId <- myThreadId
           let addr = getTestAddress remoteAddress
           Just se <- atomically $ fdScheduleEntry <$> readTVar (fdState fd)
           labelThisThread ("handler-" ++ show addr ++ "-" ++ show (seIdx se))
           unmask (threadDelay (seHandshakeDelay se))
           atomically (writePromise promise
                        (Right ( Handle { hScheduleEntry = se
                                        , hThreadId = threadId
                                        }
                               , Version (seDataFlow se)
                               )))

           -- The connection manager throws async exception to kill the
           -- connection handler thread and is closing file descriptor using
           -- 'onException' handler, like we do here.  The
           -- `MuxConnectionHandler` is starting multiplexer at this point.
           unmask $ case seActiveDelay se of
             Left d  -> threadDelay d
                     >> throwIO (ioe (show addr ++ "-" ++ show (seIdx se)))
             Right _ -> forever (threadDelay 3600)

        `onException` close snocket fd

    ioe ioe_location =
      IOError { ioe_handle      = Nothing
              , ioe_type        = OtherError
              , ioe_location    = "connection-handler: " ++ ioe_location
              , ioe_description = "application error"
              , ioe_errno       = Nothing
              , ioe_filename    = Nothing
              }


--
-- Consistent type aliases for observed traces.
--

type TestConnectionState m      = ConnectionState Addr (Handle m) Void Version m
type TestConnectionManagerTrace = ConnectionManagerTrace Addr ()
type TestTransitionTrace m      = TransitionTrace  Addr (TestConnectionState m)
type AbstractTransitionTrace    = TransitionTrace' Addr AbstractState


verifyAbstractTransition :: AbstractTransition
                         -> Bool
verifyAbstractTransition Transition { fromState, toState } =
    case (fromState, toState) of
      --
      -- Outbound
      --

      -- @Reserve@
      (UnknownConnectionSt, ReservedOutboundSt) -> True
      -- @Connected@
      (ReservedOutboundSt, UnnegotiatedSt Outbound) -> True
      -- @Negotiated^{Unidirectional}_{Outbound}@
      (UnnegotiatedSt Outbound, OutboundUniSt)  -> True
      -- @Negotiated^{Duplex}_{Outbound}@
      (UnnegotiatedSt Outbound, OutboundDupSt Ticking) -> True

      -- @DemotedToCold^{Unidirectional}_{Local}@
      (OutboundUniSt, TerminatingSt) -> True
      -- @TimeoutExpired@
      (OutboundDupSt Ticking, OutboundDupSt Expired) -> True
      -- @DemotedToCold^{Duplex}_{Local}@
      (OutboundDupSt Expired, TerminatingSt) -> True

      --
      -- Outbound ↔ Inbound
      --

      -- @DemotedToCold^{Duplex}_{Local}@
      (OutboundDupSt Ticking, InboundIdleSt Duplex) -> True
      -- @Awake^{Duplex}_{Local}
      (InboundIdleSt Duplex, OutboundDupSt Ticking) -> True
      -- @PromotedToWarm^{Duplex}_{Remote}@
      (OutboundDupSt Ticking, DuplexSt) -> True
      (OutboundDupSt Expired, DuplexSt) -> True
      -- @PromotedToWarm^{Duplex}_{Local}@
      (InboundSt Duplex, DuplexSt) -> True
      -- @DemotedToCold^{Duplex}_{Remote}@
      (DuplexSt, OutboundDupSt Ticking) -> True
      -- @DemotedToCold^{Duplex}_{Local}@
      (DuplexSt, InboundSt Duplex) -> True

      --
      -- Inbound
      --

      -- @Accepted@
      (UnknownConnectionSt, UnnegotiatedSt Inbound) -> True
      -- @Overwritten@
      (ReservedOutboundSt, UnnegotiatedSt Inbound) -> True
      -- @Negotiated^{Duplex}_{Inbound}
      (UnnegotiatedSt Inbound, InboundIdleSt Duplex) -> True
      -- @Negotiated^{Unidirectional}_{Inbound}
      (UnnegotiatedSt Inbound, InboundIdleSt Unidirectional) -> True

      -- @Awake^{Duplex}_{Remote}
      (InboundIdleSt Duplex, InboundSt Duplex) -> True
      -- @Commit^{Duplex}
      (InboundIdleSt Duplex, TerminatingSt) -> True
      -- @DemotedToCold^{Duplex}_{Local}
      (InboundSt Duplex, InboundIdleSt Duplex) -> True

      -- @Awake^{Unidirectional}_{Remote}
      (InboundIdleSt Unidirectional, InboundSt Unidirectional) -> True
      -- @Commit^{Unidirectional}
      (InboundIdleSt Unidirectional, TerminatingSt) -> True
      -- @DemotedToCold^{Unidirectional}_{Local}
      (InboundSt Unidirectional, InboundIdleSt Unidirectional) -> True

      --
      -- Terminate
      --

      -- @Termiante@
      (TerminatingSt, TerminatedSt) -> True
      -- implicit terminate transition
      --
      -- The second pattern implicitly allows to (UnknowConnectionSt,
      -- UnknownConnectionSt); this can be logged by
      -- `unregisterOutboundConnectionImpl` if the connection errored before it
      -- was called.
      (_, TerminatedSt) -> True
      (_, UnknownConnectionSt) -> True

      -- We accept connection in 'TerminatingSt'
      (TerminatingSt, UnnegotiatedSt Inbound) -> True

      _ -> False


newtype SkewedBool = SkewedBool Bool
  deriving Show

instance Arbitrary SkewedBool where
    arbitrary =
      frequency [ (3, pure (SkewedBool True))
                , (1, pure (SkewedBool False))
                ]


    shrink (SkewedBool True)  = [SkewedBool False]
    shrink (SkewedBool False) = []

-- | Connection manager simulation
--
--  The purpose of this simulation is to put the connection manager state
--  machine in all possible states and check that it will only execute allowed
--  transitions.
--
-- Possible extensions:
--
-- * test that resources are eventually closed;
-- * we could check that every connection eventually ends in 'TerminatedState';
-- * we can statically compute which transitions we will observe
-- * include handshake failures
--
-- This test does not cover 'TimeoutExpired' transition.  For that we would need
-- to extend the 'seRemoteTransitions' type and track if we can call
-- 'unregisterInboundConnection' while the inbound connection is not used but
-- the connection is used by the outbound side (so that
-- 'unregisterInboundConnection' will trigger 'TimeoutExpired` transition rather
-- than terminate the connection).
--
-- This test also does not cover connection pruning as we only generate a small
-- number of peers.  But we do test connection handler thread either throwing an
-- exception or being killed by an asynchronous asynchronous exception.
--
prop_connectionManagerSimulation
    :: SkewedBool
    -- ^ bind to local address or not
    -> RefinedScheduleMap Addr
    -- ^ A list of addresses to which we connect or which connect to us.  We use
    -- 'Blind' since we show the arguments using `counterexample` in a nicer
    -- way.
    -> Property
prop_connectionManagerSimulation (SkewedBool bindToLocalAddress) scheduleMap =
    let tr = runSimTrace experiment in
    -- `selectTraceEventsDynamic`, can throw 'Failure', hence we run
    -- `traceResults` first.
    counterexample ("\nSimulation Trace\n" ++ (intercalate "\n" . map show $ traceEvents tr)) $
      case traceResult True tr of
        Left failure ->
          counterexample (intercalate "\n" [ displayException failure
                                           , show scheduleMap
                                           ]) False
        Right _ ->
          let cmTrace :: [AbstractTransitionTrace]
              cmTrace = selectTraceEventsDynamic tr

          in counterexample ("\nTransition Trace\n" ++ (intercalate "\n" . map show $ cmTrace))
                            (verifyTrace cmTrace)
  where
    myAddress :: Maybe Addr
    myAddress = if bindToLocalAddress
                  then Just (TestAddress 0)
                  else Nothing

    verifyTrace :: [AbstractTransitionTrace] -> Property
    verifyTrace = conjoin
                . fmap (verifyTransitionProp . ttTransition)
      where
        verifyTransitionProp :: AbstractTransition -> Property
        verifyTransitionProp tr = counterexample ("\nUnexpected transition: " ++ show tr) (verifyAbstractTransition tr)


    experiment :: forall s. IOSim s Int
    experiment = do
        labelThisThread "th-main"
        snocket <- mkSnocket scheduleMap
        let cmTracer :: Tracer (IOSim s) TestConnectionManagerTrace
            cmTracer = Tracer (say . show)
                    {--
                      - <> Tracer (\msg -> do
                      -             t <- getMonotonicTime
                      -             Debug.traceShowM (t, msg))
                      --}
            -- The above is a useful trick for getting simulation logs in
            -- a ghci session.

            cmTrTracer :: Tracer (IOSim s) (TestTransitionTrace (IOSim s))
            cmTrTracer =
              fmap abstractState `contramap`
                   Tracer traceM
                <> Tracer (say . show)
                {--
                  - <> Tracer (\msg -> do
                  -             t <- getMonotonicTime
                  -             Debug.traceShowM (t, msg))
                  --}

        inbgovControlChannel <- ControlChannel.newControlChannel
        let connectionHandler = mkConnectionHandler snocket
        withConnectionManager
          ConnectionManagerArguments {
              cmTracer,
              cmTrTracer,
              cmMuxTracer = nullTracer,
              cmIPv4Address = myAddress,
              cmIPv6Address = Nothing,
              cmAddressType = \_ -> Just IPv4Address,
              cmSnocket = snocket,
              connectionDataFlow = \(Version df) -> df,
              cmPrunePolicy = simplePrunePolicy,
              cmConnectionsLimits = AcceptedConnectionsLimit {
                  acceptedConnectionsHardLimit = maxBound,
                  acceptedConnectionsSoftLimit = maxBound,
                  acceptedConnectionsDelay     = 0
                },
              cmTimeWaitTimeout = testTimeWaitTimeout
            }
            connectionHandler
            (\_ -> HandshakeFailure)
            (InResponderMode inbgovControlChannel)
          $ \(connectionManager
                :: ConnectionManager InitiatorResponderMode (FD (IOSim s))
                                     Addr (Handle m) Void (IOSim s)) -> do
            fd <- open snocket TestFamily
            case myAddress of
              Just localAddr ->
                bind snocket fd localAddr
              Nothing ->
                pure ()

            let go :: HasCallStack
                   => [Async (IOSim s) ()]
                   -> Accept (IOSim s) (FD (IOSim s)) Addr
                   -> [(Time, Addr, RefinedScheduleEntry)]
                   -> IOSim s [Async (IOSim s) ()]
                go threads _acceptOne [] = pure threads
                go threads (Accept acceptOne) ((time, addr, conn) : conns') =
                  case conn of
                    ScheduleOutbound {} -> do
                      time' <- getMonotonicTime
                      let delay = time `diffTime` time'
                      when (delay < 0) (throwIO NegativeDelayError)
                      threadDelay delay
                      thread <-
                        asyncWithUnmask $ \unmask -> unmask $ do
                          labelThisThread ("th-outbound-"
                                           ++ show (getTestAddress addr))
                          r <-
                            (Right <$>
                                -- 1s is the longest delay for `connect` call,
                                -- another 5s is the longest delay for
                                -- handshake negotiation.
                                timeout (1 + 5 + testTimeWaitTimeout)
                                  (requestOutboundConnection
                                    connectionManager addr))
                            `catches`
                              [ Handler $ \(e :: IOException) -> return (Left (toException e))
                              , Handler $ \(e :: SomeConnectionManagerError) ->
                                              case e of
                                                SomeConnectionManagerError (ForbiddenConnection {}) ->
                                                  return $ Left (toException e)
                                                SomeConnectionManagerError (ConnectionExists {}) ->
                                                  return $ Left (toException e)
                                                _ -> throwIO e
                              ]
                          case r of
                            Left _  -> pure ()

                            Right Nothing ->
                              throwIO (TimeoutError "requestOutboundConnection")

                            Right (Just (Disconnected {})) -> pure ()

                            Right (Just (Connected _ _ handle)) -> do
                              threadDelay (either id id (seActiveDelay conn))
                              -- if this outbound connection is not
                              -- executed within inbound connection,
                              -- we need to manually
                              -- 'unregisterInboundConnection'.  We
                              -- need to do that concurrently, since
                              -- 'unregisterOutboundConnection' can
                              -- block.
                              case seActiveDelay conn of
                                Left  _ -> killThread (hThreadId handle)
                                Right _ -> do
                                  (res, _) <-
                                    unregisterOutboundConnection
                                      connectionManager addr
                                    `concurrently`
                                      (when ( not (siReused (seExtra conn))
                                              && seDataFlow conn == Duplex ) $
                                         void $
                                           unregisterInboundConnection
                                             connectionManager addr
                                      )
                                  case res of
                                    UnsupportedState st ->
                                      throwIO (UnsupportedStateError
                                                "unregisterOutboundConnection"
                                                st)
                                    OperationSuccess _ -> pure ()


                      go (thread : threads) (Accept acceptOne) conns'

                    ScheduleInbound {} -> do
                      r <- acceptOne
                      time' <- getMonotonicTime
                      when (time /= time')
                           (throwIO (EventTimingError
                                      (concat [ show time'
                                              , " ≠ "
                                              , show time
                                              ])))
                      case r of
                        (Accepted fd' addr', acceptNext) -> do
                          thread <-
                            async $ do
                              labelThisThread ("th-inbound-"
                                                ++ show (getTestAddress addr))
                              Just conn' <-
                                fdScheduleEntry
                                  <$> atomically (readTVar (fdState fd'))
                              when (addr /= addr' && seIdx conn /= seIdx conn') $
                                throwIO (MismatchedScheduleEntry (addr, seIdx conn)
                                                                 (addr', seIdx conn'))
                              _ <-
                                includeInboundConnection
                                  connectionManager fd' addr
                              t <- getMonotonicTime

                              let activeDelay = either id id (seActiveDelay conn)
                              r <-
                                -- we race: if 'seActiveDelay' is 'Left' the
                                -- thread that does remote transitions will be
                                -- interrupted.
                                race
                                  (forM_ (seRemoteTransitions conn) $ \(x, y) -> do
                                    threadDelay x
                                    _ <-
                                      promotedToWarmRemote
                                        connectionManager addr
                                    threadDelay y
                                    _ <-
                                      demotedToColdRemote
                                        connectionManager addr
                                    return ()
                                  )
                                  (threadDelay activeDelay)
                              let waitUntil = activeDelay
                                              `addTime` t
                              t' <- getMonotonicTime
                              unless (t' <= waitUntil)
                                   (throwIO (EventTimingError
                                              (concat
                                                [ show t'
                                                , " ≰ "
                                                , show waitUntil
                                                ])))
                              assert (t' <= waitUntil) $
                                threadDelay (waitUntil `diffTime` t')

                              case r of
                                Right  _ ->
                                  return ()
                                Left _ ->
                                  -- TODO: should we run 'unregisterInboundConnection' depending on 'seActiveDelay'
                                  void $
                                    unregisterInboundConnection
                                      connectionManager addr
                          go (thread : threads) acceptNext conns'
                        (AcceptFailure err, _acceptNext) ->
                          throwIO err

            -- run poor man's server which just reads the control channel,
            -- otherwise it would block if there are more than 10 connections.
            forever (atomically (ControlChannel.readMessage inbgovControlChannel) $> ())
              `race_`
              (do a <- accept snocket fd
                  threads <- go [] a (schedule scheduleMap)
                  -- awaits until all 'Promise's are resolved (or throw an exception)
                  traverse_ (waitCatch >=> checkException) threads
              )

            atomically $ numberOfConnections connectionManager

    checkException :: Either SomeException a -> IOSim s ()
    checkException Right {}   = pure ()
    checkException (Left err) = do
      case fromException err :: Maybe SomeConnectionManagerError of
        Nothing -> throwIO err
        -- 'ConnectionExists', 'ImpossibleConnection' and 'ConnectionFailure'
        -- are excluded by the of generator.  On any of these exception the test
        -- will fail.
        Just (SomeConnectionManagerError e@ImpossibleConnection {}) -> throwIO e
        Just (SomeConnectionManagerError e@ImpossibleState {})      -> throwIO e
        Just (SomeConnectionManagerError e@ForbiddenOperation {})   -> throwIO e
        Just (SomeConnectionManagerError e@UnknownPeer {})          -> throwIO e

        -- If 'ForbiddenConnection' is thrown we let the test continue.
        Just (SomeConnectionManagerError ForbiddenConnection {})    -> pure ()
        Just (SomeConnectionManagerError ConnectionExists {})       -> pure ()
        Just (SomeConnectionManagerError ConnectionTerminating {})  -> pure ()
        Just (SomeConnectionManagerError ConnectionTerminated {})   -> pure ()


-- | This includes the @Overwritten@ transition.
--
unit_overwritten :: Property
unit_overwritten =
    prop_connectionManagerSimulation
      (SkewedBool True)
      (ScheduleMap $ Map.fromList
        [ ( TestAddress 1
          , Schedule
            [ ScheduleOutbound {
                seIdx = Idx 0,
                seStart = 0,
                seConnDelay = Left 0.1,
                seHandshakeDelay = 0,
                seDataFlow = Duplex,
                seActiveDelay = Left 0,
                seExtra =
                  ScheduleInfo {
                    siExists = False,
                    siReused = False,
                    siForbidden = False,
                    siOverwrite = False,
                    siBlockHandshake = Nothing
                  }
              }
            , ScheduleInbound {
                seIdx = Idx 1,
                seStart = 0,
                seHandshakeDelay = 1,
                seDataFlow = Duplex,
                seActiveDelay = Right 1,
                seRemoteTransitions = [],
                seExtra =
                  ScheduleInfo {
                    siExists = False,
                    siReused = False,
                    siForbidden = False,
                    siOverwrite = True,
                    siBlockHandshake = Nothing
                  }
              }
            ]
          )
        ]
      )


-- | This includes @TimeExpired@ followed by @DemotedToCold^{Duplex}_{Remote}@.
-- Unfortunately, there's no way to simulate @TimeExpired@ followed by
-- @PromotedToWarm^{Duplex}_{Remote}@.
--
unit_timeoutExpired :: Property
unit_timeoutExpired =
    prop_connectionManagerSimulation
      (SkewedBool True)
      (ScheduleMap $ Map.fromList
        [ ( TestAddress 1
          , Schedule
            [ ScheduleInbound {
                seIdx = Idx 0,
                seStart = 0,
                seHandshakeDelay = 0,
                seDataFlow = Duplex,
                seActiveDelay = Right 5,
                seRemoteTransitions = [],
                seExtra =
                  ScheduleInfo {
                    siExists = False,
                    siReused = False,
                    siForbidden = False,
                    siOverwrite = True,
                    siBlockHandshake = Nothing
                  }
              }
            , ScheduleOutbound {
                seIdx = Idx 1,
                seStart = 1,
                seConnDelay = Right 0,
                seHandshakeDelay = 0,
                seDataFlow = Duplex,
                seActiveDelay = Right 10,
                seExtra =
                  ScheduleInfo {
                    siExists = False,
                    siReused = True,
                    siForbidden = False,
                    siOverwrite = False,
                    siBlockHandshake = Nothing
                  }
              }
            ]
          )
        ])



--
-- Utils
--

(/\) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(/\) f g = getAll . ((All . f) <> (All . g))

infixr 3 /\

(\/) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(\/) f g = getAny . ((Any . f) <> (Any . g))

infixr 2 \/

implies :: Bool -> Bool -> Bool
implies a b = not a || b

-- lower than fixity of `&&` and `||`
infixr 1 `implies`

mkLabel :: Int -- ^ width
        -> Int -- ^ result
        -> String
mkLabel _ n  | n == 0 = show n
mkLabel a n =
  let min_ = (n `div` a) * a
  in concat [ if min_ == 0 then "(" else "["
            , show min_
            , ", "
            , show (min_ + a)
            , ")"
            ]


both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f

--
-- QuickCheck Utils
--


-- Exclude `seIdx` and `seExtra` from the `Ord` instance.  We want to measure
-- progress of the shrinker with this instance.
--
smallerScheduleEntry :: ScheduleEntry extra -> ScheduleEntry extra -> Bool
smallerScheduleEntry = \a b ->
    case (a, b) of
      (ScheduleOutbound {}, ScheduleInbound {})  -> True
      (ScheduleInbound {},  ScheduleOutbound {}) -> False
      (ScheduleOutbound {}, ScheduleOutbound {}) ->
           f a b
        && seConnDelay a <= seConnDelay b
      (ScheduleInbound {},  ScheduleInbound {})  ->
           f a b
        && smallerList (<=)
                       (seRemoteTransitions a)
                       (seRemoteTransitions b)
  where
    f s s' = seStart s          <= seStart s'
          && seDataFlow s       <= seDataFlow s'
          && seHandshakeDelay s <= seHandshakeDelay s'
          && seActiveDelay s    <= seActiveDelay s'
          && seDataFlow s       <= seDataFlow s'

-- | Check that a list is smaller (`<=`), than the other one.
-- It is useful for checking progress of a shrinker.
--
smallerList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
smallerList smaller as bs =
    case length as `compare` length bs of
      LT -> True
      EQ -> all (uncurry smaller) (as `zip` bs)
      GT -> False

prop_smallerList :: [NonNegative Int] -> Bool
prop_smallerList as
    = all (\as' -> smallerList (<=) as' as)
    . shrink
    $ as


smallerMap :: Ord k => (a -> a -> Bool) -> Map k a -> Map k a -> Bool
smallerMap smaller as bs =
     Map.keysSet as `Set.isSubsetOf` Map.keysSet bs
  && getAll
      (fold
        (Map.intersectionWith (\a b -> All (a `smaller` b)) as bs))


shrinkMap :: Ord k => (a -> [a]) -> Map k a -> [Map k a]
shrinkMap shrinkValue m =
    let as = Map.toList m in

        -- shrink the map
        [ Map.fromList as'
        | as' <- shrinkList (const []) as
        ]

        -- shrink values, one at a time
     ++ [ Map.insert k a' m
        | (k, a) <- as
        , a'     <- shrinkValue a
        ]

prop_smallerMap :: Map Int (NonNegative Int) -> Bool
prop_smallerMap as =
      all (\as' -> smallerMap (<=) as' as)
    . shrinkMap shrink
    $ as
