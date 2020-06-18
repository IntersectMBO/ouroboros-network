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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TupleSections              #-}

-- 'TestAddress' 'Arbitrary' instance.
{-# OPTIONS_GHC -Wno-orphans                   #-}
-- 'ScheduleEntry' have partial fields.
{-# OPTIONS_GHC -Wno-partial-fields            #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Test.Ouroboros.Network.ConnectionManager
  ( tests
  ) where

import           Prelude hiding (read)

import           Control.Exception (AssertionFailed (..), IOException, assert)
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

import           Data.Bifunctor (bimap)
import           Data.Either (isLeft)
import           Data.Functor (void, ($>))
import           Data.Foldable (foldl', traverse_, forM_)
import           Data.List (intercalate, sortOn)
import           Data.Maybe (isJust)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (All (..), First (..), Any (..))
import           Data.Void (Void)
import           Quiet

import           Network.Mux.Types

import           Test.QuickCheck hiding (shrinkMap)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.Snocket (Snocket (..), Accept (..), Accepted (..),
                   AddressFamily(TestFamily), TestAddress (..))
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.MuxMode
import           Ouroboros.Network.Server.RateLimiting
import qualified Ouroboros.Network.Server2.ControlChannel as ControlChannel

import           Ouroboros.Network.Testing.Utils (Delay (..), SmallDelay (..),
                   genDelayWithPrecision)
import qualified Debug.Trace as Debug


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.ConnectionManager"
  [ testProperty "ScheduleEntry generator"   prop_generator_ScheduleEntry
  , testProperty "ScheduleEntry shrinker"    prop_shrinker_ScheduleEntry
  , testProperty "RefinedSchedule generator" prop_generator_RefinedSchedule
  , testProperty "RefinedSchedule shrinker"  prop_shrinker_RefinedSchedule
  , testProperty "ReuseSchedule generator"   prop_generator_ReuseSchedule
  , testProperty "fixupSchedule"             prop_fixupSchedule
  , testProperty "connection manager"        prop_connectionManager
  ]


-- | Addresss type.  '0' indicates local address, the 'Arbitrary' generator only
-- returns (strictly) positive integers.
--
type Addr = TestAddress Int


instance Arbitrary Addr where
    arbitrary =
      TestAddress <$>
        -- from one side we want a small address pool (this makes a greater
        -- chance of reusing a connection), but we also want to allow
        -- variability
        frequency [ (66, elements [1..4])
                  , (33, suchThat arbitrary (\a -> a > 5 && a < 25) )
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


data ConnState = UnconnectedState
               | ConnectedState
               | AcceptedState
               | ListeningState
               | ClosedState
  deriving Eq

data Bound = Bound | NotBound

data FDState = FDState {
    fdLocalAddress    :: Addr,
    fdRemoteAddress   :: Maybe Addr,
    fdConnectionState :: ConnState,
    fdBound           :: Bound,
    fdScheduleEntry   :: Maybe RefinedScheduleEntry
  }

newtype FD m = FD { fdState :: StrictTVar m FDState }

-- TODO: use `IOException` instead
data SnocketErr =
      InvalidArgument
    | AcceptErr
    | ConnectErr
    | BindErr
    | ListenErr
  deriving Show

instance Exception SnocketErr


-- | A pure snocket.  Reading always blocks forever, writing is immediate.
--
-- This very roughly captures socket semantics, but it's good enough for the
-- time being for the testing we want to do.  In particular this does not rule
-- out situations which the kernel would forbid, e.g. the two connections with
-- the same four-tuples.
--
mkSnocket :: forall m.
             ( MonadDelay m
             , MonadMask  m
             , MonadMonotonicTime m
             , MonadSTM   m
             , MonadThrow (STM m)
             )
          => [Addr]
          -- ^ list of remote addresses which connect to us; this allows to
          -- construct 'accept' record field of 'Snocket'.
          -> RefinedScheduleMap Addr
          -- ^ we need the schedule to know how much time 'connect' will take
          -- and weather it errors or not.
          -> m (Snocket m (FD m) Addr)
mkSnocket _remoteAddresses (ScheduleMap scheduleMap) = do
    -- we only keep track of outbound connections which will call 'connect'.
    v <- newTVarIO (    filter (    isScheduleOutbound
                                 /\ not . siExists . sExtra
                                 /\ not . siReused . sExtra
                               )
                    <$> scheduleMap)
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
        Just [] -> throwSTM (AssertionFailed "mkSnocket.pop: invariant violation")
        Just (x : xs) -> writeTVar v (Map.insert addr xs m)
                      $> x

    getLocalAddr (FD v) =
      fdLocalAddress <$> atomically (readTVar v)

    getRemoteAddr (FD v) = do
      mbRemote <- fdRemoteAddress <$> atomically (readTVar v)
      case mbRemote of
        Nothing   -> throwIO InvalidArgument
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
          _ -> throwIO ConnectErr
        return se
      Debug.traceShowM ("connect", sIdx se)
      case sConnDelay se of
        Left d  -> when (d > 0) (threadDelay d)
                >> throwIO (userError $ "connect: errored (schedule.idx " ++ show (sIdx se) ++ ")")
        Right d -> threadDelay d


    bind (FD fd) localAddr =
      atomically $ do
        fds@FDState { fdBound } <- readTVar fd
        case fdBound of
          NotBound -> writeTVar fd fds { fdLocalAddress = localAddr
                                       , fdBound = Bound
                                       }
          Bound -> throwIO BindErr

    accept :: FD m -> Accept m (FD m) Addr
    accept (FD fd) = Accept $ go initialAcceptSchedule
      where
        initialAcceptSchedule :: [(Time, (Addr, RefinedScheduleEntry))]
        initialAcceptSchedule =
            sortOn fst
          . concatMap (\(a, ss) -> map (\(t, s) -> (t, (a, s))) ss)
          . Map.assocs
          . fmap ( reverse
                 . filter (not . isScheduleOutbound . snd)
                 . snd
                 . foldl' (\(!t, !acc) s ->
                            let t' = sStart s `addTime` t
                            in (t', (t', s) : acc)
                          )
                          (Time 0, [])
                 )
          $ scheduleMap

        go [] = pure (AcceptFailure (toException AcceptErr), Accept $ go [])
        go ((blockUntil, (remoteAddr, se)) : xs) = do
          t <- getMonotonicTime
          let delta = blockUntil `diffTime` t
          when (delta > 0) $ threadDelay delta
          fd' <- atomically $ do
            FDState { fdLocalAddress = localAddr } <- readTVar fd
            newTVar FDState {
                        fdLocalAddress    = localAddr,
                        fdRemoteAddress   = Just remoteAddr,
                        fdConnectionState = AcceptedState,
                        fdBound           = Bound,
                        fdScheduleEntry   = Just se
                      }
          pure (Accepted (FD fd') remoteAddr, Accept $ go xs)

    toBearer _ _ _ =
      MuxBearer {
          write   = \_ _ -> getMonotonicTime,
          read    = \_ -> forever (threadDelay 3600),
          sduSize = 1500
        }

    listen (FD fd) = atomically $ do
      fds@FDState{ fdConnectionState } <- readTVar fd
      case fdConnectionState of
        UnconnectedState ->
          writeTVar fd (fds { fdConnectionState = ListeningState })
        _ -> throwIO ListenErr

    close (FD fd) =
      uninterruptibleMask_
        $ atomically
        $ modifyTVar fd (\fds -> fds { fdConnectionState = ClosedState })


-- | Shedule entry.  Both inbound and outbound mark the time while the
-- connection is used by either inbount / outbound side.  For inbound entries we
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
-- connection overwritting the state of the one that was already accepted), but
-- it would never happen in a real system (forbidden by TCP rules).
--
data ScheduleEntry extra =
    -- | Outbound connection which successful handshake negotiation.
    -- @
    --    sStart sConnDealy sHandshakeDelay sActiveDelay
    -- ───•──────•──────────•───────────────•───▶
    --                                          τ
    -- @
    -- or outbound connection where handshake timed out.
    -- @
    --    sStart sConnDealy                 sActiveDelay
    -- ───•──────•──────────────────────────•───▶
    --                                          τ
    -- @
    -- or outbound connections where connect fails:
    -- @
    --    sStart sConnDealy (failure)
    -- ───•──────•──────────────────────────────▶
    --                                          τ
    -- @
    --
    -- 'sStart' is a delay from when last connection was scheduledr;
    -- 'sHandshakeDelay' and 'sActiveDelay' is a delay from 'sStart'
    --
    ScheduleOutbound {
        sIdx            :: Int,
        sStart          :: DiffTime,
        sConnDelay      :: Either DiffTime DiffTime,
        -- ^ 'connect' delay, 'Left' indicate a connection error after given
        -- delay.
        --
        -- TODO: use 'Left' with non-positive delay as an error in
        -- 'openToConnect'.
        --
        sHandshakeDelay :: DiffTime,
        sActiveDelay    :: DiffTime,
        sDataFlow       :: DataFlow,
        -- ^ delay since 'sStart' or 'sHandshakeDelay'.
        sExtra          :: extra
      }
    -- | Inbound connection which finished handshake negotiation.
    -- @
    --    sStart   sHandshakeDelay  remotePromoteToWarm  remoteDemoteToCold  sActiveDelay
    -- ───•──────•──────────────────•────────────────────•────────⋯⋯─────────•───▶
    --                                                                           τ
    -- @
  | ScheduleInbound {
        sIdx               :: Int,
        sStart             :: DiffTime,
        sHandshakeDelay    :: DiffTime,
        sActiveDelay       :: DiffTime,
        sDataFlow          :: DataFlow,
        sRemoteTransitions :: [(DiffTime, DiffTime)],
        -- ^ (delay, lenght)
        sExtra             :: extra
      }
  deriving (Eq, Functor, Show)


isScheduleOutbound :: ScheduleEntry extra -> Bool
isScheduleOutbound ScheduleOutbound {} = True
isScheduleOutbound ScheduleInbound  {} = False


-- Generate 'ScheduleOutbound' which lifetime is limited by the size parameter.
-- Some small number of generated cases might be blocked on handshake.
--
genScheduleOutbound :: Int -> Gen (ScheduleEntry ())
genScheduleOutbound size = do
    sIdx   <- arbitrary
    sStart <- frequency
     [ ( 1
       , resize (size `div` 4) (genDelayWithPrecision 1)
       )
     , ( 9
       , resize (size + size `div` 2) (genDelayWithPrecision 1)
         `suchThat` (>= realToFrac (size - size `div` 16))
       )
     ]
    sConnDelay <- frequency
      [ (9, Right <$> resize 1 (genDelayWithPrecision 10))
      , (1, Left  <$> resize 1 (genDelayWithPrecision 10))
      ]
    maxActive <- resize size (genDelayWithPrecision 10)
                 `suchThat` (> 0)
    sHandshakeDelay
      <- suchThat (getSmallDelay <$> arbitrary)
                  (< (maxActive / 3) `max` 0.1)
    sActiveDelay <- resize (round maxActive) (genDelayWithPrecision 10)
                    `suchThat` (\a -> a + sHandshakeDelay <= maxActive)
    sDataFlow <- elements [ Unidirectional, Duplex ]
    return ScheduleOutbound {
        sIdx,
        sStart,
        sConnDelay,
        sHandshakeDelay,
        sActiveDelay,
        sDataFlow,
        sExtra = ()
      }

genScheduleInbound :: Int -> Gen (ScheduleEntry ())
genScheduleInbound size = do
    sIdx   <- arbitrary
    sStart <-
      frequency
        [ (1,  return 0)
        , (3,  resize 1 (genDelayWithPrecision 10))
        , (16, suchThat (resize 100 (genDelayWithPrecision 1)) (> 60))
        ]
    maxActive <- resize size (genDelayWithPrecision 10)
                 `suchThat` (> 0)
    sHandshakeDelay
      <- suchThat (getSmallDelay <$> arbitrary)
                  (< (maxActive / 3) `max` 0.1)
    sDataFlow <- frequency [ (1, pure Unidirectional)
                           , (2, pure Duplex)
                           ]
    sActiveDelay <- resize (round maxActive) (genDelayWithPrecision 10)
                    `suchThat` (\a -> a + sHandshakeDelay <= maxActive)
    let size' = size `div` 5
    sRemoteTransitions <-
          fixupRemoteTransitions sActiveDelay
      <$> listOf ((,) <$> resize size' (genDelayWithPrecision 10)
                      <*> resize size' (genDelayWithPrecision 10))
    return ScheduleInbound {
        sIdx,
        sStart,
        sHandshakeDelay,
        sActiveDelay,
        sRemoteTransitions,
        sDataFlow,
        sExtra = ()
      }


-- TODO: this generator needs to be tuned. We ought to have greater change for
-- generating edge cases:
-- * race conditions between inbound / outbound connections
-- * ScheduleInbound should be refined to contains information when remote
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

        go (sa@ScheduleOutbound { sStart
                                , sConnDelay
                                , sHandshakeDelay
                                , sActiveDelay
                                , sDataFlow
                                }) =
            [ sa { sStart = sStart' }
            | Delay sStart' <- shrink (Delay sStart)
            ]
          ++
            [ sa { sConnDelay = bimap getDelay getDelay sConnDelay' }
            | sConnDelay' <- shrink (bimap Delay Delay sConnDelay)
            ]
          ++
            [ sa { sActiveDelay = sActiveDelay'
                 }
            | Delay sActiveDelay' <- shrink (Delay sActiveDelay)
            , sActiveDelay' >= 0
            ]
          ++
            [ sa { sHandshakeDelay = sHandshakeDelay' }
            | sHandshakeDelay' <- getDelay `map` shrink (Delay sHandshakeDelay)
            ]
          ++
            [ sa { sDataFlow = sDataFlow' }
            | sDataFlow' <- shrinkDataFlow sDataFlow
            ]
        go (sa@ScheduleInbound { sStart
                               , sHandshakeDelay
                               , sActiveDelay
                               , sDataFlow
                               , sRemoteTransitions
                               }) =
            [ sa { sStart = sStart' }
            | Delay sStart' <- shrink (Delay sStart)
            ]
          ++
            [ sa { sActiveDelay = sActiveDelay'
                 , sRemoteTransitions =
                     fixupRemoteTransitions
                       sActiveDelay'
                       sRemoteTransitions
                 }
            | Delay sActiveDelay' <- shrink (Delay sActiveDelay)
            ]
          ++
            [ sa { sHandshakeDelay = sHandshakeDelay'
                 }
            | sHandshakeDelay' <-
                getDelay `map` shrink (Delay sHandshakeDelay)
            ]
          ++
            [ sa { sRemoteTransitions = sRemoteTransitions'' }
            | sRemoteTransitions' <- shrinkList (const [])
                                                (bimap Delay Delay `map` sRemoteTransitions)
            , let sRemoteTransitions'' =
                      fixupRemoteTransitions sActiveDelay
                    . map (bimap getDelay getDelay)
                    $ sRemoteTransitions'
            ]
          ++
            [ sa { sDataFlow = sDataFlow' }
            | sDataFlow' <- shrinkDataFlow sDataFlow
            ]


-- make sure that remote transition schedule is contained while the
-- inbound connection is active.
fixupRemoteTransitions :: DiffTime -> [(DiffTime, DiffTime)] -> [(DiffTime, DiffTime)]
fixupRemoteTransitions active = reverse . snd . foldl' f (0, [])
  where
    f as@(t, !acc) a@(d, l) | s <- t + d + l
                            , s <= active    = (s, a : acc)
                            | otherwise      = as


scheduleEntryInvariant :: ScheduleEntry extra -> Bool
scheduleEntryInvariant sa =
       sStart sa >= 0
    && sHandshakeDelay sa >= 0
    && sActiveDelay sa >= 0
    && case sa of
        ScheduleInbound {} ->
            foldl' (\acc (d, l) -> acc + d + l) 0 (sRemoteTransitions sa)
          < sActiveDelay sa
        ScheduleOutbound {} -> True


prop_generator_ScheduleEntry :: ScheduleEntry () -> Property
prop_generator_ScheduleEntry s =
    label (case s of
            ScheduleOutbound {} -> "outbound"
            ScheduleInbound {}  -> "inbound"
          ) $

    label (mkLabel 20 . round $ entryLifeTime) $

    scheduleEntryInvariant s
  where
    -- for outbound connections we include 'connect' time into life time
    entryLifeTime :: DiffTime
    entryLifeTime =
      case s of
        ScheduleOutbound { sConnDelay = Left  connDelay } -> connDelay
        ScheduleOutbound { sConnDelay = Right connDelay, sHandshakeDelay } ->
            connDelay + sHandshakeDelay + sActiveDelay s
        ScheduleInbound { sHandshakeDelay } ->
            sHandshakeDelay + sActiveDelay s


prop_shrinker_ScheduleEntry :: ScheduleEntry () -> Bool
prop_shrinker_ScheduleEntry = all scheduleEntryInvariant . shrink


-- | Provides 'QuickCheck' instance for a list of 'ScheduleEntry'-ies.  This
-- newtype models connection schedule between two fixed endpoints of a network.
--
newtype Schedule extra = Schedule { getSchedule :: [ScheduleEntry extra] }
  deriving (Eq, Show)

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
    forAll (fixupSchedule <$> genReuseSchedule 200) $ \s ->

      label (  "reused "
            ++ mkLabel 10 ((100 * length (filter (siReused . sExtra)
                                         s))
                           `div` length s
                          )
            ) $

      label (  "not exists "
            ++ mkLabel 10 ((100 * length (filter (not . siExists . sExtra)
                                         s))
                            `div` length s
                          )
            ) $

      label (  "exists or reused "
            ++ mkLabel 10 ((100 * length (filter (    siExists . sExtra
                                                   \/ siReused . sExtra)
                                          s))
                            `div` length s
                          )
            ) $ True


--
-- Fix and refine random @[ScheduleEntry any]@.
--

-- | Internal state of the 'fixupSchedule' function.
--
data State = State {
      -- | Time when last connection started.
      time           :: !Time

    , dataFlow       :: !(First DataFlow)

    , handshakeUntil :: !(First Time)

      -- | Time when outbound connection started and when it will terminate.
    , outboundUntil  :: !(First (Time, Time))

      -- | Time when inbound connection will terminate.
    , inboundUntil   :: !(First Time)
    }
  deriving Show

-- | Used to check if at the current time we have a scheduled outbound
-- connection.
--
hasOutbound :: State -> Bool
hasOutbound = isJust . getFirst . outboundUntil

-- | Used to check if at the current time we have a scheduled inbound
-- connection.
--
hasInbound :: State -> Bool
hasInbound = isJust . getFirst . inboundUntil

isUnidirectional :: State -> Bool
isUnidirectional = maybe False (Unidirectional ==) . getFirst . dataFlow

reindexSchedule :: [ScheduleEntry any] -> [ScheduleEntry any]
reindexSchedule =
      map (\(sIdx, s) -> s { sIdx })
    . zip [0..]


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
    -- For these connections which will block on handshake that was already
    -- started by another connection.  For 'siReused` which where already
    -- negotiated this is 'False'.
  }
  deriving (Show, Eq)

type RefinedScheduleEntry       = ScheduleEntry ScheduleInfo

-- | Refine & fix up a schedule.
--
-- For each address we analyse the sequence of connections.  We keep 'State'
-- which measures time progress and keep track of existing inbound / outbound
-- connection.
--
-- NOTE: since we group by address the runtime must do the same, i.e. group by
-- address and execute each schedule in a separate thread.  Only then the
-- 'ScheduleInfo' computed by 'fixupSchedule' will agree with what can be
-- observed.
--
fixupSchedule :: forall any.
                 Show any
              => [       ScheduleEntry any]
              -> [RefinedScheduleEntry    ]
fixupSchedule =
      reindexSchedule
      -- get back the original order
    . sortOn sIdx
    . go initialState []
    . reindexSchedule
  where
    updateState :: Time -> DataFlow -> State -> State
    updateState time df State { dataFlow, handshakeUntil, outboundUntil, inboundUntil } =
      let outboundUntil' =
            (case outboundUntil of
              First (Just (_, a)) | a < time -> First Nothing
              _                              -> outboundUntil)
          inboundUntil' =
            (case inboundUntil of
              First (Just a) | a < time -> First Nothing
              _                         -> inboundUntil)
          handshakeUntil' =
            (case handshakeUntil of
              First (Just a) | a < time -> First Nothing
              _                         -> handshakeUntil)

          dataFlow' = if   isJust (getFirst inboundUntil')
                        || isJust (getFirst outboundUntil')
                        then dataFlow
                        else First (Just df)
      in State { time
               , dataFlow       = dataFlow'
               , outboundUntil  = outboundUntil'
               , inboundUntil   = inboundUntil'
               , handshakeUntil = handshakeUntil'
            }

    initialState :: State
    initialState = State
            { time           = Time 0
            , dataFlow       = First Nothing
            , handshakeUntil = First Nothing
            , outboundUntil  = First Nothing
            , inboundUntil   = First Nothing
            }

    -- this function assumes that all addr are equal!
    go :: State
       -> [RefinedScheduleEntry]
       -> [ScheduleEntry any]
       -> [RefinedScheduleEntry]
    go _s !acc [] = reverse acc

    go !s !acc (a@ScheduleOutbound { sStart
                                   , sConnDelay
                                   , sHandshakeDelay
                                   , sDataFlow
                                   , sActiveDelay
                                   } : as) =
      let t  = sStart `addTime` time s
          s' = updateState t sDataFlow s
          -- 'requestOutboundConnection' blocks for 'testTimeWaitTimeout' if
          -- there exists a connection in 'TerminatingState'. @t'@ is the
          -- effective time when the connection will start.
          -- Note: we use @s@ to compute @t'@ rather than @s'@, because
          -- 'updateState' does not takes the above into account.  This allows
          -- us to test more scenarios.
          t' = case outboundUntil s of
                First Nothing                  -> t
                First (Just (_started, until)) -> t `max`
                                                    (testTimeWaitTimeout `addTime` until)
          hasOutbound' = hasOutbound s'
          hasInbound'  = hasInbound  s'
      -- @exists || reused@ is true only if 'includeOutboundConnection' will
      -- call 'connect'.
      in case sConnDelay of
        -- no outbound nor inbound connection; 'connect' fails
        (Left connDelay) | not hasOutbound'
                         , not hasInbound' ->
          let outboundUntil' = First (Just (t, connDelay `addTime` t'))
              si = ScheduleInfo {
                    siExists         = False,
                    siReused         = False,
                    siForbidden      = False,
                    siOverwrite      = False,
                    siBlockHandshake = Nothing
                  }
              a'  = a  { sExtra = si }
              s'' = s' { outboundUntil = outboundUntil' }
          in go s'' (a' : acc) as

        -- no outbound nor inbound connection; 'connect' succeeds
        (Right connDelay ) | not hasOutbound'
                           , not hasInbound' ->
          let handshakeUntil' =     sHandshakeDelay `addTime` connDelay `addTime` t'
              outboundUntil'  = (t, sActiveDelay    `addTime` handshakeUntil')

              s'' = s' { outboundUntil  = First (Just outboundUntil')
                       , handshakeUntil = First (Just handshakeUntil')
                       }
              si = ScheduleInfo {
                    siExists         = False,
                    siReused         = False,
                    siForbidden      = False,
                    siOverwrite      = False,
                    siBlockHandshake = Nothing
                  }
              a'  = a  { sExtra = si }
          in go s'' (a' : acc) as

        -- if there exists outbound connection, we never call 'connect'
        _ | hasOutbound' ->
          let si = ScheduleInfo {
                    siExists         = True,
                    siReused         = False,
                    siForbidden      = False,
                    siOverwrite      = False,
                    siBlockHandshake = Nothing
                  }
              a' = a { sDataFlow = 
                        case dataFlow s' of
                          First (Just df) -> df
                          First Nothing   -> error "fixupSchedule: invariant violation"
                     , sExtra = si
                     }
          in go s' (a' : acc) as

        -- if we reuse an inbound connection, we never call 'connect'
        _ | hasInbound' ->
          let si = ScheduleInfo {
                    siExists = False,
                    siReused = True,
                    siForbidden = hasInbound' && isUnidirectional s',
                    siOverwrite = False,
                    siBlockHandshake =
                      case getFirst $ handshakeUntil s of
                        Nothing -> Just True
                        Just h  -> Just (t <= h)
                  }
              a'  = a  { sDataFlow = 
                           case dataFlow s' of
                             First (Just df) -> df
                             First Nothing   -> error "fixupSchedule: invariant violation"
                       , sExtra = si }
              s'' = s' { outboundUntil = First (Just (t, sActiveDelay `addTime` t))
                       }
          in go s'' (a' : acc) as

    go !s !acc (a@ScheduleInbound { sStart
                                  , sHandshakeDelay
                                  , sDataFlow
                                  , sActiveDelay
                                  } : as) =
      let t  = sStart `addTime` time s
          s' = updateState t sDataFlow s

          -- when @t == time s@ the inbound connection will overwrite outbound
          -- connection
          hasOutbound' = case outboundUntil s' of
                           First Nothing                 -> False
                           -- after 'updateState', @_until@ is guaranteed to be
                           -- past the current time @t@.  The condition here
                           -- allows to only bypass the condition if the
                           -- outbound connection was started at the same moment
                           -- in time as the current time.  This way we allow to
                           -- generate schedule which can do the `Overwritten`
                           -- transition.
                           First (Just (started, _until)) -> started < t
          hasInbound'  = hasInbound s'
      in Debug.traceShow ( "OUTBOUND"
                         , t
                         , hasOutbound'
                         , hasOutbound s'
                         , hasInbound'
                         , s'
                         , a
                         ) $ if hasInbound' || hasOutbound'
         -- ignore an inbound connection if:
         --
         -- * there is a running inbound connection or;
         -- * an outbound connection.  For outbound connection we allow to have
         --   inbound connection started at the same time as outbound one, this
         --   simulates race condition that the connection manager can resolve.
         then go s acc as
         else
           let handshakeUntil' =
                 if not hasOutbound' && not hasInbound'
                   then First (Just (sHandshakeDelay `addTime` t))
                   else handshakeUntil s
               time' =
                case handshakeUntil' of
                  First (Just u) -> sActiveDelay `addTime` u
                  First Nothing  -> sActiveDelay `addTime` t
               siOverwrite =
                 case outboundUntil s' of
                   First Nothing                  -> False
                   First (Just (started, _until)) -> started <= t

               si = ScheduleInfo {
                   siExists        = False
                 , siReused        = False
                 , siForbidden     = False
                 , siOverwrite
                 , siBlockHandshake = Nothing
                 }
               s'' = s' { inboundUntil   = inboundUntil s' <> (First (Just time'))
                        , outboundUntil  = if siOverwrite
                                             then First Nothing
                                             else outboundUntil s'
                        , handshakeUntil = handshakeUntil'
                        }
               a'  = a  { sExtra = si }

               acc' = 
                 if siOverwrite
                   then modifyOutbound (\x -> x { sConnDelay = Left 0 }) acc
                   else acc

           in go s'' (a' : acc') as

    modifyOutbound :: (RefinedScheduleEntry -> RefinedScheduleEntry)
                   -> [RefinedScheduleEntry]
                   -> [RefinedScheduleEntry]
    modifyOutbound f as = 
      -- modify all 'ScheduleOutbound' until first that has non zero 'sStart'
      case span ((== 0) . sStart /\ isScheduleOutbound) as of
        (as', x@ScheduleOutbound {} : xs) -> map f as' ++ f x : xs
        (as', xs)                         -> map f as' ++ xs


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


-- | A basic property test for 'RefinedSchedule' with extended statistics.
--
prop_fixupSchedule :: RefinedSchedule
                   -> Property
prop_fixupSchedule (Schedule schedule) =
    let outNo = length
              . filter (\a ->
                         case a of
                           ScheduleOutbound {sExtra} -> 
                             (not . siExists /\ not . siForbidden) sExtra
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
        cs = length (filter (isJust . siBlockHandshake . sExtra) schedule)
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
            , mkLabel 25 inNo
            ]) $

    -- % of outbound connection which error
    (label $ concat
           [ "connection error "
           , if outNo > 0
               then mkLabel 5 ((100 * length ( filter (   not . siExists . sExtra
                                                       /\ not . siReused . sExtra
                                                       /\ isLeft . sConnDelay)
                                                -- 'sConnDelay' is partial function!
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
               then mkLabel 5 ((100 * length (filter (siExists . sExtra)
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
               then mkLabel 25 ((100 * length (filter (siReused . sExtra)
                                                      schedule))
                                `div` outNo
                               )
               else "0"
           , "%"
           ]) $

    -- % of all connections which:
    -- * are outbound and reuse an existing inbound connection
    -- * are blocked on ongoing handshake of the inbound connection
    (label $ concat
           [ "reuse-handshake blocking "
           , if cs > 0
               then mkLabel 25 ((100 * length (filter ((Just True ==) . siBlockHandshake . sExtra)
                                                      schedule))
                                `div` cs
                               )
               else "0"
           , "%"
           ]) $

    -- % of all connections which:
    -- * are outbound and reuse an existing inbound connection
    -- * are not blocked on ongoing handshake of the inbound connection
    (label $ concat
           [ "reuse-handshake non-blocking "
           , if cs > 0
               then mkLabel 25 ((100 * length (filter ((Just False ==) . siBlockHandshake . sExtra)
                                                      schedule))
                                `div` cs
                               )
               else "0"
           , "%"
           ]) $

    -- number of inbound connections which will overwrite an outbound connection
    label (concat
            [ "overwrite "
            ,  mkLabel 2 (length (filter (siOverwrite . sExtra)
                                         schedule))
            ]) $

    -- number of forbidden connections, i.e. outbound connections which turns
    -- out to be 'Unidirectional' and cannot be reused.
    label (concat
          [ "forbidden "
          ,  mkLabel 10 (length (filter (siForbidden . sExtra)
                                        schedule))
          ]) $

    --
    -- properties
    --
          schedule === fixupSchedule schedule
          -- foldMap: https://github.com/nick8325/quickcheck/pull/305
      .&&. conjoin (map (\a -> counterexample (show a) $
                                    if    siExists    (sExtra a)
                                       || siReused    (sExtra a)
                                       || siForbidden (sExtra a)
                                    then
                                      case a of
                                        ScheduleOutbound {} -> True
                                        ScheduleInbound  {} -> False
                                    else True
                                 && if siOverwrite (sExtra a)
                                    then
                                      case a of
                                        ScheduleOutbound {} -> False
                                        ScheduleInbound  {} -> True
                                    else True
                        )
                        schedule)


-- | Arbitrary instance used to generate a connection schedule between two
-- endpoints.
--
instance Arbitrary RefinedSchedule where
    arbitrary = do
      NonNegative n <- resize 20 arbitrary
      as <- concat
        <$> vectorOf n
              (frequency
                [ (3, resize 10 (listOf1 arbitrary))
                , (7, genReuseSchedule 200)
                ])
      return (Schedule (fixupSchedule as))

    shrink = map Schedule
           . map fixupSchedule
           . shrinkList shrink
           . map ($> ()) -- ScheduleInfo does not have arbitrary instance
           . getSchedule
{-
      where
        shrinkList' []     = []
        shrinkList' (x:xs) = [ xs ]
                     ++ [ x:xs' | xs' <- shrinkList' xs ]
                     ++ [ x':xs | x'  <- shrink x ]
-}

prop_generator_RefinedSchedule :: RefinedSchedule -> Property
prop_generator_RefinedSchedule (Schedule s) =
       s === fixupSchedule s
  .&&. all scheduleEntryInvariant s


prop_shrinker_RefinedSchedule :: RefinedSchedule -> Property
prop_shrinker_RefinedSchedule =
      conjoin 
    . map prop_generator_RefinedSchedule
    . shrink


-- | Connection schedule for multiple nodes.  Each map entry represents
-- outbound and inbound connection to / from that node (star network topology).
--
newtype ScheduleMap' addr extra = ScheduleMap { getScheduleMap :: Map addr [ScheduleEntry extra] }
  deriving (Eq, Functor, Show)

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
      $ schedule

instance Ord addr => Semigroup (ScheduleMap' addr extra) where
    ScheduleMap a <> ScheduleMap b = ScheduleMap (Map.unionWith (flip (++)) a b)

instance Ord addr => Monoid (ScheduleMap' addr extra) where
    mempty = ScheduleMap Map.empty

type ScheduleMap          addr  = ScheduleMap'  addr ()
type RefinedScheduleMap   addr  = ScheduleMap'  addr ScheduleInfo


-- | Only used to test 'fixupSchedule'
--
instance Arbitrary (ScheduleMap Int) where
    arbitrary = do
      Small n <- arbitrary
      as <- vectorOf n arbitrary
      return (ScheduleMap (Map.fromList (zip [0.. (n - 1)] as)))


    shrink = map ScheduleMap
           . shrinkMap (shrinkList shrink)
           . getScheduleMap


instance (Arbitrary addr, Ord addr) => Arbitrary (RefinedScheduleMap addr) where
    arbitrary = do
      addrs <- resize 20 orderedList
      as <- vectorOf (length addrs)
                     -- use @'Arbitrary' 'RefinedSchedule'@
                     (getSchedule <$> arbitrary)
      return (ScheduleMap (Map.fromList (zip addrs as)))

    shrink = map ScheduleMap
           . shrinkMap ( map getSchedule
                       -- use @'Arbitrary' 'RefinedSchedule'@
                       . shrink
                       . Schedule
                       )
           . getScheduleMap


--
-- ConnectionHandler
--

-- | Connection handle.  It contains STM action which will let the connection
-- handler know that the connection is over.
--
data Handle = Handle { hScheduleEntry :: RefinedScheduleEntry }

-- | Version
data Version = Version DataFlow

-- | A connection handler.  It will block for 'sHandshakeDelay' before
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
                                         Addr Handle
                                         Void Version
                                         m
mkConnectionHandler snocket =
    ConnectionHandler $
      WithInitiatorResponderMode
        (handler "outbound-handler")
        (handler "inbound-handler")
  where
    handler :: String
            -> ConnectionHandlerFn handlerTrace (FD m) Addr Handle Void Version m
    handler label_ fd promise _ ConnectionId { remoteAddress } _ =
      MaskedAction $ \unmask ->
        do let addr = getTestAddress remoteAddress
           labelThisThread (label_ ++ "-" ++ show addr)
           Just se <- atomically $ fdScheduleEntry <$> readTVar (fdState fd)
           unmask (threadDelay (sHandshakeDelay se))
           atomically (writePromise promise
                        (Right ( Handle { hScheduleEntry = se }
                               , Version (sDataFlow se)
                               )))

           -- The connection manager throws async exception to kill the
           -- connection handler thread and is closing file descriptor using
           -- 'onException' handler, like we do here.  The
           -- `MuxConnectionHandler` is starting multiplexer at this point.
           unmask $ forever (threadDelay 3600)
        
        `onException` close snocket fd


type TestConnectionState m      = ConnectionState Addr Handle Void Version m
type TestConnectionManagerTrace = ConnectionManagerTrace Addr ()
type TestTransitionTrace m      = TransitionTrace  Addr (TestConnectionState m)
type AbstractTransitionTrace    = TransitionTrace' Addr AbstractState

-- | This property interleaves inbound and outbound connections and then
-- verifies that:
--
-- * we are reusing connections
-- * all threads forked by the connection manager are killed when the callback
--   exists
--
-- This test relies on the fact that 'Connections' exclude behaviour that throws
-- exceptions (see 'checkException' for which exceptions are not allowed).
--
-- TODO: this test does not test all possible interactions in connection
-- manager state machine.  We should specify them using something like
-- `quickcheck-state-machine`.
--
prop_connectionManager
    :: Maybe (Negative Int)
    -- ^ local address, by using a nagative integer we force it to be
    -- different from any one from the list of remote addresses.
    -> RefinedScheduleMap Addr
    -- ^ A list of addresses to which we connect or which connect to us.  We use
    -- 'Blind' since we show the arguments using `counterexample` in a nicer
    -- way.
    -> Property
prop_connectionManager myAddress' sm@(ScheduleMap schedule) =
    Debug.trace ("START\n" ++ prettyScheduleMap sm) $
    let tr = runSimTrace experiment in
    -- `selectTraceEventsDynamic`, can throw 'Failure', hence we run
    -- `traceResults` first.
    counterexample (intercalate "\n" . map show $ traceEvents tr) $
      case traceResult True tr of
        Left failure ->
          counterexample (intercalate "\n" [ displayException failure
                                           , show sm
                                           ]) False
        Right _ ->
          let cmTrace :: [AbstractTransitionTrace]
              cmTrace = selectTraceEventsDynamic tr

              numberOfReused =
                  length
                . filter (\TransitionTrace { ttTransition } ->
                           case fromState ttTransition of
                             InboundIdleSt {} -> True
                             InboundSt {}     -> True
                             _                -> False)
                $ cmTrace
          in -- classify True ("number of reused connections: " ++ show numberOfReused) $
             verifyTrace cmTrace
  where
    inboundAddresses :: [Addr]
    inboundAddresses =
         Map.keys 
       . Map.filter (any (not . isScheduleOutbound))
       $ schedule

    myAddress :: Maybe Addr
    myAddress = (\(Negative addr) -> TestAddress addr) <$> myAddress'

    verifyTrace :: [AbstractTransitionTrace] -> Property
    verifyTrace = conjoin
                . fmap (verifyTransition . ttTransition)
      where
        verifyTransition :: AbstractTransition -> Property
        verifyTransition tr = counterexample (show tr) True


    experiment :: forall s. IOSim s Int
    experiment = do
        labelThisThread "th-main"
        snocket <- mkSnocket inboundAddresses (ScheduleMap schedule)
        let cmTracer :: Tracer (IOSim s) TestConnectionManagerTrace
            cmTracer = Tracer (say . show)
                    <> Tracer (\msg -> do
                                t <- getMonotonicTime
                                Debug.traceM $ show (t, msg))

            cmTrTracer :: Tracer (IOSim s) (TestTransitionTrace (IOSim s))
            cmTrTracer =
              fmap abstractState `contramap`
                   Tracer traceM <> Tracer (say . show)
                <> Tracer (\msg -> do
                            t <- getMonotonicTime
                            Debug.traceM $ show (t, msg))

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
                                     Addr Handle Void (IOSim s)) -> do
            fd <- open snocket TestFamily
            case myAddress of
              Just localAddr ->
                bind snocket fd localAddr
              Nothing ->
                pure ()

            let go :: HasCallStack
                   => [Async (IOSim s) ()]
                   -> Accept (IOSim s) (FD (IOSim s)) Addr
                   -> Addr
                   -> [RefinedScheduleEntry]
                   -> IOSim s [Async (IOSim s) ()]
                go threads _acceptOne _ [] = pure threads
                go threads (Accept acceptOne) addr (conn : conns') =
                  case conn of
                    ScheduleOutbound {} -> do
                      threadDelay (sStart conn)
                      thread <-
                        asyncWithUnmask $ \unmask ->
                          handle
                            (\(e :: SomeConnectionManagerError) -> do
                              case e of
                                SomeConnectionManagerError (ForbiddenConnection {}) -> return ()
                                SomeConnectionManagerError (ConnectionExists {})    -> return ()
                                _ -> throwIO e
                            )
                            $ unmask
                                (do labelThisThread ("th-outbound-"
                                                     ++ show (getTestAddress addr))
                                    t <- getMonotonicTime
                                    let msg = show (t, "requestOutboundConnection", sIdx conn, sExtra conn )
                                    Debug.traceM msg
                                    say msg
                                    r <-
                                      try @_ @SomeException
                                        $ requestOutboundConnection
                                            connectionManager addr
                                    case r of
                                      Left e  -> do
                                        case fromException e of
                                          Just (AssertionFailed _) ->
                                            throwIO e
                                          Nothing -> do
                                            let msg = show ("requestOutboundConnection:error", sIdx conn, e)
                                            Debug.traceM msg
                                            say msg
                                            pure ()

                                      Right r -> do
                                        t <- getMonotonicTime
                                        Debug.traceShowM (t, "requestOutboundConnection:success", sIdx conn, case r of
                                                                                                               Connected {} -> "Connected"
                                                                                                               Disconnected {} -> "Disconnected")
                                        when (siExists (sExtra conn) || siForbidden (sExtra conn) || isLeft (sConnDelay conn)) $
                                          throwIO (AssertionFailed (concat
                                                    [ "Expected exception: "
                                                    , show conn
                                                    ]))
                                        threadDelay (sActiveDelay conn)
                                        t <- getMonotonicTime
                                        let msg = show (t, "unregisterOutboundConnection", sIdx conn)
                                        Debug.traceM msg
                                        say msg
                                        (res, _) <- 
                                          unregisterOutboundConnection
                                            connectionManager addr
                                          `concurrently`
                                            (-- if this outbound connection is not
                                             -- executed within inbound connection,
                                             -- we need to manually
                                             -- unregisterInboundConnection.
                                             unless (siReused (sExtra conn) && sDataFlow conn == Duplex) $ void $ do
                                               t <- getMonotonicTime
                                               let msg = show (t, "unregisterInboundConnection:outbound", sIdx conn)
                                               Debug.traceM msg
                                               say msg
                                               unregisterInboundConnection connectionManager addr
                                            )
                                        let msg = show ("unregisterOutboundConnection:result", sIdx conn, res)
                                        Debug.traceM msg
                                        say msg
                                        return ()
                                             
                                )
                      go (thread : threads) (Accept acceptOne) addr conns'

                    ScheduleInbound {} -> do
                      -- threadDelay (sStart conn)
                      r <- acceptOne
                      case r of
                        (Accepted fd' _, acceptNext) -> do
                          thread <-
                            async $ do
                              labelThisThread ("th-inbound-"
                                                ++ show (getTestAddress addr))
                              Just se' <-
                                fdScheduleEntry
                                  <$> atomically (readTVar (fdState fd'))
                              when (conn /= se') $
                                throwIO (AssertionFailed (concat
                                          [ "unexpected ScheduleEntry: "
                                          , show $ sIdx conn
                                          , " ≠ "
                                          , show $ sIdx se'
                                          ]))


                              t <- getMonotonicTime
                              let msg = show (t, "includeInboundConnection", sIdx conn)
                              Debug.traceM msg
                              say msg
                              _ <-
                                includeInboundConnection
                                  connectionManager fd' addr
                              t <- getMonotonicTime

                              remoteThread <- async $
                                forM_ (sRemoteTransitions conn) $ \(x, y) -> do
                                  threadDelay x
                                  t <- getMonotonicTime
                                  let msg = show (t, "promotedToWarmRemote", sIdx conn)
                                  Debug.traceM msg
                                  say msg
                                  _ <-
                                    promotedToWarmRemote
                                      connectionManager addr
                                  threadDelay y
                                  t <- getMonotonicTime
                                  let msg = show (t, "demotedToWarmRemote", sIdx conn)
                                  Debug.traceM msg
                                  say msg
                                  _ <-
                                    demotedToColdRemote
                                      connectionManager addr
                                  return ()

                              wait remoteThread
                              let waitUntil = sActiveDelay conn `addTime` t
                              t' <- getMonotonicTime
                              {--
                                - Debug.traceShowM
                                -   (t' <= waitUntil, t', waitUntil)
                                --}
                              assert (t' <= waitUntil) $
                                threadDelay (waitUntil `diffTime` t')

                              t <- getMonotonicTime
                              let msg = show (t, "unregisterInboundConnection", sIdx conn)
                              Debug.traceM msg
                              say msg
                              resUnreg <-
                                unregisterInboundConnection
                                  connectionManager addr
                              say (show resUnreg)
                              return ()
                          go (thread : threads) acceptNext addr conns'
                        (AcceptFailure err, _acceptNext) ->
                          throwIO err

            -- run poor man's server which just reads the control channel,
            -- otherwise it would block if there are more than 10 connections.
            forever (atomically (ControlChannel.readMessage inbgovControlChannel) $> ())
              `race_`
              (do threads <- Map.traverseWithKey (go [] (accept snocket fd)) schedule
                  -- awaits until all 'Promise's are resolved (or throw an exception)
                  traverse_ (waitCatch >=> checkException)
                            (concat $ Map.elems threads)
              )

            atomically $ numberOfConnections connectionManager

    checkException :: Either SomeException a -> IOSim s ()
    checkException Right {}   = pure ()
    checkException (Left err) = do
      say ("MAIN_CAUGHT: " ++ show err)
      case fromException err :: Maybe SomeConnectionManagerError of
        Nothing                        -> do
          say "MAIN_CAUGHT: RETHROW"
          throwIO err
        -- 'ConnectionExists', 'ImpossibleConnection' and 'ConnectionFailure'
        -- are excluded by the of generator.  On any of these exception the test
        -- will fail.
        Just (SomeConnectionManagerError e@ImpossibleConnection {}) -> throwIO e
        Just (SomeConnectionManagerError e@ImpossibleState {})      -> throwIO e
        Just (SomeConnectionManagerError e@ForbiddenOperation {})   -> throwIO e
        Just (SomeConnectionManagerError e@UnknownPeer {})          -> throwIO e

        -- If 'ForbiddenConnection' is thrown we let the test continue.
        Just (SomeConnectionManagerError ForbiddenConnection {})    -> pure ()
        Just (SomeConnectionManagerError ConnectionExists {})       -> do
          say "MAIN_CAUGHT: IGNORE"
          pure ()
        Just (SomeConnectionManagerError ConnectionTerminating {})  -> pure ()
        Just (SomeConnectionManagerError ConnectionTerminated {})   -> pure ()

--
-- Utils
--

(/\) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(/\) f g = getAll . ((All . f) <> (All . g))

infixr 3 /\

(\/) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(\/) f g = getAny . ((Any . f) <> (Any . g))

infixr 2 \/


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


--
--
--

sm :: RefinedScheduleMap (TestAddress Int)
sm = ScheduleMap {getScheduleMap = Map.fromList [ (TestAddress {getTestAddress = 22},
      [ScheduleOutbound {sIdx = 0, sStart = 0, sConnDelay = Right 0.033333333333, sHandshakeDelay = 0.071428571428, sActiveDelay = 0.033333333333, sDataFlow = Unidirectional,
                         sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
      ,ScheduleOutbound {sIdx = 1, sStart = 14.8, sConnDelay = Left 0, sHandshakeDelay = 0.088888888888, sActiveDelay = 0, sDataFlow = Unidirectional,
                         sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
      ,ScheduleInbound {sIdx = 2, sStart = 0, sHandshakeDelay = 4, sActiveDelay = 41.433333333333, sDataFlow = Unidirectional,
                        sRemoteTransitions = [(2,18.6),(2.166666666666,18.666666666666)],
                        sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = True, siBlockHandshake = Nothing}}
      ])]}

x :: RefinedScheduleEntry
x = ScheduleInbound {sIdx = 2, sStart = 0, sHandshakeDelay = 4, sActiveDelay = 41.433333333333, sDataFlow = Unidirectional,
                     sRemoteTransitions = [(2,18.6),(2.166666666666,18.666666666666)],
                     sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = True, siBlockHandshake = Nothing}}

sm1 :: [RefinedScheduleEntry]
sm1 = [ScheduleOutbound {sIdx = 0, sStart = 0, sConnDelay = Right 0.033333333333, sHandshakeDelay = 0.071428571428, sActiveDelay = 0.033333333333, sDataFlow = Unidirectional,
                         sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
      ,ScheduleOutbound {sIdx = 1, sStart = 0.1, sConnDelay = Left 0, sHandshakeDelay = 0.088888888888, sActiveDelay = 0, sDataFlow = Unidirectional,
                         sExtra = ScheduleInfo {siExists = True, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
      ,ScheduleInbound {sIdx = 2, sStart = 0, sHandshakeDelay = 0, sActiveDelay = 0.033333333333, sDataFlow = Unidirectional,
                        sRemoteTransitions = [],
                        sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = True, siBlockHandshake = Nothing}}]


-- TODO: this fails:
--
-- > λ quickCheck (prop_connectionManager Nothing sm2)
-- "(Time 0s,\"includeInboundConnection\",0)"
-- "(Time 0s,\"requestOutboundConnection\",1)"
-- "(Time 0s,\"unregisterInboundConnection\",0)"
-- "(Time 5s,\"requestOutboundConnection\",2)"
-- "(Time 5s,\"includeInboundConnection\",3)"
-- ("connect",1) -- THIS IS WRONG!
-- "(Time 5s,\"unregisterOutboundConnection\",2)"
-- *** Failed! (after 1 test):                            
-- Exception:
--   Assertion failed
--   CallStack (from HasCallStack):
--     assert, called at src/Ouroboros/Network/ConnectionManager/Core.hs:1276:17 in ouroboros-network-framework-0.1.0.0-inplace:Ouroboros.Network.ConnectionManager.Core
-- Exception thrown while showing test case:
--   Assertion failed
--   CallStack (from HasCallStack):
--     assert, called at src/Ouroboros/Network/ConnectionManager/Core.hs:1276:17 in ouroboros-network-framework-0.1.0.0-inplace:Ouroboros.Network.ConnectionManager.Core
--

sm2, sm3, sm4, sm5, sm6, sm7, sm8, sm9 :: RefinedScheduleMap (TestAddress Int)
sm2 = ScheduleMap {getScheduleMap = Map.fromList [(TestAddress {getTestAddress = 22},
       [ScheduleInbound {sIdx = 0, sStart = 0, sHandshakeDelay = 0, sActiveDelay = 0, sDataFlow = Unidirectional,
                         sRemoteTransitions = [],
                         sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
       ,ScheduleOutbound {sIdx = 1, sStart = 0, sConnDelay = Right 0, sHandshakeDelay = 0, sActiveDelay = 0, sDataFlow = Unidirectional,
                          sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = True, siOverwrite = False, siBlockHandshake = Just True}}
       ,ScheduleOutbound {sIdx = 2, sStart = 4.9, sConnDelay = Left 0, sHandshakeDelay = 0.025, sActiveDelay = 0, sDataFlow = Unidirectional,
                          sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
       ,ScheduleInbound {sIdx = 3, sStart = 0, sHandshakeDelay = 0, sActiveDelay = 0.033333333333, sDataFlow = Unidirectional,
                         sRemoteTransitions = [],
                         sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = True, siBlockHandshake = Nothing}}
       ])]}

sm3 = ScheduleMap {getScheduleMap = Map.fromList [(TestAddress {getTestAddress = 22},
       [ScheduleInbound {sIdx = 0, sStart = 0, sHandshakeDelay = 0, sActiveDelay = 0, sDataFlow = Unidirectional,
                         sRemoteTransitions = [],
                         sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
       ,ScheduleOutbound {sIdx = 1, sStart = 0, sConnDelay = Right 0, sHandshakeDelay = 0, sActiveDelay = 0, sDataFlow = Unidirectional,
                          sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = True, siOverwrite = False, siBlockHandshake = Just True}}
       ,ScheduleOutbound {sIdx = 2, sStart = 5, sConnDelay = Left 0, sHandshakeDelay = 0.025, sActiveDelay = 0, sDataFlow = Unidirectional,
                          sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
       ,ScheduleInbound {sIdx = 3, sStart = 0, sHandshakeDelay = 0, sActiveDelay = 0.033333333333, sDataFlow = Unidirectional,
                         sRemoteTransitions = [],
                         sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = True, siBlockHandshake = Nothing}}
       ])]}

sm4 = ScheduleMap {getScheduleMap = Map.fromList [(TestAddress {getTestAddress = 22},
        [ScheduleInbound {sIdx = 0, sStart = 0, sHandshakeDelay = 4.833333333333, sActiveDelay = 0.1, sDataFlow = Duplex,
                          sRemoteTransitions = [],
                          sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
        ,ScheduleOutbound {sIdx = 1, sStart = 0, sConnDelay = Right 0.057142857142, sHandshakeDelay = 0, sActiveDelay = 0.033333333333, sDataFlow = Unidirectional,
                           sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = True, siOverwrite = False, siBlockHandshake = Just True}}
        ,ScheduleOutbound {sIdx = 2, sStart = 4.8, sConnDelay = Right 0, sHandshakeDelay = 0.066666666666, sActiveDelay = 0, sDataFlow = Unidirectional,
                           sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = True, siOverwrite = False, siBlockHandshake = Just True}}
        ])]}

sm5 = ScheduleMap {getScheduleMap = Map.fromList [(TestAddress {getTestAddress = 22},
         [ScheduleInbound {sIdx = 0, sStart = 0, sHandshakeDelay = 4.833333333333, sActiveDelay = 0.1, sDataFlow = Duplex,
                           sRemoteTransitions = [],
                           sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
         ,ScheduleOutbound {sIdx = 1, sStart = 0, sConnDelay = Right 0.057142857142, sHandshakeDelay = 0, sActiveDelay = 0.033333333333, sDataFlow = Unidirectional,
                            sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = True, siOverwrite = False, siBlockHandshake = Just True}}
         ,ScheduleOutbound {sIdx = 2, sStart = 4.9, sConnDelay = Right 0, sHandshakeDelay = 0.066666666666, sActiveDelay = 0, sDataFlow = Unidirectional,
                            sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = True, siOverwrite = False, siBlockHandshake = Just False}}
         ])]}

sm6 = ScheduleMap {getScheduleMap = Map.fromList [(TestAddress {getTestAddress = 17},
        [ScheduleInbound {sIdx = 0, sStart = 79.8, sHandshakeDelay = 4, sActiveDelay = 45, sDataFlow = Duplex,
                          sRemoteTransitions = [(2,16.833333333333),(2,11.666666666666),(9.3,2)],
                          sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
        ,ScheduleOutbound {sIdx = 1, sStart = 24, sConnDelay = Right 1, sHandshakeDelay = 0.428571428571, sActiveDelay = 1.5, sDataFlow = Duplex,
                           sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = False, siOverwrite = False, siBlockHandshake = Just False}}
        ])]}

sm7 = ScheduleMap {getScheduleMap = Map.fromList
        [(TestAddress 3,
          [ScheduleOutbound {sIdx = 0, sStart = 21, sConnDelay = Left 0, sHandshakeDelay = 0, sActiveDelay = 2, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 1, sStart = 19, sConnDelay = Right 1, sHandshakeDelay = 1, sActiveDelay = 0, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 2, sStart = 24, sConnDelay = Right 0.857142857142, sHandshakeDelay = 1, sActiveDelay = 1.75, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 3, sStart = 21, sConnDelay = Right 0, sHandshakeDelay = 0.2, sActiveDelay = 7, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 4, sStart = 27, sConnDelay = Right 0, sHandshakeDelay = 0.111111111111, sActiveDelay = 2, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleInbound {sIdx = 5, sStart = 68, sHandshakeDelay = 2, sActiveDelay = 174.833333333333, sDataFlow = Duplex, sRemoteTransitions = [(21.6,15),(11.4,50.2),(19.666666666666,1.833333333333),(17.1 ,18)], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 6, sStart = 52, sConnDelay = Right 0.285714285714, sHandshakeDelay = 1.5, sActiveDelay = 10.125, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = False, siOverwrite = False, siBlockHandshake = Just False}}
          ,ScheduleOutbound {sIdx = 7, sStart = 54, sConnDelay = Right 0.5, sHandshakeDelay = 2.666666666666, sActiveDelay = 1.222222222222, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = False, siOverwrite = False, siBlockHandshake = Just True}}
          ,ScheduleOutbound {sIdx = 8, sStart = 54, sConnDelay = Right 0.833333333333, sHandshakeDelay = 2.285714285714, sActiveDelay = 19.8, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = False, siOverwrite = False, siBlockHandshake = Just True}}
          ,ScheduleOutbound {sIdx = 9, sStart = 0, sConnDelay = Left 0.666666666666, sHandshakeDelay = 0.142857142857, sActiveDelay = 0, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = True, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 10, sStart = 53, sConnDelay = Right 0.333333333333, sHandshakeDelay = 0.166666666666, sActiveDelay = 0, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleInbound {sIdx = 11, sStart = 0.5, sHandshakeDelay = 3, sActiveDelay = 3.5, sDataFlow = Unidirectional, sRemoteTransitions = [], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 12, sStart = 50, sConnDelay = Left 0.5, sHandshakeDelay = 1, sActiveDelay = 15.333333333333, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 13, sStart = 49, sConnDelay = Right 0.333333333333, sHandshakeDelay = 0, sActiveDelay = 15, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 14, sStart = 65, sConnDelay = Right 0.428571428571, sHandshakeDelay = 4.625, sActiveDelay = 19.3, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound{sIdx = 15, sStart = 59, sConnDelay = Right 0.75, sHandshakeDelay = 1.166666666666, sActiveDelay = 3.666666666666, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ])
        ,(TestAddress 4,
          [ScheduleInbound {sIdx = 0, sStart = 75, sHandshakeDelay = 2.571428571428, sActiveDelay = 20.6, sDataFlow = Duplex, sRemoteTransitions = [], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 1, sStart = 60, sConnDelay = Right 0.142857142857, sHandshakeDelay = 5, sActiveDelay = 15.3, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 2, sStart = 58, sConnDelay = Right 0.857142857142, sHandshakeDelay = 2.4, sActiveDelay = 9.1, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 3, sStart = 38, sConnDelay = Right 1, sHandshakeDelay = 1.666666666666, sActiveDelay = 0.75, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 4, sStart = 58, sConnDelay = Right 0, sHandshakeDelay = 3.333333333333, sActiveDelay = 4.25, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 5, sStart = 50, sConnDelay = Right 0.714285714285, sHandshakeDelay = 4.333333333333, sActiveDelay = 9, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleInbound {sIdx = 6, sStart = 70, sHandshakeDelay = 0.666666666666, sActiveDelay = 7, sDataFlow = Duplex, sRemoteTransitions = [], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 7, sStart = 29, sConnDelay = Right 0.444444444444, sHandshakeDelay = 4, sActiveDelay = 7, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 8, sStart = 32, sConnDelay = Right 0, sHandshakeDelay = 2, sActiveDelay = 16, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 9, sStart = 33, sConnDelay = Left 0.5, sHandshakeDelay = 0.666666666666, sActiveDelay = 0, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 10, sStart = 3, sConnDelay = Right 1, sHandshakeDelay = 3.8, sActiveDelay = 7.75, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 11, sStart = 32, sConnDelay = Right 0.9, sHandshakeDelay = 0.75, sActiveDelay = 5.5, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 12, sStart = 23, sConnDelay = Right 1, sHandshakeDelay = 1.857142857142, sActiveDelay = 6.2, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 13, sStart = 23, sConnDelay = Right 1, sHandshakeDelay = 4.333333333333, sActiveDelay = 12.3, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 14, sStart = 28, sConnDelay = Right 0, sHandshakeDelay = 2.166666666666, sActiveDelay = 8.4, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 15, sStart = 22, sConnDelay = Right 0.777777777777, sHandshakeDelay = 2.333333333333, sActiveDelay = 5.5, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleInbound {sIdx = 16, sStart = 97, sHandshakeDelay = 3, sActiveDelay = 14.666666666666, sDataFlow = Unidirectional, sRemoteTransitions = [], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 17, sStart = 33, sConnDelay = Right 1, sHandshakeDelay = 2, sActiveDelay = 162.777777777777, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 18, sStart = 259, sConnDelay = Right 0, sHandshakeDelay = 3.25, sActiveDelay = 12.25, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleInbound {sIdx = 19, sStart = 75, sHandshakeDelay = 3.5, sActiveDelay = 92, sDataFlow = Unidirectional, sRemoteTransitions = [], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 20, sStart = 65, sConnDelay = Right 0.4, sHandshakeDelay = 0.555555555555, sActiveDelay = 0.5, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = True, siOverwrite = False, siBlockHandshake = Just False}}
          ,ScheduleOutbound {sIdx = 21, sStart = 59, sConnDelay = Right 0.375, sHandshakeDelay = 2.5, sActiveDelay = 2.4, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 22, sStart = 47, sConnDelay = Right 0, sHandshakeDelay = 1.3, sActiveDelay = 6.857142857142, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 23, sStart = 65, sConnDelay = Right 0.6, sHandshakeDelay = 1.857142857142, sActiveDelay = 16.125, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 24, sStart = 24, sConnDelay = Left 0.166666666666, sHandshakeDelay = 0.714285714285, sActiveDelay = 2, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ])]}

sm8 = ScheduleMap {getScheduleMap = Map.fromList
        [(TestAddress 3,
          [ScheduleOutbound {sIdx = 0, sStart = 21, sConnDelay = Left 0, sHandshakeDelay = 0, sActiveDelay = 2, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 1, sStart = 19, sConnDelay = Right 1, sHandshakeDelay = 1, sActiveDelay = 0, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 2, sStart = 24, sConnDelay = Right 0.857142857142, sHandshakeDelay = 1, sActiveDelay = 1.75, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 3, sStart = 21, sConnDelay = Right 0, sHandshakeDelay = 0.2, sActiveDelay = 7, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 4, sStart = 27, sConnDelay = Right 0, sHandshakeDelay = 0.111111111111, sActiveDelay = 2, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleInbound {sIdx = 5, sStart = 68, sHandshakeDelay = 2, sActiveDelay = 174.833333333333, sDataFlow = Duplex, sRemoteTransitions = [(21.6,15),(11.4,50.2),(19.666666666666,1.833333333333),(17.1 ,18)], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 6, sStart = 52, sConnDelay = Right 0.285714285714, sHandshakeDelay = 1.5, sActiveDelay = 10.125, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = False, siOverwrite = False, siBlockHandshake = Just False}}
          ,ScheduleOutbound {sIdx = 7, sStart = 54, sConnDelay = Right 0.5, sHandshakeDelay = 2.666666666666, sActiveDelay = 1.222222222222, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = False, siOverwrite = False, siBlockHandshake = Just True}}
          ,ScheduleOutbound {sIdx = 8, sStart = 54, sConnDelay = Right 0.833333333333, sHandshakeDelay = 2.285714285714, sActiveDelay = 19.8, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = False, siOverwrite = False, siBlockHandshake = Just True}}
          ,ScheduleOutbound {sIdx = 9, sStart = 0, sConnDelay = Left 0.666666666666, sHandshakeDelay = 0.142857142857, sActiveDelay = 0, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = True, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 10, sStart = 53, sConnDelay = Right 0.333333333333, sHandshakeDelay = 0.166666666666, sActiveDelay = 0, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleInbound {sIdx = 11, sStart = 0.5, sHandshakeDelay = 3, sActiveDelay = 3.5, sDataFlow = Unidirectional, sRemoteTransitions = [], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 12, sStart = 50, sConnDelay = Left 0.5, sHandshakeDelay = 1, sActiveDelay = 15.333333333333, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 13, sStart = 49, sConnDelay = Right 0.333333333333, sHandshakeDelay = 0, sActiveDelay = 15, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 14, sStart = 65, sConnDelay = Right 0.428571428571, sHandshakeDelay = 4.625, sActiveDelay = 19.3, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound{sIdx = 15, sStart = 59, sConnDelay = Right 0.75, sHandshakeDelay = 1.166666666666, sActiveDelay = 3.666666666666, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ])]}

sm9 = ScheduleMap {getScheduleMap = Map.fromList
        [(TestAddress 4,
          [ScheduleInbound {sIdx = 0, sStart = 75, sHandshakeDelay = 2.571428571428, sActiveDelay = 20.6, sDataFlow = Duplex, sRemoteTransitions = [], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 1, sStart = 60, sConnDelay = Right 0.142857142857, sHandshakeDelay = 5, sActiveDelay = 15.3, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 2, sStart = 58, sConnDelay = Right 0.857142857142, sHandshakeDelay = 2.4, sActiveDelay = 9.1, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 3, sStart = 38, sConnDelay = Right 1, sHandshakeDelay = 1.666666666666, sActiveDelay = 0.75, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 4, sStart = 58, sConnDelay = Right 0, sHandshakeDelay = 3.333333333333, sActiveDelay = 4.25, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 5, sStart = 50, sConnDelay = Right 0.714285714285, sHandshakeDelay = 4.333333333333, sActiveDelay = 9, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleInbound {sIdx = 6, sStart = 70, sHandshakeDelay = 0.666666666666, sActiveDelay = 7, sDataFlow = Duplex, sRemoteTransitions = [], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 7, sStart = 29, sConnDelay = Right 0.444444444444, sHandshakeDelay = 4, sActiveDelay = 7, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 8, sStart = 32, sConnDelay = Right 0, sHandshakeDelay = 2, sActiveDelay = 16, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 9, sStart = 33, sConnDelay = Left 0.5, sHandshakeDelay = 0.666666666666, sActiveDelay = 0, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 10, sStart = 3, sConnDelay = Right 1, sHandshakeDelay = 3.8, sActiveDelay = 7.75, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 11, sStart = 32, sConnDelay = Right 0.9, sHandshakeDelay = 0.75, sActiveDelay = 5.5, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 12, sStart = 23, sConnDelay = Right 1, sHandshakeDelay = 1.857142857142, sActiveDelay = 6.2, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 13, sStart = 23, sConnDelay = Right 1, sHandshakeDelay = 4.333333333333, sActiveDelay = 12.3, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 14, sStart = 28, sConnDelay = Right 0, sHandshakeDelay = 2.166666666666, sActiveDelay = 8.4, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 15, sStart = 22, sConnDelay = Right 0.777777777777, sHandshakeDelay = 2.333333333333, sActiveDelay = 5.5, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleInbound {sIdx = 16, sStart = 97, sHandshakeDelay = 3, sActiveDelay = 14.666666666666, sDataFlow = Unidirectional, sRemoteTransitions = [], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 17, sStart = 33, sConnDelay = Right 1, sHandshakeDelay = 2, sActiveDelay = 162.777777777777, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 18, sStart = 259, sConnDelay = Right 0, sHandshakeDelay = 3.25, sActiveDelay = 12.25, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleInbound {sIdx = 19, sStart = 75, sHandshakeDelay = 3.5, sActiveDelay = 92, sDataFlow = Unidirectional, sRemoteTransitions = [], sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 20, sStart = 65, sConnDelay = Right 0.4, sHandshakeDelay = 0.555555555555, sActiveDelay = 0.5, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = True, siForbidden = True, siOverwrite = False, siBlockHandshake = Just False}}
          ,ScheduleOutbound {sIdx = 21, sStart = 59, sConnDelay = Right 0.375, sHandshakeDelay = 2.5, sActiveDelay = 2.4, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 22, sStart = 47, sConnDelay = Right 0, sHandshakeDelay = 1.3, sActiveDelay = 6.857142857142, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 23, sStart = 65, sConnDelay = Right 0.6, sHandshakeDelay = 1.857142857142, sActiveDelay = 16.125, sDataFlow = Duplex, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ,ScheduleOutbound {sIdx = 24, sStart = 24, sConnDelay = Left 0.166666666666, sHandshakeDelay = 0.714285714285, sActiveDelay = 2, sDataFlow = Unidirectional, sExtra = ScheduleInfo {siExists = False, siReused = False, siForbidden = False, siOverwrite = False, siBlockHandshake = Nothing}}
          ])]}
