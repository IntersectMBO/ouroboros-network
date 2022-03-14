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
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}

-- 'TestAddress' 'Arbitrary' instance.
{-# OPTIONS_GHC -Wno-orphans                   #-}
-- 'ScheduleEntry' have partial fields.
{-# OPTIONS_GHC -Wno-partial-fields            #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-name-shadowing            #-}

module Test.Ouroboros.Network.ConnectionManager
  ( tests
  , verifyAbstractTransition
  , validTransitionMap
  , allValidTransitionsNames
  ) where

import           Prelude hiding (read)

import           Control.Exception (assert)
import           Control.Monad (forever, unless, when, (>=>))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), contramap, nullTracer)

import           GHC.Generics
import           GHC.IO.Exception
import           GHC.Stack (HasCallStack)

import           Data.Foldable (forM_, traverse_)
import           Data.Functor (void, ($>))
import           Data.List (intercalate, sortOn)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (All (..))
import qualified Data.Text.Lazy as Text
import           Data.Void (Void)
import           Quiet
import           Text.Pretty.Simple (defaultOutputOptionsNoColor, pShowOpt)

import           Network.Mux.Types

import           Test.QuickCheck hiding (shrinkMap)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.Types
import qualified Ouroboros.Network.InboundGovernor.ControlChannel as ControlChannel
import           Ouroboros.Network.MuxMode
import           Ouroboros.Network.Server.RateLimiting
import           Ouroboros.Network.Snocket (Accept (..), Accepted (..),
                     AddressFamily (TestFamily), Snocket (..), TestAddress (..))



tests :: TestTree
tests =
  testGroup "Ouroboros.Network.ConnectionManager"
  [ -- generators, shrinkers properties
    -- TODO: replace these tests with 'Test.Ouroboros.Network.Server2' simulation.
    testProperty "overwritten"                    unit_overwritten
  , testProperty "timeoutExpired"                 unit_timeoutExpired
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


-- | Provides 'QuickCheck' instance for a list of 'ScheduleEntry'-ies.  This
-- newtype models connection schedule between two fixed endpoints of a network.
--
newtype Schedule extra = Schedule { getSchedule :: [ScheduleEntry extra] }
  deriving (Eq, Show, Functor)


--
-- Fix and refine random @[ScheduleEntry any]@.
--

-- | Internal state of the 'fixupSchedule' function.
--
data State = State {
      -- | Time when last connection started.
      time           :: !Time

    , dataFlow       :: !(Maybe DataFlow)

    , handshakeUntil :: !(Maybe Time)

      -- | Time when outbound connection started, when it will terminate and
      -- a boolean value which indicates if it errors or not.
    , outboundActive :: !IsActive

      -- | Time when inbound connection started and when it will terminate.
    , inboundActive  :: !IsActive
    }
  deriving Show

data IsActive = NotActive
              | IsActive { iaStartTime :: !Time
                         , iaEndTime   :: !Time
                         , iaError     :: !Bool
                         }
 deriving (Show, Eq)

-- @5s@ @TIME_WAIT@ timeout
testTimeWaitTimeout :: DiffTime
testTimeWaitTimeout = 5

testOutboundIdleTimeout :: DiffTime
testOutboundIdleTimeout = 5


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


-- | Connection schedule for multiple nodes.  Each map entry represents outbound
-- and inbound connection to / from that node (star network topology).
--
newtype ScheduleMap' addr extra =
        ScheduleMap { getScheduleMap :: Map addr (Schedule extra) }
  deriving (Eq, Functor)

instance (Show addr, Show extra)
      => Show (ScheduleMap' addr extra) where
    show (ScheduleMap schedule) =
      concat [ "ScheduleMap ( "
             , Text.unpack (pShowOpt defaultOutputOptionsNoColor schedule)
             , "\n  )"
             ]

instance Ord addr => Semigroup (ScheduleMap' addr extra) where
    ScheduleMap a <> ScheduleMap b = ScheduleMap (Map.unionWith f a b)
      where
        f (Schedule s) (Schedule s') = Schedule (s' ++ s)

instance Ord addr => Monoid (ScheduleMap' addr extra) where
    mempty = ScheduleMap Map.empty


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



-- | Linearised schedule of inbound connections.
--
inboundSchedule :: RefinedScheduleMap addr
                -> [(Time, addr, RefinedScheduleEntry)]
inboundSchedule =
      filter (isScheduleInbound . (\(_, _, a) -> a))
    . schedule


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

type TestConnectionState m       = ConnectionState Addr (Handle m) Void Version m
type TestConnectionManagerTrace  = ConnectionManagerTrace Addr ()
type TestTransitionTrace m       = TransitionTrace  Addr (TestConnectionState m)
type TestAbstractTransitionTrace = AbstractTransitionTrace Addr


verifyAbstractTransition :: AbstractTransition
                         -> Bool
verifyAbstractTransition Transition { fromState, toState } =
    case (fromState, toState) of
      --
      -- Outbound
      --

      -- @Reserve@
      (TerminatedSt, ReservedOutboundSt) -> True
      (UnknownConnectionSt, ReservedOutboundSt) -> True
      -- @Connected@
      (ReservedOutboundSt, UnnegotiatedSt Outbound) -> True
      -- @Negotiated^{Unidirectional}_{Outbound}@
      (UnnegotiatedSt Outbound, OutboundUniSt)  -> True
      -- @Negotiated^{Duplex}_{Outbound}@
      (UnnegotiatedSt Outbound, OutboundDupSt Ticking) -> True
      (UnnegotiatedSt _,        TerminatingSt) -> True

      -- @DemotedToCold^{Unidirectional}_{Local}@
      (OutboundUniSt, OutboundIdleSt Unidirectional) -> True
      -- @TimeoutExpired@
      (OutboundDupSt Ticking, OutboundDupSt Expired) -> True
      -- @DemotedToCold^{Duplex}_{Local}@
      (OutboundDupSt Expired, OutboundIdleSt Duplex) -> True
      -- identity transition executed by 'demotedToColdRemote'
      (OutboundIdleSt dataFlow, OutboundIdleSt dataFlow') -> dataFlow == dataFlow'

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
      -- can be executed by 'demotedToColdRemote'
      (OutboundDupSt expired, OutboundDupSt expired')
                                        -> expired == expired'
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
      (TerminatedSt, UnnegotiatedSt Inbound) -> True
      (UnknownConnectionSt, UnnegotiatedSt Inbound) -> True
      -- @Overwritten@
      (ReservedOutboundSt, UnnegotiatedSt Inbound) -> True
      -- @Negotiated^{Duplex}_{Inbound}
      (UnnegotiatedSt Inbound, InboundIdleSt Duplex) -> True
      -- @Negotiated^{Unidirectional}_{Inbound}
      (UnnegotiatedSt Inbound, InboundIdleSt Unidirectional) -> True

      -- 'unregisterOutboundConnection' and 'demotedToColdRemote' might perfrom
      (InboundIdleSt Duplex, InboundIdleSt Duplex) -> True
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
      -- OutboundIdleSt
      --

      (OutboundIdleSt Duplex, InboundSt Duplex) -> True
      (OutboundIdleSt _dataFlow, TerminatingSt) -> True

      --
      -- Terminate
      --

      -- @Terminate@
      (TerminatingSt, TerminatedSt) -> True

      -- explicit prohibition of reflexive terminate transition
      (TerminatedSt, TerminatedSt) -> False
      -- implicit terminate transition
      (_, TerminatedSt) -> True

      -- explicit prohibition of reflexive unknown transition
      (UnknownConnectionSt, UnknownConnectionSt) -> False
      (_, UnknownConnectionSt) -> True

      -- We accept connection in 'TerminatingSt'
      (TerminatingSt, UnnegotiatedSt Inbound) -> True

      _ -> False

-- | Maps each valid transition into one number. Collapses all invalid transition into a
-- single number.
--
-- NOTE: Should be in sync with 'verifyAbstractTransition'
--
validTransitionMap :: AbstractTransition
                   -> (Int, String)
validTransitionMap t@Transition { fromState, toState } =
    case (fromState, toState) of
      (TerminatedSt            , ReservedOutboundSt)                -> (01, show t)
      (UnknownConnectionSt     , ReservedOutboundSt)                -> (02, show t)
      (ReservedOutboundSt      , UnnegotiatedSt Outbound)           -> (03, show t)
      (UnnegotiatedSt Outbound , OutboundUniSt)                     -> (04, show t)
      (UnnegotiatedSt Outbound , OutboundDupSt Ticking)             -> (05, show t)
      (OutboundUniSt           , OutboundIdleSt Unidirectional)     -> (06, show t)
      (OutboundDupSt Ticking   , OutboundDupSt Expired)             -> (07, show t)
      (OutboundDupSt Expired   , OutboundIdleSt Duplex)             -> (08, show t)
      (OutboundIdleSt dataFlow , OutboundIdleSt dataFlow')
        | dataFlow == dataFlow'                                     -> (09, show t)
      (OutboundDupSt Ticking   , InboundIdleSt Duplex)              -> (10, show t)
      (InboundIdleSt Duplex    , OutboundDupSt Ticking)             -> (11, show t)
      (OutboundDupSt Ticking   , DuplexSt)                          -> (12, show t)
      (OutboundDupSt Expired   , DuplexSt)                          -> (13, show t)
      (OutboundDupSt expired   , OutboundDupSt expired')
        | expired == expired'                                       -> (14, show t)
      (InboundSt Duplex             , DuplexSt)                     -> (15, show t)
      (DuplexSt                     , OutboundDupSt Ticking)        -> (16, show t)
      (DuplexSt                     , InboundSt Duplex)             -> (17, show t)
      (TerminatedSt                 , UnnegotiatedSt Inbound)       -> (18, show t)
      (UnknownConnectionSt          , UnnegotiatedSt Inbound)       -> (19, show t)
      (ReservedOutboundSt           , UnnegotiatedSt Inbound)       -> (20, show t)
      (UnnegotiatedSt Inbound       , InboundIdleSt Duplex)         -> (21, show t)
      (UnnegotiatedSt Inbound       , InboundIdleSt Unidirectional) -> (22, show t)
      (InboundIdleSt Duplex         , InboundIdleSt Duplex)         -> (23, show t)
      (InboundIdleSt Duplex         , InboundSt Duplex)             -> (24, show t)
      (InboundIdleSt Duplex         , TerminatingSt)                -> (25, show t)
      (InboundSt Duplex             , InboundIdleSt Duplex)         -> (26, show t)
      (InboundIdleSt Unidirectional , InboundSt Unidirectional)     -> (27, show t)
      (InboundIdleSt Unidirectional , TerminatingSt)                -> (28, show t)
      (InboundSt Unidirectional     , InboundIdleSt Unidirectional) -> (29, show t)
      (OutboundIdleSt Duplex        , InboundSt Duplex)             -> (30, show t)
      (OutboundIdleSt _dataFlow , TerminatingSt)                    -> (31, show t)
      (TerminatingSt            , TerminatedSt)                     -> (32, show t)
      (_                        , TerminatedSt)                     -> (33, show t)
      (_                        , UnknownConnectionSt)              -> (34, show t)
      (TerminatingSt            , UnnegotiatedSt Inbound)           -> (35, show t)
      _                                                             -> (99, show t)

-- | List of all valid transition's names.
--
-- NOTE: Should be in sync with 'verifyAbstractTransition', but due to #3516
-- abrupt terminating transitions and identity transitions are trimmed for now,
-- until we tweak the generators to include more connection errors.
--
allValidTransitionsNames :: [String]
allValidTransitionsNames =
  map show
  [ Transition UnknownConnectionSt             ReservedOutboundSt
  -- , Transition TerminatedSt                    ReservedOutboundSt
  , Transition ReservedOutboundSt              (UnnegotiatedSt Outbound)
  , Transition (UnnegotiatedSt Outbound)       OutboundUniSt
  , Transition (UnnegotiatedSt Outbound)       (OutboundDupSt Ticking)
  , Transition OutboundUniSt                   (OutboundIdleSt Unidirectional)
  , Transition (OutboundDupSt Ticking)         (OutboundDupSt Expired)
  -- , Transition (OutboundDupSt Expired)         (OutboundIdleSt Duplex)
  -- , Transition (OutboundIdleSt Unidirectional) (OutboundIdleSt Unidirectional)
  -- , Transition (OutboundIdleSt Duplex)         (OutboundIdleSt Duplex)
  , Transition (OutboundDupSt Ticking)         (InboundIdleSt Duplex)
  , Transition (InboundIdleSt Duplex)          (OutboundDupSt Ticking)
  , Transition (OutboundDupSt Ticking)         DuplexSt
  -- , Transition (OutboundDupSt Expired)         DuplexSt
  -- , Transition (OutboundDupSt Ticking)         (OutboundDupSt Ticking)
  -- , Transition (OutboundDupSt Expired)         (OutboundDupSt Expired)
  , Transition (InboundSt Duplex)              DuplexSt
  , Transition DuplexSt                        (OutboundDupSt Ticking)
  , Transition DuplexSt                        (InboundSt Duplex)
  -- , Transition TerminatedSt                    (UnnegotiatedSt Inbound)
  , Transition UnknownConnectionSt             (UnnegotiatedSt Inbound)
  , Transition ReservedOutboundSt              (UnnegotiatedSt Inbound)
  , Transition (UnnegotiatedSt Inbound)        (InboundIdleSt Duplex)
  , Transition (UnnegotiatedSt Inbound)        (InboundIdleSt Unidirectional)
  -- , Transition (InboundIdleSt Duplex)          (InboundIdleSt Duplex)
  , Transition (InboundIdleSt Duplex)          (InboundSt Duplex)
  -- , Transition (InboundIdleSt Duplex)          TerminatingSt
  -- , Transition (InboundSt Duplex)              (InboundIdleSt Duplex)
  -- , Transition (InboundIdleSt Unidirectional)  (InboundSt Unidirectional)
  -- , Transition (InboundIdleSt Unidirectional)  TerminatingSt
  -- , Transition (InboundSt Unidirectional)      (InboundIdleSt Unidirectional)
  -- , Transition (OutboundIdleSt Duplex)         (InboundSt Duplex)
  -- , Transition (OutboundIdleSt Unidirectional) TerminatingSt
  -- , Transition (OutboundIdleSt Duplex)         TerminatingSt
  , Transition TerminatingSt                   TerminatedSt
  -- , Transition TerminatedSt                    UnknownConnectionSt
  -- , Transition TerminatingSt                   (UnnegotiatedSt Inbound)
  -- , Transition (_)                             (TerminatedSt)
  -- , Transition (_)                             (UnknownConnectionSt)
  ]

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
prop_valid_transitions
    :: SkewedBool
    -- ^ bind to local address or not
    -> RefinedScheduleMap Addr
    -- ^ A list of addresses to which we connect or which connect to us.  We use
    -- 'Blind' since we show the arguments using `counterexample` in a nicer
    -- way.
    -> Property
prop_valid_transitions (SkewedBool bindToLocalAddress) scheduleMap =
    let tr = runSimTrace experiment in
    -- `selectTraceEventsDynamic`, can throw 'Failure', hence we run
    -- `traceResults` first.
    counterexample ("\nSimulation Trace\n" ++ (intercalate "\n" . map show $ traceEvents tr)) $
      case traceResult True tr of
        Left failure ->
          counterexample (displayException failure) False
        Right _ ->
          let cmTrace :: [TestAbstractTransitionTrace]
              cmTrace = selectTraceEventsDynamic tr

          in counterexample ("\nTransition Trace\n" ++ (intercalate "\n" . map show $ cmTrace))
                            (verifyTrace cmTrace)
  where
    myAddress :: Maybe Addr
    myAddress = if bindToLocalAddress
                  then Just (TestAddress 0)
                  else Nothing

    verifyTrace :: [TestAbstractTransitionTrace] -> Property
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
        result <- withConnectionManager
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
              cmTimeWaitTimeout = testTimeWaitTimeout,
              cmOutboundIdleTimeout = testOutboundIdleTimeout
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

                            Right (Just (Connected _ _ _)) -> do
                              threadDelay (either id id (seActiveDelay conn))
                              -- if this outbound connection is not
                              -- executed within inbound connection,
                              -- we need to manually
                              -- 'unregisterInboundConnection'.  We
                              -- need to do that concurrently, since
                              -- 'unregisterOutboundConnection' can
                              -- block.
                              case seActiveDelay conn of
                                Left  _ -> pure ()
                                Right _ -> do
                                  when ( not (siReused (seExtra conn))
                                         && seDataFlow conn == Duplex ) $
                                    -- we need to perform the @TimeoutExpired@
                                    -- transition, in order for
                                    -- 'unregisterOutboundConnection' to be
                                    -- successful.
                                    void $
                                      unregisterInboundConnection
                                        connectionManager addr

                                  res <-
                                      unregisterOutboundConnection
                                        connectionManager addr
                                  case res of
                                    UnsupportedState st ->
                                      throwIO (UnsupportedStateError
                                                "unregisterOutboundConnection"
                                                st)
                                    OperationSuccess     _ -> pure ()
                                    TerminatedConnection _ -> pure ()


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
                                  connectionManager maxBound fd' addr
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

            -- we need to wait at least `testTimeWaitTimeout` to let all the
            -- outstanding threads to terminate
            threadDelay (testTimeWaitTimeout + 1)
            atomically $ numberOfConnections connectionManager

        -- we need to wait at least `testTimeWaitTimeout` to let all the
        -- outstanding threads to terminate.  This can happen in a rare case
        threadDelay (10 * testTimeWaitTimeout)
        return result

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
        -- the test environment can Nequests outbound connection when there is
        -- one in 'PreTerminatingState'
        Just (SomeConnectionManagerError  (ForbiddenOperation _
                                            (OutboundIdleSt _) _))
                                         -> pure ()
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
    prop_valid_transitions
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
    prop_valid_transitions
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
