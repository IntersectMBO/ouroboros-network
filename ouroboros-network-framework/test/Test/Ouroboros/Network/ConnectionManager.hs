{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Test.Ouroboros.Network.ConnectionManager
  ( tests
  ) where

import           Prelude hiding (read)

import           Control.Exception (Exception (..), SomeException)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadSay
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), nullTracer)

import           Data.Bifunctor (bimap)
import           Data.Either (rights)
import           Data.List (intercalate, nub)

import           Network.Mux.Types

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Network.Snocket (Snocket (..), Accept (..), Accepted (..), AddressFamily(TestFamily))
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.Types hiding (ConnectError)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.ConnectionManager"
  [ testProperty "pure connection manager" prop_connectionManager
  ]


data ConnState = UnconnectedState
               | ConnectedState
               | AcceptedState
               | ListeningState
               | ClosedState

data Bound = Bound | NotBound

data FDState = FDState {
    fdLocalAddress    :: Int,
    fdRemoteAddress   :: Maybe Int,
    fdConnectionState :: ConnState,
    fdBound           :: Bound
  }

newtype FD m = FD (StrictTVar m FDState)

-- TODO: use `IOException` instead
data SnocketError =
      InvalidArgument
    | AcceptError
    | ConnectError
    | BindError
    | ListenError
  deriving Show

instance Exception SnocketError


-- | A pure snocket.  Reading always blocks forever, writing is imeediate.
--
-- This very roughly captures socket semantics, but it's good enough for the
-- time being for the testing we want to do.
--
pureSnocket :: forall m.
               ( MonadDelay m
               , MonadMonotonicTime m
               , MonadSTM   m
               , MonadThrow m
               , MonadThrow (STM m)
               )
            => [Int]
            -- list of remote addresses which connect to us
            -> Snocket m (FD m) Int
pureSnocket remoteAddresses =
    Snocket {
      getLocalAddr, getRemoteAddr, addrFamily,
      open, openToConnect,
      connect, listen, accept,
      bind, close, toBearer
    }
  where
    getLocalAddr (FD v) =
      fdLocalAddress <$> atomically (readTVar v)

    getRemoteAddr (FD v) = do
      mbRemote <- fdRemoteAddress <$> atomically (readTVar v)
      case mbRemote of
        Nothing   -> throwM InvalidArgument
        Just addr -> pure addr

    addrFamily _ = TestFamily

    open _ =
      FD <$>
        newTVarM FDState {
            fdLocalAddress = 0,
            fdRemoteAddress = Nothing,
            fdConnectionState = UnconnectedState,
            fdBound = NotBound
          }

    openToConnect _ =
      FD <$>
        newTVarM FDState {
            fdLocalAddress = 0,
            fdRemoteAddress = Nothing,
            fdConnectionState = UnconnectedState,
            fdBound = NotBound
          }

    connect (FD v) remoteAddr =
      atomically $ do
        fds@FDState { fdConnectionState } <- readTVar v
        case fdConnectionState of
          UnconnectedState ->
            writeTVar v fds { fdRemoteAddress = Just remoteAddr
                            , fdConnectionState = ConnectedState }
          _ -> throwM ConnectError

    bind (FD v) localAddr =
      atomically $ do
        fds@FDState { fdBound } <- readTVar v
        case fdBound of
          NotBound -> writeTVar v fds { fdLocalAddress = localAddr
                                      , fdBound = Bound
                                      }
          Bound -> throwM BindError


    accept :: FD m -> Accept m SomeException Int (FD m)
    accept (FD v) = Accept $ go remoteAddresses
      where
        go [] = pure (AcceptException (toException AcceptError), Accept $ go [])
        go (x : xs) = do
          v' <- atomically $ do
            FDState { fdLocalAddress = localAddr } <- readTVar v
            newTVar FDState {
                        -- this is not adequate
                        fdLocalAddress = localAddr,
                        fdRemoteAddress = Just x,
                        fdConnectionState = AcceptedState,
                        fdBound = Bound
                      }
          pure (Accepted (FD v') x, Accept $ go xs)


    toBearer _ _ _ =
      MuxBearer {
          write   = \_ _ -> getMonotonicTime,
          read    = \_ -> forever (threadDelay 3600),
          sduSize = 1500
        }

    listen (FD v) = atomically $ do
      fds@FDState{ fdConnectionState } <- readTVar v
      case fdConnectionState of
        UnconnectedState ->
          writeTVar v (fds { fdConnectionState = ListeningState })
        _ -> throwM ListenError

    close (FD v) =
      atomically $ modifyTVar v (\fds -> fds { fdConnectionState = ClosedState })



-- | A connection handler which does not do any effects, other than blocking idefinitely.
--
pureConnectionHandler :: MonadTimer m
                      => ConnectionHandler InitiatorResponderMode handlerTrace Int muxPromise m
pureConnectionHandler =
    ConnectionHandler $
      WithInitiatorResponderMode
        (\_ _ _ _ -> Action (forever (threadDelay 86400)) id)
        (\_ _ _ _ -> Action (forever (threadDelay 86400)) id)


-- | This property interleaves inbound and outbound connections and then
-- verifies that:
--
-- * all threads forked by the connection manager are killed when the callback
--   exists
-- * the number of connections managed by the connection manager is right
--   (taking bidirectional connections into account) .
--
prop_connectionManager
    :: Maybe (Negative Int)
    -- local address, by using a nagative integer we force it to be
    -- different from any one from the list of remote addresses.
    -> NonEmptyList (Either (Positive Int) (Positive Int))
    -- a list of addresses to which we connect or which connect to us.
    -> Property
prop_connectionManager localAddress (NonEmpty remoteAddresses0) =
    let tr = runSimTrace experiment
    in case traceResult True tr of
        Left err ->
          -- verbose trace output
          counterexample
            (show err ++ "\n" ++ intercalate "\n" (show `map` traceEvents tr))
            False
        Right p  -> p
  where
    experiment :: forall s. SimM s Property
    experiment =
        withConnectionManager
          ConnectionManagerArguments {
              connectionManagerTracer = sayTracer :: Tracer (SimM s) (ConnectionManagerTrace Int ()) ,
              connectionManagerMuxTracer = nullTracer,
              connectionManagerIPv4Address = getNegative <$> localAddress,
              connectionManagerIPv6Address = Nothing,
              connectionManagerAddressType = const Nothing,
              connectionHandler = pureConnectionHandler,
              connectionSnocket
            }
          $ \connectionManager -> do
            fd <- open connectionSnocket TestFamily
            case localAddress of
              Just (Negative localAddr) ->
                bind connectionSnocket fd localAddr
              Nothing ->
                pure ()

            let go :: Accept (SimM s) SomeException Int (FD (SimM s))
                   -> [Either Int Int]
                   -> SimM s ()
                go _acceptOne [] = pure ()
                go acceptOne (Left x : xs) = do
                  _ <- includeOutboundConnection connectionManager x
                  go acceptOne xs
                go (Accept acceptOne) (Right x : xs) = do
                  r <- acceptOne
                  case r of
                    (Accepted fd' _, acceptNext) -> do
                      _ <- includeInboundConnection connectionManager fd' x
                      go acceptNext xs
                    (AcceptException err, _acceptNext) ->
                      throwM err
            go (accept connectionSnocket fd) remoteAddresses

            n <- atomically $ numberOfConnections connectionManager
            let expectedNumberOfConnections = length $ nub $ map (either id id) remoteAddresses
            pure $ n === expectedNumberOfConnections

    remoteAddresses :: [Either Int Int]
    remoteAddresses = bimap getPositive getPositive `map` remoteAddresses0

    connectionSnocket :: forall s. Snocket (SimM s) (FD (SimM s)) Int
    connectionSnocket = pureSnocket (rights remoteAddresses)


--
-- Utils
--

sayTracer :: forall m a. (MonadSay m, Show a) => Tracer m a
sayTracer = Tracer $ say . show
