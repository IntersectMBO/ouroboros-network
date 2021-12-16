{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.PeerState (tests) where


import           Control.Exception (ArithException (..), AsyncException (..),
                     NonTermination (..))
import           Data.Functor (void)
import qualified Data.Map.Strict as Map
import           Data.Monoid (First (..))
import qualified Data.Set as Set
import           Text.Printf

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer

import           Data.Semigroup.Action
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Server.ConnectionTable
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.PeerState
import           Ouroboros.Network.Subscription.Worker

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Subscription.PeerState"
  [ testProperty "SuspendDecision semigroup" (prop_SuspendDecisionSemigroup @Int)
  , testProperty "Suspend semigroup action on PeerState (up to constructor)"
      (prop_SuspendDecisionAction @IO)
  , testProperty "worker error handling" prop_subscriptionWorker
  ]


--
-- Generators of 'SuspendDecision' and 'PeerState'
--

newtype ArbSuspendDecision t = ArbSuspendDecision {
      getArbSuspendDecision :: SuspendDecision t
    }
  deriving (Eq, Show)

genSuspendDecision :: Gen t
                   -> Gen (SuspendDecision t)
genSuspendDecision gen = oneof
    [ SuspendPeer <$> gen <*> gen
    , SuspendConsumer <$> gen
    , pure Throw
    ]

genDiffTime :: Gen DiffTime
genDiffTime = fromIntegral @Int <$> arbitrary

instance Arbitrary t => Arbitrary (ArbSuspendDecision t) where
    arbitrary = ArbSuspendDecision <$> genSuspendDecision arbitrary

-- | Subsemigroup formed by 'SuspendPeer' and 'SuspendDecision'.
--
newtype SuspendSubsemigroup t = SuspendSubsemigroup {
      getSuspendSubsemigroup :: SuspendDecision t
    }
  deriving (Eq, Show)

instance Arbitrary t => Arbitrary (SuspendSubsemigroup t) where
    arbitrary = oneof
      [ SuspendSubsemigroup <$> (SuspendPeer <$> arbitrary <*> arbitrary)
      , SuspendSubsemigroup . SuspendConsumer <$> arbitrary
      ]

newtype ArbPeerState m = ArbPeerState {
      getArbPeerState :: PeerState m
    }

instance ( Ord (ThreadId m)
         , Show (ThreadId m)
         , MonadAsync m
         ) => Show (ArbPeerState m) where
    show (ArbPeerState p) = "ArbPeerState " ++ show p

-- TODO: it only generates times, not ThreadId's.
instance Arbitrary (ArbPeerState m) where
    arbitrary = oneof
      [ pure $ ArbPeerState (HotPeer Set.empty Set.empty)
      , ArbPeerState . SuspendedConsumer Set.empty . getArbTime <$> arbitrary
      , ArbPeerState <$> (SuspendedPeer <$> (getArbTime <$> arbitrary)
                                        <*> (getArbTime <$> arbitrary))
      , pure (ArbPeerState ColdPeer)
      ]

--
-- Algebraic properties of 'SuspendDecision' and 'PeerState'
--

prop_SuspendDecisionSemigroup
    :: ( Ord t
       , Eq t
       )
    => ArbSuspendDecision t
    -> ArbSuspendDecision t
    -> ArbSuspendDecision t
    -> Bool
prop_SuspendDecisionSemigroup (ArbSuspendDecision a1)
                             (ArbSuspendDecision a2)
                             (ArbSuspendDecision a3) =
    a1 <> (a2 <> a3) == (a1 <> a2) <> a3

prop_SuspendDecisionAction
    :: forall m.
       Eq (Async m ())
    => Blind (Maybe (ArbPeerState m))
    -> ArbSuspendDecision ArbTime
    -> ArbSuspendDecision ArbTime
    -> Bool
prop_SuspendDecisionAction
      (Blind mps)
      (ArbSuspendDecision a1)
      (ArbSuspendDecision a2) =
    mps' <| (sd1 <> sd2) == (mps' <| sd1 <| sd2)
  where
    sd1 = getArbTime <$> a1
    sd2 = getArbTime <$> a2
    mps' :: Maybe (PeerState m)
    mps' = getArbPeerState <$> mps

-- | Like 'ArbPeerState' but does not generate  'HotPeer' with empty producer
-- and consumer sets.
--
newtype ArbValidPeerState m = ArbValidPeerState (PeerState m)

-- TODO
instance Show (ArbValidPeerState t) where
    show (ArbValidPeerState _) = "ArbValidPeerState"

instance Arbitrary (ArbValidPeerState m) where
    arbitrary = oneof
      [ ArbValidPeerState . SuspendedConsumer Set.empty . getArbTime <$> arbitrary
      , ArbValidPeerState <$> (SuspendedPeer <$> (getArbTime <$> arbitrary)
                                             <*> (getArbTime <$> arbitrary))
      , pure (ArbValidPeerState ColdPeer)
      ]

data ArbException where
     ArbException
      :: Exception err
      => err
      -> ArbException

instance Show ArbException where
    show (ArbException err) = "ArbException " ++ show err

data TestException1 = TestException1
  deriving Show

instance Exception TestException1

data TestException2 = TestException2
  deriving Show

instance Exception TestException2

instance Arbitrary ArbException where
    arbitrary = oneof
      [ pure (ArbException TestException1)
      , pure (ArbException TestException2)
      -- AsyncException
      -- , pure (ArbException StackOverflow)
      -- , pure (ArbException HeapOverflow)
      -- NonTermination
      -- , pure (ArbException NonTermination)
      ]

data ArbErrorPolicies = ArbErrorPolicies [ErrorPolicy] -- application error policy
                                         [ErrorPolicy] -- connection error policy
  deriving Show


genErrorPolicy :: Gen (SuspendDecision DiffTime)
               -> Gen (ErrorPolicy)
genErrorPolicy genCmd = oneof
    [ (\cmd -> ErrorPolicy (\(_e :: ArithException) -> Just cmd)) <$> genCmd,
      (\cmd -> ErrorPolicy (\(_e :: AsyncException) -> Just cmd)) <$> genCmd,
      (\cmd -> ErrorPolicy (\(_e :: NonTermination) -> Just cmd)) <$> genCmd
    ]

instance Arbitrary ArbErrorPolicies where
    arbitrary = ArbErrorPolicies <$> listOf genPolicy <*> listOf genPolicy
      where
        genPolicy = genErrorPolicy (genSuspendDecision genDiffTime)

    shrink (ArbErrorPolicies aps cps) =
        let aps' = shrinkList (const []) aps
            cps' = shrinkList (const []) cps in
        map (\(a,c) -> ArbErrorPolicies a c) $ zip aps' cps'

data Sock addr = Sock {
    remoteAddr :: addr
  , localAddr  :: addr
  }

data SnocketType where

     -- socket which allocates and connects with out an error, any error can
     -- only come from an application
     WorkingSnocket :: SnocketType

     -- socket which errors when allocating a socket
     AllocateError :: forall e. Exception e
                   => e
                   -> SnocketType

     -- socket which errors when attempting a connection
     ConnectError :: forall e. Exception e
                  => e
                  -> SnocketType

instance Show SnocketType where
    show (AllocateError e) = "AllocateError " ++show e
    show (ConnectError e)  = "ConnectError " ++show e
    show WorkingSnocket    = "WorkingSnocket"

instance Arbitrary SnocketType where
    arbitrary = oneof
      -- we are not generating 'AllocateErrors', they will not kill the worker,
      -- but only the connection thread.
      [ (\(ArbException e) -> ConnectError e) <$> arbitrary
      , pure WorkingSnocket
      ]

-- | 'addrFamily', 'accept' and 'toBearer' are not needed to run the test suite.
--
mkSnocket :: MonadThrow m
          => SnocketType
          -> addr
          -> addr
          -> Snocket m (Sock addr) addr
mkSnocket (AllocateError e) _localAddr _remoteAddr = Snocket {
    getLocalAddr = \Sock{localAddr} -> pure localAddr
  , getRemoteAddr = \Sock{remoteAddr = addr} -> pure addr
  , addrFamily = error "not supported"
  , open = \_ -> throwIO e
  , openToConnect = \_  -> throwIO e
  , connect = \_ _ -> pure ()
  , bind = \_ _ -> pure ()
  , listen = \_ -> pure ()
  , accept = \_ -> error "not supported"
  , close = \_ -> pure ()
  , toBearer = \_ _ -> error "not supported"
  }
mkSnocket (ConnectError e) localAddr remoteAddr = Snocket {
    getLocalAddr = \Sock{localAddr = addr} -> pure addr
  , getRemoteAddr = \Sock{remoteAddr = addr} -> pure addr
  , addrFamily = error "not supported"
  , open = \_ -> pure Sock {remoteAddr, localAddr}
  , openToConnect = \_ -> pure Sock {remoteAddr, localAddr}
  , connect = \_ _ -> throwIO e
  , accept = \_ -> error "not supported"
  , bind = \_ _ -> pure ()
  , listen = \_ -> pure ()
  , close = \_ -> pure ()
  , toBearer = \_ _ -> error "not supported"
  }
mkSnocket WorkingSnocket localAddr remoteAddr = Snocket {
    getLocalAddr = \Sock{localAddr = addr} -> pure addr
  , getRemoteAddr = \Sock{remoteAddr = addr} -> pure addr
  , addrFamily = error "not supported"
  , open = \_ -> pure Sock {remoteAddr, localAddr}
  , openToConnect = \_ -> pure Sock {remoteAddr, localAddr}
  , connect = \_ _ -> pure ()
  , bind = \_ _ -> pure ()
  , listen = \_ -> pure ()
  , accept = \_ -> error "not supported"
  , close = \_ -> pure ()
  , toBearer = \_ _ -> error "not supported"
  }

data ArbApp addr = ArbApp (Maybe ArbException) (Sock addr -> IO ())

instance Arbitrary (ArbApp addr) where
    arbitrary = oneof
      [ (\a@(ArbException e) -> ArbApp (Just a) (\_ -> throwIO e)) <$> arbitrary
      , pure $ ArbApp Nothing (\_ -> pure ())
      ]

newtype ArbDiffTime = ArbDiffTime {
    getArbDiffTime :: DiffTime
  }
  deriving Show
  deriving Eq
  deriving Ord
  deriving Num        via DiffTime
  deriving Fractional via DiffTime
  deriving Real       via DiffTime
  deriving RealFrac   via DiffTime

instance Arbitrary ArbDiffTime where
    arbitrary = ArbDiffTime . fromIntegral @Int <$> arbitrary

instance CoArbitrary ArbDiffTime where
    coarbitrary (ArbDiffTime t) = coarbitrary (toRational t)

instance Function ArbDiffTime where
    function = functionRealFrac

newtype ArbTime = ArbTime { getArbTime :: Time }
  deriving Show
  deriving Eq
  deriving Ord
  deriving Num        via DiffTime
  deriving Fractional via DiffTime
  deriving Real       via DiffTime
  deriving RealFrac   via DiffTime

instance Arbitrary ArbTime where
    arbitrary = ArbTime . Time . getArbDiffTime <$> arbitrary

instance CoArbitrary ArbTime where
    coarbitrary (ArbTime (Time t)) = coarbitrary (toRational t)

instance Function ArbTime where
    function = functionRealFrac

prop_subscriptionWorker
    :: SnocketType
    -> Int -- local address
    -> Int -- remote address
    -> ArbValidPeerState IO
    -> ArbErrorPolicies
    -> (Blind (ArbApp Int))
    -> Property
prop_subscriptionWorker
    sockType localAddr remoteAddr (ArbValidPeerState ps)
    (ArbErrorPolicies appErrPolicies conErrPolicies)
    (Blind (ArbApp merr app))
  =
    tabulate "peer states & app errors" [printf "%-20s %s" (peerStateType ps) (exceptionType merr)] $
    ioProperty $ do
      doneVar :: StrictTMVar IO () <- newEmptyTMVarIO
      tbl <- newConnectionTable
      peerStatesVar <- newPeerStatesVar
      worker nullTracer
             nullTracer
             tbl
             peerStatesVar
             (mkSnocket sockType localAddr remoteAddr)
             WorkerCallbacks {
                 wcSocketStateChangeTx = \ss s -> do
                   s' <- socketStateChangeTx ss s
                   case ss of
                     CreatedSocket{} -> pure s'
                     ClosedSocket{}  -> tryPutTMVar doneVar () >> pure s',
                 wcCompleteApplicationTx = completeTx,
                 wcMainTx = main doneVar
               }
             WorkerParams {
                 wpLocalAddresses = LocalAddresses {
                     laIpv4 = Just localAddr,
                     laIpv6 = Just localAddr,
                     laUnix = Nothing
                   },
                 wpSelectAddress = \_ LocalAddresses {laIpv4, laIpv6} -> getFirst (First laIpv4 <> First laIpv6),
                 wpConnectionAttemptDelay = const Nothing,
                 wpSubscriptionTarget =
                   pure $ ipSubscriptionTarget nullTracer peerStatesVar [remoteAddr],
                 wpValency = 1
               }
             (\sock -> app sock
                `finally`
                (void $ atomically $ tryPutTMVar doneVar ()))
  where
    completeTx = completeApplicationTx
       (ErrorPolicies
          appErrPolicies
          conErrPolicies)

    main :: StrictTMVar IO () -> Main IO (PeerStates IO Int) Bool
    main doneVar s = do
      done <- maybe False (const True) <$> tryReadTMVar doneVar
      let r = case sockType of
            WorkingSnocket   -> case merr of
              -- TODO: we don't have access to the time when the transition was
              -- evaluated.
              Nothing -> True
              Just (ArbException e) -> transitionSpec remoteAddr ps
                                                      (evalErrorPolicies e appErrPolicies)
                                                      s
            AllocateError _ -> True
            ConnectError e  -> transitionSpec remoteAddr ps
                                              (evalErrorPolicies e conErrPolicies)
                                              s
      if done
        then pure r
        else if r then retry else pure r

    --
    -- tabulating QuickCheck's cases
    --

    peerStateType HotPeer{}           = "HotPeer"
    peerStateType SuspendedConsumer{} = "SuspendedConsumer"
    peerStateType SuspendedPeer{}     = "SuspendedPeer"
    peerStateType ColdPeer{}          = "ColdPeer"

    exceptionType Nothing  = "no-exception"
    exceptionType (Just _) = "with-exception"

-- transition spec from a given state to a target states
transitionSpec :: Ord addr
               => addr
               -> PeerState IO
               -> Maybe (SuspendDecision DiffTime)
               -> PeerStates IO addr
               -> Bool

transitionSpec _addr _ps0 Nothing ThrowException{} = False

transitionSpec addr ps0 Nothing (PeerStates peerStates) =
    case Map.lookup addr peerStates of
      Nothing -> True
      Just ps1 -> case (ps0, ps1) of
        (ColdPeer, ColdPeer)
          -> True
        (ColdPeer, HotPeer producers consumers)
          -> not (Set.null producers) || not (Set.null consumers)
        (ColdPeer, _)
          -> False

        -- this transition can happen only if 'producers' are empty
        (SuspendedConsumer producers _consT, ColdPeer)
          | Set.null producers
          -> True
          | otherwise
          -> False
        (SuspendedConsumer _ consT, SuspendedConsumer _ consT')
          -> consT == consT'
        (SuspendedConsumer _ _consT, HotPeer _ consumers)
          -> not $ Set.null consumers
        (SuspendedConsumer _ consT, SuspendedPeer _ consT')
          -> consT' >= consT

        (SuspendedPeer{}, HotPeer producers consumers)
          | Set.null producers && Set.null consumers
          -> False
          | otherwise
          -> True
        (SuspendedPeer{}, _)
          -> True

        (HotPeer producers consumers, ColdPeer)
          | Set.null consumers && Set.null producers
          -> True
          | otherwise
          -> False
        (HotPeer{}, HotPeer producers consumers)
          | Set.null producers && Set.null consumers
          -> False
          | otherwise
          -> True
        (HotPeer{}, SuspendedConsumer{})
          -> True
        (HotPeer{}, SuspendedPeer{})
          -> True

transitionSpec _addr _ps0 (Just Throw) ThrowException{} = True
transitionSpec _addr _ps0 (Just _)     ThrowException{} = False

transitionSpec addr ps0 (Just cmd) (PeerStates peerStates) =
    case Map.lookup addr peerStates of
      Nothing -> True
      Just ps1 -> case (cmd, ps1) of
        (SuspendPeer{}, SuspendedPeer{})
          -> True
        (SuspendPeer{}, _)
          -> False
        (SuspendConsumer{}, SuspendedConsumer producers _)
          -> getProducers ps0 == producers
        (SuspendConsumer{}, SuspendedPeer{})
          -> True
        (SuspendConsumer{}, _)
          -> False
        (Throw, _)
          -> True
  where
    getProducers :: PeerState IO -> Set.Set (Async IO ())
    getProducers (HotPeer producers _)           = producers
    getProducers (SuspendedConsumer producers _) = producers
    getProducers _                               = Set.empty
