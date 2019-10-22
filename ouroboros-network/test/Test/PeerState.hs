{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.PeerState (tests) where


import           Control.Exception ( ArithException (..)
                                   , AsyncException (..)
                                   , NonTermination (..)
                                   )
import           Data.Functor (void)
import           Data.Monoid (First (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Time.Clock (secondsToDiffTime)
import           Text.Printf

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadThrow
import           Control.Tracer

import           Data.Semigroup.Action
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Server.ConnectionTable
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
genDiffTime = secondsToDiffTime <$> arbitrary

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
    [ (\cmd -> ErrorPolicy (\(_e :: ArithException) -> cmd)) <$> genCmd,
      (\cmd -> ErrorPolicy (\(_e :: AsyncException) -> cmd)) <$> genCmd,
      (\cmd -> ErrorPolicy (\(_e :: NonTermination) -> cmd)) <$> genCmd
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

data SocketType where

     -- socket which allocates and connects with out an error, any error can
     -- only come from an application
     WorkingSocket :: SocketType

     -- socket which errors when allocating a socket
     AllocateError :: forall e. Exception e
                   => e
                   -> SocketType

     -- socket which errors when attempting a connection
     ConnectError :: forall e. Exception e
                  => e
                  -> SocketType

instance Show SocketType where
    show (AllocateError e) = "AllocateError " ++show e
    show (ConnectError e) = "ConnectError " ++show e
    show WorkingSocket = "WorkingSocket"

instance Arbitrary SocketType where
    arbitrary = oneof
      -- we are not generating 'AllocateErrors', they will not kill the worker,
      -- but only the connection thread.
      [ (\(ArbException e) -> ConnectError e) <$> arbitrary
      , pure WorkingSocket
      ]

mkSocket :: MonadThrow m
          => SocketType
          -> addr
          -> Socket m addr (Sock addr)
mkSocket (AllocateError e) _remoteAddr = Socket {
    allocate = \_ -> throwM e
  , connect = \_ _ _ -> pure ()
  , close = \_ -> pure ()
  , getSocketName = \Sock{localAddr} -> pure localAddr
  , getPeerName = \Sock{remoteAddr = addr} -> pure addr
  }
mkSocket (ConnectError e) remoteAddr = Socket {
    allocate = \localAddr -> pure Sock {remoteAddr, localAddr}
  , connect = \_ _ _ -> throwM e
  , close = \_ -> pure ()
  , getSocketName = \Sock{localAddr} -> pure localAddr
  , getPeerName = \Sock{remoteAddr = addr} -> pure addr
  }
mkSocket WorkingSocket remoteAddr = Socket {
    allocate = \localAddr -> pure Sock {remoteAddr, localAddr}
  , connect = \_ _ _ -> pure ()
  , close = \_ -> pure ()
  , getSocketName = \Sock{localAddr} -> pure localAddr
  , getPeerName = \Sock{remoteAddr = addr} -> pure addr
  }

data ArbApp addr = ArbApp (Maybe ArbException) (Sock addr -> IO ())

instance Arbitrary (ArbApp addr) where
    arbitrary = oneof
      [ (\a@(ArbException e) -> ArbApp (Just a) (\_ -> throwM e)) <$> arbitrary
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
    :: SocketType
    -> Int -- local address
    -> Int -- remote address
    -> ArbValidPeerState IO
    -> (Fun (ArbTime, Int, ()) (ArbSuspendDecision ArbDiffTime))
    -> ArbErrorPolicies
    -> (Blind (ArbApp Int))
    -> Property
prop_subscriptionWorker
    sockType localAddr remoteAddr (ArbValidPeerState ps)
    returnCallback (ArbErrorPolicies appErrPolicies conErrPolicies)
    (Blind (ArbApp merr app))
  =
    tabulate "peer states & app errors" [printf "%-20s %s" (peerStateType ps) (exceptionType merr)] $
    ioProperty $ do
      doneVar :: StrictTMVar IO () <- newEmptyTMVarM
      tbl <- newConnectionTable
      peerStatesVar <- newPeerStatesVar
      worker nullTracer
             tbl
             peerStatesVar
             (mkSocket sockType remoteAddr)
             (\ss s -> do
              s' <- socketStateChangeTx ss s
              case ss of
                CreatedSocket{} -> pure s'
                ClosedSocket{}  -> tryPutTMVar doneVar () >> pure s')
             completeTx
             (main doneVar)
             (LocalAddresses {
                laIpv4 = Just localAddr,
                laIpv6 = Just localAddr,
                laUnix = Nothing
                })
                (\_ LocalAddresses {laIpv4, laIpv6} -> getFirst (First laIpv4 <> First laIpv6))
             (const Nothing)
             (pure $ ipSubscriptionTarget nullTracer peerStatesVar [remoteAddr])
             1
             (\sock -> app sock
                `finally`
                (void $ atomically $ tryPutTMVar doneVar ()))
  where
    completeTx = completeApplicationTx
       nullTracer
       (ErrorPolicies
          appErrPolicies
          conErrPolicies
          (\t addr r -> fmap getArbDiffTime . getArbSuspendDecision $ case returnCallback of
              Fn3 f -> f (ArbTime t) addr r
              _     -> error "impossible happend"))

    main :: StrictTMVar IO () -> Main IO (PeerStates IO Int) Bool
    main doneVar s = do
      done <- maybe False (const True) <$> tryReadTMVar doneVar
      let r = case sockType of
            WorkingSocket   -> case merr of
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
    getProducers (HotPeer producers _) = producers
    getProducers (SuspendedConsumer producers _) = producers
    getProducers _ = Set.empty
