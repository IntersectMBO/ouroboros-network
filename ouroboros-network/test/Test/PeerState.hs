{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.PeerState (tests) where


import           Control.Exception ( ArithException (..)
                                   , AsyncException (..)
                                   , NonTermination (..)
                                   )
import qualified Data.Set as Set
import           Data.Time.Clock (secondsToDiffTime)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow

import           Data.Semigroup.Action
import           Ouroboros.Network.Subscription.PeerState

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Subscription.PeerState"
  [ testProperty "SuspendCommand semigroup" (prop_SuspendCommandSemigroup @Int)
  , testProperty "Suspend semigroup action on PeerState (up to constructor)"
      (prop_SuspendCommandAction @IO @Int)
  ]


--
-- Generators of 'SuspendCommand' and 'PeerState'
--

newtype ArbitrarySuspendCommand t = ArbitrarySuspendCommand {
      getArbitrarySuspendCommand :: SuspendCommand t
    }
  deriving (Eq, Show)

genSuspendCommand :: Gen t
                  -> Gen (SuspendCommand t)
genSuspendCommand gen = oneof
    [ SuspendPeer <$> gen <*> gen
    , SuspendConsumer <$> gen
    , pure Throw
    ]

genDiffTime :: Gen DiffTime
genDiffTime = secondsToDiffTime <$> arbitrary

instance Arbitrary t => Arbitrary (ArbitrarySuspendCommand t) where
    arbitrary = ArbitrarySuspendCommand <$> genSuspendCommand arbitrary

-- | Subsemigroup formed by 'SuspendPeer' and 'SuspendCommand'.
--
newtype SuspendSubsemigroup t = SuspendSubsemigroup {
      getSuspendSubsemigroup :: SuspendCommand t
    }
  deriving (Eq, Show)

instance Arbitrary t => Arbitrary (SuspendSubsemigroup t) where
    arbitrary = oneof
      [ SuspendSubsemigroup <$> (SuspendPeer <$> arbitrary <*> arbitrary)
      , SuspendSubsemigroup . SuspendConsumer <$> arbitrary
      ]

newtype ArbitraryPeerState m t = ArbitraryPeerState {
      getArbitraryPeerState :: PeerState m t
    }

-- TODO: it only generates times, not ThreadId's.
instance Arbitrary t => Arbitrary (ArbitraryPeerState m t) where
    arbitrary = oneof
      [ pure $ ArbitraryPeerState $ HotPeer Set.empty Set.empty
      , ArbitraryPeerState . SuspendedConsumer Set.empty <$> arbitrary
      , ArbitraryPeerState <$> (SuspendedPeer <$> arbitrary <*> arbitrary)
      , pure (ArbitraryPeerState ColdPeer)
      ]

--
-- Algebraic properties of 'SuspendCommand' and 'PeerState'
--

prop_SuspendCommandSemigroup
    :: ( Ord t
       , Eq t
       )
    => ArbitrarySuspendCommand t
    -> ArbitrarySuspendCommand t
    -> ArbitrarySuspendCommand t
    -> Bool
prop_SuspendCommandSemigroup (ArbitrarySuspendCommand a1)
                             (ArbitrarySuspendCommand a2)
                             (ArbitrarySuspendCommand a3) =
    a1 <> (a2 <> a3) == (a1 <> a2) <> a3

prop_SuspendCommandAction
    :: forall m t.
      ( Eq   t
      , Ord  t
      , Show t
      , Eq (Async m ())
      )
    => Blind (Maybe (ArbitraryPeerState m t))
    -> ArbitrarySuspendCommand t
    -> ArbitrarySuspendCommand t
    -> Bool
prop_SuspendCommandAction
      (Blind mps)
      (ArbitrarySuspendCommand a1)
      (ArbitrarySuspendCommand a2) =
    mps' <| (a1 <> a2) == (mps' <| a1) <| a2
  where
    mps' :: Maybe (PeerState m t)
    mps' = getArbitraryPeerState <$> mps


data ArbitraryException where
     ArbitraryException
      :: Exception err
      => err
      -> ArbitraryException

instance Show ArbitraryException where
    show (ArbitraryException err) = "ArbitraryException " ++ show err

instance Arbitrary (ArbitraryException) where
    arbitrary = oneof
      -- ArithException
      [ pure (ArbitraryException Overflow)
      , pure (ArbitraryException Underflow)
      -- AsyncException
      , pure (ArbitraryException StackOverflow)
      , pure (ArbitraryException HeapOverflow)
      -- NonTermination
      , pure (ArbitraryException NonTermination)
      ]

newtype ArbitraryErrorPolicies = ArbitraryErrorPolicies [ErrorPolicy]
  deriving Show


genErrorPolicy :: Gen (SuspendCommand DiffTime)
               -> Gen ErrorPolicy
genErrorPolicy genCmd = oneof
    [ (\cmd -> ErrorPolicy (\(_e :: ConnOrAppException ArithException) -> cmd)) <$> genCmd,
      (\cmd -> ErrorPolicy (\(_e :: ConnOrAppException AsyncException) -> cmd)) <$> genCmd,
      (\cmd -> ErrorPolicy (\(_e :: ConnOrAppException NonTermination) -> cmd)) <$> genCmd
    ]

instance Arbitrary ArbitraryErrorPolicies where
    arbitrary = ArbitraryErrorPolicies <$> listOf genPolicy
      where
        genPolicy = genErrorPolicy (genSuspendCommand genDiffTime)

    shrink (ArbitraryErrorPolicies ps) = map ArbitraryErrorPolicies $ shrinkList (const []) ps
