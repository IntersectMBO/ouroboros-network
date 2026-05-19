{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Network.Protocol.ChainSync.Codec.TimeLimits.Test where

import Control.Monad.Class.MonadTime.SI (DiffTime)
import System.Random (mkStdGen)

import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Cardano.Network.Protocol.ChainSync.Codec.TimeLimits (timeLimitsChainSync)

import Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncIdleTimeout (..),
           maxChainSyncTimeout, minChainSyncTimeout)
import Ouroboros.Network.Protocol.ChainSync.Type (SingChainSync (..),
           SingNextKind (..))
import Ouroboros.Network.Protocol.Limits (ProtocolTimeLimitsWithRnd (..),
           shortWait)

import Test.QuickCheck (Arbitrary (..), Property, elements, oneof, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, (===))


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.ChainSync.Timelimits"
    [ testProperty "timeout in 'StIntersect'"
        prop_short_wait_timeout_in_intersect
    , testProperty "timeout in 'StNext' 'StCanWait'"
        prop_short_wait_timeout_in_canwait
    , testProperty "timeout in 'StIdle' state"
        prop_timeout_in_idle
    , testProperty "timeout range for non-trustable peers in 'StNext' 'StMustReply' state"
        prop_timeout_range_for_not_trustable_in_mustreply
    , testProperty "no timeout for trustable peers in 'StNext' 'StMustReply' state"
        prop_no_timeout_for_trustable_peers_in_mustreply
    ]

-- | For state 'StIntersect', the timeout is always 'shortWait'
prop_short_wait_timeout_in_intersect
  :: PeerTrustable -> ChainSyncIdleTimeout -> Int -> Property
prop_short_wait_timeout_in_intersect peerTrustable idleTimeout seed =
  timeout === shortWait
  where
    limits = timeLimitsChainSync idleTimeout peerTrustable
    (timeout, _) =
      timeLimitForStateWithRnd limits SingIntersect (mkStdGen seed)

-- | For state 'StNext' 'StCanAwait', the timeout is always 'shortWait'
prop_short_wait_timeout_in_canwait
  :: PeerTrustable -> ChainSyncIdleTimeout -> Int -> Property
prop_short_wait_timeout_in_canwait peerTrustable idleTimeout seed =
  timeout === shortWait
  where
    limits = timeLimitsChainSync idleTimeout peerTrustable
    (timeout, _) =
      timeLimitForStateWithRnd limits (SingNext SingCanAwait) (mkStdGen seed)

-- | For state 'StIdle', the timeout is 'ChainSyncIdleTimeout'
prop_timeout_in_idle
  :: PeerTrustable -> ChainSyncIdleTimeout -> Int -> Property
prop_timeout_in_idle peerTrustable idleTimeout seed =
  timeout === timeout'
  where
    timeout' = case idleTimeout of
      ChainSyncNoIdleTimeout -> Nothing
      ChainSyncIdleTimeout t -> Just t
    limits = timeLimitsChainSync idleTimeout peerTrustable
    (timeout, _) = timeLimitForStateWithRnd limits SingIdle (mkStdGen seed)

-- | For non-trustable peers and in 'StNext' 'StMustReply' state, the timeout is
-- always within the specified range
prop_timeout_range_for_not_trustable_in_mustreply
  :: ChainSyncIdleTimeout -> Int -> Property
prop_timeout_range_for_not_trustable_in_mustreply idleTimeout seed =
  property $
    maybe
      False
      (\t -> t >= minChainSyncTimeout && t <= maxChainSyncTimeout)
      timeout
  where
    limits = timeLimitsChainSync idleTimeout IsNotTrustable
    (timeout, _) =
      timeLimitForStateWithRnd limits (SingNext SingMustReply) (mkStdGen seed)

-- | For trustable peers, there's never a timeout in 'StNext' 'StMustReply'
-- state
prop_no_timeout_for_trustable_peers_in_mustreply
  :: ChainSyncIdleTimeout -> Int -> Property
prop_no_timeout_for_trustable_peers_in_mustreply idleTimeout seed =
  timeout === Nothing
  where
    limits = timeLimitsChainSync idleTimeout IsTrustable
    (timeout, _) =
      timeLimitForStateWithRnd limits (SingNext SingMustReply) (mkStdGen seed)


instance Arbitrary PeerTrustable where
  arbitrary = elements [IsTrustable, IsNotTrustable]

instance Arbitrary ChainSyncIdleTimeout where
  arbitrary = oneof
    [ pure ChainSyncNoIdleTimeout
    , ChainSyncIdleTimeout <$> arbitrary
    ]

instance Arbitrary DiffTime where
  arbitrary = fromRational <$> arbitrary

instance Show ChainSyncIdleTimeout where
  show ChainSyncNoIdleTimeout   = "ChainSyncNoIdleTimeout"
  show (ChainSyncIdleTimeout t) = "ChainSyncIdleTimeout " ++ show t
