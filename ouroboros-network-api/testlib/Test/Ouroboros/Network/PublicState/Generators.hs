{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PublicState.Generators where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ConnectionManager.Public
import Ouroboros.Network.PublicState

import Test.QuickCheck


instance Arbitrary Provenance where
  arbitrary = elements [Inbound, Outbound]

instance Arbitrary DataFlow where
  arbitrary = elements [Unidirectional, Duplex]

instance Arbitrary TimeoutExpired where
  arbitrary = elements [Expired, Ticking]

instance Arbitrary AbstractState where
  arbitrary = oneof
    [ pure UnknownConnectionSt
    , pure ReservedOutboundSt
    , UnnegotiatedSt <$> arbitrary
    , InboundIdleSt  <$> arbitrary
    , InboundSt      <$> arbitrary
    , pure OutboundUniSt
    , OutboundDupSt  <$> arbitrary
    , OutboundIdleSt <$> arbitrary
    , pure DuplexSt
    , pure WaitRemoteIdleSt
    , pure TerminatingSt
    , pure TerminatedSt
    ]

toPair :: ConnectionId addr -> (addr, addr)
toPair ConnectionId {remoteAddress, localAddress} = (remoteAddress, localAddress)

fromPair :: (addr, addr) -> ConnectionId addr
fromPair (remoteAddress, localAddress) = ConnectionId {remoteAddress, localAddress}

instance (Ord addr, Arbitrary addr) => Arbitrary (ConnectionManagerState addr) where
  arbitrary = do
    connectionMap <- Map.fromList
      <$> listOf ((\(a,b,c) -> (ConnectionId a b, c))
                  `fmap`
                  ((,,) <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary))
    registered <- Set.fromList <$> arbitrary
    let registeredOutboundConnections =
          registered
          `Set.difference`
          Set.map remoteAddress (Map.keysSet connectionMap)
    return ConnectionManagerState {
      connectionMap,
      registeredOutboundConnections
    }

  shrink a@ConnectionManagerState {
      connectionMap,
      registeredOutboundConnections
    } =
    [ a { connectionMap = connectionMap' }
    | connectionMap'
      <- Map.fromList `map` shrinkList (const []) (Map.toList connectionMap)
    ]
    ++
    [ a { registeredOutboundConnections = registeredOutboundConnections' }
    | registeredOutboundConnections'
      <- Set.fromList `map` shrinkList (const []) (Set.toList registeredOutboundConnections)
    ]

disjoint3 :: Ord a => Gen a -> Gen (Set a, Set a, Set a)
disjoint3 gen = do
    (a,b,c) <-
      (,,) <$> (Set.fromList <$> listOf gen)
           <*> (Set.fromList <$> listOf gen)
           <*> (Set.fromList <$> listOf gen)
    return ( a
           , b `Set.difference` a
           , c `Set.difference` a
               `Set.difference` b
           )

disjoint4 :: Ord a => Gen a -> Gen (Set a, Set a, Set a, Set a)
disjoint4 gen = do
    (a,b,c,d) <-
      (,,,) <$> (Set.fromList <$> listOf gen)
            <*> (Set.fromList <$> listOf gen)
            <*> (Set.fromList <$> listOf gen)
            <*> (Set.fromList <$> listOf gen)
    return ( a
           , b `Set.difference` a
           , c `Set.difference` a
               `Set.difference` b
           , d `Set.difference` a
               `Set.difference` b
               `Set.difference` c
           )


instance (Ord addr, Arbitrary addr) => Arbitrary (InboundState addr) where
  arbitrary = do
    (remoteHotSet, remoteWarmSet, remoteColdSet, remoteIdleSet)
      <- disjoint4 (ConnectionId <$> arbitrary <*> arbitrary)
    return InboundState {
      remoteHotSet,
      remoteWarmSet,
      remoteColdSet,
      remoteIdleSet
    }
  shrink a@InboundState {
      remoteHotSet,
      remoteWarmSet,
      remoteColdSet,
      remoteIdleSet
    } =
    [ a { remoteHotSet = remoteHotSet' }
    | remoteHotSet' <- (Set.fromList . map fromPair) `map` shrink (toPair `map` Set.toList remoteHotSet)
    ]
    ++
    [ a { remoteWarmSet = remoteWarmSet' }
    | remoteWarmSet' <- (Set.fromList . map fromPair) `map` shrink (toPair `map` Set.toList remoteWarmSet)
    ]
    ++
    [ a { remoteColdSet = remoteColdSet' }
    | remoteColdSet' <- (Set.fromList . map fromPair) `map` shrink (toPair `map` Set.toList remoteColdSet)
    ]
    ++
    [ a { remoteIdleSet = remoteIdleSet' }
    | remoteIdleSet' <- (Set.fromList . map fromPair) `map` shrink (toPair `map` Set.toList remoteIdleSet)
    ]

instance (Ord addr, Arbitrary addr) => Arbitrary (OutboundState addr) where
  arbitrary = do
    (coldPeers, warmPeers, hotPeers)
      <- disjoint3 arbitrary
    return OutboundState {
      coldPeers,
      warmPeers,
      hotPeers
    }
  shrink a@OutboundState {
      coldPeers,
      warmPeers,
      hotPeers
    } =
    [ a { coldPeers = coldPeers' }
    | coldPeers' <- Set.fromList `map` shrink (Set.toList coldPeers)
    ]
    ++
    [ a { warmPeers = warmPeers' }
    | warmPeers' <- Set.fromList `map` shrink (Set.toList warmPeers)
    ]
    ++
    [ a { hotPeers = hotPeers' }
    | hotPeers' <- Set.fromList `map` shrink (Set.toList hotPeers)
    ]

instance (Ord addr, Arbitrary addr) => Arbitrary (NetworkState addr) where
  arbitrary =
        NetworkState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

  shrink a@NetworkState {
      connectionManagerState,
      inboundGovernorState,
      outboundGovernorState
    } =
    [ a { connectionManagerState = connectionManagerState' }
    | connectionManagerState' <- shrink connectionManagerState
    ]
    ++
    [ a { inboundGovernorState = inboundGovernorState' }
    | inboundGovernorState' <- shrink inboundGovernorState
    ]
    ++
    [ a { outboundGovernorState = outboundGovernorState' }
    | outboundGovernorState' <- shrink outboundGovernorState
    ]


