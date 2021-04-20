{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.Instances (

    -- test types
    PeerAddr(..),

    -- test utils
    renderRanges,

    -- generator tests
    prop_arbitrary_PeerSelectionTargets,
    prop_shrink_PeerSelectionTargets,

  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Word (Word32)
import           Data.Text.Encoding (encodeUtf8)

import           Ouroboros.Network.PeerSelection.RootPeersDNS (DomainAddress(..), RelayAddress(..))
import           Ouroboros.Network.PeerSelection.Governor
import           Ouroboros.Network.PeerSelection.Types

import           Test.QuickCheck
import qualified Data.IP as IP


--
-- QuickCheck instances
--

-- | Simple address representation for the tests
--
newtype PeerAddr = PeerAddr Int
  deriving (Eq, Ord, Show)

-- | We mostly avoid using this instance since we need careful control over
-- the peer addrs, e.g. to make graphs work, and sets overlap etc. But it's
-- here for the few cases that need it, and it is used for (lack-of) shrinking.
--
instance Arbitrary PeerAddr where
  arbitrary = PeerAddr . getNonNegative <$> arbitrary
  shrink _  = []


instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NonEmpty.fromList <$> listOf1 arbitrary

  shrink = shrinkMap from to
    where
      to :: NonEmpty a -> NonEmptyList a
      to xs = NonEmpty (NonEmpty.toList xs)

      from :: NonEmptyList a -> NonEmpty a
      from (NonEmpty xs) = NonEmpty.fromList xs


instance Arbitrary PeerAdvertise where
  arbitrary = elements [ DoAdvertisePeer, DoNotAdvertisePeer ]

  shrink DoAdvertisePeer    = []
  shrink DoNotAdvertisePeer = [DoAdvertisePeer]


instance Arbitrary PeerSelectionTargets where
  arbitrary = do
    targetNumberOfKnownPeers       <-            min 10000 . getNonNegative <$> arbitrary
    targetNumberOfRootPeers        <- choose (0, min 100  targetNumberOfKnownPeers)
    targetNumberOfEstablishedPeers <- choose (0, min 1000 targetNumberOfKnownPeers)
    targetNumberOfActivePeers      <- choose (0, min 100  targetNumberOfEstablishedPeers)
    return PeerSelectionTargets {
      targetNumberOfRootPeers,
      targetNumberOfKnownPeers,
      targetNumberOfEstablishedPeers,
      targetNumberOfActivePeers
    }

  shrink (PeerSelectionTargets r k e a) =
    [ targets'
    | (r',k',e',a') <- shrink (r,k,e,a)
    , let targets' = PeerSelectionTargets r' k' e' a'
    , sanePeerSelectionTargets targets' ]

instance Arbitrary DomainAddress where
  arbitrary =
    DomainAddress . encodeUtf8
      <$> elements domains
      <*> (fromIntegral <$> (arbitrary :: Gen Int))
    where
      domains = [ "test1"
                , "test2"
                , "test3"
                , "test4"
                , "test5"
                ]

genIPv4 :: Gen IP.IP
genIPv4 =
    IP.IPv4 . IP.toIPv4w <$> arbitrary

genIPv6 :: Gen IP.IP
genIPv6 =
    IP.IPv6 . IP.toIPv6w <$> genFourWord32
  where
    genFourWord32 :: Gen (Word32, Word32, Word32, Word32)
    genFourWord32 =
       (,,,) <$> arbitrary
             <*> arbitrary
             <*> arbitrary
             <*> arbitrary

instance Arbitrary RelayAddress where
  arbitrary = do
    isDomain <- arbitrary
    if isDomain
      then RelayDomain <$> arbitrary
      else RelayAddress
             <$> oneof [genIPv4, genIPv6]
             <*> (fromIntegral <$> (arbitrary :: Gen Int))

prop_arbitrary_PeerSelectionTargets :: PeerSelectionTargets -> Bool
prop_arbitrary_PeerSelectionTargets =
    sanePeerSelectionTargets

prop_shrink_PeerSelectionTargets :: PeerSelectionTargets -> Bool
prop_shrink_PeerSelectionTargets =
    all sanePeerSelectionTargets . shrink


--
-- QuickCheck utils
--

renderRanges :: Int -> Int -> String
renderRanges r n = show lower ++ " -- " ++ show upper
  where
    lower = n - n `mod` r
    upper = lower + (r-1)

