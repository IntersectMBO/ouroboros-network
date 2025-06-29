{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: Needed for PeerSharing arbitrary instance see
-- todo there.
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.Handshake.Test where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadCatch, MonadMask, MonadThrow,
           bracket)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (nullTracer)

import Network.Mux.Bearer qualified as Mx
import Network.Mux.Types (MiniProtocolDir (..), MiniProtocolNum (..),
           bearerAsChannel)
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Proofs

import Test.Ouroboros.Network.Protocol.Utils (prop_codec_cborM,
           prop_codec_valid_cbor_encoding, splits2, splits3)

import Ouroboros.Network.Channel
import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.Driver.Simple (runConnectedPeers,
           runConnectedPeersAsymmetric, runPeer)
import Ouroboros.Network.Snocket (TestAddress (..))
import Ouroboros.Network.Snocket qualified as Snocket
import Simulation.Network.Snocket

import Ouroboros.Network.Protocol.Handshake.Client
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Direct
import Ouroboros.Network.Protocol.Handshake.Server
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.Handshake.Version

import Codec.CBOR.Write qualified as CBOR

import Ouroboros.Network.Magic
import Ouroboros.Network.NodeToClient.Version as NTC
import Ouroboros.Network.NodeToNode.Version as NTN

import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol"
    [ testGroup "Handshake"
        [ testProperty "connect"               prop_connect
        , testProperty "channel ST"            prop_channel_ST
        , testProperty "channel IO"            prop_channel_IO
        , testProperty "pipe IO"               prop_pipe_IO
        , testProperty "channel asymmetric ST" prop_channel_asymmetric_ST
        , testProperty "channel asymmetric IO" prop_channel_asymmetric_IO
        , testProperty "pipe asymmetric IO"    prop_pipe_asymmetric_IO

        , testGroup "VersionData"
          [ testProperty "acceptable_symmetric"
              prop_acceptable_symmetric_VersionData
          , testProperty "acceptOrRefuse"
              prop_acceptOrRefuse_symmetric_VersionData
          , testProperty "simultaneous open ST"
              prop_channel_simultaneous_open_ST
          , testProperty "simultaneous open IO"
              prop_channel_simultaneous_open_IO
          , testProperty "simultaneous open SimNet"
              prop_channel_simultaneous_open_SimNet
          ]

        , testGroup "NodeToNode"
          [ testProperty "acceptable_symmetric"
              prop_acceptable_symmetric_NodeToNode
          , testProperty "acceptOrRefuse"
              prop_acceptOrRefuse_symmetric_NodeToNode
          , testProperty "simultaneous open ST"
              prop_channel_simultaneous_open_NodeToNode_ST
          , testProperty "simultaneous open IO"
              prop_channel_simultaneous_open_NodeToNode_IO
          , testProperty "simultaneous open SimNet"
              prop_channel_simultaneous_open_NodeToNode_SimNet
          , testProperty "query version ST"
              prop_query_version_NodeToNode_ST
          , testProperty "query version IO"
              prop_query_version_NodeToNode_IO
          , testProperty "query version SimNet"
              prop_query_version_NodeToNode_SimNet
          , testProperty "peerSharing symmetry"
              prop_peerSharing_symmetric_NodeToNode_SimNet
          ]

        , testGroup "NodeToClient"
          [ testProperty "acceptable_symmetric"
              prop_acceptable_symmetric_NodeToClient
          , testProperty "acceptOrRefuse"
              prop_acceptOrRefuse_symmetric_NodeToClient
          , testProperty "simultaneous open ST"
              prop_channel_simultaneous_open_NodeToClient_ST
          , testProperty "simultaneous open IO"
              prop_channel_simultaneous_open_NodeToClient_IO
          , testProperty "simultaneous open SimNet"
              prop_channel_simultaneous_open_NodeToClient_SimNet
          , testProperty "query version ST"
              prop_query_version_NodeToClient_ST
          , testProperty "query version IO"
              prop_query_version_NodeToClient_IO
          , testProperty "query version SimNet"
              prop_query_version_NodeToClient_SimNet
          ]

        , testProperty "codec RefuseReason"    prop_codec_RefuseReason
        , testProperty "codec"                 prop_codec_Handshake
        , testProperty "codec 2-splits"        prop_codec_splits2_Handshake
        , testProperty "codec 3-splits"      $ withMaxSuccess 30
                                               prop_codec_splits3_Handshake
        , testProperty "codec cbor"            prop_codec_cbor
        , testProperty "codec valid cbor"      prop_codec_valid_cbor
        , testGroup "Generators"
          [ testProperty "ArbitraryVersions" $
              checkCoverage prop_arbitrary_ArbitraryVersions
          , testProperty "arbitrary ArbitraryValidVersions"
              prop_arbitrary_ArbitraryValidVersions
          , testProperty "shrink ArbitraryValidVersions"
              prop_shrink_ArbitraryValidVersions
          ]
        ]
    ]

--
-- Test Versions
--
-- Notes: Associated data are chosen in such a way that a decoder will fail
-- interpreting one of them as the other.  This is done on purpose for testing
-- wrongly encoded data (protocol version & associated version data mismatch)
--

-- | Testing version number
--
data VersionNumber
  = Version_0
  | Version_1
  | Version_2
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Arbitrary VersionNumber where
  arbitrary = elements [minBound .. maxBound]

versionNumberCodec :: CodecCBORTerm (String, Maybe Int) VersionNumber
versionNumberCodec = CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm Version_0 = CBOR.TInt 0
    encodeTerm Version_1 = CBOR.TInt 1
    encodeTerm Version_2 = CBOR.TInt 2

    decodeTerm (CBOR.TInt 0) = Right Version_0
    decodeTerm (CBOR.TInt 1) = Right Version_1
    decodeTerm (CBOR.TInt 2) = Right Version_2
    decodeTerm (CBOR.TInt n) = Left ("unknown version", Just n)
    decodeTerm _             = Left ("unknown tag", Nothing)


versionNumberHandshakeCodec :: ( MonadST    m
                               , MonadThrow m
                               )
                            => Codec (Handshake VersionNumber CBOR.Term)
                                      CBOR.DeserialiseFailure m ByteString
versionNumberHandshakeCodec = codecHandshake versionNumberCodec

data VersionData = VersionData {
    dataVersion0 :: Int,
    dataVersion1 :: Bool,
    dataVersion2 :: Bool
  }
  deriving (Eq, Show, Generic)

instance Acceptable VersionData where
    acceptableVersion d d' =
      if dataVersion0 d /= dataVersion0 d'
        then Refuse (T.pack "incompatible")
        else Accept $ VersionData { dataVersion0 = dataVersion0 d,
                                    dataVersion1 = dataVersion1 d
                                                && dataVersion1 d',
                                    dataVersion2 = dataVersion2 d
                                                || dataVersion2 d' }

instance Queryable VersionData where
    queryVersion _d = False

dataCodecCBORTerm :: VersionNumber -> CodecCBORTerm Text VersionData
dataCodecCBORTerm Version_0 = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      -- We are using @CBOR.TInt@ instead of @CBOR.TInteger@, since for small
      -- integers generated by QuickCheck they will be encoded as @TkInt@ and then
      -- are decoded back to @CBOR.TInt@ rather than @COBR.TInteger@.  The same for
      -- other @CodecCBORTerm@ records.
      encodeTerm VersionData { dataVersion0 } =
        CBOR.TInt dataVersion0

      decodeTerm (CBOR.TInt dataVersion0) =
        Right VersionData { dataVersion0,
                            dataVersion1 = False,
                            dataVersion2 = False }
      decodeTerm n =
        Left $ T.pack $ "decodeTerm VersionData: unrecognised tag: " ++ show n

dataCodecCBORTerm Version_1 = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm VersionData { dataVersion0,
                               dataVersion1 } =
        CBOR.TList [ CBOR.TInt dataVersion0,
                     CBOR.TBool dataVersion1 ]

      decodeTerm (CBOR.TList [ CBOR.TInt dataVersion0,
                               CBOR.TBool dataVersion1 ])
        = Right VersionData { dataVersion0,
                              dataVersion1,
                              dataVersion2 = False }
      decodeTerm n
        = Left $ T.pack $ "decodeTerm VersionData: unrecognised tag: " ++ show n

dataCodecCBORTerm Version_2 = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm VersionData { dataVersion0,
                               dataVersion1,
                               dataVersion2 } =
        CBOR.TList [ CBOR.TInt dataVersion0,
                     CBOR.TBool dataVersion1,
                     CBOR.TBool dataVersion2 ]

      decodeTerm (CBOR.TList [ CBOR.TInt  dataVersion0,
                               CBOR.TBool dataVersion1,
                               CBOR.TBool dataVersion2 ])
        = Right VersionData { dataVersion0,
                              dataVersion1,
                              dataVersion2 }
      decodeTerm n
        = Left $ T.pack $ "decodeTerm Data: unrecognised tag: " ++ show n

arbitraryVersionData :: VersionNumber -> Gen VersionData
arbitraryVersionData Version_0 = (\a -> VersionData a False False)
                              <$> arbitrary
arbitraryVersionData Version_1 = (\a b -> VersionData a b False)
                              <$> arbitrary
                              <*> arbitrary
arbitraryVersionData Version_2 = VersionData
                              <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary

instance Arbitrary VersionData where
  arbitrary = arbitrary >>= arbitraryVersionData

instance CoArbitrary VersionData where

--
-- ProtocolVersion generators
--

application :: VersionData -> VersionData -> Bool
application d = \d' ->
     (dataVersion0 d == dataVersion0 d')
     -- we take `&&` (see `acceptableVersions`
     -- below), which is like `*` in Z₂
  && (dataVersion1 d >= dataVersion1 d')
     -- we take `||` (see `acceptableVersions`
     -- below), which is like `+` in Z₂
  && (dataVersion2 d <= dataVersion2 d')

-- |
-- Generate a valid @'ProtocolVersion' 'VersionNumber' r@
--
genValidVersion
  :: VersionNumber
  -> Gen (Version VersionData Bool)
genValidVersion version = do
  d <- arbitraryVersionData version
  return $ Version (application d) d


-- |
-- Generate an invalid @'ProtocolVersion' 'VersionNumber' r@.
--
genInvalidVersion
  :: VersionNumber
  -> Gen (Version VersionData Bool)
genInvalidVersion Version_0 =
    oneof [ genValidVersion Version_1
          , genValidVersion Version_2 ]
genInvalidVersion Version_1 =
    oneof [ genValidVersion Version_0
          , genValidVersion Version_2 ]
genInvalidVersion Version_2 =
    oneof [ genValidVersion Version_0
          , genValidVersion Version_1 ]

-- |
-- Generate valid @Versions@.
--
genValidVersions :: Gen (Versions VersionNumber VersionData Bool)
genValidVersions = do
  vns <- nub <$> resize 3 (listOf1 (arbitrary :: Gen VersionNumber))
  vs <- traverse genValidVersion vns
  return $ Versions $ Map.fromList $ zip vns vs

-- |
-- Generate possibly invalid @Versions@.
--
genVersions :: Gen (Versions VersionNumber VersionData Bool)
genVersions = do
  vns <- nub <$> resize 3 (listOf1 (arbitrary :: Gen VersionNumber))
  vs <- traverse (\v -> oneof [genValidVersion v, genInvalidVersion v]) vns
  return $ Versions $ Map.fromList $ zip vns vs

newtype ArbitraryValidVersions = ArbitraryValidVersions {
      runArbitraryValidVersions :: Versions VersionNumber VersionData Bool
    }

instance Show ArbitraryValidVersions where
    show (ArbitraryValidVersions (Versions vs)) = show (Map.map versionData vs)

instance Arbitrary ArbitraryValidVersions where
    arbitrary = ArbitraryValidVersions <$> genValidVersions
    -- TODO: shrink (issue #3407)

prop_arbitrary_ArbitraryValidVersions
  :: ArbitraryValidVersions
  -> Bool
prop_arbitrary_ArbitraryValidVersions (ArbitraryValidVersions vs) =
    Map.foldlWithKey' (\r vn s -> r && validVersion vn s) True (getVersions vs)

prop_shrink_ArbitraryValidVersions
  :: ArbitraryValidVersions
  -> Bool
prop_shrink_ArbitraryValidVersions a = all id
  [ Map.foldlWithKey' (\r vn s -> r && validVersion vn s) True (getVersions vs')
  | ArbitraryValidVersions vs' <- shrink a
  ]

-- |
-- Generators for pairs of arbitrary list of versions.
--
data ArbitraryVersions =
  ArbitraryVersions
    (Versions VersionNumber VersionData Bool)
    (Versions VersionNumber VersionData Bool)

instance Show ArbitraryVersions where
    show (ArbitraryVersions (Versions vs) (Versions vs'))
      = "ArbitraryVersions " ++ show (Map.map versionData vs) ++ " " ++ show (Map.map versionData vs')

instance Arbitrary ArbitraryVersions where
    arbitrary = frequency
      [ (1, (\v -> ArbitraryVersions v v) <$> genVersions)
      , (2, ArbitraryVersions <$> genVersions <*> genVersions)
      ]
    shrink (ArbitraryVersions (Versions vs) (Versions vs')) =
      [ ArbitraryVersions (Versions $ Map.fromList vs'') (Versions vs')
      | vs'' <- shrinkList (const []) (Map.toList vs)
      ] ++
      [ ArbitraryVersions (Versions vs) (Versions $ Map.fromList vs'')
      | vs'' <- shrinkList (const []) (Map.toList vs')
      ]


-- |
-- Check if a @'ProtocolVersion' 'VersionNumber' r@ is valid.
--
validVersion :: VersionNumber -> Version VersionData Bool -> Bool
validVersion Version_0 ((Version _ d)) = dataVersion1 d == False
                                      && dataVersion2 d == False
validVersion Version_1 ((Version _ d)) = dataVersion2 d == False
validVersion Version_2 ((Version _ _)) = True


prop_arbitrary_ArbitraryVersions :: ArbitraryVersions -> Property
prop_arbitrary_ArbitraryVersions (ArbitraryVersions (Versions vs) (Versions vs')) =
    -- in 75% of cases the intersection is non-empty
    cover 75 intersect "non-empty intersection" $

    -- in 10% of cases the intersection is empty
    cover 10 (not intersect) "empty intersection" $

    -- in 25% of cases the common max version is valid
    cover 25 (case Map.lookupMax intersection of
               Nothing      -> False
               Just (vn, s) -> validVersion vn s)
               "valid common max version" $

    -- in 25% of cases all the versions in @vs'@ are either not in @vs@ or are
    -- not valid
    cover 25
      (Map.foldlWithKey' (\r vn s -> r && (not (vn `elem` knownVersionNumbers) || not (validVersion vn s))) True vs)
      "all versions are either unknown or not valid" $

    property True
  where
    intersection = vs `Map.intersection` vs'
    intersect    = not (Map.null intersection)

    knownVersionNumbers = Map.keys vs'

maybeAccept :: Accept a -> Maybe a
maybeAccept (Accept a) = Just a
maybeAccept (Refuse _) = Nothing

-- | Run a handshake protocol, without going via a channel.
--
prop_connect :: ArbitraryVersions -> Property
prop_connect (ArbitraryVersions clientVersions serverVersions) =
  let (serverRes, clientRes) =
        pureHandshake
          ((maybeAccept .) . acceptableVersion)
          serverVersions
          clientVersions
  in case runSimOrThrow
           (connect
              (handshakeClientPeer
                (cborTermVersionDataCodec dataCodecCBORTerm)
                acceptableVersion
                clientVersions)
              (handshakeServerPeer
                (cborTermVersionDataCodec dataCodecCBORTerm)
                acceptableVersion
                queryVersion
                serverVersions)) of
      (clientRes', serverRes', TerminalStates SingDone SingDone) ->
           fromMaybe False clientRes === either (const False) extractRes clientRes'
        .&&.
           fromMaybe False serverRes === either (const False) extractRes serverRes'
  where
    extractRes (HandshakeNegotiationResult r _ _) = r
    extractRes (HandshakeQueryResult _)           = False

--
-- Properties using a channel
--

-- | Run a simple block-fetch client and server using connected channels.
--
prop_channel :: ( MonadAsync m
                , MonadCatch m
                , MonadST m
                )
             => m (Channel m ByteString, Channel m ByteString)
             -> Versions VersionNumber VersionData Bool
             -> Versions VersionNumber VersionData Bool
             -> m Property
prop_channel createChannels clientVersions serverVersions =
  let (!serverRes, !clientRes) =
        pureHandshake
          ((maybeAccept .) . acceptableVersion)
          serverVersions
          clientVersions
  in do
    (!clientRes', !serverRes') <-
      runConnectedPeers
        createChannels nullTracer versionNumberHandshakeCodec
        (handshakeClientPeer
          (cborTermVersionDataCodec dataCodecCBORTerm)
          acceptableVersion
          clientVersions)
        (handshakeServerPeer
          (cborTermVersionDataCodec dataCodecCBORTerm)
          acceptableVersion
          queryVersion
          serverVersions)
    pure $!
      case (clientRes', serverRes') of
        -- both succeeded, we just check that the application (which is
        -- a boolean value) is the one that was put inside 'Version'
        (Right (HandshakeNegotiationResult !c _ _), Right (HandshakeNegotiationResult !s _ _)) ->
               Just c === clientRes
          .&&. Just s === serverRes

        -- both queried versions
        (Right (HandshakeQueryResult _), Right (HandshakeQueryResult _)) ->
          property True

        -- both failed
        (Left{}, Left{})   -> property True

        -- it should not happen that one protocol succeeds and the other end
        -- fails
        _                  -> property False


-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_ST :: ArbitraryVersions -> Property
prop_channel_ST (ArbitraryVersions clientVersions serverVersions) =
    runSimOrThrow (prop_channel createConnectedChannels clientVersions serverVersions)


-- | Run 'prop_channel' in the IO monad.
--
prop_channel_IO :: ArbitraryVersions -> Property
prop_channel_IO (ArbitraryVersions clientVersions serverVersions) =
    ioProperty (prop_channel createConnectedChannels clientVersions serverVersions)


-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_IO :: ArbitraryVersions -> Property
prop_pipe_IO (ArbitraryVersions clientVersions serverVersions) =
    ioProperty (prop_channel createPipeConnectedChannels clientVersions serverVersions)

--
-- Asymmetric tests
--


-- | Run a simple handshake client and server using connected channels.
-- The server can only decode a subset of versions send by client.
-- This test is using a fixed server 'Versions' which can only accept
-- a single version 'Version_1' (it cannot decode any other version).
--
prop_channel_asymmetric
    :: ( MonadAsync m
       , MonadCatch m
       , MonadLabelledSTM m
       , MonadMask  m
       , MonadST m
       )
    => m (Channel m ByteString, Channel m ByteString)
    -> Versions VersionNumber VersionData Bool
    -- ^ client versions
    -> m Property
prop_channel_asymmetric createChannels clientVersions = do
    (clientRes', serverRes') <-
      runConnectedPeersAsymmetric
        createChannels
        nullTracer
        versionNumberHandshakeCodec
        (codecHandshake versionNumberCodec')
        (handshakeClientPeer
          (cborTermVersionDataCodec dataCodecCBORTerm)
          acceptableVersion
          clientVersions)
        (handshakeServerPeer
          (cborTermVersionDataCodec dataCodecCBORTerm)
          acceptableVersion
          queryVersion
          serverVersions)
    pure $
      case (clientRes', serverRes') of
        (Right (HandshakeNegotiationResult c _ _), Right (HandshakeNegotiationResult s _ _))
                           -> Just c === clientRes
                         .&&. Just s === serverRes
        (Right (HandshakeQueryResult _), Right (HandshakeQueryResult _))
                           -> property True

        (Left{}, Left{})   -> property True
        _                  -> property False

  where
    -- server versions
    serverVersions :: Versions VersionNumber VersionData Bool
    serverVersions =
      let d = VersionData 0 True True in
      Versions
        $ Map.singleton
            Version_1
            (Version (application d) d)


    -- This codec does not know how to decode 'Version_0' and 'Version_2'.
    versionNumberCodec' :: CodecCBORTerm (String, Maybe Int) VersionNumber
    versionNumberCodec' = CodecCBORTerm { encodeTerm, decodeTerm }
      where
        encodeTerm Version_1 = CBOR.TInt 1
        encodeTerm _         = error "server encoder error"

        decodeTerm (CBOR.TInt 1) = Right Version_1
        decodeTerm (CBOR.TInt n) = Left ("unknown version", Just n)
        decodeTerm _             = Left ("unknown tag", Nothing)

    (serverRes, clientRes) =
      pureHandshake
        ((maybeAccept .) . acceptableVersion)
        serverVersions
        clientVersions



-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_asymmetric_ST :: ArbitraryVersions -> Property
prop_channel_asymmetric_ST (ArbitraryVersions clientVersions _serverVersions) =
    runSimOrThrow (prop_channel_asymmetric createConnectedChannels clientVersions)


-- | Run 'prop_channel' in the IO monad.
--
prop_channel_asymmetric_IO :: ArbitraryVersions -> Property
prop_channel_asymmetric_IO (ArbitraryVersions clientVersions _serverVersions) =
    ioProperty (prop_channel_asymmetric createConnectedChannels clientVersions)


-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_asymmetric_IO :: ArbitraryVersions -> Property
prop_pipe_asymmetric_IO (ArbitraryVersions clientVersions _serverVersions) =
    ioProperty (prop_channel_asymmetric createPipeConnectedChannels clientVersions)



--
-- NodeToNode generators
--

newtype ArbitraryNodeToNodeVersion =
        ArbitraryNodeToNodeVersion { getNodeToNodeVersion :: NodeToNodeVersion }
  deriving Show

instance Arbitrary ArbitraryNodeToNodeVersion where
    arbitrary = elements (ArbitraryNodeToNodeVersion <$> [minBound .. maxBound])

newtype ArbitraryNodeToNodeVersionData =
        ArbitraryNodeToNodeVersionData
          { getNodeToNodeVersionData :: NodeToNodeVersionData }
    deriving Show
    deriving Acceptable via NodeToNodeVersionData

-- | With the introduction of PeerSharing to 'NodeToNodeVersionData' this type's
-- 'Acceptable' instance is no longer symmetric. Because when handshake is
-- performed we keep only the remote's side PeerSharing information. Due to this,
-- the 'ArbitraryNodeToNodeVersionData' needs to have a custom 'Eq' type that
-- ignores this parameter. We also ignore the query field which may differ
-- between parties.
--
instance Eq ArbitraryNodeToNodeVersionData where
  (==) (ArbitraryNodeToNodeVersionData (NodeToNodeVersionData nm dm ps _))
       (ArbitraryNodeToNodeVersionData (NodeToNodeVersionData nm' dm' ps' _))
    = nm == nm' && dm == dm' && ps == ps'

instance Queryable ArbitraryNodeToNodeVersionData where
    queryVersion = queryVersion . getNodeToNodeVersionData

instance Arbitrary ArbitraryNodeToNodeVersionData where
    arbitrary = fmap (fmap (fmap ArbitraryNodeToNodeVersionData))
              . NodeToNodeVersionData
             <$> (NetworkMagic <$> arbitrary)
             <*> elements [ InitiatorOnlyDiffusionMode
                          , InitiatorAndResponderDiffusionMode
                          ]
             <*> elements [ PeerSharingDisabled
                          , PeerSharingEnabled
                          ]
             <*> arbitrary
    shrink (ArbitraryNodeToNodeVersionData
             (NodeToNodeVersionData magic mode peerSharing query)) =
        [ ArbitraryNodeToNodeVersionData (NodeToNodeVersionData magic' mode peerSharing' query)
        | magic' <- NetworkMagic <$> shrink (unNetworkMagic magic)
        , peerSharing' <- shrinkPeerSharing peerSharing
        ]
        ++
        [ ArbitraryNodeToNodeVersionData (NodeToNodeVersionData magic mode' peerSharing' query)
        | mode' <- shrinkMode mode
        , peerSharing' <- shrinkPeerSharing peerSharing
        ]
        ++
        [ ArbitraryNodeToNodeVersionData (NodeToNodeVersionData magic mode peerSharing' query')
        | query' <- shrink query
        , peerSharing' <- shrinkPeerSharing peerSharing
        ]
      where
        shrinkMode :: DiffusionMode -> [DiffusionMode]
        shrinkMode InitiatorOnlyDiffusionMode = []
        shrinkMode InitiatorAndResponderDiffusionMode = [InitiatorOnlyDiffusionMode]

        shrinkPeerSharing PeerSharingDisabled = []
        shrinkPeerSharing PeerSharingEnabled  = [PeerSharingDisabled]

newtype ArbitraryNodeToNodeVersions =
        ArbitraryNodeToNodeVersions
          { getArbitraryNodeToNodeVersiosn :: Versions NodeToNodeVersion
                                                       ArbitraryNodeToNodeVersionData Bool }

instance Show ArbitraryNodeToNodeVersions where
    show (ArbitraryNodeToNodeVersions (Versions vs))
      = "ArbitraryNodeToNodeVersions " ++ show (Map.map versionData vs)

instance Arbitrary ArbitraryNodeToNodeVersions where
    arbitrary = do
      vs <- listOf (getNodeToNodeVersion <$> arbitrary)
      ds <- vectorOf (length vs) arbitrary
      r  <- arbitrary
      return $ ArbitraryNodeToNodeVersions
             $ Versions
             $ Map.fromList
                [ (v, Version (const r) d)
                | (v, d) <- zip vs ds
                ]
    -- TODO: shrink (issue #3407)


--
-- NodeToClient generators
--

newtype ArbitraryNodeToClientVersion =
        ArbitraryNodeToClientVersion { getNodeToClientVersion :: NodeToClientVersion }
    deriving Show

instance Arbitrary ArbitraryNodeToClientVersion where
    arbitrary = elements (ArbitraryNodeToClientVersion <$> [minBound .. maxBound])

newtype ArbitraryNodeToClientVersionData =
        ArbitraryNodeToClientVersionData
          { getNodeToClientVersionData :: NodeToClientVersionData }
    deriving Show

instance Arbitrary ArbitraryNodeToClientVersionData where
    arbitrary = ( (ArbitraryNodeToClientVersionData .)
                . NodeToClientVersionData
                )
            <$> (NetworkMagic <$> arbitrary)
            <*> arbitrary
    shrink (ArbitraryNodeToClientVersionData
             (NodeToClientVersionData magic query)) =
        [ ArbitraryNodeToClientVersionData (NodeToClientVersionData magic' query)
        | magic' <- NetworkMagic <$> shrink (unNetworkMagic magic)
        ]
        ++
        [ ArbitraryNodeToClientVersionData (NodeToClientVersionData magic query')
        | query' <- shrink query
        ]

newtype ArbitraryNodeToClientVersions =
        ArbitraryNodeToClientVersions
          { getArbitraryNodeToClientVersiosn :: Versions NodeToClientVersion
                                                       NodeToClientVersionData Bool }

instance Show ArbitraryNodeToClientVersions where
    show (ArbitraryNodeToClientVersions (Versions vs))
      = "ArbitraryNodeToClientVersions " ++ show (Map.map versionData vs)

instance Arbitrary ArbitraryNodeToClientVersions where
    arbitrary = do
      vs <- listOf (getNodeToClientVersion <$> arbitrary)
      ds <- vectorOf (length vs) (getNodeToClientVersionData <$> arbitrary)
      r  <- arbitrary
      return $ ArbitraryNodeToClientVersions
             $ Versions
             $ Map.fromList
                [ (v, Version (const r) d)
                | (v, d) <- zip vs ds
                ]
    -- TODO: shrink (issue #3407)


prop_acceptable_symmetric
  :: ( Acceptable vData
     , Eq vData
     )
  => vData
  -> vData
  -> Bool
prop_acceptable_symmetric vData vData' =
    case (acceptableVersion vData vData', acceptableVersion vData' vData) of
      (Accept a, Accept b) -> a == b
      (Refuse _, Refuse _) -> True
      (_       , _       ) -> False

prop_acceptable_symmetric_VersionData
  :: VersionData
  -> VersionData
  -> Bool
prop_acceptable_symmetric_VersionData a b =
    prop_acceptable_symmetric a b


prop_acceptable_symmetric_NodeToNode
  :: ArbitraryNodeToNodeVersionData
  -> ArbitraryNodeToNodeVersionData
  -> Bool
prop_acceptable_symmetric_NodeToNode a b =
    prop_acceptable_symmetric a b


prop_acceptable_symmetric_NodeToClient
  :: ArbitraryNodeToClientVersionData
  -> ArbitraryNodeToClientVersionData
  -> Bool
prop_acceptable_symmetric_NodeToClient (ArbitraryNodeToClientVersionData a)
                                       (ArbitraryNodeToClientVersionData b) =
    prop_acceptable_symmetric a b


-- | Run 'prop_query_version' in the simulation monad.
--
prop_query_version_NodeToNode_ST :: ArbitraryNodeToNodeVersions
                                 -> ArbitraryNodeToNodeVersions
                                 -> Property
prop_query_version_NodeToNode_ST
     (ArbitraryNodeToNodeVersions clientVersions)
     (ArbitraryNodeToNodeVersions serverVersions) =
   runSimOrThrow $ prop_query_version
                    createConnectedChannels
                    (codecHandshake nodeToNodeVersionCodec)
                    (cborTermVersionDataCodec (fmap transformNodeToNodeVersionData nodeToNodeCodecCBORTerm))
                    clientVersions
                    serverVersions
                    (\(ArbitraryNodeToNodeVersionData vd) ->
                      ArbitraryNodeToNodeVersionData $
                        vd { NTN.query = True
                           , NTN.peerSharing = PeerSharingEnabled
                           })

-- | Run 'prop_query_version' in the IO monad.
--
prop_query_version_NodeToNode_IO :: ArbitraryNodeToNodeVersions
                                 -> ArbitraryNodeToNodeVersions
                                 -> Property
prop_query_version_NodeToNode_IO
     (ArbitraryNodeToNodeVersions clientVersions)
     (ArbitraryNodeToNodeVersions serverVersions) =
   ioProperty $ prop_query_version
                    createConnectedChannels
                    (codecHandshake nodeToNodeVersionCodec)
                    (cborTermVersionDataCodec (fmap transformNodeToNodeVersionData nodeToNodeCodecCBORTerm))
                    clientVersions
                    serverVersions
                    (\(ArbitraryNodeToNodeVersionData vd) ->
                      ArbitraryNodeToNodeVersionData $
                        vd { NTN.query = True
                           , NTN.peerSharing = PeerSharingEnabled
                           })

-- | Run 'prop_query_version' with SimNet.
--
prop_query_version_NodeToNode_SimNet :: ArbitraryNodeToNodeVersions
                                     -> ArbitraryNodeToNodeVersions
                                     -> Property
prop_query_version_NodeToNode_SimNet
     (ArbitraryNodeToNodeVersions clientVersions)
     (ArbitraryNodeToNodeVersions serverVersions) =
   runSimOrThrow $ prop_query_version
                    createConnectedChannels
                    (codecHandshake nodeToNodeVersionCodec)
                    (cborTermVersionDataCodec (fmap transformNodeToNodeVersionData nodeToNodeCodecCBORTerm))
                    clientVersions
                    serverVersions
                    (\(ArbitraryNodeToNodeVersionData vd) ->
                      ArbitraryNodeToNodeVersionData $
                        vd { NTN.query = True
                           , NTN.peerSharing = PeerSharingEnabled
                           })

-- | Run 'prop_query_version' in the simulation monad.
--
prop_query_version_NodeToClient_ST :: ArbitraryNodeToClientVersions
                                   -> ArbitraryNodeToClientVersions
                                   -> Property
prop_query_version_NodeToClient_ST
     (ArbitraryNodeToClientVersions clientVersions)
     (ArbitraryNodeToClientVersions serverVersions) =
   runSimOrThrow $ prop_query_version
                    createConnectedChannels
                    (codecHandshake nodeToClientVersionCodec)
                    (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
                    clientVersions
                    serverVersions
                    (\vd -> vd {NTC.query = True})

-- | Run 'prop_query_version' in the IO monad.
--
prop_query_version_NodeToClient_IO :: ArbitraryNodeToClientVersions
                                   -> ArbitraryNodeToClientVersions
                                   -> Property
prop_query_version_NodeToClient_IO
     (ArbitraryNodeToClientVersions clientVersions)
     (ArbitraryNodeToClientVersions serverVersions) =
   ioProperty $ prop_query_version
                    createConnectedChannels
                    (codecHandshake nodeToClientVersionCodec)
                    (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
                    clientVersions
                    serverVersions
                    (\vd -> vd {NTC.query = True})

-- | Run 'prop_query_version' with SimNet.
--
prop_query_version_NodeToClient_SimNet :: ArbitraryNodeToClientVersions
                                       -> ArbitraryNodeToClientVersions
                                       -> Property
prop_query_version_NodeToClient_SimNet
     (ArbitraryNodeToClientVersions clientVersions)
     (ArbitraryNodeToClientVersions serverVersions) =
   runSimOrThrow $ prop_query_version
                    createConnectedChannels
                    (codecHandshake nodeToClientVersionCodec)
                    (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
                    clientVersions
                    serverVersions
                    (\vd -> vd {NTC.query = True})


-- | Run a query for the server's supported version.
--
prop_query_version :: ( MonadAsync m
                      , MonadCatch m
                      , MonadST m
                      , Eq vData
                      , Acceptable vData
                      , Queryable vData
                      , Show vData
                      , Ord vNumber
                      , Show vNumber
                      )
                   => m (Channel m ByteString, Channel m ByteString)
                   -> Codec (Handshake vNumber CBOR.Term)
                             CBOR.DeserialiseFailure m ByteString
                   -> VersionDataCodec CBOR.Term vNumber vData
                   -> Versions vNumber vData Bool
                   -> Versions vNumber vData Bool
                   -> (vData -> vData)
                   -> m Property
prop_query_version createChannels codec versionDataCodec clientVersions serverVersions setQuery = do
  (clientRes, _serverRes) <-
    runConnectedPeers
      createChannels nullTracer codec
      (handshakeClientPeer
        versionDataCodec
        acceptableVersion
        clientVersions')
      (handshakeServerPeer
        versionDataCodec
        acceptableVersion
        queryVersion
        serverVersions)
  pure $ case clientRes of
    -- Ignore handshake errors.
    Left _ ->
      property True
    -- We should receive the queried versions.
    Right HandshakeNegotiationResult {} ->
      -- We will only receive a negotiated result if the negotiated version does not support queries.
      property False
    -- On successful handshakes, the received versions should match the server versions (ignoring `query`).
    Right (HandshakeQueryResult serverVersions') ->
      setQueryAll serverVersions' === setQueryAll (Right . versionData <$> getVersions serverVersions)
  where
    setQueryAll vs = (setQuery <$>) <$> vs
    setQueryVersions (Versions vs) = Versions $ (\_k v -> v { versionData = setQuery (versionData v) }) `Map.mapWithKey` vs
    clientVersions' = setQueryVersions clientVersions


-- | Run a query for the server's supported version.
--
prop_peerSharing_symmetric :: ( MonadAsync m
                           , MonadCatch m
                           , MonadST m
                           )
                           => m (Channel m ByteString, Channel m ByteString)
                           -> Codec (Handshake NodeToNodeVersion CBOR.Term)
                                     CBOR.DeserialiseFailure m ByteString
                           -> VersionDataCodec CBOR.Term NodeToNodeVersion ArbitraryNodeToNodeVersionData
                           -> Versions NodeToNodeVersion ArbitraryNodeToNodeVersionData Bool
                           -> Versions NodeToNodeVersion ArbitraryNodeToNodeVersionData Bool
                           -> m Property
prop_peerSharing_symmetric createChannels codec versionDataCodec clientVersions serverVersions = do
  (clientRes, serverRes) <-
    runConnectedPeers
      createChannels nullTracer codec
      (handshakeClientPeer
        versionDataCodec
        acceptableVersion
        clientVersions)
      (handshakeServerPeer
        versionDataCodec
        acceptableVersion
        queryVersion
        serverVersions)
  pure $ case (clientRes, serverRes) of
    -- TODO: make this return ArbitraryNodeToNodeVersionData rather than a pair
    -- of NodeToNodeVersionData
    (  Right (HandshakeNegotiationResult _ v (ArbitraryNodeToNodeVersionData clientResult))
     , Right (HandshakeNegotiationResult _ v' (ArbitraryNodeToNodeVersionData serverResult))
     ) | v == v'
       , v >= NodeToNodeV_14 ->
         counterexample
              ("VersionNumber: " ++ show v)
          $ clientResult === serverResult
       | v == v'
       , v < NodeToNodeV_14  -> property True
       | otherwise  -> counterexample "Version mismatch" False
    (Right _, Left _) -> counterexample "Acceptance mismatch" False
    (Left _, Right _) -> counterexample "Acceptance mismatch" False
    _ -> property True

-- | Run 'prop_peerSharing_symmetric' with SimNet.
--
prop_peerSharing_symmetric_NodeToNode_SimNet
  :: ArbitraryNodeToNodeVersions
  -> ArbitraryNodeToNodeVersions
  -> Property
prop_peerSharing_symmetric_NodeToNode_SimNet
     (ArbitraryNodeToNodeVersions clientVersions)
     (ArbitraryNodeToNodeVersions serverVersions) =
   runSimOrThrow $ prop_peerSharing_symmetric
                    createConnectedChannels
                    (codecHandshake nodeToNodeVersionCodec)
                    (cborTermVersionDataCodec (fmap transformNodeToNodeVersionData nodeToNodeCodecCBORTerm))
                    clientVersions
                    serverVersions

-- | 'acceptOrRefuse' is symmetric in the following sense:
--
-- Either both sides:
-- * accept the same version and version data; or
-- * refuse
--
-- The refuse reason might differ, although if one side refuses it with
-- `Refused` the other side must refuse the same version.
--
prop_acceptOrRefuse_symmetric
  :: forall vNumber vData r.
     ( Acceptable vData
     , Eq   vData
     , Show vData
     , Ord  vNumber
     , Show vNumber
     , Eq   r
     , Show r
     )
  => Versions vNumber vData r
  -> Versions vNumber vData r
  -> Property
prop_acceptOrRefuse_symmetric clientVersions serverVersions =
    case ( acceptOrRefuse codec acceptableVersion clientVersions serverMap
         , acceptOrRefuse codec acceptableVersion serverVersions clientMap
         ) of
      (Right (_, vNumber, vData), Right (_, vNumber', vData')) ->
             vNumber === vNumber'
        .&&. vData   === vData'
      (Left (VersionMismatch vNumbers _), Left (VersionMismatch vNumbers' _)) ->
             vNumbers  === Map.keys clientMap
        .&&. vNumbers' === Map.keys serverMap
      (Left HandshakeDecodeError {}, Left _) ->
        property True
      (Left _, Left HandshakeDecodeError {}) ->
        property True
      (Left (Refused vNumber _), Left (Refused vNumber' _)) ->
        vNumber === vNumber'
      (_      , _      ) ->
        property False

  where
    codec :: VersionDataCodec vData vNumber vData
    codec = VersionDataCodec {
        encodeData = \_ vData -> vData,
        decodeData = \_ vData -> Right vData
      }

    toMap :: Versions vNumber vData r
          -> Map vNumber vData
    toMap (Versions m) = versionData `Map.map` m

    clientMap = toMap clientVersions
    serverMap = toMap serverVersions


prop_acceptOrRefuse_symmetric_VersionData
  :: ArbitraryVersions
  -> Property
prop_acceptOrRefuse_symmetric_VersionData (ArbitraryVersions a b) =
    prop_acceptOrRefuse_symmetric a b


prop_acceptOrRefuse_symmetric_NodeToNode
  :: ArbitraryNodeToNodeVersions
  -> ArbitraryNodeToNodeVersions
  -> Property
prop_acceptOrRefuse_symmetric_NodeToNode (ArbitraryNodeToNodeVersions a)
                                         (ArbitraryNodeToNodeVersions b) =

  prop_acceptOrRefuse_symmetric a b


prop_acceptOrRefuse_symmetric_NodeToClient
  :: ArbitraryNodeToClientVersions
  -> ArbitraryNodeToClientVersions
  -> Property
prop_acceptOrRefuse_symmetric_NodeToClient (ArbitraryNodeToClientVersions a)
                                           (ArbitraryNodeToClientVersions b) =

  prop_acceptOrRefuse_symmetric a b


-- | Run two handshake clients against each other, which simulates a TCP
-- simultaneous open.
--
prop_channel_simultaneous_open
    :: ( MonadAsync m
       , MonadCatch m
       , MonadST m
       , Acceptable vData
       , Ord vNumber
       )
    => m (Channel m ByteString, Channel m ByteString)
    -> Codec (Handshake vNumber CBOR.Term)
              CBOR.DeserialiseFailure m ByteString
    -> VersionDataCodec CBOR.Term vNumber vData
    -> Versions vNumber vData Bool
    -> Versions vNumber vData Bool
    -> m Property
prop_channel_simultaneous_open createChannels codec versionDataCodec clientVersions serverVersions =
  let (serverRes, clientRes) =
        pureHandshake
          ((maybeAccept .) . acceptableVersion)
          serverVersions
          clientVersions
  in do
    (clientChannel, serverChannel) <- createChannels
    let client  = handshakeClientPeer
                    versionDataCodec
                    acceptableVersion
                    clientVersions
        client' = handshakeClientPeer
                    versionDataCodec
                    acceptableVersion
                    serverVersions
    (clientRes', serverRes') <-
      (fst <$> runPeer nullTracer
                      -- (("client",) `contramap` Tracer Debug.traceShowM)
                       codec clientChannel client)
        `concurrently`
      (fst <$> runPeer nullTracer
                      -- (("server",) `contramap` Tracer Debug.traceShowM)
                       codec serverChannel client')
    pure $
      case (clientRes', serverRes') of
        -- both succeeded, we just check that the application (which is
        -- a boolean value) is the one that was put inside 'Version'
        (Right (HandshakeNegotiationResult c _ _), Right (HandshakeNegotiationResult s _ _)) ->
          label "both-succeed" $
               Just c === clientRes
          .&&. Just s === serverRes

        -- both queried versions
        (Right (HandshakeQueryResult _), Right (HandshakeQueryResult _)) ->
          label "both-query" True

        -- both failed
        (Left{}, Left{})   -> label "both-failed" True

        -- it should not happen that one protocol succeeds and the other end
        -- fails
        _                  -> property False

-- | Run 'prop_channel_simultaneous_open' in the simulation monad.
--
prop_channel_simultaneous_open_ST :: ArbitraryVersions -> Property
prop_channel_simultaneous_open_ST (ArbitraryVersions clientVersions serverVersions) =
  runSimOrThrow $ prop_channel_simultaneous_open
                    createConnectedChannels
                    versionNumberHandshakeCodec
                    (cborTermVersionDataCodec dataCodecCBORTerm)
                    clientVersions
                    serverVersions

-- | Run 'prop_channel_simultaneous_open' in the IO monad.
--
prop_channel_simultaneous_open_IO :: ArbitraryVersions -> Property
prop_channel_simultaneous_open_IO (ArbitraryVersions clientVersions serverVersions) =
  ioProperty $ prop_channel_simultaneous_open
                 createConnectedChannels
                 versionNumberHandshakeCodec
                 (cborTermVersionDataCodec dataCodecCBORTerm)
                 clientVersions
                 serverVersions


prop_channel_simultaneous_open_NodeToNode_ST :: ArbitraryNodeToNodeVersions
                                             -> ArbitraryNodeToNodeVersions
                                             -> Property
prop_channel_simultaneous_open_NodeToNode_ST
    (ArbitraryNodeToNodeVersions clientVersions)
    (ArbitraryNodeToNodeVersions serverVersions) =
  runSimOrThrow $ prop_channel_simultaneous_open
                    createConnectedChannels
                    (codecHandshake nodeToNodeVersionCodec)
                    (cborTermVersionDataCodec (fmap transformNodeToNodeVersionData nodeToNodeCodecCBORTerm))
                    clientVersions
                    serverVersions

transformNodeToNodeVersionData :: CodecCBORTerm Text NodeToNodeVersionData
          -> CodecCBORTerm Text ArbitraryNodeToNodeVersionData
transformNodeToNodeVersionData (CodecCBORTerm g h) =
  CodecCBORTerm { encodeTerm = \(ArbitraryNodeToNodeVersionData a) -> g a
                , decodeTerm = fmap (fmap ArbitraryNodeToNodeVersionData) h
                }


prop_channel_simultaneous_open_NodeToNode_IO :: ArbitraryNodeToNodeVersions
                                             -> ArbitraryNodeToNodeVersions
                                             -> Property
prop_channel_simultaneous_open_NodeToNode_IO
    (ArbitraryNodeToNodeVersions clientVersions)
    (ArbitraryNodeToNodeVersions serverVersions) =
  ioProperty $ prop_channel_simultaneous_open
                    createConnectedChannels
                    (codecHandshake nodeToNodeVersionCodec)
                    (cborTermVersionDataCodec (fmap transformNodeToNodeVersionData nodeToNodeCodecCBORTerm))
                    clientVersions
                    serverVersions


prop_channel_simultaneous_open_NodeToClient_ST :: ArbitraryNodeToClientVersions
                                               -> ArbitraryNodeToClientVersions
                                               -> Property
prop_channel_simultaneous_open_NodeToClient_ST
    (ArbitraryNodeToClientVersions clientVersions)
    (ArbitraryNodeToClientVersions serverVersions) =
  runSimOrThrow $ prop_channel_simultaneous_open
                    createConnectedChannels
                    (codecHandshake nodeToClientVersionCodec)
                    (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
                    clientVersions
                    serverVersions


prop_channel_simultaneous_open_NodeToClient_IO :: ArbitraryNodeToClientVersions
                                               -> ArbitraryNodeToClientVersions
                                               -> Property
prop_channel_simultaneous_open_NodeToClient_IO
    (ArbitraryNodeToClientVersions clientVersions)
    (ArbitraryNodeToClientVersions serverVersions) =
  ioProperty $ prop_channel_simultaneous_open
                    createConnectedChannels
                    (codecHandshake nodeToClientVersionCodec)
                    (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
                    clientVersions
                    serverVersions


prop_channel_simultaneous_open_sim
    :: forall vNumber vData m.
       ( Alternative (STM m)
       , MonadAsync       m
       , MonadCatch       m
       , MonadDelay       m
       , MonadFork        m
       , MonadLabelledSTM m
       , MonadMask        m
       , MonadMonotonicTime m
       , MonadST          m
       , MonadThrow  (STM m)
       , MonadTime        m
       , MonadTimer       m
       , Acceptable vData
       , Ord vNumber
       )
    => Codec (Handshake vNumber CBOR.Term)
              CBOR.DeserialiseFailure m ByteString
    -> VersionDataCodec CBOR.Term vNumber vData
    -> Versions vNumber vData Bool
    -> Versions vNumber vData Bool
    -> m Property
prop_channel_simultaneous_open_sim codec versionDataCodec
                                   clientVersions serverVersions =
    let attenuation = noAttenuation { biConnectionDelay = 1 } in
    withSnocket nullTracer
                attenuation
                Map.empty
              $ \sn _ -> do
      let addr, addr' :: TestAddress Int
          addr  = Snocket.TestAddress 1
          addr' = Snocket.TestAddress 2
      -- listening snockets
      bracket (Snocket.open  sn Snocket.TestFamily)
              (Snocket.close sn) $ \fdLst ->
        bracket (Snocket.open  sn Snocket.TestFamily)
                (Snocket.close sn) $ \fdLst' -> do
          Snocket.bind sn fdLst  addr
          Snocket.bind sn fdLst' addr'
          Snocket.listen sn fdLst
          Snocket.listen sn fdLst'
          -- connection snockets
          bracket ((,) <$> Snocket.open sn Snocket.TestFamily
                       <*> Snocket.open sn Snocket.TestFamily
                  )
                  (\(fdConn, fdConn') ->
                      -- we need concurrently close both sockets: they need to
                      -- communicate between each other while they close.
                      Snocket.close sn fdConn
                      `concurrently_`
                      Snocket.close sn fdConn'
                  ) $ \(fdConn, fdConn') -> do
            Snocket.bind sn fdConn  addr
            Snocket.bind sn fdConn' addr'
            concurrently_
              (Snocket.connect sn fdConn  addr')
              (Snocket.connect sn fdConn' addr)
            bearer  <- Mx.getBearer makeFDBearer
                        1
                        fdConn
                        Nothing
            bearer' <- Mx.getBearer makeFDBearer
                        1
                        fdConn'
                        Nothing
            let chann  = bearerAsChannel nullTracer bearer  (MiniProtocolNum 0) InitiatorDir
                chann' = bearerAsChannel nullTracer bearer' (MiniProtocolNum 0) InitiatorDir
            res <- prop_channel_simultaneous_open
              (pure (chann, chann'))
              codec
              versionDataCodec
              clientVersions
              serverVersions
            return res


prop_channel_simultaneous_open_SimNet :: ArbitraryVersions
                                                        -> Property
prop_channel_simultaneous_open_SimNet
  (ArbitraryVersions clientVersions serverVersions) =
    runSimOrThrow $ prop_channel_simultaneous_open_sim
      versionNumberHandshakeCodec
      (cborTermVersionDataCodec dataCodecCBORTerm)
      clientVersions
      serverVersions

prop_channel_simultaneous_open_NodeToNode_SimNet :: ArbitraryNodeToNodeVersions
                                                 -> ArbitraryNodeToNodeVersions
                                                 -> Property
prop_channel_simultaneous_open_NodeToNode_SimNet
    (ArbitraryNodeToNodeVersions clientVersions)
    (ArbitraryNodeToNodeVersions serverVersions) =
      runSimOrThrow $ prop_channel_simultaneous_open_sim
        (codecHandshake nodeToNodeVersionCodec)
        (cborTermVersionDataCodec (fmap transformNodeToNodeVersionData nodeToNodeCodecCBORTerm))
        clientVersions
        serverVersions

prop_channel_simultaneous_open_NodeToClient_SimNet :: ArbitraryNodeToClientVersions
                                                   -> ArbitraryNodeToClientVersions
                                                   -> Property
prop_channel_simultaneous_open_NodeToClient_SimNet
    (ArbitraryNodeToClientVersions clientVersions)
    (ArbitraryNodeToClientVersions serverVersions) =
      runSimOrThrow $ prop_channel_simultaneous_open_sim
        (codecHandshake nodeToClientVersionCodec)
        (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
        clientVersions
        serverVersions



--
-- Codec tests
--

instance Eq (AnyMessage (Handshake VersionNumber CBOR.Term)) where
  AnyMessage (MsgProposeVersions vs) == AnyMessage (MsgProposeVersions vs')
    = vs == vs'

  AnyMessage (MsgReplyVersions vs) == AnyMessage (MsgReplyVersions vs')
    = vs == vs'

  AnyMessage (MsgAcceptVersion vNumber vParams) == AnyMessage (MsgAcceptVersion vNumber' vParams')
    = vNumber == vNumber' && vParams == vParams'

  AnyMessage (MsgRefuse vReason) == AnyMessage (MsgRefuse vReason')
    = vReason == vReason'

  _ == _ = False

instance Arbitrary (AnyMessage (Handshake VersionNumber CBOR.Term)) where
  arbitrary = oneof
    [     AnyMessage
        . MsgProposeVersions
        . Map.mapWithKey (\v -> encodeTerm (dataCodecCBORTerm v) . versionData)
        . getVersions
      <$> genVersions

    ,     AnyMessage
        . MsgReplyVersions
        . Map.mapWithKey (\v -> encodeTerm (dataCodecCBORTerm v) . versionData)
        . getVersions
      <$> genVersions

    ,     AnyMessage
        . uncurry MsgAcceptVersion
      <$> genValidVersion'

    ,     AnyMessage
        . MsgRefuse
        . runArbitraryRefuseReason
      <$> arbitrary
    ]
    where
      genValidVersion' :: Gen (VersionNumber, CBOR.Term)
      genValidVersion' = do
        vn <- arbitrary
        Version _ vData <- genValidVersion vn
        pure (vn, encodeTerm (dataCodecCBORTerm vn) vData)


newtype ArbitraryRefuseReason = ArbitraryRefuseReason {
    runArbitraryRefuseReason :: RefuseReason VersionNumber
  }
  deriving (Eq, Show)


instance Arbitrary ArbitraryRefuseReason where
    arbitrary = ArbitraryRefuseReason <$> oneof
      [ VersionMismatch
          <$> arbitrary
          -- this argument is not supposed to be sent, only received:
          <*> pure []
      , HandshakeDecodeError <$> arbitrary <*> arbitraryText
      , Refused <$> arbitrary <*> arbitraryText
      ]
      where
        arbitraryText = T.pack <$> arbitrary


--
--  TODO: these tests should be moved to 'ouroboros-network-framework'
--

-- TODO: we are not testing the cases where we decode version numbers that we do
-- not know about.
prop_codec_RefuseReason
  :: ArbitraryRefuseReason
  -> Bool
prop_codec_RefuseReason (ArbitraryRefuseReason vReason) =
  case CBOR.deserialiseFromBytes
        (decodeRefuseReason versionNumberCodec)
        (CBOR.toLazyByteString $ encodeRefuseReason versionNumberCodec vReason) of
    Left _                  -> False
    Right (bytes, vReason') -> BL.null bytes && vReason' == vReason

prop_codec_Handshake
  :: AnyMessage (Handshake VersionNumber CBOR.Term)
  -> Bool
prop_codec_Handshake msg =
  runSimOrThrow (prop_codecM (codecHandshake versionNumberCodec) msg)

prop_codec_splits2_Handshake
  :: AnyMessage (Handshake VersionNumber CBOR.Term)
  -> Bool
prop_codec_splits2_Handshake msg =
  runSimOrThrow (prop_codec_splitsM splits2 (codecHandshake versionNumberCodec) msg)

prop_codec_splits3_Handshake
  :: AnyMessage (Handshake VersionNumber CBOR.Term)
  -> Bool
prop_codec_splits3_Handshake msg =
  runSimOrThrow (prop_codec_splitsM splits3 (codecHandshake versionNumberCodec) msg)

prop_codec_cbor
  :: AnyMessage (Handshake VersionNumber CBOR.Term)
  -> Bool
prop_codec_cbor msg =
  runSimOrThrow (prop_codec_cborM (codecHandshake versionNumberCodec) msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: AnyMessage (Handshake VersionNumber CBOR.Term)
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding (codecHandshake versionNumberCodec)
