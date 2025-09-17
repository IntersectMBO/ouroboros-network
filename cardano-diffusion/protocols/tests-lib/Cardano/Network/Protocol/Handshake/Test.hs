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

module Cardano.Network.Protocol.Handshake.Test where

import Data.ByteString.Lazy (ByteString)
import Data.Map qualified as Map
import Data.Text (Text)

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (nullTracer)

import Network.TypedProtocol.Codec

import Cardano.Network.NodeToClient.Version as NTC
import Cardano.Network.NodeToNode.Version as NTN

import Ouroboros.Network.Channel
import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.Driver.Simple (runConnectedPeers)
import Ouroboros.Network.Magic
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))

import Ouroboros.Network.Protocol.Handshake.Client
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Server
import Ouroboros.Network.Protocol.Handshake.Test hiding (tests)
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.Handshake.Version

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol"
    [ testGroup "Handshake"
      [ testGroup "NodeToNode"
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
      ]
    ]




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
prop_peerSharing_symmetric ::
                           ( MonadAsync m
                           , MonadCatch m
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
