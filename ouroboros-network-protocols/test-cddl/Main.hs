{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import           Control.Monad.Except

import qualified Codec.CBOR.Read as CBOR
import           Codec.CBOR.Term (Term (..))
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise.Class (Serialise)
import qualified Codec.Serialise.Class as Serialise
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR


import           Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL.Char8
import           Data.List (sortOn)
import qualified Data.Map as Map
import           Data.Ord (Down (..))
import qualified Data.Text as Text
import           Data.Word (Word16)

import           System.Directory (doesDirectoryExist)
import           System.Exit (ExitCode (..))
import           System.FilePath
import           System.IO (hClose)
import           System.IO.Temp (withTempFile)
import           System.Process.ByteString.Lazy

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Core

import           Ouroboros.Network.Block (Point, SlotNo, Tip, decodeTip,
                     encodeTip, unwrapCBORinCBOR, wrapCBORinCBOR)
import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Magic
import           Ouroboros.Network.Mock.ConcreteBlock (Block, BlockHeader (..))

import           Ouroboros.Network.NodeToClient.Version (NodeToClientVersion,
                     NodeToClientVersionData (..), nodeToClientCodecCBORTerm)
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..),
                     NodeToNodeVersion (..), NodeToNodeVersionData (..),
                     nodeToNodeCodecCBORTerm)
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (PortNumber)
import           Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetch)
import           Ouroboros.Network.Protocol.BlockFetch.Test ()
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import qualified Ouroboros.Network.Protocol.BlockFetch.Type as BlockFetch
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Test ()
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import           Ouroboros.Network.Protocol.Handshake.Codec
                     (nodeToClientHandshakeCodec, nodeToNodeHandshakeCodec)
import           Ouroboros.Network.Protocol.Handshake.Test (VersionNumber,
                     versionNumberHandshakeCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import qualified Ouroboros.Network.Protocol.Handshake.Type as Handshake
import           Ouroboros.Network.Protocol.KeepAlive.Codec (codecKeepAlive_v2)
import           Ouroboros.Network.Protocol.KeepAlive.Test ()
import           Ouroboros.Network.Protocol.KeepAlive.Type (KeepAlive)
import qualified Ouroboros.Network.Protocol.KeepAlive.Type as KeepAlive
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Test as LocalStateQuery
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import           Ouroboros.Network.Protocol.LocalTxMonitor.Codec
                     (codecLocalTxMonitor)
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Test as LocalTxMonitor
import           Ouroboros.Network.Protocol.LocalTxMonitor.Type (LocalTxMonitor)
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Type as LocalTxMonitor
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
                     (codecLocalTxSubmission)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Test as LocalTxSubmission
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
                     (LocalTxSubmission)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSubmission
import           Ouroboros.Network.Protocol.TxSubmission2.Codec
                     (codecTxSubmission2)
import           Ouroboros.Network.Protocol.TxSubmission2.Test (Tx, TxId)
import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TxSubmission2

import           Network.Socket (SockAddr (..))
import           Ouroboros.Network.PeerSelection.PeerSharing
                     (PeerSharing (..), decodeRemoteAddress,
                     encodeRemoteAddress)
import           Ouroboros.Network.Protocol.PeerSharing.Codec (codecPeerSharing)
import           Ouroboros.Network.Protocol.PeerSharing.Test ()
import           Ouroboros.Network.Protocol.PeerSharing.Type
                     (ClientHasAgency (TokIdle), ServerHasAgency (..))
import qualified Ouroboros.Network.Protocol.PeerSharing.Type as PeerSharing
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()
import           Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (QuickCheckMaxSize (..), testProperty)

-- | The main program, it requires both
--
-- - 'cddl' program
-- - 'diag2cbor.rb' script
--
-- to be installed in the '$PATH'.
--
main :: IO ()
main = do
  cddlSpecs <- readCDDLSpecs
  defaultMain (tests cddlSpecs)

tests :: CDDLSpecs -> TestTree
tests CDDLSpecs { cddlChainSync
                , cddlBlockFetch
                , cddlLocalTxSubmission
                , cddlLocalTxMonitor
                , cddlTxSubmission2
                , cddlKeepAlive
                , cddlLocalStateQuery
                , cddlHandshakeNodeToNodeV7To10
                , cddlHandshakeNodeToNodeV11
                , cddlHandshakeNodeToClient
                , cddlPeerSharing
                } =
  adjustOption (const $ QuickCheckMaxSize 10) $
  testGroup "cddl"
    [ testGroup "encoding"
      -- validate encoding against a specification
      [ testProperty "NodeToNode.Handshake V7 to V10"
                                         (prop_encodeHandshakeNodeToNodeV7To10
                                               cddlHandshakeNodeToNodeV7To10)
      , testProperty "NodeToNode.Handshake V11"
                                         (prop_encodeHandshakeNodeToNodeV11
                                               cddlHandshakeNodeToNodeV11)
      , -- If this fails whilst adding a new node-to-client version, ensure that
        -- all the necessary changes are included:
        --
        -- + 'NodeVersion' data type
        -- + 'NodeToClientVersion' data type
        -- + 'versionNumber' in the 'handshake-node-to-client.cddl' file
        testProperty "NodeToClient.Handshake"
                                         (prop_encodeHandshakeNodeToClient
                                               cddlHandshakeNodeToClient)
      , testProperty "ChainSync"         (prop_encodeChainSync
                                               cddlChainSync)
      , testProperty "BlockFetch"        (prop_encodeBlockFetch
                                               cddlBlockFetch)
      , testProperty "TxSubmission2"     (prop_encodeTxSubmission2
                                               cddlTxSubmission2)
      , testProperty "KeepAlive"         (prop_encodeKeepAlive
                                               cddlKeepAlive)
      , testProperty "LocalTxSubmission" (prop_encodeLocalTxSubmission
                                               cddlLocalTxSubmission)
      , testProperty "LocalTxMonitor" (prop_encodeLocalTxMonitor
                                               cddlLocalTxMonitor)
      , testProperty "LocalStateQuery"   (prop_encodeLocalStateQuery
                                               cddlLocalStateQuery)
      , testProperty "PeerSharing "      (prop_encodePeerSharing
                                               cddlPeerSharing)
      ]
    , testGroup "decoder"
      -- validate decoder by generating messages from the specification
      [ testCase "NodeToNode.Handshake V7 to V10"
                                     (unit_decodeHandshakeNodeToNode
                                           cddlHandshakeNodeToNodeV7To10)
      , testCase "NodeToNode.Handshake V11"
                                     (unit_decodeHandshakeNodeToNode
                                           cddlHandshakeNodeToNodeV11)
      , testCase "NodeToClient.Handshake"
                                     (unit_decodeHandshakeNodeToClient
                                           cddlHandshakeNodeToClient)
      , testCase "ChainSync"         (unit_decodeChainSync
                                           cddlChainSync)
      , testCase "BlockFetch"        (unit_decodeBlockFetch
                                           cddlBlockFetch)
      , testCase "TxSubmission2"     (unit_decodeTxSubmission2
                                           cddlTxSubmission2)
      , testCase "KeepAlive"         (unit_decodeKeepAlive
                                           cddlKeepAlive)
      , testCase "LocalTxSubmission" (unit_decodeLocalTxSubmission
                                           cddlLocalTxSubmission)
      , testCase "LocalTxMonitor" (unit_decodeLocalTxMonitor
                                           cddlLocalTxMonitor)
      , testCase "LocalStateQuery"   (unit_decodeLocalStateQuery
                                           cddlLocalStateQuery)
      , testCase "PeerSharing"       (unit_decodePeerSharing
                                           cddlPeerSharing)
      ]
    ]

-- | A 'CDDL' specifcation for a protocol 'ps'.
--
newtype CDDLSpec ps = CDDLSpec BL.ByteString

data CDDLSpecs = CDDLSpecs {
    cddlHandshakeNodeToClient     :: CDDLSpec (Handshake NodeToClientVersion CBOR.Term),
    cddlHandshakeNodeToNodeV7To10 :: CDDLSpec (Handshake NodeToNodeVersion   CBOR.Term),
    cddlHandshakeNodeToNodeV11    :: CDDLSpec (Handshake NodeToNodeVersion   CBOR.Term),
    cddlChainSync                 :: CDDLSpec (ChainSync
                                                 BlockHeader
                                                 (Point BlockHeader)
                                                 (Tip BlockHeader)),
    cddlBlockFetch                :: CDDLSpec (BlockFetch Block (Point Block)),
    cddlTxSubmission2             :: CDDLSpec (TxSubmission2 TxId Tx),
    cddlKeepAlive                 :: CDDLSpec KeepAlive,
    cddlLocalTxSubmission         :: CDDLSpec (LocalTxSubmission
                                                 LocalTxSubmission.Tx
                                                 LocalTxSubmission.Reject),
    cddlLocalTxMonitor            :: CDDLSpec (LocalTxMonitor TxId Tx SlotNo),
    cddlLocalStateQuery           :: CDDLSpec (LocalStateQuery
                                                 Block (Point Block)
                                                 LocalStateQuery.Query),
    cddlPeerSharing               :: CDDLSpec (PeerSharing.PeerSharing SockAddr)
  }


readCDDLSpecs :: IO CDDLSpecs
readCDDLSpecs = do
    dir <- bool (                                       "test-cddl" </> "specs") -- False
                ("ouroboros-network-protocols-test" </> "test-cddl" </> "specs") -- True
       <$> doesDirectoryExist "ouroboros-network-protocols-test"
    common                <- BL.readFile (dir </> "common.cddl")
    handshakeNodeToClient <- BL.readFile (dir </> "handshake-node-to-client.cddl")
    handshakeNodeToNodeV7To10 <- BL.readFile (dir </> "handshake-node-to-node.cddl")
    handshakeNodeToNodeV11    <- BL.readFile (dir </> "handshake-node-to-node-v11.cddl")
    chainSync             <- BL.readFile (dir </> "chain-sync.cddl")
    blockFetch            <- BL.readFile (dir </> "block-fetch.cddl")
    txSubmission2         <- BL.readFile (dir </> "tx-submission2.cddl")
    keepAlive             <- BL.readFile (dir </> "keep-alive.cddl")
    localTxSubmission     <- BL.readFile (dir </> "local-tx-submission.cddl")
    localTxMonitor        <- BL.readFile (dir </> "local-tx-monitor.cddl")
    localStateQuery       <- BL.readFile (dir </> "local-state-query.cddl")
    peerSharing           <- BL.readFile (dir </> "peer-sharing.cddl")
    -- append common definitions; they must be appended since the first
    -- definition is the entry point for a cddl spec.
    return CDDLSpecs {
        cddlHandshakeNodeToClient = CDDLSpec $ handshakeNodeToClient,
        cddlHandshakeNodeToNodeV7To10 = CDDLSpec $ handshakeNodeToNodeV7To10,
        cddlHandshakeNodeToNodeV11    = CDDLSpec $ handshakeNodeToNodeV11,
        cddlChainSync             = CDDLSpec $ chainSync
                                            <> common,
        cddlBlockFetch            = CDDLSpec $ blockFetch
                                            <> common,
        cddlTxSubmission2         = CDDLSpec $ txSubmission2
                                            <> common,
        cddlKeepAlive             = CDDLSpec keepAlive,
        cddlLocalTxSubmission     = CDDLSpec $ localTxSubmission
                                            <> common,
        cddlLocalTxMonitor        = CDDLSpec $ localTxMonitor
                                            <> common,
        cddlLocalStateQuery       = CDDLSpec $ localStateQuery
                                            <> common,
        cddlPeerSharing           = CDDLSpec $ peerSharing
                                            <> common
      }

--
-- Mini-Protocol Codecs
--

chainSyncCodec :: Codec (ChainSync BlockHeader (Point BlockHeader) (Tip BlockHeader))
                        CBOR.DeserialiseFailure IO BL.ByteString
chainSyncCodec =
    codecChainSync
      (wrapCBORinCBOR Serialise.encode)
      (unwrapCBORinCBOR (const <$> Serialise.decode))
      Serialise.encode
      Serialise.decode
      (encodeTip Serialise.encode)
      (decodeTip Serialise.decode)


blockFetchCodec :: Codec (BlockFetch Block (Point Block))
                         CBOR.DeserialiseFailure IO BL.ByteString
blockFetchCodec =
    codecBlockFetch
      (wrapCBORinCBOR Serialise.encode)
      (unwrapCBORinCBOR (const <$> Serialise.decode))
      Serialise.encode
      Serialise.decode


txSubmissionCodec2 :: Codec (TxSubmission2 TxId Tx)
                            CBOR.DeserialiseFailure IO BL.ByteString
txSubmissionCodec2 =
    codecTxSubmission2
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode


localTxSubmissionCodec :: Codec (LocalTxSubmission LocalTxSubmission.Tx LocalTxSubmission.Reject)
                                CBOR.DeserialiseFailure IO BL.ByteString
localTxSubmissionCodec =
    codecLocalTxSubmission
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode


localTxMonitorCodec :: Codec (LocalTxMonitor TxId Tx SlotNo)
                                CBOR.DeserialiseFailure IO BL.ByteString
localTxMonitorCodec =
    codecLocalTxMonitor
      Serialise.encode Serialise.decode
      Serialise.encode Serialise.decode
      Serialise.encode Serialise.decode


localStateQueryCodec :: Codec (LocalStateQuery Block (Point Block) LocalStateQuery.Query)
                              CBOR.DeserialiseFailure IO BL.ByteString
localStateQueryCodec =
    LocalStateQuery.codec


--
-- Test encodings
--


-- | Validate mini-protocol codec against its cddl specification.
--
validateEncoder
    :: ( forall (st :: ps). Show (ClientHasAgency st)
       , forall (st :: ps). Show (ServerHasAgency st)
       , forall (st :: ps) (st' :: ps). Show (Message ps st st')
       )
    => CDDLSpec ps
    -> Codec ps CBOR.DeserialiseFailure IO BL.ByteString
    -> AnyMessageAndAgency ps
    -> Property
validateEncoder spec
                Codec { encode }
                anyMsg@(AnyMessageAndAgency agency msg) =
    counterexample (show anyMsg) $
    counterexample (show terms) $
    ioProperty $
      either (\err -> counterexample err False)
             (\_   -> property True)
      <$> validateCBOR spec blob
  where
    blob  = encode agency msg
    terms = CBOR.deserialiseFromBytes CBOR.decodeTerm blob


-- | Match encoded cbor against cddl specifiction.
--
validateCBOR :: CDDLSpec ps
             -> BL.ByteString
             -> IO (Either String ())
validateCBOR (CDDLSpec spec) blob =
    withTemporaryFile spec $ \fileName -> do
      res <- unpackResult $
               readProcessWithExitCode
                 "cddl"
                 [fileName, "validate", "-"]
                 blob
      return $ case res of
        Left err -> Left err
        Right _  -> Right ()


-- | Newtype for testing Handshake CDDL Specification from version 7 to
-- version 10. After version 10 (i.e. version 11) a new extra parameter is
-- added and we need a new CDDL specification (see
-- specs/handshake-node-to-node-v11.cddl).
--
newtype NtNHandshakeV7To10 =
  NtNHandshakeV7To10
    (AnyMessageAndAgency (Handshake NodeToNodeVersion CBOR.Term))
    deriving Show

-- | Newtype for testing Handshake CDDL Specification from version 11 onward.
--
newtype NtNHandshakeV11 =
  NtNHandshakeV11
    (AnyMessageAndAgency (Handshake NodeToNodeVersion CBOR.Term))
    deriving Show

genNtNHandshake :: Gen NodeToNodeVersion
                -> Gen (AnyMessageAndAgency (Handshake NodeToNodeVersion Term))
genNtNHandshake genVersion = oneof
    [     AnyMessageAndAgency (ClientAgency Handshake.TokPropose)
        . Handshake.MsgProposeVersions
        . Map.fromList
        . map (\(v, d) -> (v, encodeTerm (nodeToNodeCodecCBORTerm v) d))
      <$> listOf ((,) <$> genVersion <*> genData)

    ,     AnyMessageAndAgency (ServerAgency Handshake.TokConfirm)
        . uncurry Handshake.MsgAcceptVersion
        . (\(v, d) -> (v, encodeTerm (nodeToNodeCodecCBORTerm v) d))
      <$> ((,) <$> genVersion <*> genData)

    ,     AnyMessageAndAgency (ServerAgency Handshake.TokConfirm)
        . Handshake.MsgRefuse
      <$> genRefuseReason
    ]
  where
    genData :: Gen NodeToNodeVersionData
    genData = NodeToNodeVersionData
          <$> (NetworkMagic <$> arbitrary)
          <*> oneof
                [ pure InitiatorOnlyDiffusionMode
                , pure InitiatorAndResponderDiffusionMode
                ]
          <*> oneof
                [ pure NoPeerSharing
                , pure PeerSharingPrivate
                , pure PeerSharingPublic
                ]

    genRefuseReason :: Gen (Handshake.RefuseReason NodeToNodeVersion)
    genRefuseReason = oneof
      [ Handshake.VersionMismatch
          <$> listOf genVersion
          <*> pure []
      , Handshake.HandshakeDecodeError
          <$> genVersion
          <*> (Text.pack <$> arbitrary)
      , Handshake.Refused
          <$> genVersion
          <*> (Text.pack <$> arbitrary)
      ]

-- TODO: issue 4294
instance Arbitrary NtNHandshakeV7To10 where
  arbitrary = do
    let genVersion = elements [minBound .. NodeToNodeV_10]
    NtNHandshakeV7To10 <$> genNtNHandshake genVersion

instance Arbitrary NtNHandshakeV11 where
  arbitrary = do
    let genVersion = elements [NodeToNodeV_11 .. maxBound]
    NtNHandshakeV11 <$> genNtNHandshake genVersion


prop_encodeHandshakeNodeToNodeV7To10
    :: CDDLSpec            (Handshake NodeToNodeVersion CBOR.Term)
    -> NtNHandshakeV7To10
    -> Property
prop_encodeHandshakeNodeToNodeV7To10 spec (NtNHandshakeV7To10 x) =
  validateEncoder spec nodeToNodeHandshakeCodec x

prop_encodeHandshakeNodeToNodeV11
    :: CDDLSpec            (Handshake NodeToNodeVersion CBOR.Term)
    -> NtNHandshakeV11
    -> Property
prop_encodeHandshakeNodeToNodeV11 spec (NtNHandshakeV11 x) =
  validateEncoder spec nodeToNodeHandshakeCodec x

-- TODO: add our regular tests for `Handshake NodeToClientVerision CBOR.Term`
-- codec.
--
instance Arbitrary (AnyMessageAndAgency (Handshake NodeToClientVersion CBOR.Term)) where
    arbitrary = oneof
        [     AnyMessageAndAgency (ClientAgency Handshake.TokPropose)
            . Handshake.MsgProposeVersions
            . Map.fromList
            . map (\(v, d) -> (v, encodeTerm (nodeToClientCodecCBORTerm v) d))
          <$> listOf ((,) <$> genVersion <*> genData)

        ,     AnyMessageAndAgency (ServerAgency Handshake.TokConfirm)
            . uncurry Handshake.MsgAcceptVersion
            . (\(v, d) -> (v, encodeTerm (nodeToClientCodecCBORTerm v) d))
          <$> ((,) <$> genVersion <*> genData)

        ,     AnyMessageAndAgency (ServerAgency Handshake.TokConfirm)
            . Handshake.MsgRefuse
          <$> genRefuseReason
        ]
      where
        genVersion :: Gen NodeToClientVersion
        genVersion = elements [minBound .. maxBound]

        genData :: Gen NodeToClientVersionData
        genData = NodeToClientVersionData
              <$> (NetworkMagic <$> arbitrary)

        genRefuseReason :: Gen (Handshake.RefuseReason NodeToClientVersion)
        genRefuseReason = oneof
          [ Handshake.VersionMismatch
              <$> listOf genVersion
              <*> pure []
          , Handshake.HandshakeDecodeError
              <$> genVersion
              <*> (Text.pack <$> arbitrary)
          , Handshake.Refused
              <$> genVersion
              <*> (Text.pack <$> arbitrary)
          ]


prop_encodeHandshakeNodeToClient
    :: CDDLSpec            (Handshake NodeToClientVersion CBOR.Term)
    -> AnyMessageAndAgency (Handshake NodeToClientVersion CBOR.Term)
    -> Property
prop_encodeHandshakeNodeToClient spec = validateEncoder spec nodeToClientHandshakeCodec


prop_encodeChainSync
    :: CDDLSpec            (ChainSync BlockHeader
                                      (Point BlockHeader)
                                      (Tip BlockHeader))
    -> AnyMessageAndAgency (ChainSync BlockHeader
                                      (Point BlockHeader)
                                      (Tip BlockHeader))
    -> Property
prop_encodeChainSync spec = validateEncoder spec chainSyncCodec


prop_encodeBlockFetch
    :: CDDLSpec            (BlockFetch Block (Point Block))
    -> AnyMessageAndAgency (BlockFetch Block (Point Block))
    -> Property
prop_encodeBlockFetch spec = validateEncoder spec blockFetchCodec


prop_encodeTxSubmission2
    :: CDDLSpec            (TxSubmission2 TxId Tx)
    -> AnyMessageAndAgency (TxSubmission2 TxId Tx)
    -> Property
prop_encodeTxSubmission2 spec = validateEncoder spec txSubmissionCodec2


prop_encodeKeepAlive
    :: CDDLSpec            KeepAlive
    -> AnyMessageAndAgency KeepAlive
    -> Property
prop_encodeKeepAlive spec = validateEncoder spec codecKeepAlive_v2


prop_encodeLocalTxSubmission
    :: CDDLSpec            (LocalTxSubmission LocalTxSubmission.Tx
                                              LocalTxSubmission.Reject)
    -> AnyMessageAndAgency (LocalTxSubmission LocalTxSubmission.Tx
                                              LocalTxSubmission.Reject)
    -> Property
prop_encodeLocalTxSubmission spec = validateEncoder spec localTxSubmissionCodec

prop_encodeLocalTxMonitor
    :: CDDLSpec            (LocalTxMonitor TxId Tx SlotNo)
    -> AnyMessageAndAgency (LocalTxMonitor TxId Tx SlotNo)
    -> Property
prop_encodeLocalTxMonitor spec = validateEncoder spec localTxMonitorCodec

prop_encodeLocalStateQuery
    :: CDDLSpec            (LocalStateQuery Block (Point Block) LocalStateQuery.Query)
    -> AnyMessageAndAgency (LocalStateQuery Block (Point Block) LocalStateQuery.Query)
    -> Property
prop_encodeLocalStateQuery spec = validateEncoder spec localStateQueryCodec

instance Arbitrary PortNumber where
  arbitrary = fromIntegral @Word16 <$> arbitrary

instance Arbitrary SockAddr where
  arbitrary = oneof [ SockAddrInet <$> arbitrary
                                   <*> arbitrary
                    , SockAddrInet6 <$> arbitrary
                                    <*> arbitrary
                                    <*> arbitrary
                                    <*> arbitrary
                    ]

prop_encodePeerSharing
    :: CDDLSpec            (PeerSharing.PeerSharing SockAddr)
    -> AnyMessageAndAgency (PeerSharing.PeerSharing SockAddr)
    -> Property
prop_encodePeerSharing spec =
  validateEncoder spec (codecPeerSharing encodeRemoteAddress decodeRemoteAddress)


--
-- Test decoders
--


data SomeAgency ps where
    SomeAgency :: PeerHasAgency (pr :: PeerRole) (st :: ps)
               -> SomeAgency ps


-- | Generate valid encoded messages from a specification using `cddl generate`
-- (and encoded with `diag2cbor.rb`) and check that we can decode it at one of
-- the given agencies.
--
validateDecoder :: Maybe (CBOR.Term -> CBOR.Term)
                -- ^ transform a generated term
                -> CDDLSpec ps
                -> Codec ps CBOR.DeserialiseFailure IO BL.ByteString
                -> [SomeAgency ps]
                -> Int
                -> Assertion
validateDecoder transform (CDDLSpec spec) codec stoks rounds = do
    eterms <- runExceptT $ generateCBORFromSpec spec rounds
    case eterms of
      Left err -> assertFailure err
      Right terms ->
        forM_ terms $ \(generated_term, encoded_term) -> do
          let encoded_term' = case transform of
                 Nothing -> encoded_term
                 Just tr -> case CBOR.deserialiseFromBytes CBOR.decodeTerm encoded_term of
                   Right (rest, term)  | BL.null rest
                                      -> CBOR.toLazyByteString (CBOR.encodeTerm (tr term))
                   Right _            -> error   "validateDecoder: trailing bytes"
                   Left err           -> error $ "validateDecoder: decoding error: "
                                              ++ show err
              Right (_, decoded_term) =
                CBOR.deserialiseFromBytes CBOR.decodeTerm encoded_term'
          res <- decodeMsg codec stoks encoded_term'
          case res of
            Just errs -> assertFailure $ concat
              [ "decoding failures:\n"
              , unlines (map show errs)
              , "while decoding:\n"
              , show decoded_term
              , "\n"
              , BL.Char8.unpack generated_term
              ]
            Nothing -> return ()


generateCBORFromSpec :: BL.ByteString -> Int -> ExceptT String IO [(BL.ByteString, BL.ByteString)]
generateCBORFromSpec spec rounds = do
    terms <-
      ExceptT $ withTemporaryFile spec $ \filePath ->
        unpackResult $
          readProcessWithExitCode
            "cddl"
            [filePath, "generate", show rounds]
            BL.empty
    traverse (\bs -> (bs,) <$> diagToBytes bs) (BL.Char8.lines terms)
  where
    diagToBytes :: BL.ByteString -> ExceptT String IO BL.ByteString
    diagToBytes = ExceptT
                . unpackResult
                . readProcessWithExitCode "diag2cbor.rb" ["-"]


-- | Try decode at all given agencies.  If one suceeds return
-- 'Nothing' otherwise return all 'DeserialiseFailure's.
--
decodeMsg :: forall ps.
             Codec ps CBOR.DeserialiseFailure IO BL.ByteString
          -> [SomeAgency ps]
          -- ^ list of all gencies to try
          -> BL.ByteString
          -> IO (Maybe [CBOR.DeserialiseFailure])
decodeMsg codec stoks bs =
    -- sequence [Nothing, ...] = Nothing
    fmap (sequence :: [Maybe CBOR.DeserialiseFailure] -> Maybe [CBOR.DeserialiseFailure]) $
    forM stoks $ \(SomeAgency stok) -> do
        decoder <- decode codec stok
        res <- runDecoder [bs] decoder
        return $ case res of
          Left err -> Just (err)
          Right {} -> Nothing


unit_decodeHandshakeNodeToNode
    :: CDDLSpec (Handshake NodeToNodeVersion CBOR.Term)
    -> Assertion
unit_decodeHandshakeNodeToNode spec =
    validateDecoder (Just handshakeFix)
      spec nodeToNodeHandshakeCodec
      [ SomeAgency $ ClientAgency Handshake.TokPropose
      , SomeAgency $ ServerAgency Handshake.TokConfirm
      ]
      100


unit_decodeHandshakeNodeToClient
    :: CDDLSpec (Handshake NodeToClientVersion CBOR.Term)
    -> Assertion
unit_decodeHandshakeNodeToClient spec =
    validateDecoder (Just handshakeFix)
      spec nodeToClientHandshakeCodec
      [ SomeAgency $ ClientAgency Handshake.TokPropose
      , SomeAgency $ ServerAgency Handshake.TokConfirm
      ]
      100


unit_decodeChainSync
    :: CDDLSpec (ChainSync BlockHeader (Point BlockHeader) (Tip BlockHeader))
    -> Assertion
unit_decodeChainSync spec =
    validateDecoder Nothing
      spec chainSyncCodec
      [ SomeAgency $ ClientAgency ChainSync.TokIdle
      , SomeAgency $ ServerAgency (ChainSync.TokNext ChainSync.TokCanAwait)
      , SomeAgency $ ServerAgency (ChainSync.TokNext ChainSync.TokMustReply)
      , SomeAgency $ ServerAgency (ChainSync.TokIntersect)
      ]
      100


unit_decodeBlockFetch
    :: CDDLSpec (BlockFetch Block (Point Block))
    -> Assertion
unit_decodeBlockFetch spec =
    validateDecoder Nothing
      spec blockFetchCodec
      [ SomeAgency $ ClientAgency BlockFetch.TokIdle
      , SomeAgency $ ServerAgency BlockFetch.TokBusy
      , SomeAgency $ ServerAgency BlockFetch.TokStreaming
      ]
      100


unit_decodeTxSubmission2
    :: CDDLSpec (TxSubmission2 TxId Tx)
    -> Assertion
unit_decodeTxSubmission2 spec =
    validateDecoder (Just txSubmissionFix)
      spec txSubmissionCodec2
      [ SomeAgency
        $ ClientAgency TxSubmission2.TokInit
      , SomeAgency
        $ ClientAgency
        $ TxSubmission2.TokTxIds TxSubmission2.TokBlocking
      , SomeAgency
        $ ClientAgency
        $ TxSubmission2.TokTxIds TxSubmission2.TokNonBlocking
      , SomeAgency
        $ ClientAgency
        $ TxSubmission2.TokTxs
      , SomeAgency
        $ ServerAgency
        $ TxSubmission2.TokIdle
      ]
      100


unit_decodeKeepAlive
    :: CDDLSpec KeepAlive
    -> Assertion
unit_decodeKeepAlive spec =
    validateDecoder Nothing
      spec codecKeepAlive_v2
      [ SomeAgency $ ClientAgency KeepAlive.TokClient
      , SomeAgency $ ServerAgency KeepAlive.TokServer
      ]
      100


unit_decodeLocalTxSubmission
  :: CDDLSpec (LocalTxSubmission LocalTxSubmission.Tx LocalTxSubmission.Reject)
  -> Assertion
unit_decodeLocalTxSubmission spec =
    validateDecoder Nothing
      spec localTxSubmissionCodec
      [ SomeAgency $ ClientAgency LocalTxSubmission.TokIdle
      , SomeAgency $ ServerAgency LocalTxSubmission.TokBusy
      ]
      100


unit_decodeLocalTxMonitor
  :: CDDLSpec (LocalTxMonitor TxId Tx SlotNo)
  -> Assertion
unit_decodeLocalTxMonitor spec =
    validateDecoder Nothing
      spec localTxMonitorCodec
      [ SomeAgency $ ClientAgency LocalTxMonitor.TokIdle
      , SomeAgency $ ClientAgency LocalTxMonitor.TokAcquired
      , SomeAgency $ ServerAgency LocalTxMonitor.TokAcquiring
      , SomeAgency $ ServerAgency (LocalTxMonitor.TokBusy LocalTxMonitor.TokNextTx)
      , SomeAgency $ ServerAgency (LocalTxMonitor.TokBusy LocalTxMonitor.TokHasTx)
      , SomeAgency $ ServerAgency (LocalTxMonitor.TokBusy LocalTxMonitor.TokGetSizes)
      ]
      100


unit_decodeLocalStateQuery
    :: CDDLSpec (LocalStateQuery Block (Point Block) LocalStateQuery.Query)
    -> Assertion
unit_decodeLocalStateQuery spec =
    validateDecoder Nothing
      spec localStateQueryCodec
      [ SomeAgency $ ClientAgency LocalStateQuery.TokIdle
      , SomeAgency $ ClientAgency LocalStateQuery.TokAcquired
      , SomeAgency $ ServerAgency LocalStateQuery.TokAcquiring
      , SomeAgency $ ServerAgency (LocalStateQuery.TokQuerying LocalStateQuery.QueryPoint)
      ]
      100

unit_decodePeerSharing
    :: CDDLSpec (PeerSharing.PeerSharing SockAddr)
    -> Assertion
unit_decodePeerSharing spec =
    validateDecoder Nothing
      spec (codecPeerSharing encodeRemoteAddress decodeRemoteAddress)
      [ SomeAgency $ ClientAgency TokIdle
      , SomeAgency $ ServerAgency TokBusy
      ]
      100


--
-- Utils
--


unpackResult :: IO (ExitCode, BL.ByteString, BL.ByteString)
             -> IO (Either String BL.ByteString)
unpackResult r = r >>= \case
    (ExitFailure _, _, err) -> return (Left $ BL.Char8.unpack err)
    (ExitSuccess, bytes, _) -> return (Right bytes)


withTemporaryFile :: BL.ByteString -> (FilePath -> IO a) -> IO a
withTemporaryFile bs k =
    withTempFile "." "tmp" $
      \fileName h -> BL.hPut h bs
                  >> hClose h
                  >> k fileName


-- | The cddl spec cannot differentiate between fix-length list encoding and
-- infinite-length encoding.  The cddl tool always generates fix-length
-- encoding but tx-submission codec is accepting only infinite-length
-- encoding.
--
txSubmissionFix :: CBOR.Term -> CBOR.Term
txSubmissionFix term =
    case term of
      TList [TInt tag, TList l] -> TList [TInt tag, TListI l]
      _                         -> term


-- | order entries in a dictionary
--
handshakeFix :: CBOR.Term -> CBOR.Term
handshakeFix term =
    case term of
      TList [TInt 0, TMap l] ->
        TList
          [ TInt 0
          , TMap (sortOn
                   (\(k, _) -> case k of
                     TInt i     -> (fromIntegral i :: Integer)
                     TInteger i -> (fromIntegral i :: Integer)
                     _          -> error "orderHandshakeDict: unexpected key")
                   l
                 )
          ]
      _ -> term
