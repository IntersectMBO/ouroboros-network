{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import           Control.Monad (forM, forM_)
import           Control.Monad.Except (ExceptT (..), runExceptT)

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
import qualified Network.TypedProtocol.Stateful.Codec as Stateful

import           Ouroboros.Network.Block (Point, SlotNo, Tip, decodeTip,
                     encodeTip, unwrapCBORinCBOR, wrapCBORinCBOR)
import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Magic
import qualified Ouroboros.Network.Mock.ConcreteBlock as Concrete (Block)

import           Ouroboros.Network.NodeToClient.Version (NodeToClientVersion,
                     NodeToClientVersionData (..), nodeToClientCodecCBORTerm)
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..),
                     NodeToNodeVersion (..), NodeToNodeVersionData (..),
                     nodeToNodeCodecCBORTerm)

import qualified Ouroboros.Network.NodeToClient.Version as NtCVersion
import qualified Ouroboros.Network.NodeToNode.Version as NtNVersion
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
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec
                     (codecLocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Codec as LocalStateQuery
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
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..),
                     decodeRemoteAddress, encodeRemoteAddress)
import           Ouroboros.Network.Protocol.PeerSharing.Codec (codecPeerSharing)
import           Ouroboros.Network.Protocol.PeerSharing.Test ()
import qualified Ouroboros.Network.Protocol.PeerSharing.Type as PeerSharing

import           Test.ChainGenerators ()
import           Test.Data.CDDL (Any (..))

import           Test.QuickCheck hiding (Result (..))
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
                , cddlHandshakeNodeToNodeV11ToLast
                , cddlHandshakeNodeToClient
                , cddlPeerSharing
                , cddlNodeToNodeVersionDataV7To10
                , cddlNodeToNodeVersionDataV11ToLast
                } =
  adjustOption (const $ QuickCheckMaxSize 10) $
  testGroup "cddl"
    [ testGroup "encoding"
      -- validate encoding against a specification
      [ testProperty "NodeToNode.Handshake V7 to V10"
                                         (prop_encodeHandshakeNodeToNodeV7To10
                                               cddlHandshakeNodeToNodeV7To10)
      , testProperty "NodeToNode.Handshake V11 to Last"
                                         (prop_encodeHandshakeNodeToNodeV11ToLast
                                               cddlHandshakeNodeToNodeV11ToLast)
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
      , testProperty "LocalTxMonitor"    (prop_encodeLocalTxMonitor
                                               cddlLocalTxMonitor)
      , testProperty "LocalStateQuery"   (prop_encodeLocalStateQuery
                                               cddlLocalStateQuery)
      , testProperty "PeerSharing "      (prop_encodePeerSharing
                                               cddlPeerSharing)

      , testProperty "NodeToNodeVersionData V7 to V10"   (prop_encodeNodeToNodeVersionDataV7To10
                                                            cddlNodeToNodeVersionDataV7To10)
      , testProperty "NodeToNodeVersionData V11 to Last" (prop_encodeNodeToNodeVersionDataV11ToLast
                                                            cddlNodeToNodeVersionDataV11ToLast)
      ]
    , testGroup "decoder"
      -- validate decoder by generating messages from the specification
      [ testCase "NodeToNode.Handshake V7 to V10"
                                     (unit_decodeHandshakeNodeToNode
                                           cddlHandshakeNodeToNodeV7To10)
      , testCase "NodeToNode.Handshake V11 to Last"
                                     (unit_decodeHandshakeNodeToNode
                                           cddlHandshakeNodeToNodeV11ToLast)
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
      , testCase "LocalTxMonitor"    (unit_decodeLocalTxMonitor
                                           cddlLocalTxMonitor)
      , testCase "LocalStateQuery"   (unit_decodeLocalStateQuery
                                           cddlLocalStateQuery)
      , testCase "PeerSharing"       (unit_decodePeerSharing
                                           cddlPeerSharing)

      , testCase "NodeToNodeVersionData V7 to V10"   (unit_decodeNodeToNodeVersionData
                                                        cddlNodeToNodeVersionDataV7To10)
      , testCase "NodeToNodeVersionData V11 to Last" (unit_decodeNodeToNodeVersionDataV11ToLast
                                                        cddlNodeToNodeVersionDataV11ToLast)
      ]
    ]

-- | A 'CDDL' specifcation for a protocol 'ps'.
--
newtype CDDLSpec ps = CDDLSpec BL.ByteString

data CDDLSpecs = CDDLSpecs {
    cddlHandshakeNodeToClient        :: CDDLSpec (Handshake NodeToClientVersion CBOR.Term),
    cddlHandshakeNodeToNodeV7To10    :: CDDLSpec (Handshake NodeToNodeVersion   CBOR.Term),
    cddlHandshakeNodeToNodeV11ToLast :: CDDLSpec (Handshake NodeToNodeVersion   CBOR.Term),
    cddlChainSync                    :: CDDLSpec (ChainSync BlockHeader HeaderPoint HeaderTip),
    cddlBlockFetch                   :: CDDLSpec (BlockFetch Block BlockPoint),
    cddlTxSubmission2                :: CDDLSpec (TxSubmission2 TxId Tx),
    cddlKeepAlive                    :: CDDLSpec KeepAlive,
    cddlLocalTxSubmission            :: CDDLSpec (LocalTxSubmission
                                                    LocalTxSubmission.Tx
                                                    LocalTxSubmission.Reject),
    cddlLocalTxMonitor               :: CDDLSpec (LocalTxMonitor TxId Tx SlotNo),
    cddlLocalStateQuery              :: CDDLSpec (LocalStateQuery Block BlockPoint Query),
    cddlPeerSharing                  :: CDDLSpec (PeerSharing.PeerSharing SockAddr),

    cddlNodeToNodeVersionDataV7To10    :: CDDLSpec NodeToNodeVersionData,
    cddlNodeToNodeVersionDataV11ToLast :: CDDLSpec NodeToNodeVersionData
  }


readCDDLSpecs :: IO CDDLSpecs
readCDDLSpecs = do
    dir <- bool (                                  "test-cddl" </> "specs") -- False
                ("ouroboros-network-protocols" </> "test-cddl" </> "specs") -- True
       <$> doesDirectoryExist "ouroboros-network-protocols"
    common                <- BL.readFile (dir </> "common.cddl")
    handshakeNodeToClient <- BL.readFile (dir </> "handshake-node-to-client.cddl")
    handshakeNodeToNodeV7To10    <- BL.readFile (dir </> "handshake-node-to-node.cddl")
    handshakeNodeToNodeV11ToLast <- BL.readFile (dir </> "handshake-node-to-node-v11.cddl")
    chainSync             <- BL.readFile (dir </> "chain-sync.cddl")
    blockFetch            <- BL.readFile (dir </> "block-fetch.cddl")
    txSubmission2         <- BL.readFile (dir </> "tx-submission2.cddl")
    keepAlive             <- BL.readFile (dir </> "keep-alive.cddl")
    localTxSubmission     <- BL.readFile (dir </> "local-tx-submission.cddl")
    localTxMonitor        <- BL.readFile (dir </> "local-tx-monitor.cddl")
    localStateQuery       <- BL.readFile (dir </> "local-state-query.cddl")
    peerSharing           <- BL.readFile (dir </> "peer-sharing.cddl")

    nodeToNodeVersionDataV7To10 <- BL.readFile (dir </> "node-to-node-version-data.cddl")
    nodeToNodeVersionDataV11ToLast <- BL.readFile (dir </> "node-to-node-version-data-v11.cddl")
    -- append common definitions; they must be appended since the first
    -- definition is the entry point for a cddl spec.
    return CDDLSpecs {
        cddlHandshakeNodeToClient        = CDDLSpec $ handshakeNodeToClient,
        cddlHandshakeNodeToNodeV7To10    = CDDLSpec $ handshakeNodeToNodeV7To10,
        cddlHandshakeNodeToNodeV11ToLast = CDDLSpec $ handshakeNodeToNodeV11ToLast,
        cddlChainSync                    = CDDLSpec $ chainSync
                                                   <> common,
        cddlBlockFetch                   = CDDLSpec $ blockFetch
                                                   <> common,
        cddlTxSubmission2                = CDDLSpec $ txSubmission2
                                                   <> common,
        cddlKeepAlive                    = CDDLSpec keepAlive,
        cddlLocalTxSubmission            = CDDLSpec $ localTxSubmission
                                                   <> common,
        cddlLocalTxMonitor               = CDDLSpec $ localTxMonitor
                                                   <> common,
        cddlLocalStateQuery              = CDDLSpec $ localStateQuery
                                                   <> common,
        cddlPeerSharing                  = CDDLSpec $ peerSharing
                                                   <> common,

        cddlNodeToNodeVersionDataV7To10    = CDDLSpec nodeToNodeVersionDataV7To10,
        cddlNodeToNodeVersionDataV11ToLast = CDDLSpec nodeToNodeVersionDataV11ToLast
      }


newtype BlockHeader = BlockHeader Any
  deriving (Eq, Show, Arbitrary, Serialise)

newtype HeaderPoint = HeaderPoint Any
  deriving (Eq, Show, Arbitrary, Serialise)

newtype HeaderTip = HeaderTip Any
  deriving (Eq, Show, Arbitrary, Serialise)

newtype Block = Block Any
  deriving (Eq, Show, Arbitrary, Serialise)

newtype BlockPoint = BlockPoint Any
  deriving (Eq, Show, Arbitrary, Serialise)

newtype Result = Result Any
  deriving (Eq, Show, Arbitrary, Serialise)

-- TODO: add payload to the query
data Query result where
    Query :: Any -> Query Result

encodeQuery :: Query result -> CBOR.Encoding
encodeQuery (Query a) = Serialise.encode a

decodeQuery :: forall s. CBOR.Decoder s (LocalStateQuery.Some Query)
decodeQuery = LocalStateQuery.Some . Query <$> Serialise.decode

instance LocalStateQuery.ShowQuery Query where
    showResult (Query query) result = show (query, result)
deriving instance Show (Query result)
instance Arbitrary (Query Result) where
    arbitrary = Query <$> arbitrary

--
-- Mini-Protocol Codecs
--

chainSyncCodec :: Codec (ChainSync BlockHeader HeaderPoint HeaderTip)
                        CBOR.DeserialiseFailure IO BL.ByteString
chainSyncCodec =
    codecChainSync
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode


blockFetchCodec :: Codec (BlockFetch Block BlockPoint)
                         CBOR.DeserialiseFailure IO BL.ByteString
blockFetchCodec =
    codecBlockFetch
      Serialise.encode
      Serialise.decode
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


localStateQueryCodec :: Stateful.Codec
                             (LocalStateQuery Block BlockPoint Query)
                              CBOR.DeserialiseFailure
                              LocalStateQuery.State IO BL.ByteString
localStateQueryCodec =
    codecLocalStateQuery
      Serialise.encode Serialise.decode
      encodeQuery decodeQuery
      (\Query{} -> Serialise.encode) (\Query{} -> Serialise.decode)



--
-- Test encodings
--


-- | Validate mini-protocol codec against its cddl specification.
--
validateEncoder
    :: forall ps.
       ( forall (st :: ps) sing. sing ~ StateToken st => Show sing
       )
    => CDDLSpec ps
    -> Codec ps CBOR.DeserialiseFailure IO BL.ByteString
    -> AnyMessage ps
    -> Property
validateEncoder spec
                Codec { encode }
                (AnyMessage msg) =
    counterexample sterms $
    ioProperty $
      either (\err -> counterexample err False)
             (\_   -> property True)
      <$> validateCBOR spec blob
  where
    blob :: BL.ByteString
    blob  = encode msg

    terms :: Either CBOR.DeserialiseFailure (BL.ByteString, Term)
    terms = CBOR.deserialiseFromBytes CBOR.decodeTerm blob

    -- Adding type signature forces a type error:
    -- "Couldn't match kind ps1 with *"
    -- sterms :: String
    sterms = show terms


-- | Validate CBORTermCodec against its cddl specification.
--
validateCBORTermEncoder
    :: Show a
    => CDDLSpec a
    -> CodecCBORTerm fail a
    -> a
    -> Property
validateCBORTermEncoder spec
                        CodecCBORTerm { encodeTerm }
                        a =
    counterexample (show a) $
    counterexample sterms $
    ioProperty $
      either (\err -> counterexample err False)
             (\_   -> property True)
      <$> validateCBOR spec blob
  where
    blob  = CBOR.toLazyByteString
          . CBOR.encodeTerm
          . encodeTerm
          $ a
    terms = CBOR.deserialiseFromBytes CBOR.decodeTerm blob

    -- Adding type signature forces a type error:
    -- "Couldn't match kind ps1 with *"
    -- sterms :: String
    sterms = show terms


validateEncoderSt
    :: forall ps f.
       ( forall (st :: ps) sing. sing ~ StateToken st => Show sing
       )
    => CDDLSpec ps
    -> Stateful.Codec ps CBOR.DeserialiseFailure f IO BL.ByteString
    -> Stateful.AnyMessage ps f
    -> Property
validateEncoderSt spec
                  Stateful.Codec { Stateful.encode }
                  (Stateful.AnyMessage _ f msg) =
    counterexample sterms $
    ioProperty $
      either (\err -> counterexample err False)
             (\_   -> property True)
      <$> validateCBOR spec blob
  where
    blob :: BL.ByteString
    blob  = encode f msg

    terms :: Either CBOR.DeserialiseFailure (BL.ByteString, Term)
    terms = CBOR.deserialiseFromBytes CBOR.decodeTerm blob

    -- Adding type signature forces a type error:
    -- "Couldn't match kind ps1 with *"
    -- sterms :: String
    sterms = show terms


-- | Match encoded CBOR against cddl specification.
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
    (AnyMessage (Handshake NodeToNodeVersion CBOR.Term))
    deriving Show

newtype NtNHandshakeV11ToLast =
  NtNHandshakeV11ToLast
    (AnyMessage (Handshake NodeToNodeVersion CBOR.Term))
    deriving Show

genNtNHandshake :: Gen NodeToNodeVersion
                -> Gen (AnyMessage (Handshake NodeToNodeVersion Term))
genNtNHandshake genVersion = oneof
    [     AnyMessage
        . Handshake.MsgProposeVersions
        . Map.fromList
        . map (\(v, d) -> (v, encodeTerm (nodeToNodeCodecCBORTerm v) d))
      <$> listOf ((,) <$> genVersion <*> genData)

    ,     AnyMessage
        . uncurry Handshake.MsgAcceptVersion
        . (\(v, d) -> (v, encodeTerm (nodeToNodeCodecCBORTerm v) d))
      <$> ((,) <$> genVersion <*> genData)

    ,     AnyMessage
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
          <*> arbitrary

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

instance Arbitrary NtNHandshakeV11ToLast where
  arbitrary = do
    let genVersion = elements [NodeToNodeV_11 ..]
    NtNHandshakeV11ToLast <$> genNtNHandshake genVersion


prop_encodeHandshakeNodeToNodeV7To10
    :: CDDLSpec            (Handshake NodeToNodeVersion CBOR.Term)
    -> NtNHandshakeV7To10
    -> Property
prop_encodeHandshakeNodeToNodeV7To10 spec (NtNHandshakeV7To10 x) =
  validateEncoder spec nodeToNodeHandshakeCodec x

prop_encodeHandshakeNodeToNodeV11ToLast
    :: CDDLSpec            (Handshake NodeToNodeVersion CBOR.Term)
    -> NtNHandshakeV11ToLast
    -> Property
prop_encodeHandshakeNodeToNodeV11ToLast spec (NtNHandshakeV11ToLast x) =
  validateEncoder spec nodeToNodeHandshakeCodec x

-- TODO: add our regular tests for `Handshake NodeToClientVerision CBOR.Term`
-- codec.
--
instance Arbitrary (AnyMessage (Handshake NodeToClientVersion CBOR.Term)) where
    arbitrary = oneof
        [     AnyMessage
            . Handshake.MsgProposeVersions
            . Map.fromList
            . map (\(v, d) -> (v, encodeTerm (nodeToClientCodecCBORTerm v) d))
          <$> listOf ((,) <$> genVersion <*> genData)

        ,     AnyMessage
            . uncurry Handshake.MsgAcceptVersion
            . (\(v, d) -> (v, encodeTerm (nodeToClientCodecCBORTerm v) d))
          <$> ((,) <$> genVersion <*> genData)

        ,     AnyMessage
            . Handshake.MsgRefuse
          <$> genRefuseReason
        ]
      where
        genVersion :: Gen NodeToClientVersion
        genVersion = elements [minBound .. maxBound]

        genData :: Gen NodeToClientVersionData
        genData = NodeToClientVersionData
              <$> (NetworkMagic <$> arbitrary)
              <*> arbitrary

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
    :: CDDLSpec   (Handshake NodeToClientVersion CBOR.Term)
    -> AnyMessage (Handshake NodeToClientVersion CBOR.Term)
    -> Property
prop_encodeHandshakeNodeToClient spec = validateEncoder spec nodeToClientHandshakeCodec


prop_encodeChainSync
    :: CDDLSpec   (ChainSync BlockHeader
                             HeaderPoint
                             HeaderTip)
    -> AnyMessage (ChainSync BlockHeader
                             HeaderPoint
                             HeaderTip)
    -> Property
prop_encodeChainSync spec = validateEncoder spec chainSyncCodec


prop_encodeBlockFetch
    :: CDDLSpec   (BlockFetch Block BlockPoint)
    -> AnyMessage (BlockFetch Block BlockPoint)
    -> Property
prop_encodeBlockFetch spec = validateEncoder spec blockFetchCodec


prop_encodeTxSubmission2
    :: CDDLSpec   (TxSubmission2 TxId Tx)
    -> AnyMessage (TxSubmission2 TxId Tx)
    -> Property
prop_encodeTxSubmission2 spec = validateEncoder spec txSubmissionCodec2


prop_encodeKeepAlive
    :: CDDLSpec   KeepAlive
    -> AnyMessage KeepAlive
    -> Property
prop_encodeKeepAlive spec = validateEncoder spec codecKeepAlive_v2


prop_encodeLocalTxSubmission
    :: CDDLSpec   (LocalTxSubmission LocalTxSubmission.Tx
                                     LocalTxSubmission.Reject)
    -> AnyMessage (LocalTxSubmission LocalTxSubmission.Tx
                                     LocalTxSubmission.Reject)
    -> Property
prop_encodeLocalTxSubmission spec = validateEncoder spec localTxSubmissionCodec

prop_encodeLocalTxMonitor
    :: CDDLSpec   (LocalTxMonitor TxId Tx SlotNo)
    -> AnyMessage (LocalTxMonitor TxId Tx SlotNo)
    -> Property
prop_encodeLocalTxMonitor spec = validateEncoder spec localTxMonitorCodec

prop_encodeLocalStateQuery
    :: CDDLSpec   (LocalStateQuery Block BlockPoint Query)
    -- TODO: find a better solution that using a 'Blind'
    -> LocalStateQuery.AnyMessageWithResult Block BlockPoint Query Result
    -> Property
prop_encodeLocalStateQuery spec (LocalStateQuery.AnyMessageWithResult msg) =
    validateEncoderSt spec localStateQueryCodec msg

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
    :: CDDLSpec   (PeerSharing.PeerSharing SockAddr)
    -> AnyMessage (PeerSharing.PeerSharing SockAddr)
    -> Property
prop_encodePeerSharing spec =
  validateEncoder spec (codecPeerSharing encodeRemoteAddress decodeRemoteAddress)

newtype NtNVersionV7To10    = NtNVersionV7To10 NodeToNodeVersion
  deriving Show
newtype NtNVersionV11       = NtNVersionV11 NodeToNodeVersion
  deriving Show
newtype NtNVersionV11ToLast = NtNVersionV11ToLast NodeToNodeVersion
  deriving Show

instance Arbitrary NtNVersionV7To10 where
  arbitrary = NtNVersionV7To10 <$> elements [NodeToNodeV_7 .. NodeToNodeV_10]

instance Arbitrary NtNVersionV11ToLast where
  arbitrary = NtNVersionV11ToLast <$> elements [NodeToNodeV_11 ..]

instance Arbitrary NodeToNodeVersionData where
    arbitrary =
      NodeToNodeVersionData
        <$> (NetworkMagic <$> arbitrary)
        <*> oneof [ pure InitiatorOnlyDiffusionMode
                  , pure InitiatorAndResponderDiffusionMode
                  ]
        <*> oneof [ pure NoPeerSharing
                  , pure PeerSharingPrivate
                  , pure PeerSharingPublic
                  ]
        <*> arbitrary

newtype NtNVersionDataV11ToLast = NtNVersionDataV11ToLast (NodeToNodeVersion, NodeToNodeVersionData)
  deriving Show

instance Arbitrary NtNVersionDataV11ToLast where
  arbitrary = do
    NtNVersionV11ToLast ntnVersion <- arbitrary
    ntnVersionData <- arbitrary
    return (NtNVersionDataV11ToLast (ntnVersion, ntnVersionData))

prop_encodeNodeToNodeVersionDataV7To10
    :: CDDLSpec NodeToNodeVersionData
    -> NtNVersionV7To10
    -> NodeToNodeVersionData
    -> Property
prop_encodeNodeToNodeVersionDataV7To10 spec (NtNVersionV7To10 v) a =
  validateCBORTermEncoder spec (nodeToNodeCodecCBORTerm v)
                          a { peerSharing = NoPeerSharing,
                              NtNVersion.query = False
                            }

prop_encodeNodeToNodeVersionDataV11ToLast
    :: CDDLSpec NodeToNodeVersionData
    -> NtNVersionDataV11ToLast
    -> Property
prop_encodeNodeToNodeVersionDataV11ToLast spec (NtNVersionDataV11ToLast (v, a)) =
  validateCBORTermEncoder spec (nodeToNodeCodecCBORTerm v) a

--
-- Test decoders
--


data SomeAgency ps where
    SomeAgency :: ActiveState st
               => StateToken (st :: ps)
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

-- | Generate valid encoded messages from a specification using `cddl generate`
-- (and encoded with `diag2cbor.rb`) and check that we can decode it using a given
-- codec.
--
validateCBORTermDecoder
  :: Show fail
  => Maybe (CBOR.Term -> CBOR.Term)
  -- ^ transform a generated term
  -> CDDLSpec a
  -> CodecCBORTerm fail a
  -> Int
  -> Assertion
validateCBORTermDecoder transform (CDDLSpec spec) codec rounds = do
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
              Right (_, decoded_term) = CBOR.deserialiseFromBytes CBOR.decodeTerm encoded_term'
              res = decodeTerm codec decoded_term
          case res of
            Left err -> assertFailure $ concat
              [ "decoding failures:\n"
              , show err
              , "while decoding:\n"
              , show decoded_term
              , "\n"
              , BL.Char8.unpack generated_term
              ]
            Right _ -> return ()


data SomeAgencySt ps f where
    SomeAgencySt :: ActiveState st
                 => StateToken (st :: ps)
                 -> f st
                 -> SomeAgencySt ps f


validateDecoderSt :: Maybe (CBOR.Term -> CBOR.Term)
                  -- ^ transform a generated term
                  -> CDDLSpec ps
                  -> Stateful.Codec ps CBOR.DeserialiseFailure f IO BL.ByteString
                  -> [SomeAgencySt ps f]
                  -> Int
                  -> Assertion
validateDecoderSt transform (CDDLSpec spec) codec stoks rounds = do
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
          res <- decodeMsgSt codec stoks encoded_term'
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


-- | Try decode at all given agencies.  If one succeeds return
-- 'Nothing' otherwise return all 'DeserialiseFailure's.
--
decodeMsg :: forall ps.
             Codec ps CBOR.DeserialiseFailure IO BL.ByteString
          -> [SomeAgency ps]
          -- ^ list of all agencies to try
          -> BL.ByteString
          -> IO (Maybe [CBOR.DeserialiseFailure])
decodeMsg codec stoks bs =
    -- sequence [Nothing, ...] = Nothing
    fmap (sequence :: [Maybe CBOR.DeserialiseFailure] -> Maybe [CBOR.DeserialiseFailure]) $
    forM stoks $ \(SomeAgency (stok :: StateToken st)) -> do
        decoder <- (decode codec stok :: IO (DecodeStep BL.ByteString CBOR.DeserialiseFailure IO (SomeMessage st)))
        res <- runDecoder [bs] decoder
        return $ case res of
          Left err -> Just err
          Right {} -> Nothing

decodeMsgSt :: forall ps f.
               Stateful.Codec ps CBOR.DeserialiseFailure f IO BL.ByteString
            -> [SomeAgencySt ps f]
            -- ^ list of all gencies to try
            -> BL.ByteString
            -> IO (Maybe [CBOR.DeserialiseFailure])
decodeMsgSt codec stoks bs =
    -- sequence [Nothing, ...] = Nothing
    fmap (sequence :: [Maybe CBOR.DeserialiseFailure] -> Maybe [CBOR.DeserialiseFailure]) $
    forM stoks $ \(SomeAgencySt (stok :: StateToken st) f) -> do
        decoder <- (Stateful.decode codec stok f :: IO (DecodeStep BL.ByteString CBOR.DeserialiseFailure IO (SomeMessage st)))
        res <- runDecoder [bs] decoder
        return $ case res of
          Left err -> Just err
          Right {} -> Nothing

unit_decodeHandshakeNodeToNode
    :: CDDLSpec (Handshake NodeToNodeVersion CBOR.Term)
    -> Assertion
unit_decodeHandshakeNodeToNode spec =
    validateDecoder (Just handshakeFix)
      spec nodeToNodeHandshakeCodec
      [ SomeAgency Handshake.SingPropose
      , SomeAgency Handshake.SingConfirm
      ]
      100


unit_decodeHandshakeNodeToClient
    :: CDDLSpec (Handshake NodeToClientVersion CBOR.Term)
    -> Assertion
unit_decodeHandshakeNodeToClient spec =
    validateDecoder (Just handshakeFix)
      spec nodeToClientHandshakeCodec
      [ SomeAgency Handshake.SingPropose
      , SomeAgency Handshake.SingConfirm
      ]
      100


unit_decodeChainSync
    :: CDDLSpec (ChainSync BlockHeader HeaderPoint HeaderTip)
    -> Assertion
unit_decodeChainSync spec =
    validateDecoder Nothing
      spec chainSyncCodec
      [ SomeAgency ChainSync.SingIdle
      , SomeAgency (ChainSync.SingNext ChainSync.SingCanAwait)
      , SomeAgency (ChainSync.SingNext ChainSync.SingMustReply)
      , SomeAgency (ChainSync.SingIntersect)
      ]
      100


unit_decodeBlockFetch
    :: CDDLSpec (BlockFetch Block BlockPoint)
    -> Assertion
unit_decodeBlockFetch spec =
    validateDecoder Nothing
      spec blockFetchCodec
      [ SomeAgency BlockFetch.SingBFIdle
      , SomeAgency BlockFetch.SingBFBusy
      , SomeAgency BlockFetch.SingBFStreaming
      ]
      100


unit_decodeTxSubmission2
    :: CDDLSpec (TxSubmission2 TxId Tx)
    -> Assertion
unit_decodeTxSubmission2 spec =
    validateDecoder (Just txSubmissionFix)
      spec txSubmissionCodec2
      [ SomeAgency TxSubmission2.SingInit
      , SomeAgency $ TxSubmission2.SingTxIds TxSubmission2.SingBlocking
      , SomeAgency $ TxSubmission2.SingTxIds TxSubmission2.SingNonBlocking
      , SomeAgency $ TxSubmission2.SingTxs
      , SomeAgency $ TxSubmission2.SingIdle
      ]
      100


unit_decodeKeepAlive
    :: CDDLSpec KeepAlive
    -> Assertion
unit_decodeKeepAlive spec =
    validateDecoder Nothing
      spec codecKeepAlive_v2
      [ SomeAgency KeepAlive.SingClient
      , SomeAgency KeepAlive.SingServer
      ]
      100


unit_decodeLocalTxSubmission
  :: CDDLSpec (LocalTxSubmission LocalTxSubmission.Tx LocalTxSubmission.Reject)
  -> Assertion
unit_decodeLocalTxSubmission spec =
    validateDecoder Nothing
      spec localTxSubmissionCodec
      [ SomeAgency LocalTxSubmission.SingIdle
      , SomeAgency LocalTxSubmission.SingBusy
      ]
      100


unit_decodeLocalTxMonitor
  :: CDDLSpec (LocalTxMonitor TxId Tx SlotNo)
  -> Assertion
unit_decodeLocalTxMonitor spec =
    validateDecoder Nothing
      spec localTxMonitorCodec
      [ SomeAgency LocalTxMonitor.SingIdle
      , SomeAgency LocalTxMonitor.SingAcquired
      , SomeAgency LocalTxMonitor.SingAcquiring
      , SomeAgency (LocalTxMonitor.SingBusy LocalTxMonitor.SingNextTx)
      , SomeAgency (LocalTxMonitor.SingBusy LocalTxMonitor.SingHasTx)
      , SomeAgency (LocalTxMonitor.SingBusy LocalTxMonitor.SingGetSizes)
      ]
      100

unit_decodeLocalStateQuery
    :: CDDLSpec (LocalStateQuery Block BlockPoint Query)
    -> Assertion
unit_decodeLocalStateQuery spec =
    validateDecoderSt Nothing
      spec localStateQueryCodec
      [ SomeAgencySt LocalStateQuery.SingIdle
                     LocalStateQuery.StateIdle
      , SomeAgencySt LocalStateQuery.SingAcquired
                     LocalStateQuery.StateAcquired
      , SomeAgencySt LocalStateQuery.SingAcquiring
                     LocalStateQuery.StateAcquiring
        -- note: we use a bottom, because the `codecLocalStateQuery` via
        -- `decodeQuery` will not scrutinize the query payload.
      , SomeAgencySt LocalStateQuery.SingQuerying
                     (LocalStateQuery.StateQuerying
                        (Query (error "invariant violation: lazy value")))
      ]
      100

unit_decodePeerSharing
    :: CDDLSpec (PeerSharing.PeerSharing SockAddr)
    -> Assertion
unit_decodePeerSharing spec =
    validateDecoder Nothing
      spec (codecPeerSharing encodeRemoteAddress decodeRemoteAddress)
      [ SomeAgency PeerSharing.SingIdle
      , SomeAgency PeerSharing.SingBusy
      ]
      100

unit_decodeNodeToNodeVersionData
    :: CDDLSpec NodeToNodeVersionData
    -> Assertion
unit_decodeNodeToNodeVersionData spec =
    forM_ [NodeToNodeV_7 .. NodeToNodeV_10] $ \v ->
    validateCBORTermDecoder Nothing spec (nodeToNodeCodecCBORTerm v) 100

unit_decodeNodeToNodeVersionDataV11ToLast
    :: CDDLSpec NodeToNodeVersionData
    -> Assertion
unit_decodeNodeToNodeVersionDataV11ToLast spec =
    forM_ [NodeToNodeV_11 ..] $ \v ->
    validateCBORTermDecoder Nothing spec (nodeToNodeCodecCBORTerm v) 100

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
      TList [TInt x, TMap l] | x == 0 || x == 3 ->
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
