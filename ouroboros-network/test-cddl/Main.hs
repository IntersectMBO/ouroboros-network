{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
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
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import           Control.Monad.Except

import qualified Codec.Serialise.Class as Serialise
import           Codec.CBOR.Term (Term (..))
import qualified Codec.CBOR.Term     as CBOR
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Write    as CBOR

import           Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL.Char8
import           Data.List (sortOn)
import qualified Data.Map as Map
import           Data.Ord (Down (..))
import qualified Data.Text as Text

import           System.Directory (doesDirectoryExist)
import           System.Exit (ExitCode(..))
import           System.FilePath
import           System.IO (hClose)
import           System.IO.Temp (withTempFile)
import           System.Process.ByteString.Lazy

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Block (Point, Tip, encodeTip,
                   decodeTip, wrapCBORinCBOR, unwrapCBORinCBOR)
import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Magic
import           Ouroboros.Network.Testing.ConcreteBlock (BlockHeader (..), Block)

import           Ouroboros.Network.Codec
import           Ouroboros.Network.NodeToNode (NodeToNodeVersion (..),
                   NodeToNodeVersionData (..), nodeToNodeHandshakeCodec)
import           Ouroboros.Network.NodeToNode.Version (DiffusionMode (..),
                   nodeToNodeCodecCBORTerm)
import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..),
                   NodeToClientVersionData (..), nodeToClientHandshakeCodec)
import           Ouroboros.Network.NodeToClient.Version (nodeToClientCodecCBORTerm)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Test ()
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import qualified Ouroboros.Network.Protocol.BlockFetch.Type as BlockFetch
import           Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetch)
import           Ouroboros.Network.Protocol.BlockFetch.Test ()
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import qualified Ouroboros.Network.Protocol.Handshake.Type as Handshake
import           Ouroboros.Network.Protocol.Handshake.Test (VersionNumber,
                   versionNumberHandshakeCodec)
import           Ouroboros.Network.Protocol.KeepAlive.Type (KeepAlive)
import qualified Ouroboros.Network.Protocol.KeepAlive.Type as KeepAlive
import           Ouroboros.Network.Protocol.KeepAlive.Codec (codecKeepAlive_v2)
import           Ouroboros.Network.Protocol.KeepAlive.Test ()
import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission)
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as TxSubmission
import           Ouroboros.Network.Protocol.TxSubmission.Codec (codecTxSubmission)
import           Ouroboros.Network.Protocol.TxSubmission.Test (TxId, Tx)
import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TxSubmission2
import           Ouroboros.Network.Protocol.TxSubmission2.Codec (codecTxSubmission2)
import           Ouroboros.Network.Protocol.TxSubmission2.Test ()
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSubmission
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec (codecLocalTxSubmission)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Test as LocalTxSubmission
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Test as LocalStateQuery
import qualified Ouroboros.Network.Protocol.Trans.Hello.Type as Hello

import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()
import           Test.Tasty (defaultMain, TestTree, testGroup, adjustOption)
import           Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))
import           Test.Tasty.HUnit


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
                , cddlTxSubmission
                , cddlLocalTxSubmission
                , cddlTxSubmission2
                , cddlKeepAlive
                , cddlLocalStateQuery
                , cddlHandshakeNodeToNode
                , cddlHandshakeNodeToClient
                } =
  adjustOption (const $ QuickCheckMaxSize 10) $
  testGroup "cddl"
    [ testGroup "encoding"
      -- validate encoding against a specification
      [ testProperty "NodeToNode.Handshake"
                                         (prop_encodeHandshakeNodeToNode
                                               cddlHandshakeNodeToNode)
      , testProperty "NodeToClient.Handshake"
                                         (prop_encodeHandshakeNodeToClient
                                               cddlHandshakeNodeToClient)
      , testProperty "ChainSync"         (prop_encodeChainSync
                                               cddlChainSync)
      , testProperty "BlockFetch"        (prop_encodeBlockFetch
                                               cddlBlockFetch)
      , testProperty "TxSubmission"      (prop_encodeTxSubmission
                                               cddlTxSubmission)
      , testProperty "TxSubmission2"     (prop_encodeTxSubmission2
                                               cddlTxSubmission2)
      , testProperty "KeepAlive"         (prop_encodeKeepAlive
                                               cddlKeepAlive)
      , testProperty "LocalTxSubmission" (prop_encodeLocalTxSubmission
                                               cddlLocalTxSubmission)
      , testProperty "LocalStateQuery"   (prop_encodeLocalStateQuery
                                               cddlLocalStateQuery)
      ]
    , testGroup "decoder"
      -- validate decoder by generating messages from the specification
      [ testCase "NodeToNode.Handshake"
                                     (unit_decodeHandshakeNodeToNode
                                           cddlHandshakeNodeToNode)
      , testCase "NodeToClient.Handshake"
                                     (unit_decodeHandshakeNodeToClient
                                           cddlHandshakeNodeToClient)
      , testCase "ChainSync"         (unit_decodeChainSync
                                           cddlChainSync)
      , testCase "BlockFetch"        (unit_decodeBlockFetch
                                           cddlBlockFetch)
      , testCase "TxSubmission"      (unit_decodeTxSubmission
                                           cddlTxSubmission)
      , testCase "TxSubmission2"     (unit_decodeTxSubmission2
                                           cddlTxSubmission2)
      , testCase "KeepAlive"         (unit_decodeKeepAlive
                                           cddlKeepAlive)
      , testCase "LocalTxSubmission" (unit_decodeLocalTxSubmission
                                           cddlLocalTxSubmission)
      , testCase "LocalStateQuery"   (unit_decodeLocalStateQuery
                                           cddlLocalStateQuery)
      ]
    ]


-- | A 'CDDL' specifcation for a protocol 'ps'.
--
newtype CDDLSpec ps = CDDLSpec BL.ByteString

data CDDLSpecs = CDDLSpecs {
    cddlHandshakeNodeToClient :: CDDLSpec (Handshake NodeToClientVersion CBOR.Term),
    cddlHandshakeNodeToNode   :: CDDLSpec (Handshake NodeToNodeVersion   CBOR.Term),
    cddlChainSync             :: CDDLSpec (ChainSync
                                             BlockHeader
                                             (Point BlockHeader)
                                             (Tip BlockHeader)),
    cddlBlockFetch            :: CDDLSpec (BlockFetch Block (Point Block)),
    cddlTxSubmission          :: CDDLSpec (TxSubmission TxId Tx),
    cddlTxSubmission2         :: CDDLSpec (TxSubmission2 TxId Tx),
    cddlKeepAlive             :: CDDLSpec KeepAlive,
    cddlLocalTxSubmission     :: CDDLSpec (LocalTxSubmission
                                             LocalTxSubmission.Tx
                                             LocalTxSubmission.Reject),
    cddlLocalStateQuery       :: CDDLSpec (LocalStateQuery
                                             Block (Point Block)
                                             LocalStateQuery.Query)
  }


readCDDLSpecs :: IO CDDLSpecs
readCDDLSpecs = do
    dir <- bool (                        "test-cddl" </> "specs") -- False
                ("ouroboros-network" </> "test-cddl" </> "specs") -- True
       <$> doesDirectoryExist "ouroboros-network"
    common                <- BL.readFile (dir </> "common.cddl")
    handshakeNodeToClient <- BL.readFile (dir </> "handshake-node-to-client.cddl")
    handshakeNodeToNode   <- BL.readFile (dir </> "handshake-node-to-node.cddl")
    chainSync             <- BL.readFile (dir </> "chain-sync.cddl")
    blockFetch            <- BL.readFile (dir </> "block-fetch.cddl")
    txSubmission          <- BL.readFile (dir </> "tx-submission.cddl")
    txSubmission2         <- BL.readFile (dir </> "tx-submission2.cddl")
    keepAlive             <- BL.readFile (dir </> "keep-alive.cddl")
    localTxSubmission     <- BL.readFile (dir </> "local-tx-submission.cddl")
    localStateQuery       <- BL.readFile (dir </> "local-state-query.cddl")
    -- append common definitions; they must be appended since the first
    -- definition is the entry point for a cddl spec.
    return CDDLSpecs {
        cddlHandshakeNodeToClient = CDDLSpec $ handshakeNodeToClient,
        cddlHandshakeNodeToNode   = CDDLSpec $ handshakeNodeToNode,
        cddlChainSync             = CDDLSpec $ chainSync
                                            <> common,
        cddlBlockFetch            = CDDLSpec $ blockFetch
                                            <> common,
        cddlTxSubmission          = CDDLSpec $ txSubmission
                                            <> common,
        cddlTxSubmission2         = CDDLSpec $ txSubmission2
                                            <> txSubmission
                                            <> common,
        cddlKeepAlive             = CDDLSpec keepAlive,
        cddlLocalTxSubmission     = CDDLSpec $ localTxSubmission
                                            <> common,
        cddlLocalStateQuery       = CDDLSpec $ localStateQuery
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


txSubmissionCodec :: Codec (TxSubmission TxId Tx)
                           CBOR.DeserialiseFailure IO BL.ByteString
txSubmissionCodec =
    codecTxSubmission
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


localStateQueryCodec :: Codec (LocalStateQuery Block (Point Block) LocalStateQuery.Query)
                              CBOR.DeserialiseFailure IO BL.ByteString
localStateQueryCodec =
    LocalStateQuery.codec True


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


-- TODO: add our regular tests for `Handshake NodeToNodeVerision CBOR.Term`
-- codec.
--
instance Arbitrary (AnyMessageAndAgency (Handshake NodeToNodeVersion CBOR.Term)) where
    arbitrary = oneof
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
        genVersion :: Gen NodeToNodeVersion
        genVersion = elements [NodeToNodeV_4 ..]

        genData :: Gen NodeToNodeVersionData
        genData = NodeToNodeVersionData
              <$> (NetworkMagic <$> arbitrary)
              <*> oneof
                    [ pure InitiatorOnlyDiffusionMode
                    , pure InitiatorAndResponderDiffusionMode
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


prop_encodeHandshakeNodeToNode
    :: CDDLSpec            (Handshake NodeToNodeVersion CBOR.Term)
    -> AnyMessageAndAgency (Handshake NodeToNodeVersion CBOR.Term)
    -> Property
prop_encodeHandshakeNodeToNode spec = validateEncoder spec nodeToNodeHandshakeCodec


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
        genVersion = elements [NodeToClientV_1 ..]

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


prop_encodeTxSubmission
    :: CDDLSpec            (TxSubmission TxId Tx)
    -> AnyMessageAndAgency (TxSubmission TxId Tx)
    -> Property
prop_encodeTxSubmission spec = validateEncoder spec txSubmissionCodec


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


prop_encodeLocalStateQuery
    :: CDDLSpec            (LocalStateQuery Block (Point Block) LocalStateQuery.Query)
    -> AnyMessageAndAgency (LocalStateQuery Block (Point Block) LocalStateQuery.Query)
    -> Property
prop_encodeLocalStateQuery spec = validateEncoder spec localStateQueryCodec


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


unit_decodeTxSubmission
    :: CDDLSpec (TxSubmission TxId Tx)
    -> Assertion
unit_decodeTxSubmission spec =
    validateDecoder (Just txSubmissionFix)
      spec txSubmissionCodec
      [ SomeAgency $ ClientAgency (TxSubmission.TokTxIds TxSubmission.TokBlocking)
      , SomeAgency $ ClientAgency (TxSubmission.TokTxIds TxSubmission.TokNonBlocking)
      , SomeAgency $ ClientAgency TxSubmission.TokTxs
      , SomeAgency $ ServerAgency TxSubmission.TokIdle
      ]
      100


unit_decodeTxSubmission2
    :: CDDLSpec (TxSubmission2 TxId Tx)
    -> Assertion
unit_decodeTxSubmission2 spec =
    validateDecoder (Just txSubmissionFix)
      spec txSubmissionCodec2
      [ SomeAgency
        $ ClientAgency
          Hello.TokHello
      , SomeAgency
        $ ClientAgency
        $ Hello.TokClientTalk
            (TxSubmission.TokTxIds TxSubmission.TokBlocking)
      , SomeAgency
        $ ClientAgency
        $ Hello.TokClientTalk
            (TxSubmission.TokTxIds TxSubmission.TokNonBlocking)
      , SomeAgency
        $ ClientAgency
        $ Hello.TokClientTalk TxSubmission.TokTxs
      , SomeAgency
        $ ServerAgency
        $ Hello.TokServerTalk TxSubmission.TokIdle
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
      _ -> term


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
                     _ -> error "orderHandshakeDict: unexpected key")
                   l
                 )
          ]
      _ -> term
