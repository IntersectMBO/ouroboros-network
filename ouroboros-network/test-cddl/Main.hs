{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import System.Exit (ExitCode(..))
import System.Process.ByteString.Lazy
import Control.Exception.Base (throw)
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 as Char8 (lines, unpack)
import Data.List (intercalate)

import System.Directory (doesDirectoryExist)
import System.FilePath

import Test.QuickCheck
import Test.Tasty (defaultMain, TestTree, testGroup, adjustOption)
import Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))

import qualified Codec.Serialise.Class as Serialise
import Codec.CBOR.Decoding as CBOR (Decoder, decodeBytes ,decodeListLenOf, decodeWord)
import Codec.CBOR.Encoding (encodeBytes, encodeListLen, encodeWord)
import Codec.CBOR.Term as CBOR
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)

import Ouroboros.Network.Block (HeaderHash, Point, Tip, encodeTip, decodeTip, wrapCBORinCBOR, unwrapCBORinCBOR)
import Ouroboros.Network.Testing.ConcreteBlock (BlockHeader (..), Block)

import Ouroboros.Network.Codec
import Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import Ouroboros.Network.Protocol.ChainSync.Test ()
import Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetch)
import Ouroboros.Network.Protocol.BlockFetch.Test ()
import Ouroboros.Network.Protocol.BlockFetch.Type as BlockFetch
import Ouroboros.Network.Protocol.Handshake.Type as Handshake
import Ouroboros.Network.Protocol.Handshake.Test (VersionNumber, versionNumberHandshakeCodec)
import Ouroboros.Network.Protocol.TxSubmission.Codec (codecTxSubmission)
import Ouroboros.Network.Protocol.TxSubmission.Type as TxSubmission
import Ouroboros.Network.Protocol.TxSubmission.Test (TxId, Tx)
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec (codecLocalTxSubmission)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSubmission
import Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import Ouroboros.Network.Protocol.LocalStateQuery.Test (Query (..))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Test as LocalStateQuery
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Test as LocalTxSubmission (Tx, Reject)


-- | The main program, it requires both
--
-- - 'cddl' program
-- - 'diag2cbor.rb' script
--
-- to be installed in the '$PATH'.
--
main :: IO ()
main = do
  a <- doesDirectoryExist "ouroboros-network"
  let cddlSpec =
        if a then "ouroboros-network" </> "test" </> "messages.cddl"
             else "test" </> "messages.cddl"
  defaultMain (tests cddlSpec)


tests :: FilePath -> TestTree
tests cddlSpec =
  adjustOption (const $ QuickCheckMaxSize 10) $ testGroup "messages.cddl-spec"
  [ -- These tests call the CDDL-tool to parse an arbitray message.
    -- The parser of the CDDL-tool is slow (exponential runtime & space).
    testGroup "encoding"
    [ testProperty "ChainSync"         (prop_encodeChainSync cddlSpec)
    , testProperty "BlockFetch"        (prop_encodeBlockFetch cddlSpec)
    , testProperty "TxSubmission"      (prop_encodeTxSubmission cddlSpec)
    , testProperty "Handshake"         (prop_encodeHandshake cddlSpec)
    , testProperty "LocalTxSubmission" (prop_encodeLocalTxSubmission cddlSpec)
    , testProperty "LocalStateQuery"   (prop_encodeLocalStateQuery cddlSpec)
    ]
  , testGroup "decoding"
    [ testProperty "generate and decode" (ioProperty $ validateDecoder 200 cddlSpec)
    ]
  ]

--
-- Mini-Protocol Codecs
--

codecCS :: Codec (ChainSync BlockHeader (Point BlockHeader) (Tip BlockHeader))
                 DeserialiseFailure IO ByteString
codecCS = codecChainSync
            (wrapCBORinCBOR Serialise.encode) (unwrapCBORinCBOR (const <$> Serialise.decode))
            Serialise.encode (Serialise.decode :: CBOR.Decoder s (Point BlockHeader))
            (encodeTip Serialise.encode) (decodeTip Serialise.decode)


codecBF :: Codec (BlockFetch Block (Point Block))
                 DeserialiseFailure IO ByteString
codecBF = codecBlockFetch
            (wrapCBORinCBOR Serialise.encode) (unwrapCBORinCBOR (const <$> Serialise.decode))
            Serialise.encode Serialise.decode


codecTS :: Codec (TxSubmission TxId Tx)
                 DeserialiseFailure IO ByteString
codecTS = codecTxSubmission Serialise.encode Serialise.decode Serialise.encode Serialise.decode


codecLT :: Codec (LocalTxSubmission LocalTxSubmission.Tx LocalTxSubmission.Reject)
                 DeserialiseFailure IO ByteString
codecLT = codecLocalTxSubmission Serialise.encode Serialise.decode Serialise.encode Serialise.decode


codecLSQ :: Codec (LocalStateQuery Block (Point Block) Query)
                  DeserialiseFailure IO ByteString
codecLSQ = LocalStateQuery.codec True


--
-- Encoders Tests
--


-- | Validate mini-protocol codec against its cddl specification.
--
validateEncoder
    :: FilePath
    -- ^ "messages.cddl" spec file path
    -> Word
    -- ^ index of the mini-protocol specification in the cddl specification
    -> Codec ps DeserialiseFailure IO ByteString
    -> AnyMessageAndAgency ps
    -> Property
validateEncoder cddlSpec tagWord codec (AnyMessageAndAgency agency msg) =
    ioProperty $ validateCBOR cddlSpec wrappedMsg
  where
    innerBS = encode codec agency msg
    body = case deserialiseFromBytes decodeTerm innerBS of
        Right (_,res) -> res
        Left err -> error $ "test-cddl.encodeMsg: internal error:" ++ show err
    wrappedMsg = toLazyByteString (encodeListLen 2 <> (encodeWord tagWord) <> encodeTerm body)


prop_encodeChainSync
    :: FilePath
    -> AnyMessageAndAgency (ChainSync BlockHeader (Point BlockHeader) (Tip BlockHeader))
    -> Property
prop_encodeChainSync cddlSpec = validateEncoder cddlSpec 0 codecCS

prop_encodeBlockFetch
    :: FilePath
    -> AnyMessageAndAgency (BlockFetch Block (Point Block))
    -> Property
prop_encodeBlockFetch cddlSpec = validateEncoder cddlSpec 3 codecBF

prop_encodeTxSubmission
    :: FilePath
    -> AnyMessageAndAgency (TxSubmission TxId Tx)
    -> Property
prop_encodeTxSubmission cddlSpec = validateEncoder cddlSpec 4 codecTS

prop_encodeLocalTxSubmission
    :: FilePath
    -> AnyMessageAndAgency (LocalTxSubmission LocalTxSubmission.Tx LocalTxSubmission.Reject)
    -> Property
prop_encodeLocalTxSubmission cddlSpec = validateEncoder cddlSpec 6 codecLT

-- TODO: this test should use 'nodeToNodeHandshakeCodec' and
-- 'nodeToClientHandshakeCodec'
prop_encodeHandshake
    :: FilePath
    -> AnyMessageAndAgency (Handshake VersionNumber CBOR.Term)
    -> Property
prop_encodeHandshake cddlSpec = validateEncoder cddlSpec 5 versionNumberHandshakeCodec

prop_encodeLocalStateQuery
    :: FilePath
    -> AnyMessageAndAgency (LocalStateQuery Block (Point Block) Query)
    -> Property
prop_encodeLocalStateQuery cddlSpec = validateEncoder cddlSpec 7 codecLSQ


--
-- Decoders tests
--

-- | Test decoders against messages generated from the cddl specification.
--
-- TODO: split this test to make one test per codec.
--
validateDecoder :: Int -> FilePath -> IO ()
validateDecoder rounds cddlSpec = do
    terms <- generateCBORDiag cddlSpec rounds
    forM_ (Char8.lines terms) $ \diag ->
            diagToBytes diag
        >>= return . withIndefiniteList
        >>= runParser

runParser :: ByteString -> IO ()
runParser =  decodeMsg . decodeTopTerm

generateCBORDiag :: FilePath -> Int -> IO ByteString
generateCBORDiag cddlSpec rounds = unpackResult $ readProcessWithExitCode "cddl" [cddlSpec, "generate", show rounds] BSL.empty

validateCBOR :: FilePath -> ByteString -> IO ()
validateCBOR cddlSpec bytes = void $ unpackResult $ readProcessWithExitCode "cddl" [cddlSpec, "validate", "-"] bytes

diagToBytes :: ByteString -> IO ByteString
diagToBytes diag = unpackResult $ readProcessWithExitCode "diag2cbor.rb" ["-"] diag

unpackResult :: IO (ExitCode, ByteString, ByteString) -> IO ByteString
unpackResult r = r >>= \case
    (ExitFailure _, _, err) -> error $ Char8.unpack err
    (ExitSuccess, bytes, err) ->
      if BSL.null err
        then return bytes
        else error $ concat [ "test-cddl.unpackResults: ExitSucess but unexpected output on stderr: \n"
                            , Char8.unpack err, "\n"
                            , "Probably a warning."
                            ]

data DummyBytes = DummyBytes BSI.ByteString
    deriving (Show)

instance Serialise.Serialise DummyBytes where
    encode (DummyBytes b) = encodeBytes b
    decode = DummyBytes <$> decodeBytes

instance Arbitrary DummyBytes where
    arbitrary = (DummyBytes . BSI.packBytes) <$> arbitrary

type instance HeaderHash DummyBytes = ()

-- | The cddl spec cannot differentiate between fix-length list encoding and
-- infinite-length encoding.  The cddl tool always generates fix-length
-- encoding but some parsers only accept infinite-length encode.
-- 'withIndefiniteList' rewrites those messages.
--
withIndefiniteList :: ByteString -> ByteString
withIndefiniteList bs =
  case deserialiseFromBytes decodeTerm bs of
      Right (rest, term) | BSL.null rest ->
        case term of
          (TList [TInt 4, TList [TInt 1, TList l]]) ->
            -- 'MsgReplyTxIds'
            recodeTxSubmission 1 l
          (TList [TInt 4, TList [TInt 2, TList l]]) ->
            -- 'MsgRequestTxs'
            recodeTxSubmission 2 l
          (TList [TInt 4, TList [TInt 3, TList l]]) ->
            -- 'MsgReplyTxs'
            recodeTxSubmission 3 l
          _ -> bs
      Right _  -> error   "test-cddl.withIndefiniteList: trailing bytes"
      Left err -> error $ "test-cddl.withIndefiniteList: decoding error: " ++ show err
  where
    recodeTxSubmission tag l = toLazyByteString $ encodeTerm (TList [TInt 4, TList [TInt tag, TListI l]])

-- | Split the ByteString into the tag-word and the rest.
--
decodeTopTerm :: ByteString -> (Word, ByteString)
decodeTopTerm input
    = case deserialiseFromBytes (decodeListLenOf 2 >> decodeWord) input of
        Right (bs, tag) -> (tag, bs)
        Left err -> throw err

-- | Decode a message. Throw an error if the message is not decodeable.
--
decodeMsg :: (Word, ByteString) -> IO ()
decodeMsg (tag, input) = case tag of
    0 -> tryParsers ["chainSync"]             chainSyncParsers
    -- 1 & 2 were used for ReqResp and PingPong
    3 -> tryParsers ["blockFetch"]            blockFetchParsers
    4 -> tryParsers ["txSubmission"]          txSubmissionParsers
    5 -> tryParsers ["handshake"]             handshakeParsers
    6 -> tryParsers ["localTxSubmission"]     localTxSubmissionParsers
    7 -> tryParsers ["localStateQuery"]       localStateQueryParsers
    _ -> error "test-cddl.decodeMessage: unkown tag"
  where
    -- typed-protocols codecs are parameterized on the tokens which
    -- serve as singletons to identify the state types. For each message
    -- we know which protocol it should belong to, but not which state,
    -- so we just run all of them and expect that at least one passes.
    -- This isn't ideal but it's better than nothing. In case they all
    -- fail, error messages for each are given in order.
    tryParsers :: [String] -> [IO (Maybe String)] -> IO ()
    tryParsers traces [] = error $ intercalate "\n" (reverse (show (BSL.unpack input) : "parse failed: " : traces))
    tryParsers traces (h:t) = h >>= \case
        Nothing  -> return ()
        Just errorMsg -> tryParsers (errorMsg : traces) t

    runCodec
      :: IO (DecodeStep ByteString DeserialiseFailure IO (SomeMessage st))
      -> ByteString
      -> IO (Maybe String)
    runCodec cont bs = cont >>= \case
        DecodeDone _msg _rest -> error "test-cddl.runCodec: codec is DecodeDone"
        DecodeFail _f         -> error "test-cddl.runCodec: codec is DecodeFail"
        DecodePartial next    -> (next $ Just bs) >>= \case
            DecodePartial _      -> return (Just "expecting more input")
            DecodeDone _msg rest -> case rest of
                Nothing   -> return Nothing
                Just _r   -> return (Just "leftover input")
            DecodeFail _f -> return (Just (show _f))

    run :: forall ps (pr :: PeerRole) (st :: ps).
            Codec ps DeserialiseFailure IO ByteString
         -> PeerHasAgency pr st -> IO (Maybe String)
    run codec state = runCodec ((decode codec) state) input

    runCS = run codecCS

    chainSyncParsers = [
          runCS (ClientAgency ChainSync.TokIdle)
        , runCS (ServerAgency (ChainSync.TokNext TokCanAwait))
        , runCS (ClientAgency ChainSync.TokIdle)
        , runCS (ServerAgency ChainSync.TokIntersect)
        , runCS (ServerAgency ChainSync.TokIntersect)
        ]

    runBlockFetch = run codecBF

    blockFetchParsers = [
          runBlockFetch (ClientAgency BlockFetch.TokIdle)
        , runBlockFetch (ServerAgency BlockFetch.TokBusy)
        , runBlockFetch (ServerAgency BlockFetch.TokStreaming)
        ]

    -- Use 'nodeToNodeHandshakeCodec' and 'nodeToClientHandshakeCodec'
    runHandshake = run (versionNumberHandshakeCodec
                          :: Codec (Handshake VersionNumber CBOR.Term)
                                   DeserialiseFailure IO ByteString)
    handshakeParsers = [
          runHandshake (ClientAgency TokPropose)
        , runHandshake (ServerAgency TokConfirm)
        ]

    runTS = run codecTS
    txSubmissionParsers = [
          runTS (ServerAgency TxSubmission.TokIdle)
        , runTS (ClientAgency (TokTxIds TokBlocking))
        , runTS (ClientAgency (TokTxIds TokNonBlocking))
        , runTS (ClientAgency TokTxs)
        ]

    runLT = run codecLT
    localTxSubmissionParsers = [
          runLT (ClientAgency LocalTxSubmission.TokIdle)
        , runLT (ServerAgency LocalTxSubmission.TokBusy)
        ]

    runLSQ = run codecLSQ
    localStateQueryParsers = [
          runLSQ (ClientAgency LocalStateQuery.TokIdle)
        , runLSQ (ClientAgency LocalStateQuery.TokAcquired)
        , runLSQ (ServerAgency LocalStateQuery.TokAcquiring)
        , runLSQ (ServerAgency (LocalStateQuery.TokQuerying QueryPoint))
        ]
