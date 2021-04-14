{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main
where

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
import Ouroboros.Network.Protocol.ChainSync.Type as CS
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


cddlTool :: FilePath
cddlTool = "cddl"

diag2cborTool :: FilePath
diag2cborTool = "diag2cbor.rb"

main :: IO ()
main = do
    a <- doesDirectoryExist "ouroboros-network"
    let specPath =
          if a then "ouroboros-network" </> "test" </> "messages.cddl"
               else "test" </> "messages.cddl"
    defaultMain (tests specPath)

tests :: FilePath -> TestTree
tests specPath =
  adjustOption (const $ QuickCheckMaxSize 10) $ testGroup "messages.cddl-spec"
  [
-- These tests call the CDDL-tool to parse an arbitray message.
-- The parser of the CDDL-tool is slow (exponential runtime & space).
    testProperty "encode ChainSync"             (prop_specCS specPath)
  , testProperty "encode BlockFetch"            (prop_specBF specPath)
  , testProperty "encode TxSubmission"          (prop_specTxSubmission specPath)
  , testProperty "encode Handshake"             (prop_specHandshake specPath)
  , testProperty "encode local Tx submission"   (prop_specLocalTxSubmission specPath)
  , testProperty "encode LocalStateQuery"       (prop_specLocalStateQuery specPath)
  -- Test the parsers with CDDL-generated messages.
  , testProperty "generate and decode" $ ioProperty $ generateAndDecode 100 specPath
  ]

-- The concrete/monomorphic types used for the test.
type MonoCodec x = Codec x DeserialiseFailure IO ByteString
type CS  = ChainSync BlockHeader (Point BlockHeader) (Tip BlockHeader)
type BF  = BlockFetch Block (Point Block)
type HS  = Handshake VersionNumber CBOR.Term
type TS  = TxSubmission TxId Tx
type LT  = LocalTxSubmission LocalTxSubmission.Tx LocalTxSubmission.Reject
type LSQ = LocalStateQuery Block (Point Block) Query

codecCS :: MonoCodec CS
codecCS = codecChainSync
            (wrapCBORinCBOR Serialise.encode) (unwrapCBORinCBOR (const <$> Serialise.decode))
            Serialise.encode (Serialise.decode :: CBOR.Decoder s (Point BlockHeader))
            (encodeTip Serialise.encode) (decodeTip Serialise.decode)

codecBF :: MonoCodec BF
codecBF = codecBlockFetch
            (wrapCBORinCBOR Serialise.encode) (unwrapCBORinCBOR (const <$> Serialise.decode))
            Serialise.encode Serialise.decode

codecTS :: MonoCodec TS
codecTS = codecTxSubmission Serialise.encode Serialise.decode Serialise.encode Serialise.decode

codecLT :: MonoCodec LT
codecLT = codecLocalTxSubmission Serialise.encode Serialise.decode Serialise.encode Serialise.decode

codecLSQ :: MonoCodec LSQ
codecLSQ = LocalStateQuery.codec True

prop_specCS :: FilePath -> AnyMessageAndAgency CS -> Property
prop_specCS specPath = prop_CDDLSpec specPath (0, codecCS)

prop_specBF :: FilePath -> AnyMessageAndAgency BF -> Property
prop_specBF specPath = prop_CDDLSpec specPath (3, codecBF)

prop_specTxSubmission :: FilePath -> AnyMessageAndAgency TS -> Property
prop_specTxSubmission specPath = prop_CDDLSpec specPath (4, codecTS)

prop_specLocalTxSubmission :: FilePath -> AnyMessageAndAgency LT -> Property
prop_specLocalTxSubmission specPath = prop_CDDLSpec specPath (6, codecLT)

-- TODO: this test should use 'nodeToNodeHandshakeCodec' and
-- 'nodeToClientHandshakeCodec'
prop_specHandshake :: FilePath -> AnyMessageAndAgency HS -> Property
prop_specHandshake specPath = prop_CDDLSpec specPath (5, versionNumberHandshakeCodec)

prop_specLocalStateQuery :: FilePath -> AnyMessageAndAgency LSQ -> Property
prop_specLocalStateQuery specPath = prop_CDDLSpec specPath (7, codecLSQ)

prop_CDDLSpec :: FilePath -- ^ "messages.cddl" spec file path
              -> (Word, MonoCodec ps)
              -> AnyMessageAndAgency ps -> Property
prop_CDDLSpec specPath (tagWord, codec) (AnyMessageAndAgency agency msg)
    = ioProperty $ do
--         print $ BSL.unpack wrappedMsg
         validateCBOR specPath wrappedMsg
    where
        innerBS = encode codec agency msg
        body = case deserialiseFromBytes decodeTerm innerBS of
            Right (_,res) -> res
            Left err -> error $ "encodeMsg : internal error :" ++ show err
        wrappedMsg = toLazyByteString (encodeListLen 2 <> (encodeWord tagWord) <> encodeTerm body)

generateAndDecode :: Int -> FilePath -> IO ()
generateAndDecode rounds cddlSpec = do
    terms <- generateCBORDiag cddlSpec rounds
    forM_ (Char8.lines terms) $ \diag -> do
--        Char8.putStrLn diag
        diagToBytes diag >>= (return . rewriteList) >>= runParser

runParser :: ByteString -> IO ()
runParser =  decodeMsg . decodeTopTerm

generateCBORDiag :: FilePath -> Int -> IO ByteString
generateCBORDiag cddlSpec rounds = unpackResult $ readProcessWithExitCode cddlTool [cddlSpec, "generate", show rounds] BSL.empty

validateCBOR :: FilePath -> ByteString -> IO ()
validateCBOR cddlSpec bytes = void $ unpackResult $ readProcessWithExitCode cddlTool [cddlSpec, "validate", "-"] bytes

diagToBytes :: ByteString -> IO ByteString
diagToBytes diag = unpackResult $ readProcessWithExitCode diag2cborTool ["-"] diag

unpackResult :: IO (ExitCode, ByteString, ByteString) -> IO ByteString
unpackResult r = r >>= \case
    (ExitFailure _, _, err) -> error $ Char8.unpack err
    (ExitSuccess, bytes, err) -> if BSL.null err
        then return bytes
        else error $ concat [ "unpackResults: ExitSucess but unexpected output on stderr: \n"
                            , Char8.unpack err, "\n"
                            , "Probably a warning."
                            ]

decodeFile :: FilePath -> IO ()
decodeFile f = BSL.readFile f >>= runParser

data DummyBytes = DummyBytes BSI.ByteString
    deriving (Show)

instance Serialise.Serialise DummyBytes where
    encode (DummyBytes b) = encodeBytes b
    decode = DummyBytes <$> decodeBytes

instance Arbitrary DummyBytes where
    arbitrary = (DummyBytes . BSI.packBytes) <$> arbitrary

type instance HeaderHash DummyBytes = ()

-- | The cddl spec cannot differentiate between fix-length list encoding and infinite-length encoding.
-- The cddl tool always generates fix-length encode but some parsers only accept infinite-length encode.
-- rewriteList rewrites those messages.
rewriteList :: ByteString -> ByteString
rewriteList bs = case term of
    (TList [TInt 4, TList [TInt 1, TList l]]) -> recodeTxSubmission 1 l   -- MsgReplyTxIds
    (TList [TInt 4, TList [TInt 2, TList l]]) -> recodeTxSubmission 2 l   -- MsgRequestTxs
    (TList [TInt 4, TList [TInt 3, TList l]]) -> recodeTxSubmission 3 l   -- MsgReplyTxs
    _ -> bs
    where
        term = case deserialiseFromBytes decodeTerm bs of
            Right (rest, t) | BSL.null rest -> t
            Right (_, _)                    -> error "rewriteList : trailing Bytes"
            Left err                        -> error $ show ("rewriteList : decoding error", err)
        recodeTxSubmission tag l = toLazyByteString $ encodeTerm (TList [TInt 4, TList [TInt tag, TListI l]])

-- | Split the ByteString into the tag-word and the rest.
decodeTopTerm :: ByteString -> (Word, ByteString)
decodeTopTerm input
    = case deserialiseFromBytes (decodeListLenOf 2 >> decodeWord) input of
        Right (bs, tag) -> (tag, bs)
        Left err -> throw err

-- Decode a message. Throw an error if the message is not decodeable.
decodeMsg :: (Word, ByteString) -> IO ()
decodeMsg (tag, input) = case tag of
    0 -> tryParsers ["chainSync"]             chainSyncParsers
    -- 1 & 2 were used for ReqResp and PingPong
    3 -> tryParsers ["blockFetch"]            blockFetchParsers
    4 -> tryParsers ["txSubmission"]          txSubmissionParsers
    5 -> tryParsers ["handshake"]             handshakeParsers
    6 -> tryParsers ["localTxSubmission"]     localTxSubmissionParsers
    7 -> tryParsers ["localStateQuery"]       localStateQueryParsers
    _ -> error "unkown tag"
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
            DecodeDone _msg _rest -> error "runCodec: codec is DecodeDone"
            DecodeFail _f         -> error "runCodec: codec is DecodeFail"
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
              runCS (ClientAgency CS.TokIdle)
            , runCS (ServerAgency (CS.TokNext TokCanAwait))
            , runCS (ClientAgency CS.TokIdle)
            , runCS (ServerAgency CS.TokIntersect)
            , runCS (ServerAgency CS.TokIntersect)
            ]

        runBlockFetch = run codecBF

        blockFetchParsers = [
              runBlockFetch (ClientAgency BlockFetch.TokIdle)
            , runBlockFetch (ServerAgency BlockFetch.TokBusy)
            , runBlockFetch (ServerAgency BlockFetch.TokStreaming)
            ]

        -- Use 'nodeToNodeHandshakeCodec' and 'nodeToClientHandshakeCodec'
        runHandshake = run (versionNumberHandshakeCodec :: MonoCodec HS)
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
