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

import Test.QuickCheck
import Test.Tasty (defaultMain, TestTree, testGroup, adjustOption)
import Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))

import qualified Codec.Serialise.Class as Serialise
import Codec.CBOR.Decoding as CBOR (Decoder, decodeBytes ,decodeListLenOf, decodeWord)
import Codec.CBOR.Encoding (encodeBytes, encodeListLen, encodeWord)
import Codec.CBOR.Term as CBOR
import Codec.CBOR.Read
import Codec.CBOR.Write (toLazyByteString)

import Ouroboros.Network.Block (HeaderHash, Point, Tip, encodeTip, decodeTip)
import Ouroboros.Network.Testing.ConcreteBlock (BlockHeader (..), Block)

import Ouroboros.Network.Protocol.ChainSync.Type as CS
import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import Ouroboros.Network.Protocol.ChainSync.Test ()
import Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetch)
import Ouroboros.Network.Protocol.BlockFetch.Test ()
import Ouroboros.Network.Protocol.BlockFetch.Type as BlockFetch
import Ouroboros.Network.Protocol.Handshake.Codec (codecHandshake)
import Ouroboros.Network.Protocol.Handshake.Type as Handshake
import Ouroboros.Network.Protocol.Handshake.Test (VersionNumber)
import Ouroboros.Network.Protocol.TxSubmission.Codec (codecTxSubmission)
import Ouroboros.Network.Protocol.TxSubmission.Type as TxSubmission
import Ouroboros.Network.Protocol.TxSubmission.Test (TxId, Tx)

import Network.TypedProtocol.Codec
import Network.TypedProtocol.ReqResp.Codec.Cbor (codecReqResp)
import Network.TypedProtocol.ReqResp.Type as ReqResp
import Network.TypedProtocol.PingPong.Type as PingPong
import Network.TypedProtocol.PingPong.Codec.Cbor (codecPingPong)

-- These files must be in place:
specFile :: FilePath
specFile = "test/messages.cddl"

cddlTool :: FilePath
cddlTool = "cddl"

diag2cborTool :: FilePath
diag2cborTool = "diag2cbor.rb"

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  adjustOption (const $ QuickCheckMaxSize 10) $ testGroup "messages.cddl-spec"
  [
-- These tests call the CDDL-tool to parse an arbitray message.
-- The parser of the CDDL-tool is slow (exponential runtime & space).
    testProperty "encode ChainSync"             prop_specCS
  , testProperty "encode Request-Response"      prop_specReqResp
  , testProperty "encode BlockFetch"            prop_specBF
  , testProperty "encode TxSubmission"          prop_specTxSubmission
  , testProperty "encode Handshake"             prop_specHandshake
  , testProperty "encode PingPong"              prop_specPingPong
-- Test the parsers with CDDL-generated messages.
  , testProperty "generate and decode" $ ioProperty $ generateAndDecode 100 specFile
  ]

-- The concrete/monomorphic types used for the test.
type MonoCodec x = Codec x Codec.CBOR.Read.DeserialiseFailure IO ByteString
type CS = ChainSync BlockHeader (Tip BlockHeader)
type RR = ReqResp DummyBytes DummyBytes
type BF = BlockFetch Block
type HS = Handshake VersionNumber CBOR.Term
type TS = TxSubmission TxId Tx

codecCS :: MonoCodec CS
codecCS = codecChainSync Serialise.encode (fmap const Serialise.decode)
                         Serialise.encode (Serialise.decode :: CBOR.Decoder s (Point BlockHeader))
                         (encodeTip Serialise.encode) (decodeTip Serialise.decode)

codecBF :: MonoCodec BF
codecBF = codecBlockFetch Serialise.encode (fmap const Serialise.decode) Serialise.encode Serialise.decode

codecTS :: MonoCodec TS
codecTS = codecTxSubmission Serialise.encode Serialise.decode Serialise.encode Serialise.decode

prop_specCS :: AnyMessageAndAgency CS -> Property
prop_specCS = prop_CDDLSpec (0, codecCS)

prop_specReqResp :: AnyMessageAndAgency RR -> Property
prop_specReqResp = prop_CDDLSpec (1, codecReqResp)

prop_specPingPong :: AnyMessageAndAgency PingPong -> Property
prop_specPingPong = prop_CDDLSpec (2, codecPingPong)

prop_specBF :: AnyMessageAndAgency BF -> Property
prop_specBF = prop_CDDLSpec (3, codecBF)

prop_specTxSubmission :: AnyMessageAndAgency TS -> Property
prop_specTxSubmission = prop_CDDLSpec (4, codecTS)

prop_specHandshake :: AnyMessageAndAgency HS -> Property
prop_specHandshake = prop_CDDLSpec (5, codecHandshake)

prop_CDDLSpec :: (Word, MonoCodec ps) -> AnyMessageAndAgency ps -> Property
prop_CDDLSpec (tagWord, codec) (AnyMessageAndAgency agency msg)
    = ioProperty $ do
--         print $ BSL.unpack wrappedMsg
         validateCBOR specFile wrappedMsg
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
    0 -> tryParsers ["chainSync"]     chainSyncParsers
    1 -> tryParsers ["reqResp"]       reqRespParsers
    2 -> tryParsers ["pingPong"]      pingPongParsers
    3 -> tryParsers ["blockFetch"]    blockFetchParsers
    4 -> tryParsers ["txSubmission"]  txSubmissionParsers
    5 -> tryParsers ["handshake"]     handshakeParsers
    _ -> error "unkown tag"
    where
        -- typed-protocols codecs are parameterized on the tokens which
        -- serve as singletons to identify the state types. For each message
        -- we know which protocol it should belong to, but not which state,
        -- so we just run all of them and expect that at least one passes.
        -- This isn't ideal but it's better than nothing. In case they all
        -- fail, error messages for each are given in order.
        tryParsers :: [String] -> [IO (Maybe String)] -> IO ()
        tryParsers traces [] = error $ intercalate "\n" (reverse ("parse failed: " : traces))
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

        runReqResp = run (codecReqResp :: MonoCodec RR)
        reqRespParsers = [
              runReqResp (ClientAgency ReqResp.TokIdle)
            , runReqResp (ServerAgency ReqResp.TokBusy)
            ]

        runPingPong = run (codecPingPong :: MonoCodec PingPong)
        pingPongParsers = [
              runPingPong (ClientAgency PingPong.TokIdle)
            , runPingPong (ServerAgency PingPong.TokBusy)
            ]

        runBlockFetch = run codecBF

        blockFetchParsers = [
              runBlockFetch (ClientAgency BlockFetch.TokIdle)
            , runBlockFetch (ServerAgency BlockFetch.TokBusy)
            , runBlockFetch (ServerAgency BlockFetch.TokStreaming)
            ]

        runHandshake = run (codecHandshake :: MonoCodec HS)
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

instance (Arbitrary req, Arbitrary resp) => Arbitrary (AnyMessageAndAgency (ReqResp req resp)) where
  arbitrary = oneof
    [ AnyMessageAndAgency (ClientAgency ReqResp.TokIdle) . MsgReq  <$> arbitrary
    , AnyMessageAndAgency (ServerAgency ReqResp.TokBusy) . MsgResp <$> arbitrary
    , pure $ AnyMessageAndAgency (ClientAgency ReqResp.TokIdle) ReqResp.MsgDone
    ]

instance (Show req, Show resp) => Show (AnyMessageAndAgency (ReqResp req resp)) where
  show (AnyMessageAndAgency _ msg) = show msg

instance (Eq req, Eq resp) => Eq (AnyMessage (ReqResp req resp)) where
  AnyMessage (MsgReq r1)  == AnyMessage (MsgReq r2)  = r1 == r2
  AnyMessage (MsgResp r1) == AnyMessage (MsgResp r2) = r1 == r2
  AnyMessage ReqResp.MsgDone      == AnyMessage ReqResp.MsgDone      = True
  _                               == _                               = False

instance Arbitrary (AnyMessageAndAgency PingPong) where
  arbitrary = elements
    [ AnyMessageAndAgency (ClientAgency PingPong.TokIdle) MsgPing
    , AnyMessageAndAgency (ServerAgency PingPong.TokBusy) MsgPong
    , AnyMessageAndAgency (ClientAgency PingPong.TokIdle) PingPong.MsgDone
    ]

instance Eq (AnyMessage PingPong) where
  AnyMessage MsgPing == AnyMessage MsgPing = True
  AnyMessage MsgPong == AnyMessage MsgPong = True
  AnyMessage PingPong.MsgDone == AnyMessage PingPong.MsgDone = True
  _                  ==                  _ = False

instance Show (AnyMessageAndAgency PingPong) where
  show (AnyMessageAndAgency _ msg) = show msg
