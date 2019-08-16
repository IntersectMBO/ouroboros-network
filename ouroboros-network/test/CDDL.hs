{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main
where

import System.Exit (ExitCode(..))
import System.Process.ByteString.Lazy
import Control.Monad
import Control.Exception.Base (throw)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 as Char8 (putStrLn, unpack, lines)
import Data.List (intercalate)
import Data.Word (Word32)
import qualified Codec.Serialise.Class as Serialise
import Codec.CBOR.Decoding (decodeWord, decodeListLenOf, decodeBytes)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR (Term)
import Codec.CBOR.Read

import Ouroboros.Network.Block (Point)
import Ouroboros.Network.Protocol.ChainSync.Type as CS
import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import Network.TypedProtocol.ReqResp.Type as ReqResp
import Network.TypedProtocol.ReqResp.Codec.Cbor (codecReqResp)
import Network.TypedProtocol.PingPong.Type as PingPong
import Network.TypedProtocol.PingPong.Codec.Cbor (codecPingPong)
import Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetch)
import Ouroboros.Network.Protocol.BlockFetch.Type as BlockFetch
import Ouroboros.Network.Protocol.Handshake.Codec (codecHandshake)
import Ouroboros.Network.Protocol.Handshake.Type as Handshake
import Ouroboros.Network.Block (HeaderHash)

import Network.TypedProtocol.Codec

-- the concrete types used for the test
type CS = ChainSync DummyBytes (Point DummyBytes)
type RR = ReqResp DummyBytes DummyBytes
type BF = BlockFetch DummyBytes
type HS = Handshake Word32 CBOR.Term

main :: IO ()
main = generateAndDecode "cddl" "diag2cbor.rb" "test/messages.cddl" 100

generateAndDecode :: FilePath -> FilePath -> FilePath -> Int -> IO ()
generateAndDecode cddlCmd diag2cborCmd cddlSpec rounds = do
    diags <- generateCBORDiag cddlCmd cddlSpec rounds
    forM_ diags $ \diag -> do
        Char8.putStrLn diag
        bytes <- diagToBytes diag2cborCmd diag
        decodeMsg $ decodeTopTerm bytes

generateCBORDiag :: FilePath -> FilePath -> Int -> IO [ByteString]
generateCBORDiag cddlCmd cddlSpec rounds = do
    result <- readProcessWithExitCode cddlCmd [cddlSpec, "generate", show rounds] BS.empty
    case result of
        (ExitFailure _, _, err) -> error $ Char8.unpack err
        (ExitSuccess, diags, err) -> do
            Char8.putStrLn err
            return $ Char8.lines diags

diagToBytes :: FilePath -> ByteString -> IO ByteString
diagToBytes diag2cborCmd diag = do
    result <- readProcessWithExitCode diag2cborCmd ["-"] diag
    case result of
        (ExitFailure _ , _, err) -> error $ Char8.unpack err
        (ExitSuccess, bytes, _) -> return bytes

decodeFile :: FilePath -> IO ()
decodeFile f =
    BS.readFile f >>= decodeMsg . decodeTopTerm

data DummyBytes = DummyBytes
instance Serialise.Serialise DummyBytes where
    encode _ = error "encode Serialise DummyBytes"
    decode = decodeBytes >> return DummyBytes
type instance HeaderHash DummyBytes = ()

type MonoCodec x = Codec x Codec.CBOR.Read.DeserialiseFailure IO ByteString

-- | Split the ByteString into the tag-word and the rest.
decodeTopTerm :: ByteString -> (Word, ByteString)
decodeTopTerm input
    = case deserialiseFromBytes (decodeListLenOf 2 >> decodeWord) input of
        Right (bs,tag) -> (tag,bs)
        Left err -> throw err

-- Decode a message. Throw an error if the message is not decodeable.
decodeMsg :: (Word, ByteString) -> IO ()
decodeMsg (tag, input) = case tag of
    0 -> tryParsers ["chainSync"]  chainSyncParsers
    1 -> tryParsers ["reqResp"]    reqRespParsers
    2 -> tryParsers ["pingPong"]   pingPongParsers
    3 -> tryParsers ["blockFetch"] blockFetchParsers
    4 -> return () -- "txSubmissionMessage" in branch
    5 -> tryParsers ["handshake"]  handshakeParsers
    -- Point and list of point.
    6 -> case deserialiseFromBytes decodePoint input of
           Left err -> error ("point: " ++ show err)
           Right (_, _) -> pure ()
    7 -> case deserialiseFromBytes decodePoints input of
           Left err -> error ("points: " ++ show err)
           Right (_, _) -> pure ()
    _ -> error "unkown tag"
    where

        decodePoint :: CBOR.Decoder s (Point DummyBytes)
        decodePoint = Serialise.decode

        decodePoints :: CBOR.Decoder s [Point DummyBytes]
        decodePoints = decodeList decodePoint

        decodeList :: CBOR.Decoder s a -> CBOR.Decoder s [a]
        decodeList dec = do
          mn <- CBOR.decodeListLenOrIndef
          case mn of
            Nothing -> CBOR.decodeSequenceLenIndef (flip (:)) [] reverse   dec
            Just n  -> CBOR.decodeSequenceLenN     (flip (:)) [] reverse n dec

        -- typed-protocols codecs are parameterized on the tokens which
        -- serve as singletons to identify the state types. For each message
        -- we know which protocol it should belong to, but not which state,
        -- so we just run all of them at expect that at least one passes.
        -- This isn't ideal but it's better than nothing. In case they all
        -- fail, error messages for each are given in order.
        tryParsers :: [String] -> [IO (Maybe String)] -> IO ()
        tryParsers traces [] = error $ intercalate "\n" (reverse ("parse failed: " : traces))
        tryParsers traces (h:t) = h >>= \case
            Nothing  -> return ()
            Just str -> tryParsers (str : traces) t

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

        codecCS :: MonoCodec CS
        codecCS = codecChainSync Serialise.encode (fmap const Serialise.decode)
                                 Serialise.encode (Serialise.decode :: CBOR.Decoder s (Point DummyBytes))

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

        codecBF :: MonoCodec BF
        codecBF = codecBlockFetch Serialise.encode (fmap const Serialise.decode)
                                  Serialise.encode (Serialise.decode :: CBOR.Decoder s (HeaderHash DummyBytes))

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
