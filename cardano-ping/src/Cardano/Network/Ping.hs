{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Network.Ping
  ( PingOpts(..)
  , LogMsg(..)
  , NodeVersion(..)
  , HandshakeFailure(..)
  , StatPoint(..)
  , mainnetMagic
  , pingClient
  , logger
  , supportedNodeToNodeVersions
  , supportedNodeToClientVersions
  ) where

import           Control.Exception (bracket)
import           Control.Monad (replicateM, unless, when)
import           Control.Concurrent.Class.MonadSTM.Strict ( MonadSTM(atomically), takeTMVar, StrictTMVar )
import           Control.Monad.Class.MonadTime.SI (UTCTime, diffTime, MonadMonotonicTime(getMonotonicTime), MonadTime(getCurrentTime), Time)
import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Data.Aeson (Value, ToJSON(toJSON), object, encode, KeyValue((.=)))
import           Data.Bits (clearBit, setBit, testBit)
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe (fromMaybe,)
import           Data.TDigest (insert, maximumValue, minimumValue, tdigest, mean, quantile, stddev, TDigest)
import           Data.Text (unpack)
import           Data.Word (Word16, Word32)
import           Network.Mux.Bearer (MakeBearer (..), makeSocketBearer)
import           Network.Mux.Timeout (TimeoutFn, withTimeoutSerial)
import           Network.Mux.Types (MuxSDUHeader(MuxSDUHeader, mhTimestamp, mhDir, mhLength, mhNum), MiniProtocolNum(..), MiniProtocolDir(InitiatorDir), MuxBearer(read, write), MuxSDU(..), RemoteClockModel(RemoteClockModel))
import           Network.Socket (AddrInfo, StructLinger (..))
import           Text.Printf (printf)

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Control.Monad.Class.MonadTimer as MT
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS.Char
import qualified Data.List as L
import qualified Network.Socket as Socket
import qualified System.IO as IO

data PingOpts = PingOpts
  { pingOptsCount    :: Word32
    -- ^ Number of messages to send to the server
  , pingOptsHost     :: Maybe String
    -- ^ The host to connect to
  , pingOptsUnixSock :: Maybe String
    -- ^ The unix socket to connect to
  , pingOptsPort     :: String
    -- ^ The port to connect to
  , pingOptsMagic    :: Word32
    -- ^ The network magic to use for the connection
  , pingOptsJson     :: Bool
    -- ^ Print output in JSON
  , pingOptsQuiet    :: Bool
    -- ^ Less verbose output
  } deriving (Eq, Show)

mainnetMagic :: Word32
mainnetMagic = 764824073

handshakeNum ::  MiniProtocolNum
handshakeNum = MiniProtocolNum 0

keepaliveNum :: MiniProtocolNum
keepaliveNum = MiniProtocolNum 8

nodeToClientVersionBit :: Int
nodeToClientVersionBit = 15

data LogMsg = LogMsg ByteString
            | LogEnd

logger :: StrictTMVar IO LogMsg -> Bool -> IO ()
logger msgQueue json = go True
  where
    go first = do
      msg <- atomically $ takeTMVar  msgQueue
      case msg of
        LogMsg bs -> do
          let bs' = case (json, first) of
                (True, False)  -> LBS.Char.pack ",\n" <> bs
                (True, True)   -> LBS.Char.pack "{ \"pongs\": [ " <> bs
                (False, True)  -> LBS.Char.pack "   timestamp,                         host,                          cookie,  sample,  median,     p90,    mean,     min,     max,     std\n" <> bs
                (False, False) -> bs

          LBS.Char.putStr bs'
          go False
        LogEnd -> when json $ IO.putStrLn "] }"

supportedNodeToNodeVersions :: Word32 -> [NodeVersion]
supportedNodeToNodeVersions magic =
  [ NodeToNodeVersionV7  magic False
  , NodeToNodeVersionV8  magic False
  , NodeToNodeVersionV9  magic False
  , NodeToNodeVersionV10 magic False
  ]

supportedNodeToClientVersions :: Word32 -> [NodeVersion]
supportedNodeToClientVersions magic =
  [ NodeToClientVersionV9  magic
  , NodeToClientVersionV10 magic
  , NodeToClientVersionV11 magic
  , NodeToClientVersionV12 magic
  , NodeToClientVersionV13 magic
  , NodeToClientVersionV14 magic
  ]

data NodeVersion
  = NodeToClientVersionV9  Word32
  | NodeToClientVersionV10 Word32
  | NodeToClientVersionV11 Word32
  | NodeToClientVersionV12 Word32
  | NodeToClientVersionV13 Word32
  | NodeToClientVersionV14 Word32
  | NodeToNodeVersionV1    Word32
  | NodeToNodeVersionV2    Word32
  | NodeToNodeVersionV3    Word32
  | NodeToNodeVersionV4    Word32 Bool
  | NodeToNodeVersionV5    Word32 Bool
  | NodeToNodeVersionV6    Word32 Bool
  | NodeToNodeVersionV7    Word32 Bool
  | NodeToNodeVersionV8    Word32 Bool
  | NodeToNodeVersionV9    Word32 Bool
  | NodeToNodeVersionV10   Word32 Bool
  deriving (Eq, Ord, Show)

keepAliveReqEnc :: NodeVersion -> Word16 -> CBOR.Encoding
keepAliveReqEnc v cookie | v >= NodeToNodeVersionV7 minBound minBound =
        CBOR.encodeListLen 2
    <>  CBOR.encodeWord 0
    <>  CBOR.encodeWord16 cookie
keepAliveReqEnc _ cookie =
        CBOR.encodeWord 0
    <>  CBOR.encodeWord16 cookie

keepAliveReq :: NodeVersion -> Word16 -> ByteString
keepAliveReq v c = CBOR.toLazyByteString $ keepAliveReqEnc v c

keepAliveDone :: NodeVersion -> ByteString
keepAliveDone v | v >= NodeToNodeVersionV7 minBound minBound =
    CBOR.toLazyByteString $
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 2
keepAliveDone _ =
    CBOR.toLazyByteString $
      CBOR.encodeWord 2


handshakeReqEnc :: [NodeVersion] -> CBOR.Encoding
handshakeReqEnc [] = error "null version list"
handshakeReqEnc versions =
      CBOR.encodeListLen 2
  <>  CBOR.encodeWord 0
  <>  CBOR.encodeMapLen (fromIntegral $ L.length versions)
  <>  mconcat [ encodeVersion v
              | v <- versions
              ]
  where
    encodeVersion :: NodeVersion -> CBOR.Encoding
    encodeVersion (NodeToClientVersionV9 magic) =
          CBOR.encodeWord (9 `setBit` nodeToClientVersionBit)
      <>  CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToClientVersionV10 magic) =
          CBOR.encodeWord (10 `setBit` nodeToClientVersionBit)
      <>  CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToClientVersionV11 magic) =
          CBOR.encodeWord (11 `setBit` nodeToClientVersionBit)
      <>  CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToClientVersionV12 magic) =
          CBOR.encodeWord (12 `setBit` nodeToClientVersionBit)
      <>  CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToClientVersionV13 magic) =
          CBOR.encodeWord (13 `setBit` nodeToClientVersionBit)
      <>  CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToClientVersionV14 magic) =
          CBOR.encodeWord (14 `setBit` nodeToClientVersionBit)
      <>  CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToNodeVersionV1 magic) =
          CBOR.encodeWord 1
      <>  CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToNodeVersionV2 magic) =
          CBOR.encodeWord 2
      <>  CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToNodeVersionV3 magic) =
          CBOR.encodeWord 3
      <>  CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToNodeVersionV4  magic mode) = encodeWithMode 4  magic mode
    encodeVersion (NodeToNodeVersionV5  magic mode) = encodeWithMode 5  magic mode
    encodeVersion (NodeToNodeVersionV6  magic mode) = encodeWithMode 6  magic mode
    encodeVersion (NodeToNodeVersionV7  magic mode) = encodeWithMode 7  magic mode
    encodeVersion (NodeToNodeVersionV8  magic mode) = encodeWithMode 8  magic mode
    encodeVersion (NodeToNodeVersionV9  magic mode) = encodeWithMode 9  magic mode
    encodeVersion (NodeToNodeVersionV10 magic mode) = encodeWithMode 10 magic mode


    encodeWithMode :: Word -> Word32 -> Bool -> CBOR.Encoding
    encodeWithMode vn magic mode =
          CBOR.encodeWord vn
      <>  CBOR.encodeListLen 2
      <>  CBOR.encodeInt (fromIntegral magic)
      <>  CBOR.encodeBool mode

handshakeReq :: [NodeVersion] -> ByteString
handshakeReq []       = LBS.empty
handshakeReq versions = CBOR.toLazyByteString $ handshakeReqEnc versions

data HandshakeFailure
  = UnknownVersionInRsp Word
  | UnknownKey Word
  | UnknownTag Word
  | VersionMissmath [Word]
  | DecodeError Word String
  | Refused Word String
  deriving Show

newtype KeepAliveFailure = KeepAliveFailureKey Word deriving Show

keepAliveRspDec :: NodeVersion
                -> CBOR.Decoder s (Either KeepAliveFailure Word16)
keepAliveRspDec v | v >= NodeToNodeVersionV7 minBound minBound = do
  len <- CBOR.decodeListLen
  key <- CBOR.decodeWord
  case (len, key) of
    (2, 1) -> Right <$> CBOR.decodeWord16
    (_, k) -> return $ Left $ KeepAliveFailureKey k
keepAliveRspDec _ = do
  key <- CBOR.decodeWord
  case key of
    1 -> Right <$> CBOR.decodeWord16
    k -> return $ Left $ KeepAliveFailureKey k

handshakeDec :: CBOR.Decoder s (Either HandshakeFailure NodeVersion)
handshakeDec = do
  _ <- CBOR.decodeListLen
  key <- CBOR.decodeWord
  case key of
    1 -> decodeVersion
    2 -> do
      _ <- CBOR.decodeListLen
      tag <- CBOR.decodeWord
      case tag of
        0 -> do -- VersionMismatch
          len <- CBOR.decodeListLen
          x <- replicateM len CBOR.decodeWord
          return $ Left $ VersionMissmath x
        1 -> do -- HandshakeDecodeError
          vn <- CBOR.decodeWord
          msg <- unpack <$> CBOR.decodeString
          return $ Left $ DecodeError vn msg
        2 -> do -- Refused
          vn <- CBOR.decodeWord
          msg <- unpack <$> CBOR.decodeString
          return $ Left $ Refused vn msg
        _ -> return $ Left $ UnknownTag tag

    k -> return $ Left $ UnknownKey k
  where
    decodeVersion :: CBOR.Decoder s (Either HandshakeFailure NodeVersion)
    decodeVersion = do
      version <- CBOR.decodeWord
      let cb = version `clearBit` nodeToClientVersionBit
      let tb = version `testBit`  nodeToClientVersionBit
      case (cb, tb) of
        (7,  False) -> decodeWithMode NodeToNodeVersionV7
        (8,  False) -> decodeWithMode NodeToNodeVersionV8
        (9,  False) -> decodeWithMode NodeToNodeVersionV9
        (10, False) -> decodeWithMode NodeToNodeVersionV10
        (9,  True)  -> Right . NodeToClientVersionV9 <$> CBOR.decodeWord32
        (10, True)  -> Right . NodeToClientVersionV10 <$> CBOR.decodeWord32
        (11, True)  -> Right . NodeToClientVersionV11 <$> CBOR.decodeWord32
        (12, True)  -> Right . NodeToClientVersionV12 <$> CBOR.decodeWord32
        (13, True)  -> Right . NodeToClientVersionV13 <$> CBOR.decodeWord32
        (14, True)  -> Right . NodeToClientVersionV14 <$> CBOR.decodeWord32
        _           -> return $ Left $ UnknownVersionInRsp version

    decodeWithMode :: (Word32 -> Bool -> NodeVersion) -> CBOR.Decoder s (Either HandshakeFailure NodeVersion)
    decodeWithMode vnFun = do
      _ <- CBOR.decodeListLen
      magic <- CBOR.decodeWord32
      Right . vnFun magic <$> CBOR.decodeBool

wrap :: MiniProtocolNum -> MiniProtocolDir -> LBS.ByteString -> MuxSDU
wrap ptclNum ptclDir blob = MuxSDU
  { msHeader = MuxSDUHeader
    { mhTimestamp = RemoteClockModel 0
    , mhNum       = ptclNum
    , mhDir       = ptclDir
    , mhLength    = fromIntegral $ LBS.length blob
    }
  , msBlob = blob
  }


data StatPoint = StatPoint
  { spTimestamp :: UTCTime
  , spHost      :: String
  , spCookie    :: Word16
  , spSample    :: Double
  , spMedian    :: Double
  , spP90       :: Double
  , spMean      :: Double
  , spMin       :: Double
  , spMax       :: Double
  , spStd       :: Double
  }

instance Show StatPoint where
  show :: StatPoint -> String
  show StatPoint {..} =
    printf "%36s, %-28s, %7d, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f"
      (show spTimestamp) spHost spCookie spSample spMedian spP90 spMean spMin spMax spStd

instance ToJSON StatPoint where
  toJSON :: StatPoint -> Value
  toJSON StatPoint {..} =
    object
      [ "timestamp" .= spTimestamp
      , "host"      .= spHost
      , "cookie"    .= spCookie
      , "sample"    .= spSample
      , "median"    .= spMedian
      , "p90"       .= spP90
      , "mean"      .= spMean
      , "min"       .= spMin
      , "max"       .= spMax
      ]

toStatPoint :: UTCTime -> String -> Word16 -> Double -> TDigest 5 -> StatPoint
toStatPoint ts host cookie sample td =
  StatPoint
    { spTimestamp = ts
    , spHost      = host
    , spCookie    = cookie
    , spSample    = sample
    , spMedian    = quantile' 0.5
    , spP90       = quantile' 0.9
    , spMean      = mean'
    , spMin       = minimumValue td
    , spMax       = maximumValue td
    , spStd       = stddev'
    }
  where
    quantile' :: Double -> Double
    quantile' q = fromMaybe 0 (quantile q td)

    mean' :: Double
    mean' = fromMaybe 0 (mean td)

    stddev' :: Double
    stddev' = fromMaybe 0 (stddev td)

pingClient :: Tracer IO LogMsg -> Tracer IO String -> PingOpts -> [NodeVersion] -> AddrInfo -> IO ()
pingClient stdout stderr PingOpts{pingOptsQuiet, pingOptsJson, pingOptsCount} versions peer = bracket
  (Socket.socket (Socket.addrFamily peer) Socket.Stream Socket.defaultProtocol)
  Socket.close
  (\sd -> withTimeoutSerial $ \timeoutfn -> do
    when (Socket.addrFamily peer /= Socket.AF_UNIX) $ do
      Socket.setSocketOption sd Socket.NoDelay 1
      Socket.setSockOpt sd Socket.Linger
        StructLinger
          { sl_onoff  = 1
          , sl_linger = 0
          }

    !t0_s <- getMonotonicTime
    Socket.connect sd (Socket.addrAddress peer)
    !t0_e <- getMonotonicTime
    peerStr <- peerString
    unless pingOptsQuiet $ printf "%s network rtt: %.3f\n" peerStr $ toSample t0_e t0_s

    let timeout = 30
    bearer <- getBearer makeSocketBearer timeout nullTracer sd

    !t1_s <- write bearer timeoutfn $ wrap handshakeNum InitiatorDir (handshakeReq versions)
    (msg, !t1_e) <- nextMsg bearer timeoutfn handshakeNum
    unless pingOptsQuiet $ printf "%s handshake rtt: %s\n" peerStr (show $ diffTime t1_e t1_s)

    case CBOR.deserialiseFromBytes handshakeDec msg of
      Left err -> eprint $ printf "%s Decoding error %s" peerStr (show err)
      Right (_, Left err) -> eprint $ printf "%s Protocol error %s" peerStr (show err)
      Right (_, Right version) -> do
        unless pingOptsQuiet $ printf "%s Negotiated version %s\n" peerStr (show version)
        keepAlive bearer timeoutfn peerStr version (tdigest []) 0
        -- send terminating message
        _ <- write bearer timeoutfn $ wrap keepaliveNum InitiatorDir (keepAliveDone version)
        -- protocol idle timeout
        MT.threadDelay 5

  )
  where

    peerString :: IO String
    peerString =
      case Socket.addrFamily peer of
        Socket.AF_UNIX -> return $ show $ Socket.addrAddress peer
        _ -> do
          (Just host, Just port) <-
            Socket.getNameInfo
              [Socket.NI_NUMERICHOST, Socket.NI_NUMERICSERV]
              True True (Socket.addrAddress peer)
          return $ host <> ":" <> port

    toSample :: Time -> Time -> Double
    toSample t_e t_s = realToFrac $ diffTime t_e t_s

    eprint :: String -> IO ()
    eprint = traceWith stderr

    nextMsg ::  MuxBearer IO -> TimeoutFn IO -> MiniProtocolNum -> IO (LBS.ByteString, Time)
    nextMsg bearer timeoutfn ptclNum = do
      (sdu, t_e) <- Network.Mux.Types.read bearer timeoutfn
      if mhNum (msHeader sdu) == ptclNum
        then return (msBlob sdu, t_e)
        else nextMsg bearer timeoutfn ptclNum

    keepAlive :: MuxBearer IO
              -> TimeoutFn IO
              -> String
              -> NodeVersion
              -> TDigest 5
              -> Word32
              -> IO ()
    keepAlive _ _ _ _ _ cookie | cookie == pingOptsCount = return ()
    keepAlive bearer timeoutfn peerStr version td !cookie = do
      let cookie16 = fromIntegral cookie
      !t_s <- write bearer timeoutfn $ wrap keepaliveNum InitiatorDir (keepAliveReq version cookie16)
      (!msg, !t_e) <- nextMsg bearer timeoutfn keepaliveNum
      let rtt = toSample t_e t_s
          td' = insert rtt td
      case CBOR.deserialiseFromBytes (keepAliveRspDec version) msg of
        Left err -> eprint $ printf "%s keepalive decoding error %s" peerStr (show err)
        Right (_, Left err) -> eprint $ printf "%s keepalive protocol error %s" peerStr (show err)
        Right (_, Right cookie') -> do
          when (cookie' /= cookie16) $ eprint $ printf "%s cookie missmatch %d /= %d"
            peerStr cookie' cookie

          now <- getCurrentTime
          let point = toStatPoint now peerStr cookie16 rtt td'
          if pingOptsJson
            then traceWith stdout $ LogMsg (encode point)
            else traceWith stdout $ LogMsg $ LBS.Char.pack $ show point <> "\n"
          MT.threadDelay 1
          keepAlive bearer timeoutfn peerStr version td' (cookie + 1)
