{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes        #-}

module Main (main) where

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Write    as CBOR
import           Control.Exception
import           Control.Monad (replicateM, when, unless)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer hiding (timeout)
import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Data.Aeson hiding (Options, json)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSC (pack, putStr)
import           Data.List (foldl')
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Word
import           Data.TDigest
import           Data.Text (unpack)
import           Network.Socket ( AddrInfo)
import qualified Network.Socket as Socket
import           Text.Printf
import           System.Environment (getArgs, getProgName)
import           System.Exit
import           System.Console.GetOpt
import           System.IO (hFlush, hPutStr, hPrint, stderr, stdout)

import           Network.Mux.Bearer.Socket
import           Network.Mux.Types
import           Network.Mux.Timeout

mainnetMagic :: Word32
mainnetMagic = 764824073

handshakeNum ::  MiniProtocolNum
handshakeNum = MiniProtocolNum 0

keepaliveNum :: MiniProtocolNum
keepaliveNum = MiniProtocolNum 8

data Flag = CountF String | HelpF | HostF String | PortF String | MagicF String | QuietF | JsonF deriving Show

optionDescriptions :: [OptDescr Flag]
optionDescriptions = [
    Option "c" ["count"]  (ReqArg CountF "count")  "number of pings to send",
    Option "" ["help"]  (NoArg HelpF)  "print help",
    Option "h" ["host"]  (ReqArg HostF "host")  "hostname/ip, e.g relay.iohk.example",
    Option "m" ["magic"] (ReqArg MagicF "magic") ("magic, defaults to " ++ show mainnetMagic),
    Option "j" ["json"]  (NoArg JsonF ) "json output flag",
    Option "p" ["port"]  (ReqArg PortF "port") "portnumber, e.g 1234",
    Option "q" ["quiet"] (NoArg QuietF ) "quiet flag, csv/json only output."
  ]

data Options = Options {
      maxCount :: Word32
    , host     :: Maybe String
    , port     :: String
    , magic    :: Word32
    , json     :: Bool
    , quiet    :: Bool
    , help     :: Bool
    } deriving Show

defaultOpts :: Options
defaultOpts = Options {
      maxCount = maxBound
    , host     = Nothing
    , port     = "3001"
    , json     = False
    , quiet    = False
    , magic    = mainnetMagic
    , help     = False
    }

buildOptions ::  Flag -> Options -> Options
buildOptions (CountF c)   opt = opt { maxCount = Prelude.read c }
buildOptions HelpF        opt = opt { help = True }
buildOptions (HostF host) opt = opt { host = Just host }
buildOptions (PortF port) opt = opt { port = port }
buildOptions (MagicF m)   opt = opt { magic = Prelude.read m }
buildOptions JsonF        opt = opt { json = True }
buildOptions QuietF       opt = opt { quiet = True }

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
                                (True, False)  -> BSC.pack ",\n" <> bs
                                (True, True)   -> BSC.pack "{ \"pongs\": [ " <> bs
                                (False, True)  -> BSC.pack "                           timestamp,                 host,  cookie,  sample,  median,     p90,    mean,     min,     max,     std\n" <> bs
                                (False, False) -> bs

                 BSC.putStr bs'
                 go False
             LogEnd ->
                 when json $ putStrLn "] }"


main :: IO ()
main = do
    args <- getArgs
    let (flags, _, _ ) = getOpt RequireOrder optionDescriptions args
        options = foldr buildOptions defaultOpts flags
        hints = Socket.defaultHints { Socket.addrSocketType = Socket.Stream }

    when (help options) $ do
        progName <- getProgName
        putStrLn $ usageInfo progName optionDescriptions
        exitSuccess

    msgQueue <- newEmptyTMVarIO

    when (isNothing $ host options) $ do
        putStrLn "Specify host/ip with '-h <hostname>'"
        exitWith (ExitFailure 1)

    addresses <- Socket.getAddrInfo (Just hints) (host options) (Just $ port options)

    laid <- async $ logger msgQueue $ json options
    caids <- mapM (async . pingClient (Tracer $ doLog msgQueue) options) addresses
    res <- zip addresses <$> mapM waitCatch caids
    doLog msgQueue LogEnd
    wait laid
    case foldl' partition ([],[]) res of
         ([], _) -> do
             exitSuccess
         (es, []) -> do
             mapM_ (hPrint stderr) es
             exitWith (ExitFailure 1)
         (es, _) -> do
             unless (quiet options) $
               mapM_ (hPrint stderr) es
             exitSuccess

  where

    partition :: ([(AddrInfo, SomeException)], [AddrInfo])
              -> (AddrInfo, Either SomeException ())
              -> ([(AddrInfo, SomeException)], [AddrInfo])
    partition (es, as) (a, Left e) = ((a, e) : es, as)
    partition (es, as) (a, Right _) = (es, a : as)

    doLog :: StrictTMVar IO LogMsg -> LogMsg -> IO ()
    doLog msgQueue msg = atomically $ putTMVar msgQueue msg

data NodeToNodeVersion = NodeToNodeVersionV1 Word32
                       | NodeToNodeVersionV2 Word32
                       | NodeToNodeVersionV3 Word32
                       | NodeToNodeVersionV4 Word32 Bool
                       | NodeToNodeVersionV5 Word32 Bool
                       | NodeToNodeVersionV6 Word32 Bool
                       | NodeToNodeVersionV7 Word32 Bool
                       deriving (Eq, Ord, Show)

keepAliveReqEnc :: NodeToNodeVersion -> Word16 -> CBOR.Encoding
keepAliveReqEnc (NodeToNodeVersionV7 _ _) cookie =
       CBOR.encodeListLen 2
    <> CBOR.encodeWord 0
    <> CBOR.encodeWord16 cookie
keepAliveReqEnc _ cookie =
       CBOR.encodeWord 0
    <> CBOR.encodeWord16 cookie

keepAliveReq :: NodeToNodeVersion -> Word16 -> ByteString
keepAliveReq v c = CBOR.toLazyByteString $ keepAliveReqEnc v c


handshakeReqEnc :: [NodeToNodeVersion] -> CBOR.Encoding
handshakeReqEnc [] = error "null version list"
handshakeReqEnc versions =
       CBOR.encodeListLen 2
    <> CBOR.encodeWord 0
    <> CBOR.encodeMapLen (fromIntegral $ length versions)
    <> mconcat [ encodeVersion v
               | v <- versions
               ]
  where
    encodeVersion :: NodeToNodeVersion -> CBOR.Encoding
    encodeVersion (NodeToNodeVersionV1 magic) =
          CBOR.encodeWord 1
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToNodeVersionV2 magic) =
          CBOR.encodeWord 2
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToNodeVersionV3 magic) =
          CBOR.encodeWord 3
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToNodeVersionV4 magic mode) = encodeWithMode 4 magic mode
    encodeVersion (NodeToNodeVersionV5 magic mode) = encodeWithMode 5 magic mode
    encodeVersion (NodeToNodeVersionV6 magic mode) = encodeWithMode 6 magic mode
    encodeVersion (NodeToNodeVersionV7 magic mode) = encodeWithMode 7 magic mode


    encodeWithMode :: Word -> Word32 -> Bool -> CBOR.Encoding
    encodeWithMode vn magic mode =
          CBOR.encodeWord vn
       <> CBOR.encodeListLen 2
       <> CBOR.encodeInt (fromIntegral magic)
       <> CBOR.encodeBool mode

handshakeReq :: [NodeToNodeVersion] -> ByteString
handshakeReq [] = BL.empty
handshakeReq versions = CBOR.toLazyByteString $ handshakeReqEnc versions

data HandshakeFailure = UnknownVersionInRsp Word
                      | UnknownKey Word
                      | UnknownTag Word
                      | VersionMissmath [Word]
                      | DecodeError Word String
                      | Refused Word String
                      deriving Show

newtype KeepAliveFailure = KeepAliveFailureKey Word deriving Show

keepAliveRspDec :: NodeToNodeVersion
                -> CBOR.Decoder s (Either KeepAliveFailure Word16)
keepAliveRspDec (NodeToNodeVersionV7 _ _) = do
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

handshakeDec :: CBOR.Decoder s (Either HandshakeFailure NodeToNodeVersion)
handshakeDec = do
    _ <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case key of
         1 -> do
             decodeVersion
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
    decodeVersion :: CBOR.Decoder s (Either HandshakeFailure NodeToNodeVersion)
    decodeVersion = do
        version <- CBOR.decodeWord
        case version of
             1 -> Right . NodeToNodeVersionV1 <$> CBOR.decodeWord32
             2 -> Right . NodeToNodeVersionV2 <$> CBOR.decodeWord32
             3 -> Right . NodeToNodeVersionV3 <$> CBOR.decodeWord32
             4 -> decodeWithMode NodeToNodeVersionV4
             5 -> decodeWithMode NodeToNodeVersionV5
             6 -> decodeWithMode NodeToNodeVersionV6
             7 -> decodeWithMode NodeToNodeVersionV7
             v -> return $ Left $ UnknownVersionInRsp v

    decodeWithMode :: (Word32 -> Bool -> NodeToNodeVersion)
                   -> CBOR.Decoder s (Either HandshakeFailure NodeToNodeVersion)
    decodeWithMode vnFun = do
        _ <- CBOR.decodeListLen
        magic <- CBOR.decodeWord32
        Right . vnFun magic <$> CBOR.decodeBool

wrap :: MiniProtocolNum -> MiniProtocolDir -> BL.ByteString -> MuxSDU
wrap ptclNum ptclDir blob = MuxSDU {
      msHeader = MuxSDUHeader {
                mhTimestamp = RemoteClockModel 0,
                mhNum       = ptclNum,
                mhDir       = ptclDir,
                mhLength    = fromIntegral $ BL.length blob
              }
    , msBlob = blob
    }


data StatPoint = StatPoint {
      spTimestamp :: !UTCTime
    , spHost      :: !String
    , spCookie    :: !Word16
    , spSample    :: !Double
    , spMedian    :: !Double
    , spP90       :: !Double
    , spMean      :: !Double
    , spMin       :: !Double
    , spMax       :: !Double
    , spStd       :: !Double
    }

instance Show StatPoint where
    show StatPoint {..} =
        printf "%36s, %20s, %7d, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f"
            (show spTimestamp) spHost spCookie spSample spMedian spP90 spMean spMin spMax spStd

instance ToJSON StatPoint where
  toJSON StatPoint {..} =
      object [ "timestamp" .= spTimestamp
             , "host"      .=  spHost
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
    StatPoint {
          spTimestamp = ts
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


pingClient :: Tracer IO LogMsg -> Options -> AddrInfo -> IO ()
pingClient tracer Options{quiet, magic, json, maxCount} peer = bracket
    (Socket.socket (Socket.addrFamily peer) Socket.Stream Socket.defaultProtocol)
    Socket.close
    (\sd -> withTimeoutSerial $ \timeoutfn -> do
        !t0_s <- getMonotonicTime
        Socket.connect sd (Socket.addrAddress peer)
        !t0_e <- getMonotonicTime
        (Just host, Just port) <- Socket.getNameInfo [Socket.NI_NUMERICHOST, Socket.NI_NUMERICSERV]
                                      True True (Socket.addrAddress peer)
        let peerStr = host ++ ":" ++ port
        unless quiet $ printf "%s TCP rtt: %.3f\n" peerStr $ toSample t0_e t0_s
        let timeout = 30
            bearer = socketAsMuxBearer timeout nullTracer sd

        !t1_s <- write bearer timeoutfn $ wrap handshakeNum InitiatorDir
                    (handshakeReq [ NodeToNodeVersionV1 magic
                                  , NodeToNodeVersionV2 magic
                                  , NodeToNodeVersionV3 magic
                                  , NodeToNodeVersionV4 magic False
                                  , NodeToNodeVersionV5 magic False
                                  , NodeToNodeVersionV6 magic False
                                  , NodeToNodeVersionV7 magic False
                    ])
        (msg, !t1_e) <- nextMsg bearer timeoutfn handshakeNum
        unless quiet $ printf "%s handshake rtt: %s\n" peerStr (show $ diffTime t1_e t1_s)
        case CBOR.deserialiseFromBytes handshakeDec msg of
             Left err -> do
                 eprint $ printf "%s Decoding error %s\n" peerStr (show err)
             Right (_, Left err) -> do
                 eprint $ printf "%s Protocol error %s\n" peerStr (show err)
             Right (_, Right version) -> do
                 unless quiet $ printf "%s Negotiated version %s\n" peerStr (show version)
                 case version of
                      (NodeToNodeVersionV1 _) -> return ()
                      (NodeToNodeVersionV2 _) -> return ()
                      _                       -> do
                          keepAlive bearer timeoutfn peerStr version (tdigest []) 0

    )
  where
    toSample :: Time -> Time -> Double
    toSample t_e t_s = realToFrac $ diffTime t_e t_s

    eprint :: String -> IO ()
    eprint = hPutStr stderr

    nextMsg ::  MuxBearer IO -> TimeoutFn IO -> MiniProtocolNum -> IO (BL.ByteString, Time)
    nextMsg bearer timeoutfn ptclNum = do
        (sdu, t_e) <- Network.Mux.Types.read bearer timeoutfn
        if mhNum (msHeader sdu) == ptclNum
           then return (msBlob sdu, t_e)
           else nextMsg bearer timeoutfn ptclNum

    keepAlive :: MuxBearer IO
              -> TimeoutFn IO
              -> String
              -> NodeToNodeVersion
              -> TDigest 5
              -> Word32
              -> IO ()
    keepAlive _ _ _ _ _ cookie | cookie == maxCount = return ()
    keepAlive bearer timeoutfn peerStr version td !cookie = do
        let cookie16 = fromIntegral cookie
        !t_s <- write bearer timeoutfn $ wrap keepaliveNum InitiatorDir (keepAliveReq version cookie16)
        (!msg, !t_e) <- nextMsg bearer timeoutfn keepaliveNum
        let rtt = toSample t_e t_s
            td' = insert rtt td
        case CBOR.deserialiseFromBytes (keepAliveRspDec version) msg of
             Left err -> eprint $ printf "%s keepalive decoding error %s\n" peerStr (show err)
             Right (_, Left err) -> eprint $ printf "%s keepalive protocol error %s\n" peerStr (show err)
             Right (_, Right cookie') -> do
                 when (cookie' /= cookie16) $ eprint $ printf "%s cookie missmatch %d /= %d\n"
                     peerStr cookie' cookie

                 now <- getCurrentTime
                 let point = toStatPoint now peerStr cookie16 rtt td'
                 if json
                    then traceWith tracer $ LogMsg (encode point)
                    else traceWith tracer $ LogMsg $ BSC.pack $ show point <> "\n"
                 hFlush stdout
                 threadDelay 1
                 keepAlive bearer timeoutfn peerStr version td' (cookie + 1)
