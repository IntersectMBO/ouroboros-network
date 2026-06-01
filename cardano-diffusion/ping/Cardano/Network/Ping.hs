{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeData            #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Network.Ping
  ( PingOpts (..)
  , pingOptsParser
  , PingMode (..)
  , LogFormat (..)
  , LogMsg (..)
  , StatPoint (..)
  , ProtocolFlavour (..)
  , pingClients
  , mainnetMagic
  , NetworkMagic (..)
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (NFData)
import Control.Monad (unless, void, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, mkTracer, nullTracer, traceWith)

import Codec.CBOR.Term qualified as CBOR
import Codec.Serialise qualified as Serialise
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, encode, object)
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as LBS.Char
import Data.IP
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.TDigest (TDigest)
import Data.TDigest qualified as TDigest
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Word (Word16, Word32)
import Network.DNS qualified as DNS
import Network.Mux (MiniProtocolInfo (..))
import Network.Mux qualified as Mx
import Network.Mux.Bearer (MakeBearer (..), makeSocketBearer)
import Network.Socket (AddrInfo, StructLinger (..))
import Network.Socket qualified as Socket
import Options.Applicative
import Options.Applicative.Help.Pretty qualified as Pretty
import System.Directory (doesFileExist)
import System.IO qualified as IO
import System.Random (initStdGen)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Cardano.Network.Diffusion.Configuration (defaultChainSyncIdleTimeout)
import Cardano.Network.NodeToClient qualified as NodeToClient
import Cardano.Network.NodeToClient.Version
import Cardano.Network.NodeToNode qualified as NodeToNode
import Cardano.Network.NodeToNode.Version
import Cardano.Network.OrphanInstances ()
import Cardano.Network.PeerSelection (PeerSharing (..), PeerTrustable (..))
import Cardano.Network.Protocol.ChainSync.Client (ChainSyncClient)
import Cardano.Network.Protocol.ChainSync.Client qualified as ChainSync
import Cardano.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Cardano.Network.Protocol.ChainSync.Codec.TimeLimits qualified as ChainSync
import Cardano.Network.Protocol.KeepAlive.Client (KeepAliveClient (..))
import Cardano.Network.Protocol.KeepAlive.Client qualified as KeepAlive
import Cardano.Network.Protocol.KeepAlive.Codec qualified as KeepAlive
import Cardano.Network.Protocol.KeepAlive.Type qualified as KeepAlive

import Ouroboros.Network.Block hiding (blockNo)
import Ouroboros.Network.ConnectionId
import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.Protocol.Handshake hiding (Accept (..),
           RefuseReason (..))
import Ouroboros.Network.Util.ShowProxy

data LogFormat = AsJSON | AsText
  deriving (Eq, Show)


data PingMode =
    TipMode
    -- puery tip
  | PingMode
    -- ping
  | QueryMode
    -- query handshake parameters
  deriving (Eq, Show)

type Port = Word

-- | There are three stages for resolving addresses.
--
-- 1. FilePath might be a file path or a DNS name
-- 2. FilePath was resolved as a DNS name (or not, if a dns lookup failed)
-- 3. Resolved DNS names into IPs and ports.
--
-- See `resolveAddress`
--
type data Stage = Unresolved ResolvedFilePath | ResolvedDNS
type data ResolvedFilePath = FilePathUnresolved | FilePathResolved


data Address (stage :: Stage) where
    FilePath :: FilePath -> Address resolved

    Domain   :: String
             -> Port
             -> Address (Unresolved FilePathResolved)

    IP       :: IP
             -> Port
             -> Address resolved

data PingOpts stage = PingOpts
  { pingOptsCount     :: Word32
    -- ^ Number of messages to send to the server
  , pingOptsMagic     :: NetworkMagic
    -- ^ The network magic to use for all connections
  , pingOptsJson      :: LogFormat
    -- ^ Print output in JSON
  , pingOptsQuiet     :: Bool
    -- ^ Less verbose output
  , pingOptsMode      :: PingMode
    -- ^ Ping mode
  , pingOptsAddresses :: [Address stage]
    -- ^ list of addresses
  } -- deriving (Eq, Show)

mainnetMagic :: NetworkMagic
mainnetMagic = NetworkMagic 764824073

pingOptsParser :: Parser (PingOpts (Unresolved FilePathUnresolved))
pingOptsParser =
  PingOpts
    <$> option auto
        (  long "count"
        <> short 'c'
        <> help (mconcat
                [ "Stop after sending count requests and receiving count responses.  "
                , "If this option is not specified, ping will operate until interrupted.  "
                ])
        <> metavar "COUNT"
        <> value maxBound
        <> showDefault
        )
    <*> option (NetworkMagic <$> auto)
        (  long "network-magic"
        <> short 'm'
        <> help "Network magic."
        <> value mainnetMagic
        <> metavar "MAGIC"
        <> showDefaultWith (show . unNetworkMagic)
        )
    <*> flag  AsText AsJSON
        (  long "json"
        <> short 'j'
        <> help "JSON output flag."
        )
    <*> flag False True
        (  long "quiet"
        <> short 'q'
        <> help "Quiet flag, CSV/JSON only output."
        )
    <*> option pingMode
        (  long "mode"
        <> helpDoc (Just $ Pretty.hang 2 $
                 "Mode, either ping, tip or query:"
              <> Pretty.softline
              <> "ping  - send pings via keep-alive protocol (node-to-node only),"
              <> Pretty.softline
              <> "tip   - query tip via chain-sync protocol (node-to-node / node-to-client),"
              <> Pretty.softline
              <> "query - query handshake parameters (node-to-node / node-to-client)."
           )
        <> value PingMode
        <> metavar "MODE"
        )
    <*> argParser
  where
    pingMode :: ReadM PingMode
    pingMode =
      eitherReader $ \case
        "tip" -> Right TipMode
        "ping" -> Right PingMode
        "query" -> Right QueryMode
        _ -> Left "unexpected string"


    argParser :: Parser [Address (Unresolved FilePathUnresolved)]
    argParser =
        some addrParser
      where
        addrParser :: Parser (Address (Unresolved FilePathUnresolved))
        addrParser =
            argument
              (     uncurry IP <$> readIPv4AndPort
                <|> uncurry IP <$> readIPv6AndPort
                <|> FilePath   <$> readDomainNameOrFilePath
              )
              (  help "List of IP/DNS address and ports or UNIX socket paths, e.g. 127.0.0.1:3001 [::1]:3001 example.org:3001."
              <> metavar "ADDRS"
              )
          where
            -- note: `Read` instances for `IP`, `IPv4`, `IPv6` expect no trailing
            -- characters after the address, thus we need to find the split position
            -- first.

            -- parse IPv4 address and port in a form `127.0.0.1:3001`
            readIPv4AndPort :: ReadM (IP, Port)
            readIPv4AndPort =
              eitherReader $ \s -> do
                case splitWith ':' s of
                  Nothing -> Left s
                  Just (addrStr, portStr) ->
                    maybe (Left s) Right $
                    (,) <$> readMaybe addrStr
                        <*> readMaybe portStr

            -- parse IPv6 address and port in a form `[::1]:3001` or a UNIX file path
            readIPv6AndPort :: ReadM (IP, Port)
            readIPv6AndPort =
              eitherReader $ \s ->
                case s of
                  ('[':s') ->
                     case splitWith ']' s' of
                       Just (addrStr, ':' : portStr) ->
                         maybe (Left s) Right $
                         (,) <$> readMaybe addrStr
                             <*> readMaybe portStr
                       _ -> Left s
                  _ -> Left s

            readDomainNameOrFilePath :: ReadM String
            readDomainNameOrFilePath = eitherReader Right


data LogMsg = LogMsg BL.ByteString
            | LogEnd
            deriving Show


-- | Logging is done concurrently from multiple ping clients.
--
loggerThread :: PingOpts stage -> StrictTMVar IO LogMsg -> IO ()
loggerThread
    PingOpts { pingOptsJson,
               pingOptsMode
             }
    msgQueue
    =
    go True
  where
    go first = do
      msg <- atomically $ takeTMVar msgQueue
      case msg of
        LogMsg bs -> do
          let bs' = case (pingOptsJson, first, pingOptsMode) of
                (AsJSON, False, PingMode) ->
                  LBS.Char.pack ",\n" <> bs
                (AsJSON, False, TipMode) ->
                  LBS.Char.pack ",\n" <> bs
                (AsJSON, True, PingMode) ->
                  LBS.Char.pack "{ \"pongs\": [ " <> bs
                (AsJSON, True, TipMode)  ->
                  LBS.Char.pack "{ \"tip\": [ " <> bs
                (AsText, True, PingMode) ->
                  LBS.Char.pack "timestamp,                      host,                         cookie,  sample,  median,     p90,    mean,     min,     max,     std\n" <> bs
                (AsText, True, TipMode)   -> bs
                (AsText, False, _)        -> bs

                (_     , _, QueryMode) -> bs

          LBS.Char.putStr bs'
          go False
        -- Output valid JSON even when no pongs were received
        LogEnd -> when (pingOptsJson == AsJSON && pingOptsMode /= QueryMode) $
          if first
            then IO.putStrLn $ case pingOptsMode of
              TipMode   -> "{ \"tip\": [] }"
              PingMode  -> "{ \"pongs\": [] }"
              QueryMode -> ""
            else IO.putStrLn "] }"

sduTimeout :: DiffTime
sduTimeout = 30

data PingTip = PingTip {
    ptHost    :: !(Either FilePath (IP, Socket.PortNumber))
  , ptRtt     :: !Double
  , ptHash    :: !ByteString
  , ptBlockNo :: !BlockNo
  , ptSlotNo  :: !SlotNo
  }

hexStr :: ByteString -> String
hexStr = BS.foldr (\b -> (<>) (printf "%02x" b)) ""

instance Show PingTip where
  show PingTip{..} =
    case ptHost of
      Right (ip, port) ->
        printf "host: %s:%d, rtt: %f, hash %s, blockNo: %d slotNo: %d"
               (show ip)
               (fromIntegral port :: Word16)
               ptRtt
               (hexStr ptHash)
               (unBlockNo ptBlockNo)
               (unSlotNo ptSlotNo)
      Left path ->
        printf "host: %s, rtt: %f, hash %s, blockNo: %d slotNo: %d"
               path
               ptRtt
               (hexStr ptHash)
               (unBlockNo ptBlockNo)
               (unSlotNo ptSlotNo)

instance ToJSON PingTip where
  toJSON PingTip{..} =
    case ptHost of
      Right (ip, port) ->
        object [
            "rtt"     .= ptRtt
          , "hash"    .= hexStr ptHash
          , "blockNo" .= ptBlockNo
          , "slotNo"  .= ptSlotNo
          , "addr"    .= show ip
          , "port"    .= (fromIntegral port :: Word16)
          ]
      Left path ->
        object [
            "rtt"     .= ptRtt
          , "hash"    .= hexStr ptHash
          , "blockNo" .= ptBlockNo
          , "slotNo"  .= ptSlotNo
          , "path"    .= path
          ]

data StatPoint = StatPoint
  { spTimestamp :: UTCTime
    -- ^ time-stamp of the sample value
  , spHost      :: TL.Text
    -- ^ host
  , spCookie    :: Word16
    -- ^ sample number
  , spSample    :: Double
    -- ^ current sample value
  , spMedian    :: Double
    -- ^ median value
  , spP90       :: Double
    -- ^ 90 percentile
  , spMean      :: Double
    -- ^ mean value
  , spMin       :: Double
    -- ^ minimal value
  , spMax       :: Double
    -- ^ maximal value
  , spStd       :: Double
    -- ^ standard deviation
  }

instance Show StatPoint where
  show :: StatPoint -> String
  show StatPoint {..} =
    printf "%-31s %-28s %7d, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f"
      (iso8601Show spTimestamp ++ ",") (show spHost ++ ",") spCookie spSample spMedian spP90 spMean spMin spMax spStd

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
      , "std"       .= spStd
      ]

toStatPoint :: UTCTime -> TL.Text -> Word16 -> Double -> TDigest 5 -> StatPoint
toStatPoint ts host cookie sample td =
  StatPoint
    { spTimestamp = ts
    , spHost      = host
    , spCookie    = cookie
    , spSample    = sample
    , spMedian    = quantile' 0.5
    , spP90       = quantile' 0.9
    , spMean      = mean'
    , spMin       = TDigest.minimumValue td
    , spMax       = TDigest.maximumValue td
    , spStd       = stddev'
    }
  where
    quantile' :: Double -> Double
    quantile' q = fromMaybe 0 (TDigest.quantile q td)

    mean' :: Double
    mean' = fromMaybe 0 (TDigest.mean td)

    stddev' :: Double
    stddev' = fromMaybe 0 (TDigest.stddev td)


keepAliveDelay :: DiffTime
keepAliveDelay = 1


-- | An idle timeout applied before closing the connection, to let the other
-- side exit cleanly.
idleTimeout :: DiffTime
idleTimeout = 1


data PingClientError
  = PingClientProtocolLimitFailure ProtocolLimitFailure
  -- ^ protocol limit error

  | forall versionNumber.
    Show versionNumber
  => PingClientHandshakeProtocolError (HandshakeProtocolError versionNumber) TL.Text
  -- ^ handshake protocol error

deriving instance Show PingClientError

instance Exception PingClientError where
  displayException (PingClientProtocolLimitFailure err) =
    displayException err
  displayException (PingClientHandshakeProtocolError err peerStr) =
    printf "%s handshake error: %s" peerStr (show err)

data ProtocolFlavour version versionData where
    NodeToNode   :: ProtocolFlavour NodeToNodeVersion   NodeToNodeVersionData
    NodeToClient :: ProtocolFlavour NodeToClientVersion NodeToClientVersionData

--
-- ChainSync Tip Sampling
--

-- We don't need blocks, headers or points, so we just go away with any valid
-- CBOR term.  As a result:
-- NOTE: the `chainSync` below is used for both `NodeToNode` and `NodeToClient`
-- protocols.
type ChainSyncHeader = CBOR.Term
type ChainSyncPoint  = CBOR.Term
data ChainSyncBlock
type instance HeaderHash ChainSyncBlock = ByteString
instance ShowProxy ChainSyncBlock where
type ChainSyncTip = Tip ChainSyncBlock
instance StandardHash ChainSyncBlock


-- A `ChainSyncClient` that finds the current `Tip` over `node-to-node`
-- or `node-to-client` protocol.
chainSyncClient
  :: Tracer IO LogMsg
  -> Either FilePath (IP, Socket.PortNumber)
  -> LogFormat
  -> ChainSyncClient ChainSyncHeader ChainSyncPoint ChainSyncTip IO ()
chainSyncClient stdout host logFormat =
    ChainSync.ChainSyncClient $ do
      start <- getMonotonicTime
      return (go start)
  where
    go :: Time
       -> ChainSync.ClientStIdle ChainSyncHeader ChainSyncPoint ChainSyncTip IO ()
    go start = ChainSync.SendMsgFindIntersect []
       ChainSync.ClientStIntersect {
         ChainSync.recvMsgIntersectFound = \_ _tip ->
           ChainSync.ChainSyncClient $ do
             -- this should not happen, as we send an empty list of points
             return $ ChainSync.SendMsgDone (),
         ChainSync.recvMsgIntersectNotFound = \tip ->
           ChainSync.ChainSyncClient $ do
             end <- getMonotonicTime
             let (ptSlotNo, ptHash, ptBlockNo) = case tip of
                   TipGenesis              -> (0, mempty, 0)
                   Tip slotNo hash blockNo -> (slotNo, hash, blockNo)
                 pingTip = PingTip {
                   ptHost = host,
                   ptRtt  = toSample end start,
                   ptHash,
                   ptBlockNo,
                   ptSlotNo
                 }
             case logFormat of
               AsJSON -> traceWith stdout $ LogMsg (encode pingTip)
               AsText -> traceWith stdout $ LogMsg $ LBS.Char.pack $ show pingTip <> "\n"
             return $ ChainSync.SendMsgDone ()
       }


--
-- KeepAlive RTT sampling
--

keepAliveClient
  :: Tracer IO LogMsg
  -> TL.Text   -- ^ peer
  -> LogFormat -- ^ use JSON formatting
  -> Word32    -- ^ ping count
  -> TDigest 5
  -> KeepAliveClient IO ()
keepAliveClient stdout peerName logFormat count td0 =
    KeepAliveClient $ loop td0 0
  where
    loop :: TDigest 5
         -> Word32
         -> IO (KeepAlive.KeepAliveClientSt IO ())
    loop _td cookie
      | cookie == count
      = return $ KeepAlive.SendMsgDone (pure ())
    loop td cookie = do
      start <- getMonotonicTime
      let cookie16 :: Word16
          cookie16 = fromIntegral cookie
      return $ KeepAlive.SendMsgKeepAlive (KeepAlive.Cookie cookie16) $ do
        end <- getMonotonicTime
        now <- getCurrentTime
        let rtt = toSample end start
            td' = TDigest.insert rtt td
            point = toStatPoint now peerName cookie16 rtt td'
        case logFormat of
          AsJSON -> traceWith stdout $ LogMsg (encode point)
          AsText -> traceWith stdout $ LogMsg $ LBS.Char.pack $ show point <> "\n"
        threadDelay keepAliveDelay
        loop td' (cookie + 1)


--
-- Ping Client
--

resolveAddress :: Address (Unresolved fpResolved)
               -> IO [Address ResolvedDNS]
resolveAddress (IP addr port) = pure [IP addr port]
resolveAddress (FilePath path) =
   case splitWith ':' path
           >>= \(dnsStr, portStr) -> (dnsStr,) <$> readMaybe portStr
     of
     Just (dnsname, port) ->
       -- try resolve as a domain name
       resolveAddress (Domain dnsname port)
        >>= \case
          [] ->
            -- dns query failed, let's try if it is a file path
            doesFileExist path >>= \case
              True -> pure [FilePath path]
              False -> do
                IO.hPutStrLn IO.stderr ("WARNING: file path " ++ show path ++ " does not exist")
                pure []
          addrs -> pure addrs
     Nothing -> do
       pure [FilePath path]
resolveAddress (Domain dns port) = do
  let hostname = BS.Char.pack dns
  rs <- DNS.makeResolvSeed DNS.defaultResolvConf
  DNS.withResolver rs $ \resolver -> do
    a <- fmap (map IPv4) <$> DNS.lookupA resolver hostname
    aaaa <- fmap (map IPv6) <$> DNS.lookupAAAA resolver hostname
    case a <> aaaa of
      Left err -> do
        IO.hPutStrLn IO.stderr ("WARNING: dns error: " ++ show err)
        return []
      Right ips -> do
        IO.hPutStrLn IO.stderr ("INFO: " ++ dns ++ ":" ++ show port ++ " " ++ show ips)
        return [ IP ip port | ip <- ips ]


pingClients
  :: PingOpts (Unresolved FilePathUnresolved)
  -> IO ()
pingClients opts@PingOpts { pingOptsAddresses = addresses } = do
    msgQueue <- newEmptyTMVarIO
    let tracer :: Tracer IO LogMsg
        tracer = mkTracer $ \msg -> atomically $ putTMVar msgQueue msg

    -- resolved addresses
    resolvedAddresses <-
      concat <$> traverse resolveAddress addresses
    sockAddrs
      <- mapMaybe (either (fmap Left)
                          (fmap Right . maybeHead))
         <$> traverse
               (\case
                  IP ip@IPv4{} port ->
                    Right <$> Socket.getAddrInfo
                      (Just Socket.defaultHints { Socket.addrFamily = Socket.AF_INET })
                      (Just (show ip))
                      (Just (show port))
                  IP ip@IPv6{} port ->
                    Right <$> Socket.getAddrInfo
                      (Just Socket.defaultHints { Socket.addrFamily = Socket.AF_INET6 })
                      (Just (show ip))
                      (Just (show port))
                  FilePath path -> do
                    a <- doesFileExist path
                    if a
                      then return $ Left $ Just
                           $ Socket.AddrInfo
                              []
                              Socket.AF_UNIX
                              Socket.Stream
                              Socket.defaultProtocol
                              (Socket.SockAddrUnix path)
                              Nothing
                      else do
                        case readMaybe path :: Maybe IP of
                          Nothing -> IO.hPutStrLn IO.stderr ("WARNING: file does not exist: " ++ show path)
                          Just ip -> IO.hPutStrLn IO.stderr ("WARNING: please add a port number for " ++ show ip)
                        return (Left Nothing)
               )
               resolvedAddresses

    let opts' :: PingOpts ResolvedDNS
        opts' = opts { pingOptsAddresses = resolvedAddresses }
    mapConcurrently_ (\case
                        Right addr -> pingClient NodeToNode tracer opts' addr
                        Left  addr -> pingClient NodeToClient tracer opts' addr
                     ) sockAddrs
      `finally`
      atomically (putTMVar msgQueue LogEnd)
      `race_`
      loggerThread opts msgQueue
      `catch`
      \(e :: SomeException) -> IO.hPutStrLn IO.stderr (displayException e)
                            >> throwIO e

pingClient
  :: forall versionNumber versionData.
     ( Acceptable versionData
     , Queryable versionData
     , NFData versionData
     , Ord versionNumber
     , Show versionNumber
     , NFData versionNumber
     , ToJSON versionNumber
     )
  => ProtocolFlavour versionNumber versionData
  -> Tracer IO LogMsg
  -> PingOpts ResolvedDNS
  -> AddrInfo
  -> IO ()
pingClient protocol stdout opts@PingOpts{..} peer =
    bracket
      (Socket.socket (Socket.addrFamily peer) Socket.Stream Socket.defaultProtocol)
      Socket.close
      (\sd -> do
        when (Socket.addrFamily peer /= Socket.AF_UNIX) $ do
          Socket.setSocketOption sd Socket.NoDelay 1
          Socket.setSockOpt sd Socket.Linger
            StructLinger
              { sl_onoff  = 1
              , sl_linger = 0
              }
        runPingClient sd
      )
  where
    runPingClient :: Socket.Socket -> IO ()
    runPingClient sd = do
      peerName <- getPeerName
      let logMsg :: Format msg => msg -> IO ()
          logMsg = logMsgWithPeer opts peerName

      !t0_s <- getMonotonicTime
      Socket.connect sd (Socket.addrAddress peer)
      !t0_e <- getMonotonicTime

      logMsg $ NetworkRTT (toSample t0_e t0_s)

      connId <- ConnectionId <$> Socket.getSocketName sd
                             <*> Socket.getPeerName sd
      bearer <- getBearer makeSocketBearer sduTimeout sd Nothing

      -- Run handshake with RTT measurements
      -- NOTE: we pass all versions supported by `cardano-diffusion:api`
      r <- runHandshakeClientWithRTT
        @versionNumber @versionData @()
        bearer connId
        HandshakeArguments {
          haHandshakeTracer = nullTracer,
          haBearerTracer    = nullTracer,
          haHandshakeCodec  = case protocol of
            NodeToNode   -> NodeToNode.nodeToNodeHandshakeCodec
            NodeToClient -> NodeToClient.nodeToClientHandshakeCodec,
          haVersionDataCodec = case protocol of
            NodeToNode   -> NodeToNode.nodeToNodeVersionDataCodec
            NodeToClient -> NodeToClient.nodeToClientVersionDataCodec,
          haAcceptVersion = acceptableVersion,
          haQueryVersion  = queryVersion,
          haTimeLimits    = timeLimitsHandshake
        }
        (case protocol of
          NodeToNode ->
             foldMapVersions
               (\versionNumber ->
                 simpleSingletonVersions
                   versionNumber
                   NodeToNodeVersionData {
                     networkMagic  = pingOptsMagic,
                     diffusionMode = InitiatorOnlyDiffusionMode,
                     peerSharing   = PeerSharingDisabled,
                     query         = case pingOptsMode of
                                        QueryMode -> True
                                        _         -> False,
                     perasSupport  = if versionNumber >= NodeToNodeV_16
                                     then PerasSupported
                                     else PerasUnsupported
                   }
                   (const ())
               )
               [minBound..maxBound]

          NodeToClient ->
             foldMapVersions
               (\versionNumber ->
                 simpleSingletonVersions
                   versionNumber
                   NodeToClientVersionData {
                     networkMagic = pingOptsMagic,
                     query        = case pingOptsMode of
                                      QueryMode -> True
                                      _         -> False
                   }
                   (const ())
               )
               [minBound..maxBound]
        )
      case r of
        Left err -> throwIO (PingClientProtocolLimitFailure err)
        Right (Left err', rtt) -> do
          logMsg (HandshakeRTT rtt)
          throwIO (PingClientHandshakeProtocolError err' peerName)
        Right (Right r', rtt) -> do
          logMsg (HandshakeRTT rtt)
          case r' of
            HandshakeQueryResult versions -> do
              -- print query results if it was supported by the remote side
              when (pingOptsMode == QueryMode) $
                -- override the pingOptsQuiet flag
                logMsgWithPeer opts { pingOptsQuiet = False } peerName $
                  QueriedVersions (Map.keys versions)
            HandshakeNegotiationResult _ version _versionData -> do
              -- show negotiated version
              logMsg $ NegotiatedVersion version
              case (protocol, pingOptsMode) of
                (_, QueryMode) ->
                  -- in `QueryMode` we didn't negotiated the connection, so we
                  -- cannot run any mini-protocol.
                  pure ()

                (_, TipMode) -> do
                  --
                  -- run chain sync to get the tip
                  --
                  let host =
                        case Socket.addrAddress peer of
                           Socket.SockAddrUnix path       -> Left path
                           Socket.SockAddrInet pn ha      -> Right (IPv4 (fromHostAddress ha), pn)
                           Socket.SockAddrInet6 pn _ ha _ -> Right (IPv6 (fromHostAddress6 ha), pn)
                  stdGen <- initStdGen
                  mx <- Mx.new
                          Mx.nullTracers
                          [MiniProtocolInfo {
                            miniProtocolNum        = case protocol of
                                                       NodeToNode -> NodeToNode.chainSyncMiniProtocolNum
                                                       NodeToClient -> NodeToClient.localChainSyncMiniProtocolNum,
                            miniProtocolDir        = Mx.InitiatorDirectionOnly,
                            miniProtocolLimits     = case protocol of
                                                       NodeToNode -> NodeToNode.chainSyncProtocolLimits NodeToNode.defaultMiniProtocolParameters
                                                       NodeToClient -> NodeToClient.maximumMiniProtocolLimits,
                            miniProtocolCapability = Nothing
                          }]
                  withAsync (Mx.run mx bearer) $ \_ ->
                    (Mx.runMiniProtocol mx
                      (case protocol of
                         NodeToNode -> NodeToNode.chainSyncMiniProtocolNum
                         NodeToClient -> NodeToClient.localChainSyncMiniProtocolNum
                      )
                      Mx.InitiatorDirectionOnly
                      Mx.StartEagerly
                      (\channel ->
                        runPeerWithLimitsRnd
                          nullTracer
                          stdGen
                          (ChainSync.codecChainSync CBOR.encodeTerm CBOR.decodeTerm
                                                    CBOR.encodeTerm CBOR.decodeTerm
                                                    (encodeTip Serialise.encode)
                                                    (decodeTip Serialise.decode))
                          (ChainSync.byteLimitsChainSync (fromIntegral . BL.length))
                          (ChainSync.timeLimitsChainSync defaultChainSyncIdleTimeout IsNotTrustable)
                          channel
                          (ChainSync.chainSyncClientPeer $ chainSyncClient stdout host pingOptsJson))
                        >>= void . atomically
                    )
                    `finally` Mx.stop mx
                  threadDelay idleTimeout

                (NodeToNode, PingMode) -> do
                  --
                  -- run keepalive client to get RTT samples
                  --
                  mx <- Mx.new
                          Mx.nullTracers
                          [MiniProtocolInfo {
                            miniProtocolNum        = NodeToNode.keepAliveMiniProtocolNum,
                            miniProtocolDir        = Mx.InitiatorDirectionOnly,
                            miniProtocolLimits     = NodeToNode.keepAliveProtocolLimits NodeToNode.defaultMiniProtocolParameters,
                            miniProtocolCapability = Nothing
                          }]
                  withAsync (Mx.run mx bearer) $ \_ ->
                    (Mx.runMiniProtocol mx
                      NodeToNode.keepAliveMiniProtocolNum
                      Mx.InitiatorDirectionOnly
                      Mx.StartEagerly
                      (\channel -> do
                        runPeerWithLimits
                          nullTracer
                          KeepAlive.codecKeepAlive_v2
                          (KeepAlive.byteLimitsKeepAlive (fromIntegral . BL.length))
                          KeepAlive.timeLimitsKeepAlive
                          channel
                          (KeepAlive.keepAliveClientPeer
                            $ keepAliveClient
                                stdout
                                peerName
                                pingOptsJson
                                pingOptsCount
                                (TDigest.tdigest [])))
                        >>= void . atomically
                    )
                    `finally` Mx.stop mx
                  threadDelay idleTimeout

                (NodeToClient, PingMode) -> pure ()
                  --
                  -- ping mode over node-to-client protocol is not supported
                  --

    getPeerName :: IO TL.Text
    getPeerName =
      case Socket.addrFamily peer of
        Socket.AF_UNIX -> return $ TL.pack . show $ Socket.addrAddress peer
        _ -> do
          (Just host, Just port) <-
            Socket.getNameInfo
              [Socket.NI_NUMERICHOST, Socket.NI_NUMERICSERV]
              True True (Socket.addrAddress peer)
          return $ TL.pack $ host <> ":" <> port


toSample :: Time -> Time -> Double
toSample end start = realToFrac $ end `diffTime` start

class Format a where
  format :: LogFormat -> a -> TL.Text

newtype NegotiatedVersion versionNumber = NegotiatedVersion versionNumber

instance (ToJSON versionNumber, Show versionNumber)
      => Format (NegotiatedVersion versionNumber) where
  format AsJSON (NegotiatedVersion v) = encodeToLazyText $ object ["negotiated_version" .= toJSON v]
  format AsText (NegotiatedVersion v) = TL.pack $ printf "Negotiated version %s" (show v)

newtype QueriedVersions versionNumber = QueriedVersions [versionNumber]

instance (ToJSON versionNumber, Show versionNumber)
      => Format (QueriedVersions versionNumber) where
  format AsJSON (QueriedVersions vs) = encodeToLazyText $ object ["queried_versions" .= toJSON vs]
  format AsText (QueriedVersions vs) = TL.pack $ printf "Queried versions %s" (show vs)


newtype NetworkRTT = NetworkRTT Double

instance Format NetworkRTT where
  format AsJSON (NetworkRTT rtt) = encodeToLazyText $ object ["network_rtt" .= toJSON rtt]
  format AsText (NetworkRTT rtt) = TL.pack $ printf "network rtt: %.3f" rtt

newtype HandshakeRTT = HandshakeRTT DiffTime

instance Format HandshakeRTT where
  format AsJSON (HandshakeRTT diff) = encodeToLazyText $ object ["handshake_rtt" .= toJSON ((fromRational $ toRational diff) :: Double)]
  format AsText (HandshakeRTT diff) = TL.pack $ printf "handshake rtt: %s" $ show diff


-- note: use `logMsg` defined above in terms of `logMsgWithPeer`
logMsgWithPeer :: Format msg
               => PingOpts ResolvedDNS
               -> TL.Text -- ^ peer identifier
               -> msg
               -> IO ()
logMsgWithPeer PingOpts { pingOptsQuiet, pingOptsJson } peerStr msg =
  unless pingOptsQuiet $ TL.hPutStrLn IO.stdout (peerStr <> " " <> format pingOptsJson msg)


instance ShowProxy CBOR.Term where
  showProxy _ = "CBOR.Term"

--
-- Utils
--

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (a:_) = Just a

splitWith :: Char -> String -> Maybe (String, String)
splitWith c = go ""
    where
      go _ []
        = Nothing
      go !acc (a:as)
        | a == c
        = Just (reverse acc, as)
      go !acc (a:as)
        = go (a:acc) as
