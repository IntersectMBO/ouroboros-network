{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeData             #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Network.Ping
  ( PingOpts (..)
  , Address
  , cmdlineParser
  , PingMode (..)
  , LogFormat (..)
  , LogMsg (..)
  , StatPoint (..)
  , ProtocolFlavour (..)
  , pingClients
  , mainnetMagic
  , NetworkMagic (..)
  ) where

import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (NFData)
import Control.Exception (SomeAsyncException (..))
import Control.Monad (unless, void, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, mkTracer, nullTracer, traceWith, (>$<))

import Codec.CBOR.Term qualified as CBOR
import Codec.Serialise qualified as Serialise
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, object)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.Char
import Data.ByteString.Lazy qualified as BL
import Data.IP
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (All (..))
import Data.String (fromString)
import Data.TDigest (TDigest)
import Data.TDigest qualified as TDigest
import Data.Text (Text)
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
-- 1. SRV might be a file path or a DNS name or an SRV record
-- 2. SRV was resolved as an SRV record, if not
-- 3. try to resolve it as a DNS name
-- 4. if failed, try to use it as a file path
--
-- See `resolveAddress`
--
type data Stage = Unresolved ResolvedSRVOrFilePath | Resolved
type data ResolvedSRVOrFilePath = SRVOrFilePathUnresolved | SRVOrFilePathResolved


data Address (stage :: Stage) where
    FilePath :: FilePath -> Address Resolved

    FilePathOrDomain
             :: String
             -> Address (Unresolved SRVOrFilePathUnresolved)

    Domain   :: DNS.Domain
             -> Port
             -> Address (Unresolved SRVOrFilePathResolved)

    SRV      :: String
             -> Address (Unresolved SRVOrFilePathUnresolved)

    IP       :: IP
             -> Port
             -> Address resolved


showIPWithPort :: IP -> Port -> String
showIPWithPort ip@IPv4{} port = show ip ++ ":" ++ show port
showIPWithPort ip@IPv6{} port = "[" ++ show ip ++ "]:" ++ show port

instance Show (Address Resolved) where
  show :: Address Resolved -> String
  show (IP ip port)    = showIPWithPort ip port
  show (FilePath path) = path

deriving instance Eq (Address Resolved)
deriving instance Ord (Address Resolved)


data PingOpts = PingOpts
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
  , pingOptsSRVPrefix :: String
    -- ^ SRV prefix
  } -- deriving (Eq, Show)

mainnetMagic :: NetworkMagic
mainnetMagic = NetworkMagic 764824073

pingOptsParser :: Parser PingOpts
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
    <*> option str
        (   long "srv-prefix"
         <> help "Prefix that will be added to an SRV service name"
         <> value "_cardano._tcp"
         <> metavar "SRV_PREFIX"
         <> showDefault
        )
  where
    pingMode :: ReadM PingMode
    pingMode =
      eitherReader $ \case
        "tip" -> Right TipMode
        "ping" -> Right PingMode
        "query" -> Right QueryMode
        _ -> Left "unexpected string"


argParser :: Parser [Address (Unresolved SRVOrFilePathUnresolved)]
argParser =
    some addrParser
  where
    addrParser :: Parser (Address (Unresolved SRVOrFilePathUnresolved))
    addrParser =
        argument
          (     uncurry IP <$> readIPv4AndPort
            <|> uncurry IP <$> readIPv6AndPort
            <|>                readDomainNameOrFilePath
          )
          (  help "List of IP/DNS/SRV address and ports or UNIX socket paths, e.g. 127.0.0.1:3001 [::1]:3001 example.org:3001."
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

        readDomainNameOrFilePath :: ReadM (Address (Unresolved SRVOrFilePathUnresolved))
        readDomainNameOrFilePath = eitherReader $ \s ->
          case List.find (== ':') s of
            -- not an `SRV` record
            Just _  -> Right (FilePathOrDomain s)
            Nothing -> Right (SRV s)

cmdlineParser :: Parser (PingOpts, [Address (Unresolved SRVOrFilePathUnresolved)])
cmdlineParser = (,) <$> pingOptsParser <*> argParser


-- | Log messages to stdout.
--
data LogMsg = LogChainSyncTip PingTip
            | LogStatPoint StatPoint
            | LogNodeToClientVersionData NodeToClientVersion (Either Text NodeToClientVersionData)
            | LogNodeToNodeVersionData   NodeToNodeVersion   (Either Text NodeToNodeVersionData)
  deriving Show

-- | Log messages to stderr.
--
data PingWarning = FilePathDoesNotExist FilePath
                 | DNSError DNS.Domain DNS.DNSError
                 | DNSResolution DNS.Domain [IP] Word
                 | MissingPort IP
                 | Error SomeException
                 | ConnectError Socket.SockAddr SomeException


formatPingWarning :: PingWarning -> String
formatPingWarning msg = severity msg ++ fn msg
  where
    fn (FilePathDoesNotExist path)
      = "file path " ++ show path ++ " does not exist"
    fn (DNSError domain err)
      = unwords
      [ "dns:"
      , BS.Char.unpack domain
      , show err
      ]
    fn (DNSResolution domain ips port)
      = concat
      [ BS.Char.unpack domain
      , ": "
      , List.intercalate ", " (flip showIPWithPort port <$> ips)
      ]
    fn (MissingPort ip)
      = "missing port for " ++ show ip
    fn (Error err)
      = show err
    fn (ConnectError sockAddr err)
      = printf "%-47s %s" (show sockAddr) (show err)

    severity :: PingWarning -> String
    severity = \case
        FilePathDoesNotExist{} -> warning
        DNSError{}             -> warning
        DNSResolution{}        -> mempty
        MissingPort{}          -> warning
        Error{}                -> warning
        ConnectError{}         -> warning
      where
        warning = "WARNING: "


instance ToText LogMsg where
  toText (LogChainSyncTip tip) = TL.pack (show tip)
  toText (LogStatPoint point) = TL.pack (show point)
  toText (LogNodeToClientVersionData version versionData)
    = TL.pack $ unwords
      [ show version
      , either (TL.unpack . TL.fromStrict) showNodeToClientVersionData versionData
      ]
    where
      showNodeToClientVersionData :: NodeToClientVersionData -> String
      showNodeToClientVersionData
        (NodeToClientVersionData networkMagic query)
        = unwords
          [ show networkMagic
          , show query
          ]
  toText (LogNodeToNodeVersionData version versionData)
    = TL.pack $ unwords
      [ show version
      , either (TL.unpack . TL.fromStrict) showNodeToNodeVersionData versionData
      ]
    where
      showNodeToNodeVersionData :: NodeToNodeVersionData -> String
      showNodeToNodeVersionData
        (NodeToNodeVersionData
          networkMagic
          diffusionMode
          peerSharing
          query
          perasSupport
        )
        = unwords
          [ show networkMagic
          , show diffusionMode
          , show peerSharing
          , show query
          , show perasSupport
          ]


instance ToJSON LogMsg where
  toJSON (LogChainSyncTip tip) = toJSON tip
  toJSON (LogStatPoint point) = toJSON point
  toJSON (LogNodeToClientVersionData version versionData)
    = object [ fromString (show version) .= either toJSON toJSON versionData ]
  toJSON (LogNodeToNodeVersionData version versionData)
    = object [ fromString (show version) .= either toJSON toJSON versionData ]


sduTimeout :: DiffTime
sduTimeout = 30

data PingTip = PingTip {
    ptRtt     :: !Double
  , ptHash    :: !ByteString
  , ptBlockNo :: !BlockNo
  , ptSlotNo  :: !SlotNo
  }

hexStr :: ByteString -> String
hexStr = BS.foldr (\b -> (<>) (printf "%02x" b)) ""

instance Show PingTip where
  show PingTip{..} =
    printf "%6.3fs, %-64s, %9d, %10d"
           ptRtt
           (hexStr ptHash)
           (unBlockNo ptBlockNo)
           (unSlotNo ptSlotNo)

instance ToJSON PingTip where
  toJSON PingTip{..} =
    object [
        "rtt"     .= ptRtt
      , "hash"    .= hexStr ptHash
      , "blockNo" .= ptBlockNo
      , "slotNo"  .= ptSlotNo
      ]

data StatPoint = StatPoint
  { spTimestamp :: UTCTime
    -- ^ time-stamp of the sample value
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
    printf "%-31s %6d, %6.3f, %6.3f, %6.3f, %6.3f, %6.3f, %6.3f, %6.3f"
      (iso8601Show spTimestamp ++ ",")
      spCookie spSample spMedian spP90 spMean spMin spMax spStd

statPointHeader :: String
statPointHeader = printf "%-46s %30s, %6s, %6s, %6s, %6s, %6s, %6s, %6s, %6s"
                         ("host," :: String)
                         ("timestamp" :: String)
                         ("cookie" :: String)
                         ("sample" :: String)
                         ("median" :: String)
                         ("p90"    :: String)
                         ("mean"   :: String)
                         ("min"   :: String)
                         ("max"   :: String)
                         ("std"   :: String)

tipHeader :: String
tipHeader = printf "%-47s %6s, %64s, %9s, %10s"
                   ("host,"   :: String)
                   ("rtt"     :: String)
                   ("hash"    :: String)
                   ("blockNo" :: String)
                   ("slotNo"  :: String)

instance ToJSON StatPoint where
  toJSON :: StatPoint -> Value
  toJSON StatPoint {..} =
    object
      [ "timestamp" .= spTimestamp
      , "cookie"    .= spCookie
      , "sample"    .= spSample
      , "median"    .= spMedian
      , "p90"       .= spP90
      , "mean"      .= spMean
      , "min"       .= spMin
      , "max"       .= spMax
      , "std"       .= spStd
      ]

toStatPoint :: UTCTime -> Word16 -> Double -> TDigest 5 -> StatPoint
toStatPoint ts cookie sample td =
  StatPoint
    { spTimestamp   = ts
    , spCookie      = cookie
    , spSample      = sample
    , spMedian      = quantile' 0.5
    , spP90         = quantile' 0.9
    , spMean        = mean'
    , spMin         = TDigest.minimumValue td
    , spMax         = TDigest.maximumValue td
    , spStd         = stddev'
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
  => PingClientHandshakeProtocolError (HandshakeProtocolError versionNumber) (Address Resolved)
  -- ^ handshake protocol error

deriving instance Show PingClientError

instance Exception PingClientError where
  displayException (PingClientProtocolLimitFailure err) =
    displayException err
  displayException (PingClientHandshakeProtocolError err addr) =
    printf "%s handshake error: %s" (show addr) (show err)

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
  :: PingOpts
  -> Signal
  -> HeaderVar
  -> Tracer IO LogMsg
  -> ChainSyncClient ChainSyncHeader ChainSyncPoint ChainSyncTip IO ()
chainSyncClient opts sig@Signal { signalReadiness } headerVar stdout =
    ChainSync.ChainSyncClient $ do
      signalReadiness
      go <$> getMonotonicTime
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
                   ptRtt  = toSample end start,
                   ptHash,
                   ptBlockNo,
                   ptSlotNo
                 }
             awaitReadiness sig
             printHeader opts headerVar tipHeader
             traceWith stdout (LogChainSyncTip pingTip)
             return $ ChainSync.SendMsgDone ()
       }


--
-- KeepAlive RTT sampling
--

keepAliveClient
  :: PingOpts
  -> Signal
  -> HeaderVar
  -- ^ stat header MVar
  -> Tracer IO LogMsg
  -> TDigest 5
  -> KeepAliveClient IO ()
keepAliveClient opts@PingOpts { pingOptsCount } sig headerVar stdout td0 =
    KeepAliveClient $ loop td0 0
  where
    -- we keep sending  keep alive message from the start, but we output
    -- measurements only when all clients are ready.  This is to make the output
    -- clean.  We cannot await for all clients to be ready, since some
    -- connections might be shutdown
    loop :: TDigest 5
         -> Word32
         -> IO (KeepAlive.KeepAliveClientSt IO ())
    loop _td cookie
      | cookie == pingOptsCount
      = return $ KeepAlive.SendMsgDone (pure ())
    loop td cookie = do
      start <- getMonotonicTime
      let cookie16 :: Word16
          cookie16 = fromIntegral cookie
      return $ KeepAlive.SendMsgKeepAlive (KeepAlive.Cookie cookie16) $ do
        end <- getMonotonicTime
        now <- getCurrentTime
        ready <- signalAndGetReadiness sig
        if ready
          then do
            printHeader opts headerVar statPointHeader
            let rtt = toSample end start
                td' = TDigest.insert rtt td
                point = toStatPoint now cookie16 rtt td'
            traceWith stdout $ LogStatPoint point
            threadDelay keepAliveDelay
            loop td' (cookie + 1)
          else do
            -- we keep updating the measurements but we don't show them, other
            -- clients are still connecting or negotiating their connection.
            let rtt = toSample end start
                td' = TDigest.insert rtt td
            threadDelay keepAliveDelay
            loop td' cookie


--
-- Ping Client
--

resolveAddress :: Tracer IO PingWarning
               -> DNS.Resolver
               -> PingOpts
               -> Address (Unresolved fpResolved)
               -> IO [Address Resolved]

-- 1. Resolve an SRV records
resolveAddress stderr resolver opts@PingOpts { pingOptsSRVPrefix = srvPrefix } (SRV dns) = do
  let hostname = BS.Char.pack $ case srvPrefix of
                                  [] -> dns
                                  _  -> srvPrefix ++ "." ++ dns
  r <- DNS.lookupRaw resolver hostname DNS.SRV
  case r >>= flip DNS.fromDNSMessage selectSRV of
    Left err -> do
      traceWith stderr $ DNSError hostname err
      -- try resolve the SRV as a domain:port or a filepath
      resolveAddress stderr resolver opts (FilePathOrDomain dns)
    Right services ->
      concat <$> traverse (resolveAddress stderr resolver opts)
        [ Domain domain (fromIntegral port)
        | (domain, _, _, port, _ttl)  <- services
        ]
  where
    selectSRV DNS.DNSMessage { DNS.answer } =
      [ (domain', priority', weight', port, ttl)
      | DNS.ResourceRecord {
          DNS.rdata = DNS.RD_SRV priority' weight' port domain',
          DNS.rrttl = ttl
        } <- answer
      ]

-- 2. Resolved domain name or file path
resolveAddress stderr resolver opts (FilePathOrDomain path) =
   case splitWith ':' path
           >>= \(dnsStr, portStr) -> (dnsStr,) <$> readMaybe portStr
     of
     Just (dnsname, port) ->
       -- try resolve as a domain name
       resolveAddress stderr resolver opts (Domain (BS.Char.pack dnsname) port)
        >>= \case
          [] ->
            -- dns query failed, let's try if it is a file path
            doesFileExist path >>= \case
              True -> pure [FilePath path]
              False -> do
                traceWith stderr (FilePathDoesNotExist path)
                pure []
          addrs -> pure addrs
     Nothing ->
       doesFileExist path >>= \case
         True ->
           pure [FilePath path]
         False -> do
           traceWith stderr (FilePathDoesNotExist path)
           pure []

-- 3. Resolve domain names
resolveAddress stderr resolver PingOpts { pingOptsQuiet } (Domain hostname port) = do
  a <- fmap (map IPv4) <$> DNS.lookupA resolver hostname
  aaaa <- fmap (map IPv6) <$> DNS.lookupAAAA resolver hostname
  case a <>: aaaa of
    Left err -> do
      traceWith stderr $ DNSError hostname err
      return []
    Right ips -> do
      unless pingOptsQuiet $
        traceWith stderr $ DNSResolution hostname ips port
      return [ IP ip port | ip <- ips ]
  where
    (<>:) :: Either e [a] -> Either e [a] -> Either e [a]
    (Left e) <>: Left{}    = Left e
    Right as <>: Left{}    = Right as
    Left{}   <>: Right as  = Right as
    Right as <>: Right as' = Right (as ++ as')

-- 4. Return ip address
resolveAddress _stderr _ _ (IP addr port) = pure [IP addr port]


pingClients
  :: PingOpts
  -> [Address (Unresolved SRVOrFilePathUnresolved)]
  -> IO ()
pingClients opts@PingOpts { pingOptsJson } addresses = do
    stdoutLock <- newMVar ()
    IO.hSetBuffering IO.stdout IO.LineBuffering
    let stdout :: Tracer IO (WithHost LogMsg)
        stdout = mkTracer $ \msg -> withMVar stdoutLock $ \_ -> TL.putStrLn (format pingOptsJson msg)

    stderrLock <- newMVar ()
    IO.hSetBuffering IO.stderr IO.LineBuffering
    let stderr :: Tracer IO PingWarning
        stderr = mkTracer $ \msg -> withMVar stderrLock $ \_ ->
          case msg of
            -- Don't print IllegalDomain errors, which are common when we try
            -- to resolve a file path as a DNS name.
            DNSError _ DNS.IllegalDomain -> pure ()
            _ -> IO.hPutStrLn IO.stderr (formatPingWarning msg)

    -- resolved addresses
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
    resolvedAddresses <-
      DNS.withResolver rs $ \resolver ->
        concat <$> traverse (resolveAddress stderr resolver opts) addresses
    sockAddrs
      <- catMaybes
         <$> traverse
               (\case
                  addr@(IP ip@IPv4{} port) ->
                    fmap (addr,) . maybeHead <$> Socket.getAddrInfo
                      (Just Socket.defaultHints { Socket.addrFamily = Socket.AF_INET })
                      (Just (show ip))
                      (Just (show port))
                  addr@(IP ip@IPv6{} port) ->
                    fmap (addr,) . maybeHead <$> Socket.getAddrInfo
                      (Just Socket.defaultHints { Socket.addrFamily = Socket.AF_INET6 })
                      (Just (show ip))
                      (Just (show port))
                  addr@(FilePath path) -> do
                    return $ Just
                      ( addr
                      , Socket.AddrInfo
                         []
                         Socket.AF_UNIX
                         Socket.Stream
                         Socket.defaultProtocol
                         (Socket.SockAddrUnix path)
                         Nothing
                      )
               )
               resolvedAddresses

    headerVar <- newHeaderVar
    signalVar <- newSignalVar (fst <$> sockAddrs)
    mapConcurrently_ (\case
                      -- ignore exceptions so other ping clients can
                      -- continue
                      addr@(IP {}, _) ->
                        void $ try @_ @SomeException $
                        pingClient NodeToNode stdout stderr opts signalVar headerVar addr
                      addr@(FilePath {}, _) ->
                        void $ try @_ @SomeException $
                        pingClient NodeToClient stdout stderr opts signalVar headerVar addr
                   ) sockAddrs

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
  -> Tracer IO (WithHost LogMsg)
  -> Tracer IO PingWarning
  -> PingOpts
  -> SignalVar (Address Resolved)
  -> HeaderVar
  -> (Address Resolved, AddrInfo)
  -> IO ()
pingClient protocol stdout stderr opts@PingOpts{..} signalVar headerVar (addr, peer) =
  withSignal signalVar addr $ \sig ->
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
        runPingClient sig sd
      )
  where
    runPingClient :: Signal -> Socket.Socket -> IO ()
    runPingClient sig sd = do
      let logMsg :: (ToText msg, ToJSON msg) => msg -> IO ()
          logMsg = logMsgWithPeer opts addr
          stdout' = WithHost addr >$< stdout

      !t0_s <- getMonotonicTime

      handleJust (\e -> case fromException e of { Just SomeAsyncException {} -> Nothing; _ -> Just e })
                 (\e  -> traceWith stderr (ConnectError (Socket.addrAddress peer) e)
                      >> throwIO e
                 ) $
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
        Left err -> do
          throwIO (PingClientProtocolLimitFailure err)
        Right (Left err', rtt) -> do
          logMsg (HandshakeRTT rtt)
          throwIO (PingClientHandshakeProtocolError err' addr)
        Right (Right r', rtt) -> do
          logMsg (HandshakeRTT rtt)
          case r' of
            HandshakeQueryResult versions -> do
              signalReadiness sig
              awaitReadiness sig
              -- print query results if it was supported by the remote side
              when (pingOptsMode == QueryMode) $ void $
                Map.traverseWithKey
                  (\version versionData ->
                    traceWith stdout' $
                      case protocol of
                        NodeToClient ->
                          LogNodeToClientVersionData version versionData
                        NodeToNode ->
                          LogNodeToNodeVersionData version versionData
                  )
                  versions
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
                          (ChainSync.chainSyncClientPeer $ chainSyncClient opts sig headerVar stdout'))
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
                      (\channel ->
                        runPeerWithLimits
                          nullTracer
                          KeepAlive.codecKeepAlive_v2
                          (KeepAlive.byteLimitsKeepAlive (fromIntegral . BL.length))
                          KeepAlive.timeLimitsKeepAlive
                          channel
                          (KeepAlive.keepAliveClientPeer
                            $ keepAliveClient
                                opts
                                sig
                                headerVar
                                stdout'
                                (TDigest.tdigest [])))
                        >>= void . atomically
                    )
                    `finally` Mx.stop mx
                  threadDelay idleTimeout

                (NodeToClient, PingMode) -> pure ()
                  --
                  -- ping mode over node-to-client protocol is not supported
                  --


toSample :: Time -> Time -> Double
toSample end start = realToFrac $ end `diffTime` start

format :: (ToText a, ToJSON a) => LogFormat -> a -> TL.Text
format AsJSON = encodeToLazyText . toJSON
format AsText = toText

class ToText a where
  toText :: a -> TL.Text

data WithHost a = WithHost (Address Resolved) a

instance ToText a => ToText (WithHost a) where
  toText (WithHost host a) =
    TL.pack (printf "%-47s" (show host ++ ", ")) <> toText a

instance ToJSON a => ToJSON (WithHost a) where
  toJSON (WithHost host a) =
    case toJSON a of
      Aeson.Object o ->
        Aeson.Object (KeyMap.insert "host" (toJSON $ show host) o)

      x -> object [ "host" .= show host, "data" .= x ]

newtype NegotiatedVersion versionNumber = NegotiatedVersion versionNumber

instance Show versionNumber
      => ToText (NegotiatedVersion versionNumber) where
  toText (NegotiatedVersion v) = TL.pack $ printf "negotiated versions: %s" (show v)

instance ToJSON versionNumber
      => ToJSON (NegotiatedVersion versionNumber) where
  toJSON (NegotiatedVersion v) = object ["negotiated_versions" .= toJSON v]


newtype QueriedVersions versionNumber = QueriedVersions [versionNumber]

instance Show versionNumber
      => ToText (QueriedVersions versionNumber) where
  toText (QueriedVersions vs) =
    TL.pack $ printf "Queried versions: %s" (unwords $ show <$> vs)

instance ToJSON versionNumber
      => ToJSON (QueriedVersions versionNumber) where
  toJSON (QueriedVersions vs) =
    object ["queried_versions" .= toJSON vs]


newtype NetworkRTT = NetworkRTT Double

instance ToText NetworkRTT where
  toText (NetworkRTT rtt) =
    TL.pack $ printf "network rtt: %.3fs" rtt

instance ToJSON NetworkRTT where
  toJSON (NetworkRTT rtt) =
    object ["network_rtt" .= toJSON rtt]


newtype HandshakeRTT = HandshakeRTT DiffTime

instance ToText HandshakeRTT where
  toText (HandshakeRTT diff) =
    TL.pack $ printf "handshake rtt: %.3fs" (realToFrac diff :: Double)

instance ToJSON HandshakeRTT where
  toJSON (HandshakeRTT diff) =
    object ["handshake rtt" .= toJSON ((fromRational $ toRational diff) :: Double)]


-- note: use `logMsg` defined above in terms of `logMsgWithPeer`
logMsgWithPeer :: (ToText msg, ToJSON msg)
               => PingOpts
               -> Address Resolved
               -> msg
               -> IO ()
logMsgWithPeer PingOpts { pingOptsQuiet, pingOptsJson } addr msg =
  unless pingOptsQuiet $ TL.hPutStrLn IO.stdout (format pingOptsJson (WithHost addr msg))


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


type SignalVar addr = StrictTVar IO (Map addr Bool)

data Signal = Signal {
    signalReadiness :: IO (),
    getReadiness    :: STM IO Bool
  }

awaitReadiness :: Signal -> IO ()
awaitReadiness Signal { getReadiness } =
  atomically (getReadiness >>= check)

signalAndGetReadiness :: Signal -> IO Bool
signalAndGetReadiness Signal { signalReadiness, getReadiness } = do
  signalReadiness
  atomically getReadiness

newSignalVar :: Ord addr
             => [addr]
             -> IO (SignalVar addr)
newSignalVar addrs = newTVarIO (Map.fromList [(addr, False) | addr <- addrs])

withSignal :: Ord addr
           => SignalVar addr
           -> addr
           -> (Signal -> IO a)
           -> IO a
withSignal var addr k =
  let signalReadiness :: IO ()
      signalReadiness = atomically $ modifyTVar var (Map.insert addr True)

      getReadiness :: STM IO Bool
      getReadiness = getAll . foldMap All <$> readTVar var
  in k Signal { signalReadiness, getReadiness }
     `onException`
     atomically (modifyTVar var (Map.insert addr True))


data HeaderState = NotPrinted | Printing | Printed
type HeaderVar = StrictTVar IO HeaderState

newHeaderVar :: IO HeaderVar
newHeaderVar = newTVarIO NotPrinted

printHeader :: PingOpts
            -> HeaderVar
            -> String
            -> IO ()
printHeader PingOpts { pingOptsJson } headerVar hdr = do
  when (pingOptsJson == AsText) $ do
    st <- atomically $ do
      st <- readTVar headerVar
      case st of
        NotPrinted -> writeTVar headerVar Printing
                   >> pure st
        Printing   -> retry
        Printed    -> return st
    case st of
      NotPrinted -> do
        IO.putStrLn hdr
        atomically (writeTVar headerVar Printed)
      Printing   -> error "impossible"
      Printed    -> pure ()
