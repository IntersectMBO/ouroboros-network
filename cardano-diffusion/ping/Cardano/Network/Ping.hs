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
  ( -- * API
    pingClients
  , pingClients'
  , pingClient
    -- * Options and arguments
  , PingOpts (..)
  , Stage (..)
  , ResolvedSRVOrFilePath (..)
  , Address (IP)
  , mkAddress
  , cmdlineParser
  , PingMode (..)
  , AcceptFilePath (..)
  , LogFormat (..)
  , ColorMode (..)
    -- * Log messages
  , PingWarning (..)
  , PingException (..)
  , WithHost (..)
  , LogMsg (..)
  , StatPoint (..)
  , mkStdOutTracer
  , mkStdErrTracer
  , mkHeaderTracer
    -- ** Formatting log messages
  , format
  , ToText (..)
    -- * Exceptions
  , PingClientException (..)
    -- * Cardano main-net configuration
  , mainnetMagic
    -- * Re-exports
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

import Codec.CBOR.Read qualified as CBOR
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
import Data.Either (lefts)
import Data.Foldable (traverse_)
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
import Network.Socket (AddrInfo, SockAddr, StructLinger (..))
import Network.Socket qualified as Socket
import Options.Applicative
import Options.Applicative.Help.Pretty qualified as Pretty
import System.Directory (doesFileExist)
import System.Exit qualified as IO
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

data ColorMode = ColorAuto | ColorNever | ColorAlways
  deriving (Eq, Show)


data PingMode =
    TipMode
    -- ^ query tip
  | PingMode
    -- ^ ping
  | QueryMode
    -- ^ query handshake parameters
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

deriving instance Show (Address resolved)


-- | A smart constructor for `Address` type.
--
mkAddress :: String -> Address (Unresolved SRVOrFilePathUnresolved)
mkAddress s | illegalDomain = FilePathOrDomain s
            | otherwise     = SRV s
  where
    -- A basic test to decide between `FilePathOrDomain` or `SRV`.  The `dns`
    -- library has a slightly more advanced version and returns `IllegalDomain`
    -- error.
    illegalDomain :: Bool
    illegalDomain
      | ':' `List.elem` s    = True
      | '/' `List.elem` s    = True
      | '.' `List.notElem` s = True
      | otherwise            = False


showIPWithPort :: IP -> Port -> String
showIPWithPort ip@IPv4{} port = show ip ++ ":" ++ show port
showIPWithPort ip@IPv6{} port = "[" ++ show ip ++ "]:" ++ show port

ppAddress :: Address stage -> String
ppAddress (IP ip port)            = showIPWithPort ip port
ppAddress (FilePath path)         = path
ppAddress (FilePathOrDomain path) = path
ppAddress (Domain domain port)    = BS.Char.unpack domain ++ ":" ++ show port
ppAddress (SRV domain)            = domain

deriving instance Eq (Address Resolved)
deriving instance Ord (Address Resolved)

data SomeAddress where
    SomeAddress :: forall (stage :: Stage).
                   Address stage
                -> SomeAddress

instance Show SomeAddress where
  show (SomeAddress address) = show address

ppSomeAddress :: SomeAddress -> String
ppSomeAddress (SomeAddress addr) = ppAddress addr

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
  , pingOptsColor     :: ColorMode
    -- ^ Colorised output
  }

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
    <*> option colorMode
        (   long "color"
         <> help "Colorized output: auto, never or always."
         <> value ColorAuto
         <> metavar "COLOR"
         <> showDefaultWith (\case { ColorAuto -> "auto"; ColorNever -> "never"; ColorAlways -> "always" })
        )
  where
    pingMode :: ReadM PingMode
    pingMode =
      eitherReader $ \case
        "tip" -> Right TipMode
        "ping" -> Right PingMode
        "query" -> Right QueryMode
        _ -> Left "unexpected string"

    colorMode :: ReadM ColorMode
    colorMode =
      eitherReader $ \case
        "auto"   -> Right ColorAuto
        "never"  -> Right ColorNever
        "always" -> Right ColorAlways
        _        -> Left "expected auto, never or always"


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
        readDomainNameOrFilePath = eitherReader $ Right . mkAddress

cmdlineParser :: Parser (PingOpts, [Address (Unresolved SRVOrFilePathUnresolved)])
cmdlineParser = (,) <$> pingOptsParser <*> argParser


-- | Log messages to stdout.
--
data LogMsg = LogChainSyncTip PingTip
            | LogStatPoint StatPoint
            | LogNodeToClientVersionData NodeToClientVersion (Either Text NodeToClientVersionData)
            | LogNodeToNodeVersionData   NodeToNodeVersion   (Either Text NodeToNodeVersionData)
  deriving Show



data LogInfoMsg = LogNetworkRTT NetworkRTT
                | LogHandshakeRTT HandshakeRTT
                | forall versionNumber.
                  (Show versionNumber, ToText versionNumber, ToJSON versionNumber)
                  => LogNegotiatedVersion (NegotiatedVersion versionNumber)

deriving instance Show LogInfoMsg


data AddressResolutionError =
     FilePathDoesNotExistError FilePath
   | NoPortNumberError SomeAddress
   | DNSError SomeAddress DNS.DNSError
  deriving Show

instance Exception AddressResolutionError where
  displayException (FilePathDoesNotExistError path)
    = "file path " ++ show path ++ " does not exist"
  displayException (NoPortNumberError addr)
    = "missing port number for " ++ ppSomeAddress addr
  displayException (DNSError addr err)
    = ppSomeAddress addr ++ ": " ++ displayException err

-- | Log messages to stderr.
--
data PingWarning = AddressResolutionError AddressResolutionError
                 | DNSResolution DNS.Domain [IP] Word
                 | Error SomeException
                 | ConnectError SockAddr SomeException


instance ToText PingWarning where
  toText msg = severity msg <> fn msg
    where
      fn (AddressResolutionError err)
        = TL.pack $ displayException err
      fn (DNSResolution domain ips port)
        = TL.pack $ concat
        [ BS.Char.unpack domain
        , ": "
        , List.intercalate ", " (flip showIPWithPort port <$> ips)
        ]
      fn (Error err)
        = TL.pack $ displayException err
      fn (ConnectError sockAddr err)
        = TL.pack $ printf "%-47s %s" (show sockAddr) (displayException err)

      severity :: PingWarning -> TL.Text
      severity = \case
          AddressResolutionError{} -> warning
          DNSResolution{}          -> mempty
          Error{}                  -> warning
          ConnectError{}           -> warning
        where
          warning = TL.pack "WARNING: "


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

instance ToText LogInfoMsg where
  toText (LogNetworkRTT rtt)      = toText rtt
  toText (LogHandshakeRTT rtt)    = toText rtt
  toText (LogNegotiatedVersion v) = toText v

instance ToJSON LogMsg where
  toJSON (LogChainSyncTip tip) = toJSON tip
  toJSON (LogStatPoint point) = toJSON point
  toJSON (LogNodeToClientVersionData version versionData)
    = object [ fromString (show version) .= either toJSON toJSON versionData ]
  toJSON (LogNodeToNodeVersionData version versionData)
    = object [ fromString (show version) .= either toJSON toJSON versionData ]

instance ToJSON LogInfoMsg where
  toJSON (LogNetworkRTT rtt)      = toJSON rtt
  toJSON (LogHandshakeRTT rtt)    = toJSON rtt
  toJSON (LogNegotiatedVersion v) = toJSON v


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


data Header = PingHeader | TipHeader

instance ToText Header where
  toText PingHeader = TL.pack
                    $ printf "%-46s %30s, %6s, %6s, %6s, %6s, %6s, %6s, %6s, %6s"
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
  toText TipHeader = TL.pack
                   $ printf "%-47s %6s, %64s, %9s, %10s"
                       ("host,"   :: String)
                       ("rtt"     :: String)
                       ("hash"    :: String)
                       ("blockNo" :: String)
                       ("slotNo"  :: String)

instance ToJSON Header where
  toJSON _ = Aeson.Null

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


data PingClientException
  = ProtocolLimitException ProtocolLimitFailure SockAddr
  -- ^ protocol limit error

  | forall versionNumber.
    Show versionNumber
  => HandshakeException (HandshakeProtocolError versionNumber) SockAddr
  -- ^ handshake protocol error

  | IOException IOError SockAddr
  -- ^ IO errors

  | DecodingException CBOR.DeserialiseFailure SockAddr
  -- ^ cbor exceptions

  | MuxException Mx.Error SockAddr
  -- ^ mux exceptions

deriving instance Show PingClientException

instance Exception PingClientException where
  displayException (ProtocolLimitException err addr) =
    printf "%s protocol limits error: %s" (show addr) (displayException err)
  displayException (HandshakeException err addr) =
    printf "%s handshake error: %s" (show addr) (show err)
  displayException (IOException err addr) =
    printf "%s io error: %s" (show addr) (displayException err)
  displayException (DecodingException err addr) =
    printf "%s decoding error: %s" (show addr) (displayException err)
  displayException (MuxException err addr) =
    printf "%s mux error: %s" (show addr) (displayException err)

data ProtocolFlavour version versionData where
    NodeToNode   :: ProtocolFlavour NodeToNodeVersion   NodeToNodeVersionData
    NodeToClient :: ProtocolFlavour NodeToClientVersion NodeToClientVersionData

data SomeProtocolFlavour where
    SomeProtocolFlavour :: forall versionNumber versionData.
                           ( Acceptable versionData
                           , Queryable versionData
                           , NFData versionData
                           , Ord versionNumber
                           , Show versionNumber
                           , NFData versionNumber
                           , ToJSON versionNumber
                           , ToText versionNumber
                           )
                        => ProtocolFlavour versionNumber versionData
                        -> SomeProtocolFlavour

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
  :: Signal
  -> Tracer IO LogMsg
  -> Tracer IO Header
  -> ChainSyncClient ChainSyncHeader ChainSyncPoint ChainSyncTip IO ()
chainSyncClient sig@Signal { signalReadiness } stdout headerTracer =
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
             traceWith headerTracer TipHeader
             traceWith stdout (LogChainSyncTip pingTip)
             return $ ChainSync.SendMsgDone ()
       }


--
-- KeepAlive RTT sampling
--

keepAliveClient
  :: PingOpts
  -> Signal
  -> Tracer IO LogMsg
  -> Tracer IO Header
  -> TDigest 5
  -> KeepAliveClient IO ()
keepAliveClient PingOpts { pingOptsCount } sig stdout headerTracer td0 =
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
            traceWith headerTracer PingHeader
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


-- | `resolveAddress` has two modes of operations.  It can try to resolve an
-- `Address` to a `FilePath`, or not.  In the latter case the errors will be
-- more precise.  It is useful when validating an srv record / domain names / ip
-- addresses.
--
data AcceptFilePath = AddressMightBeAFilePath | AddressIsNotAFilePath


resolveAddress
  :: Tracer IO PingWarning
  -- ^ stderr
  -> DNS.Resolver
  -> PingOpts
  -> AcceptFilePath
  -- ^ if ``
  -- then we won't try to resolve an address as a `FilePath`.
  -> Address (Unresolved SRVOrFilePathUnresolved)
  -- ^ Either `FilePathOrDomain` or `SRV`, use `mkAddress` to
  -- construct the right one.
  -> IO ([Address Resolved], [AddressResolutionError])
resolveAddress
    stderr resolver
    PingOpts { pingOptsQuiet, pingOptsSRVPrefix = srvPrefix }
    acceptFilePath
    =
    go
  where
    -- resolution loop
    go :: Address (Unresolved fpResolved)
       -> IO ([Address Resolved], [AddressResolutionError])

    -- 1. Resolve an SRV record
    go addr@(SRV dns) = do
      let hostname = BS.Char.pack $ case srvPrefix of
                                      [] -> dns
                                      _  -> srvPrefix ++ "." ++ dns
      r <- DNS.lookupRaw resolver hostname DNS.SRV
      case r >>= flip DNS.fromDNSMessage selectSRV of
        Left err -> do
          let err' = DNSError (SomeAddress addr) err
          traceWith stderr (AddressResolutionError err')
          case acceptFilePath of
            AddressMightBeAFilePath ->
              -- try resolve the SRV as a file path
              go (FilePathOrDomain dns)
            AddressIsNotAFilePath ->
              pure ([], [err'])
        Right services ->
          concatBoth <$> traverse go
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
    go addr@(FilePathOrDomain path) =
       case splitWith ':' path
               >>= \(dnsStr, portStr) -> (dnsStr,) <$> readMaybe portStr
         of
         Just (dnsname, port) ->
           -- try resolve as a domain name
           go (Domain (BS.Char.pack dnsname) port)
             >>= \case
               ([], errs) | AddressMightBeAFilePath <- acceptFilePath ->
                 -- dns query failed, let's try if it is a file path
                 doesFileExist path >>= \case
                   True -> pure ([FilePath path], errs)
                   False -> do
                     let err = FilePathDoesNotExistError path
                     traceWith stderr (AddressResolutionError err)
                     pure ([], err : errs)
               (addrs, errs) -> pure (addrs, errs)
         Nothing | AddressMightBeAFilePath <- acceptFilePath -> do
           doesFileExist path >>= \case
             True ->
               pure ([FilePath path], [])
             False -> do
               let err = FilePathDoesNotExistError path
               traceWith stderr (AddressResolutionError err)
               pure ([], [err])
         Nothing -> do
           pure ([], [NoPortNumberError (SomeAddress addr)])

    -- 3. Resolve domain names
    go addr@(Domain hostname port) = do
      a <- fmap (map IPv4) <$> DNS.lookupA resolver hostname
      aaaa <- fmap (map IPv6) <$> DNS.lookupAAAA resolver hostname
      case a <>: aaaa of
        (ips, errs) -> do
          let errs' = [ DNSError (SomeAddress addr) err | err <- errs ]
          traverse_ (traceWith stderr . AddressResolutionError) errs'
          unless pingOptsQuiet $
            traceWith stderr $ DNSResolution hostname ips port
          return ( [ IP ip port | ip <- ips ]
                 , errs'
                 )
      where
        (<>:) :: Eq e => Either e [a] -> Either e [a] -> ([a], [e])
        (Left e) <>: Left e'   | e /= e'
                               = ([], [e, e'])
                               | otherwise
                               = ([], [e])
        Right as <>: Left e'   = (as, [e'])
        Left e   <>: Right as  = (as, [e])
        Right as <>: Right as' = (as ++ as', [])

    -- 4. Return ip address
    go (IP addr port) = pure ([IP addr port], [])


pingClients
  :: PingOpts
  -> [Address (Unresolved SRVOrFilePathUnresolved)]
  -> IO ()
pingClients
  opts@PingOpts { pingOptsJson }
  addresses = do
  stdout <- mkStdOutTracer
  stderr <- mkStdErrTracer
  headerTracer <- mkHeaderTracer opts stdout

  errors <-
    pingClients'
      (format pingOptsJson >$< stdout)
      (format pingOptsJson >$< stdout)
      headerTracer
      (toText >$< stderr)
      opts
      AddressMightBeAFilePath addresses
  unless (null errors)
    IO.exitFailure


data PingException
  = AddressResolutionException AddressResolutionError
  | PingClientException PingClientException
  deriving Show

instance Exception PingException where
  displayException (AddressResolutionException e) = displayException e
  displayException (PingClientException e)        = displayException e

-- | A low level API which returns all address resolution & clients errors.
--
pingClients'
  :: Tracer IO (WithHost LogMsg)
  -- ^ trace log messages
  -> Tracer IO (WithHost LogInfoMsg)
  -- ^ info messages (e.g. RTTs, negotiated version numbers)
  -> Tracer IO Header
  -- ^ print header for ping measurements or tip information; it should be
  -- enabled if the first tracer is enabled.
  -> Tracer IO PingWarning
  -- ^ trace warning messages
  -> PingOpts
  -- ^ options for the ping command
  -> AcceptFilePath
  -- ^ accept file paths as addresses; if set to `AddressIsNotAFilePath` we'll
  -- never return file path related errors in `AddressResolutionError`.
  -> [Address (Unresolved SRVOrFilePathUnresolved)]
  -> IO [PingException]
  -- ^ returns two list:
  -- * list of resolve errors (printed with `PingWarning` tracer)
  -- * list of `pingClient` errors
pingClients' stdout infoTracer headerTracer stderr opts acceptFilePath addresses = do
    -- resolved addresses
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
    (resolvedAddresses, resolveErrors) <-
      DNS.withResolver rs $ \resolver ->
        concatBoth <$> traverse (resolveAddress stderr resolver opts acceptFilePath) addresses
    sockAddrs
      <- catMaybes
         <$> traverse
               (\case
                  (IP ip@IPv4{} port) ->
                    maybeHead <$> Socket.getAddrInfo
                      (Just Socket.defaultHints { Socket.addrFamily = Socket.AF_INET })
                      (Just (show ip))
                      (Just (show port))
                  (IP ip@IPv6{} port) ->
                    maybeHead <$> Socket.getAddrInfo
                      (Just Socket.defaultHints { Socket.addrFamily = Socket.AF_INET6 })
                      (Just (show ip))
                      (Just (show port))
                  (FilePath path) -> do
                    return $ Just $
                      Socket.AddrInfo
                       []
                       Socket.AF_UNIX
                       Socket.Stream
                       Socket.defaultProtocol
                       (Socket.SockAddrUnix path)
                       Nothing
               )
               resolvedAddresses

    signalVar <- newSignalVar (Socket.addrAddress <$> sockAddrs)
    results <-
      mapConcurrently
        (pingClient' stdout infoTracer headerTracer stderr opts signalVar)
        sockAddrs
    return $ (AddressResolutionException <$> resolveErrors)
          ++ (PingClientException <$> lefts results)


-- | Run a single ping client.
--
pingClient
  :: Tracer IO (WithHost LogMsg)
  -- ^ stdout
  -> Tracer IO (WithHost LogInfoMsg)
  -- ^ info tracer
  -> Tracer IO Header
  -> Tracer IO PingWarning
  -- ^ stderr
  -> PingOpts
  -> AddrInfo
  -> IO (Either PingClientException ())
pingClient stdout infoTracer headerTracer stderr opts addrInfo = do
  signalVar <- newSignalVar [Socket.addrAddress addrInfo]
  pingClient' stdout infoTracer headerTracer stderr opts signalVar addrInfo


-- | Low level API to run a single ping client.
--
pingClient'
  :: Tracer IO (WithHost LogMsg)
  -> Tracer IO (WithHost LogInfoMsg)
  -> Tracer IO Header
  -> Tracer IO PingWarning
  -> PingOpts
  -> SignalVar SockAddr
  -> AddrInfo
  -> IO (Either PingClientException ())
pingClient' stdout infoTracer headerTracer stderr opts@PingOpts{..} signalVar addrInfo =
  handlePingClientExceptions $
  withSignal signalVar addr $ \sig ->
    bracket
      (Socket.socket (Socket.addrFamily addrInfo) Socket.Stream Socket.defaultProtocol)
      Socket.close
      (\sd -> do
        when (Socket.addrFamily addrInfo /= Socket.AF_UNIX) $ do
          Socket.setSocketOption sd Socket.NoDelay 1
          Socket.setSockOpt sd Socket.Linger
            StructLinger
              { sl_onoff  = 1
              , sl_linger = 0
              }
        let someProtocol :: SomeProtocolFlavour
            someProtocol = case Socket.addrFamily addrInfo of
              Socket.AF_UNIX -> SomeProtocolFlavour NodeToClient
              _              -> SomeProtocolFlavour NodeToNode
        case someProtocol of
          SomeProtocolFlavour protocol -> runPingClient protocol sig sd
      )
  where
    addr :: SockAddr
    addr = Socket.addrAddress addrInfo

    handlePingClientExceptions
      :: IO ()
      -> IO (Either PingClientException ())
    handlePingClientExceptions io =
      (Right <$> io)
      `catch` \case
        err | Just e <- fromException err -> pure (Left (IOException e addr))
            | Just e <- fromException err -> pure (Left (DecodingException e addr))
            | Just e <- fromException err -> pure (Left (MuxException e addr))
            | Just e <- fromException err -> pure (Left e)
            | otherwise
            -> throwIO err

    runPingClient :: forall versionNumber versionData.
                     ( Acceptable versionData
                     , Queryable versionData
                     , NFData versionData
                     , Ord versionNumber
                     , Show versionNumber
                     , NFData versionNumber
                     , ToJSON versionNumber
                     , ToText versionNumber
                     )
                  => ProtocolFlavour versionNumber versionData
                  -> Signal
                  -> Socket.Socket
                  -> IO ()
    runPingClient protocol sig sd = do
      let stdout' :: Tracer IO LogMsg
          stdout' =  WithHost addr >$< stdout

          infoTracer' :: Tracer IO LogInfoMsg
          infoTracer' = WithHost addr >$< infoTracer

      !t0_s <- getMonotonicTime

      handleJust (\e -> case fromException e of { Just SomeAsyncException {} -> Nothing; _ -> Just e })
                 (\e  -> traceWith stderr (ConnectError addr e)
                      >> throwIO e
                 ) $
        Socket.connect sd addr
      !t0_e <- getMonotonicTime

      traceWith infoTracer' (LogNetworkRTT $ NetworkRTT (toSample t0_e t0_s))

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
          throwIO (ProtocolLimitException err addr)
        Right (Left err', rtt) -> do
          traceWith infoTracer' (LogHandshakeRTT $ HandshakeRTT rtt)
          throwIO (HandshakeException err' addr)
        Right (Right r', rtt) -> do
          traceWith infoTracer' (LogHandshakeRTT $ HandshakeRTT rtt)
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
              traceWith infoTracer' (LogNegotiatedVersion $ NegotiatedVersion version)
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
                          (ChainSync.chainSyncClientPeer $ chainSyncClient sig stdout' headerTracer))
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
                                stdout'
                                headerTracer
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

instance ToText String where
  toText = TL.pack

instance ToText TL.Text where
  toText = id

instance ToText NodeToClientVersion where
  toText = TL.pack . show

instance ToText NodeToNodeVersion where
  toText = TL.pack . show

data WithHost a = WithHost SockAddr a
  deriving Show

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
  deriving Show

instance Show versionNumber
      => ToText (NegotiatedVersion versionNumber) where
  toText (NegotiatedVersion v) = TL.pack $ printf "negotiated versions: %s" (show v)

instance ToJSON versionNumber
      => ToJSON (NegotiatedVersion versionNumber) where
  toJSON (NegotiatedVersion v) = object ["negotiated_versions" .= toJSON v]


{-
newtype QueriedVersions versionNumber = QueriedVersions [versionNumber]

instance Show versionNumber
      => ToText (QueriedVersions versionNumber) where
  toText (QueriedVersions vs) =
    TL.pack $ printf "Queried versions: %s" (unwords $ show <$> vs)

instance ToJSON versionNumber
      => ToJSON (QueriedVersions versionNumber) where
  toJSON (QueriedVersions vs) =
    object ["queried_versions" .= toJSON vs]
-}


newtype NetworkRTT = NetworkRTT Double
  deriving Show

instance ToText NetworkRTT where
  toText (NetworkRTT rtt) =
    TL.pack $ printf "network rtt: %.3fs" rtt

instance ToJSON NetworkRTT where
  toJSON (NetworkRTT rtt) =
    object ["network_rtt" .= toJSON rtt]


newtype HandshakeRTT = HandshakeRTT DiffTime
  deriving Show

instance ToText HandshakeRTT where
  toText (HandshakeRTT diff) =
    TL.pack $ printf "handshake rtt: %.3fs" (realToFrac diff :: Double)

instance ToJSON HandshakeRTT where
  toJSON (HandshakeRTT diff) =
    object ["handshake rtt" .= toJSON ((fromRational $ toRational diff) :: Double)]

instance ShowProxy CBOR.Term where
  showProxy _ = "CBOR.Term"

--
-- Utils
--

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (a:_) = Just a

concatBoth :: [([a], [b])] -> ([a], [b])
concatBoth a = (concatMap fst a, concatMap snd a)

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

mkHeaderTracer
  :: PingOpts
  -> Tracer IO TL.Text
  -> IO (Tracer IO Header)
mkHeaderTracer PingOpts { pingOptsJson = AsJSON } _ = pure nullTracer
mkHeaderTracer PingOpts { pingOptsJson = AsText, pingOptsColor } stdout = do
  headerVar <- newHeaderVar
  return $ mkTracer $ \hdr -> do
    st <- atomically $ do
      st <- readTVar headerVar
      case st of
        NotPrinted -> writeTVar headerVar Printing
                   >> pure st
        Printing   -> retry
        Printed    -> return st
    case st of
      NotPrinted -> do
        useColor <- case pingOptsColor of
          ColorAlways -> pure True
          ColorNever  -> pure False
          ColorAuto   -> IO.hIsTerminalDevice IO.stdout
        traceWith stdout (if useColor
                             then "\ESC[1m" <> toText hdr <> "\ESC[0m"
                             else toText hdr)
        atomically (writeTVar headerVar Printed)
      Printing   -> error "impossible"
      Printed    -> pure ()


mkStdOutTracer :: IO (Tracer IO TL.Text)
mkStdOutTracer = do
  lock <- newMVar ()
  IO.hSetBuffering IO.stdout IO.LineBuffering
  return $ mkTracer $ \msg -> withMVar lock $ \_ ->
    TL.putStrLn msg


mkStdErrTracer :: IO (Tracer IO TL.Text)
mkStdErrTracer = do
  lock <- newMVar ()
  IO.hSetBuffering IO.stderr IO.LineBuffering
  return $ mkTracer $ \msg -> withMVar lock $ \_ ->
    TL.hPutStrLn IO.stderr msg
