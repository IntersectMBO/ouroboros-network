{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Network.Ping
  ( PingOpts (..)
  , LogFormat (..)
  , LogMsg (..)
  , StatPoint (..)
  , ProtocolFlavour (..)
  , pingClients
  , mainnetMagic
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (unless, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI qualified as MT
import Control.Tracer (Tracer (..), nullTracer, traceWith)

import Codec.CBOR.Term qualified as CBOR
import Codec.Serialise qualified as Serialise
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, encode, object)
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as LBS.Char
import Data.IP
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.TDigest (TDigest)
import Data.TDigest qualified as TDigest
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Word (Word16, Word32)
import Network.Mux (MiniProtocolInfo (..))
import Network.Mux qualified as Mx
import Network.Mux.Bearer (MakeBearer (..), makeSocketBearer)
import Network.Socket (AddrInfo, StructLinger (..))
import Network.Socket qualified as Socket
import System.IO qualified as IO
import System.Random (initStdGen)
import Text.Printf (printf)

import Cardano.Network.Diffusion.Configuration (defaultChainSyncIdleTimeout)
import Cardano.Network.NodeToClient qualified as NodeToClient
import Cardano.Network.NodeToClient.Version
import Cardano.Network.NodeToNode qualified as NodeToNode
import Cardano.Network.NodeToNode.Version
import Cardano.Network.OrphanInstances ()
import Cardano.Network.Protocol.ChainSync.Client (ChainSyncClient)
import Cardano.Network.Protocol.ChainSync.Client qualified as ChainSync
import Cardano.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Cardano.Network.Protocol.KeepAlive.Client (KeepAliveClient (..))
import Cardano.Network.Protocol.KeepAlive.Client qualified as KeepAlive
import Cardano.Network.Protocol.KeepAlive.Codec qualified as KeepAlive
import Cardano.Network.Protocol.KeepAlive.Type qualified as KeepAlive

import Ouroboros.Network.Block hiding (blockNo)
import Ouroboros.Network.ConnectionId
import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Protocol.Handshake hiding (Accept (..),
           RefuseReason (..))
import Ouroboros.Network.Util.ShowProxy

data LogFormat = AsJSON | AsText
  deriving (Eq, Show)

data PingOpts = PingOpts
  { pingOptsCount          :: Word32
    -- ^ Number of messages to send to the server
  , pingOptsHost           :: Maybe String
    -- ^ The host to connect to
  , pingOptsHandshakeQuery :: Bool
    -- ^ Whether to send a query during the handshake to request the available protocol versions
  , pingOptsUnixSock       :: Maybe String
    -- ^ The unix socket to connect to
  , pingOptsPort           :: String
    -- ^ The port to connect to
  , pingOptsMagic          :: Word32
    -- ^ The network magic to use for the connection
  , pingOptsJson           :: LogFormat
    -- ^ Print output in JSON
  , pingOptsQuiet          :: Bool
    -- ^ Less verbose output
  , pingOptsGetTip         :: Bool
    -- ^ Get Tip after handshake
  } deriving (Eq, Show)

mainnetMagic :: Word32
mainnetMagic = 764824073

data LogMsg = LogMsg BL.ByteString
            | LogEnd
            deriving Show


-- | Logging is done concurrently from multiple ping clients.
--
loggerThread :: PingOpts -> StrictTMVar IO LogMsg -> IO ()
loggerThread
    PingOpts { pingOptsJson,
               pingOptsGetTip,
               pingOptsHandshakeQuery
             }
    msgQueue
    =
    go True
  where
    go first = do
      msg <- atomically $ takeTMVar msgQueue
      case msg of
        LogMsg bs -> do
          let bs' = case (pingOptsJson, first, pingOptsGetTip) of
                (AsJSON, False, _)    ->
                  LBS.Char.pack ",\n" <> bs
                (AsJSON, True, False) ->
                  LBS.Char.pack "{ \"pongs\": [ " <> bs
                (AsJSON, True, True)  ->
                  LBS.Char.pack "{ \"tip\": [ " <> bs
                (AsText, True, False) ->
                  LBS.Char.pack "timestamp,                      host,                         cookie,  sample,  median,     p90,    mean,     min,     max,     std\n" <> bs
                (AsText, True, True)  -> bs
                (AsText, False, _)    -> bs

          LBS.Char.putStr bs'
          go False
        LogEnd -> when (pingOptsJson == AsJSON && not pingOptsHandshakeQuery) $ IO.putStrLn "] }"

sduTimeout :: MT.DiffTime
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
  , spHost      :: TL.Text
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


keepAliveDelay :: MT.DiffTime
keepAliveDelay = 1

idleTimeout :: MT.DiffTime
idleTimeout = 5


data PingClientError
  = PingClientProtocolLimitFailure ProtocolLimitFailure
  -- ^ protocol limit error

  | forall versionNumber.
    Show versionNumber
  => PingClientHandshakeProtocolError (HandshakeProtocolError versionNumber) TL.Text
  -- ^ handshake protocol error

  | PingClientIPAddressFailure TL.Text
  -- ^ failed to get IP address from SockAddr

deriving instance Show PingClientError

instance Exception PingClientError where
  displayException (PingClientIPAddressFailure peerStr) =
    printf "%s expected an IP address\n" peerStr
  displayException (PingClientProtocolLimitFailure err) =
    displayException err
  displayException (PingClientHandshakeProtocolError err peerStr) =
    printf "%s handshake error: %s" peerStr (show err)

data ProtocolFlavour version versionData where
    NodeToNode   :: NetworkMagic
                 -> Bool
                 -> ProtocolFlavour NodeToNodeVersion
                                    NodeToNodeVersionData
    NodeToClient :: NetworkMagic
                 -> Bool
                 -> ProtocolFlavour NodeToClientVersion
                                    NodeToClientVersionData

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
  -> TDigest 5
  -> KeepAlive.Cookie
  -> KeepAliveClient IO ()
keepAliveClient stdout peerName logFormat td0 cookie0 =
    KeepAliveClient $ loop td0 cookie0
  where
    loop :: TDigest 5
         -> KeepAlive.Cookie
         -> IO (KeepAlive.KeepAliveClientSt IO ())
    loop td cookie = do
      start <- getMonotonicTime
      return $ KeepAlive.SendMsgKeepAlive cookie $ do
        end <- getMonotonicTime
        now <- getCurrentTime
        let rtt = toSample end start
            td' = TDigest.insert rtt td
            point = toStatPoint now peerName (KeepAlive.unCookie cookie) rtt td'
        case logFormat of
          AsJSON -> traceWith stdout $ LogMsg (encode point)
          AsText -> traceWith stdout $ LogMsg $ LBS.Char.pack $ show point <> "\n"
        MT.threadDelay keepAliveDelay
        loop td' (KeepAlive.Cookie (KeepAlive.unCookie cookie + 1))


--
-- Ping Client
--

pingClients
  :: forall versionNumber versionData.
     ( Acceptable versionData
     , Queryable versionData
     , Ord versionNumber
     , Show versionNumber
     , ToJSON versionNumber
     )
  => ProtocolFlavour versionNumber versionData
  -> Tracer IO String
  -> PingOpts
  -> [AddrInfo]
  -> IO ()
pingClients protocol stderr opts peers = do

    msgQueue <- newEmptyTMVarIO
    let tracer :: Tracer IO LogMsg
        tracer = Tracer $ \msg -> atomically $ putTMVar msgQueue msg

    mapConcurrently_ (pingClient protocol tracer opts) peers
      `race_`
      loggerThread opts msgQueue
      `catch`
      \(e :: SomeException) -> traceWith stderr (displayException e)


pingClient
  :: forall versionNumber versionData.
     ( Acceptable versionData
     , Queryable versionData
     , Ord versionNumber
     , Show versionNumber
     , ToJSON versionNumber
     )
  => ProtocolFlavour versionNumber versionData
  -> Tracer IO LogMsg
  -> PingOpts
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
            NodeToNode {}   -> NodeToNode.nodeToNodeHandshakeCodec
            NodeToClient {} -> NodeToClient.nodeToClientHandshakeCodec,
          haVersionDataCodec = case protocol of
            NodeToNode {}   -> cborTermVersionDataCodec nodeToNodeCodecCBORTerm
            NodeToClient {} -> cborTermVersionDataCodec nodeToClientCodecCBORTerm,
          haAcceptVersion = acceptableVersion,
          haQueryVersion  = queryVersion,
          haTimeLimits    = timeLimitsHandshake
        }
        (case protocol of
           NodeToNode networkMagic query  ->
             foldMapVersions
               (\versionNumber ->
                 simpleSingletonVersions
                   versionNumber
                   NodeToNodeVersionData {
                     networkMagic,
                     diffusionMode = InitiatorOnlyDiffusionMode,
                     peerSharing   = PeerSharingDisabled,
                     query
                   }
                   (const ())
               )
               [minBound..maxBound]

           NodeToClient networkMagic query  ->
             foldMapVersions
               (\versionNumber ->
                 simpleSingletonVersions
                   versionNumber
                   NodeToClientVersionData {
                     networkMagic,
                     query
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
              unless pingOptsHandshakeQuery $
                logMsg $ QueriedVersions (Map.keys versions)
            HandshakeNegotiationResult _ version _versionData -> do
              -- show negotiated version
              logMsg $ NegotiatedVersion version
              case protocol of
                _ | pingOptsGetTip -> do
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
                                                       NodeToNode {} -> NodeToNode.chainSyncMiniProtocolNum
                                                       NodeToClient {} -> NodeToClient.localChainSyncMiniProtocolNum,
                            miniProtocolDir        = Mx.InitiatorDirectionOnly,
                            miniProtocolLimits     = case protocol of
                                                       NodeToNode {} -> NodeToNode.chainSyncProtocolLimits NodeToNode.defaultMiniProtocolParameters
                                                       NodeToClient {} -> NodeToClient.maximumMiniProtocolLimits,
                            miniProtocolCapability = Nothing
                          }]
                  race_ (Mx.run mx bearer)
                        (Mx.runMiniProtocol mx
                          NodeToNode.chainSyncMiniProtocolNum
                          Mx.InitiatorDirectionOnly
                          Mx.StartEagerly
                          $ \channel -> do
                             runPeerWithLimitsRnd
                              nullTracer
                              stdGen
                              (ChainSync.codecChainSync CBOR.encodeTerm CBOR.decodeTerm
                                                        CBOR.encodeTerm CBOR.decodeTerm
                                                        (encodeTip Serialise.encode)
                                                        (decodeTip Serialise.decode))
                              (ChainSync.byteLimitsChainSync (fromIntegral . BL.length))
                              (ChainSync.timeLimitsChainSync defaultChainSyncIdleTimeout)
                              channel
                              (ChainSync.chainSyncClientPeer $ chainSyncClient stdout host pingOptsJson)
                          )
                    `finally` Mx.stop mx
                NodeToNode {} | otherwise -> do
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
                  race_ (Mx.run mx bearer)
                        (Mx.runMiniProtocol mx
                          NodeToNode.chainSyncMiniProtocolNum
                          Mx.InitiatorDirectionOnly
                          Mx.StartEagerly
                          $ \channel -> do
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
                                    (TDigest.tdigest [])
                                    (KeepAlive.Cookie 0))
                          )
                    `finally` Mx.stop mx
                NodeToClient {} | otherwise -> pure ()
              MT.threadDelay idleTimeout

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

instance Format [NodeToClientVersion] where
  format AsJSON vs = encodeToLazyText $ object ["negotiated_versions" .= toJSON vs]
  format AsText v = TL.pack $ printf "Negotiated versions %s" (show v)

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
               => PingOpts
               -> TL.Text -- ^ peer identifier
               -> msg
               -> IO ()
logMsgWithPeer PingOpts { pingOptsQuiet, pingOptsJson } peerStr msg =
  unless pingOptsQuiet $ TL.hPutStrLn IO.stdout (peerStr <> " " <> format pingOptsJson msg)


instance ShowProxy CBOR.Term where
  showProxy _ = "CBOR.Term"
