{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}



{-# OPTIONS_GHC -Wno-orphans  #-}

{-- From

https://github.com/IntersectMBO/cardano-node/pull/6331/files#diff-ee820dc9475e1d652becd547fe121d65cb0892518b54b1a6e92db3319835bbaa

https://github.com/IntersectMBO/cardano-node/blob/eeb0a0d8c99600e5add006d291467156b197b943/cardano-node/src/Cardano/Node/Tracing/Tracers/Diffusion.hs

--}

module DMQ.Tracer.Types
  () where


import           Cardano.Logging
--import           Cardano.Node.Configuration.TopologyP2P ()

#ifdef linux_HOST_OS
import           Network.Mux.TCPInfo (StructTCPInfo (..))
#endif
import qualified Ouroboros.Network.Diffusion.Types as Diff
import           Ouroboros.Network.PeerSelection.LedgerPeers (NumberOfPeers (..), PoolStake (..),
                   TraceLedgerPeers (..))
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import qualified Network.Mux as Mux
import           Network.Mux.Types (SDUHeader (..), unRemoteClockModel)
import           Network.TypedProtocol.Codec (AnyMessage (..))

import           Data.Aeson (Value (String), (.=))
import qualified Data.List as List
import           Data.Text (Text, pack)
import           Data.Typeable
import           Formatting

--------------------------------------------------------------------------------
-- Mux Tracer
--------------------------------------------------------------------------------

instance (LogFormatting peer, LogFormatting tr, Typeable tr) =>
    LogFormatting (Mux.WithBearer peer tr) where
    forMachine dtal (Mux.WithBearer b ev) =
      mconcat [ "bearer" .= forMachine dtal b
              , "event"  .= forMachine dtal ev
              -- "kind"   .= (show . typeOf $ ev)
              ]
    forHuman (Mux.WithBearer b ev) = "With mux bearer " <> forHumanOrMachine b
                                      <> ". " <> forHumanOrMachine ev

instance MetaTrace tr => MetaTrace (Mux.WithBearer peer tr) where
    namespaceFor (Mux.WithBearer _peer obj) = (nsCast . namespaceFor) obj
    severityFor ns Nothing = severityFor (nsCast ns :: Namespace tr) Nothing
    severityFor ns (Just (Mux.WithBearer _peer obj)) =
      severityFor (nsCast ns) (Just obj)
    privacyFor ns Nothing = privacyFor (nsCast ns :: Namespace tr) Nothing
    privacyFor ns (Just (Mux.WithBearer _peer obj)) =
      privacyFor (nsCast ns) (Just obj)
    detailsFor ns Nothing = detailsFor (nsCast ns :: Namespace tr) Nothing
    detailsFor ns (Just (Mux.WithBearer _peer obj)) =
      detailsFor (nsCast ns) (Just obj)
    documentFor ns = documentFor (nsCast ns :: Namespace tr)
    metricsDocFor ns = metricsDocFor (nsCast ns :: Namespace tr)
    allNamespaces = map nsCast (allNamespaces :: [Namespace tr])

instance LogFormatting Mux.BearerTrace where
    forMachine _dtal Mux.TraceRecvHeaderStart = mconcat
      [ "msg"  .= String "Bearer Receive Header Start"
      -- "kind" .= String "Mux.TraceRecvHeaderStart"
      ]
    forMachine _dtal (Mux.TraceRecvHeaderEnd SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = mconcat
      [ "msg"  .=  String "Bearer Receive Header End"
      , "timestamp" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "miniProtocolNum" .= String (showT mhNum)
      , "miniProtocolDir" .= String (showT mhDir)
      , "length" .= String (showT mhLength)
      -- "kind" .= String "Mux.TraceRecvHeaderStart"
      ]
    forMachine _dtal (Mux.TraceRecvDeltaQObservation SDUHeader { mhTimestamp, mhLength } ts) = mconcat
      [ "msg"  .=  String "Bearer DeltaQ observation"
      , "timeRemote" .=  String (showT ts)
      , "timeLocal" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "length" .= String (showT mhLength)
      -- "kind" .= String "Mux.TraceRecvDeltaQObservation"
      ]
    forMachine _dtal (Mux.TraceRecvDeltaQSample d sp so dqs dqvm dqvs estR sdud) = mconcat
      [ "msg"  .=  String "Bearer DeltaQ Sample"
      , "duration" .=  String (showT d)
      , "packets" .= String (showT sp)
      , "sumBytes" .= String (showT so)
      , "DeltaQ_S" .= String (showT dqs)
      , "DeltaQ_VMean" .= String (showT dqvm)
      , "DeltaQ_VVar" .= String (showT dqvs)
      , "DeltaQ_estR" .= String (showT estR)
      , "sizeDist" .= String (showT sdud)
      -- "kind" .= String "Mux.TraceRecvDeltaQSample"
      ]
    forMachine _dtal (Mux.TraceRecvStart len) = mconcat
      [ "msg"  .= String "Bearer Receive Start"
      , "length" .= String (showT len)
      -- "kind" .= String "Mux.TraceRecvStart"
      ]
    forMachine _dtal (Mux.TraceRecvRaw len) = mconcat
      [ "msg"  .= String "Bearer Receive Raw"
      , "length" .= String (showT len)
      -- "kind" .= String "Mux.TraceRecvRaw"
      ]
    forMachine _dtal (Mux.TraceRecvEnd len) = mconcat
      [ "msg"  .= String "Bearer Receive End"
      , "length" .= String (showT len)
      -- "kind" .= String "Mux.TraceRecvEnd"
      ]
    forMachine _dtal (Mux.TraceSendStart SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = mconcat
      [ "msg"  .= String "Bearer Send Start"
      , "timestamp" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "miniProtocolNum" .= String (showT mhNum)
      , "miniProtocolDir" .= String (showT mhDir)
      , "length" .= String (showT mhLength)
      -- "kind" .= String "Mux.TraceSendStart"
      ]
    forMachine _dtal Mux.TraceSendEnd = mconcat
      [ "msg"  .= String "Bearer Send End"
      -- "kind" .= String "Mux.TraceSendEnd"
      ]
    forMachine _dtal Mux.TraceSDUReadTimeoutException = mconcat
      [ "msg"  .= String "Timed out reading SDU"
      -- "kind" .= String "Mux.TraceSDUReadTimeoutException"
      ]
    forMachine _dtal Mux.TraceSDUWriteTimeoutException = mconcat
      [ "msg"  .= String "Timed out writing SDU"
      -- "kind" .= String "Mux.TraceSDUWriteTimeoutException"
      ]
    forMachine _dtal Mux.TraceEmitDeltaQ = mempty
#ifdef linux_HOST_OS
    forMachine _dtal (Mux.TraceTCPInfo StructTCPInfo
            { tcpi_snd_mss, tcpi_rcv_mss, tcpi_lost, tcpi_retrans
            , tcpi_rtt, tcpi_rttvar, tcpi_snd_cwnd }
            len) = mconcat
      [ "msg"  .= String "TCPInfo"
      , "rtt"  .= (fromIntegral tcpi_rtt :: Word)
      , "rttvar" .= (fromIntegral tcpi_rttvar :: Word)
      , "snd_cwnd" .= (fromIntegral tcpi_snd_cwnd :: Word)
      , "snd_mss" .= (fromIntegral tcpi_snd_mss :: Word)
      , "rcv_mss" .= (fromIntegral tcpi_rcv_mss :: Word)
      , "lost" .= (fromIntegral tcpi_lost :: Word)
      , "retrans" .= (fromIntegral tcpi_retrans :: Word)
      , "length" .= len
      -- "kind" .= String "Mux.TraceTCPInfo"
      ]
#else
    forMachine _dtal (Mux.TraceTCPInfo _ len) = mconcat
      [ "msg"  .= String "TCPInfo"
      , "len"  .= String (showT len)
      -- "kind" .= String "Mux.TraceTCPInfo"
      ]
#endif

    forHuman Mux.TraceRecvHeaderStart =
      "Bearer Receive Header Start"
    forHuman (Mux.TraceRecvHeaderEnd SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) =
      sformat ("Bearer Receive Header End: ts:" % prefixHex % "(" % shown % ") " % shown % " len " % int)
        (unRemoteClockModel mhTimestamp) mhNum mhDir mhLength
    forHuman (Mux.TraceRecvDeltaQObservation SDUHeader { mhTimestamp, mhLength } ts) =
      sformat ("Bearer DeltaQ observation: remote ts" % int % " local ts " % shown % " length " % int)
         (unRemoteClockModel mhTimestamp) ts mhLength
    forHuman (Mux.TraceRecvDeltaQSample d sp so dqs dqvm dqvs estR sdud) =
      sformat ("Bearer DeltaQ Sample: duration " % fixed 3 % " packets " % int % " sumBytes "
        % int % " DeltaQ_S " % fixed 3 % " DeltaQ_VMean " % fixed 3 % "DeltaQ_VVar " % fixed 3
        % " DeltaQ_estR " % fixed 3 % " sizeDist " % string)
        d sp so dqs dqvm dqvs estR sdud
    forHuman (Mux.TraceRecvStart len) =
      sformat ("Bearer Receive Start: length " % int) len
    forHuman (Mux.TraceRecvRaw len) =
      sformat ("Bearer Receive Raw: length " % int) len
    forHuman (Mux.TraceRecvEnd len) =
      sformat ("Bearer Receive End: length " % int) len
    forHuman (Mux.TraceSendStart SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) =
      sformat ("Bearer Send Start: ts: " % prefixHex % " (" % shown % ") " % shown % " length " % int)
        (unRemoteClockModel mhTimestamp) mhNum mhDir mhLength
    forHuman Mux.TraceSendEnd =
      "Bearer Send End"
    forHuman Mux.TraceSDUReadTimeoutException =
      "Timed out reading SDU"
    forHuman Mux.TraceSDUWriteTimeoutException =
      "Timed out writing SDU"
    forHuman Mux.TraceEmitDeltaQ = mempty
#ifdef linux_HOST_OS
    forHuman (Mux.TraceTCPInfo StructTCPInfo
            { tcpi_snd_mss, tcpi_rcv_mss, tcpi_lost, tcpi_retrans
            , tcpi_rtt, tcpi_rttvar, tcpi_snd_cwnd }
            len) =
      sformat ("TCPInfo rtt " % int % " rttvar " % int % " snd_cwnd " % int %
               " snd_mss " % int % " rcv_mss " % int % " lost " % int %
               " retrans " % int % " len " % int)
              (fromIntegral tcpi_rtt :: Word)
              (fromIntegral tcpi_rttvar :: Word)
              (fromIntegral tcpi_snd_cwnd :: Word)
              (fromIntegral tcpi_snd_mss :: Word)
              (fromIntegral tcpi_rcv_mss :: Word)
              (fromIntegral tcpi_lost :: Word)
              (fromIntegral tcpi_retrans :: Word)
              len
#else
    forHuman (Mux.TraceTCPInfo _ len) = sformat ("TCPInfo len " % int) len
#endif

instance MetaTrace Mux.BearerTrace where
    namespaceFor Mux.TraceRecvHeaderStart {}       =
      Namespace [] ["RecvHeaderStart"]
    namespaceFor Mux.TraceRecvHeaderEnd {}         =
      Namespace [] ["RecvHeaderEnd"]
    namespaceFor Mux.TraceRecvStart {}             =
      Namespace [] ["RecvStart"]
    namespaceFor Mux.TraceRecvRaw {}               =
      Namespace [] ["RecvRaw"]
    namespaceFor Mux.TraceRecvEnd {}               =
      Namespace [] ["RecvEnd"]
    namespaceFor Mux.TraceSendStart {}             =
      Namespace [] ["SendStart"]
    namespaceFor Mux.TraceSendEnd                  =
      Namespace [] ["SendEnd"]
    namespaceFor Mux.TraceRecvDeltaQObservation {} =
      Namespace [] ["RecvDeltaQObservation"]
    namespaceFor Mux.TraceRecvDeltaQSample {}      =
      Namespace [] ["RecvDeltaQSample"]
    namespaceFor Mux.TraceSDUReadTimeoutException  =
      Namespace [] ["SDUReadTimeoutException"]
    namespaceFor Mux.TraceSDUWriteTimeoutException =
      Namespace [] ["SDUWriteTimeoutException"]
    namespaceFor Mux.TraceEmitDeltaQ               =
      Namespace [] ["TraceEmitDeltaQ"]
    namespaceFor Mux.TraceTCPInfo {}               =
      Namespace [] ["TCPInfo"]

    severityFor (Namespace _ ["RecvHeaderStart"]) _       = Just Debug
    severityFor (Namespace _ ["RecvRaw"]) _               = Just Debug
    severityFor (Namespace _ ["RecvHeaderEnd"]) _         = Just Debug
    severityFor (Namespace _ ["RecvStart"]) _             = Just Debug
    severityFor (Namespace _ ["RecvEnd"]) _               = Just Debug
    severityFor (Namespace _ ["SendStart"]) _             = Just Debug
    severityFor (Namespace _ ["SendEnd"]) _               = Just Debug
    severityFor (Namespace _ ["RecvDeltaQObservation"]) _ = Just Debug
    severityFor (Namespace _ ["RecvDeltaQSample"]) _      = Just Debug
    severityFor (Namespace _ ["SDUReadTimeoutException"]) _  = Just Notice
    severityFor (Namespace _ ["SDUWriteTimeoutException"]) _ = Just Notice
    severityFor (Namespace _ ["TCPInfo"]) _               = Just Debug
    severityFor (Namespace _ ["TraceEmitDeltaQ"]) _       = Nothing
    severityFor _ _                                       = Nothing

    documentFor (Namespace _ ["RecvHeaderStart"])       = Just
      "Bearer receive header start."
    documentFor (Namespace _ ["RecvRaw"])               = Just
      "Bearer receive raw."
    documentFor (Namespace _ ["RecvHeaderEnd"])         = Just
      "Bearer receive header end."
    documentFor (Namespace _ ["RecvStart"])             = Just
      "Bearer receive start."
    documentFor (Namespace _ ["RecvEnd"])               = Just
      "Bearer receive end."
    documentFor (Namespace _ ["SendStart"])             = Just
      "Bearer send start."
    documentFor (Namespace _ ["SendEnd"])               = Just
      "Bearer send end."
    documentFor (Namespace _ ["RecvDeltaQObservation"]) = Just
      "Bearer DeltaQ observation."
    documentFor (Namespace _ ["RecvDeltaQSample"])      = Just
      "Bearer DeltaQ sample."
    documentFor (Namespace _ ["SDUReadTimeoutException"])  = Just
      "Timed out reading SDU."
    documentFor (Namespace _ ["SDUWriteTimeoutException"]) = Just
      "Timed out writing SDU."
    documentFor (Namespace _ ["TraceEmitDeltaQ"])       = Nothing
    documentFor (Namespace _ ["TCPInfo"])               = Just
      "TCPInfo."
    documentFor _                                       = Nothing

    allNamespaces = [
        Namespace [] ["RecvHeaderStart"]
      , Namespace [] ["RecvRaw"]
      , Namespace [] ["RecvHeaderEnd"]
      , Namespace [] ["RecvStart"]
      , Namespace [] ["RecvEnd"]
      , Namespace [] ["SendStart"]
      , Namespace [] ["SendEnd"]
      , Namespace [] ["RecvDeltaQObservation"]
      , Namespace [] ["RecvDeltaQSample"]
      , Namespace [] ["SDUReadTimeoutException"]
      , Namespace [] ["SDUWriteTimeoutException"]
      , Namespace [] ["TraceEmitDeltaQ"]
      , Namespace [] ["TCPInfo"]
      ]

instance LogFormatting Mux.ChannelTrace where
    forMachine _dtal (Mux.TraceChannelRecvStart mid) = mconcat
      [ "msg"  .= String "Channel Receive Start"
      , "miniProtocolNum" .= String (showT mid)
      -- "kind" .= String "Mux.TraceChannelRecvStart"
      ]
    forMachine _dtal (Mux.TraceChannelRecvEnd mid len) = mconcat
      [ "msg"  .= String "Channel Receive End"
      , "miniProtocolNum" .= String (showT mid)
      , "length" .= String (showT len)
      -- "kind" .= String "Mux.TraceChannelRecvEnd"
      ]
    forMachine _dtal (Mux.TraceChannelSendStart mid len) = mconcat
      [ "msg"  .= String "Channel Send Start"
      , "miniProtocolNum" .= String (showT mid)
      , "length" .= String (showT len)
      -- "kind" .= String "Mux.TraceChannelSendStart"
      ]
    forMachine _dtal (Mux.TraceChannelSendEnd mid) = mconcat
      [ "msg"  .= String "Channel Send End"
      , "miniProtocolNum" .= String (showT mid)
      -- "kind" .= String "Mux.TraceChannelSendEnd"
      ]

    forHuman (Mux.TraceChannelRecvStart mid) =
      sformat ("Channel Receive Start on " % shown) mid
    forHuman (Mux.TraceChannelRecvEnd mid len) =
      sformat ("Channel Receive End on (" % shown % ") " % int) mid len
    forHuman (Mux.TraceChannelSendStart mid len) =
      sformat ("Channel Send Start on (" % shown % ") " % int) mid len
    forHuman (Mux.TraceChannelSendEnd mid) =
      sformat ("Channel Send End on " % shown) mid

instance MetaTrace Mux.ChannelTrace where
    namespaceFor Mux.TraceChannelRecvStart {}      =
      Namespace [] ["ChannelRecvStart"]
    namespaceFor Mux.TraceChannelRecvEnd {}        =
      Namespace [] ["ChannelRecvEnd"]
    namespaceFor Mux.TraceChannelSendStart {}      =
      Namespace [] ["ChannelSendStart"]
    namespaceFor Mux.TraceChannelSendEnd {}        =
      Namespace [] ["ChannelSendEnd"]

    severityFor (Namespace _ ["ChannelRecvStart"]) _      = Just Debug
    severityFor (Namespace _ ["ChannelRecvEnd"]) _        = Just Debug
    severityFor (Namespace _ ["ChannelSendStart"]) _      = Just Debug
    severityFor (Namespace _ ["ChannelSendEnd"]) _        = Just Debug
    severityFor _ _                                       = Nothing

    documentFor (Namespace _ ["ChannelRecvStart"])      = Just
      "Channel receive start."
    documentFor (Namespace _ ["ChannelRecvEnd"])        = Just
      "Channel receive end."
    documentFor (Namespace _ ["ChannelSendStart"])      = Just
      "Channel send start."
    documentFor (Namespace _ ["ChannelSendEnd"])        = Just
      "Channel send end."
    documentFor _                                       = Nothing

    allNamespaces = [
        Namespace [] ["ChannelRecvStart"]
      , Namespace [] ["ChannelRecvEnd"]
      , Namespace [] ["ChannelSendStart"]
      , Namespace [] ["ChannelSendEnd"]
      ]

instance LogFormatting Mux.Trace where
    forMachine _dtal (Mux.TraceState new) = mconcat
      [ "msg"  .= String "MuxState"
      , "state" .= String (showT new)
      -- "kind" .= String "Mux.TraceState"
      ]
    forMachine _dtal (Mux.TraceCleanExit mid dir) = mconcat
      [ "msg"  .= String "Miniprotocol terminated cleanly"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      -- "kind" .= String "Mux.TraceCleanExit"
      ]
    forMachine _dtal (Mux.TraceExceptionExit mid dir exc) = mconcat
      [ "msg"  .= String "Miniprotocol terminated with exception"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      , "exception" .= String (showT exc)
      -- "kind" .= String "Mux.TraceExceptionExit"
      ]
    forMachine _dtal (Mux.TraceStartEagerly mid dir) = mconcat
      [ "msg"  .= String "Eagerly started"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      -- "kind" .= String "Mux.TraceStartEagerly"
      ]
    forMachine _dtal (Mux.TraceStartOnDemand mid dir) = mconcat
      [ "msg"  .= String "Preparing to start"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      -- "kind" .= String "Mux.TraceStartOnDemand"
      ]
    forMachine _dtal (Mux.TraceStartOnDemandAny mid dir) = mconcat
      [ "msg"  .= String "Preparing to start"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      -- "kind" .= String "Mux.TraceStartOnDemandAny"
      ]
    forMachine _dtal (Mux.TraceStartedOnDemand mid dir) = mconcat
      [ "msg"  .= String "Started on demand"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      -- "kind" .= String "Mux.TraceStartedOnDemand"
      ]
    forMachine _dtal (Mux.TraceTerminating mid dir) = mconcat
      [ "msg"  .= String "Terminating"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      -- "kind" .= String "Mux.TraceTerminating"
      ]
    forMachine _dtal Mux.TraceStopping = mconcat
      [ "msg"  .= String "Mux stopping"
      -- "kind" .= String "Mux.TraceStopping"
      ]
    forMachine _dtal Mux.TraceStopped = mconcat
      [ "msg"  .= String "Mux stoppped"
      -- "kind" .= String "Mux.TraceStopped"
      ]

    forHuman (Mux.TraceState new) =
      sformat ("State: " % shown) new
    forHuman (Mux.TraceCleanExit mid dir) =
      sformat ("Miniprotocol (" % shown % ") " % shown % " terminated cleanly")
      mid dir
    forHuman (Mux.TraceExceptionExit mid dir e) =
      sformat ("Miniprotocol (" % shown % ") " % shown %
        " terminated with exception " % shown) mid dir e
    forHuman (Mux.TraceStartEagerly mid dir) =
      sformat ("Eagerly started (" % shown % ") in " % shown) mid dir
    forHuman (Mux.TraceStartOnDemand mid dir) =
      sformat ("Preparing to start (" % shown % ") in " % shown) mid dir
    forHuman (Mux.TraceStartOnDemandAny mid dir) =
      sformat ("Preparing to start (" % shown % ") in " % shown) mid dir
    forHuman (Mux.TraceStartedOnDemand mid dir) =
      sformat ("Started on demand (" % shown % ") in " % shown) mid dir
    forHuman (Mux.TraceTerminating mid dir) =
      sformat ("Terminating (" % shown % ") in " % shown) mid dir
    forHuman Mux.TraceStopping = "Mux stopping"
    forHuman Mux.TraceStopped  = "Mux stoppped"

instance MetaTrace Mux.Trace where
    namespaceFor Mux.TraceState {}                 =
      Namespace [] ["State"]
    namespaceFor Mux.TraceCleanExit {}             =
      Namespace [] ["CleanExit"]
    namespaceFor Mux.TraceExceptionExit {}         =
      Namespace [] ["ExceptionExit"]
    namespaceFor Mux.TraceStartEagerly {}          =
      Namespace [] ["StartEagerly"]
    namespaceFor Mux.TraceStartOnDemand {}         =
      Namespace [] ["StartOnDemand"]
    namespaceFor Mux.TraceStartOnDemandAny {}      =
      Namespace [] ["StartOnDemandAny"]
    namespaceFor Mux.TraceStartedOnDemand {}       =
      Namespace [] ["StartedOnDemand"]
    namespaceFor Mux.TraceTerminating {}           =
      Namespace [] ["Terminating"]
    namespaceFor Mux.TraceStopping                 =
      Namespace [] ["Stopping"]
    namespaceFor Mux.TraceStopped                  =
      Namespace [] ["Stopped"]

    severityFor (Namespace _ ["State"]) _                 = Just Info
    severityFor (Namespace _ ["CleanExit"]) _             = Just Notice
    severityFor (Namespace _ ["ExceptionExit"]) _         = Just Notice
    severityFor (Namespace _ ["StartEagerly"]) _          = Just Debug
    severityFor (Namespace _ ["StartOnDemand"]) _         = Just Debug
    severityFor (Namespace _ ["StartOnDemandAny"]) _       = Just Debug
    severityFor (Namespace _ ["StartedOnDemand"]) _       = Just Debug
    severityFor (Namespace _ ["Terminating"]) _           = Just Debug
    severityFor (Namespace _ ["Stopping"]) _              = Just Debug
    severityFor (Namespace _ ["Stopped"]) _               = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["State"])                 = Just
      "State."
    documentFor (Namespace _ ["CleanExit"])             = Just
      "Miniprotocol terminated cleanly."
    documentFor (Namespace _ ["ExceptionExit"])         = Just
      "Miniprotocol terminated with exception."
    documentFor (Namespace _ ["StartEagerly"])          = Just
      "Eagerly started."
    documentFor (Namespace _ ["StartOnDemand"])         = Just
      "Preparing to start."
    documentFor (Namespace _ ["StartedOnDemand"])       = Just
      "Started on demand."
    documentFor (Namespace _ ["StartOnDemandAny"])      = Just
      "Start whenever any other protocol has started."
    documentFor (Namespace _ ["Terminating"])           = Just
      "Terminating."
    documentFor (Namespace _ ["Stopping"])              = Just
      "Mux shutdown."
    documentFor (Namespace _ ["Stopped"])              = Just
      "Mux shutdown."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["State"]
      , Namespace [] ["CleanExit"]
      , Namespace [] ["ExceptionExit"]
      , Namespace [] ["StartEagerly"]
      , Namespace [] ["StartOnDemand"]
      , Namespace [] ["StartOnDemandAny"]
      , Namespace [] ["StartedOnDemand"]
      , Namespace [] ["Terminating"]
      , Namespace [] ["Stopping"]
      , Namespace [] ["Stopped"]
      ]


--------------------------------------------------------------------------------
-- Handshake Tracer
--------------------------------------------------------------------------------

instance (Show term, Show ntcVersion) =>
  LogFormatting (AnyMessage (HS.Handshake ntcVersion term)) where
  forMachine _dtal (AnyMessageAndAgency stok msg) =
    mconcat [ "msg" .= (String . showT $ msg)
            , "agency" .= String (pack $ show stok)
            -- "kind" .= String kind
            ]
{--
    where
      kind = case msg of
        HS.MsgProposeVersions {} -> "ProposeVersions"
        HS.MsgReplyVersions   {} -> "ReplyVersions"
        HS.MsgQueryReply      {} -> "QueryReply"
        HS.MsgAcceptVersion   {} -> "AcceptVersion"
        HS.MsgRefuse          {} -> "Refuse"
--}

  forHuman (AnyMessageAndAgency stok msg) =
    "Handshake (agency, message) = " <> "(" <> showT stok <> "," <> forHumanOrMachine (AnyMessage msg) <> ")"

instance MetaTrace (AnyMessage (HS.Handshake a b)) where
  namespaceFor (AnyMessage msg) = Namespace [] $ case msg of
    HS.MsgProposeVersions {} -> ["ProposeVersions"]
    HS.MsgReplyVersions   {} -> ["ReplyVersions"]
    HS.MsgQueryReply      {} -> ["QueryReply"]
    HS.MsgAcceptVersion   {} -> ["AcceptVersion"]
    HS.MsgRefuse          {} -> ["Refuse"]

  severityFor (Namespace _ [sym]) _ = case sym of
    "ProposeVersions" -> Just Info
    "ReplyVersions"   -> Just Info
    "QueryReply"      -> Just Info
    "AcceptVersion"   -> Just Info
    "Refuse"          -> Just Info
    _otherwise        -> Nothing
  severityFor _ _ = Nothing

  documentFor (Namespace _ sym) = wrap . mconcat $ case sym of
    ["ProposeVersions"] ->
      [ "Propose versions together with version parameters.  It must be"
      , " encoded to a sorted list.."
      ]
    ["ReplyVersions"]   ->
      [ "`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It"
      , " is not supported to explicitly send this message. It can only be"
      , " received as a copy of 'MsgProposeVersions' in a simultaneous open"
      , " scenario."
      ]
    ["QueryReply"]      ->
      [ "`MsgQueryReply` received as a response to a handshake query in "
      , " 'MsgProposeVersions' and lists the supported versions."
      ]
    ["AcceptVersion"]   ->
      [ "The remote end decides which version to use and sends chosen version."
      , "The server is allowed to modify version parameters."
      ]
    ["Refuse"]          -> ["It refuses to run any version."]
    _otherwise          -> [] :: [Text]
    where
      wrap it = case it of
        ""  -> Nothing
        it' -> Just it'

  allNamespaces = [
      Namespace [] ["ProposeVersions"]
    , Namespace [] ["ReplyVersions"]
    , Namespace [] ["QueryReply"]
    , Namespace [] ["AcceptVersion"]
    , Namespace [] ["Refuse"]
    ]


--------------------------------------------------------------------------------
-- DiffusionInit Tracer
--------------------------------------------------------------------------------

instance (Show ntnAddr, Show ntcAddr) =>
  LogFormatting (Diff.DiffusionTracer ntnAddr ntcAddr) where
  forMachine _dtal (Diff.RunServer sockAddr) = mconcat
    [ "socketAddress" .= String (pack (show sockAddr))
    -- "kind" .= String "RunServer"
    ]

  forMachine _dtal (Diff.RunLocalServer localAddress) = mconcat
    [ "localAddress" .= String (pack (show localAddress))
    -- "kind" .= String "RunLocalServer"
    ]
  forMachine _dtal (Diff.UsingSystemdSocket localAddress) = mconcat
    [ "path" .= String (pack . show $ localAddress)
    -- "kind" .= String "UsingSystemdSocket"
    ]

  forMachine _dtal (Diff.CreateSystemdSocketForSnocketPath localAddress) = mconcat
    [ "path" .= String (pack . show $ localAddress)
    -- "kind" .= String "CreateSystemdSocketForSnocketPath"
    ]
  forMachine _dtal (Diff.CreatedLocalSocket localAddress) = mconcat
    [ "path" .= String (pack . show $ localAddress)
    -- "kind" .= String "CreatedLocalSocket"
    ]
  forMachine _dtal (Diff.ConfiguringLocalSocket localAddress socket) = mconcat
    [ "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    -- "kind" .= String "ConfiguringLocalSocket"
    ]
  forMachine _dtal (Diff.ListeningLocalSocket localAddress socket) = mconcat
    [ "path" .=  String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    -- "kind" .= String "ListeningLocalSocket"
    ]
  forMachine _dtal (Diff.LocalSocketUp localAddress fd) = mconcat
    [ "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show fd))
    -- "kind" .= String "LocalSocketUp"
    ]
  forMachine _dtal (Diff.CreatingServerSocket socket) = mconcat
    [ "socket" .= String (pack (show socket))
    -- "kind" .= String "CreatingServerSocket"
    ]
  forMachine _dtal (Diff.ListeningServerSocket socket) = mconcat
    [ "socket" .= String (pack (show socket))
    -- "kind" .= String "ListeningServerSocket"
    ]
  forMachine _dtal (Diff.ServerSocketUp socket) = mconcat
    [ "socket" .= String (pack (show socket))
    -- "kind" .= String "ServerSocketUp"
    ]
  forMachine _dtal (Diff.ConfiguringServerSocket socket) = mconcat
    [ "socket" .= String (pack (show socket))
    -- "kind" .= String "ConfiguringServerSocket"
    ]
  forMachine _dtal (Diff.UnsupportedLocalSystemdSocket path) = mconcat
    [ "path" .= String (pack (show path))
    -- "kind" .= String "UnsupportedLocalSystemdSocket"
    ]
  forMachine _dtal Diff.UnsupportedReadySocketCase = mconcat
    [ -- "kind" .= String "UnsupportedReadySocketCase"
    ]
  forMachine _dtal (Diff.DiffusionErrored exception) = mconcat
    [ "error" .= String (pack (show exception))
    -- "kind" .= String "DiffusionErrored"
    ]
  forMachine _dtal (Diff.SystemdSocketConfiguration config) = mconcat
    [ "path" .= String (pack (show config))
    -- "kind" .= String "SystemdSocketConfiguration"
    ]

instance MetaTrace (Diff.DiffusionTracer ntnAddr ntcAddr) where
    namespaceFor Diff.RunServer {} =
      Namespace [] ["RunServer"]
    namespaceFor Diff.RunLocalServer {} =
      Namespace [] ["RunLocalServer"]
    namespaceFor Diff.UsingSystemdSocket {} =
      Namespace [] ["UsingSystemdSocket"]
    namespaceFor Diff.CreateSystemdSocketForSnocketPath {} =
      Namespace [] ["CreateSystemdSocketForSnocketPath"]
    namespaceFor Diff.CreatedLocalSocket {} =
      Namespace [] ["CreatedLocalSocket"]
    namespaceFor Diff.ConfiguringLocalSocket {} =
      Namespace [] ["ConfiguringLocalSocket"]
    namespaceFor Diff.ListeningLocalSocket {} =
      Namespace [] ["ListeningLocalSocket"]
    namespaceFor Diff.LocalSocketUp {} =
      Namespace [] ["LocalSocketUp"]
    namespaceFor Diff.CreatingServerSocket {} =
      Namespace [] ["CreatingServerSocket"]
    namespaceFor Diff.ListeningServerSocket {} =
      Namespace [] ["ListeningServerSocket"]
    namespaceFor Diff.ServerSocketUp {} =
      Namespace [] ["ServerSocketUp"]
    namespaceFor Diff.ConfiguringServerSocket {} =
      Namespace [] ["ConfiguringServerSocket"]
    namespaceFor Diff.UnsupportedLocalSystemdSocket {} =
      Namespace [] ["UnsupportedLocalSystemdSocket"]
    namespaceFor Diff.UnsupportedReadySocketCase {} =
      Namespace [] ["UnsupportedReadySocketCase"]
    namespaceFor Diff.DiffusionErrored {} =
      Namespace [] ["DiffusionErrored"]
    namespaceFor Diff.SystemdSocketConfiguration {} =
      Namespace [] ["SystemdSocketConfiguration"]

    severityFor (Namespace _ ["RunServer"]) _ = Just Info
    severityFor (Namespace _ ["RunLocalServer"]) _ = Just Info
    severityFor (Namespace _ ["UsingSystemdSocket"]) _ = Just Info
    severityFor (Namespace _ ["CreateSystemdSocketForSnocketPath"]) _ = Just Info
    severityFor (Namespace _ ["CreatedLocalSocket"]) _ = Just Info
    severityFor (Namespace _ ["ConfiguringLocalSocket"]) _ = Just Info
    severityFor (Namespace _ ["ListeningLocalSocket"]) _ = Just Info
    severityFor (Namespace _ ["LocalSocketUp"]) _ = Just Info
    severityFor (Namespace _ ["CreatingServerSocket"]) _ = Just Info
    severityFor (Namespace _ ["ListeningServerSocket"]) _ = Just Info
    severityFor (Namespace _ ["ServerSocketUp"]) _ = Just Info
    severityFor (Namespace _ ["ConfiguringServerSocket"]) _ = Just Info
    severityFor (Namespace _ ["UnsupportedLocalSystemdSocket"]) _ = Just Warning
    severityFor (Namespace _ ["UnsupportedReadySocketCase"]) _ = Just Info
    severityFor (Namespace _ ["DiffusionErrored"]) _ = Just Critical
    severityFor (Namespace _ ["SystemdSocketConfiguration"]) _ = Just Warning
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["RunServer"]) = Just
      "RunServer"
    documentFor (Namespace _ ["RunLocalServer"]) = Just
      "RunLocalServer"
    documentFor (Namespace _ ["UsingSystemdSocket"]) = Just
      "UsingSystemdSocket"
    documentFor (Namespace _ ["CreateSystemdSocketForSnocketPath"]) = Just
      "CreateSystemdSocketForSnocketPath"
    documentFor (Namespace _ ["CreatedLocalSocket"]) = Just
      "CreatedLocalSocket"
    documentFor (Namespace _ ["ConfiguringLocalSocket"]) = Just
      "ConfiguringLocalSocket"
    documentFor (Namespace _ ["ListeningLocalSocket"]) = Just
      "ListeningLocalSocket"
    documentFor (Namespace _ ["LocalSocketUp"]) = Just
      "LocalSocketUp"
    documentFor (Namespace _ ["CreatingServerSocket"]) = Just
      "CreatingServerSocket"
    documentFor (Namespace _ ["ListeningServerSocket"]) = Just
      "ListeningServerSocket"
    documentFor (Namespace _ ["ServerSocketUp"]) = Just
      "ServerSocketUp"
    documentFor (Namespace _ ["ConfiguringServerSocket"]) = Just
      "ConfiguringServerSocket"
    documentFor (Namespace _ ["UnsupportedLocalSystemdSocket"]) = Just
      "UnsupportedLocalSystemdSocket"
    documentFor (Namespace _ ["UnsupportedReadySocketCase"]) = Just
      "UnsupportedReadySocketCase"
    documentFor (Namespace _ ["DiffusionErrored"]) = Just
      "DiffusionErrored"
    documentFor (Namespace _ ["SystemdSocketConfiguration"]) = Just
      "SystemdSocketConfiguration"
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["RunServer"]
      , Namespace [] ["RunLocalServer"]
      , Namespace [] ["UsingSystemdSocket"]
      , Namespace [] ["CreateSystemdSocketForSnocketPath"]
      , Namespace [] ["CreatedLocalSocket"]
      , Namespace [] ["ConfiguringLocalSocket"]
      , Namespace [] ["ListeningLocalSocket"]
      , Namespace [] ["LocalSocketUp"]
      , Namespace [] ["CreatingServerSocket"]
      , Namespace [] ["ListeningServerSocket"]
      , Namespace [] ["ServerSocketUp"]
      , Namespace [] ["ConfiguringServerSocket"]
      , Namespace [] ["UnsupportedLocalSystemdSocket"]
      , Namespace [] ["UnsupportedReadySocketCase"]
      , Namespace [] ["DiffusionErrored"]
      , Namespace [] ["SystemdSocketConfiguration"]
      ]

--------------------------------------------------------------------------------
-- LedgerPeers Tracer
--------------------------------------------------------------------------------

instance LogFormatting TraceLedgerPeers where
  forMachine _dtal (PickedLedgerPeer addr _ackStake stake) =
    mconcat
      [ "address" .= show addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      -- "kind" .= String "PickedLedgerPeer"
      ]
  forMachine _dtal (PickedLedgerPeers (NumberOfPeers n) addrs) =
    mconcat
      [ "desiredCount" .= n
      , "count" .= List.length addrs
      , "addresses" .= show addrs
      -- "kind" .= String "PickedLedgerPeers"
      ]
  forMachine _dtal (PickedBigLedgerPeer addr _ackStake stake) =
    mconcat
      [ "address" .= show addr
      , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
      -- "kind" .= String "PickedBigLedgerPeer"
      ]
  forMachine _dtal (PickedBigLedgerPeers (NumberOfPeers n) addrs) =
    mconcat
      [ "desiredCount" .= n
      , "count" .= List.length addrs
      , "addresses" .= show addrs
      -- "kind" .= String "PickedBigLedgerPeers"
      ]
  forMachine _dtal (FetchingNewLedgerState cnt bigCnt) =
    mconcat
      [ "numberOfLedgerPeers" .= cnt
      , "numberOfBigLedgerPeers" .= bigCnt
      -- "kind" .= String "FetchingNewLedgerState"
      ]
  forMachine _dtal DisabledLedgerPeers =
    mconcat
      [ -- "kind" .= String "DisabledLedgerPeers"
      ]
{--

src/DMQ/Tracer/Types.hs:814:26: error: [GHC-39999]
    • No instance for ‘aeson-2.2.3.0:Data.Aeson.Types.ToJSON.ToJSON
                         Ouroboros.Network.PeerSelection.LedgerPeers.Type.UseLedgerPeers’
        arising from a use of ‘.=’
    • In the expression: "useLedgerPeers" .= ulp
      In the first argument of ‘mconcat’, namely
        ‘["useLedgerPeers" .= ulp]’
      In the expression: mconcat ["useLedgerPeers" .= ulp]
    |
814 |       [ "useLedgerPeers" .= ulp


  forMachine _dtal (TraceUseLedgerPeers ulp) =
    mconcat
      [ "useLedgerPeers" .= ulp
      -- "kind" .= String "UseLedgerPeers"
      ]

--}
  forMachine _dtal (TraceUseLedgerPeers _ulp) =
    mconcat
      [ "useLedgerPeers" .= (0::Int)
      -- "kind" .= String "UseLedgerPeers"
      ]
  forMachine _dtal WaitingOnRequest =
    mconcat
      [ -- "kind" .= String "WaitingOnRequest"
      ]
  forMachine _dtal (RequestForPeers (NumberOfPeers np)) =
    mconcat
      [ "numberOfPeers" .= np
      -- "kind" .= String "RequestForPeers"
      ]
  forMachine _dtal (ReusingLedgerState cnt age) =
    mconcat
      [ "numberOfPools" .= cnt
      , "ledgerStateAge" .= age
      -- "kind" .= String "ReusingLedgerState"
      ]
  forMachine _dtal FallingBackToPublicRootPeers =
    mconcat
      [ --"kind" .= String "FallingBackToPublicRootPeers"
      ]
  forMachine _dtal (NotEnoughLedgerPeers (NumberOfPeers target) numOfLedgerPeers) =
    mconcat
      [ "target" .= target
      , "numOfLedgerPeers" .= numOfLedgerPeers
      -- "kind" .= String "NotEnoughLedgerPeers"
      ]
  forMachine _dtal (NotEnoughBigLedgerPeers (NumberOfPeers target) numOfBigLedgerPeers) =
    mconcat
      [ "target" .= target
      , "numOfBigLedgerPeers" .= numOfBigLedgerPeers
      -- "kind" .= String "NotEnoughBigLedgerPeers"
      ]
  forMachine _dtal (TraceLedgerPeersDomains daps) =
    mconcat
      [ "domainAccessPoints" .= daps
      -- "kind" .= String "TraceLedgerPeersDomains"
      ]
  forMachine _dtal UsingBigLedgerPeerSnapshot =
    mconcat
      [ --"kind" .= String "UsingBigLedgerPeerSnapshot"
      ]

instance MetaTrace TraceLedgerPeers where
    namespaceFor PickedLedgerPeer {} =
      Namespace [] ["PickedLedgerPeer"]
    namespaceFor PickedLedgerPeers {} =
      Namespace [] ["PickedLedgerPeers"]
    namespaceFor PickedBigLedgerPeer {} =
      Namespace [] ["PickedBigLedgerPeer"]
    namespaceFor PickedBigLedgerPeers {} =
      Namespace [] ["PickedBigLedgerPeers"]
    namespaceFor FetchingNewLedgerState {} =
      Namespace [] ["FetchingNewLedgerState"]
    namespaceFor DisabledLedgerPeers {} =
      Namespace [] ["DisabledLedgerPeers"]
    namespaceFor TraceUseLedgerPeers {} =
      Namespace [] ["TraceUseLedgerPeers"]
    namespaceFor WaitingOnRequest {} =
      Namespace [] ["WaitingOnRequest"]
    namespaceFor RequestForPeers {} =
      Namespace [] ["RequestForPeers"]
    namespaceFor ReusingLedgerState {} =
      Namespace [] ["ReusingLedgerState"]
    namespaceFor FallingBackToPublicRootPeers {} =
      Namespace [] ["FallingBackToPublicRootPeers"]
    namespaceFor NotEnoughLedgerPeers {} =
      Namespace [] ["NotEnoughLedgerPeers"]
    namespaceFor NotEnoughBigLedgerPeers {} =
      Namespace [] ["NotEnoughBigLedgerPeers"]
    namespaceFor TraceLedgerPeersDomains {} =
      Namespace [] ["TraceLedgerPeersDomains"]
    namespaceFor UsingBigLedgerPeerSnapshot {} =
      Namespace [] ["UsingBigLedgerPeerSnapshot"]

    severityFor (Namespace _ ["PickedPeer"]) _ = Just Debug
    severityFor (Namespace _ ["PickedPeers"]) _ = Just Info
    severityFor (Namespace _ ["FetchingNewLedgerState"]) _ = Just Info
    severityFor (Namespace _ ["DisabledLedgerPeers"]) _ = Just Info
    severityFor (Namespace _ ["TraceUseLedgerAfter"]) _ = Just Info
    severityFor (Namespace _ ["WaitingOnRequest"]) _ = Just Debug
    severityFor (Namespace _ ["RequestForPeers"]) _ = Just Debug
    severityFor (Namespace _ ["ReusingLedgerState"]) _ = Just Debug
    severityFor (Namespace _ ["FallingBackToPublicRootPeers"]) _ = Just Info
    severityFor (Namespace _ ["NotEnoughLedgerPeers"]) _ = Just Warning
    severityFor (Namespace _ ["NotEnoughBigLedgerPeers"]) _ = Just Warning
    severityFor (Namespace _ ["TraceLedgerPeersDomains"]) _ = Just Debug
    severityFor (Namespace _ ["TraceLedgerPeersResult"]) _ = Just Debug
    severityFor (Namespace _ ["TraceLedgerPeersFailure"]) _ = Just Debug
    severityFor (Namespace _ ["UsingBigLedgerPeerSnapshot"]) _ = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["PickedPeer"]) = Just
      "Trace for a peer picked with accumulated and relative stake of its pool."
    documentFor (Namespace _ ["PickedPeers"]) = Just
      "Trace for the number of peers we wanted to pick and the list of peers picked."
    documentFor (Namespace _ ["FetchingNewLedgerState"]) = Just $ mconcat
      [ "Trace for fetching a new list of peers from the ledger. Int is the number of peers"
      , " returned."
      ]
    documentFor (Namespace _ ["DisabledLedgerPeers"]) = Just
      "Trace for when getting peers from the ledger is disabled, that is DontUseLedger."
    documentFor (Namespace _ ["TraceUseLedgerAfter"]) = Just
      "Trace UseLedgerAfter value."
    documentFor (Namespace _ ["WaitingOnRequest"]) = Just
      ""
    documentFor (Namespace _ ["RequestForPeers"]) = Just
      "RequestForPeers (NumberOfPeers 1)"
    documentFor (Namespace _ ["ReusingLedgerState"]) = Just
      ""
    documentFor (Namespace _ ["FallingBackToPublicRootPeers"]) = Just
      ""
    documentFor (Namespace _ ["TraceLedgerPeersDomains"]) = Just
      ""
    documentFor (Namespace _ ["TraceLedgerPeersResult"]) = Just
      ""
    documentFor (Namespace _ ["TraceLedgerPeersFailure"]) = Just
      ""
    documentFor (Namespace _ ["UsingBigLedgerPeerSnapshot"]) = Just $ mconcat
      [ "Trace for when a request for big ledger peers is fulfilled from the snapshot file"
      , " defined in the topology configuration file."]
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["PickedPeer"]
      , Namespace [] ["PickedPeers"]
      , Namespace [] ["FetchingNewLedgerState"]
      , Namespace [] ["DisabledLedgerPeers"]
      , Namespace [] ["TraceUseLedgerAfter"]
      , Namespace [] ["WaitingOnRequest"]
      , Namespace [] ["RequestForPeers"]
      , Namespace [] ["ReusingLedgerState"]
      , Namespace [] ["FallingBackToPublicRootPeers"]
      , Namespace [] ["TraceLedgerPeersDomains"]
      , Namespace [] ["TraceLedgerPeersResult"]
      , Namespace [] ["TraceLedgerPeersFailure"]
      , Namespace [] ["UsingBigLedgerPeerSnapshot"]
      ]
