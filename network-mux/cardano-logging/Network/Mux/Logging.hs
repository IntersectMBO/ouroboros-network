{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

-- Orphan instances module for Cardano tracer.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Extracted from "cardano-node" `Cardano.Node.Tracing.Tracers.Diffusion`.
-- Branch "ana/10.6-final-integration-mix"

--------------------------------------------------------------------------------

module Network.Mux.Logging () where

--------------------------------------------------------------------------------

---------
-- base -
---------
import Data.Typeable
---------------------
-- Package: "aeson" -
---------------------
import "aeson" Data.Aeson (Value (String), (.=))
--------------------------
-- Package: "formatting" -
--------------------------
import "formatting" Formatting
---------------------------
-- Package: "network-mux" -
---------------------------
import qualified "network-mux" -- "network-mux:network-mux"
  Network.Mux as Mux
#ifdef linux_HOST_OS
import           "network-mux" -- "network-mux:network-mux"
  Network.Mux.TCPInfo (StructTCPInfo (..))
#endif
import           "network-mux" -- "network-mux:network-mux"
  Network.Mux.Types
    ( SDUHeader (..), unRemoteClockModel
    )
--------------------------------
-- Package: "trace-dispatcher" -
--------------------------------
import "trace-dispatcher" Cardano.Logging

--------------------------------------------------------------------------------
-- Mux Tracer
--------------------------------------------------------------------------------

instance (LogFormatting peer, LogFormatting tr, Typeable tr) =>
    LogFormatting (Mux.WithBearer peer tr) where
    forMachine dtal (Mux.WithBearer b ev) =
      mconcat [ "kind"   .= (show . typeOf $ ev)
              , "bearer" .= forMachine dtal b
              , "event"  .= forMachine dtal ev ]
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
      [ "kind" .= String "Mux.TraceRecvHeaderStart"
      , "msg"  .= String "Bearer Receive Header Start"
      ]
    forMachine _dtal (Mux.TraceRecvHeaderEnd SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = mconcat
      [ "kind" .= String "Mux.TraceRecvHeaderStart"
      , "msg"  .=  String "Bearer Receive Header End"
      , "timestamp" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "miniProtocolNum" .= String (showT mhNum)
      , "miniProtocolDir" .= String (showT mhDir)
      , "length" .= String (showT mhLength)
      ]
    forMachine _dtal (Mux.TraceRecvDeltaQObservation SDUHeader { mhTimestamp, mhLength } ts) = mconcat
      [ "kind" .= String "Mux.TraceRecvDeltaQObservation"
      , "msg"  .=  String "Bearer DeltaQ observation"
      , "timeRemote" .=  String (showT ts)
      , "timeLocal" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "length" .= String (showT mhLength)
      ]
    forMachine _dtal (Mux.TraceRecvDeltaQSample d sp so dqs dqvm dqvs estR sdud) = mconcat
      [ "kind" .= String "Mux.TraceRecvDeltaQSample"
      , "msg"  .=  String "Bearer DeltaQ Sample"
      , "duration" .=  String (showT d)
      , "packets" .= String (showT sp)
      , "sumBytes" .= String (showT so)
      , "DeltaQ_S" .= String (showT dqs)
      , "DeltaQ_VMean" .= String (showT dqvm)
      , "DeltaQ_VVar" .= String (showT dqvs)
      , "DeltaQ_estR" .= String (showT estR)
      , "sizeDist" .= String (showT sdud)
      ]
    forMachine _dtal (Mux.TraceRecvStart len) = mconcat
      [ "kind" .= String "Mux.TraceRecvStart"
      , "msg"  .= String "Bearer Receive Start"
      , "length" .= String (showT len)
      ]
    forMachine _dtal (Mux.TraceRecvRaw len) = mconcat
      [ "kind" .= String "Mux.TraceRecvRaw"
      , "msg"  .= String "Bearer Receive Raw"
      , "length" .= String (showT len)
      ]
    forMachine _dtal (Mux.TraceRecvEnd len) = mconcat
      [ "kind" .= String "Mux.TraceRecvEnd"
      , "msg"  .= String "Bearer Receive End"
      , "length" .= String (showT len)
      ]
    forMachine _dtal (Mux.TraceSendStart SDUHeader { mhTimestamp, mhNum, mhDir, mhLength }) = mconcat
      [ "kind" .= String "Mux.TraceSendStart"
      , "msg"  .= String "Bearer Send Start"
      , "timestamp" .= String (showTHex (unRemoteClockModel mhTimestamp))
      , "miniProtocolNum" .= String (showT mhNum)
      , "miniProtocolDir" .= String (showT mhDir)
      , "length" .= String (showT mhLength)
      ]
    forMachine _dtal Mux.TraceSendEnd = mconcat
      [ "kind" .= String "Mux.TraceSendEnd"
      , "msg"  .= String "Bearer Send End"
      ]
    forMachine _dtal Mux.TraceSDUReadTimeoutException = mconcat
      [ "kind" .= String "Mux.TraceSDUReadTimeoutException"
      , "msg"  .= String "Timed out reading SDU"
      ]
    forMachine _dtal Mux.TraceSDUWriteTimeoutException = mconcat
      [ "kind" .= String "Mux.TraceSDUWriteTimeoutException"
      , "msg"  .= String "Timed out writing SDU"
      ]
    forMachine _dtal Mux.TraceEmitDeltaQ = mempty
#ifdef linux_HOST_OS
    forMachine _dtal (Mux.TraceTCPInfo StructTCPInfo
            { tcpi_snd_mss, tcpi_rcv_mss, tcpi_lost, tcpi_retrans
            , tcpi_rtt, tcpi_rttvar, tcpi_snd_cwnd }
            len) = mconcat
      [ "kind" .= String "Mux.TraceTCPInfo"
      , "msg"  .= String "TCPInfo"
      , "rtt"  .= (fromIntegral tcpi_rtt :: Word)
      , "rttvar" .= (fromIntegral tcpi_rttvar :: Word)
      , "snd_cwnd" .= (fromIntegral tcpi_snd_cwnd :: Word)
      , "snd_mss" .= (fromIntegral tcpi_snd_mss :: Word)
      , "rcv_mss" .= (fromIntegral tcpi_rcv_mss :: Word)
      , "lost" .= (fromIntegral tcpi_lost :: Word)
      , "retrans" .= (fromIntegral tcpi_retrans :: Word)
      , "length" .= len
      ]
#else
    forMachine _dtal (Mux.TraceTCPInfo _ len) = mconcat
      [ "kind" .= String "Mux.TraceTCPInfo"
      , "msg"  .= String "TCPInfo"
      , "len"  .= String (showT len)
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
      [ "kind" .= String "Mux.TraceChannelRecvStart"
      , "msg"  .= String "Channel Receive Start"
      , "miniProtocolNum" .= String (showT mid)
      ]
    forMachine _dtal (Mux.TraceChannelRecvEnd mid len) = mconcat
      [ "kind" .= String "Mux.TraceChannelRecvEnd"
      , "msg"  .= String "Channel Receive End"
      , "miniProtocolNum" .= String (showT mid)
      , "length" .= String (showT len)
      ]
    forMachine _dtal (Mux.TraceChannelSendStart mid len) = mconcat
      [ "kind" .= String "Mux.TraceChannelSendStart"
      , "msg"  .= String "Channel Send Start"
      , "miniProtocolNum" .= String (showT mid)
      , "length" .= String (showT len)
      ]
    forMachine _dtal (Mux.TraceChannelSendEnd mid) = mconcat
      [ "kind" .= String "Mux.TraceChannelSendEnd"
      , "msg"  .= String "Channel Send End"
      , "miniProtocolNum" .= String (showT mid)
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
      [ "kind" .= String "Mux.TraceState"
      , "msg"  .= String "MuxState"
      , "state" .= String (showT new)
      ]
    forMachine _dtal (Mux.TraceCleanExit mid dir) = mconcat
      [ "kind" .= String "Mux.TraceCleanExit"
      , "msg"  .= String "Miniprotocol terminated cleanly"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (Mux.TraceExceptionExit mid dir exc) = mconcat
      [ "kind" .= String "Mux.TraceExceptionExit"
      , "msg"  .= String "Miniprotocol terminated with exception"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      , "exception" .= String (showT exc)
      ]
    forMachine _dtal (Mux.TraceStartEagerly mid dir) = mconcat
      [ "kind" .= String "Mux.TraceStartEagerly"
      , "msg"  .= String "Eagerly started"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (Mux.TraceStartOnDemand mid dir) = mconcat
      [ "kind" .= String "Mux.TraceStartOnDemand"
      , "msg"  .= String "Preparing to start"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (Mux.TraceStartOnDemandAny mid dir) = mconcat
      [ "kind" .= String "Mux.TraceStartOnDemandAny"
      , "msg"  .= String "Preparing to start"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (Mux.TraceStartedOnDemand mid dir) = mconcat
      [ "kind" .= String "Mux.TraceStartedOnDemand"
      , "msg"  .= String "Started on demand"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal (Mux.TraceTerminating mid dir) = mconcat
      [ "kind" .= String "Mux.TraceTerminating"
      , "msg"  .= String "Terminating"
      , "miniProtocolNum" .= String (showT mid)
      , "miniProtocolDir" .= String (showT dir)
      ]
    forMachine _dtal Mux.TraceStopping = mconcat
      [ "kind" .= String "Mux.TraceStopping"
      , "msg"  .= String "Mux stopping"
      ]
    forMachine _dtal Mux.TraceStopped = mconcat
      [ "kind" .= String "Mux.TraceStopped"
      , "msg"  .= String "Mux stoppped"
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

