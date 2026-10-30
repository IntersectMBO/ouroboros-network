{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Network.Mux.Tracing () where

import Data.Aeson (Object, Value (String), object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable
import Data.Word (Word64)
import Formatting

import Cardano.Logging
import Network.Mux qualified as Mux
#ifdef linux_HOST_OS
import Network.Mux.TCPInfo (StructTCPInfo (..))
#endif
import Network.Mux.Types (MiniProtocolDir (..), SDUHeader (..),
           unRemoteClockModel)

--------------------------------------------------------------------------------
-- Mux Tracer
--------------------------------------------------------------------------------

instance (LogFormatting peer, LogFormatting tr, Typeable tr) =>
    LogFormatting (Mux.WithBearer peer tr) where
    forMachine dtal (Mux.WithBearer b ev) =
      mconcat [ "kind"   .= (show . typeOf $ ev)
              , "bearer" .= forMachine dtal b
              , "event"  .= forMachine dtal ev ]
    forHuman (Mux.WithBearer b ev) = "With mux bearer " <> forHuman b
                                      <> ". " <> forHuman ev

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

    severityFor (Namespace _ ["RecvHeaderStart"]) _          = Just Debug
    severityFor (Namespace _ ["RecvRaw"]) _                  = Just Debug
    severityFor (Namespace _ ["RecvHeaderEnd"]) _            = Just Debug
    severityFor (Namespace _ ["RecvStart"]) _                = Just Debug
    severityFor (Namespace _ ["RecvEnd"]) _                  = Just Debug
    severityFor (Namespace _ ["SendStart"]) _                = Just Debug
    severityFor (Namespace _ ["SendEnd"]) _                  = Just Debug
    severityFor (Namespace _ ["RecvDeltaQObservation"]) _    = Just Debug
    severityFor (Namespace _ ["RecvDeltaQSample"]) _         = Just Debug
    severityFor (Namespace _ ["SDUReadTimeoutException"]) _  = Just Notice
    severityFor (Namespace _ ["SDUWriteTimeoutException"]) _ = Just Notice
    severityFor (Namespace _ ["TCPInfo"]) _                  = Just Debug
    severityFor (Namespace _ ["TraceEmitDeltaQ"]) _          = Nothing
    severityFor _ _                                          = Nothing

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
    forMachine _dtal (Mux.TraceChannelSendStart mid) = mconcat
      [ "kind" .= String "Mux.TraceChannelSendStart"
      , "msg"  .= String "Channel Send Start"
      , "miniProtocolNum" .= String (showT mid)
      ]
    forMachine _dtal (Mux.TraceChannelSendEnd mid len) = mconcat
      [ "kind" .= String "Mux.TraceChannelSendEnd"
      , "msg"  .= String "Channel Send End"
      , "miniProtocolNum" .= String (showT mid)
      , "length" .= String (showT len)
      ]

    forHuman (Mux.TraceChannelRecvStart mid) =
      sformat ("Channel Receive Start on " % shown) mid
    forHuman (Mux.TraceChannelRecvEnd mid len) =
      sformat ("Channel Receive End on (" % shown % ") " % int) mid len
    forHuman (Mux.TraceChannelSendStart mid) =
      sformat ("Channel Send Start on " % shown) mid
    forHuman (Mux.TraceChannelSendEnd mid len) =
      sformat ("Channel Send End on (" % shown % ") " % int) mid len

instance MetaTrace Mux.ChannelTrace where
    namespaceFor Mux.TraceChannelRecvStart {}      =
      Namespace [] ["ChannelRecvStart"]
    namespaceFor Mux.TraceChannelRecvEnd {}        =
      Namespace [] ["ChannelRecvEnd"]
    namespaceFor Mux.TraceChannelSendStart {}      =
      Namespace [] ["ChannelSendStart"]
    namespaceFor Mux.TraceChannelSendEnd {}        =
      Namespace [] ["ChannelSendEnd"]

    severityFor (Namespace _ ["ChannelRecvStart"]) _ = Just Debug
    severityFor (Namespace _ ["ChannelRecvEnd"]) _   = Just Debug
    severityFor (Namespace _ ["ChannelSendStart"]) _ = Just Debug
    severityFor (Namespace _ ["ChannelSendEnd"]) _   = Just Debug
    severityFor _ _                                  = Nothing

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

txsMempoolTimeoutHardCounterName :: Text
txsMempoolTimeoutHardCounterName = "txsMempoolTimeoutHard"

impliesMempoolTimeoutHard :: Mux.Trace -> Bool
impliesMempoolTimeoutHard = \case
  Mux.TraceExceptionExit _mid _dir e
{-- TODO: In cardano-node master this is implemented as:
 --
 -- > | Just _ <- fromException @ExnMempoolTimeout e
 -- >   -> True
 --
 -- but `ExnMempoolTimeout` is defined in `ouroboros-consensus` which is not a
 -- dependency of `ouroboros-network`.
 --}
    | isPrefixOf "ExnMempoolTimeout " (show e) -> True
  _ -> False

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
    forMachine _dtal (Mux.TraceNewMux _) = mconcat
      [ "kind" .= String "Mux.TraceNewMux"
      , "msg"  .= String "New Mux"
      ]
    forMachine _dtal Mux.TraceStarting = mconcat
      [ "kind" .= String "Mux.TraceSarting"
      , "msg"  .= String "Mux starting"
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
    forHuman (Mux.TraceNewMux _) = "New Mux"
    forHuman Mux.TraceStarting = "Mux Starting"
    forHuman Mux.TraceStopping = "Mux stopping"
    forHuman Mux.TraceStopped = "Mux stoppped"

    asMetrics = \case
      Mux.TraceState{} -> []
      Mux.TraceCleanExit{} -> []
      ev@Mux.TraceExceptionExit{} ->
        -- Somewhat awkward to "catch" this Consensus exception here, but
        -- Diffusion Layer is indeed the ultimate manager of the per-peer
        -- threads.
        [ CounterM txsMempoolTimeoutHardCounterName Nothing
        | impliesMempoolTimeoutHard ev
        ]
      Mux.TraceStartEagerly{} -> []
      Mux.TraceStartOnDemand{} -> []
      Mux.TraceStartOnDemandAny{} -> []
      Mux.TraceStartedOnDemand{} -> []
      Mux.TraceTerminating{} -> []
      Mux.TraceNewMux{} -> []
      Mux.TraceStarting{} -> []
      Mux.TraceStopping{} -> []
      Mux.TraceStopped{} -> []

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
    namespaceFor (Mux.TraceNewMux _)               =
      Namespace [] ["NewMux"]
    namespaceFor Mux.TraceStarting                 =
      Namespace [] ["Starting"]
    namespaceFor Mux.TraceStopping                 =
      Namespace [] ["Stopping"]
    namespaceFor Mux.TraceStopped                  =
      Namespace [] ["Stopped"]

    severityFor (Namespace _ ["State"]) _            = Just Info
    severityFor (Namespace _ ["CleanExit"]) _        = Just Notice
    severityFor (Namespace _ ["ExceptionExit"]) _    = Just Notice
    severityFor (Namespace _ ["StartEagerly"]) _     = Just Debug
    severityFor (Namespace _ ["StartOnDemand"]) _    = Just Debug
    severityFor (Namespace _ ["StartOnDemandAny"]) _ = Just Debug
    severityFor (Namespace _ ["StartedOnDemand"]) _  = Just Debug
    severityFor (Namespace _ ["Terminating"]) _      = Just Debug
    severityFor (Namespace _ ["NewMux"]) _           = Just Debug
    severityFor (Namespace _ ["Starting"]) _         = Just Debug
    severityFor (Namespace _ ["Stopping"]) _         = Just Debug
    severityFor (Namespace _ ["Stopped"]) _          = Just Debug
    severityFor _ _                                  = Nothing

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
    documentFor (Namespace _ ["NewMux"])                = Just
      "New Mux"
    documentFor (Namespace _ ["Starting"])              = Just
      "Mux Starting"
    documentFor (Namespace _ ["Stopping"])              = Just
      "Mux shutdown."
    documentFor (Namespace _ ["Stopped"])               = Just
      "Mux shutdown."
    documentFor _ = Nothing

    metricsDocFor (Namespace _ ["State"])               = []
    metricsDocFor (Namespace _ ["CleanExit"])           = []
    metricsDocFor (Namespace _ ["ExceptionExit"])       =
      [ (txsMempoolTimeoutHardCounterName, "Transactions that hard timed out in mempool")
      ]
    metricsDocFor (Namespace _ ["StartEagerly"])        = []
    metricsDocFor (Namespace _ ["StartOnDemand"])       = []
    metricsDocFor (Namespace _ ["StartedOnDemand"])     = []
    metricsDocFor (Namespace _ ["StartOnDemandAny"])    = []
    metricsDocFor (Namespace _ ["Terminating"])         = []
    metricsDocFor (Namespace _ ["NewMux"])              = []
    metricsDocFor (Namespace _ ["Starting"])            = []
    metricsDocFor (Namespace _ ["Stopping"])            = []
    metricsDocFor (Namespace _ ["Stopped"])             = []
    metricsDocFor _                                     = []

    allNamespaces = [
        Namespace [] ["State"]
      , Namespace [] ["CleanExit"]
      , Namespace [] ["ExceptionExit"]
      , Namespace [] ["StartEagerly"]
      , Namespace [] ["StartOnDemand"]
      , Namespace [] ["StartOnDemandAny"]
      , Namespace [] ["StartedOnDemand"]
      , Namespace [] ["Terminating"]
      , Namespace [] ["NewMux"]
      , Namespace [] ["Starting"]
      , Namespace [] ["Stopping"]
      , Namespace [] ["Stopped"]
      ]

--------------------------------------------------------------------------------
-- Mux counters
--------------------------------------------------------------------------------

-- | Which muxes a counters snapshot is of.
data Side = Remote | Local

sideDoc :: Side -> Text
sideDoc Remote = "node-to-node"
sideDoc Local  = "node-to-client"

-- | A metric's name on a side: the node-to-node name as given, the
-- node-to-client one marked local.
onSide :: Side -> Text -> Text
onSide Remote name = name
onSide Local  name = "local" <> Text.toUpper (Text.take 1 name) <> Text.drop 1 name

-- | One failure count of the bearer sides: its node-to-node metric's name,
-- in the egress or the ingress family or by itself; its label; the count;
-- and what the metric documents.
data FailureCount = FailureCount {
    fcName  :: Text,
    fcLabel :: Text,
    fcCount :: Word64,
    fcDoc   :: Text
  }

-- | A failure count's JSON key: its name without the family.
fcKey :: FailureCount -> Key
fcKey = Key.fromText . Text.takeWhileEnd (/= '.') . fcName

-- | The failure counts of the bearer sides; the mini-protocols' are by
-- protocol, apart.
failureCounts :: Mux.MuxFailures -> [FailureCount]
failureCounts Mux.MuxFailures {
                Mux.mfWriteTimeouts, Mux.mfWriteErrors, Mux.mfReadTimeouts,
                Mux.mfOverruns, Mux.mfProtocolErrors, Mux.mfReadErrors,
                Mux.mfPeerClosed, Mux.mfOtherErrors } =
  [ FailureCount "egress.writeTimeouts" "write timeouts" mfWriteTimeouts
      "Muxes dropped because the peer took nothing within the SDU timeout."
  , FailureCount "egress.writeErrors" "write errors" mfWriteErrors
      "Muxes dropped because a write failed otherwise than by the peer closing."
  , FailureCount "ingress.readTimeouts" "read timeouts" mfReadTimeouts
      "Muxes dropped because an SDU did not arrive in full within the SDU timeout."
  , FailureCount "ingress.overruns" "overruns" mfOverruns
      "Muxes dropped because the peer exceeded a mini-protocol's ingress limit."
  , FailureCount "ingress.protocolErrors" "protocol errors" mfProtocolErrors
      ("Muxes dropped for undecodable SDUs, unknown mini-protocols, or data "
         <> "an initiator-only mux cannot take.")
  , FailureCount "ingress.readErrors" "read errors" mfReadErrors
      ("Muxes dropped because a read failed otherwise than by the peer closing: "
         <> "a connection that timed out, a host unreachable.")
  , FailureCount "peerClosed" "peers closed" mfPeerClosed
      ("Muxes dropped because the peer closed the connection: read as its end while "
         <> "a mini-protocol was running, or met as a reset or a broken pipe by a "
         <> "read or a write.")
  , FailureCount "otherErrors" "other errors" mfOtherErrors
      ("Muxes dropped because the muxer or the demuxer died with an exception of "
         <> "a kind not listed: an encoding that would not force, a bug.")
  ]

-- | Our side's direction of a mini-protocol, as metric names carry it.
dirName :: MiniProtocolDir -> Text
dirName InitiatorDir = "initiator"
dirName ResponderDir = "responder"

instance LogFormatting Mux.CountersTrace where
    forMachine _dtal = \case
      Mux.TraceRemoteCounters c -> countsObject "Mux.TraceRemoteCounters" c
      Mux.TraceLocalCounters  c -> countsObject "Mux.TraceLocalCounters" c
      where
        countsObject :: Text -> Mux.MuxCounts -> Object
        countsObject kind
                     Mux.MuxCounts { Mux.mcFailures = failures, Mux.mcSent, Mux.mcReceived } =
          mconcat $
               [ "kind" .= String kind ]
            ++ [ fcKey c .= fcCount c | c <- failureCounts failures ]
            ++ [ "miniProtocolFailures" .= sum protocols
               , "miniProtocolFailuresByProtocol" .= byProtocol "failures" protocols
               , "bytesSent" .= sum mcSent
               , "bytesSentByProtocol" .= byProtocol "bytes" mcSent
               , "bytesReceived" .= sum mcReceived
               , "bytesReceivedByProtocol" .= byProtocol "bytes" mcReceived
               ]
          where
            protocols :: Map (Mux.MiniProtocolNum, MiniProtocolDir) Word64
            protocols = Mux.mfMiniProtocols failures
        byProtocol :: Key -> Map (Mux.MiniProtocolNum, MiniProtocolDir) Word64 -> [Value]
        byProtocol what counts =
          [ object [ "protocol" .= n, "direction" .= String (showT d), what .= b ]
          | ((Mux.MiniProtocolNum n, d), b) <- Map.toList counts
          ]
    forHuman = \case
      Mux.TraceRemoteCounters c -> "Node-to-node mux counters: " <> countsLine c
      Mux.TraceLocalCounters  c -> "Node-to-client mux counters: " <> countsLine c
      where
        -- The byte totals, and the failure counts that are not zero.
        countsLine :: Mux.MuxCounts -> Text
        countsLine Mux.MuxCounts { Mux.mcFailures = failures, Mux.mcSent, Mux.mcReceived } =
          Text.intercalate ", " $
               [ showT (sum mcSent) <> " bytes sent"
               , showT (sum mcReceived) <> " bytes received" ]
            ++ [ showT (fcCount c) <> " " <> fcLabel c
               | c <- failureCounts failures
               , fcCount c /= 0
               ]
            ++ [ showT n <> " mini-protocol failures"
               | let n = sum (Mux.mfMiniProtocols failures)
               , n /= 0
               ]
    asMetrics = \case
      Mux.TraceRemoteCounters c -> countsMetrics Remote c
      Mux.TraceLocalCounters  c -> countsMetrics Local c
      where
        countsMetrics :: Side -> Mux.MuxCounts -> [Metric]
        countsMetrics side
                      Mux.MuxCounts { Mux.mcFailures = failures, Mux.mcSent, Mux.mcReceived } =
             [ IntM (onSide side (fcName c)) (fromIntegral (fcCount c))
             | c <- failureCounts failures
             ]
          ++ perProtocol (onSide side "miniProtocols.failures") (Mux.mfMiniProtocols failures)
          ++ perProtocol (onSide side "egress.bytes")           mcSent
          ++ perProtocol (onSide side "ingress.bytes")          mcReceived

        -- The total, and one counter per mini-protocol number and direction;
        -- the numbers are the application's, so the names carry them.
        perProtocol :: Text -> Map (Mux.MiniProtocolNum, MiniProtocolDir) Word64 -> [Metric]
        perProtocol name counts =
            IntM name (fromIntegral (sum counts))
          : [ IntM (name <> "." <> showT n <> "." <> dirName d) (fromIntegral b)
            | ((Mux.MiniProtocolNum n, d), b) <- Map.toList counts
            ]

instance MetaTrace Mux.CountersTrace where
    namespaceFor Mux.TraceRemoteCounters {} = Namespace [] ["Remote"]
    namespaceFor Mux.TraceLocalCounters {}  = Namespace [] ["Local"]

    severityFor (Namespace _ ["Remote"]) _ = Just Info
    severityFor (Namespace _ ["Local"])  _ = Just Info
    severityFor _ _                        = Nothing

    documentFor (Namespace _ ["Remote"]) = Just (countersDoc Remote)
    documentFor (Namespace _ ["Local"])  = Just (countersDoc Local)
    documentFor _                        = Nothing

    metricsDocFor (Namespace _ ["Remote"]) = metricsDoc Remote
    metricsDocFor (Namespace _ ["Local"])  = metricsDoc Local
    metricsDocFor _                        = []

    allNamespaces = [ Namespace [] ["Remote"], Namespace [] ["Local"] ]

-- | What a side's snapshots are.
countersDoc :: Side -> Text
countersDoc side =
  "Node-wide counters of the " <> sideDoc side <> " muxes, every counters "
    <> "interval: how they have failed, and the bytes they sent and read."

-- | The metrics of one side. The per-protocol ones, named by the
-- application's numbers, are described with their totals.
metricsDoc :: Side -> [(Text, Text)]
metricsDoc side =
     [ (onSide side (fcName c), fcDoc c)
     | c <- failureCounts Mux.noFailures
     ]
  ++ [ (onSide side "miniProtocols.failures",
        "Muxes dropped because a mini-protocol's thread died with an exception of its "
          <> "own; the same per mini-protocol and direction under "
          <> ".failures.<num>.<initiator|responder>.")
     , (onSide side "egress.bytes",
        "Bytes written, SDU headers included; the same per mini-protocol and "
          <> "direction under .bytes.<num>.<initiator|responder>, the numbers "
          <> "the application's.")
     , (onSide side "ingress.bytes",
        "Bytes read, SDU headers included; an SDU that overran its ingress limit is "
          <> "not counted; the same per mini-protocol and direction under "
          <> ".bytes.<num>.<initiator|responder>.")
     ]
