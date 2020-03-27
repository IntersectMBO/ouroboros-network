module Network.NTP.Client.Trace where

import           Control.Exception
import           Network.NTP.Client.Packet (IPVersion, Microsecond, NtpOffset, ResultOrFailure)

data NtpTrace
    = NtpTraceStartNtpClient
    | NtpTraceTriggerUpdate
    | NtpTraceRestartDelay !Int
    | NtpTraceRestartingClient
    | NtpTraceClientSleeping
    | NtpTraceIOError !IOError
    | NtpTraceLookupServerFailed !String
    | NtpTraceClientStartQuery
    | NtpTraceNoLocalAddr
    | NtpTraceIPv4IPv6NoReplies
    | NtpTraceReportPolicyQueryFailed
    | NtpTraceQueryResult !Microsecond
    | NtpTraceRunProtocolError !IPVersion !SomeException
    | NtpTraceRunProtocolResults !(ResultOrFailure  SomeException [NtpOffset])
    | NtpTraceSocketOpen !IPVersion
    | NtpTraceSocketClosed !IPVersion
    | NtpTracePacketSent !IPVersion
    | NtpTracePacketSentError !IPVersion !IOError
    | NtpTracePacketDecodeError !IPVersion !String
    | NtpTracePacketReceived !IPVersion
    | NtpTraceWaitingForRepliesTimeout !IPVersion
    deriving (Show)
