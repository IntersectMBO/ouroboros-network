module Network.NTP.Client.Trace where

import           Control.Exception
import           Network.Socket (SockAddr)
import           Network.NTP.Client.Packet (IPVersion, Microsecond, NtpOffset, ResultOrFailure)

data NtpTrace
    = NtpTraceStartNtpClient
    | NtpTraceTriggerUpdate
    | NtpTraceRestartDelay !Int
    | NtpTraceRestartingClient
    | NtpTraceClientSleeping
    | NtpTraceIOError !IOError
    | NtpTraceLookupsFails
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
    | NtpTracePacketSendError !SockAddr !IOError
    | NtpTracePacketDecodeError !IPVersion !String
    | NtpTracePacketReceived !IPVersion
    | NtpTraceWaitingForRepliesTimeout !IPVersion
    deriving (Show)
