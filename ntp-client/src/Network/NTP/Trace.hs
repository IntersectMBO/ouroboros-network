module Network.NTP.Trace
where
import           Network.NTP.Packet (Microsecond)

data IPVersion = IPv4 | IPv6
    deriving (Show)

data NtpTrace
    = NtpTraceStartNtpClient
    | NtpTraceTriggerUpdate
    | NtpTraceRestartDelay Int
    | NtpTraceRestartingClient
    | NtpTraceClientSleeping
    | NtpTraceIOError !IOError
    | NtpTraceLookupServerFailed !String
    | NtpTraceClientStartQuery
    | NtpTraceNoLocalAddr
    | NtpTraceIPv4IPv6BothFailed
    | NtpTraceReportPolicyQueryFailed
    | NtpTraceQueryResult !Microsecond
    | NtpTraceRunProtocolError !IPVersion !IOError
    | NtpTraceRunProtocolNoResult !IPVersion
    | NtpTraceRunProtocolSuccess !IPVersion
    | NtpTraceSocketOpen !IPVersion
    | NtpTraceSocketClosed !IPVersion
    | NtpTracePacketSent !IPVersion
    | NtpTracePacketSentError !IPVersion !IOError
    | NtpTracePacketDecodeError !IPVersion !String
    | NtpTracePacketReceived !IPVersion
    | NtpTraceWaitingForRepliesTimeout !IPVersion
    deriving (Show)
