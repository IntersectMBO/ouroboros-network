module Network.NTP.Trace
where
import           Control.Exception (IOException)
import           Network.NTP.Packet (Microsecond)

data IPVersion = IPv4 | IPv6
    deriving (Show)

data NtpTrace
    = NtpTraceIOError IOError
    | NtpTraceStartNtpClient
    | NtpTraceClientActNow
    | NtpTraceRestartDelay Int
    | NtpTraceRestartingClient
    | NtpTraceClientStartQuery
    | NtpTraceRunProtocolSuccess !IPVersion
    | NtpTraceRunProtocolNoResult !IPVersion
    | NtpTraceRunProtocolError !IPVersion IOError
    | NtpTraceIPv4IPv6BothFailed
    | NtpTraceUpdateStatusQueryFailed
    | NtpTraceUpdateStatusClockOffset Microsecond
    | NtpTraceSocketOpen
    | NtpTraceSocketClosed
    | NtpTracePacketSent
    | NtpTracePacketSentError IOException
    | NtpTraceClientWaitingForRepliesTimeout
    | NtpTraceReceiveLoopPacketReceived
    | NtpTraceClientSleeping

    




    | NtpTraceSocketReaderDecodeError String
    | NtpTraceSocketReaderIOException IOException
    | NtpTraceQueryLoopIOException IOException
    | NtpTraceOneshotClientIOError IOException

    | NtpTraceSocketCreated String String

    deriving (Show)
