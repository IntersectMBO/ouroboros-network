{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Network.NTP.Util
    ( ntpPort
    , WithAddrFamily (..)
    , runWithAddrFamily
    , getAddrFamily
    , AddrFamily (..)
    , Addresses
    , Sockets
    , resolveNtpHost
    , sendPacket

    , createAndBindSock
    , udpLocalAddresses

    , pairThese
    ) where

import           Control.Exception (Exception, IOException, catch, throw)
import           Control.Monad (void)
import           Control.Tracer
import           Data.Bifunctor (Bifunctor (..))
import           Data.Binary (encode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (traverse_)
import           Data.List (find)
import           Data.Semigroup (First (..), Last (..), Option (..),
                     Semigroup (..))
import           Data.These (These (..))
import           Network.Socket (AddrInfo,
                     AddrInfoFlag (AI_ADDRCONFIG, AI_PASSIVE),
                     Family (AF_INET, AF_INET6), PortNumber, SockAddr (..),
                     Socket, SocketOption (ReuseAddr), SocketType (Datagram),
                     addrAddress, addrFamily, addrFlags, addrSocketType)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString (sendTo)

import           Network.NTP.Packet (NtpPacket)
import           Network.NTP.Trace (NtpTrace (..))

data AddrFamily = IPv4 | IPv6
    deriving Show

-- |
-- Newtype wrapper which tags the type with either IPv4 or IPv6 phantom type.
data WithAddrFamily (t :: AddrFamily) a where
    WithIPv6 :: a -> WithAddrFamily 'IPv6 a
    WithIPv4 :: a -> WithAddrFamily 'IPv4 a

instance Show a => Show (WithAddrFamily t a) where
    show a = show (getAddrFamily a) ++ " " ++ show (runWithAddrFamily a)

instance Eq a => Eq (WithAddrFamily t a) where
    a == b = runWithAddrFamily a == runWithAddrFamily b

instance Functor (WithAddrFamily t) where
    fmap f (WithIPv6 a) =  WithIPv6 (f a)
    fmap f (WithIPv4 a) =  WithIPv4 (f a)

runWithAddrFamily :: WithAddrFamily t a -> a
runWithAddrFamily (WithIPv6 a) = a
runWithAddrFamily (WithIPv4 a) = a

getAddrFamily :: WithAddrFamily t a -> AddrFamily
getAddrFamily (WithIPv6 _) = IPv6
getAddrFamily (WithIPv4 _) = IPv4

-- |
-- Note that the composition of `foldThese . bimap f g` is a proof that
-- @'These a b@ is the [free
-- product](https://en.wikipedia.org/wiki/Free_product) of two semigroups @a@
-- and @b@.
foldThese
    :: Semigroup a
    => These a a
    -> a
foldThese (This a)      = a
foldThese (That a)      = a
foldThese (These a1 a2) = a1 <> a2

pairThese
    :: These a b
    -> These x y
    -> Maybe (These (a, x) (b, y))
pairThese (These a b) (These x y) = Just $ These (a, x) (b, y)
pairThese (This a)    (This x)    = Just $ This (a, x)
pairThese (These a _) (This x)    = Just $ This (a, x)
pairThese (This a)    (These x _) = Just $ This (a, x)
pairThese (That b)    (That y)    = Just $ That (b, y)
pairThese (These _ b) (That y)    = Just $ That (b, y)
pairThese (That b)    (These _ y) = Just $ That (b, y)
pairThese _            _          = Nothing

-- |
-- Store created sockets.  If system supports IPv6 and IPv4 we create socket for
-- IPv4 and IPv6.  Otherwise only one.
type Sockets = These
    (Last (WithAddrFamily 'IPv6 Socket))
    (Last (WithAddrFamily 'IPv4 Socket))

-- |
-- A counter part of @'Ntp.Client.Sockets'@ data type.
type Addresses = These
    (First (WithAddrFamily 'IPv6 SockAddr))
    (First (WithAddrFamily 'IPv4 SockAddr))

ntpPort :: PortNumber
ntpPort = 123

-- |
-- Returns a list of alternatives. At most of length two,
-- at most one ipv6 / ipv4 address.
resolveHost :: Tracer IO NtpTrace -> String -> IO (Maybe Addresses)
resolveHost tracer host = do
    let hints = Socket.defaultHints
            { addrSocketType = Datagram
            , addrFlags = [AI_ADDRCONFIG]  -- since we use @AF_INET@ family
            }
    -- TBD why catch here? Why not let @'resolveHost'@ throw the exception?
    addrInfos <- Socket.getAddrInfo (Just hints) (Just host) Nothing
            `catch` (\(e :: IOException) -> (traceWith tracer $ NtpTraceResolvHostIOException e) >> return [])

    let maddr = getOption $ foldMap fn addrInfos
    case maddr of
        Nothing   -> traceWith tracer $ NtpTraceResolveHostNotResolved host
        Just _addr -> traceWith tracer $ NtpTraceResolveHostResolved host
          {-
          where
                g :: First (WithAddrFamily t SockAddr) -> [SockAddr]
                g (First a) = [runWithAddrFamily a]
                addrs :: [SockAddr]
                addrs = foldThese . bimap g g $ addr
          -}

    return maddr
    where
        -- Return supported address: one ipv6 and one ipv4 address.
        fn :: AddrInfo -> Option Addresses
        fn addr = case Socket.addrFamily addr of
            Socket.AF_INET6 ->
                Option $ Just $ This $ First $ (WithIPv6 $ Socket.addrAddress addr)
            Socket.AF_INET  ->
                Option $ Just $ That $ First $ (WithIPv4 $ Socket.addrAddress addr)
            _               -> mempty

resolveNtpHost :: Tracer IO NtpTrace -> String -> IO (Maybe Addresses)
resolveNtpHost tracer host = do
    addr <- resolveHost tracer host
    return $ fmap (bimap adjustPort adjustPort) addr
    where
        adjustPort :: First (WithAddrFamily t SockAddr) -> First (WithAddrFamily t SockAddr)
        adjustPort = fmap $ fmap (replacePort ntpPort)

replacePort :: PortNumber -> SockAddr ->  SockAddr
replacePort port (SockAddrInet  _ host)            = SockAddrInet  port host
replacePort port (SockAddrInet6 _ flow host scope) = SockAddrInet6 port flow host scope
replacePort _    sockAddr                          = sockAddr

createAndBindSock
    :: Tracer IO NtpTrace
    -> AddrFamily
    -- ^ indicates which socket family to create, either @AF_INET6@ or @AF_INET@
    -> [AddrInfo]
    -- ^ list of local addresses
    -> IO (Maybe Sockets)
createAndBindSock tracer addressFamily addrs =
    traverse createDo (selectAddr addrs)
  where
    selectAddr :: [AddrInfo] -> Maybe AddrInfo
    selectAddr = find $ \addr ->
        case addressFamily of
            IPv6 -> addrFamily addr == AF_INET6
            IPv4 -> addrFamily addr == AF_INET

    createDo addr = do
        sock <- Socket.socket (addrFamily addr) Datagram Socket.defaultProtocol
        Socket.setSocketOption sock ReuseAddr 1
        Socket.bind sock (addrAddress addr)
        traceWith tracer $ NtpTraceSocketCreated (show $ addrFamily addr) (show $ addrAddress addr)
--        logInfo $
--            sformat ("Created socket (family/addr): "%shown%"/"%shown)
--                    (addrFamily addr) (addrAddress addr)
        case addressFamily of
            IPv6 -> return $ This $ Last $ (WithIPv6 sock)
            IPv4 -> return $ That $ Last $ (WithIPv4 sock)

udpLocalAddresses :: IO [AddrInfo]
udpLocalAddresses = do
    let hints = Socket.defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Datagram }
#if MIN_VERSION_network(2,8,0)
        port = Socket.defaultPort
#else
        port = Socket.aNY_PORT
#endif
    --                 Hints        Host    Service
    Socket.getAddrInfo (Just hints) Nothing (Just $ show port)

data SendToException
    = NoMatchingSocket
    | SendToIOException AddrFamily IOException
    deriving Show

instance Exception SendToException


-- |
-- Send a request to @addr :: Addresses@ using @sock :: Sockets@.
sendTo
    :: Sockets
    -> ByteString
    -> Addresses
    -- ^ addresses to send to
    -> IO ()
sendTo sock bs addr = case pairThese sock addr of
    Just s -> foldThese $ bimap fn fn s
    Nothing -> throw NoMatchingSocket
    where
    fn :: ( Last (WithAddrFamily t Socket)
          , First (WithAddrFamily t SockAddr)
          )
       -> IO ()
    fn (Last sock_, First addr_) =
        void (Socket.ByteString.sendTo (runWithAddrFamily sock_) bs (runWithAddrFamily addr_))
            `catch` handleIOException (getAddrFamily addr_)

    handleIOException :: AddrFamily -> IOException -> IO ()
    handleIOException addressFamily e = throw (SendToIOException addressFamily e)

-- |
-- Low level primitive which sends a request to a single NTP server.
sendPacket
    :: Tracer IO NtpTrace
    -> Sockets
    -> NtpPacket
    -> [Addresses]
    -> IO ()
sendPacket tracer sock packet addrs = do
    let bs = LBS.toStrict $ encode $ packet
    traverse_
        (\addr ->
            (sendTo sock bs addr)
                `catch` handleSendToException addr
        )
        addrs
  where
    handleSendToException :: Addresses -> SendToException -> IO ()
    handleSendToException addr NoMatchingSocket =
        traceWith tracer $ NtpTraceSendPacketNoMatchingSocket (show addr) (show sock)
    handleSendToException addr (SendToIOException addressFamily ioerr) = do
        traceWith tracer $ NtpTraceSentToIOException (show addressFamily) ioerr
        case (addr, addressFamily) of
            -- try to send the packet to the other address in case the current
            -- system does not support IPv4/6.
            (These _ r, IPv6) -> do
                traceWith tracer $ NtpTraceSentTryResend $ show $ runWithAddrFamily $ getFirst r
                sendPacket tracer sock packet [That r]
            (These l _, IPv4) -> do
                traceWith tracer $ NtpTraceSentTryResend $ show $ runWithAddrFamily $ getFirst l
                sendPacket tracer sock packet [This  l]
            _                 -> traceWith tracer $ NtpTraceSentNotRetrying
