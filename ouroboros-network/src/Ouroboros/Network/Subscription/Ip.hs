{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IP subscription worker implentation.
module Ouroboros.Network.Subscription.Ip
    ( ipSubscriptionWorker
    , subscriptionWorker
    , SubscriptionTrace (..)
    , IPSubscriptionTarget (..)
    , WithIPList (..)
    ) where


{- The parallel connection attemps implemented in this module is inspired by
 - RFC8305, https://tools.ietf.org/html/rfc8305 .
 -}

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer
import           Data.Time.Clock (DiffTime)
import qualified Network.Socket as Socket
import           Text.Printf

import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Subscriber
import           Ouroboros.Network.Subscription.Worker


data IPSubscriptionTarget = IPSubscriptionTarget {
    -- | List of destinations to possibly connect to
      ispIps     :: ![Socket.SockAddr]
    -- | Number of parallel connections to keep actice.
    , ispValency :: !Int
    } deriving (Eq, Show)



-- | Spawns a subscription worker which will attempt to keep the specified
-- number of connections (Valency) active towards the list of IP addresses
-- given in IPSubscriptionTarget.
--
ipSubscriptionWorker
    :: forall a x.
       Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> ConnectionTable IO Socket.SockAddr
    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> IPSubscriptionTarget
    -> Main IO () x
    -> (Socket.Socket -> IO a)
    -> IO x
ipSubscriptionWorker tracer tbl localAddresses connectionAttemptDelay ips main k = do
    sVar <- newTVarM ()
    subscriptionWorker
            (WithIPList localAddresses (ispIps ips)
              `contramap` tracer)
            tbl
            sVar
            ioSocket
            (\_ s -> pure s)
            (\_ s -> pure (s, pure ()))
            main
            localAddresses
            connectionAttemptDelay
            getTargets
            (ispValency ips)
            k
  where
    getTargets :: IO (SubscriptionTarget IO Socket.SockAddr)
    getTargets = pure $ listSubscriptionTarget $ ispIps ips


-- | Like 'worker' but in 'IO'; It only instantness local address selection.
--
subscriptionWorker
    :: Tracer IO (SubscriptionTrace Socket.SockAddr)
    -> ConnectionTable IO Socket.SockAddr
    -> StateVar IO s

    -> Socket IO Socket.SockAddr Socket.Socket
    -- callbacks
    -> SocketStateChange   IO s Socket.SockAddr
    -> CompleteApplication IO s Socket.SockAddr a
    -- ^ complete connection callback
    -> Main IO s t
    -- ^ main callback

    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> IO (SubscriptionTarget IO Socket.SockAddr)
    -- ^ subscription targets
    -> Int
    -- ^ valency
    -> (Socket.Socket -> IO a)
    -- ^ application to run on each connection
    -> IO t
subscriptionWorker
  tr tbl sVar socket
  socketStateChangeTx
  completeApplicationTx mainTx
  localAddresses
  connectionAttemptDelay getTargets valency k =
    worker tr tbl sVar socket
           socketStateChangeTx
           completeApplicationTx mainTx
           localAddresses
           selectAddr connectionAttemptDelay
           getTargets valency k

  where
    selectAddr :: Socket.SockAddr
               -> LocalAddresses Socket.SockAddr
               -> Maybe Socket.SockAddr
    selectAddr Socket.SockAddrInet{}  (LocalAddresses (Just localAddr) _ _ ) = Just localAddr
    selectAddr Socket.SockAddrInet6{} (LocalAddresses _ (Just localAddr) _ ) = Just localAddr
    selectAddr Socket.SockAddrUnix{}  (LocalAddresses _ _ (Just localAddr) ) = Just localAddr
    selectAddr _ _ = Nothing

data WithIPList a = WithIPList {
      wilSrc   :: !(LocalAddresses Socket.SockAddr)
    , wilDsts  :: ![Socket.SockAddr]
    , wilEvent :: !a
    }

instance (Show a) => Show (WithIPList a) where
    show (WithIPList (LocalAddresses Nothing (Just ipv6) Nothing) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show ipv6) (show wilDsts) (show wilEvent)
    show (WithIPList (LocalAddresses (Just ipv4) Nothing Nothing) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show ipv4) (show wilDsts) (show wilEvent)
    show (WithIPList (LocalAddresses Nothing Nothing (Just unix)) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show unix) (show wilDsts) (show wilEvent)
    show (WithIPList (LocalAddresses (Just ipv4) (Just ipv6) Nothing) wilDsts wilEvent) =
        printf  "IPs: %s %s %s %s" (show ipv4) (show ipv6)
                                   (show wilDsts) (show wilEvent)
    show WithIPList {wilSrc, wilDsts, wilEvent} =
        printf "IPs: %s %s %s" (show wilSrc) (show wilDsts) (show wilEvent)

