{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Network.Protocol.Limits where

import           Control.Exception
import           Control.Monad.Class.MonadTime.SI

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Util.ShowProxy


data ProtocolSizeLimits ps bytes = ProtocolSizeLimits {
       sizeLimitForState :: forall (pr :: PeerRole) (st :: ps).
                            PeerHasAgency pr st -> Word,

       dataSize          :: bytes -> Word
     }

data ProtocolTimeLimits ps = ProtocolTimeLimits {
       timeLimitForState :: forall (pr :: PeerRole) (st :: ps).
                            PeerHasAgency pr st -> Maybe DiffTime
     }

data ProtocolLimitFailure where
    ExceededSizeLimit :: forall (pr :: PeerRole) ps (st :: ps).
                         ( forall (st' :: ps). Show (ClientHasAgency st')
                         , forall (st' :: ps). Show (ServerHasAgency st')
                         , ShowProxy ps
                         )
                      => PeerHasAgency pr st
                      -> ProtocolLimitFailure
    ExceededTimeLimit :: forall (pr :: PeerRole) ps (st :: ps).
                         ( forall (st' :: ps). Show (ClientHasAgency st')
                         , forall (st' :: ps). Show (ServerHasAgency st')
                         , ShowProxy ps
                         )
                      => PeerHasAgency pr st
                      -> ProtocolLimitFailure

instance Show ProtocolLimitFailure where
    show (ExceededSizeLimit (stok :: PeerHasAgency pr (st :: ps))) =
      concat
        [ "ExceededSizeLimit ("
        , showProxy (Proxy :: Proxy ps)
        , ") ("
        , show stok
        , ")"
        ]
    show (ExceededTimeLimit (stok :: PeerHasAgency pr (st :: ps))) =
      concat
        [ "ExceededTimeLimit ("
        , showProxy (Proxy :: Proxy ps)
        , ") ("
        , show stok
        , ")"
        ]

instance Exception ProtocolLimitFailure where


-- TODO: better limits

largeByteLimit :: Word
largeByteLimit = 2500000

smallByteLimit :: Word
smallByteLimit = 0xffff

shortWait :: Maybe DiffTime
shortWait = Just 10

longWait :: Maybe DiffTime
longWait = Just 60

waitForever :: Maybe DiffTime
waitForever = Nothing

