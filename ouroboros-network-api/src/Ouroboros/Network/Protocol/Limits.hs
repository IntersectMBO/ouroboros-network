{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Protocol.Limits where

import Control.Exception
import Control.Monad.Class.MonadTime.SI

import Network.TypedProtocol.Core

import Ouroboros.Network.Util.ShowProxy


data ProtocolSizeLimits ps bytes = ProtocolSizeLimits {
       sizeLimitForState :: forall (st :: ps). ActiveState st
                         => StateToken st -> Word,

       dataSize          :: bytes -> Word
     }

newtype ProtocolTimeLimits ps = ProtocolTimeLimits {
       timeLimitForState :: forall  (st :: ps). ActiveState st
                         => StateToken st -> Maybe DiffTime
     }

data ProtocolLimitFailure where
    ExceededSizeLimit :: forall ps (st :: ps).
                         ( Show (StateToken st)
                         , ShowProxy ps
                         , ActiveState st
                         )
                      => StateToken st
                      -> ProtocolLimitFailure
    ExceededTimeLimit :: forall ps (st :: ps).
                         ( Show (StateToken st)
                         , ShowProxy ps
                         , ActiveState st
                         )
                      => StateToken st
                      -> ProtocolLimitFailure

instance Show ProtocolLimitFailure where
    show (ExceededSizeLimit (stok :: StateToken (st :: ps))) =
      concat
        [ "ExceededSizeLimit ("
        , showProxy (Proxy :: Proxy ps)
        , ") "
        , show (activeAgency :: ActiveAgency st)
        , " ("
        , show stok
        , ")"
        ]
    show (ExceededTimeLimit (stok :: StateToken (st :: ps))) =
      concat
        [ "ExceededTimeLimit ("
        , showProxy (Proxy :: Proxy ps)
        , ") "
        , show (activeAgency :: ActiveAgency st)
        , " ("
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

