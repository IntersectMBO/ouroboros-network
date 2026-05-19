{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Network.Protocol.Limits where

import Control.DeepSeq (NFData (..))
import Control.Exception
import Control.Monad.Class.MonadTime.SI
import System.Random (StdGen)

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

newtype ProtocolTimeLimitsWithRnd ps = ProtocolTimeLimitsWithRnd {
      timeLimitForStateWithRnd :: forall (st :: ps). ActiveState st
                               => StateToken st -> StdGen -> (Maybe DiffTime, StdGen)
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
instance NFData ProtocolLimitFailure where
  rnf = \case
      -- for singletons WHNF => NF
      (ExceededTimeLimit sing) -> sing
                            `seq` rnf (showProxy (singToProxy sing))
                            `seq` singToActiveAgency sing
                            `seq` ()
      (ExceededSizeLimit sing) -> sing
                            `seq` rnf (showProxy (singToProxy sing))
                            `seq` singToActiveAgency sing
                            `seq` ()
    where
      singToProxy :: forall ps (st :: ps). StateToken st -> Proxy ps
      singToProxy _ = Proxy

      singToActiveAgency :: forall ps (st :: ps).
                            ActiveState st
                         => StateToken st
                         -> ActiveAgency st
      singToActiveAgency _ = activeAgency


-- TODO: better limits

largeByteLimit :: Word
largeByteLimit = 2500000

smallByteLimit :: Word
smallByteLimit = 0xffff -- 65535

shortWait :: Maybe DiffTime
shortWait = Just 10

longWait :: Maybe DiffTime
longWait = Just 60

waitForever :: Maybe DiffTime
waitForever = Nothing

