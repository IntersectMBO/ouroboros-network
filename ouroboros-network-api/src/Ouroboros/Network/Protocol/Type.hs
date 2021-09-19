{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Network.Protocol.Type where
{-# DEPRECATED Ouroboros.Network.Protocol.Type "Import Ouroboros.Network.Protocol.Limits instead" #-}

import Control.Exception
import Control.Monad.Class.MonadTime
import Data.Singletons

import Network.TypedProtocol.Core

import Ouroboros.Network.Util.ShowProxy


data ProtocolSizeLimits ps bytes = ProtocolSizeLimits {
       sizeLimitForState :: forall (st :: ps). ActiveState st
                         => Sing st -> Word,

       dataSize          :: bytes -> Word
     }

data ProtocolTimeLimits ps = ProtocolTimeLimits {
       timeLimitForState :: forall  (st :: ps). ActiveState st
                         => Sing st -> Maybe DiffTime
     }

data ProtocolLimitFailure where
    ExceededSizeLimit :: forall ps (st :: ps).
                         ( Show (Sing st)
                         , ShowProxy ps
                         , ActiveState st
                         )
                      => Sing st
                      -> ProtocolLimitFailure
    ExceededTimeLimit :: forall ps (st :: ps).
                         ( Show (Sing st)
                         , ShowProxy ps
                         , ActiveState st
                         )
                      => Sing st
                      -> ProtocolLimitFailure

instance Show ProtocolLimitFailure where
    show (ExceededSizeLimit (stok :: Sing (st :: ps))) =
      concat
        [ "ExceededSizeLimit ("
        , showProxy (Proxy :: Proxy ps)
        , ") "
        , show (activeAgency :: ActiveAgency st)
        , " ("
        , show stok
        , ")"
        ]
    show (ExceededTimeLimit (stok :: Sing (st :: ps))) =
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
