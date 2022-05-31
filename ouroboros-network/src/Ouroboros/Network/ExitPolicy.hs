{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Ouroboros.Network.ExitPolicy
  ( ReconnectDelay (..)
  , ExitPolicy (..)
  , stdExitPolicy
  , ReturnPolicy
  , alwaysCleanReturnPolicy
  ) where

import           Control.Monad.Class.MonadTime

newtype ReconnectDelay = ReconnectDelay { reconnectDelay :: DiffTime }
  deriving Eq
  deriving newtype Num

-- | 'ReconnectDelay' is an additive monoid.
--
instance Semigroup ReconnectDelay where
    ReconnectDelay a <> ReconnectDelay b = ReconnectDelay (a + b)

instance Monoid ReconnectDelay where
    mempty = ReconnectDelay 0

type ReturnPolicy a = a -> ReconnectDelay

-- | 'ReturnPolicy' allows to compute reconnection delay from value return by
-- a mini-protocol.  If a mini-protocol returned with an error 'epErrorDelay'
-- is used.
data ExitPolicy a =
    ExitPolicy {
        -- | Compute 'ReturnCommand' from return value.
        --
        epReturnDelay :: ReturnPolicy a,

        -- | The delay when a mini-protocol returned with an error.
        --
        epErrorDelay  :: ReconnectDelay
      }

alwaysCleanReturnPolicy :: ReconnectDelay -- ^ reconnection delay on error
                        -> ExitPolicy a
alwaysCleanReturnPolicy = ExitPolicy mempty

-- | 'ExitPolicy' with 10s error delay.
--
stdExitPolicy :: ReturnPolicy a -> ExitPolicy a
stdExitPolicy epReturnDelay =
    ExitPolicy {
        epReturnDelay,
        epErrorDelay = 10
      }
