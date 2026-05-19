{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Ouroboros.Network.ExitPolicy
  ( RepromoteDelay (..)
  , ExitPolicy (..)
  , ReturnPolicy
  , alwaysCleanReturnPolicy
  ) where

import Control.Monad.Class.MonadTime.SI
import Data.Semigroup (Max (..))

-- | After demoting a peer to Warm or Cold, we use a delay to re-promote it
-- back.
--
newtype RepromoteDelay = RepromoteDelay { repromoteDelay :: DiffTime }
  deriving (Eq, Ord)
  deriving newtype Num
  deriving newtype Fractional
  deriving Semigroup via Max DiffTime


-- It ought to be derived via 'Quiet' but 'Difftime' lacks 'Generic' instance.
instance Show RepromoteDelay where
    show (RepromoteDelay d) = "RepromoteDelay " ++ show d

type ReturnPolicy a = a -> RepromoteDelay

-- | 'ReturnPolicy' allows to compute reconnection delay from value return by
-- a mini-protocol.  If a mini-protocol returned with an error 'epErrorDelay'
-- is used.
data ExitPolicy a =
    ExitPolicy {
        -- | Compute 'RepromoteDelay' from a return value.
        --
        epReturnDelay :: ReturnPolicy a,

        -- | The delay when a mini-protocol returned with an error.
        --
        epErrorDelay  :: RepromoteDelay
      }

alwaysCleanReturnPolicy :: RepromoteDelay -- ^ re-promote delay on error
                        -> ExitPolicy a
alwaysCleanReturnPolicy = ExitPolicy $ \_ -> 0
