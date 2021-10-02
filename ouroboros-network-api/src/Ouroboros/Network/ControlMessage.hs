{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Network.ControlMessage where

import           Control.Monad.Class.MonadSTM

-- | Control signal sent to a mini-protocol. Expected to exit, on 'Continue' it
-- should continue its operation
--
data ControlMessage =
    -- | Continue operation.
      Continue

    -- | Hold on, e.g. do not sent messages until resumed.  This is not used for
    -- any hot protocol.
    --
    | Quiesce

    -- | The client is expected to terminate as soon as possible.
    --
    | Terminate
  deriving (Eq, Show)

-- |  'ControlMessageSTM' should depend on `muxMode` (we only need to schedule
-- stop for initiator side).  This is not done only because this would break
-- tests, but once the old api is removed it should be possible.
--
type ControlMessageSTM m = STM m ControlMessage

continueForever :: Applicative (STM m)
                => proxy m
                -> ControlMessageSTM m
continueForever _ = pure Continue


-- | First to finish synchronisation between 'Terminate' state of
-- 'ControlMessage' and an stm action.
--
-- This should return @STM m (Maybe a)@ but 'STM' is a non-injective type
-- family, and we would need to pass @Proxy m@ to fix an ambiguous type (or use
-- 'AllowAmbiguousTypes' extension).
--
timeoutWithControlMessage :: MonadSTM m
                          => ControlMessageSTM m
                          -> STM m a
                          -> m (Maybe a)
timeoutWithControlMessage controlMessageSTM stm =
    atomically $ timeoutWithControlMessageSTM controlMessageSTM stm

timeoutWithControlMessageSTM :: MonadSTM m
                             => ControlMessageSTM m
                             -> STM m a
                             -> STM m (Maybe a)
timeoutWithControlMessageSTM controlMessageSTM stm =
  do cntrlMsg <- controlMessageSTM
     case cntrlMsg of
       Terminate -> return Nothing
       Continue  -> retry
       Quiesce   -> retry
  `orElse` (Just <$> stm)
