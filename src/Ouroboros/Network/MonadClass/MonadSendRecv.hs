{-# LANGUAGE TypeFamilies #-}
module Ouroboros.Network.MonadClass.MonadSendRecv
  ( MonadSendRecv (..)
  ) where

class Monad m => MonadSendRecv m where
  type BiChan m :: * -> * -> *

  newChan :: m (BiChan m s r)
  sendMsg :: BiChan m s r -> s -> m ()
  recvMsg :: BiChan m s r -> m r
