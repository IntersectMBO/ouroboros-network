{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Connections.Socket.Client
  ( client
  ) where

import Control.Exception (SomeException)
import Control.Monad.Class.MonadThrow
import qualified Network.Socket as Socket (SockAddr)

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.Connections.Types
import Ouroboros.Network.Snocket (Snocket)
import qualified Ouroboros.Network.Snocket as Snocket

-- | A client backed by a 'Snocket'. It will bind and connect according to the
-- 'ConnectionId'. What this means is determined by the 'Snocket'.
client
  :: forall m socket addr request reject accept .
     ( MonadCatch m, MonadMask m )
  => Snocket m socket addr
  -> Connections (ConnectionId addr) socket request reject accept m
  -> ConnectionId addr
  -> request Local
  -> m (Outcome Local SomeException reject accept socket m)
client snocket connections connid request =
    includeResource connections connid resource request

  where

    resource = New acquire

    -- Must put connection errors into Left.
    -- It's actually OK to catch all exceptions here, because the Connections
    -- term is required to appropriately deal with async exceptions.
    acquire :: m (Either SomeException (AcquiredResource socket m))
    acquire = mask $ \restore -> do
      result <- try (restore (Snocket.openToConnect snocket (localAddress connid)))
      case result of
        Left err     -> pure (Left err)
        Right socket -> do
          result' <- try (restore (bindAndConnect socket))
          case result' of
            Left err -> do
              Snocket.close snocket socket
              pure (Left err)
            Right () -> pure (Right (AcquiredResource socket (Snocket.close snocket socket)))

    bindAndConnect :: socket -> m ()
    bindAndConnect socket = do
      Snocket.bind snocket socket (localAddress connid)
      Snocket.connect snocket socket (remoteAddress connid)
