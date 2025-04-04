{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.KESAgent.Processes.Agent.Monad
where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import qualified Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.DirectSerialise (
  DirectDeserialise (..),
  DirectSerialise (..),
 )
import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Monad.Class.MonadAsync (MonadAsync)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadCatch)
import Control.Monad.Class.MonadTime (MonadTime)
import Control.Monad.Class.MonadTimer (MonadTimer)
import Data.Typeable (Typeable)

import Cardano.Crypto.KES.Class (ContextKES, KESAlgorithm (..))
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (OCertSignable)
import Cardano.KESAgent.Protocols.VersionedProtocol

type MonadAgent m =
  ( Monad m
  , MonadAsync m
  , MonadCatch m
  , MonadFail m
  , MonadMVar m
  , MonadST m
  , MonadTimer m
  , MonadTime m
  )

type AgentCrypto c =
  ( ContextDSIGN (DSIGN c) ~ ()
  , ContextKES (KES c) ~ ()
  , DSIGN.Signable (DSIGN c) (OCertSignable c)
  , DirectSerialise (SignKeyKES (KES c))
  , DirectDeserialise (SignKeyKES (KES c))
  , Crypto c
  , NamedCrypto c
  , Typeable c
  )
