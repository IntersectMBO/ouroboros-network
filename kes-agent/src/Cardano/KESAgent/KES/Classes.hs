{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.KESAgent.KES.Classes
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.Service.Protocol

import Cardano.Binary
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class

import Ouroboros.Network.RawBearer

import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM.TChan
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime ( MonadTime (..) )
import Control.Monad.Class.MonadTimer
import Data.Typeable ( Typeable )

-- | Shorthand to group typeclasses relating to (cryptographic) memory
-- management, including direct access to mlocked and bytestring memory, MVars,
-- timing, exceptions, and manual memory management.
class ( MonadAsync m
      , MonadCatch m
      , MonadDelay m
      , MonadFail m
      , MonadMVar m
      , MonadST m
      , MonadSTM m
      , MonadThrow m
      , MonadTime m
      ) => MonadMemoryEffects m

deriving anyclass instance {-# OVERLAPPING #-}
      ( MonadAsync m
      , MonadCatch m
      , MonadDelay m
      , MonadFail m
      , MonadMVar m
      , MonadST m
      , MonadSTM m
      , MonadThrow m
      , MonadTime m
      )
      => MonadMemoryEffects m

-- | Shorthand to group typeclasses relating to KES cryptography and the KES
-- Agent Protocol. This includes KES cryptography itself, as well as direct
-- serialisation/deserialisation, and the KES Agent Protocol.
class ( Crypto c
      , Typeable c
      , ContextKES (KES c) ~ ()
      , VersionedProtocol (ServiceProtocol m c)
      , KESAlgorithm (KES c)
      , DirectDeserialise (SignKeyKES (KES c))
      , DirectSerialise (SignKeyKES (KES c))
      , MonadMemoryEffects m
      ) => MonadKES m c

deriving anyclass instance {-# OVERLAPPING #-}
      ( Crypto c
      , Typeable c
      , ContextKES (KES c) ~ ()
      , VersionedProtocol (ServiceProtocol m c)
      , KESAlgorithm (KES c)
      , DirectDeserialise (SignKeyKES (KES c))
      , DirectSerialise (SignKeyKES (KES c))
      , MonadMemoryEffects m
      ) => MonadKES m c
