{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE DerivingStrategies #-}
{-#LANGUAGE DeriveAnyClass #-}
{-#LANGUAGE UndecidableInstances #-}

module Cardano.KESAgent.Classes
where

import Cardano.Binary
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class
import Cardano.Crypto.MonadMLock
import Cardano.KESAgent.DirectBearer
import Cardano.KESAgent.OCert
import Cardano.KESAgent.Protocol
import Control.Concurrent.Class.MonadSTM.TChan
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadMVar
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime (MonadTime (..))
import Control.Monad.Class.MonadTimer
import Data.Typeable (Typeable)

-- | Shorthand to group typeclasses relating to (cryptographic) memory
-- management, including direct access to mlocked and bytestring memory, MVars,
-- timing, exceptions, and manual memory management.
class ( MonadAsync m
      , MonadByteStringMemory m
      , MonadCatch m
      , MonadDelay m
      , MonadFail m
      , MonadMVar m
      , MonadST m
      , MonadSTM m
      , MonadThrow m
      , MonadTime m
      , MonadUnmanagedMemory m
      , MonadMLock m
      ) => MonadMemoryEffects m

deriving anyclass instance
      ( MonadAsync m
      , MonadByteStringMemory m
      , MonadCatch m
      , MonadDelay m
      , MonadFail m
      , MonadMVar m
      , MonadST m
      , MonadSTM m
      , MonadThrow m
      , MonadTime m
      , MonadUnmanagedMemory m
      , MonadMLock m
      )
      => MonadMemoryEffects m

-- | Shorthand to group typeclasses relating to KES cryptography and the KES
-- Agent Protocol. This includes KES cryptography itself, as well as direct
-- serialisation/deserialisation, and the KES Agent Protocol.
class ( Crypto c
      , Typeable c
      , ContextKES (KES c) ~ ()
      , VersionedProtocol (KESProtocol m c)
      , KESSignAlgorithm m (KES c)
      , DirectDeserialise m (SignKeyKES (KES c))
      , DirectSerialise m (SignKeyKES (KES c))
      , MonadMemoryEffects m
      ) => MonadKES m c

deriving anyclass instance
      ( Crypto c
      , Typeable c
      , ContextKES (KES c) ~ ()
      , VersionedProtocol (KESProtocol m c)
      , KESSignAlgorithm m (KES c)
      , DirectDeserialise m (SignKeyKES (KES c))
      , DirectSerialise m (SignKeyKES (KES c))
      , MonadMemoryEffects m
      ) => MonadKES m c


-- | Shorthand to group typeclasses relating to networking.
class ( ToDirectBearer m fd
      ) => MonadNetworking m fd

deriving anyclass instance
      ( ToDirectBearer m fd
      ) => MonadNetworking m fd
