
module Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore
  ( -- * DNS semaphore
    DNSSemaphore
  , newLedgerAndPublicRootDNSSemaphore
  , newDNSLocalRootSemaphore
  , withDNSSemaphore
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Concurrent.Class.MonadSTM.TSem
import           Control.Monad.Class.MonadThrow

-- | Maximal concurrency when resolving DNS names of root and ledger peers.
--
maxDNSConcurrency :: Integer
maxDNSConcurrency = 8

-- | Maximal concurrency when resolving DNS names of local root peers.
--
maxDNSLocalRootConcurrency :: Integer
maxDNSLocalRootConcurrency = 2

-- | A semaphore used to limit concurrency of dns names resolution.
--
newtype DNSSemaphore m = DNSSemaphore (TSem m)

-- | Create a `DNSSemaphore` for root and ledger peers.
--
newLedgerAndPublicRootDNSSemaphore :: MonadSTM m => m (DNSSemaphore m)
newLedgerAndPublicRootDNSSemaphore = DNSSemaphore <$> atomically (newTSem maxDNSConcurrency)

-- | Create a `DNSSemaphore` for local root peers.
--
newDNSLocalRootSemaphore :: MonadSTM m => STM m (DNSSemaphore m)
newDNSLocalRootSemaphore = DNSSemaphore <$> newTSem maxDNSLocalRootConcurrency

-- | Run a computation by attempting to acquire the semaphore first.
-- On termination or failure free the semaphore
--
withDNSSemaphore :: (MonadSTM m, MonadThrow m) => DNSSemaphore m -> m a -> m a
withDNSSemaphore (DNSSemaphore s) =
    bracket_ (atomically $ waitTSem s)
             (atomically $ signalTSem s)

