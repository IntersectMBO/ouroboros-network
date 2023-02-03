-- | A Trivial backing store that just stores the slot number on a TVar. To be
-- used by tests.
module Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Trivial (trivialBackingStore) where

import           Cardano.Slotting.Slot (WithOrigin (..))
import           Control.Monad (void)
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike

{-------------------------------------------------------------------------------
  A trivial backing store that holds only the slot number
-------------------------------------------------------------------------------}

trivialBackingStore :: IOLike m => values -> m (BackingStore m keys values diff)
trivialBackingStore emptyValues = do
  seqNo <- IOLike.newTVarIO Origin
  pure $ BackingStore
              (pure ())
              (\_ _ -> pure ())
              (IOLike.atomically $ do
                  s <- IOLike.readTVar seqNo
                  pure $ ( s
                         , BackingStoreValueHandle
                             (pure ())
                             (\_ -> pure emptyValues)
                             (\_ -> pure emptyValues)
                         )
              )
              (\s _ -> void
                     $ IOLike.atomically
                     $ IOLike.modifyTVar seqNo (\_ -> At s))
