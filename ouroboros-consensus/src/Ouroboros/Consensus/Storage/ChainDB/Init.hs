-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
-- > import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
module Ouroboros.Consensus.Storage.ChainDB.Init (
    InitChainDB(..)
  , fromFull
  , cast
  ) where

import           Data.Coerce
import           Data.Functor.Contravariant

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util.IOLike

-- | Restricted interface to the 'ChainDB' used on node initialization
newtype InitChainDB m blk = InitChainDB {
      -- | Add a block to the DB when the current chain is empty.
      --
      -- The given action is only called when the current chain is empty.
      addBlockIfEmpty :: m blk -> m ()
    }

instance Functor m => Contravariant (InitChainDB m) where
  contramap f db = InitChainDB {
        addBlockIfEmpty = addBlockIfEmpty db . (f <$>)
      }

fromFull :: IOLike m => ChainDB m blk -> InitChainDB m blk
fromFull db = InitChainDB {
      addBlockIfEmpty = \mkBlk -> do
          tip <- atomically $ ChainDB.getTipPoint db
          case tip of
            BlockPoint {} -> return ()
            GenesisPoint  -> do
              blk <- mkBlk
              ChainDB.addBlock_ db blk
    }

cast :: (Functor m, Coercible blk blk') => InitChainDB m blk -> InitChainDB m blk'
cast = contramap coerce
