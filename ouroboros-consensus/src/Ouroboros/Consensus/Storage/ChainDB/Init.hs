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
data InitChainDB m blk = InitChainDB {
      -- | Check if the current chain is empty
      checkEmpty :: m Bool

      -- | Add a block to the DB
    , addBlock   :: blk -> m ()
    }

instance Contravariant (InitChainDB m) where
  contramap f db = InitChainDB {
        checkEmpty = checkEmpty db
      , addBlock   = addBlock   db . f
      }

fromFull :: IOLike m => ChainDB m blk -> InitChainDB m blk
fromFull db = InitChainDB {
      checkEmpty = do
          tip <- atomically $ ChainDB.getTipPoint db
          return $ case tip of
                     BlockPoint {} -> False
                     GenesisPoint  -> True

    , addBlock = ChainDB.addBlock_ db
    }

cast :: Coercible blk blk' => InitChainDB m blk -> InitChainDB m blk'
cast = contramap coerce
