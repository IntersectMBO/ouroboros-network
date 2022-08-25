{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
-- > import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
module Ouroboros.Consensus.Storage.ChainDB.Init (
    InitChainDB (..)
  , fromFull
  , map
  ) where

import           Prelude hiding (map)

import           Data.Proxy

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import           Ouroboros.Consensus.Util.IOLike

-- | Restricted interface to the 'ChainDB' used on node initialization
data InitChainDB m blk = InitChainDB {
      -- | Add a block to the DB
      addBlock         :: blk -> m ()

      -- | Return the current ledger state
    , getCurrentLedger :: m (LedgerState blk)
    }

fromFull ::
     forall m blk wt. (IOLike m, GetTip (LedgerState blk), IsSwitchLedgerTables wt)
  => ChainDB m blk wt -> InitChainDB m blk
fromFull db = InitChainDB {
      addBlock         =
        ChainDB.addBlock_ db InvalidBlockPunishment.noPunishment
    , getCurrentLedger = atomically f
    }
  where f = undefined -- case singByProxy (Proxy @wt) of
          -- SWithLedgerTables -> destroyLedgerTables . ledgerState <$> ChainDB.getCurrentLedger db
          -- SWithoutLedgerTables -> ledgerState <$> ChainDB.getCurrentLedger db

map ::
     Functor m
  => (blk' -> blk)
  -> (LedgerState blk -> LedgerState blk')
  -> InitChainDB m blk -> InitChainDB m blk'
map f g db = InitChainDB {
      addBlock         = addBlock db . f
    , getCurrentLedger = g <$> getCurrentLedger db
    }
