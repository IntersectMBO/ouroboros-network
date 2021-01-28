{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.HardFork.History.Follower (
    -- * API
    HistoryFollower (..)
  , WithInterpreter (..)
  , WithInterDefault
  , mkWithInterpreter
  , newHistoryFollower
    -- * Client
  , ClientHistoryFollower (..)
  , WithInterClient
  , newClientFollower
  ) where

import           Control.Monad.Class.MonadSTM.Strict hiding (newTVarIO)
import           Data.Either (isRight)
import           Data.SOP.Strict

import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HardFork.History.Qry
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Storage.ChainDB
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Network.Block
import           Ouroboros.Network.Point


-- | The 'HistoryFollower' can be used to extend the 'Follower' with metadata
-- which come from the ledger state. Currently this metadata is a History
-- 'Interpreter' but it's technically possible to generalise this.
--
-- In the same way that a 'Follower' keeps track of the tip of a user, the
-- 'HistoryFollower' keeps track and mirrors the Interpreter that a user has.
-- Everytime a new 'Interpreter' is created and returned to the user, it is also
-- cached. The api returns 'Nothing' when the cached 'Interpreter' is capable
-- of serving the specified point, so the user doesn't need a new one.
--
newtype HistoryFollower m blk = HistoryFollower {
      getInterpreter :: Point blk -> STM m (Maybe (Interpreter (HardForkIndices blk)))
    }

data WithInterpreter blk b = WithInterpreter {
      payload     :: b
    , interpreter :: Maybe (Interpreter (HardForkIndices blk))
    } deriving Show

type WithInterDefault blk = WithInterpreter blk (Serialised blk)

mkWithInterpreter ::
     b
  -> Maybe (Interpreter (HardForkIndices blk))
  -> WithInterpreter blk b
mkWithInterpreter b mInter = WithInterpreter {
      payload     = b
    , interpreter = mInter
    }

instance (ShowProxy blk, ShowProxy b) => ShowProxy (WithInterpreter blk b) where
    showProxy _ = mconcat [
        "WithInterpreter "
      , showProxy (Proxy :: Proxy blk)
      , " "
      , showProxy (Proxy :: Proxy b)
      ]

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

newHistoryFollower ::
     forall blk m.
     ( IOLike m
     , HasHardForkHistory blk
     , IsLedger (LedgerState blk)
     )
  => LedgerConfig blk
  -> ChainDB m blk
  -> m (HistoryFollower m blk)
newHistoryFollower cfg chainDB = do
    varState <- newTVarIO Nothing
    return $ HistoryFollower {
        getInterpreter =
          getInterpreterImpl cfg (ledgerState <$> getCurrentLedger chainDB) varState
      }

getInterpreterImpl ::
     (IOLike m, HasHardForkHistory blk)
  => LedgerConfig blk
  -> STM m (LedgerState blk)
  -> StrictTVar m (Maybe (Interpreter (HardForkIndices blk)))
  -> Point blk
  -> STM m (Maybe (Interpreter (HardForkIndices blk)))
getInterpreterImpl cfg getState varInterpreter point = do
    minter <- readTVar varInterpreter
    case (minter, pointSlot point) of
      (Just inter, At slot) | isSlotInHorizon slot inter -> return Nothing
      _ -> do
        st <- getState
        let inter = History.mkInterpreter $ hardForkSummary cfg st
        writeTVar varInterpreter $ Just inter
        return $ Just inter

-- | Run a sanity query to check if the slot is still in the Horizon.
isSlotInHorizon :: SlotNo -> Interpreter xs -> Bool
isSlotInHorizon slotNo inter = isRight $ interpretQuery inter query
  where
    query = (,) <$> slotToWallclock slotNo <*> slotToEpoch' slotNo

{-------------------------------------------------------------------------------
  Client History Follower
-------------------------------------------------------------------------------}

-- | This can be used by a client. Given a 'WithInterClient',
-- 'getInterpreter' is the best effort to retrieve an 'Interpreter'.
newtype ClientHistoryFollower m blk = ClientHistoryFollower {
      getInterpreterClient :: WithInterClient blk
                           -> STM m (Maybe (Interpreter (HardForkIndices blk)))
    }

newClientFollower :: MonadSTM m => m (ClientHistoryFollower m blk)
newClientFollower = do
    mInter <- newTVarIO Nothing
    return $ ClientHistoryFollower $ getInterClientImpl mInter

getInterClientImpl ::
     MonadSTM m
  => StrictTVar m (Maybe (Interpreter (HardForkIndices blk)))
  -> WithInterClient blk
  -> STM m (Maybe (Interpreter (HardForkIndices blk)))
getInterClientImpl interVar withInter =
    case interpreter withInter of
      Just inter -> do
        writeTVar interVar (Just inter)
        return $ Just inter
      Nothing -> do
        mInter <- readTVar interVar
        return mInter

type WithInterClient blk = WithInterpreter blk blk
