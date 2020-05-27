{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Consensus.Block.Forge (
    CanForge (..)
    -- * MaintainForgeState
  , MaintainForgeState(..)
  , defaultMaintainForgeState
  , castMaintainForgeState
    -- * Infrastructure for dealing with state updates
  , Update(..)
  , updateFromTVar
  , liftUpdate
  , hoistUpdate
  , coerceUpdate
  , traceUpdate
  ) where

import           Control.Tracer (Tracer, traceWith)
import           Crypto.Random (MonadRandom)
import           Data.Bifunctor (first)
import           Data.Coerce

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  CanForge
-------------------------------------------------------------------------------}

class ( NoUnexpectedThunks (ForgeState blk)
      , Show (ForgeState blk)
      ) => CanForge blk where

  -- | (Chain-independent) state required to forge blocks
  type ForgeState blk :: *

  -- Default to ()
  type ForgeState blk = ()

  -- | Forge a new block
  --
  -- TODO: This should be a pure function
  -- <https://github.com/input-output-hk/ouroboros-network/issues/2058>
  forgeBlock
    :: MonadRandom m
    => TopLevelConfig blk
    -> ForgeState blk
    -> BlockNo                -- ^ Current block number
    -> TickedLedgerState blk  -- ^ Current ledger
    -> [GenTx blk]            -- ^ Txs to add in the block
    -> IsLeader (BlockProtocol blk)
    -> m blk

{-------------------------------------------------------------------------------
  Maintaining the 'ForgeState'
-------------------------------------------------------------------------------}

data MaintainForgeState (m :: * -> *) blk = MaintainForgeState {
      -- | Initial forge state
      initForgeState   :: ForgeState blk

      -- | Update the forge state
      --
      -- This function is the reason that 'MaintainForgeState' is a record:
      -- this function may have all kinds of things in its closure; for example,
      -- we might need access to some external hardware crypto hardware
      -- device.
    , updateForgeState :: Update m (ForgeState blk)
                       -- ^ Lens into the node's state
                       -> SlotNo
                       -- ^ Current slot
                       -> m ()
    }

defaultMaintainForgeState :: (Monad m, ForgeState blk ~ ())
                          => MaintainForgeState m blk
defaultMaintainForgeState = MaintainForgeState {
      initForgeState   = ()
    , updateForgeState = \_ _ -> return ()
    }

castMaintainForgeState :: ForgeState blk ~ ForgeState blk'
                       => MaintainForgeState m blk -> MaintainForgeState m blk'
castMaintainForgeState maintainForgeState = MaintainForgeState {
      initForgeState   = initForgeState   maintainForgeState
    , updateForgeState = updateForgeState maintainForgeState
    }

{-------------------------------------------------------------------------------
  Updating the state
-------------------------------------------------------------------------------}

-- | Update a stateful value
newtype Update m a = Update {
      -- | Update the value, and produce a result
      --
      -- If 'Nothing', the action will be retried.
      runUpdate :: forall b. (a -> Maybe (a, b)) -> m b
    }

updateFromTVar :: MonadSTM m => StrictTVar m a -> Update m a
updateFromTVar var = Update $ \f -> atomically $ do
    a <- readTVar var
    case f a of
      Nothing      -> retry
      Just (a', b) -> writeTVar var a' >> return b

hoistUpdate :: (forall x. m x -> n x) -> Update m a -> Update n a
hoistUpdate hoist upd = Update {
      runUpdate = hoist . runUpdate upd
    }

liftUpdate :: (large -> small)
           -> (small -> large -> large)
           -> Update m large
           -> Update m small
liftUpdate get set (Update update) = Update $ \f ->
    update $ \large ->
      first (flip set large) <$> (f (get large))

coerceUpdate :: Coercible a b => Update m a -> Update m b
coerceUpdate = liftUpdate coerce (\new _old -> coerce new)

traceUpdate :: forall m a. Monad m => Tracer m a -> Update m a -> Update m a
traceUpdate tracer upd = Update $ \(f :: a -> Maybe (a, b)) -> do
    -- Wrap @f'@ so that we get the new value of the state back
    let f' :: a -> Maybe (a, (a, b))
        f' = fmap (\(a', b) -> (a', (a', b))) . f
    (a', b) <- runUpdate upd f'
    traceWith tracer a'
    return b
