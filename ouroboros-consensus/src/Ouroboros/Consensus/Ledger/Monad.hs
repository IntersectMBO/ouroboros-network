{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeFamilies      #-}

-- | A monad for the interface to the ledger layer
module Ouroboros.Consensus.Ledger.Monad (
    -- * Ledger events
    AuxLedgerEvent
  , VoidLedgerEvent
    -- * Ledger monad
  , LedgerResult (..)
  , LedgerT (..)
  , coerceLedgerT
  , evalLedgerT
  , hoistLedgerT
  , ledgerT
  , liftLedgerT
  , runLedgerT
  , withLedgerT
  ) where

import           Control.Monad.Except
import           Control.Monad.Morph (MFunctor (..))
import           Control.Monad.Reader (ReaderT (ReaderT), runReaderT,
                     withReaderT)
import           Control.Monad.State.Strict (StateT (StateT), runStateT)
import           Data.Functor ((<&>))
import           Data.Kind (Type)

{-------------------------------------------------------------------------------
  Ledger monad
-------------------------------------------------------------------------------}

-- | Event emitted by the ledger
--
-- TODO we call this 'AuxLedgerEvent' to differentiate from 'LedgerEvent' in
-- 'InspectLedger'. When that module is rewritten to make use of ledger
-- derived events, we may rename this type.
type family AuxLedgerEvent l :: Type

-- | A 'Data.Void.Void' isomorph for ledgers that have no events.
--
data VoidLedgerEvent

-- | The monad transformer used when interacting with the ledger
newtype LedgerT l m a = LedgerT {
    unLedgerT :: forall ev. ReaderT (AuxLedgerEvent l -> ev) (StateT [ev] m) a
  }

-- | The primary way to build a 'LedgerT' computation
ledgerT :: Functor m => m (LedgerResult l a) -> LedgerT l m a
ledgerT m =
    LedgerT $ ReaderT $ \r -> StateT $ \s ->
      m <&> \case
        LedgerResult {
            lrEvents = events
          , lrResult = !a
          } -> (a, s <> map r events)

instance Monad m => Functor (LedgerT l m) where
  fmap f (LedgerT m) = LedgerT (fmap f m)

instance Monad m => Applicative (LedgerT l m) where
  pure a = LedgerT (pure a)
  (<*>) = ap

instance Monad m => Monad (LedgerT l m) where
  LedgerT m >>= k = LedgerT $ do
    a <- m
    unLedgerT $ k a

-- | The result of evaluating a 'LedgerT' computation
--
-- Note: this type intentionally does not instantiate 'Applicative' or
-- 'Monad'. Use 'LedgerT' and 'runLedgerT' for that.
data LedgerResult l a = LedgerResult
  { lrEvents :: [AuxLedgerEvent l]
  , lrResult :: !a
  }
  deriving (Foldable, Functor, Traversable)

instance MonadTrans (LedgerT l) where
  lift = liftLedgerT

liftLedgerT :: Monad m => m a -> LedgerT l m a
liftLedgerT m = LedgerT (lift (lift m))

instance MFunctor (LedgerT l) where
  hoist = hoistLedgerT

coerceLedgerT ::
     (AuxLedgerEvent l ~ AuxLedgerEvent l')
  => LedgerT l m a
  -> LedgerT l' m a
coerceLedgerT (LedgerT m) = LedgerT m

withLedgerT ::
     (AuxLedgerEvent l -> AuxLedgerEvent l')
  -> LedgerT l m a
  -> LedgerT l' m a
withLedgerT inj (LedgerT m) = LedgerT $ withReaderT (. inj) m

hoistLedgerT ::
     Monad m
  => (forall b. m b -> n b)
  -> LedgerT l m a
  -> LedgerT l n a
hoistLedgerT f (LedgerT m) = LedgerT (hoist (hoist f) m)

-- | Run a 'LedgerT' computation
runLedgerT :: Monad m => LedgerT l m a -> m (LedgerResult l a)
runLedgerT m = do
    (a, events) <- runStateT (runReaderT (unLedgerT m) id) []
    pure $ LedgerResult {
        lrEvents = events
      , lrResult = a
      }

-- | Run a 'LedgerT' computation, forgetting all unnecessary oputput
evalLedgerT :: Monad m => LedgerT l m a -> m a
evalLedgerT = fmap lrResult .  runLedgerT

