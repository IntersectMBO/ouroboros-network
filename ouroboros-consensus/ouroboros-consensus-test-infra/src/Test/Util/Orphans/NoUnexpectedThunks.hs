{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Orphans.NoUnexpectedThunks () where

import           Control.Monad.IOSim
import           Control.Monad.ST.Lazy
import           Control.Monad.ST.Unsafe (unsafeSTToIO)
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm

import           Cardano.Prelude (NoUnexpectedThunks (..), ThunkInfo (..))

-- | Just like the IO instance, we don't actually check anything here
instance NoUnexpectedThunks (SimM s a) where
  showTypeOf _ = "SimM"
  whnfNoUnexpectedThunks _ctxt _act = return NoUnexpectedThunks

instance NoUnexpectedThunks a => NoUnexpectedThunks (StrictTVar (SimM s) a) where
  showTypeOf _ = "StrictTVar SimM"
  whnfNoUnexpectedThunks ctxt tvar = do
      a <- unsafeSTToIO $ lazyToStrictST $ execReadTVar (toLazyTVar tvar)
      noUnexpectedThunks ctxt a

instance NoUnexpectedThunks a => NoUnexpectedThunks (StrictMVar (SimM s) a) where
  showTypeOf _ = "StrictMVar SimM"
  whnfNoUnexpectedThunks ctxt StrictMVar { tvar } = do
      a <- unsafeSTToIO $ lazyToStrictST $ execReadTVar tvar
      noUnexpectedThunks ctxt a
