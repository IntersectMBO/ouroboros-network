{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Orphans.NoThunks () where

import           NoThunks.Class (NoThunks (..))

import           Control.Monad.IOSim
import           Control.Monad.ST.Lazy
import           Control.Monad.ST.Unsafe (unsafeSTToIO)
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm


-- | Just like the IO instance, we don't actually check anything here
instance NoThunks (IOSim s a) where
  showTypeOf _ = "IOSim"
  wNoThunks _ctxt _act = return Nothing

instance NoThunks a => NoThunks (StrictTVar (IOSim s) a) where
  showTypeOf _ = "StrictTVar IOSim"
  wNoThunks ctxt tvar = do
      a <- unsafeSTToIO $ lazyToStrictST $ execReadTVar (toLazyTVar tvar)
      noThunks ctxt a

instance NoThunks a => NoThunks (StrictMVar (IOSim s) a) where
  showTypeOf _ = "StrictMVar IOSim"
  wNoThunks ctxt StrictMVar { tvar } = do
      a <- unsafeSTToIO $ lazyToStrictST $ execReadTVar tvar
      noThunks ctxt a
