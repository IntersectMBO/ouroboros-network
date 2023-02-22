{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Orphans.NoThunks () where

import           Control.Monad.IOSim
import           Control.Monad.ST.Lazy
import           Control.Monad.ST.Unsafe (unsafeSTToIO)
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
import           Ouroboros.Consensus.Util.Orphans ()
import           System.FS.Sim.FsTree
import           System.FS.Sim.MockFS


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

{-------------------------------------------------------------------------------
  fs-sim
-------------------------------------------------------------------------------}

deriving instance NoThunks MockFS
deriving instance NoThunks a => NoThunks (FsTree a)
deriving instance NoThunks HandleMock
deriving instance NoThunks HandleState
deriving instance NoThunks OpenHandleState
deriving instance NoThunks ClosedHandleState
deriving instance NoThunks FilePtr
