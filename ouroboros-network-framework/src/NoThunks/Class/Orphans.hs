{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module NoThunks.Class.Orphans where

import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as IntPSQ
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as OrdPSQ

import           Network.Socket (SockAddr)
import           Control.Monad.Class.MonadTime.SI (Time (..))
import           Control.DeepSeq (NFData (..))

import           NoThunks.Class

noThunksInKeysPrioritiesAndValues
  :: (NoThunks k, NoThunks v, NoThunks p)
  => Context -> [(k, v, p)] -> IO (Maybe ThunkInfo)
noThunksInKeysPrioritiesAndValues ctxt =
      allNoThunks
    . concatMap (\(k, v, p) -> [ noThunks ctxt k
                               , noThunks ctxt v
                               , noThunks ctxt p
                               ])

instance (NoThunks p, NoThunks k, NoThunks v) => NoThunks (OrdPSQ k p v) where
    wNoThunks ctxt a = noThunksInKeysPrioritiesAndValues ctxt (OrdPSQ.toList a)
    showTypeOf _ = "OrdPSQ"

instance (NoThunks k, NoThunks v) => NoThunks (IntPSQ k v) where
    wNoThunks ctxt a = noThunksInKeysPrioritiesAndValues ctxt ((\(k, p, v) -> (k, p, v)) <$> IntPSQ.toList a)
    showTypeOf _ = "IntPSQ"

deriving via InspectHeap SockAddr instance NoThunks SockAddr
deriving anyclass instance (NoThunks Time)
deriving newtype instance (NFData Time)
