{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Test.Dynamic.General (
    runTestNetwork
    -- * Re-exports
  , TestOutput (..)
  ) where

import           Control.Monad.Class.MonadTime
import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.ThreadRegistry

import           Test.Dynamic.Network
import           Test.Dynamic.TxGen

runTestNetwork ::
  forall blk.
     ( RunNode blk
     , TxGen blk
     , TracingConstraints blk
     )
  => (CoreNodeId -> ProtocolInfo blk)
  -> NumCoreNodes
  -> NumSlots
  -> Seed
  -> TestOutput blk
runTestNetwork pInfo numCoreNodes numSlots seed =
    runSimOrThrow $
        withThreadRegistry $ \registry -> do
            testBtime <- newTestBlockchainTime registry numSlots slotLen
            broadcastNetwork
                registry
                testBtime
                numCoreNodes
                pInfo
                (seedToChaCha seed)
                slotLen
  where
    slotLen :: DiffTime
    slotLen = 100000
