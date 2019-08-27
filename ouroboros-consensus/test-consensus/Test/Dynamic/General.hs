{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Dynamic.General (
    prop_general
  , runTestNetwork
    -- * Re-exports
  , TestOutput (..)
  ) where

import qualified Data.Map as Map
import           Test.QuickCheck

import           Control.Monad.Class.MonadTime
import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Network.Block (HasHeader)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol (LeaderSchedule (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))

import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import qualified Ouroboros.Consensus.Util.ResourceRegistry as ResourceRegistry

import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock

import           Test.Dynamic.Network
import           Test.Dynamic.TxGen
import           Test.Dynamic.Util

import           Test.Util.Range

-- | Execute a fully-connected network of nodes that all join immediately
--
-- Runs the network for the specified number of slots, and returns the
-- resulting 'TestOutput'.
--
runTestNetwork ::
  forall blk.
     ( RunNode blk
     , TxGen blk
     )
  => (CoreNodeId -> ProtocolInfo blk)
  -> NumCoreNodes
  -> NumSlots
  -> Seed
  -> TestOutput blk
runTestNetwork pInfo numCoreNodes numSlots seed = runSimOrThrow $ do
    registry  <- ResourceRegistry.new
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

-- | The properties always required
--
-- Includes:
--
-- * The competitive chains at the end of the simulation respect the expected
--   bound on fork length
-- * The nodes do not leak file handles
--
prop_general ::
     ( Condense blk
     , Eq blk
     , HasHeader blk
     )
  => SecurityParam
  -> LeaderSchedule
  -> TestOutput blk
  -> Property
prop_general k schedule TestOutput{testOutputNodes} =
    counterexample ("schedule: " <> condense schedule) $
    counterexample ("nodeChains: " <> condense nodeChains) $
    tabulate "shortestLength" [show (rangeK k (shortestLength nodeChains))] $
    prop_all_common_prefix
        maxForkLength
        (Map.elems nodeChains) .&&.
    conjoin
      [ fileHandleLeakCheck nid nodeInfo
      | (nid, nodeInfo) <- Map.toList nodeInfos ]
  where
    NumBlocks maxForkLength = determineForkLength k schedule

    nodeChains = nodeOutputFinalChain <$> testOutputNodes
    nodeInfos  = nodeOutputNodeInfo   <$> testOutputNodes

    fileHandleLeakCheck :: NodeId -> NodeInfo blk MockFS -> Property
    fileHandleLeakCheck nid nodeInfo = conjoin
        [ checkLeak "ImmutableDB" $ nodeInfoImmDbFs nodeInfo
        , checkLeak "VolatileDB"  $ nodeInfoVolDbFs nodeInfo
        , checkLeak "LedgerDB"    $ nodeInfoLgrDbFs nodeInfo
        ]
      where
        checkLeak dbName fs = counterexample
          ("Node " <> show nid <> "'s " <> dbName <> " is leaking file handles")
          (Mock.numOpenHandles fs === 0)
