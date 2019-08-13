{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Dynamic.HardFork
  ( tests
  )
where

import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General
import           Test.Dynamic.Util

import           Test.Util.Orphans.Arbitrary ()

import Debug.Trace
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.HardFork

tests :: TestTree
tests =
  testGroup "Dynamic chain generation"
    [ testProperty "simple PBFT/Praos Hard Fork convergence" $
        withMaxSuccess 1 $ prop_simple_hard_fork_convergence k
    , testProperty "broken" $ withMaxSuccess 1 $ prop_broken
    ]
  where
    k = defaultSecurityParam

class
  OuroborosTag (BlockProtocol blk)
  => BrokenBlock blk

instance
  (CanHardFork (BlockProtocol blk1) (BlockProtocol blk2), CanHardForkBlock blk1 blk2)
  => BrokenBlock (Forked blk1 blk2)

prop_broken :: Property
prop_broken = prop_broken' pInfo
  where
    prop_broken'
      :: BrokenBlock blk
      => ProtocolInfo blk
      -> Property
    prop_broken' p = traceShow (protocolSecurityParam (pInfoConfig p)) undefined
    pInfo :: ProtocolInfo SimpleForkedBlock
    pInfo =
      protocolInfo
        (NumCoreNodes 1)
        (CoreNodeId 0)
        (ProtocolMockHardFork defaultDemoPBftParams defaultDemoPraosParams)

prop_simple_hard_fork_convergence
  :: SecurityParam
  -> NumCoreNodes
  -> NumSlots
  -> Seed
  -> Property
prop_simple_hard_fork_convergence k numCoreNodes numSlots seed =
  prop_general k
    (roundRobinLeaderSchedule numCoreNodes numSlots)
    testOutput
  where
    testOutput =
      runTestNetwork
        ( \nid ->
          protocolInfo
            numCoreNodes
            nid
            (ProtocolMockHardFork defaultDemoPBftParams defaultDemoPraosParams)
        )
        numCoreNodes
        numSlots
        seed

-- prop_simple_hard_fork_convergence
--   :: SecurityParam
--   -> NumCoreNodes
--   -> NumSlots
--   -> Seed
--   -> Property
-- prop_simple_hard_fork_convergence sp numCoreNodes@(NumCoreNodes _nn) _ =
--   prop_simple_protocol_convergence
--     ( \nid ->
--       protocolInfo
--         numCoreNodes
--         nid
--         (ProtocolMockHardFork defaultDemoPBftParams defaultDemoPraosParams)
--     )
--     isValid
--     numCoreNodes
--     15
--   where
--     -- TODO: Include some tests for a fork being valid and the tests for praos being valid
--     -- TODO: Split the Chain in two and reuse the validity tests from the PBFT and Praos tests :)
--     isValid
--       :: [NodeId]
--       -> Map NodeId
--            ( NodeConfig ProtocolMockHardFork
--            , Chain
--                ( Forked
--                    (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
--                    (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)
--                )
--            )
--       -> Property
--     isValid nodeIds final =
--       counterexample (show final') $
--         tabulate "shortestLength" [show (rangeK sp (shortestLength final'))] $
--         Map.keys final ===
--         nodeIds .&&.
--         allEqual (takeChainPrefix <$> Map.elems final')
--       where
--         -- Without the 'NodeConfig's
--         final' = snd <$> final
--         takeChainPrefix
--           :: Chain
--                ( Forked
--                    (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
--                    (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)
--                )
--           -> Chain
--                ( Forked
--                    (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
--                    (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)
--                )
--         takeChainPrefix = id -- in PBFT, chains should indeed all be equal.
