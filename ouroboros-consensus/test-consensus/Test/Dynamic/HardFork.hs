module Test.Dynamic.HardFork
  ( tests
  )
where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.MockChain.Chain (Chain)

import           Test.Dynamic.General
import           Test.Dynamic.Util

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Range

tests :: TestTree
tests =
  testGroup "Dynamic chain generation"
    [ testProperty "simple PBFT/Praos Hard Fork convergence" $
        withMaxSuccess 1 $ prop_simple_hard_fork_convergence sp
    ]
  where
    sp = defaultSecurityParam

prop_simple_hard_fork_convergence
  :: SecurityParam
  -> NumCoreNodes
  -> NumSlots
  -> Seed
  -> Property
prop_simple_hard_fork_convergence sp numCoreNodes@(NumCoreNodes _nn) _ =
  prop_simple_protocol_convergence
    ( \nid ->
      protocolInfo
        numCoreNodes
        nid
        (ProtocolMockHardFork defaultDemoPBftParams defaultDemoPraosParams)
    )
    isValid
    numCoreNodes
    15
  where
    -- TODO: Include some tests for a fork being valid and the tests for praos being valid
    -- TODO: Split the Chain in two and reuse the validity tests from the PBFT and Praos tests :)
    isValid
      :: [NodeId]
      -> Map NodeId
           ( NodeConfig ProtocolMockHardFork
           , Chain
               ( Forked
                   (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
                   (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)
               )
           )
      -> Property
    isValid nodeIds final =
      counterexample (show final') $
        tabulate "shortestLength" [show (rangeK sp (shortestLength final'))] $
        Map.keys final ===
        nodeIds .&&.
        allEqual (takeChainPrefix <$> Map.elems final')
      where
        -- Without the 'NodeConfig's
        final' = snd <$> final
        takeChainPrefix
          :: Chain
               ( Forked
                   (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
                   (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)
               )
          -> Chain
               ( Forked
                   (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
                   (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)
               )
        takeChainPrefix = id -- in PBFT, chains should indeed all be equal.
