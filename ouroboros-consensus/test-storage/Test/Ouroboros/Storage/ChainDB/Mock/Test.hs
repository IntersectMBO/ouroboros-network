{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.ChainDB.Mock.Test (tests) where

import           Control.Exception (Exception)
import           Control.Monad
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim

import           Ouroboros.Network.MockChain.Chain (Chain (..), ChainUpdate)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock

import qualified Test.Ouroboros.Storage.ChainDB.Mock as Mock
import           Test.Ouroboros.Storage.ChainDB.Model (SupportedBlock (..))

tests :: TestTree
tests = testGroup "Mock" [
      testProperty "chainRoundtrip" prop_chainRoundtrip
    , testProperty "reader"         prop_reader
    ]

prop_chainRoundtrip :: BlockChain -> Property
prop_chainRoundtrip bc = runSimOrThrow test
  where
    test :: forall s. SimM s Property
    test = do
        db <- ChainDB.fromChain openDB (blockChain bc)
        c' <- ChainDB.toChain db
        return $ blockChain bc === c'

prop_reader :: BlockTree -> Permutation -> Property
prop_reader bt p = runSimOrThrow test
  where
    blocks = permute p $ treeToBlocks bt

    test :: forall s. SimM s Property
    test = withRegistry $ \registry -> do
        db       <- openDB
        reader   <- ChainDB.newBlockReader db registry
        chainVar <- uncheckedNewTVarM Genesis

        -- Fork a thread that applies all instructions from the reader
        _tid <- fork $ monitorReader chainVar reader

        -- Feed all blocks to the chain DB
        mapM_ (ChainDB.addBlock db) blocks

        -- Give reader chance to finish
        threadDelay 1000000

        -- Reconstructed chain should be equal to the current chain
        current       <- ChainDB.toChain db
        reconstructed <- atomically $ readTVar chainVar
        return $ current === reconstructed

    monitorReader :: StrictTVar (SimM s) (Chain TestBlock)
                  -> ChainDB.Reader (SimM s) TestBlock TestBlock
                  -> SimM s ()
    monitorReader chainVar reader = forever $ do
        upd <- ChainDB.readerInstructionBlocking reader
        atomically $ do
          chain <- readTVar chainVar
          case Chain.applyChainUpdate upd chain of
            Just chain' -> writeTVar chainVar chain'
            Nothing     -> throwM $ InvalidUpdate chain upd

data InvalidUpdate = InvalidUpdate (Chain TestBlock) (ChainUpdate TestBlock TestBlock)
  deriving (Show)

instance Exception InvalidUpdate

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

openDB :: forall s. SimM s (ChainDB (SimM s) TestBlock)
openDB = Mock.openDB
    singleNodeTestConfig
    testInitExtLedger
    -- We don't care about time or future here
    (fixedBlockchainTime maxBound)

{-------------------------------------------------------------------------------
  Orphan instances
-------------------------------------------------------------------------------}

instance SupportedBlock TestBlock where
  isEBB = const IsNotEBB
