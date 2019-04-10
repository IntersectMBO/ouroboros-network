{-# LANGUAGE RankNTypes #-}

module Test.Ouroboros.Storage.ChainDB.Mock (tests) where

import           Control.Exception (Exception)
import           Control.Monad
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim

import           Ouroboros.Network.Chain (Chain (..), ChainUpdate)
import qualified Ouroboros.Network.Chain as Chain

import           Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Storage.ChainDB.Mock as Mock

import           Test.Ouroboros.Storage.ChainDB.TestBlock

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
    blocks = permutation p $ treeToBlocks bt

    test :: forall s. SimM s Property
    test = do
        db       <- openDB
        reader   <- ChainDB.newReader db
        chainVar <- atomically $ newTVar Genesis

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

    monitorReader :: TVar (SimM s) (Chain TestBlock)
                  -> ChainDB.Reader (SimM s) TestBlock
                  -> SimM s ()
    monitorReader chainVar reader = forever $ do
        upd <- ChainDB.readerInstructionBlocking reader
        atomically $ do
          chain <- readTVar chainVar
          case Chain.applyChainUpdate upd chain of
            Just chain' -> writeTVar chainVar chain'
            Nothing     -> throwM $ InvalidUpdate chain upd

data InvalidUpdate = InvalidUpdate (Chain TestBlock) (ChainUpdate TestBlock)
  deriving (Show)

instance Exception InvalidUpdate

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

openDB :: forall s. SimM s (ChainDB (SimM s) TestBlock TestBlock)
openDB = Mock.openDB testConfig testInitExtLedger id
