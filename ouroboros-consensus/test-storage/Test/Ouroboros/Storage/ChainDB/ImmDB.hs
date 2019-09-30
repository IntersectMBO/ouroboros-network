{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.Ouroboros.Storage.ChainDB.ImmDB
  ( tests
  ) where

import           Data.Proxy (Proxy (..))
import           Data.Reflection (give)

import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim (runSimOrThrow)

import           Control.Tracer (nullTracer)

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update

import           Ouroboros.Network.Block (SlotNo (..), blockHash, blockPoint)

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB, ByronGiven)
import           Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig)
import           Ouroboros.Consensus.Ledger.Byron.Forge (forgeGenesisEBB)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..),
                     PBftSignatureThreshold (..), ProtocolInfo (..),
                     protocolInfo)
import           Ouroboros.Consensus.Node.Run (RunNode (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.ChainDB.Impl.ImmDB (ImmDB, ImmDbArgs (..))
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Storage.EpochInfo (newEpochInfo)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (simHasFS)
import           Ouroboros.Storage.ImmutableDB
                     (ValidationPolicy (ValidateMostRecentEpoch))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy


tests :: TestTree
tests = testGroup "ImmDB"
    [ testCase "getBlockWithPoint EBB at tip" test_getBlockWithPoint_EBB_at_tip
    ]

test_getBlockWithPoint_EBB_at_tip :: Assertion
test_getBlockWithPoint_EBB_at_tip =
    runSimOrThrow $ giveByron $ withImmDB $ \immDB -> do
      ImmDB.appendBlock immDB ebb
      mbEbb' <- ImmDB.getBlockWithPoint immDB (blockPoint ebb)
      return $ mbEbb' @?= Just ebb
  where
    ebb = giveByron $ forgeGenesisEBB testCfg (SlotNo 0)

withImmDB :: forall m blk a.
             ( MonadCatch m
             , MonadST    m
             , MonadSTM   m
             , ByronGiven
             , blk ~ ByronBlockOrEBB ByronConfig
             )
          => (ImmDB m blk -> m a) -> m a
withImmDB k = do
    immDbFsVar <- uncheckedNewTVarM Mock.empty
    epochInfo  <- newEpochInfo $ nodeEpochSize (Proxy @blk) testCfg
    bracket (ImmDB.openDB (mkArgs immDbFsVar epochInfo)) ImmDB.closeDB k
  where
    mkArgs immDbFsVar epochInfo = ImmDbArgs
      { immErr         = EH.monadCatch
      , immHasFS       = simHasFS EH.monadCatch immDbFsVar
      , immDecodeHash  = nodeDecodeHeaderHash (Proxy @blk)
      , immDecodeBlock = nodeDecodeBlock testCfg
      , immEncodeHash  = nodeEncodeHeaderHash (Proxy @blk)
      , immEncodeBlock = nodeEncodeBlock testCfg
      , immEpochInfo   = epochInfo
      , immValidation  = ValidateMostRecentEpoch
      , immIsEBB       = \blk -> if nodeIsEBB blk
                                 then Just (blockHash blk)
                                 else Nothing
      , immTracer      = nullTracer
      }

testCfg :: NodeConfig (BlockProtocol (ByronBlockOrEBB ByronConfig))
testCfg = pInfoConfig $ protocolInfo (NumCoreNodes 1) (CoreNodeId 0) prot
  where
    prot = ProtocolRealPBFT
      Dummy.dummyConfig
      (Just (PBftSignatureThreshold 0.5))
      (Update.ProtocolVersion 1 0 0)
      (Update.SoftwareVersion (Update.ApplicationName "Cardano Test") 2)
      Nothing

giveByron :: forall a. (ByronGiven => a) -> a
giveByron a =
  give (Genesis.gdProtocolMagicId Dummy.dummyGenesisData) $
  give Dummy.dummyEpochSlots a
