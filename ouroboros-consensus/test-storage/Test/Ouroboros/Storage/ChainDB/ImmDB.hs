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

import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim (runSimOrThrow)

import           Control.Tracer (nullTracer)

import qualified Cardano.Chain.Update as Update

import           Ouroboros.Network.Block (SlotNo (..), blockPoint)

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Ledger.Byron (ByronBlock)
import           Ouroboros.Consensus.Ledger.Byron.Forge (forgeGenesisEBB)
import           Ouroboros.Consensus.Node.ProtocolInfo
                     (PBftSignatureThreshold (..), ProtocolInfo (..),
                     protocolInfo)
import           Ouroboros.Consensus.Node.Run (RunNode (..))
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.ChainDB.API (BlockOrHeader (..))
import           Ouroboros.Storage.ChainDB.Impl.ImmDB (ImmDB, ImmDbArgs (..))
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Storage.EpochInfo (newEpochInfo)
import           Ouroboros.Storage.ImmutableDB
                     (ValidationPolicy (ValidateMostRecentEpoch))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (simHasFS)
import           Test.Util.Orphans.IOLike ()

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

tests :: TestTree
tests = testGroup "ImmDB"
    [ testCase "getBlockWithPoint EBB at tip" test_getBlockWithPoint_EBB_at_tip
    ]

test_getBlockWithPoint_EBB_at_tip :: Assertion
test_getBlockWithPoint_EBB_at_tip =
    runSimOrThrow $ withImmDB $ \immDB -> do
      ImmDB.appendBlock immDB ebb
      mbEbb' <- ImmDB.getBlockOrHeaderWithPoint immDB Block (blockPoint ebb)
      return $ mbEbb' @?= Just ebb
  where
    ebb = forgeGenesisEBB testCfg (SlotNo 0)

withImmDB :: IOLike m => (ImmDB m ByronBlock -> m a) -> m a
withImmDB k = do
    immDbFsVar <- uncheckedNewTVarM Mock.empty
    epochInfo  <- newEpochInfo $ nodeEpochSize (Proxy @ByronBlock) testCfg
    bracket (ImmDB.openDB (mkArgs immDbFsVar epochInfo)) ImmDB.closeDB k
  where
    mkArgs immDbFsVar epochInfo = ImmDbArgs
      { immErr            = EH.monadCatch
      , immHasFS          = simHasFS EH.monadCatch immDbFsVar
      , immDecodeHash     = nodeDecodeHeaderHash (Proxy @ByronBlock)
      , immDecodeHeader = nodeDecodeHeader testCfg
      , immDecodeBlock    = nodeDecodeBlock testCfg
      , immEncodeHash     = nodeEncodeHeaderHash (Proxy @ByronBlock)
      , immEncodeBlock    = nodeEncodeBlockWithInfo testCfg
      , immEpochInfo      = epochInfo
      , immHashInfo       = nodeHashInfo (Proxy @ByronBlock)
      , immValidation     = ValidateMostRecentEpoch
      , immIsEBB          = nodeIsEBB
      , immCheckIntegrity = nodeCheckIntegrity testCfg
      , immAddHdrEnv      = nodeAddHeaderEnvelope (Proxy @ByronBlock)
      , immTracer         = nullTracer
      }

testCfg :: NodeConfig (BlockProtocol ByronBlock)
testCfg = pInfoConfig $ protocolInfo prot
  where
    prot = ProtocolRealPBFT
      Dummy.dummyConfig
      (Just (PBftSignatureThreshold 0.5))
      (Update.ProtocolVersion 1 0 0)
      (Update.SoftwareVersion (Update.ApplicationName "Cardano Test") 2)
      Nothing
