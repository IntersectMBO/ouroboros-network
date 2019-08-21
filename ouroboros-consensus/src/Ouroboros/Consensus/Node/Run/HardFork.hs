{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Node.Run.HardFork () where

import           Data.Maybe (catMaybes, fromJust)
import           Data.Proxy (Proxy (..))
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))

import           Ouroboros.Network.Block (fmapChainHash)

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.HardFork
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.HardFork

instance
  ( RunNode blk1
  , RunNode blk2
  , CanHardFork (BlockProtocol blk1) (BlockProtocol blk2)
  , CanHardForkBlock blk1 blk2
  )
  => RunNode (Forked blk1 blk2) where

  nodeForgeBlock nodeConfig slotNo blockNo chainHash txs isLeader =
    getNodeState >>= \case
      BeforeFork _ ->
        fmap (BeforeFork . fromJust) . runBeforeMaybeT $
          nodeForgeBlock
            (nodeConfigBeforeFork nodeConfig)
            slotNo
            blockNo
            (fmapChainHash (unsafeBeforeFork . unForkedHeaderHash) chainHash)
            (unsafeBeforeFork . unForkedGenTx <$> txs)
            (unsafeBeforeFork isLeader)
      AfterFork _ ->
        fmap (AfterFork . fromJust) .
        runAfterMaybeT $
        nodeForgeBlock
          (nodeConfigAfterFork nodeConfig)
          slotNo
          blockNo
          ( fmapChainHash
            (forked (translateHeaderHash @blk1 @blk2) id . unForkedHeaderHash)
            chainHash
          )
          -- TODO: Do we want to translate transactions across the fork?
          (catMaybes $ maybeAfterFork . unForkedGenTx <$> txs)
          (unsafeAfterFork isLeader)
    where
      maybeAfterFork :: Forked a b -> Maybe b
      maybeAfterFork (BeforeFork _) = Nothing
      maybeAfterFork (AfterFork b) = Just b

  nodeBlockMatchesHeader forkedHeader forkedBlock =
    case (unForkedHeader forkedHeader, forkedBlock) of
      (BeforeFork header, BeforeFork block) ->
        nodeBlockMatchesHeader header block
      (AfterFork header, AfterFork block) -> nodeBlockMatchesHeader header block
      _ -> False

  nodeBlockFetchSize =
    forked nodeBlockFetchSize nodeBlockFetchSize . unForkedHeader

  nodeIsEBB = forked nodeIsEBB nodeIsEBB

  -- TODO: Define this properly - need some sort of stateful thing
  nodeEpochSize _ nodeConfig =
    nodeEpochSize (Proxy @blk2) (nodeConfigAfterFork nodeConfig)

  nodeStartTime          = \_ _ -> SystemStart dummyDate
    where
      --  This doesn't matter much
      dummyDate = UTCTime (fromGregorian 2019 8 13) 0

  nodeEncodeBlock ForkedNodeConfig {nodeConfigBeforeFork, nodeConfigAfterFork} =
    forked
      (nodeEncodeBlock nodeConfigBeforeFork)
      (nodeEncodeBlock nodeConfigAfterFork)

  nodeEncodeHeader ForkedNodeConfig {nodeConfigBeforeFork, nodeConfigAfterFork} =
    forked
      (nodeEncodeHeader nodeConfigBeforeFork)
      (nodeEncodeHeader nodeConfigAfterFork) .
      unForkedHeader

  nodeEncodeGenTx = forked nodeEncodeGenTx nodeEncodeGenTx . unForkedGenTx

  nodeEncodeGenTxId =
    forked nodeEncodeGenTxId nodeEncodeGenTxId . unForkedGenTxId

  nodeEncodeHeaderHash _ =
    forked
      (nodeEncodeHeaderHash (Proxy @blk1))
      (nodeEncodeHeaderHash (Proxy @blk2)) .
      unForkedHeaderHash

  nodeEncodeLedgerState ForkedNodeConfig {nodeConfigBeforeFork, nodeConfigAfterFork} =
    forked
      (nodeEncodeLedgerState nodeConfigBeforeFork)
      (nodeEncodeLedgerState nodeConfigAfterFork) .
      unForkedLedgerState

  nodeEncodeChainState _ =
    forked
      (nodeEncodeChainState (Proxy @blk1))
      (encodeAfterForkChainState
        (nodeEncodeChainState (Proxy @blk1))
        (nodeEncodeChainState (Proxy @blk2))
      )

  nodeEncodeApplyTxError _ =
    encodeForkedApplyTxErr
      (nodeEncodeApplyTxError (Proxy @blk1))
      (nodeEncodeApplyTxError (Proxy @blk2))

  -- TODO: Do this properly
  nodeDecodeHeader nodeConfig =
    (.) (ForkedHeader . AfterFork) <$>
      nodeDecodeHeader (nodeConfigAfterFork nodeConfig)

  -- TODO: Do this properly
  nodeDecodeBlock nodeConfig =
    (.) AfterFork <$> nodeDecodeBlock (nodeConfigAfterFork nodeConfig)

  -- TODO: Do this properly
  nodeDecodeGenTx = ForkedGenTx . AfterFork <$> nodeDecodeGenTx @blk2

  -- TODO: Do this properly
  nodeDecodeGenTxId = ForkedGenTxId . AfterFork <$> nodeDecodeGenTxId @blk2

  -- TODO: Do this properly
  nodeDecodeHeaderHash _ =
    ForkedHeaderHash . AfterFork <$> nodeDecodeHeaderHash (Proxy @blk2)

  -- TODO: Do this properly
  nodeDecodeLedgerState nodeConfig =
    ForkedLedgerState . AfterFork <$>
      nodeDecodeLedgerState (nodeConfigAfterFork nodeConfig)

  -- TODO: Do this properly
  nodeDecodeChainState _ =
    AfterFork <$>
      decodeAfterForkChainState
        (nodeDecodeChainState (Proxy @blk1))
        (nodeDecodeChainState (Proxy @blk2))

  -- TODO: Do this properly
  nodeDecodeApplyTxError _ =
    AfterForkApplyTxErr <$> nodeDecodeApplyTxError (Proxy @blk2)
