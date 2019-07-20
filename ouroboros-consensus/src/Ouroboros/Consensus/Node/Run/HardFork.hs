{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Node.Run.HardFork () where

import           Data.Maybe (catMaybes, fromJust)
import           Data.Proxy (Proxy (..))

import           Ouroboros.Network.Block (fmapChainHash)

import           Ouroboros.Consensus.Ledger.HardFork
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.HardFork

import Debug.Trace

instance
  (RunNode blk1, RunNode blk2, CanHardForkBlock blk1 blk2)
  => RunNode (Forked blk1 blk2) where

  -- TODO: Figure out where the NodeState actually changes...
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
        trace "producing new block!" $
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
            (catMaybes $ maybeAfterFork . unForkedGenTx <$> txs) -- TODO: Do we want to translate transactions across the fork?
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
  nodeEpochSize _ = nodeEpochSize (Proxy @blk1)

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

  -- TODO: Do this properly
  nodeDecodeHeader nodeConfig =
    ForkedHeader . BeforeFork <$>
      nodeDecodeHeader (nodeConfigBeforeFork nodeConfig)

  -- TODO: Do this properly
  nodeDecodeBlock nodeConfig =
    BeforeFork <$> nodeDecodeBlock (nodeConfigBeforeFork nodeConfig)

  -- TODO: Do this properly
  nodeDecodeGenTx = ForkedGenTx . BeforeFork <$> nodeDecodeGenTx @blk1

  -- TODO: Do this properly
  nodeDecodeGenTxId = ForkedGenTxId . BeforeFork <$> nodeDecodeGenTxId @blk1

  -- TODO: Do this properly
  nodeDecodeHeaderHash _ =
    ForkedHeaderHash . BeforeFork <$> nodeDecodeHeaderHash (Proxy @blk1)

  -- TODO: Do this properly
  nodeDecodeLedgerState nodeConfig =
    ForkedLedgerState . BeforeFork <$>
      nodeDecodeLedgerState (nodeConfigBeforeFork nodeConfig)

  -- TODO: Do this properly
  nodeDecodeChainState _ = BeforeFork <$> nodeDecodeChainState (Proxy @blk1)
