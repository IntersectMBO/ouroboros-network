{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Node.Run.DualByron () where

import           Cardano.Chain.Slotting (EpochSlots)
import           Data.Proxy

import           Ouroboros.Network.Block (BlockNo (..), pattern BlockPoint,
                     ChainHash (..), pattern GenesisPoint, SlotNo (..))

import qualified Ouroboros.Storage.ChainDB as ChainDB

import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Dual
import           Ouroboros.Consensus.Ledger.Dual.Byron
import           Ouroboros.Consensus.Node.Run.Abstract
import qualified Ouroboros.Consensus.Node.Run.Byron as Byron
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtConfig
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Util.IOLike

pb :: Proxy ByronBlock
pb = Proxy

instance RunNode DualByronBlock where
  nodeForgeBlock = forgeDualByronBlock

  -- Just like Byron, we need to start with an EBB
  nodeInitChainDB cfg chainDB = do
      tip <- atomically $ ChainDB.getTipPoint chainDB
      case tip of
        BlockPoint {} -> return () -- Chain is not empty
        GenesisPoint  -> ChainDB.addBlock chainDB genesisEBB
          where
            genesisEBB :: DualByronBlock
            genesisEBB = DualBlock {
                  dualBlockMain   = byronEBB
                , dualBlockAux    = Nothing
                , dualBlockBridge = mempty
                }

            byronEBB :: ByronBlock
            byronEBB = forgeEBB
                         (extNodeConfigP cfg)
                         (SlotNo 0)
                         (BlockNo 0)
                         GenesisHash

  -- Node config is a consensus concern, determined by the main block only
  nodeEpochSize       = \_p -> nodeEpochSize       pb . extNodeConfigP
  nodeStartTime       = \_p -> nodeStartTime       pb . extNodeConfigP
  nodeNetworkMagic    = \_p -> nodeNetworkMagic    pb . extNodeConfigP
  nodeProtocolMagicId = \_p -> nodeProtocolMagicId pb . extNodeConfigP

  -- The max block size we set to the max block size of the /concrete/ block
  -- (Correspondingly, 'txSize' for the Byron spec returns 0)
  nodeMaxBlockSize          = nodeMaxBlockSize          . dualLedgerStateMain
  nodeBlockEncodingOverhead = nodeBlockEncodingOverhead . dualLedgerStateMain

  -- The hash we use is the hash of the concrete block
  nodeHashInfo = \_p -> nodeHashInfo pb

  -- We can look at the concrete header to see if this is an EBB
  nodeIsEBB = nodeIsEBB . dualHeaderMain

  -- For now the size of the block is just an estimate, and so we just reuse
  -- the estimate from the concrete header.
  nodeBlockFetchSize = nodeBlockFetchSize . dualHeaderMain

  -- We don't really care too much about data loss or malicious behaviour for
  -- the dual ledger tests, so integrity and match checks can just use the
  -- concrete implementation
  nodeBlockMatchesHeader hdr = nodeBlockMatchesHeader (dualHeaderMain hdr) . dualBlockMain
  nodeCheckIntegrity     cfg = nodeCheckIntegrity     (extNodeConfigP cfg) . dualBlockMain

  -- The header is just the concrete header, so we can just reuse the Byron def
  nodeAddHeaderEnvelope = \_ -> nodeAddHeaderEnvelope pb

  -- Encoders
  nodeEncodeBlockWithInfo = const $ encodeDualBlockWithInfo encodeByronBlockWithInfo
  nodeEncodeHeader        = const $ encodeDualHeader        encodeByronHeader
  nodeEncodeLedgerState   = const $ encodeDualLedgerState   encodeByronLedgerState
  nodeEncodeApplyTxError  = const $ encodeDualGenTxErr      encodeByronApplyTxError
  nodeEncodeHeaderHash    = const $ encodeByronHeaderHash
  nodeEncodeGenTx         = encodeDualGenTx   encodeByronGenTx
  nodeEncodeGenTxId       = encodeDualGenTxId encodeByronGenTxId
  nodeEncodeChainState    = \_proxy _cfg -> encodeByronChainState
  nodeEncodeQuery         = \case {}
  nodeEncodeResult        = \case {}

  -- Decoders
  nodeDecodeBlock         = decodeDualBlock  . decodeByronBlock  . extractEpochSlots
  nodeDecodeHeader        = decodeDualHeader . decodeByronHeader . extractEpochSlots
  nodeDecodeGenTx         = decodeDualGenTx   decodeByronGenTx
  nodeDecodeGenTxId       = decodeDualGenTxId decodeByronGenTxId
  nodeDecodeHeaderHash    = const $ decodeByronHeaderHash
  nodeDecodeLedgerState   = const $ decodeDualLedgerState decodeByronLedgerState
  nodeDecodeApplyTxError  = const $ decodeDualGenTxErr    decodeByronApplyTxError
  nodeDecodeChainState    = \_proxy cfg ->
                               let k = pbftSecurityParam $
                                         pbftParams (extNodeConfigP cfg)
                               in decodeByronChainState k
  nodeDecodeQuery         = error "DualByron.nodeDecodeQuery"
  nodeDecodeResult        = \case {}

extractEpochSlots :: NodeConfig DualByronProtocol -> EpochSlots
extractEpochSlots = Byron.extractEpochSlots . extNodeConfigP
