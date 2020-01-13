{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ouroboros.Consensus.Node.Run.Byron () where

import           Data.Coerce (coerce)

import qualified Cardano.Chain.Block as Cardano.Block
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.ProtocolConstants (kEpochSlots)
import           Cardano.Chain.Slotting (EpochSlots (..))
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Network.Block (BlockNo (..), pattern BlockPoint,
                     ChainHash (..), pattern GenesisPoint, SlotNo (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Ledger.Byron
import qualified Ouroboros.Consensus.Ledger.Byron.Auxiliary as Aux
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Util.IOLike

import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.Common (EpochNo (..), EpochSize (..))

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance RunNode ByronBlock where
  nodeForgeBlock            = forgeByronBlock
  nodeBlockMatchesHeader    = verifyBlockMatchesHeader
  nodeBlockFetchSize        = const 2000 -- TODO #593
  nodeIsEBB                 = \hdr -> case byronHeaderRaw hdr of
    Aux.ABOBBlockHdr _       -> Nothing
    Aux.ABOBBoundaryHdr bhdr -> Just
                              . EpochNo
                              . Cardano.Block.boundaryEpoch
                              $ bhdr

  -- The epoch size is fixed and can be derived from @k@ by the ledger
  -- ('kEpochSlots').
  nodeEpochSize             = \_proxy cfg _epochNo -> return
                                . (coerce :: EpochSlots -> EpochSize)
                                . kEpochSlots
                                . Genesis.gdK
                                . extractGenesisData
                                $ cfg

  nodeMaxBlockSize          = Aux.getMaxBlockSize . byronLedgerState
  nodeBlockEncodingOverhead = const byronBlockEncodingOverhead

  -- If the current chain is empty, produce a genesis EBB and add it to the
  -- ChainDB. Only an EBB can have Genesis (= empty chain) as its predecessor.
  nodeInitChainDB cfg chainDB = do
    tip <- atomically $ ChainDB.getTipPoint chainDB
    case tip of
      -- Chain is not empty
      BlockPoint {} -> return ()
      GenesisPoint  -> ChainDB.addBlock chainDB genesisEBB
        where
          genesisEBB = forgeEBB cfg (SlotNo 0) (BlockNo 0) GenesisHash

  -- Extract it from the 'Genesis.Config'
  nodeStartTime             = const
                            $ SystemStart
                            . Genesis.gdStartTime
                            . extractGenesisData
  nodeNetworkMagic          = const
                            $ NetworkMagic
                            . Crypto.unProtocolMagicId
                            . Genesis.gdProtocolMagicId
                            . extractGenesisData
  nodeProtocolMagicId       = const
                            $ Genesis.gdProtocolMagicId
                            . extractGenesisData
  nodeHashInfo              = const byronHashInfo
  nodeCheckIntegrity        = verifyBlockIntegrity
  nodeAddHeaderEnvelope     = const byronAddHeaderEnvelope

  nodeEncodeBlockWithInfo   = const encodeByronBlockWithInfo
  nodeEncodeHeader          = const encodeByronHeader
  nodeEncodeGenTx           = encodeByronGenTx
  nodeEncodeGenTxId         = encodeByronGenTxId
  nodeEncodeHeaderHash      = const encodeByronHeaderHash
  nodeEncodeLedgerState     = const encodeByronLedgerState
  nodeEncodeChainState      = \_proxy _cfg -> encodeByronChainState
  nodeEncodeApplyTxError    = const encodeByronApplyTxError

  nodeDecodeBlock           = decodeByronBlock  . extractEpochSlots
  nodeDecodeHeader          = decodeByronHeader . extractEpochSlots
  nodeDecodeGenTx           = decodeByronGenTx
  nodeDecodeGenTxId         = decodeByronGenTxId
  nodeDecodeHeaderHash      = const decodeByronHeaderHash
  nodeDecodeLedgerState     = const decodeByronLedgerState
  nodeDecodeChainState      = \_proxy cfg ->
                                 let k = pbftSecurityParam $ pbftParams cfg
                                 in decodeByronChainState k
  nodeDecodeApplyTxError    = const decodeByronApplyTxError

extractGenesisData :: NodeConfig ByronConsensusProtocol -> Genesis.GenesisData
extractGenesisData = Genesis.configGenesisData . getGenesisConfig

extractEpochSlots :: NodeConfig ByronConsensusProtocol -> EpochSlots
extractEpochSlots = Genesis.configEpochSlots . getGenesisConfig
