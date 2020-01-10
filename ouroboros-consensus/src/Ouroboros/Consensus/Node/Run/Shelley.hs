{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Node.Run.Shelley () where

import           BlockChain (Block (..))
import           Cardano.Binary (fromCBOR, toCBOR)
import           Cardano.Slotting.EpochInfo
import           Data.Functor.Identity (runIdentity)
import           LedgerState (esPp, nesEs)
import           Ouroboros.Consensus.Ledger.Shelley
import           Ouroboros.Consensus.Ledger.Shelley.Block
                     (Header (ShelleyHeader), encodeShelleyBlockWithInfo,
                     shelleyHashInfo)
import           Ouroboros.Consensus.Ledger.Shelley.Ledger
                     (decodeShelleyLedgerState, encodeShelleyLedgerState)
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Protocol.TPraos
import           PParams (_maxBBSize, _maxBHSize)

instance RunNode ShelleyBlock where
  nodeForgeBlock = forgeShelleyBlock
  nodeBlockMatchesHeader
    (ShelleyHeader header _)
    (ShelleyBlock (Block header' _))
      = header == header'
  nodeBlockFetchSize = const 2000 -- TODO
  nodeIsEBB = const Nothing
  nodeMaxBlockSize (ShelleyLedgerState _ _ ls) = fromIntegral $
    (_maxBBSize . esPp $ nesEs ls) + (_maxBHSize . esPp $ nesEs ls)

  nodeBlockEncodingOverhead = const 1 -- Single list tag.
                                      -- Check this isn't altered by the TxWits stuff
  nodeCheckIntegrity = \_ _ -> True

  nodeEpochSize = \_proxy cfg epochNo ->
    pure . runIdentity $
      epochInfoSize
        ( tpraosEpochInfo $
            tpraosParams cfg
        )
        epochNo

  nodeStartTime =
    const $
      sncStartTime
        . tpraosExtraConfig
  nodeNetworkMagic =
    const $
      sncNetworkMagic
        . tpraosExtraConfig
  nodeProtocolMagicId =
    const $
      sncProtocolMagicId
        . tpraosExtraConfig

  nodeHashInfo = const shelleyHashInfo

  nodeEncodeBlockWithInfo = const encodeShelleyBlockWithInfo
  nodeEncodeHeader = const toCBOR
  nodeEncodeGenTx = toCBOR
  nodeEncodeGenTxId = toCBOR
  nodeEncodeHeaderHash = const toCBOR
  nodeEncodeLedgerState = const encodeShelleyLedgerState
  nodeEncodeChainState = \_proxy _cfg -> toCBOR
  nodeEncodeApplyTxError = const toCBOR

  nodeDecodeBlock = const $ const <$> fromCBOR
  nodeDecodeHeader = const $ const <$> fromCBOR
  nodeDecodeGenTx = fromCBOR
  nodeDecodeGenTxId = fromCBOR
  nodeDecodeHeaderHash = const fromCBOR
  nodeDecodeLedgerState = const decodeShelleyLedgerState
  nodeDecodeChainState = \_proxy _cfg -> fromCBOR
  nodeDecodeApplyTxError = const fromCBOR
