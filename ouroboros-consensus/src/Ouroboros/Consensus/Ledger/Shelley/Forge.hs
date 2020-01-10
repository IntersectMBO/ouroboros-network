{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Ouroboros.Consensus.Ledger.Shelley.Forge
  ( forgeShelleyBlock,
    shelleyGenesisHash
  )
where

import           BlockChain (BHBody (..), BHeader (..), Block (..),
                     HashHeader (..), TxSeq (..), bbHash)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot
import           Crypto.Random (MonadRandom)
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import qualified Data.Sequence as Seq
import           Keys (KESig (..), pattern VKey, hash)
import           Ouroboros.Consensus.Ledger.Abstract (ledgerTipHash)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Shelley.Block
import           Ouroboros.Consensus.Ledger.Shelley.Config
import           Ouroboros.Consensus.Ledger.Shelley.Mempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Network.Block (ChainHash (..))

-- | Previous hash for the Shelley genesis block.
shelleyGenesisHash :: HashHeader TPraosStandardCrypto
shelleyGenesisHash = HashHeader
  . coerce
  $ hash
    @(HASH TPraosStandardCrypto)
    @ByteString
    "And the lamb lies down on Broadway"


forgeShelleyBlock ::
  ( HasNodeState (TPraos ShelleyNodeConfig TPraosStandardCrypto) m,
    MonadRandom m
  ) =>
  NodeConfig (TPraos ShelleyNodeConfig TPraosStandardCrypto) ->
  SlotNo ->
  BlockNo ->
  ExtLedgerState ShelleyBlock ->
  [GenTx ShelleyBlock] ->
  TPraosProof TPraosStandardCrypto ->
  m ShelleyBlock
forgeShelleyBlock cfg slotNo blockNo extLedger (Seq.fromList -> txs) isLeader = do
  praosPayload <- forgeTPraosFields cfg isLeader mkBhBody
  return . ShelleyBlock $ Block (header praosPayload) body
  where
    body = TxSeq $ (\(ShelleyTx _ tx) -> tx) <$> txs
    header pp = BHeader (tpraosToSign pp) (KESig $ tpraosSignature pp)
    prevHash' = case ledgerTipHash (ledgerState extLedger) of
      GenesisHash -> shelleyGenesisHash
      BlockHash h -> unShelleyHash h
    mkBhBody
      TPraosToSign
        { tptsIssuerVK,
          tptsVrfVK,
          tptsEta,
          tptsLeader,
          tptsOCert
        } =
        BHBody
          { bheaderPrev = prevHash',
            bheaderVk = VKey tptsIssuerVK,
            bheaderVrfVk = tptsVrfVK,
            bheaderSlotNo = slotNo,
            bheaderBlockNo = blockNo,
            bheaderEta = tptsEta,
            bheaderL = tptsLeader,
            bsize = 20000, -- TODO
            bhash = bbHash body,
            bheaderOCert = tptsOCert,
            bprotvert = sncProtocolVersion $ tpraosExtraConfig cfg
          }
