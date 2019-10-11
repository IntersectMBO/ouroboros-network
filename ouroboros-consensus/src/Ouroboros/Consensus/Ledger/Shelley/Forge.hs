{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeFamilies     #-}

module Ouroboros.Consensus.Ledger.Shelley.Forge
  ( forgeShelleyBlock
  )
where

import           BlockChain (BHBody (..), BHeader (..), Block (..), TxSeq (..),
                     bbHash)
import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot
import           Crypto.Random (MonadRandom)
import           Data.Sequence (Seq)
import           Keys (KESig (..), pattern VKey)
import           Ouroboros.Consensus.Ledger.Shelley.Block
import           Ouroboros.Consensus.Ledger.Shelley.Config
import           Ouroboros.Consensus.Ledger.Shelley.Mempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.TPraos

forgeShelleyBlock ::
  ( HasNodeState (TPraos ShelleyNodeConfig TPraosStandardCrypto) m,
    MonadRandom m
  ) =>
  NodeConfig (TPraos ShelleyNodeConfig TPraosStandardCrypto) ->
  SlotNo ->
  BlockNo ->
  ShelleyHash ->
  Seq (GenTx ShelleyBlock) ->
  TPraosProof TPraosStandardCrypto ->
  m ShelleyBlock
forgeShelleyBlock cfg slotNo blockNo prevHash txs isLeader = do
  praosPayload <- forgeTPraosFields cfg isLeader mkBhBody
  return . ShelleyBlock $ Block (header praosPayload) body
  where
    body = TxSeq $ (\(ShelleyTx _ tx) -> tx) <$> txs
    header pp = BHeader (tpraosToSign pp) (KESig $ tpraosSignature pp)
    mkBhBody
      TPraosToSign
        { tptsIssuerVK,
          tptsVrfVK,
          tptsEta,
          tptsLeader,
          tptsOCert
        } =
        BHBody
          { bheaderPrev = unShelleyHash prevHash,
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
