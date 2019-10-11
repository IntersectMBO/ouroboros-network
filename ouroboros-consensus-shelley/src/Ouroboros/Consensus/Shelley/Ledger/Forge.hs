{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
module Ouroboros.Consensus.Shelley.Ledger.Forge (
    forgeShelleyBlock
  ) where

import           Crypto.Random (MonadRandom)
import qualified Data.Sequence as Seq

import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block (castHash)

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract (ledgerTipHash)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.State (NodeState)
import qualified Ouroboros.Consensus.Node.State as NodeState

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Mempool
import           Ouroboros.Consensus.Shelley.Protocol

forgeShelleyBlock
  :: (MonadRandom m, TPraosCrypto c)
  => TopLevelConfig (ShelleyBlock c)
  -> NodeState.Update m (NodeState (ShelleyBlock c))
  -> SlotNo                           -- ^ Current slot
  -> BlockNo                          -- ^ Current block number
  -> ExtLedgerState (ShelleyBlock c)  -- ^ Ledger
  -> [GenTx (ShelleyBlock c)]         -- ^ Txs to add in the block
  -> TPraosProof c                    -- ^ Leader proof ('IsLeader')
  -> m (ShelleyBlock c)
forgeShelleyBlock cfg updateNodeState curSlot curNo extLedger txs isLeader = do
    tpraosFields <-
      forgeTPraosFields updateNodeState isLeader kesPeriod mkBhBody
    return $ mkShelleyBlock $ SL.Block (mkHeader tpraosFields) body
  where
    TPraosConfig { tpraosParams } = configConsensus cfg

    -- The current KES period
    kesPeriod :: SL.KESPeriod
    kesPeriod = SL.KESPeriod $ fromIntegral $
      unSlotNo curSlot `div` tpraosSlotsPerKESPeriod tpraosParams

    body = SL.TxSeq $ Seq.fromList $ (\(ShelleyTx _ tx) -> tx) <$> txs

    mkHeader TPraosFields { tpraosSignature, tpraosToSign } =
      SL.BHeader tpraosToSign (SL.KESig tpraosSignature)

    prevHash =
        toShelleyPrevHash
      . castHash
      . ledgerTipHash
      . ledgerState
      $ extLedger

    mkBhBody toSign = SL.BHBody {
          bheaderPrev    = prevHash
        , bheaderVk      = SL.VKey tpraosToSignIssuerVK
        , bheaderVrfVk   = tpraosToSignVrfVK
        , bheaderSlotNo  = curSlot
        , bheaderBlockNo = curNo
        , bheaderEta     = tpraosToSignEta
        , bheaderL       = tpraosToSignLeader
        , bsize          = fromIntegral $ SL.bBodySize body
        , bhash          = SL.bbHash body
        , bheaderOCert   = tpraosToSignOCert
        , bprotver       = shelleyProtocolVersion $ configBlock cfg
        }
      where
        TPraosToSign {
            tpraosToSignIssuerVK
          , tpraosToSignVrfVK
          , tpraosToSignEta
          , tpraosToSignLeader
          , tpraosToSignOCert
          } = toSign
