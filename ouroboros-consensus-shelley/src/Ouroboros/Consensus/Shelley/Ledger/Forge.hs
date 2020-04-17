{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
module Ouroboros.Consensus.Shelley.Ledger.Forge (
    forgeShelleyBlock
  ) where

import           Control.Exception
import           Crypto.Random (MonadRandom)
import qualified Data.Sequence.Strict as Seq

import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block (castHash)

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Node.State (NodeState)
import qualified Ouroboros.Consensus.Node.State as NodeState

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Integrity
import           Ouroboros.Consensus.Shelley.Ledger.Mempool
import           Ouroboros.Consensus.Shelley.Protocol

forgeShelleyBlock
  :: (MonadRandom m, TPraosCrypto c)
  => TopLevelConfig (ShelleyBlock c)
  -> NodeState.Update m (NodeState (ShelleyBlock c))
  -> BlockNo                             -- ^ Current block number
  -> TickedLedgerState (ShelleyBlock c)  -- ^ Current ledger
  -> [GenTx (ShelleyBlock c)]            -- ^ Txs to add in the block
  -> TPraosProof c                       -- ^ Leader proof ('IsLeader')
  -> m (ShelleyBlock c)
forgeShelleyBlock cfg updateNodeState curNo tickedLedger txs isLeader = do
    tpraosFields <-
      forgeTPraosFields updateNodeState isLeader kesPeriod mkBhBody
    let blk = mkShelleyBlock $ SL.Block (mkHeader tpraosFields) body
    return $ assert (verifyBlockIntegrity tpraosSlotsPerKESPeriod blk) blk

  where
    TPraosConfig { tpraosParams = TPraosParams { tpraosSlotsPerKESPeriod } } =
      configConsensus cfg
    curSlot = tickedSlotNo tickedLedger

    -- The current KES period
    kesPeriod :: SL.KESPeriod
    kesPeriod = SL.KESPeriod $ fromIntegral $
      unSlotNo curSlot `div` tpraosSlotsPerKESPeriod

    body = SL.TxSeq $ Seq.fromList $ (\(ShelleyTx _ tx) -> tx) <$> txs

    mkHeader TPraosFields { tpraosSignature, tpraosToSign } =
      SL.BHeader tpraosToSign (SL.KESig tpraosSignature)

    prevHash =
        toShelleyPrevHash
      . castHash
      . ledgerTipHash
      . tickedLedgerState
      $ tickedLedger

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
