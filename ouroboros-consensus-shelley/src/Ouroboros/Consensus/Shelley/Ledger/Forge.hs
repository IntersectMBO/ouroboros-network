{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Forge (
    -- * ForgeState
    TPraosForgeState (..)
    -- * Forging
  , forgeShelleyBlock
  ) where

import           Control.Exception
import           Crypto.Random (MonadRandom)
import           Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as Seq
import           Data.Typeable (typeRep)
import           GHC.Generics (Generic)

import qualified Cardano.Crypto.KES.Class as KES
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block (castHash)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Integrity
import           Ouroboros.Consensus.Shelley.Ledger.Mempool
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.Crypto

{-------------------------------------------------------------------------------
  CanForge
-------------------------------------------------------------------------------}

instance TPraosCrypto c => CanForge (ShelleyBlock c) where
  type ForgeState (ShelleyBlock c) = TPraosForgeState c
  forgeBlock = forgeShelleyBlock

{-------------------------------------------------------------------------------
  ForgeState
-------------------------------------------------------------------------------}

-- | This is the state containing the private key corresponding to the public
-- key that Praos uses to verify headers. That is why we call it
-- 'TPraosForgeState' instead of 'ShelleyForgeState'.
data TPraosForgeState c =
    -- | The online KES key used to sign blocks is available at the given period
    TPraosKeyAvailable !(HotKey c)

    -- | The KES key is being evolved by another thread
    --
    -- Any thread that sees this value should back off and retry.
  | TPraosKeyEvolving

    -- | This node is not a core node, it doesn't have the capability to sign
    -- blocks.
    --
    -- The 'ForgeState' of such a node will always be 'ShelleyNoKey'.
  | TPraosNoKey
  deriving (Generic)

-- We override 'showTypeOf' to make sure to show @c@
instance TPraosCrypto c => NoUnexpectedThunks (TPraosForgeState c) where
  showTypeOf _ = show $ typeRep (Proxy @(TPraosForgeState c))

-- | Get the KES key from the node state, evolve if its KES period doesn't
-- match the given one.
evolveKESKeyIfNecessary
  :: forall m c. (MonadRandom m, TPraosCrypto c)
  => Update m (TPraosForgeState c)
  -> SL.KESPeriod -- ^ Relative KES period (to the start period of the OCert)
  -> m (HotKey c)
evolveKESKeyIfNecessary updateForgeState (SL.KESPeriod kesPeriod) = do
    getOudatedKeyOrCurrentKey >>= \case
      Right currentKey -> return currentKey
      Left outdatedKey -> do
        let newKey = evolveKey outdatedKey
        saveNewKey newKey
        return newKey
  where
    -- | Return either (@Left@) an outdated key with its current period (setting
    -- the node state to 'TPraosKeyEvolving') or (@Right@) a key that's
    -- up-to-date w.r.t. the current KES period (leaving the node state to
    -- 'TPraosKeyAvailable').
    getOudatedKeyOrCurrentKey
      :: m (Either (HotKey c) (HotKey c))
    getOudatedKeyOrCurrentKey = runUpdate updateForgeState $ \case
      TPraosKeyEvolving ->
        -- Another thread is currently evolving the key; wait
        Nothing
      TPraosKeyAvailable hk@(HotKey kesPeriodOfKey _)
        | kesPeriodOfKey < kesPeriod
          -- Must evolve key
        -> return (TPraosKeyEvolving, Left hk)
        | otherwise
        -> return (TPraosKeyAvailable hk, Right hk)
      TPraosNoKey ->
        error "no KES key available"

    -- | Evolve the given key so that its KES period matches @kesPeriod@.
    evolveKey :: HotKey c -> HotKey c
    evolveKey (HotKey oldPeriod outdatedKey) = go outdatedKey oldPeriod kesPeriod
      where
        go !sk c t
          | t < c
          = error "Asked to evolve KES key to old period"
          | c == t
          = HotKey kesPeriod sk
          | otherwise
          = case KES.updateKES () sk c of
              Nothing  -> error "Could not update KES key"
              Just sk' -> go sk' (c + 1) t

    -- | PRECONDITION: we're in the 'TPraosKeyEvolving' node state.
    saveNewKey :: HotKey c -> m ()
    saveNewKey newKey = runUpdate updateForgeState $ \case
      TPraosKeyEvolving -> Just (TPraosKeyAvailable newKey, ())
      _                 -> error "must be in evolving state"

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

forgeShelleyBlock
  :: (MonadRandom m, TPraosCrypto c)
  => TopLevelConfig (ShelleyBlock c)
  -> Update m (ForgeState (ShelleyBlock c))
  -> BlockNo                             -- ^ Current block number
  -> TickedLedgerState (ShelleyBlock c)  -- ^ Current ledger
  -> [GenTx (ShelleyBlock c)]            -- ^ Txs to add in the block
  -> TPraosProof c                       -- ^ Leader proof ('IsLeader')
  -> m (ShelleyBlock c)
forgeShelleyBlock cfg updateForgeState curNo tickedLedger txs isLeader = do
    hotKESKey <-
      evolveKESKeyIfNecessary updateForgeState (SL.KESPeriod kesEvolution)
    let tpraosFields = forgeTPraosFields hotKESKey isLeader mkBhBody
        blk = mkShelleyBlock $ SL.Block (mkHeader tpraosFields) body
    return $ assert (verifyBlockIntegrity tpraosSlotsPerKESPeriod blk) blk

  where
    TPraosProof { tpraosIsCoreNode } = isLeader
    TPraosIsCoreNode { tpraosIsCoreNodeOpCert } = tpraosIsCoreNode

    -- The current KES period
    kesPeriodNat = fromIntegral $ unSlotNo curSlot `div` tpraosSlotsPerKESPeriod
    SL.OCert _ _ (SL.KESPeriod c0) _ = tpraosIsCoreNodeOpCert
    kesEvolution = if kesPeriodNat >= c0 then kesPeriodNat - c0 else 0

    TPraosConfig { tpraosParams = TPraosParams { tpraosSlotsPerKESPeriod } } =
      configConsensus cfg
    curSlot = tickedSlotNo tickedLedger

    body = SL.TxSeq $ Seq.fromList $ (\(ShelleyTx _ tx) -> tx) <$> txs

    mkHeader TPraosFields { tpraosSignature, tpraosToSign } =
      SL.BHeader tpraosToSign tpraosSignature

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
