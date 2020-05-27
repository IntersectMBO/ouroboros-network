{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Forge (
    -- * ForgeState
    TPraosForgeState (..)
    -- * Forging
  , forgeShelleyBlock
    -- * Updating the state
  , evolveKESKeyIfNecessary
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
  deriving (Generic)

deriving instance TPraosCrypto c => Show (TPraosForgeState c)

-- We override 'showTypeOf' to make sure to show @c@
instance TPraosCrypto c => NoUnexpectedThunks (TPraosForgeState c) where
  showTypeOf _ = show $ typeRep (Proxy @(TPraosForgeState c))

-- | Get the KES key from the node state, evolve if its KES period doesn't
-- match the given one.
evolveKESKeyIfNecessary
  :: forall m c. (MonadRandom m, TPraosCrypto c)
  => Update m (TPraosForgeState c)
  -> SL.KESPeriod -- ^ Relative KES period (to the start period of the OCert)
  -> m ()
evolveKESKeyIfNecessary updateForgeState (SL.KESPeriod kesPeriod) = do
    getOutdatedKey >>= \case
      Nothing -> return ()
      Just outdatedKey -> do
        let newKey = evolveKey outdatedKey
        saveNewKey newKey
  where
    -- | Return maybe (@Just@) an outdated key with its current period (setting
    -- the node state to 'TPraosKeyEvolving') or (@Nothing@) if the key is
    -- up-to-date w.r.t. the current KES period.
    getOutdatedKey :: m (Maybe (HotKey c))
    getOutdatedKey = runUpdate updateForgeState $ \case
      TPraosKeyEvolving ->
        -- Another thread is currently evolving the key; wait
        Nothing
      TPraosKeyAvailable hk@(HotKey kesPeriodOfKey _)
        | kesPeriodOfKey < kesPeriod
          -- Must evolve key
        -> return (TPraosKeyEvolving, Just hk)
        | otherwise
        -> return (TPraosKeyAvailable hk, Nothing)

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
  -> ForgeState (ShelleyBlock c)
  -> BlockNo                             -- ^ Current block number
  -> TickedLedgerState (ShelleyBlock c)  -- ^ Current ledger
  -> [GenTx (ShelleyBlock c)]            -- ^ Txs to add in the block
  -> TPraosProof c                       -- ^ Leader proof ('IsLeader')
  -> m (ShelleyBlock c)
forgeShelleyBlock cfg forgeState curNo tickedLedger txs isLeader = do
    return $ assert (verifyBlockIntegrity tpraosSlotsPerKESPeriod blk) blk

  where
    TPraosConfig { tpraosParams = TPraosParams { tpraosSlotsPerKESPeriod } } =
      configConsensus cfg

    -- TODO: If we use a StrictMVar for the key, can get rid of 'error'

    curSlot      = tickedSlotNo tickedLedger
    tpraosFields = forgeTPraosFields hotKESKey isLeader mkBhBody
    blk          = mkShelleyBlock $ SL.Block (mkHeader tpraosFields) body
    hotKESKey    = case forgeState of
                     TPraosKeyAvailable key -> key
                     _otherwise             -> error "forgeShelley: no key"
    body         = SL.TxSeq $ Seq.fromList $ (\(ShelleyTx _ tx) -> tx) <$> txs

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
