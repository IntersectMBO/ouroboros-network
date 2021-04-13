{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Cardano (CardanoTxGenExtra (..)) where

import           Control.Exception (assert)

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.SOP.Strict
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

import           Ouroboros.Consensus.Block (SlotNo (..))
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
                     (tickedHardForkLedgerStatePerEra)
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
                     (currentState, getHardForkState)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Tele
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig, LedgerState,
                     applyChainTick)
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))

import           Cardano.Crypto (toVerification)
import qualified Cardano.Crypto.Signing as Byron

import qualified Cardano.Chain.Common as Byron
import           Cardano.Chain.Genesis (GeneratedSecrets (..))

import qualified Cardano.Ledger.Hashes as SL
import qualified Cardano.Ledger.SafeHash as SL
import           Cardano.Ledger.Val ((<->))
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.Address as SL (BootstrapAddress (..))
import qualified Shelley.Spec.Ledger.Address.Bootstrap as SL
                     (makeBootstrapWitness)
import qualified Shelley.Spec.Ledger.BaseTypes as SL (truncateUnitInterval)
import qualified Shelley.Spec.Ledger.Tx as SL (WitnessSetHKD (..))
import qualified Shelley.Spec.Ledger.UTxO as SL (makeWitnessVKey)

import           Ouroboros.Consensus.Shelley.Ledger (GenTx, ShelleyBlock,
                     mkShelleyTx)
import           Ouroboros.Consensus.Shelley.Ledger.Ledger (Ticked,
                     tickedShelleyLedgerState)

import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Cardano.Block (CardanoEras, GenTx (..),
                     ShelleyEra)
import           Ouroboros.Consensus.Cardano.Node (CardanoHardForkConstraints)

import qualified Test.ThreadNet.Infra.Shelley as Shelley
import           Test.ThreadNet.TxGen

data CardanoTxGenExtra c = CardanoTxGenExtra
  { ctgeByronGenesisKeys :: GeneratedSecrets
  , ctgeNetworkMagic     :: Byron.NetworkMagic
  , ctgeShelleyCoreNodes :: [Shelley.CoreNode c]
  }

instance CardanoHardForkConstraints c => TxGen (CardanoBlock c) where

  type TxGenExtra (CardanoBlock c) = CardanoTxGenExtra c

  -- TODO also generate " typical " Byron and Shelley transactions
  testGenTxs (CoreNodeId i) _ncn curSlot cfg extra ls =
      pure $ maybeToList $ migrateUTxO migrationInfo curSlot lcfg ls
    where
      lcfg = topLevelConfigLedger cfg

      CardanoTxGenExtra
        { ctgeByronGenesisKeys
        , ctgeNetworkMagic
        , ctgeShelleyCoreNodes
        } = extra

      GeneratedSecrets
        { gsRichSecrets
        } = ctgeByronGenesisKeys

      migrationInfo = MigrationInfo
        { byronMagic = ctgeNetworkMagic
        , byronSK
        , paymentSK
        , poolSK
        , stakingSK
        , vrfSK
        }

      byronSK :: Byron.SigningKey
      byronSK = gsRichSecrets !! fromIntegral i

      Shelley.CoreNode
        { Shelley.cnDelegateKey = paymentSK
        , Shelley.cnStakingKey  = stakingSK
        , Shelley.cnVRF         = vrfSK
        } = ctgeShelleyCoreNodes !! fromIntegral i

      -- Reuse the payment key as the pool key, since it's an individual
      -- stake pool and the namespaces are separate.
      poolSK :: SL.SignKeyDSIGN c
      poolSK = paymentSK

-- | See 'migrateUTxO'
data MigrationInfo c = MigrationInfo
  { byronMagic :: Byron.NetworkMagic
    -- ^ Needed for creating a Byron address.
  , byronSK    :: Byron.SigningKey
    -- ^ The core node's Byron secret.
  , paymentSK  :: SL.SignKeyDSIGN c
  , poolSK     :: SL.SignKeyDSIGN c
  , stakingSK  :: SL.SignKeyDSIGN c
  , vrfSK      :: SL.SignKeyVRF   c
    -- ^ To be re-used by the individual pool.
  }

-- | Convert a core node's utxo from Byron to an active Shelley stake pool.
--
-- Returns a transaction that registers a staking key, registers an individual
-- stake pool, delegates that stake key to that stake pool, and transfers all
-- utxo from the Byron 'byronAddr' to the Shelley address corresponding to the
-- pair of 'paymentSK' and 'stakingSK'.
--
-- It returns 'Nothing' if the core node does not have any utxo in its
-- 'byronAddr' (eg if this transaction has already been applied).
migrateUTxO ::
     forall c. CardanoHardForkConstraints c
  => MigrationInfo c
  -> SlotNo
  -> LedgerConfig (CardanoBlock c)
  -> LedgerState (CardanoBlock c)
  -> Maybe (GenTx (CardanoBlock c))
migrateUTxO migrationInfo curSlot lcfg lst
    | Just utxo <- mbUTxO =

    let picked :: Map (SL.TxIn c) (SL.TxOut (ShelleyEra c))
        picked =
            Map.filter pick $ SL.unUTxO utxo
          where
            pick (SL.TxOut addr _) =
                addr == SL.AddrBootstrap (SL.BootstrapAddress byronAddr)

        -- Total held by 'byronAddr'
        pickedCoin :: SL.Coin
        pickedCoin = foldMap (\(SL.TxOut _ coin) -> coin) picked

        -- NOTE: The Cardano ThreadNet tests use the
        -- ouroboros-consensus-shelley-test infra's genesis config, which sets
        -- relevant protocol params to 0.
        fee, deposits, spentCoin :: SL.Coin
        fee       = SL.Coin  0
        deposits  = SL.Coin 0
        spentCoin = deposits <> fee

        unspentCoin :: SL.Coin
        unspentCoin =
            assert (pickedCoin > spentCoin) $
            pickedCoin <-> spentCoin

        body :: SL.TxBody (ShelleyEra c)
        body = SL.TxBody
          { SL._certs    = StrictSeq.fromList $
              [ SL.DCertDeleg $ SL.RegKey $ Shelley.mkCredential stakingSK
              , SL.DCertPool  $ SL.RegPool $ poolParams unspentCoin
              , SL.DCertDeleg $ SL.Delegate $ SL.Delegation
                  { SL._delegator = Shelley.mkCredential stakingSK
                  , SL._delegatee = Shelley.mkKeyHash poolSK
                  }
              ]
          , SL._inputs   = Map.keysSet picked
          , SL._mdHash   = SL.SNothing
          , SL._outputs  =
              StrictSeq.singleton $ SL.TxOut shelleyAddr unspentCoin
          , SL._ttl      = SlotNo maxBound
          , SL._txUpdate = SL.SNothing
          , SL._txfee    = fee
          , SL._wdrls    = SL.Wdrl Map.empty
          }

        bodyHash :: SL.SafeHash c SL.EraIndependentTxBody
        bodyHash = SL.hashAnnotated body

        -- Witness the use of bootstrap address's utxo.
        byronWit :: SL.BootstrapWitness c
        byronWit =
            SL.makeBootstrapWitness (SL.extractHash bodyHash) byronSK $
            Byron.addrAttributes byronAddr

        -- Witness the stake delegation.
        delegWit :: SL.WitVKey 'SL.Witness c
        delegWit =
            SL.makeWitnessVKey
              bodyHash
              (Shelley.mkKeyPair stakingSK)

        -- Witness the pool registration.
        poolWit :: SL.WitVKey 'SL.Witness c
        poolWit =
            SL.makeWitnessVKey
              bodyHash
              (Shelley.mkKeyPair poolSK)

    in
    if Map.null picked then Nothing else
    (Just . GenTxShelley. mkShelleyTx) $
    SL.Tx
      { SL._body       = body
      , SL._metadata   = SL.SNothing
      , SL._witnessSet = SL.WitnessSet
                           (Set.fromList [delegWit, poolWit])
                           mempty
                           (Set.singleton byronWit)
      }

    | otherwise           = Nothing

  where
    mbUTxO :: Maybe (SL.UTxO (ShelleyEra c))
    mbUTxO =
        fmap getUTxOShelley $
        ejectShelleyTickedLedgerState $
        applyChainTick lcfg curSlot $
        lst

    MigrationInfo
      { byronMagic
      , byronSK
      , paymentSK
      , poolSK
      , stakingSK
      , vrfSK
      } = migrationInfo

    byronAddr :: Byron.Address
    byronAddr =
        Byron.makeVerKeyAddress byronMagic $ toVerification byronSK

    -- We use a base reference for the stake so that we can refer to it in the
    -- same tx that registers it.
    shelleyAddr :: SL.Addr c
    shelleyAddr =
        SL.Addr Shelley.networkId
          (Shelley.mkCredential paymentSK)
          (SL.StakeRefBase $ Shelley.mkCredential stakingSK)

    -- A simplistic individual pool
    poolParams :: SL.Coin -> SL.PoolParams c
    poolParams pledge = SL.PoolParams
        { SL._poolCost   = SL.Coin 1
        , SL._poolMD     = SL.SNothing
        , SL._poolMargin = SL.truncateUnitInterval 0
        , SL._poolOwners = Set.singleton $ Shelley.mkKeyHash poolSK
        , SL._poolPledge = pledge
        , SL._poolId     = Shelley.mkKeyHash poolSK
        , SL._poolRAcnt  =
            SL.RewardAcnt Shelley.networkId $ Shelley.mkCredential poolSK
        , SL._poolRelays = StrictSeq.empty
        , SL._poolVrf    = Shelley.mkKeyHashVrf vrfSK
        }

-----

ejectShelleyNS ::
     NS f (CardanoEras c)
  -> Maybe (f (ShelleyBlock (ShelleyEra c)))
ejectShelleyNS = \case
    S (Z x) -> Just x
    _       -> Nothing

getUTxOShelley :: Ticked (LedgerState (ShelleyBlock era))
               -> SL.UTxO era
getUTxOShelley tls =
    SL._utxo $
    SL._utxoState $
    SL.esLState $
    SL.nesEs $
    tickedShelleyLedgerState tls

ejectShelleyTickedLedgerState ::
     Ticked (LedgerState (CardanoBlock c))
  -> Maybe (Ticked (LedgerState (ShelleyBlock (ShelleyEra c))))
ejectShelleyTickedLedgerState ls =
    fmap (unComp . currentState) $
    ejectShelleyNS $
    Tele.tip $
    getHardForkState $
    tickedHardForkLedgerStatePerEra $
    ls
