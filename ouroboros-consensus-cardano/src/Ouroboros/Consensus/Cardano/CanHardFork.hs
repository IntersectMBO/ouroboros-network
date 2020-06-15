{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.CanHardFork (
    ShelleyPartialLedgerConfig (..)
  ) where

import           Control.Monad.Reader (runReader)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           GHC.Generics (Generic)

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Hashing as Hashing
import           Cardano.Prelude (Natural, NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..), RequiringBoth (..))

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Protocol.PBFT (PBft, PBftCrypto)
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState

import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.EpochBoundary as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Rewards as SL
import qualified Shelley.Spec.Ledger.STS.Prtcl as SL
import qualified Shelley.Spec.Ledger.TxData as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

import           Ouroboros.Consensus.Cardano.Block

{-------------------------------------------------------------------------------
  SingleEraBlock Byron
-------------------------------------------------------------------------------}

instance SingleEraBlock ByronBlock where
  -- | TODO Currently we hard-code the transition in the partial ledger config
  -- for testing purposes.
  --
  -- This will be replaced with a proper implementation that looks for the
  -- right transaction in the ledger. See:
  -- <https://github.com/input-output-hk/ouroboros-network/issues/1786>
  singleEraTransition cfg _ledgerState =
      case byronLedgerConfigTransition cfg of
        NoHardCodedTransition       -> Nothing
        HardCodedTransitionAt epoch -> Just epoch

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Byron"
    }

instance PBftCrypto bc => HasPartialConsensusConfig (PBft bc)
  -- Use defaults

instance HasPartialLedgerConfig ByronBlock where
  type PartialLedgerConfig ByronBlock = LedgerConfig ByronBlock
  completeLedgerConfig _ _ = id

{-------------------------------------------------------------------------------
  SingleEraBlock Shelley
-------------------------------------------------------------------------------}

instance TPraosCrypto sc => SingleEraBlock (ShelleyBlock sc) where
  -- No transition from Shelley to Goguen yet
  singleEraTransition _cfg _st = Nothing

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Shelley"
    }

instance TPraosCrypto sc => HasPartialConsensusConfig (TPraos sc) where
  type PartialConsensusConfig (TPraos sc) = TPraosParams

  completeConsensusConfig _ tpraosEpochInfo tpraosParams = TPraosConfig {..}

  -- 'ChainSelConfig' is ()
  partialChainSelConfig _ _ = ()

data ShelleyPartialLedgerConfig sc = ShelleyPartialLedgerConfig {
      shelleyPartialLedgerGenesis :: !(SL.ShelleyGenesis sc)
    , shelleyPartialMaxMajorPV    :: !Natural
    }
  deriving (Generic, NoUnexpectedThunks)

instance TPraosCrypto sc => HasPartialLedgerConfig (ShelleyBlock sc) where
  type PartialLedgerConfig (ShelleyBlock sc) = ShelleyPartialLedgerConfig sc

  completeLedgerConfig _ epochInfo ShelleyPartialLedgerConfig {..} =
      mkShelleyLedgerConfig
        shelleyPartialLedgerGenesis
        epochInfo
        shelleyPartialMaxMajorPV

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

instance TPraosCrypto c => CanHardFork (CardanoEras c) where
  hardForkEraTranslation = EraTranslation {
      translateLedgerState    = PCons translateLedgerStateByronToShelleyWrapper    PNil
    , translateLedgerView     = PCons translateLedgerViewByronToShelleyWrapper     PNil
    , translateConsensusState = PCons translateConsensusStateByronToShelleyWrapper PNil
    }

{-------------------------------------------------------------------------------
  Translation from Byron to Shelley
-------------------------------------------------------------------------------}

translateHeaderHashByronToShelley
  :: forall sc. Crypto sc
  => HeaderHash ByronBlock
  -> HeaderHash (ShelleyBlock sc)
translateHeaderHashByronToShelley =
      fromRawHash (Proxy @(ShelleyBlock sc))
    . toRawHash   (Proxy @ByronBlock)

translatePointByronToShelley
  :: Crypto sc
  => Point ByronBlock
  -> Point (ShelleyBlock sc)
translatePointByronToShelley = \case
    GenesisPoint   -> GenesisPoint
    BlockPoint s h -> BlockPoint s (translateHeaderHashByronToShelley h)

translateUTxOByronToShelley :: forall sc. CC.UTxO -> SL.UTxO sc
translateUTxOByronToShelley (CC.UTxO utxoByron) =
    SL.UTxO $ Map.fromList
      [ (txInShelley, txOutShelley)
      | (txInByron, txOutByron) <- Map.toList utxoByron
      , let txInShelley  = translateTxIn  $ CC.fromCompactTxIn  txInByron
            txOutShelley = translateTxOut $ CC.fromCompactTxOut txOutByron
      ]
  where
    translateTxIn :: CC.TxIn -> SL.TxIn sc
    translateTxIn (CC.TxInUtxo txId idx) =
      SL.TxIn (translateTxId txId) (fromIntegral idx)

    translateTxOut :: CC.TxOut -> SL.TxOut sc
    translateTxOut (CC.TxOut addr amount) =
      SL.TxOut (translateAddr addr) (translateAmount amount)

    -- | We use the same hasing algorithm so we can unwrap and rewrap the
    -- bytes. We don't care about the type that is hashed, which will differ
    -- going from Byron to Shelley, we just use the hashes as IDs.
    translateTxId :: CC.TxId -> SL.TxId sc
    translateTxId = SL.TxId . Hash.UnsafeHash . Hashing.hashToBytes

    translateAmount :: CC.Lovelace -> SL.Coin
    translateAmount = SL.Coin . CC.lovelaceToInteger

    translateAddr :: CC.Address -> SL.Addr sc
    translateAddr = SL.AddrBootstrap . SL.BootstrapAddress

translateLedgerStateByronToShelleyWrapper
  :: forall sc. Crypto sc
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       ByronBlock
       (ShelleyBlock sc)
translateLedgerStateByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgShelley) ->
    Translate   $ \epochNo ledgerByron ->
      translateLedgerStateByronToShelley cfgShelley epochNo ledgerByron

translateLedgerStateByronToShelley
  :: forall sc. Crypto sc
  => LedgerConfig (ShelleyBlock sc)
  -> EpochNo
  -> LedgerState ByronBlock
  -> LedgerState (ShelleyBlock sc)
translateLedgerStateByronToShelley cfgShelley epochNo ledgerByron =
    ShelleyLedgerState {
      ledgerTip = ledgerTipShelley
    , history   = History.empty
    , shelleyState
    }
  where
    ShelleyLedgerConfig { shelleyLedgerGenesis = genesisShelley } = cfgShelley

    shelleyState :: SL.ShelleyState sc
    shelleyState = SL.NewEpochState {
        nesEL     = epochNo
      , nesBprev  = SL.BlocksMade Map.empty
      , nesBcur   = SL.BlocksMade Map.empty
      , nesEs     = epochState
      , nesRu     = SL.SNothing
      , nesPd     = SL.PoolDistr Map.empty
      , nesOsched = overlaySchedule
      }

    pparams :: SL.PParams
    pparams = sgProtocolParams genesisShelley

    -- | NOTE: we ignore the Byron delegation map because the genesis and
    -- delegation verification keys are hashed using a different hashing
    -- scheme, Blake2b_224, whereas Shelley uses Blake2b_256. This means we
    -- can't simply convert them, as Byron nowhere stores the original
    -- verification keys.
    --
    -- Fortunately, no Byron genesis delegations have happened yet, and if
    -- they did, we would be aware of them before the hard fork, as we
    -- instigate the hard fork. We just have to make sure that the hard-coded
    -- Shelley genesis contains the same genesis and delegation verification
    -- keys, but hashed with the right algorithm.
    genDelegs :: SL.GenDelegs sc
    genDelegs = SL.GenDelegs $ sgGenDelegs genesisShelley

    reserves :: SL.Coin
    reserves =
        fromIntegral (sgMaxLovelaceSupply genesisShelley) - SL.balance utxoShelley

    epochState :: SL.EpochState sc
    epochState = SL.EpochState {
        esAccountState = SL.AccountState (SL.Coin 0) reserves
      , esSnapshots    = SL.emptySnapShots
      , esLState       = ledgerState
      , esPrevPp       = pparams
      , esPp           = pparams
      , esNonMyopic    = SL.emptyNonMyopic
      }

    utxoByron :: CC.UTxO
    utxoByron = CC.cvsUtxo $ byronLedgerState ledgerByron

    utxoShelley :: SL.UTxO sc
    utxoShelley = translateUTxOByronToShelley utxoByron

    ledgerState :: SL.LedgerState sc
    ledgerState = SL.LedgerState {
        _utxoState = SL.UTxOState {
            _utxo      = utxoShelley
          , _deposited = SL.Coin 0
          , _fees      = SL.Coin 0
          , _ppups     = SL.ProposedPPUpdates Map.empty
          }
      , _delegationState = SL.DPState {
          _dstate = SL.emptyDState { SL._genDelegs = genDelegs }
        , _pstate = SL.emptyPState
        }
      }

    ledgerTipShelley :: Point (ShelleyBlock sc)
    ledgerTipShelley =
      translatePointByronToShelley $
      ledgerTipPoint' (Proxy @ByronBlock) ledgerByron

    overlaySchedule :: Map SlotNo (SL.OBftSlot sc)
    overlaySchedule =
      flip runReader (shelleyLedgerGlobals cfgShelley) $
        SL.overlaySchedule
          epochNo
          (Map.keysSet (sgGenDelegs genesisShelley))
          (sgProtocolParams genesisShelley)

translateConsensusStateByronToShelleyWrapper
  :: forall sc.
     RequiringBoth
       WrapConsensusConfig
       (Translate WrapConsensusState)
       ByronBlock
       (ShelleyBlock sc)
translateConsensusStateByronToShelleyWrapper =
    RequireBoth $ \_ _ -> Translate   $ \_ (WrapConsensusState pbftState) ->
      WrapConsensusState (translateConsensusStateByronToShelley pbftState)

translateConsensusStateByronToShelley
  :: forall bc sc.
     PBftState bc
  -> TPraosState sc
translateConsensusStateByronToShelley pbftState =
    TPraosState.empty (PBftState.tipSlot pbftState) $
      SL.PrtclState
        Map.empty
        nonce
        nonce
        nonce
        nonce
  where
    -- TODO use hash of Shelley genesis config as entropy?
    nonce = SL.NeutralNonce

translateLedgerViewByronToShelleyWrapper
  :: forall sc.
     RequiringBoth
       WrapLedgerConfig
       (Translate WrapLedgerView)
       ByronBlock
       (ShelleyBlock sc)
translateLedgerViewByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig shelleyCfg) -> Translate $ \epochNo _ ->
      WrapLedgerView (translateLedgerViewByronToShelley shelleyCfg epochNo)

-- | We construct a 'SL.LedgerView' using the Shelley genesis config in the
-- same way as 'translateLedgerStateByronToShelley'.
translateLedgerViewByronToShelley
  :: forall sc.
     LedgerConfig (ShelleyBlock sc)
  -> EpochNo
  -> SL.LedgerView sc
translateLedgerViewByronToShelley shelleyCfg epochNo = SL.LedgerView {
      lvProtParams   = sgProtocolParams genesisShelley
    , lvOverlaySched = overlaySchedule
    , lvPoolDistr    = SL.PoolDistr Map.empty
    , lvGenDelegs    = SL.GenDelegs $ sgGenDelegs genesisShelley
    }
  where
    ShelleyLedgerConfig {
        shelleyLedgerGenesis = genesisShelley
      , shelleyLedgerGlobals
      } = shelleyCfg

    overlaySchedule :: Map SlotNo (SL.OBftSlot sc)
    overlaySchedule =
      flip runReader shelleyLedgerGlobals $
        SL.overlaySchedule
          epochNo
          (Map.keysSet (sgGenDelegs genesisShelley))
          (sgProtocolParams genesisShelley)
