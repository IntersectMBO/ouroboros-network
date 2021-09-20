{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transitional Praos.
--
--   Transitional praos allows for the overlaying of Praos with an overlay
--   schedule determining slots to be produced by BFT
module Ouroboros.Consensus.Shelley.Protocol (
    MaxMajorProtVer (..)
  , SelfIssued (..)
  , TPraos
  , TPraosCanBeLeader (..)
  , TPraosChainSelectView (..)
  , TPraosFields (..)
  , TPraosIsLeader (..)
  , TPraosParams (..)
  , TPraosState (..)
  , TPraosToSign (..)
  , TPraosValidateView
  , forgeTPraosFields
  , mkShelleyGlobals
  , mkTPraosParams
    -- * Crypto
  , SL.PraosCrypto
  , StandardCrypto
    -- * CannotForge
  , TPraosCannotForge (..)
  , tpraosCheckCanForge
    -- * Type instances
  , ConsensusConfig (..)
  , Ticked (..)
  ) where

import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Control.Monad.Except (Except, runExcept, throwError,
                     withExceptT)
import           Data.Coerce (coerce)
import           Data.Function (on)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down (..))
import qualified Data.Text as T (pack)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))
import           Numeric.Natural (Natural)

import           Cardano.Binary (enforceSize, fromCBOR, toCBOR)
import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Time (SystemStart (..))

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Versioned

import qualified Cardano.Ledger.BaseTypes as SL (ActiveSlotCoeff, Seed)
import           Cardano.Ledger.Crypto (StandardCrypto, VRF)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.BHeader as SL (mkSeed, seedEta, seedL)
import qualified Cardano.Protocol.TPraos.OCert as Absolute (KESPeriod (..))

import           Ouroboros.Consensus.Shelley.Protocol.HotKey (HotKey)
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import           Ouroboros.Consensus.Shelley.Protocol.Util

{-------------------------------------------------------------------------------
  Fields required by TPraos in the header
-------------------------------------------------------------------------------}

data TPraosFields c toSign = TPraosFields {
      tpraosSignature :: SL.SignedKES c toSign
    , tpraosToSign    :: toSign
    }
  deriving (Generic)

deriving instance (NoThunks toSign, SL.PraosCrypto c)
  => NoThunks (TPraosFields c toSign)
deriving instance (Show toSign, SL.PraosCrypto c)
  => Show (TPraosFields c toSign)

-- | Fields arising from transitional praos execution which must be included in
-- the block signature.
data TPraosToSign c = TPraosToSign {
      -- | Verification key for the issuer of this block.
      --
      -- Note that unlike in Classic/BFT where we have a key for the genesis
      -- delegate on whose behalf we are issuing this block, this key
      -- corresponds to the stake pool/core node actually forging the block.
      tpraosToSignIssuerVK :: SL.VKey 'SL.BlockIssuer c
    , tpraosToSignVrfVK    :: SL.VerKeyVRF c
      -- | Verifiable result containing the updated nonce value.
    , tpraosToSignEta      :: SL.CertifiedVRF c SL.Nonce
      -- | Verifiable proof of the leader value, used to determine whether the
      -- node has the right to issue a block in this slot.
      --
      -- We include a value here even for blocks forged under the BFT
      -- schedule. It is not required that such a value be verifiable (though
      -- by default it will be verifiably correct, but unused.)
    , tpraosToSignLeader   :: SL.CertifiedVRF c Natural
      -- | Lightweight delegation certificate mapping the cold (DSIGN) key to
      -- the online KES key.
    , tpraosToSignOCert    :: SL.OCert c
    }
  deriving (Generic)

instance SL.PraosCrypto c => NoThunks (TPraosToSign c)
deriving instance SL.PraosCrypto c => Show (TPraosToSign c)

forgeTPraosFields ::
     ( SL.PraosCrypto c
     , SL.KESignable c toSign
     , Monad m
     )
  => HotKey c m
  -> CanBeLeader (TPraos c)
  -> IsLeader (TPraos c)
  -> (TPraosToSign c -> toSign)
  -> m (TPraosFields c toSign)
forgeTPraosFields hotKey TPraosCanBeLeader{..} TPraosIsLeader{..} mkToSign = do
    signature <- HotKey.sign hotKey toSign
    return TPraosFields {
        tpraosSignature = signature
      , tpraosToSign    = toSign
      }
  where
    toSign = mkToSign signedFields

    signedFields = TPraosToSign {
        tpraosToSignIssuerVK = tpraosCanBeLeaderColdVerKey
      , tpraosToSignVrfVK    = VRF.deriveVerKeyVRF tpraosCanBeLeaderSignKeyVRF
      , tpraosToSignEta      = tpraosIsLeaderEta
      , tpraosToSignLeader   = tpraosIsLeaderProof
      , tpraosToSignOCert    = tpraosCanBeLeaderOpCert
      }

-- | Because we are using the executable spec, rather than implementing the
-- protocol directly here, we have a fixed header type rather than an
-- abstraction. So our validate view is fixed to this.
type TPraosValidateView c = SL.BHeader c

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | The maximum major protocol version.
--
-- Must be at least the current major protocol version. For Cardano mainnet, the
-- Shelley era has major protocol verison __2__.
newtype MaxMajorProtVer = MaxMajorProtVer {
      getMaxMajorProtVer :: Natural
    }
  deriving (Eq, Show, Generic, NoThunks)

data TPraos c

-- | TPraos parameters that are node independent
data TPraosParams = TPraosParams {
      -- | See 'Globals.slotsPerKESPeriod'.
      tpraosSlotsPerKESPeriod :: !Word64
      -- | Active slots coefficient. This parameter represents the proportion
      -- of slots in which blocks should be issued. This can be interpreted as
      -- the probability that a party holding all the stake will be elected as
      -- leader for a given slot.
    , tpraosLeaderF           :: !SL.ActiveSlotCoeff
      -- | See 'Globals.securityParameter'.
    , tpraosSecurityParam     :: !SecurityParam
      -- | Maximum number of KES iterations, see 'Globals.maxKESEvo'.
    , tpraosMaxKESEvo         :: !Word64
      -- | Quorum for update system votes and MIR certificates, see
      -- 'Globals.quorum'.
    , tpraosQuorum            :: !Word64
      -- | All blocks invalid after this protocol version, see
      -- 'Globals.maxMajorPV'.
    , tpraosMaxMajorPV        :: !MaxMajorProtVer
      -- | Maximum number of lovelace in the system, see
      -- 'Globals.maxLovelaceSupply'.
    , tpraosMaxLovelaceSupply :: !Word64
      -- | Testnet or mainnet?
    , tpraosNetworkId         :: !SL.Network
      -- | Initial nonce used for the TPraos protocol state. Typically this is
      -- derived from the hash of the Shelley genesis config JSON file, but
      -- different values may be used for testing purposes.
      --
      -- NOTE: this is only used when translating the Byron 'ChainDepState' to
      -- the Shelley 'ChainDepState', at which point we'll need access to the
      -- initial nonce at runtime. TODO #2326.
    , tpraosInitialNonce      :: !SL.Nonce
      -- | The system start, as projected from the chain's genesis block.
    , tpraosSystemStart       :: !SystemStart
    }
  deriving (Generic, NoThunks)

mkTPraosParams
  :: MaxMajorProtVer
  -> SL.Nonce  -- ^ Initial nonce
  -> SL.ShelleyGenesis era
  -> TPraosParams
mkTPraosParams maxMajorPV initialNonce genesis = TPraosParams {
      tpraosSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesis
    , tpraosLeaderF           = SL.sgActiveSlotCoeff   genesis
    , tpraosMaxKESEvo         = SL.sgMaxKESEvolutions  genesis
    , tpraosQuorum            = SL.sgUpdateQuorum      genesis
    , tpraosMaxLovelaceSupply = SL.sgMaxLovelaceSupply genesis
    , tpraosNetworkId         = SL.sgNetworkId         genesis
    , tpraosSecurityParam     = securityParam
    , tpraosMaxMajorPV        = maxMajorPV
    , tpraosInitialNonce      = initialNonce
    , tpraosSystemStart       = systemStart
    }
  where
    securityParam = SecurityParam $ SL.sgSecurityParam genesis
    systemStart   = SystemStart   $ SL.sgSystemStart   genesis

data TPraosCanBeLeader c = TPraosCanBeLeader {
      -- | Certificate delegating rights from the stake pool cold key (or
      -- genesis stakeholder delegate cold key) to the online KES key.
      tpraosCanBeLeaderOpCert     :: !(SL.OCert c)
      -- | Stake pool cold key or genesis stakeholder delegate cold key.
    , tpraosCanBeLeaderColdVerKey :: !(SL.VKey 'SL.BlockIssuer c)
    , tpraosCanBeLeaderSignKeyVRF :: !(SL.SignKeyVRF c)
    }
  deriving (Generic)

instance SL.PraosCrypto c => NoThunks (TPraosCanBeLeader c)

-- | Assembled proof that the issuer has the right to issue a block in the
-- selected slot.
data TPraosIsLeader c = TPraosIsLeader {
      tpraosIsLeaderEta        :: SL.CertifiedVRF c SL.Nonce
    , tpraosIsLeaderProof      :: SL.CertifiedVRF c Natural
      -- | When in the overlay schedule (otherwise 'Nothing'), return the hash
      -- of the VRF verification key in the overlay schedule
    , tpraosIsLeaderGenVRFHash :: Maybe (SL.Hash c (SL.VerKeyVRF c))
    }
  deriving (Generic)

instance SL.PraosCrypto c => NoThunks (TPraosIsLeader c)

-- | Static configuration
data instance ConsensusConfig (TPraos c) = TPraosConfig {
      tpraosParams    :: !TPraosParams
    , tpraosEpochInfo :: !(EpochInfo (Except History.PastHorizonException))

      -- it's useful for this record to be EpochInfo and one other thing,
      -- because the one other thing can then be used as the
      -- PartialConsensConfig in the HFC instance.

    }
  deriving (Generic)

instance SL.PraosCrypto c => NoThunks (ConsensusConfig (TPraos c))

-- | Separate type instead of 'Bool' for the custom 'Ord' instance +
-- documentation.
data SelfIssued =
    -- | A block we produced ourself
    SelfIssued
    -- | A block produced by another node
  | NotSelfIssued
  deriving (Show, Eq)

instance Ord SelfIssued where
  compare SelfIssued    SelfIssued    = EQ
  compare NotSelfIssued NotSelfIssued = EQ
  compare SelfIssued    NotSelfIssued = GT
  compare NotSelfIssued SelfIssued    = LT

-- | View of the ledger tip for chain selection.
--
-- We order between chains as follows:
--
-- 1. By chain length, with longer chains always preferred.
-- 2. If the tip of each chain has the same slot number, we prefer the one tip
--    that we produced ourselves.
-- 3. If the tip of each chain was issued by the same agent, then we prefer
--    the chain whose tip has the highest ocert issue number.
-- 4. By the leader value of the chain tip, with lower values preferred.
data TPraosChainSelectView c = TPraosChainSelectView {
    csvChainLength :: BlockNo
  , csvSlotNo      :: SlotNo
  , csvSelfIssued  :: SelfIssued
  , csvIssuer      :: SL.VKey 'SL.BlockIssuer c
  , csvIssueNo     :: Word64
  , csvLeaderVRF   :: VRF.OutputVRF (VRF c)
  } deriving (Show, Eq)

instance SL.PraosCrypto c => Ord (TPraosChainSelectView c) where
  compare =
      mconcat [
          compare `on` csvChainLength
        , whenSame csvSlotNo (compare `on` csvSelfIssued)
        , whenSame csvIssuer (compare `on` csvIssueNo)
        , compare `on` Down . csvLeaderVRF
        ]
    where
      -- | When the @a@s are equal, use the given comparison function,
      -- otherwise, no preference.
      whenSame ::
           Eq a
        => (view -> a)
        -> (view -> view -> Ordering)
        -> (view -> view -> Ordering)
      whenSame f comp v1 v2
        | f v1 == f v2
        = comp v1 v2
        | otherwise
        = EQ

-- | Ledger view at a particular slot
newtype instance Ticked (SL.LedgerView c) = TickedPraosLedgerView {
      -- TODO: Perhaps it would be cleaner to define this as a separate type
      getTickedPraosLedgerView :: SL.LedgerView c
    }

-- | Transitional Praos consensus state.
--
-- In addition to the 'ChainDepState' provided by the ledger, we track the slot
-- number of the last applied header.
data TPraosState c = TPraosState {
      tpraosStateLastSlot      :: !(WithOrigin SlotNo)
    , tpraosStateChainDepState :: !(SL.ChainDepState c)
    }
  deriving (Generic, Show, Eq)

instance SL.PraosCrypto c => NoThunks (TPraosState c)

-- | Version 0 supported rollback, removed in #2575.
serialisationFormatVersion1 :: VersionNumber
serialisationFormatVersion1 = 1

instance SL.PraosCrypto c => Serialise (TPraosState c) where
  encode (TPraosState slot chainDepState) =
    encodeVersion serialisationFormatVersion1 $ mconcat [
        CBOR.encodeListLen 2
      , toCBOR slot
      , toCBOR chainDepState
      ]

  decode = decodeVersion
      [(serialisationFormatVersion1, Decode decodeTPraosState1)]
    where
      decodeTPraosState1 = do
        enforceSize "TPraosState" 2
        TPraosState <$> fromCBOR <*> fromCBOR

-- | Ticked 'TPraosState'
data instance Ticked (TPraosState c) = TickedChainDepState {
      tickedTPraosStateChainDepState :: SL.ChainDepState c
    , tickedTPraosStateLedgerView    :: Ticked (LedgerView (TPraos c))
    }

instance SL.PraosCrypto c => ConsensusProtocol (TPraos c) where
  type ChainDepState (TPraos c) = TPraosState c
  type IsLeader      (TPraos c) = TPraosIsLeader c
  type CanBeLeader   (TPraos c) = TPraosCanBeLeader c
  type SelectView    (TPraos c) = TPraosChainSelectView c
  type LedgerView    (TPraos c) = SL.LedgerView c
  type ValidationErr (TPraos c) = SL.ChainTransitionError c
  type ValidateView  (TPraos c) = TPraosValidateView c

  protocolSecurityParam = tpraosSecurityParam . tpraosParams

  checkIsLeader cfg TPraosCanBeLeader{..} slot cs = do
      -- First, check whether we're in the overlay schedule
      case SL.lookupInOverlaySchedule firstSlot gkeys d asc slot of
        -- Slot isn't in the overlay schedule, so we're in Praos
        Nothing
          | meetsLeaderThreshold cfg lv (SL.coerceKeyRole vkhCold) y
          -> Just TPraosIsLeader {
                tpraosIsLeaderEta        = coerce rho
              , tpraosIsLeaderProof      = coerce y
              , tpraosIsLeaderGenVRFHash = Nothing
              }
          | otherwise
          -> Nothing

       -- This is a non-active slot; nobody may produce a block
        Just SL.NonActiveSlot -> Nothing

       -- The given genesis key has authority to produce a block in this
        -- slot. Check whether we're its delegate.
        Just (SL.ActiveSlot gkhash) -> case Map.lookup gkhash dlgMap of
            Nothing
              -> error "unknown genesis key in overlay schedule"
            Just (SL.GenDelegPair dlgHash genDlgVRFHash)
              | SL.coerceKeyRole dlgHash == vkhCold
              -> Just TPraosIsLeader {
                     tpraosIsLeaderEta        = coerce rho
                     -- Note that this leader value is not checked for slots in
                     -- the overlay schedule, so we could set it to whatever we
                     -- want. We evaluate it as normal for simplicity's sake.
                   , tpraosIsLeaderProof      = coerce y
                   , tpraosIsLeaderGenVRFHash = Just genDlgVRFHash
                   }
              | otherwise
              -> Nothing
    where
      chainState = tickedTPraosStateChainDepState cs
      lv         = getTickedPraosLedgerView $ tickedTPraosStateLedgerView cs
      d          = SL.lvD lv
      asc        = tpraosLeaderF $ tpraosParams cfg
      firstSlot  =
          firstSlotOfEpochOfSlot
            (History.toPureEpochInfo $ tpraosEpochInfo cfg)
            slot
      gkeys      = Map.keysSet dlgMap
      eta0       = SL.ticknStateEpochNonce $ SL.csTickn chainState
      vkhCold    = SL.hashKey tpraosCanBeLeaderColdVerKey
      rho'       = SL.mkSeed SL.seedEta slot eta0
      y'         = SL.mkSeed SL.seedL   slot eta0

      rho = VRF.evalCertified () rho' tpraosCanBeLeaderSignKeyVRF
      y   = VRF.evalCertified () y'   tpraosCanBeLeaderSignKeyVRF

      SL.GenDelegs dlgMap = SL.lvGenDelegs lv

  tickChainDepState cfg@TPraosConfig{..}
                    (TickedPraosLedgerView lv)
                    slot
                    (TPraosState lastSlot st) =
      TickedChainDepState {
          tickedTPraosStateChainDepState = st'
        , tickedTPraosStateLedgerView    = TickedPraosLedgerView lv
        }
    where
      st' = SL.tickChainDepState
              shelleyGlobals
              lv
              ( isNewEpoch
                  (History.toPureEpochInfo tpraosEpochInfo)
                  lastSlot
                  slot
              )
              st
      shelleyGlobals = mkShelleyGlobals cfg

  updateChainDepState cfg b slot cs =
      TPraosState (NotOrigin slot) <$>
        SL.updateChainDepState
          shelleyGlobals
          lv
          b
          (tickedTPraosStateChainDepState cs)
    where
      shelleyGlobals = mkShelleyGlobals cfg
      lv = getTickedPraosLedgerView (tickedTPraosStateLedgerView cs)

  reupdateChainDepState cfg b slot cs =
      TPraosState (NotOrigin slot) $
        SL.reupdateChainDepState
          shelleyGlobals
          lv
          b
          (tickedTPraosStateChainDepState cs)
    where
      shelleyGlobals = mkShelleyGlobals cfg
      lv = getTickedPraosLedgerView (tickedTPraosStateLedgerView cs)

mkShelleyGlobals :: ConsensusConfig (TPraos c) -> SL.Globals
mkShelleyGlobals TPraosConfig{..} = SL.Globals {
      epochInfoWithErr              =
        hoistEpochInfo
          (runExcept . withExceptT (T.pack . show))
          tpraosEpochInfo
    , slotsPerKESPeriod             = tpraosSlotsPerKESPeriod
    , stabilityWindow               = SL.computeStabilityWindow               k tpraosLeaderF
    , randomnessStabilisationWindow = SL.computeRandomnessStabilisationWindow k tpraosLeaderF
    , securityParameter             = k
    , maxKESEvo                     = tpraosMaxKESEvo
    , quorum                        = tpraosQuorum
    , maxMajorPV                    = getMaxMajorProtVer tpraosMaxMajorPV
    , maxLovelaceSupply             = tpraosMaxLovelaceSupply
    , activeSlotCoeff               = tpraosLeaderF
    , networkId                     = tpraosNetworkId
    , systemStart                   = tpraosSystemStart
    }
  where
    SecurityParam k  = tpraosSecurityParam
    TPraosParams{..} = tpraosParams

-- | Check whether this node meets the leader threshold to issue a block.
meetsLeaderThreshold ::
     forall c. SL.PraosCrypto c
  => ConsensusConfig (TPraos c)
  -> LedgerView (TPraos c)
  -> SL.KeyHash 'SL.StakePool c
  -> SL.CertifiedVRF c SL.Seed
  -> Bool
meetsLeaderThreshold TPraosConfig { tpraosParams }
                     SL.LedgerView { lvPoolDistr }
                     keyHash
                     certNat =
    SL.checkLeaderValue
      (VRF.certifiedOutput certNat)
      r
      (tpraosLeaderF tpraosParams)
  where
    SL.PoolDistr poolDistr = lvPoolDistr
    r = maybe 0 SL.individualPoolStake
        $ Map.lookup keyHash poolDistr

{-------------------------------------------------------------------------------
  CannotForge
-------------------------------------------------------------------------------}

-- | Expresses that, whilst we believe ourselves to be a leader for this slot,
-- we are nonetheless unable to forge a block.
data TPraosCannotForge c =
    -- | The KES key in our operational certificate can't be used because the
    -- current (wall clock) period is before the start period of the key.
    -- current KES period.
    --
    -- Note: the opposite case, i.e., the wall clock period being after the
    -- end period of the key, is caught when trying to update the key in
    -- 'updateForgeState'.
    TPraosCannotForgeKeyNotUsableYet
      !Absolute.KESPeriod
      -- ^ Current KES period according to the wallclock slot, i.e., the KES
      -- period in which we want to use the key.
      !Absolute.KESPeriod
      -- ^ Start KES period of the KES key.

    -- | We are a genesis delegate, but our VRF key (second argument) does not
    -- match the registered key for that delegate (first argument).
  | TPraosCannotForgeWrongVRF
      !(SL.Hash c (SL.VerKeyVRF c))
      !(SL.Hash c (SL.VerKeyVRF c))
  deriving (Generic)

deriving instance SL.PraosCrypto c => Show (TPraosCannotForge c)

tpraosCheckCanForge ::
     ConsensusConfig (TPraos c)
  -> SL.Hash c (SL.VerKeyVRF c)
     -- ^ Precomputed hash of the VRF verification key
  -> SlotNo
  -> IsLeader (TPraos c)
  -> HotKey.KESInfo
  -> Either (TPraosCannotForge c) ()
tpraosCheckCanForge TPraosConfig { tpraosParams }
                    forgingVRFHash
                    curSlot
                    TPraosIsLeader { tpraosIsLeaderGenVRFHash }
                    kesInfo
  | let startPeriod = HotKey.kesStartPeriod kesInfo
  , startPeriod > wallclockPeriod
  = throwError $ TPraosCannotForgeKeyNotUsableYet wallclockPeriod startPeriod
  | Just genVRFHash <- tpraosIsLeaderGenVRFHash
  , genVRFHash /= forgingVRFHash
  = throwError $ TPraosCannotForgeWrongVRF genVRFHash forgingVRFHash
  | otherwise
  = return ()
  where
    -- The current wallclock KES period
    wallclockPeriod :: Absolute.KESPeriod
    wallclockPeriod = Absolute.KESPeriod $ fromIntegral $
        unSlotNo curSlot `div` tpraosSlotsPerKESPeriod tpraosParams

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance (Condense toSign, SL.PraosCrypto c) => Condense (TPraosFields c toSign) where
  condense = condense . tpraosToSign
