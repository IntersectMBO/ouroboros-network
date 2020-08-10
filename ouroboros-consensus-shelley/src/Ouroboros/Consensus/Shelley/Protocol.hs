{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
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
    TPraos
  , TPraosChainSelectView (..)
  , SelfIssued (..)
  , TPraosFields (..)
  , forgeTPraosFields
  , TPraosToSign (..)
  , TPraosValidateView
  , TPraosParams (..)
  , mkTPraosParams
  , TPraosCanBeLeader (..)
  , TPraosIsLeader (..)
  , mkShelleyGlobals
    -- * Crypto
  , Crypto
  , TPraosCrypto
  , TPraosStandardCrypto
    -- * Stability
  , computeStabilityWindow
  , computeRandomnessStabilisationWindow
    -- * CannotForge
  , TPraosCannotForge (..)
  , tpraosCheckCanForge
    -- * Type instances
  , ConsensusConfig (..)
  , Ticked (..)
  ) where

import           Control.Monad.Except (throwError)
import           Data.Coerce (coerce)
import           Data.Function (on)
import           Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down (..))
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Crypto.DSIGN.Class (VerKeyDSIGN)
import           Cardano.Crypto.KES.Class (SignedKES)
import qualified Cardano.Crypto.KES.Class as KES
import           Cardano.Crypto.VRF.Class (CertifiedVRF, SignKeyVRF, VerKeyVRF,
                     deriveVerKeyVRF)
import qualified Cardano.Crypto.VRF.Class as VRF
import           Cardano.Prelude (Natural, NoUnexpectedThunks (..))
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util.Condense

import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Crypto as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as Absolute (KESPeriod (..))
import qualified Shelley.Spec.Ledger.STS.Tickn as STS

import           Ouroboros.Consensus.Shelley.Protocol.Crypto
import           Ouroboros.Consensus.Shelley.Protocol.HotKey (HotKey)
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as State
import           Ouroboros.Consensus.Shelley.Protocol.Util

{-------------------------------------------------------------------------------
  Fields required by TPraos in the header
-------------------------------------------------------------------------------}

data TPraosFields c toSign = TPraosFields {
      tpraosSignature :: SignedKES (KES c) toSign
    , tpraosToSign    :: toSign
    }
  deriving (Generic)

instance (NoUnexpectedThunks toSign, TPraosCrypto c)
  => NoUnexpectedThunks (TPraosFields c toSign)
deriving instance (Show toSign, TPraosCrypto c)
  => Show (TPraosFields c toSign)

-- | Fields arising from transitional praos execution which must be included in
-- the block signature.
data TPraosToSign c = TPraosToSign {
      -- | Verification key for the issuer of this block.
      --
      -- Note that unlike in Classic/BFT where we have a key for the genesis
      -- delegate on whose behalf we are issuing this block, this key
      -- corresponds to the stake pool/core node actually forging the block.
      tpraosToSignIssuerVK :: VerKeyDSIGN (DSIGN c)
    , tpraosToSignVrfVK    :: VerKeyVRF (VRF c)
      -- | Verifiable result containing the updated nonce value.
    , tpraosToSignEta      :: CertifiedVRF (VRF c) SL.Nonce
      -- | Verifiable proof of the leader value, used to determine whether the
      -- node has the right to issue a block in this slot.
      --
      -- We include a value here even for blocks forged under the BFT
      -- schedule. It is not required that such a value be verifiable (though
      -- by default it will be verifiably correct, but unused.)
    , tpraosToSignLeader   :: CertifiedVRF (VRF c) Natural
      -- | Lightweight delegation certificate mapping the cold (DSIGN) key to
      -- the online KES key.
    , tpraosToSignOCert    :: SL.OCert c
    }
  deriving (Generic)

instance TPraosCrypto c => NoUnexpectedThunks (TPraosToSign c)
deriving instance TPraosCrypto c => Show (TPraosToSign c)

forgeTPraosFields :: ( TPraosCrypto c
                     , KES.Signable (KES c) toSign
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

    SL.VKey issuerVK = tpraosCanBeLeaderColdVerKey

    signedFields = TPraosToSign {
        tpraosToSignIssuerVK = issuerVK
      , tpraosToSignVrfVK    = deriveVerKeyVRF tpraosCanBeLeaderSignKeyVRF
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
    , tpraosMaxMajorPV        :: !Natural
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
    }
  deriving (Generic, NoUnexpectedThunks)

mkTPraosParams
  :: Natural   -- ^ Max major protocol version
  -> SL.Nonce  -- ^ Initial nonce
  -> SL.ShelleyGenesis c
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
    }
  where
    securityParam = SecurityParam $ SL.sgSecurityParam genesis

data TPraosCanBeLeader c = TPraosCanBeLeader {
      -- | Certificate delegating rights from the stake pool cold key (or
      -- genesis stakeholder delegate cold key) to the online KES key.
      tpraosCanBeLeaderOpCert     :: !(SL.OCert c)
      -- | Stake pool cold key or genesis stakeholder delegate cold key.
    , tpraosCanBeLeaderColdVerKey :: !(SL.VKey 'SL.BlockIssuer c)
    , tpraosCanBeLeaderSignKeyVRF :: !(SignKeyVRF (VRF c))
    }
  deriving (Generic)

instance Crypto c => NoUnexpectedThunks (TPraosCanBeLeader c)

-- | Assembled proof that the issuer has the right to issue a block in the
-- selected slot.
data TPraosIsLeader c = TPraosIsLeader {
      tpraosIsLeaderEta        :: CertifiedVRF (VRF c) SL.Nonce
    , tpraosIsLeaderProof      :: CertifiedVRF (VRF c) Natural
      -- | When in the overlay schedule (otherwise 'Nothing'), return the hash
      -- of the VRF verification key in the overlay schedule
    , tpraosIsLeaderGenVRFHash :: Maybe (SL.Hash c (VerKeyVRF (VRF c)))
    }
  deriving (Generic)

instance TPraosCrypto c => NoUnexpectedThunks (TPraosIsLeader c)

-- | Static configuration
data instance ConsensusConfig (TPraos c) = TPraosConfig {
      tpraosParams    :: !TPraosParams
    , tpraosEpochInfo :: !(EpochInfo Identity)
    }
  deriving (Generic)

-- Use generic instance
instance TPraosCrypto c => NoUnexpectedThunks (ConsensusConfig (TPraos c))

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
  , csvLeaderVRF   :: VRF.OutputVRF (SL.VRF c)
  } deriving (Show, Eq)

instance Crypto c => Ord (TPraosChainSelectView c) where
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

instance TPraosCrypto c => ChainSelection (TPraos c) where

  -- | Chain selection is done on the basis of the chain length first, and then
  -- operational certificate issue number.
  type SelectView (TPraos c) = TPraosChainSelectView c

-- | Ledger view at a particular slot
newtype instance Ticked (SL.LedgerView c) = TickedPraosLedgerView {
      -- TODO: Perhaps it would be cleaner to define this as a separate type
      getTickedPraosLedgerView :: SL.LedgerView c
    }

-- | Ticked ChainDep state
--
-- We add the ticked state to the history only when applying a header.
data instance Ticked (TPraosState c) = TickedPraosState {
      tickedPraosStateTicked     :: SL.ChainDepState c
    , tickedPraosStateOrig       :: TPraosState c
    , tickedPraosStateLedgerView :: Ticked (LedgerView (TPraos c))
    }

instance TPraosCrypto c => ConsensusProtocol (TPraos c) where
  type ChainDepState (TPraos c) = TPraosState c
  type IsLeader      (TPraos c) = TPraosIsLeader c
  type CanBeLeader   (TPraos c) = TPraosCanBeLeader c
  type LedgerView    (TPraos c) = SL.LedgerView c
  type ValidationErr (TPraos c) = SL.ChainTransitionError c
  type ValidateView  (TPraos c) = TPraosValidateView c

  protocolSecurityParam = tpraosSecurityParam . tpraosParams

  checkIsLeader cfg@TPraosConfig{..} TPraosCanBeLeader{..} slot cs = do
      -- First, check whether we're in the overlay schedule
      case Map.lookup slot (SL.lvOverlaySched lv) of
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
      chainState = tickedPraosStateTicked cs
      lv         = getTickedPraosLedgerView (tickedPraosStateLedgerView cs)
      eta0       = STS.ticknStateEpochNonce $ SL.csTickn chainState
      vkhCold    = SL.hashKey tpraosCanBeLeaderColdVerKey
      rho'       = SL.mkSeed SL.seedEta slot eta0
      y'         = SL.mkSeed SL.seedL   slot eta0

      rho = VRF.evalCertified () rho' tpraosCanBeLeaderSignKeyVRF
      y   = VRF.evalCertified () y'   tpraosCanBeLeaderSignKeyVRF

      SL.GenDelegs dlgMap = SL.lvGenDelegs lv

  tickChainDepState TPraosConfig{..} (TickedPraosLedgerView lv) slot cds =
      TickedPraosState {
          tickedPraosStateTicked     = cs'
        , tickedPraosStateOrig       = cds
        , tickedPraosStateLedgerView = TickedPraosLedgerView lv
        }
    where
      cs' = SL.tickChainDepState
              shelleyGlobals
              lv
              (isNewEpoch tpraosEpochInfo slot (State.lastSlot cds))
              (State.currentState cds)
      shelleyGlobals = mkShelleyGlobals tpraosEpochInfo tpraosParams

  updateChainDepState TPraosConfig{..} b slot cs = do
      newCS <- SL.updateChainDepState shelleyGlobals lv b (tickedPraosStateTicked cs)
      return
        $ State.prune (fromIntegral k)
        $ State.append slot newCS (tickedPraosStateOrig cs)
    where
      SecurityParam k = tpraosSecurityParam tpraosParams
      shelleyGlobals = mkShelleyGlobals tpraosEpochInfo tpraosParams
      lv = getTickedPraosLedgerView (tickedPraosStateLedgerView cs)

  -- Rewind the chain state
  --
  -- We don't roll back to the exact slot since that slot might not have been
  -- filled; instead we roll back the the block just before it.
  rewindChainDepState _proxy _k = State.rewind . pointSlot

mkShelleyGlobals :: EpochInfo Identity -> TPraosParams -> SL.Globals
mkShelleyGlobals epochInfo TPraosParams {..} = SL.Globals {
      epochInfo                     = epochInfo
    , slotsPerKESPeriod             = tpraosSlotsPerKESPeriod
    , stabilityWindow               =
        computeStabilityWindow tpraosSecurityParam tpraosLeaderF
    , randomnessStabilisationWindow =
        computeRandomnessStabilisationWindow tpraosSecurityParam tpraosLeaderF
    , securityParameter             = k
    , maxKESEvo                     = tpraosMaxKESEvo
    , quorum                        = tpraosQuorum
    , maxMajorPV                    = tpraosMaxMajorPV
    , maxLovelaceSupply             = tpraosMaxLovelaceSupply
    , activeSlotCoeff               = tpraosLeaderF
    , networkId                     = tpraosNetworkId
    }
  where
    SecurityParam k = tpraosSecurityParam

-- | Check whether this node meets the leader threshold to issue a block.
meetsLeaderThreshold
  :: forall c.
     Crypto c
  => ConsensusConfig (TPraos c)
  -> LedgerView (TPraos c)
  -> SL.KeyHash 'SL.StakePool c
  -> CertifiedVRF (VRF c) SL.Seed
  -> Bool
meetsLeaderThreshold
  TPraosConfig { tpraosParams }
  SL.LedgerView { lvPoolDistr }
  keyHash
  certNat
    = SL.checkLeaderValue
        (VRF.certifiedOutput certNat)
        r
        (tpraosLeaderF tpraosParams)
  where
    SL.PoolDistr poolDistr = lvPoolDistr
    r = maybe 0 SL.individualPoolStake
        $ Map.lookup keyHash poolDistr

{-------------------------------------------------------------------------------
  Stability
-------------------------------------------------------------------------------}

-- | Calculate the stability window (e.g. the number of slots needed for a block
-- to become stable) from the security param and the active slot coefficient.
--
-- The value 3k/f is determined to be a suitabe value as per
-- https://docs.google.com/document/d/1B8BNMx8jVWRjYiUBOaI3jfZ7dQNvNTSDODvT5iOuYCU/edit#heading=h.qh2zcajmu6hm
computeStabilityWindow
  :: SecurityParam
  -> SL.ActiveSlotCoeff
  -> Word64
computeStabilityWindow securityParam asc =
    ceiling $ fromIntegral @_ @Double (3 * k) / fromRational (toRational f)
  where
    SecurityParam k = securityParam
    f = SL.intervalValue . SL.activeSlotVal $ asc

-- | Calculate the randomness stabilisation window from the security param and
-- the active slot coefficient.
--
-- The value 4k/f is determined to be a suitabe value as per
-- https://docs.google.com/document/d/1B8BNMx8jVWRjYiUBOaI3jfZ7dQNvNTSDODvT5iOuYCU/edit#heading=h.qh2zcajmu6hm
computeRandomnessStabilisationWindow
  :: SecurityParam
  -> SL.ActiveSlotCoeff
  -> Word64
computeRandomnessStabilisationWindow securityParam asc =
    ceiling $ fromIntegral @_ @Double (4 * k) / fromRational (toRational f)
  where
    SecurityParam k = securityParam
    f = SL.intervalValue . SL.activeSlotVal $ asc

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
      !(SL.Hash c (VerKeyVRF (VRF c)))
      !(SL.Hash c (VerKeyVRF (VRF c)))
  deriving (Generic)

deriving instance TPraosCrypto c => Show (TPraosCannotForge c)

tpraosCheckCanForge ::
     ConsensusConfig (TPraos c)
  -> SL.Hash c (VerKeyVRF (VRF c))
     -- ^ Precomputed hash of the VRF verification key
  -> SlotNo
  -> IsLeader (TPraos c)
  -> HotKey.KESInfo
  -> Either (TPraosCannotForge c) ()
tpraosCheckCanForge TPraosConfig{tpraosParams}
                    forgingVRFHash
                    curSlot
                    TPraosIsLeader{tpraosIsLeaderGenVRFHash}
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

instance (Condense toSign, TPraosCrypto c)
  => Condense (TPraosFields c toSign) where
  -- TODO Nicer 'Condense' instance
  condense = condense . tpraosToSign
