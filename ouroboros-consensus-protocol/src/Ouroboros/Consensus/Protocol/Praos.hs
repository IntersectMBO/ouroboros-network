{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE EmptyDataDecls          #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE PatternGuards           #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns            #-}

module Ouroboros.Consensus.Protocol.Praos (
    ConsensusConfig (..)
  , Praos
  , PraosCannotForge (..)
  , PraosCrypto
  , PraosFields (..)
  , PraosIsLeader (..)
  , PraosParams (..)
  , PraosState (..)
  , PraosToSign (..)
  , PraosValidationErr (..)
  , Ticked (..)
  , forgePraosFields
  , praosCheckCanForge
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import           Cardano.Crypto.VRF (hashVerKeyVRF)
import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Ledger.BaseTypes (ActiveSlotCoeff, Nonce, (⭒))
import qualified Cardano.Ledger.BaseTypes as SL
import           Cardano.Ledger.Crypto (Crypto, DSIGN, KES, StandardCrypto, VRF)
import           Cardano.Ledger.Hashes (EraIndependentTxBody)
import           Cardano.Ledger.Keys (KeyHash, KeyRole (BlockIssuer),
                     VKey (VKey), coerceKeyRole, hashKey)
import qualified Cardano.Ledger.Keys as SL
import           Cardano.Ledger.PoolDistr
                     (IndividualPoolStake (IndividualPoolStake))
import           Cardano.Ledger.Shelley.API (computeStabilityWindow)
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Ledger.Slot (Duration (Duration), (+*))
import           Cardano.Protocol.TPraos.BHeader (BoundedNatural (bvValue),
                     checkLeaderNatValue, prevHashToNonce)
import           Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod),
                     OCert (OCert), OCertSignable)
import qualified Cardano.Protocol.TPraos.OCert as OCert
import           Cardano.Slotting.EpochInfo (EpochInfo, epochInfoEpoch,
                     epochInfoFirst, hoistEpochInfo)
import           Cardano.Slotting.Slot (EpochNo (EpochNo), SlotNo (SlotNo),
                     WithOrigin, unSlotNo)
import           Cardano.Slotting.Time (SystemStart)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (decode, encode))
import           Control.Exception (throw)
import           Control.Monad (unless)
import           Control.Monad.Except (Except, runExcept, throwError)
import           Data.Coerce (coerce)
import           Data.Functor.Identity (runIdentity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Numeric.Natural (Natural)
import           Ouroboros.Consensus.Block (WithOrigin (NotOrigin))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.Ledger.Util (isNewEpoch)
import           Ouroboros.Consensus.Protocol.Praos.Common
import           Ouroboros.Consensus.Protocol.Praos.Header (HeaderBody)
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import           Ouroboros.Consensus.Protocol.Praos.VRF (InputVRF, mkInputVRF,
                     vrfLeaderValue, vrfNonceValue)
import           Ouroboros.Consensus.Protocol.TPraos
                     (ConsensusConfig (TPraosConfig, tpraosEpochInfo, tpraosParams))
import           Ouroboros.Consensus.Ticked (Ticked)
import           Ouroboros.Consensus.Util.Versioned (VersionDecoder (Decode),
                     decodeVersion, encodeVersion)

data Praos c

class
  ( Crypto c,
    DSIGN.Signable (DSIGN c) (OCertSignable c),
    DSIGN.Signable (DSIGN c) (SL.Hash c EraIndependentTxBody),
    KES.Signable (KES c) (HeaderBody c),
    VRF.Signable (VRF c) InputVRF
  ) =>
  PraosCrypto c

instance PraosCrypto StandardCrypto

{-------------------------------------------------------------------------------
  Fields required by Praos in the header
-------------------------------------------------------------------------------}

data PraosFields c toSign = PraosFields
  { praosSignature :: SL.SignedKES c toSign,
    praosToSign    :: toSign
  }
  deriving (Generic)

deriving instance
  (NoThunks toSign, PraosCrypto c) =>
  NoThunks (PraosFields c toSign)

deriving instance
  (Show toSign, PraosCrypto c) =>
  Show (PraosFields c toSign)

-- | Fields arising from praos execution which must be included in
-- the block signature.
data PraosToSign c = PraosToSign
  { -- | Verification key for the issuer of this block.
    praosToSignIssuerVK :: SL.VKey 'SL.BlockIssuer c,
    praosToSignVrfVK    :: SL.VerKeyVRF c,
    -- | Verifiable random value. This is used both to prove the issuer is
    -- eligible to issue a block, and to contribute to the evolving nonce.
    praosToSignVrfRes   :: SL.CertifiedVRF c InputVRF,
    -- | Lightweight delegation certificate mapping the cold (DSIGN) key to
    -- the online KES key.
    praosToSignOCert    :: OCert.OCert c
  }
  deriving (Generic)

instance PraosCrypto c => NoThunks (PraosToSign c)

deriving instance PraosCrypto c => Show (PraosToSign c)

forgePraosFields ::
  ( PraosCrypto c,
    SL.KESignable c toSign,
    Monad m
  ) =>
  HotKey c m ->
  CanBeLeader (Praos c) ->
  IsLeader (Praos c) ->
  (PraosToSign c -> toSign) ->
  m (PraosFields c toSign)
forgePraosFields
  hotKey
  PraosCanBeLeader
    { praosCanBeLeaderColdVerKey,
      praosCanBeLeaderSignKeyVRF,
      praosCanBeLeaderOpCert
    }
  PraosIsLeader {praosIsLeaderVrfRes}
  mkToSign = do
    signature <- HotKey.sign hotKey toSign
    return
      PraosFields
        { praosSignature = signature,
          praosToSign = toSign
        }
    where
      toSign = mkToSign signedFields

      signedFields =
        PraosToSign
          { praosToSignIssuerVK = praosCanBeLeaderColdVerKey,
            praosToSignVrfVK = VRF.deriveVerKeyVRF praosCanBeLeaderSignKeyVRF,
            praosToSignVrfRes = praosIsLeaderVrfRes,
            praosToSignOCert = praosCanBeLeaderOpCert
          }

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | Praos parameters that are node independent
data PraosParams = PraosParams
  { -- | See 'Globals.slotsPerKESPeriod'.
    praosSlotsPerKESPeriod :: !Word64,
    -- | Active slots coefficient. This parameter represents the proportion
    -- of slots in which blocks should be issued. This can be interpreted as
    -- the probability that a party holding all the stake will be elected as
    -- leader for a given slot.
    praosLeaderF           :: !SL.ActiveSlotCoeff,
    -- | See 'Globals.securityParameter'.
    praosSecurityParam     :: !SecurityParam,
    -- | Maximum number of KES iterations, see 'Globals.maxKESEvo'.
    praosMaxKESEvo         :: !Word64,
    -- | Quorum for update system votes and MIR certificates, see
    -- 'Globals.quorum'.
    praosQuorum            :: !Word64,
    -- | All blocks invalid after this protocol version, see
    -- 'Globals.maxMajorPV'.
    praosMaxMajorPV        :: !MaxMajorProtVer,
    -- | Maximum number of lovelace in the system, see
    -- 'Globals.maxLovelaceSupply'.
    praosMaxLovelaceSupply :: !Word64,
    -- | Testnet or mainnet?
    praosNetworkId         :: !SL.Network,
    -- | The system start, as projected from the chain's genesis block.
    praosSystemStart       :: !SystemStart
  }
  deriving (Generic, NoThunks)

-- | Assembled proof that the issuer has the right to issue a block in the
-- selected slot.
newtype PraosIsLeader c = PraosIsLeader
  { praosIsLeaderVrfRes :: SL.CertifiedVRF c InputVRF
  }
  deriving (Generic)

instance PraosCrypto c => NoThunks (PraosIsLeader c)

-- | Static configuration
data instance ConsensusConfig (Praos c) = PraosConfig
  { praosParams :: !PraosParams,
    praosEpochInfo :: !(EpochInfo (Except History.PastHorizonException))
    -- it's useful for this record to be EpochInfo and one other thing,
    -- because the one other thing can then be used as the
    -- PartialConsensConfig in the HFC instance.
  }
  deriving (Generic)

instance PraosCrypto c => NoThunks (ConsensusConfig (Praos c))

type PraosValidateView c = Views.HeaderView c

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

-- | Ledger view at a particular slot
newtype instance Ticked (Views.LedgerView c) = TickedPraosLedgerView
  { getTickedPraosLedgerView :: Views.LedgerView c }

-- | Praos consensus state.
--
-- We track the last slot and the counters for operational certificates, as well
-- as a series of nonces which get updated in different ways over the course of
-- an epoch.
data PraosState c = PraosState
  { praosStateLastSlot            :: !(WithOrigin SlotNo),
    -- | Operation Certificate counters
    praosStateOCertCounters       :: !(Map (KeyHash 'BlockIssuer c) Word64),
    -- | Evolving nonce
    praosStateEvolvingNonce       :: !Nonce,
    -- | Candidate nonce
    praosStateCandidateNonce      :: !Nonce,
    -- | Epoch nonce
    praosStateEpochNonce          :: !Nonce,
    -- | Nonce constructed from the hash of the previous block
    praosStateLabNonce            :: !Nonce,
    -- | Nonce corresponding to the LAB nonce of the last block of the previous
    -- epoch
    praosStateLastEpochBlockNonce :: !Nonce
  }
  deriving (Generic, Show, Eq)

instance PraosCrypto c => NoThunks (PraosState c)

instance PraosCrypto c => ToCBOR (PraosState c) where
  toCBOR = encode

instance PraosCrypto c => FromCBOR (PraosState c) where
  fromCBOR = decode

instance PraosCrypto c => Serialise (PraosState c) where
  encode
    PraosState
      { praosStateLastSlot,
        praosStateOCertCounters,
        praosStateEvolvingNonce,
        praosStateCandidateNonce,
        praosStateEpochNonce,
        praosStateLabNonce,
        praosStateLastEpochBlockNonce
      } =
      encodeVersion 0 $
        mconcat
          [ CBOR.encodeListLen 7,
            toCBOR praosStateLastSlot,
            toCBOR praosStateOCertCounters,
            toCBOR praosStateEvolvingNonce,
            toCBOR praosStateCandidateNonce,
            toCBOR praosStateEpochNonce,
            toCBOR praosStateLabNonce,
            toCBOR praosStateLastEpochBlockNonce
          ]

  decode =
    decodeVersion
      [(0, Decode decodePraosState)]
    where
      decodePraosState = do
        enforceSize "PraosState" 7
        PraosState
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
          <*> fromCBOR

-- | Ticked 'PraosState'
data instance Ticked (PraosState c) = TickedPraosState
  { tickedPraosStateChainDepState :: PraosState c,
    tickedPraosStateLedgerView :: Ticked (Views.LedgerView c)
  }

-- | Errors which we might encounter
data PraosValidationErr c
  = VRFKeyUnknown
      !(KeyHash SL.StakePool c) -- unknown VRF keyhash (not registered)
  | VRFKeyWrongVRFKey
      !(KeyHash SL.StakePool c) -- KeyHash of block issuer
      !(SL.Hash c (SL.VerKeyVRF c)) -- VRF KeyHash registered with stake pool
      !(SL.Hash c (SL.VerKeyVRF c)) -- VRF KeyHash from Header
  | VRFKeyBadProof
      !SlotNo -- Slot used for VRF calculation
      !Nonce -- Epoch nonce used for VRF calculation
      !(VRF.CertifiedVRF (VRF c) InputVRF) -- VRF calculated nonce value
  | VRFLeaderValueTooBig Natural Rational ActiveSlotCoeff
  | KESBeforeStartOCERT
      !KESPeriod -- OCert Start KES Period
      !KESPeriod -- Current KES Period
  | KESAfterEndOCERT
      !KESPeriod -- Current KES Period
      !KESPeriod -- OCert Start KES Period
      !Word64 -- Max KES Key Evolutions
  | CounterTooSmallOCERT
      !Word64 -- last KES counter used
      !Word64 -- current KES counter
  | -- | The KES counter has been incremented by more than 1
    CounterOverIncrementedOCERT
      !Word64 -- last KES counter used
      !Word64 -- current KES counter
  | InvalidSignatureOCERT
      !Word64 -- OCert counter
      !KESPeriod -- OCert KES period
      !String -- DSIGN error message
  | InvalidKesSignatureOCERT
      !Word -- current KES Period
      !Word -- KES start period
      !Word -- expected KES evolutions
      !String -- error message given by Consensus Layer
  | NoCounterForKeyHashOCERT
      !(KeyHash 'BlockIssuer c) -- stake pool key hash
  deriving (Generic)

deriving instance PraosCrypto c => Eq (PraosValidationErr c)

deriving instance PraosCrypto c => NoThunks (PraosValidationErr c)

deriving instance PraosCrypto c => Show (PraosValidationErr c)

instance PraosCrypto c => ConsensusProtocol (Praos c) where
  type ChainDepState (Praos c) = PraosState c
  type IsLeader (Praos c) = PraosIsLeader c
  type CanBeLeader (Praos c) = PraosCanBeLeader c
  type SelectView (Praos c) = PraosChainSelectView c
  type LedgerView (Praos c) = Views.LedgerView c
  type ValidationErr (Praos c) = PraosValidationErr c
  type ValidateView (Praos c) = PraosValidateView c

  protocolSecurityParam = praosSecurityParam . praosParams

  checkIsLeader
    cfg
    PraosCanBeLeader
      { praosCanBeLeaderSignKeyVRF,
        praosCanBeLeaderColdVerKey
      }
    slot
    cs =
      if meetsLeaderThreshold cfg lv (SL.coerceKeyRole vkhCold) rho
        then
          Just
            PraosIsLeader
              { praosIsLeaderVrfRes = coerce rho
              }
        else Nothing
      where
        chainState = tickedPraosStateChainDepState cs
        lv = getTickedPraosLedgerView $ tickedPraosStateLedgerView cs
        eta0 = praosStateEpochNonce chainState
        vkhCold = SL.hashKey praosCanBeLeaderColdVerKey
        rho' = mkInputVRF slot eta0

        rho = VRF.evalCertified () rho' praosCanBeLeaderSignKeyVRF

  -- Updating the chain dependent state for Praos.
  --
  -- If we are not in a new epoch, then nothing happens. If we are in a new
  -- epoch, we do two things:
  -- - Update the epoch nonce to the combination of the candidate nonce and the
  --   nonce derived from the last block of the previous epoch.
  -- - Update the "last block of previous epoch" nonce to the nonce derived from
  --   the last applied block.
  tickChainDepState
    PraosConfig {praosEpochInfo}
    (TickedPraosLedgerView lv)
    slot
    st =
      TickedPraosState
        { tickedPraosStateChainDepState = st',
          tickedPraosStateLedgerView = TickedPraosLedgerView lv
        }
      where
        newEpoch =
          isNewEpoch
            (History.toPureEpochInfo praosEpochInfo)
            (praosStateLastSlot st)
            slot
        st' =
          if newEpoch
            then
              st
                { praosStateEpochNonce =
                    praosStateCandidateNonce st
                      ⭒ praosStateLastEpochBlockNonce st,
                  praosStateLastEpochBlockNonce = praosStateLabNonce st
                }
            else st

  -- Validate and update the chain dependent state as a result of processing a
  -- new header.
  --
  -- This consists of:
  -- - Validate the VRF checks
  -- - Validate the KES checks
  -- - Call 'reupdateChainDepState'
  --
  updateChainDepState
    cfg@( PraosConfig
            PraosParams {praosLeaderF}
            _
          )
    b
    slot
    tcs = do
      -- First, we check the KES signature, which validates that the issuer is
      -- in fact who they say they are.
      validateKESSignature cfg lv (praosStateOCertCounters cs) b
      -- Then we examing the VRF proof, which confirms that they have the
      -- right to issue in this slot.
      validateVRFSignature (praosStateEpochNonce cs) lv praosLeaderF b
      -- Finally, we apply the changes from this header to the chain state.
      pure $ reupdateChainDepState cfg b slot tcs
      where
        lv = getTickedPraosLedgerView (tickedPraosStateLedgerView tcs)
        cs = tickedPraosStateChainDepState tcs

  -- Re-update the chain dependent state as a result of processing a header.
  --
  -- This consists of:
  -- - Update the last applied block hash.
  -- - Update the evolving and (potentially) candidate nonces based on the
  --   position in the epoch.
  -- - Update the operational certificate counter.
  reupdateChainDepState
    _cfg@( PraosConfig
             PraosParams {praosSecurityParam, praosLeaderF}
             ei
           )
    b
    slot
    tcs =
      cs
        { praosStateLastSlot = NotOrigin slot,
          praosStateLabNonce = prevHashToNonce (Views.hvPrevHash b),
          praosStateEvolvingNonce = newEvolvingNonce,
          praosStateCandidateNonce =
            if slot +* Duration stabilityWindow < firstSlotNextEpoch
              then newEvolvingNonce
              else praosStateCandidateNonce cs,
          praosStateOCertCounters =
            Map.insert hk n $ praosStateOCertCounters cs
        }
      where
        epochInfoWithErr =
          hoistEpochInfo
            (either throw pure . runExcept)
            ei
        firstSlotNextEpoch = runIdentity $ do
          EpochNo currentEpochNo <- epochInfoEpoch epochInfoWithErr slot
          let nextEpoch = EpochNo $ currentEpochNo + 1
          epochInfoFirst epochInfoWithErr nextEpoch
        cs = tickedPraosStateChainDepState tcs
        stabilityWindow =
          computeStabilityWindow (maxRollbacks praosSecurityParam) praosLeaderF
        eta = vrfNonceValue (Proxy @c) $ Views.hvVrfRes b
        newEvolvingNonce = praosStateEvolvingNonce cs ⭒ eta
        OCert _ n _ _ = Views.hvOCert b
        hk = hashKey $ Views.hvVK b

-- | Check whether this node meets the leader threshold to issue a block.
meetsLeaderThreshold ::
  forall c.
  PraosCrypto c =>
  ConsensusConfig (Praos c) ->
  LedgerView (Praos c) ->
  SL.KeyHash 'SL.StakePool c ->
  VRF.CertifiedVRF (VRF c) InputVRF ->
  Bool
meetsLeaderThreshold
  PraosConfig {praosParams}
  Views.LedgerView {Views.lvPoolDistr}
  keyHash
  rho =
    checkLeaderNatValue
      (vrfLeaderValue (Proxy @c) rho)
      r
      (praosLeaderF praosParams)
    where
      SL.PoolDistr poolDistr = lvPoolDistr
      r =
        maybe 0 SL.individualPoolStake $
          Map.lookup keyHash poolDistr

validateVRFSignature ::
  forall c.
  ( PraosCrypto c
  ) =>
  Nonce ->
  Views.LedgerView c ->
  ActiveSlotCoeff ->
  Views.HeaderView c ->
  Except (PraosValidationErr c) ()
validateVRFSignature eta0 (Views.lvPoolDistr -> SL.PoolDistr pd) f b = do
  case Map.lookup hk pd of
    Nothing -> throwError $ VRFKeyUnknown hk
    Just (IndividualPoolStake sigma vrfHK) -> do
      vrfHK == hashVerKeyVRF vrfK
        ?! VRFKeyWrongVRFKey hk vrfHK (hashVerKeyVRF vrfK)
      VRF.verifyCertified
        ()
        vrfK
        (mkInputVRF slot eta0)
        vrfCert
        ?! VRFKeyBadProof slot eta0 vrfCert
      checkLeaderNatValue vrfLeaderVal sigma f
        ?! VRFLeaderValueTooBig (bvValue vrfLeaderVal) sigma f
  where
    hk = coerceKeyRole . hashKey . Views.hvVK $ b
    vrfK = Views.hvVrfVK b
    vrfCert = Views.hvVrfRes b
    vrfLeaderVal = vrfLeaderValue (Proxy @c) vrfCert
    slot = Views.hvSlotNo b

validateKESSignature ::
  PraosCrypto c =>
  ConsensusConfig (Praos c) ->
  LedgerView (Praos c) ->
  Map (KeyHash 'BlockIssuer c) Word64 ->
  Views.HeaderView c ->
  Except (PraosValidationErr c) ()
validateKESSignature
  _cfg@( PraosConfig
           PraosParams {praosMaxKESEvo, praosSlotsPerKESPeriod}
           _ei
         )
  Views.LedgerView {Views.lvPoolDistr}
  ocertCounters
  b = do
    c0 <= kp ?! KESBeforeStartOCERT c0 kp
    kp_ < c0_ + fromIntegral praosMaxKESEvo ?! KESAfterEndOCERT kp c0 praosMaxKESEvo

    let t = if kp_ >= c0_ then kp_ - c0_ else 0
    -- this is required to prevent an arithmetic underflow, in the case of kp_ <
    -- c0_ we get the above `KESBeforeStartOCERT` failure in the transition.

    DSIGN.verifySignedDSIGN () vkcold (OCert.ocertToSignable oc) tau ?!:
      InvalidSignatureOCERT n c0
    KES.verifySignedKES () vk_hot t (Views.hvSigned b) (Views.hvSignature b) ?!:
      InvalidKesSignatureOCERT kp_ c0_ t

    case currentIssueNo of
      Nothing -> do
        throwError $ NoCounterForKeyHashOCERT hk
      Just m -> do
        m <= n ?! CounterTooSmallOCERT m n
        n <= m + 1 ?! CounterOverIncrementedOCERT m n
    where
      oc@(OCert vk_hot n c0@(KESPeriod c0_) tau) = Views.hvOCert b
      (VKey vkcold) = Views.hvVK b
      SlotNo s = Views.hvSlotNo b
      hk = hashKey $ Views.hvVK b
      kp@(KESPeriod kp_) =
        if praosSlotsPerKESPeriod == 0
          then error "kesPeriod: slots per KES period was set to zero"
          else KESPeriod . fromIntegral $ s `div` praosSlotsPerKESPeriod

      currentIssueNo :: Maybe Word64
      currentIssueNo
        | Map.member hk ocertCounters = Map.lookup hk ocertCounters
        | Set.member (coerceKeyRole hk) (Map.keysSet $ SL.unPoolDistr lvPoolDistr) =
          Just 0
        | otherwise = Nothing

{-------------------------------------------------------------------------------
  CannotForge
-------------------------------------------------------------------------------}

-- | Expresses that, whilst we believe ourselves to be a leader for this slot,
-- we are nonetheless unable to forge a block.
data PraosCannotForge c
  = -- | The KES key in our operational certificate can't be used because the
    -- current (wall clock) period is before the start period of the key.
    -- current KES period.
    --
    -- Note: the opposite case, i.e., the wall clock period being after the
    -- end period of the key, is caught when trying to update the key in
    -- 'updateForgeState'.
    PraosCannotForgeKeyNotUsableYet
      !OCert.KESPeriod
      -- ^ Current KES period according to the wallclock slot, i.e., the KES
      -- period in which we want to use the key.
      !OCert.KESPeriod
      -- ^ Start KES period of the KES key.
  deriving (Generic)

deriving instance PraosCrypto c => Show (PraosCannotForge c)

praosCheckCanForge ::
  ConsensusConfig (Praos c) ->
  SlotNo ->
  HotKey.KESInfo ->
  Either (PraosCannotForge c) ()
praosCheckCanForge
  PraosConfig {praosParams}
  curSlot
  kesInfo
    | let startPeriod = HotKey.kesStartPeriod kesInfo,
      startPeriod > wallclockPeriod =
      throwError $ PraosCannotForgeKeyNotUsableYet wallclockPeriod startPeriod
    | otherwise =
      return ()
    where
      -- The current wallclock KES period
      wallclockPeriod :: OCert.KESPeriod
      wallclockPeriod =
        OCert.KESPeriod $
          fromIntegral $
            unSlotNo curSlot `div` praosSlotsPerKESPeriod praosParams


{-------------------------------------------------------------------------------
  PraosProtocolSupportsNode
-------------------------------------------------------------------------------}

instance PraosCrypto c => PraosProtocolSupportsNode (Praos c) where
  type PraosProtocolSupportsNodeCrypto (Praos c) = c

  getPraosNonces _prx cdst =
      PraosNonces {
          candidateNonce   = praosStateCandidateNonce
        , epochNonce       = praosStateEpochNonce
        , evolvingNonce    = praosStateEvolvingNonce
        , labNonce         = praosStateLabNonce
        , previousLabNonce = praosStateLastEpochBlockNonce
        }
    where
      PraosState {
          praosStateCandidateNonce
        , praosStateEpochNonce
        , praosStateEvolvingNonce
        , praosStateLabNonce
        , praosStateLastEpochBlockNonce
        } = cdst

  getOpCertCounters _prx cdst =
      praosStateOCertCounters
    where
      PraosState {
          praosStateOCertCounters
        } = cdst

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

(?!) :: Bool -> e -> Except e ()
a ?! b = unless a $ throwError b

infix 1 ?!

(?!:) :: Either e1 a -> (e1 -> e2) -> Except e2 ()
(Right _) ?!: _ = pure ()
(Left e1) ?!: f = throwError $ f e1

infix 1 ?!:
