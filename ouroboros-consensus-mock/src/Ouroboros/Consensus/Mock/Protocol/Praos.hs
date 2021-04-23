{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

-- | Proof of concept implementation of Praos
module Ouroboros.Consensus.Mock.Protocol.Praos (
    HotKey (..)
  , HotKeyEvolutionError (..)
  , Praos
  , PraosChainDepState (..)
  , PraosEvolvingStake (..)
  , PraosExtraFields (..)
  , PraosFields (..)
  , PraosParams (..)
  , emptyPraosEvolvingStake
  , evolveKey
  , forgePraosFields
    -- * Tags
  , PraosCrypto (..)
  , PraosMockCrypto
  , PraosStandardCrypto
  , PraosValidateView (..)
  , PraosValidationError (..)
  , praosValidateView
    -- * Type instances
  , BlockInfo (..)
  , ConsensusConfig (..)
  , Ticked (..)
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), serializeEncoding')
import           Codec.CBOR.Decoding (decodeListLenOf)
import           Codec.CBOR.Encoding (encodeListLen)
import           Codec.Serialise (Serialise (..))
import           Control.Monad (unless)
import           Control.Monad.Except (throwError)
import           Control.Monad.Identity (runIdentity)
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks (..))
import           Numeric.Natural

import           Cardano.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Cardano.Crypto.Hash.Class (HashAlgorithm (..), hashToBytes,
                     hashWithSerialiser, sizeHash)
import           Cardano.Crypto.Hash.MD5 (MD5)
import           Cardano.Crypto.Hash.SHA256 (SHA256)
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.KES.Mock
import           Cardano.Crypto.KES.Simple
import           Cardano.Crypto.Util
import           Cardano.Crypto.VRF.Class
import           Cardano.Crypto.VRF.Mock (MockVRF)
import           Cardano.Crypto.VRF.Simple (SimpleVRF)
import           Cardano.Slotting.EpochInfo

import           Data.Maybe (fromMaybe)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Mock.Ledger.Stake
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Fields required by Praos in the header
-------------------------------------------------------------------------------}

-- | The fields that Praos required in the header
data PraosFields c toSign = PraosFields {
      praosSignature   :: SignedKES (PraosKES c) toSign
    , praosExtraFields :: PraosExtraFields c
    }
  deriving (Generic)

instance (PraosCrypto c, Typeable toSign) => NoThunks (PraosFields c toSign)

-- | Fields that should be included in the signature
data PraosExtraFields c = PraosExtraFields {
      praosCreator :: CoreNodeId
    , praosRho     :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosY       :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    }
  deriving (Generic)

instance PraosCrypto c => NoThunks (PraosExtraFields c)

data PraosValidateView c =
    forall signed. Cardano.Crypto.KES.Class.Signable (PraosKES c) signed
                => PraosValidateView (PraosFields c signed) signed

-- | Convenience constructor for 'PraosValidateView'
praosValidateView :: ( SignedHeader hdr
                     , Cardano.Crypto.KES.Class.Signable (PraosKES c) (Signed hdr)
                     )
                  => (hdr -> PraosFields c (Signed hdr))
                  -> (hdr -> PraosValidateView c)
praosValidateView getFields hdr =
    PraosValidateView (getFields hdr) (headerSigned hdr)

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

data HotKey c =
    HotKey
      !Period  -- ^ Absolute period of the KES key
      !(SignKeyKES (PraosKES c))
  | HotKeyPoisoned
  deriving (Generic)

instance PraosCrypto c => NoThunks (HotKey c)
deriving instance PraosCrypto c => Show (HotKey c)

-- | The 'HotKey' could not be evolved to the given 'Period'.
newtype HotKeyEvolutionError = HotKeyEvolutionError Period
  deriving (Show)

-- | To be used in conjunction with, e.g., 'updateMVar'.
--
-- NOTE: when the key's period is after the target period, we shouldn't use
-- it, but we currently do. In real TPraos we check this in
-- 'tpraosCheckCanForge'.
evolveKey ::
     PraosCrypto c
  => SlotNo
  -> HotKey c
  -> (HotKey c, UpdateInfo (HotKey c) HotKeyEvolutionError)
evolveKey slotNo hotKey = case hotKey of
    HotKey keyPeriod oldKey
      | keyPeriod >= targetPeriod
      -> (hotKey, Updated hotKey)
      | otherwise
      -> case updateKES () oldKey keyPeriod of
           Nothing     ->
             (HotKeyPoisoned, UpdateFailed $ HotKeyEvolutionError targetPeriod)
           Just newKey ->
             evolveKey slotNo (HotKey (keyPeriod + 1) newKey)
    HotKeyPoisoned ->
      (HotKeyPoisoned, UpdateFailed $ HotKeyEvolutionError targetPeriod)
  where
   targetPeriod :: Period
   targetPeriod = fromIntegral $ unSlotNo slotNo

forgePraosFields :: ( PraosCrypto c
                    , Cardano.Crypto.KES.Class.Signable (PraosKES c) toSign
                    , HasCallStack
                    )
                 => PraosProof c
                 -> HotKey c
                 -> (PraosExtraFields c -> toSign)
                 -> PraosFields c toSign
forgePraosFields PraosProof{..} hotKey mkToSign =
    case hotKey of
      HotKey kesPeriod key -> PraosFields {
          praosSignature   = signedKES () kesPeriod (mkToSign signedFields) key
        , praosExtraFields = signedFields
        }
      HotKeyPoisoned -> error "trying to sign with a poisoned key"
  where
    signedFields = PraosExtraFields {
        praosCreator = praosLeader
      , praosRho     = praosProofRho
      , praosY       = praosProofY
      }

{-------------------------------------------------------------------------------
  Mock stake distribution
-------------------------------------------------------------------------------}

-- | An association from epoch to stake distributions.
--
-- Should be used when checking if someone is the leader of a particular slot.
-- This is sufficiently good for a mock protocol as far as consensus is
-- concerned. It is not strictly necessary that the stake distribution is
-- computed from previous epochs, as we just need to consider that:
--
-- 1) an attacker cannot influence it.
-- 2) all the nodes agree on the same value for each Slot.
--
-- Each pair stores the stake distribution established by the end of the epoch
-- in the first item of the pair. See 'latestEvolvedStakeDistAsOfEpoch' for the
-- intended interface.
--
-- If no value is returned, that means we are checking the stake before any
-- changes have happened so we should consult instead the 'praosInitialStake'.
newtype PraosEvolvingStake = PraosEvolvingStake (Map EpochNo StakeDist)
  deriving stock Show
  deriving newtype NoThunks

emptyPraosEvolvingStake :: PraosEvolvingStake
emptyPraosEvolvingStake = PraosEvolvingStake Map.empty

latestEvolvedStakeDistAsOfEpoch :: PraosEvolvingStake -> EpochNo -> Maybe StakeDist
latestEvolvedStakeDistAsOfEpoch (PraosEvolvingStake x) e =
  fmap snd . Map.lookupMax . fst $ Map.split e x

{-------------------------------------------------------------------------------
  Praos specific types
-------------------------------------------------------------------------------}

data VRFType = NONCE | TEST
    deriving (Show, Eq, Ord, Generic, NoThunks)

instance Serialise VRFType

instance ToCBOR VRFType where
  -- This is a cheat, and at some point we probably want to decide on Serialise/ToCBOR
  toCBOR = encode

data PraosProof c = PraosProof {
      praosProofRho :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosProofY   :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosLeader   :: CoreNodeId
    }

data PraosValidationError c =
      PraosInvalidSlot SlotNo SlotNo
    | PraosUnknownCoreId CoreNodeId
    | PraosInvalidSig String (VerKeyKES (PraosKES c)) Natural (SigKES (PraosKES c))
    | PraosInvalidCert (VerKeyVRF (PraosVRF c)) (Natural, SlotNo, VRFType) Natural (CertVRF (PraosVRF c))
    | PraosInsufficientStake Double Natural
    deriving (Generic)

-- We override 'showTypeOf' to make sure to show @c@
instance PraosCrypto c => NoThunks (PraosValidationError c) where
  showTypeOf _ = show $ typeRep (Proxy @(PraosValidationError c))

deriving instance PraosCrypto c => Show (PraosValidationError c)
deriving instance PraosCrypto c => Eq   (PraosValidationError c)

data BlockInfo c = BlockInfo
    { biSlot :: !SlotNo
    , biRho  :: !(CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType))
    }
  deriving (Generic)

deriving instance PraosCrypto c => Show     (BlockInfo c)
deriving instance PraosCrypto c => Eq       (BlockInfo c)
deriving instance PraosCrypto c => NoThunks (BlockInfo c)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data Praos c

-- | Praos parameters that are node independent
data PraosParams = PraosParams {
      praosLeaderF       :: !Double
    , praosSecurityParam :: !SecurityParam
    , praosSlotsPerEpoch :: !Word64
    }
  deriving (Generic, NoThunks)

data instance ConsensusConfig (Praos c) = PraosConfig
  { praosParams        :: !PraosParams
  , praosInitialEta    :: !Natural
  , praosInitialStake  :: !StakeDist
  , praosEvolvingStake :: !PraosEvolvingStake
  , praosSignKeyVRF    :: !(SignKeyVRF (PraosVRF c))
  , praosVerKeys       :: !(Map CoreNodeId (VerKeyKES (PraosKES c), VerKeyVRF (PraosVRF c)))
  }
  deriving (Generic)

newtype PraosChainDepState c = PraosChainDepState {
      praosHistory :: [BlockInfo c]
    }
  deriving stock   (Eq, Show)
  deriving newtype (NoThunks, Serialise)

-- | Ticking the Praos chain dep state has no effect
--
-- For the real Praos implementation, ticking is crucial, as it determines the
-- point where the "nonce under construction" is swapped out for the "active"
-- nonce. However, for the mock implementation, we keep the full history, and
-- choose the right nonce from that; this means that ticking has no effect.
--
-- We do however need access to the ticked stake distribution.
data instance Ticked (PraosChainDepState c) = TickedPraosChainDepState {
      tickedPraosLedgerView      :: Ticked (LedgerView (Praos c))
    , untickedPraosChainDepState :: PraosChainDepState c
    }

instance PraosCrypto c => ConsensusProtocol (Praos c) where
  protocolSecurityParam = praosSecurityParam . praosParams

  type LedgerView    (Praos c) = ()
  type IsLeader      (Praos c) = PraosProof           c
  type ValidationErr (Praos c) = PraosValidationError c
  type ValidateView  (Praos c) = PraosValidateView    c
  type ChainDepState (Praos c) = PraosChainDepState   c
  type CanBeLeader   (Praos c) = CoreNodeId

  checkIsLeader cfg@PraosConfig{..} nid slot (TickedPraosChainDepState _u  cds) =
      if fromIntegral (getOutputVRFNatural (certifiedOutput y)) < t
      then Just PraosProof {
               praosProofRho = rho
             , praosProofY   = y
             , praosLeader   = nid
             }
      else Nothing
    where
      (rho', y', t) = rhoYT cfg (praosHistory cds) slot nid
      rho = evalCertified () rho' praosSignKeyVRF
      y   = evalCertified () y'   praosSignKeyVRF

  tickChainDepState _ lv _ = TickedPraosChainDepState lv

  updateChainDepState cfg@PraosConfig{..}
                      (PraosValidateView PraosFields{..} toSign)
                      slot
                      (TickedPraosChainDepState TickedTrivial cds) = do
    let PraosExtraFields {..} = praosExtraFields
        nid = praosCreator

    -- check that the new block advances time
    case praosHistory cds of
        (c : _)
            | biSlot c >= slot -> throwError $ PraosInvalidSlot slot (biSlot c)
        _                      -> return ()

    -- check that block creator is a known core node
    (vkKES, vkVRF) <- case Map.lookup nid praosVerKeys of
        Nothing  -> throwError $ PraosUnknownCoreId nid
        Just vks -> return vks

    -- verify block signature
    case verifySignedKES
           ()
           vkKES
           (fromIntegral $ unSlotNo slot)
           toSign
           praosSignature of
       Right () -> return ()
       Left err -> throwError $ PraosInvalidSig
                                  err
                                  vkKES
                                  (fromIntegral $ unSlotNo slot)
                                  (getSig praosSignature)

    let (rho', y', t) = rhoYT cfg (praosHistory cds) slot nid

    -- verify rho proof
    unless (verifyCertified () vkVRF rho' praosRho) $
        throwError $ PraosInvalidCert
            vkVRF
            rho'
            (getOutputVRFNatural (certifiedOutput praosRho))
            (certifiedProof praosRho)

    -- verify y proof
    unless (verifyCertified () vkVRF y' praosY) $
        throwError $ PraosInvalidCert
            vkVRF
            y'
            (getOutputVRFNatural (certifiedOutput praosY))
            (certifiedProof praosY)

    -- verify stake
    unless (fromIntegral (getOutputVRFNatural (certifiedOutput praosY)) < t) $
        throwError $ PraosInsufficientStake t $
                       getOutputVRFNatural (certifiedOutput praosY)

    -- "store" a block by adding it to the chain dependent state
    let !bi = BlockInfo
            { biSlot  = slot
            , biRho   = praosRho
            }

    return $ PraosChainDepState $ bi : praosHistory cds

  reupdateChainDepState _
                        (PraosValidateView PraosFields{..} _)
                        slot
                        (TickedPraosChainDepState TickedTrivial cds) =
    let PraosExtraFields{..} = praosExtraFields
        !bi = BlockInfo
            { biSlot  = slot
            , biRho   = praosRho
            }
    in PraosChainDepState $ bi : praosHistory cds

  -- (Standard) Praos uses the standard chain selection rule, so no need to
  -- override (though see note regarding clock skew).

instance PraosCrypto c => NoThunks (ConsensusConfig (Praos c))
  -- use generic instance

slotEpoch :: ConsensusConfig (Praos c) -> SlotNo -> EpochNo
slotEpoch PraosConfig{..} s =
    runIdentity $ epochInfoEpoch epochInfo s
  where
    epochInfo = fixedSizeEpochInfo (EpochSize praosSlotsPerEpoch)
    PraosParams{..} = praosParams

blockInfoEpoch :: ConsensusConfig (Praos c) -> BlockInfo c -> EpochNo
blockInfoEpoch l = slotEpoch l . biSlot

epochFirst :: ConsensusConfig (Praos c) -> EpochNo -> SlotNo
epochFirst PraosConfig{..} e =
    runIdentity $ epochInfoFirst epochInfo e
  where
    epochInfo = fixedSizeEpochInfo (EpochSize praosSlotsPerEpoch)
    PraosParams{..} = praosParams

infosSlice :: SlotNo -> SlotNo -> [BlockInfo c] -> [BlockInfo c]
infosSlice from to xs = takeWhile (\b -> biSlot b >= from)
                      $ dropWhile (\b -> biSlot b > to) xs

infosEta :: forall c. (PraosCrypto c)
         => ConsensusConfig (Praos c)
         -> [BlockInfo c]
         -> EpochNo
         -> Natural
infosEta l _  0 = praosInitialEta l
infosEta l xs e =
    let e'   = e - 1
        eta' = infosEta l xs e'
        from = epochFirst l e'
        n    = div (2 * praosSlotsPerEpoch) 3
        to   = SlotNo $ unSlotNo from + n
        rhos = reverse [biRho b | b <- infosSlice from to xs]
    in  bytesToNatural
          . hashToBytes
          $ hashWithSerialiser @(PraosHash c) toCBOR (eta', e, rhos)
  where
    PraosParams{..} = praosParams l

infosStake :: ConsensusConfig (Praos c) -> [BlockInfo c] -> EpochNo -> StakeDist
infosStake s@PraosConfig{..} xs e = case ys of
    []                  -> praosInitialStake
    (BlockInfo{..} : _) -> biStake
  where
    e' = if e >= 2 then EpochNo (unEpochNo e - 2) else 0
    ys = dropWhile (\b -> blockInfoEpoch s b > e') xs

phi :: ConsensusConfig (Praos c) -> Rational -> Double
phi PraosConfig{..} alpha = 1 - (1 - praosLeaderF) ** fromRational alpha
  where
    PraosParams{..} = praosParams

leaderThreshold :: forall c. PraosCrypto c
                => ConsensusConfig (Praos c)
                -> [BlockInfo c]
                -> SlotNo
                -> CoreNodeId
                -> Double
leaderThreshold config _blockInfos s n =
    let
      alpha = stakeWithDefault 0 n
        $ fromMaybe (praosInitialStake config)
        $ latestEvolvedStakeDistAsOfEpoch (praosEvolvingStake config) (slotEpoch config s)
    in
      -- 2^(l_VRF * 8) * ϕ_f(αᵢ)
      -- the 8 factor converts from bytes to bits.
      2 ^ (sizeHash (Proxy :: Proxy (PraosHash c)) * 8) * phi config alpha

-- |Compute the rho, y and Tᵢ parameters for a given slot.
rhoYT :: PraosCrypto c
      => ConsensusConfig (Praos c)
      -> [BlockInfo c]
      -> SlotNo
      -> CoreNodeId
      -> ( (Natural, SlotNo, VRFType)
         , (Natural, SlotNo, VRFType)
         , Double
         )
rhoYT st xs s nid =
    let e   = slotEpoch st s
        eta = infosEta st xs e
        rho = (eta, s, NONCE)
        y   = (eta, s, TEST)
        t   = leaderThreshold st xs s nid
    in  (rho, y, t)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

class ( KESAlgorithm  (PraosKES  c)
      , VRFAlgorithm  (PraosVRF  c)
      , HashAlgorithm (PraosHash c)
      , Typeable c
      , Typeable (PraosVRF c)
      , Condense (SigKES (PraosKES c))
      , Cardano.Crypto.VRF.Class.Signable (PraosVRF c) (Natural, SlotNo, VRFType)
      , ContextKES (PraosKES c) ~ ()
      , ContextVRF (PraosVRF c) ~ ()
      ) => PraosCrypto (c :: Type) where
  type family PraosKES  c :: Type
  type family PraosVRF  c :: Type
  type family PraosHash c :: Type

data PraosStandardCrypto
data PraosMockCrypto

instance PraosCrypto PraosStandardCrypto where
  type PraosKES  PraosStandardCrypto = SimpleKES Ed448DSIGN 1000
  type PraosVRF  PraosStandardCrypto = SimpleVRF
  type PraosHash PraosStandardCrypto = SHA256

instance PraosCrypto PraosMockCrypto where
  type PraosKES  PraosMockCrypto = MockKES 10000
  type PraosVRF  PraosMockCrypto = MockVRF
  type PraosHash PraosMockCrypto = MD5

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance PraosCrypto c => Condense (PraosFields c toSign) where
   condense PraosFields{..} = condense praosSignature

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance PraosCrypto c => Serialise (BlockInfo c) where
  encode BlockInfo {..} = mconcat
    [ encodeListLen 2
    , encode biSlot
    , toCBOR biRho
    ]
  decode = do
    decodeListLenOf 2
    biSlot  <- decode
    biRho   <- fromCBOR
    return BlockInfo {..}

instance SignableRepresentation (Natural, SlotNo, VRFType) where
  getSignableRepresentation = serializeEncoding' . toCBOR
