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
    Praos
  , PraosFields(..)
  , PraosExtraFields(..)
  , PraosParams(..)
  , PraosChainDepState(..)
  , HotKey(..)
  , forgePraosFields
  , evolveKey
    -- * Tags
  , PraosCrypto(..)
  , PraosStandardCrypto
  , PraosMockCrypto
  , PraosValidateView(..)
  , praosValidateView
  , PraosValidationError(..)
    -- * Type instances
  , ConsensusConfig(..)
  , BlockInfo(..)
  , Ticked(..)
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Codec.CBOR.Decoding (decodeListLenOf)
import           Codec.CBOR.Encoding (encodeListLen)
import           Codec.Serialise (Serialise (..))
import           Control.Monad (unless)
import           Control.Monad.Except (throwError)
import           Control.Monad.Identity (runIdentity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Typeable
import           Data.Void
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Numeric.Natural

import           Cardano.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Cardano.Crypto.Hash.Class (HashAlgorithm (..), fromHash, hash)
import           Cardano.Crypto.Hash.MD5 (MD5)
import           Cardano.Crypto.Hash.SHA256 (SHA256)
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.KES.Mock
import           Cardano.Crypto.KES.Simple
import           Cardano.Crypto.VRF.Class
import           Cardano.Crypto.VRF.Mock (MockVRF)
import           Cardano.Crypto.VRF.Simple (SimpleVRF)
import           Cardano.Prelude (NoUnexpectedThunks (..), fromMaybe)
import           Cardano.Slotting.EpochInfo

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

instance (PraosCrypto c, Typeable toSign) => NoUnexpectedThunks (PraosFields c toSign)
  -- use generic instance

-- | Fields that should be included in the signature
data PraosExtraFields c = PraosExtraFields {
      praosCreator :: CoreNodeId
    , praosRho     :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosY       :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    }
  deriving (Generic)

instance PraosCrypto c => NoUnexpectedThunks (PraosExtraFields c)
  -- use generic instance

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

newtype HotKey c = HotKey (SignKeyKES (PraosKES c))
  deriving (Generic)

deriving instance PraosCrypto c => Show (HotKey c)

-- We override 'showTypeOf' to make sure to show @c@
instance PraosCrypto c => NoUnexpectedThunks (HotKey c) where
  showTypeOf _ = show $ typeRep (Proxy @(HotKey c))

evolveKey :: (Monad m, PraosCrypto c)
          => SlotNo -> HotKey c -> m (HotKey c)
evolveKey slotNo (HotKey oldKey) = do
    let newKey = fromMaybe (error "evolveKey: updateKES failed") $
                 updateKES () oldKey kesPeriod
    return $ HotKey newKey
  where
   kesPeriod :: Period
   kesPeriod = fromIntegral $ unSlotNo slotNo

forgePraosFields :: ( PraosCrypto c
                    , Cardano.Crypto.KES.Class.Signable (PraosKES c) toSign
                    )
                 => ConsensusConfig (Praos c)
                 -> HotKey c
                 -> PraosProof c
                 -> (PraosExtraFields c -> toSign)
                 -> PraosFields c toSign
forgePraosFields PraosConfig{..} (HotKey key) PraosProof{..} mkToSign =
    PraosFields {
        praosSignature   = signature
      , praosExtraFields = signedFields
      }
  where
    signedFields = PraosExtraFields {
        praosCreator = praosLeader
      , praosRho     = praosProofRho
      , praosY       = praosProofY
      }

    signature = signedKES () kesPeriod (mkToSign signedFields) key

    -- For the mock implementation, we consider the KES period to be the slot.
    -- In reality, there will be some kind of constant slotsPerPeriod factor.
    -- (Put another way, we consider slotsPerPeriod to be 1 here.)
    kesPeriod :: Period
    kesPeriod = fromIntegral $ unSlotNo praosProofSlot

{-------------------------------------------------------------------------------
  Praos specific types
-------------------------------------------------------------------------------}

data VRFType = NONCE | TEST
    deriving (Show, Eq, Ord, Generic, NoUnexpectedThunks)

instance Serialise VRFType
  -- use generic instance

instance ToCBOR VRFType where
  -- This is a cheat, and at some point we probably want to decide on Serialise/ToCBOR
  toCBOR = encode

deriving instance PraosCrypto c => Show (PraosExtraFields c)
deriving instance PraosCrypto c => Eq   (PraosExtraFields c)

deriving instance PraosCrypto c => Show (PraosFields c toSign)
deriving instance PraosCrypto c => Eq   (PraosFields c toSign)

data PraosProof c = PraosProof {
      praosProofRho  :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosProofY    :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosLeader    :: CoreNodeId
    , praosProofSlot :: SlotNo
    }

data PraosValidationError c =
      PraosInvalidSlot SlotNo SlotNo
    | PraosUnknownCoreId CoreNodeId
    | PraosInvalidSig String (VerKeyKES (PraosKES c)) Natural (SigKES (PraosKES c))
    | PraosInvalidCert (VerKeyVRF (PraosVRF c)) (Natural, SlotNo, VRFType) Natural (CertVRF (PraosVRF c))
    | PraosInsufficientStake Double Natural
    deriving (Generic)

-- We override 'showTypeOf' to make sure to show @c@
instance PraosCrypto c => NoUnexpectedThunks (PraosValidationError c) where
  showTypeOf _ = show $ typeRep (Proxy @(PraosValidationError c))

deriving instance PraosCrypto c => Show (PraosValidationError c)
deriving instance PraosCrypto c => Eq   (PraosValidationError c)

data BlockInfo c = BlockInfo
    { biSlot  :: !SlotNo
    , biRho   :: !(CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType))
    , biStake :: !StakeDist
    }
  deriving (Generic)

deriving instance PraosCrypto c => Show               (BlockInfo c)
deriving instance PraosCrypto c => Eq                 (BlockInfo c)
deriving instance PraosCrypto c => NoUnexpectedThunks (BlockInfo c)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data Praos c

-- | Praos parameters that are node independent
data PraosParams = PraosParams {
      praosLeaderF       :: !Double
    , praosSecurityParam :: !SecurityParam
    , praosSlotsPerEpoch :: !Word64
    , praosLifetimeKES   :: !Natural
    }
  deriving (Generic, NoUnexpectedThunks)

data instance ConsensusConfig (Praos c) = PraosConfig
  { praosParams       :: !PraosParams
  , praosInitialEta   :: !Natural
  , praosInitialStake :: !StakeDist
  , praosSignKeyVRF   :: !(SignKeyVRF (PraosVRF c))
  , praosVerKeys      :: !(Map CoreNodeId (VerKeyKES (PraosKES c), VerKeyVRF (PraosVRF c)))
  }
  deriving (Generic)

instance PraosCrypto c => ChainSelection (Praos c) where
  -- Use defaults

instance PraosCrypto c => HasChainIndepState (Praos c) where
  type ChainIndepState (Praos c) = HotKey c
  updateChainIndepState _ () = evolveKey

newtype PraosChainDepState c = PraosChainDepState {
      praosHistory :: [BlockInfo c]
    }
  deriving stock   (Eq, Show)
  deriving newtype (NoUnexpectedThunks, Serialise)

-- | Ticking the Praos chain dep state has no effect
--
-- For the real Praos implementation, ticking is crucial, as it determines the
-- point where the "nonce under construction" is swapped out for the "active"
-- nonce. However, for the mock implementation, we keep the full history, and
-- choose the right nonce from that; this means that ticking has no effect.
newtype instance Ticked (PraosChainDepState c) = TickedPraosChainDepState {
      getTickedPraosChainDepState :: PraosChainDepState c
    }

instance PraosCrypto c => ConsensusProtocol (Praos c) where
  protocolSecurityParam = praosSecurityParam . praosParams

  type LedgerView    (Praos c) = StakeDist
  type IsLeader      (Praos c) = PraosProof           c
  type ValidationErr (Praos c) = PraosValidationError c
  type ValidateView  (Praos c) = PraosValidateView    c
  type ChainDepState (Praos c) = PraosChainDepState   c
  type CanBeLeader   (Praos c) = CoreNodeId
  type CannotLead    (Praos c) = Void

  checkIsLeader cfg@PraosConfig{..} nid _cis slot _u (TickedPraosChainDepState cds) = do
      if fromIntegral (getOutputVRFNatural (certifiedOutput y)) < t
      then IsLeader PraosProof {
               praosProofRho  = rho
             , praosProofY    = y
             , praosLeader    = nid
             , praosProofSlot = slot
             }
      else NotLeader
    where
      (rho', y', t) = rhoYT cfg (praosHistory cds) slot nid

      rho = evalCertified () rho' praosSignKeyVRF
      y   = evalCertified () y'   praosSignKeyVRF

  tickChainDepState _ _ _ = TickedPraosChainDepState

  updateChainDepState cfg@PraosConfig{..}
                      (PraosValidateView PraosFields{..} toSign)
                      slot
                      (TickedStakeDist sd)
                      (TickedPraosChainDepState cds) = do
    let PraosExtraFields{..} = praosExtraFields
        nid                  = praosCreator

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

    let !bi = BlockInfo
            { biSlot  = slot
            , biRho   = praosRho
            , biStake = StakeDist sd
            }

    return $ PraosChainDepState $ bi : praosHistory cds

  -- Rewind the chain state
  --
  -- The mock implementation of Praos keeps the full history of the chain state
  -- since the dawn of time (#248). For this reason rewinding is very simple,
  -- and we can't get to a point where we can't roll back more (unless the slot
  -- number never occurred, but that would be a bug in the caller).
  --
  -- We don't roll back to the exact slot since that slot might not have been
  -- filled; instead we roll back the the block just before it.
  rewindChainDepState _proxy _k rewindTo (PraosChainDepState cds) =
      -- This may drop us back to the empty list if we go back to genesis
      Just . PraosChainDepState $
        dropWhile (\bi -> NotOrigin (biSlot bi) > pointSlot rewindTo) cds

  -- (Standard) Praos uses the standard chain selection rule, so no need to
  -- override (though see note regarding clock skew).

instance PraosCrypto c => NoUnexpectedThunks (ConsensusConfig (Praos c))
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
    in  fromHash $ hash @(PraosHash c) (eta', e, rhos)
  where
    PraosParams{..} = praosParams l

infosStake :: ConsensusConfig (Praos c) -> [BlockInfo c] -> EpochNo -> StakeDist
infosStake s@PraosConfig{..} xs e = case ys of
    []                  -> praosInitialStake
    (BlockInfo{..} : _) -> biStake
  where
    PraosParams{..} = praosParams

    e' = if e >= 2 then EpochNo (unEpochNo e - 2) else 0
    ys = dropWhile (\b -> blockInfoEpoch s b > e') xs

phi :: ConsensusConfig (Praos c) -> Rational -> Double
phi PraosConfig{..} r = 1 - (1 - praosLeaderF) ** fromRational r
  where
    PraosParams{..} = praosParams

leaderThreshold :: forall c. PraosCrypto c
                => ConsensusConfig (Praos c)
                -> [BlockInfo c]
                -> SlotNo
                -> CoreNodeId
                -> Double
leaderThreshold st xs s n =
    let a = stakeWithDefault 0 n $ infosStake st xs (slotEpoch st s)
    in  2 ^ (sizeHash (Proxy :: Proxy (PraosHash c)) * 8) * phi st a

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
      ) => PraosCrypto (c :: *) where
  type family PraosKES  c :: *
  type family PraosVRF  c :: *
  type family PraosHash c :: *

data PraosStandardCrypto
data PraosMockCrypto

instance PraosCrypto PraosStandardCrypto where
  type PraosKES  PraosStandardCrypto = SimpleKES Ed448DSIGN 1000
  type PraosVRF  PraosStandardCrypto = SimpleVRF
  type PraosHash PraosStandardCrypto = SHA256

instance PraosCrypto PraosMockCrypto where
  type PraosKES  PraosMockCrypto = MockKES 1000
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
    [ encodeListLen 3
    , encode biSlot
    , toCBOR biRho
    , encode biStake
    ]
  decode = do
    decodeListLenOf 3
    biSlot  <- decode
    biRho   <- fromCBOR
    biStake <- decode
    return BlockInfo {..}
