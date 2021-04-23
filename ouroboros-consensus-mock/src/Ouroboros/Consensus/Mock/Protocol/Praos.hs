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

-- The Praos paper can be located at https://ia.cr/2017/573
--
-- A block as defined in Praos (section 3, definition 2, then extended in Fig 9)
-- consist of a tuple:
--
-- > B = (slⱼ, st, d, B_πj, ρ, σⱼ)
--
-- where
-- - slⱼ: the slot at which the block was generated.
--        Named 'simpleSlotNo' in the 'SimpleStdHeader' block header.
--        As far as consensus is concerned this will be accessed through
--        'biSlot' in each block in the 'ChainDepState'.
--
-- - st: state, a string ∈ {0,1}^λ, which holds the hash of the previous block.
--       Named 'simplePrev' in the 'SimpleStdHeader' block header.
--
-- - d: data (transaction data in most cases).
--      Named 'simpleBody' inside 'SimpleBlock'.
--
-- - B_πj: block proof consisting of (Uᵢ, y, π).
--         Named 'praosY' inside 'PraosExtraFields'.
--      - y: a VRF output used to confirm that Uᵢ was the slot leader.
--      - π: the VRF proof of the above value.
--
--      > (y, π) ≜ VRF_evalProve(η, slⱼ, TEST) see Fig 9
--
-- - ρ: the block nonce consisting of (ρ_y, ρ_π), to capture entropy from the
--      block forging process.
--      Named 'praosRho' inside 'PraosExtraFields'.
--      - ρ_y: a VRF output used to confirm this block captured all the previous
--             entropy.
--      - ρ_π: the VRF proof of the above value.
--
--      > (ρ_y, ρ_π) ≜ VRF_evalProve(η, slⱼ, NONCE) see Fig 9
--
-- - σⱼ: a signature on (st, d, slⱼ, B_πj, ρ) with the signing key
--       for the slot slⱼ for the stakeholder Uᵢ.
--       Named 'praosSignature' in 'PraosFields'.
--
-- Protocol parameters:
-- - k: maximum number of blocks we can rollback.
--      Named 'praosSecurityParam' in 'PraosParams'.
-- - R: number of slots per epoch.
--      Named 'praosSlotsPerEpoch' in 'PraosParams'.
-- - f: the active slots coefficient, specifies roughly the proportion of
--      occupied slots per epoch.
--      Named 'praosLeaderF' in 'PraosParams'.
--
-- Some values you will encounter:
-- - η: The epoch's nonce. Captures entropy from the block chain. See Fig 8 in
--      praos paper for where it is used and Fig 10 for how it is defined.
--      Defined as the hash of the η from the previous epoch, this epoch number
--      and the ρ of the first 2/3 of the blocks in the previous epoch.
--      Commonly named through the code as 'eta'.
--
--      > η_e ≜ HASH(η_{e-1} || e || ρ_{e-1,0} ... ρ_{e-1, 2R/3})
--
-- - Tᵢ: the leader threshold for a specific stakeholder considering its
--       relative stake (therefore depending on the slot). Defined in the Praos
--       paper in Figure 4 using the definition for ϕ_f(αᵢ) from section 3.3.
--       Named 't' in the code but essentially computed by 'leaderThreshold' in
--       'rhoYT'.
--
--       > Tᵢ ≜ 2^(l_VRF) * pᵢ
--       > pᵢ = ϕ_f(αᵢ) ≜ 1 - (1 - f)^(αᵢ)

{-------------------------------------------------------------------------------
  Fields required by Praos in the header
-------------------------------------------------------------------------------}

-- | The fields that Praos required in the header
data PraosFields crypto typeBeingSigned = PraosFields {
      praosSignature   :: SignedKES (PraosKES crypto) typeBeingSigned
    , praosExtraFields :: PraosExtraFields crypto
    }
  deriving (Generic)

instance (PraosCrypto c, Typeable toSign) => NoThunks (PraosFields c toSign)

deriving instance PraosCrypto c => Show (PraosFields c toSign)
deriving instance PraosCrypto c => Eq   (PraosFields c toSign)

-- | Fields that should be included in the signature
data PraosExtraFields c = PraosExtraFields {
      praosCreator :: CoreNodeId
    , praosRho     :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosY       :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    }
  deriving (Generic)

instance PraosCrypto c => NoThunks (PraosExtraFields c)

deriving instance PraosCrypto c => Show (PraosExtraFields c)
deriving instance PraosCrypto c => Eq   (PraosExtraFields c)

-- | A validate view is an association from the (@signed@) value to the
-- @PraosFields@ that contains the signature that sign it.
--
-- In this mock implementation, this could have been simplified to use
-- @SignedSimplePraos@ but from the consensus point of view, it is not relevant
-- which actual value is being signed, that's why we use the existential.
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

-- | The key used for the given period or a stub Poisoned value.
--
-- A key will be poisoned if it failed to evolve by @updateKES@, and will remain
-- poisoned forever after that.
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

-- | Create a PraosFields using a proof, a key and the data to be signed.
--
-- It is done by signing whatever is extracted from the extra fields by @mkToSign@
-- and storing the signature and the extra fields on a @PraosFields@.
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
          praosSignature   = signedKES () kesPeriod (mkToSign fieldsToSign) key
        , praosExtraFields = fieldsToSign
        }
      HotKeyPoisoned -> error "trying to sign with a poisoned key"
  where
    fieldsToSign = PraosExtraFields {
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

-- |The two VRF invocation modes, NONCE (rho) and TEST (y). See the comment at
-- the top of the module for an explanation of these.
data VRFType = NONCE | TEST
    deriving (Show, Eq, Ord, Generic, NoThunks)

instance Serialise VRFType

instance ToCBOR VRFType where
  -- This is a cheat, and at some point we probably want to decide on Serialise/ToCBOR
  toCBOR = encode

-- |Proofs certifying ρ and y for a given slot and eta.
data PraosProof c = PraosProof {
      praosProofRho :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosProofY   :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosLeader   :: CoreNodeId
    }

-- | An error that can arise during validation
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

-- | An uninhabited type representing the Praos protocol.
data Praos c

-- | Praos parameters that are node independent
data PraosParams = PraosParams {
      praosLeaderF       :: !Double
      -- ^ f, the active slots coefficient, defined in 3.3 in the Praos paper.
    , praosSecurityParam :: !SecurityParam
      -- ^ k, maximum number of blocks we can rollback
    , praosSlotsPerEpoch :: !Word64
      -- ^ R, slots in each epoch, defined in section 3 in the Praos paper.
    }
  deriving (Generic, NoThunks)

-- | The configuration that will be provided to every node when running the
-- MockPraos protocol.
data instance ConsensusConfig (Praos c) = PraosConfig
  { praosParams        :: !PraosParams
  , praosInitialEta    :: !Natural
  , praosInitialStake  :: !StakeDist
  , praosEvolvingStake :: !PraosEvolvingStake
  , praosSignKeyVRF    :: !(SignKeyVRF (PraosVRF c))
  , praosVerKeys       :: !(Map CoreNodeId (VerKeyKES (PraosKES c), VerKeyVRF (PraosVRF c)))
  }
  deriving (Generic)

instance PraosCrypto c => NoThunks (ConsensusConfig (Praos c))

slotEpoch :: ConsensusConfig (Praos c) -> SlotNo -> EpochNo
slotEpoch PraosConfig{praosParams = PraosParams{..}} s =
    runIdentity $ epochInfoEpoch epochInfo s
  where
    epochInfo = fixedSizeEpochInfo (EpochSize praosSlotsPerEpoch)

epochFirst :: ConsensusConfig (Praos c) -> EpochNo -> SlotNo
epochFirst PraosConfig{..} e =
    runIdentity $ epochInfoFirst epochInfo e
  where
    epochInfo = fixedSizeEpochInfo (EpochSize praosSlotsPerEpoch)
    PraosParams{..} = praosParams

-- |The chain dependent state, in this case as it is a mock, we just will store
-- a list of BlockInfos that allow us to look into the past.
newtype PraosChainDepState c = PraosChainDepState {
      praosHistory :: [BlockInfo c]
    }
  deriving stock   (Eq, Show)
  deriving newtype (NoThunks, Serialise)

infosSlice :: SlotNo -> SlotNo -> [BlockInfo c] -> [BlockInfo c]
infosSlice from to xs = takeWhile (\b -> biSlot b >= from)
                      $ dropWhile (\b -> biSlot b > to) xs

infosEta :: forall c. (PraosCrypto c)
         => ConsensusConfig (Praos c)
         -> [BlockInfo c]
         -> EpochNo
         -> Natural
infosEta l _  0 =
    praosInitialEta l
infosEta l@PraosConfig{praosParams = PraosParams{..}} xs e =
    let e'   = e - 1
        -- the η from the previous epoch
        eta' = infosEta l xs e'
        -- the first slot in previous epoch
        from = epochFirst l e'
        -- 2/3 of the slots per epoch
        n    = div (2 * praosSlotsPerEpoch) 3
        -- the last of the 2/3 of slots in this epoch
        to   = SlotNo $ unSlotNo from + n
        -- the list of rhos from the first block in this epoch until the one at
        -- 2/3 of the slots. Note it is reversed, i.e. start at the oldest.
        rhos = reverse [biRho b | b <- infosSlice from to xs]
    in  bytesToNatural
          . hashToBytes
          $ hashWithSerialiser @(PraosHash c) toCBOR (eta', e, rhos)

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
      -- ^ The ticked ledger view.
    , untickedPraosChainDepState :: PraosChainDepState c
      -- ^ The unticked chain dependent state, containing the full history.
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
      -- See Figure 4 of the Praos paper.
      -- In order to be leader, y must be < Tᵢ
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

-- | Probability for stakeholder Uᵢ to be elected in slot
-- slⱼ considering its relative stake αᵢ.
phi :: ConsensusConfig (Praos c) -> Rational -> Double
phi PraosConfig{..} alpha = 1 - (1 - praosLeaderF) ** fromRational alpha
  where
    PraosParams{..} = praosParams

-- | Compute Tᵢ for a given stakeholder @n@ at a @SlotNo@. Will be computed from
-- 'praosEvolvingStake' (or taken from 'praosInitialStake' if checking epoch 0).
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
