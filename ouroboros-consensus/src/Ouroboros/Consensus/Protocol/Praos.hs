{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

-- | Proof of concept implementation of Praos
module Ouroboros.Consensus.Protocol.Praos (
    Praos
  , PraosFields(..)
  , PraosExtraFields(..)
  , PraosParams(..)
  , forgePraosFields
    -- * Tags
  , PraosCrypto(..)
  , PraosStandardCrypto
  , PraosMockCrypto
  , HeaderSupportsPraos(..)
    -- * Type instances
  , NodeConfig(..)
--  , Payload(..)
  ) where

import           Cardano.Binary (ToCBOR)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (..))
import           Control.Monad (unless)
import           Control.Monad.Except (throwError)
import           Crypto.Random (MonadRandom)
import           Data.Functor.Identity
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Numeric.Natural

import           Cardano.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Cardano.Crypto.Hash.Class (HashAlgorithm (..),
                     fromHash, hash)
import           Cardano.Crypto.Hash.MD5 (MD5)
import           Cardano.Crypto.Hash.SHA256 (SHA256)
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.KES.Mock
import           Cardano.Crypto.KES.Simple
import           Cardano.Crypto.Util (Empty)
import           Cardano.Crypto.VRF.Class
import           Cardano.Crypto.VRF.Mock (MockVRF)
import           Cardano.Crypto.VRF.Simple (SimpleVRF)

import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..))

import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Signed
import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.HList (HList (..))

import           Ouroboros.Consensus.Ledger.Mock.Stake

{-------------------------------------------------------------------------------
  Fields required by Praos in the header
-------------------------------------------------------------------------------}

-- | The fields that Praos required in the header
data PraosFields c toSign = PraosFields {
      praosSignature   :: SignedKES (PraosKES c) toSign
    , praosExtraFields :: PraosExtraFields c
    }
  deriving Generic

-- | Fields that should be included in the signature
data PraosExtraFields c = PraosExtraFields {
      praosCreator   :: CoreNodeId
    , praosRho       :: CertifiedVRF (PraosVRF c) (HList [Natural, SlotNo, VRFType])
    , praosY         :: CertifiedVRF (PraosVRF c) (HList [Natural, SlotNo, VRFType])
    }

class ( HasHeader hdr
      , SignedHeader hdr
      , Signable (PraosKES c) (Signed hdr)
      ) => HeaderSupportsPraos c hdr where
  headerPraosFields :: proxy (Praos c) -> hdr -> PraosFields c (Signed hdr)

forgePraosFields :: ( HasNodeState (Praos c) m
                    , MonadRandom m
                    , PraosCrypto c
                    )
                 => NodeConfig (Praos c)
                 -> PraosProof c
                 -> (toSign -> Encoding)
                 -> (PraosExtraFields c -> toSign)
                 -> m (PraosFields c toSign)
forgePraosFields PraosNodeConfig{..} PraosProof{..} encodeToSign mkToSign = do
    keyKES <- getNodeState
    let signedFields = PraosExtraFields {
          praosCreator = praosLeader
        , praosRho     = praosProofRho
        , praosY       = praosProofY
        }
    m <- signedKES
           encodeToSign
           (fromIntegral (unSlotNo praosProofSlot))
           (mkToSign signedFields)
           keyKES
    case m of
      Nothing                  -> error "mkOutoborosPayload: signedKES failed"
      Just (signature, newKey) -> do
        putNodeState newKey
        return $ PraosFields {
            praosSignature    = signature
          , praosExtraFields = signedFields
          }

{-------------------------------------------------------------------------------
  Praos specific types
-------------------------------------------------------------------------------}

data VRFType = NONCE | TEST
    deriving (Show, Eq, Ord, Generic)

instance Serialise VRFType
  -- use generic instance

deriving instance PraosCrypto c => Show (PraosExtraFields c)
deriving instance PraosCrypto c => Eq   (PraosExtraFields c)
deriving instance PraosCrypto c => Ord  (PraosExtraFields c)

deriving instance PraosCrypto c => Show (PraosFields c toSign)
deriving instance PraosCrypto c => Eq   (PraosFields c toSign)
deriving instance PraosCrypto c => Ord  (PraosFields c toSign)

data PraosProof c = PraosProof {
      praosProofRho  :: CertifiedVRF (PraosVRF c) (HList [Natural, SlotNo, VRFType])
    , praosProofY    :: CertifiedVRF (PraosVRF c) (HList [Natural, SlotNo, VRFType])
    , praosLeader    :: CoreNodeId
    , praosProofSlot :: SlotNo
    }

data PraosValidationError c =
      PraosInvalidSlot SlotNo SlotNo
    | PraosUnknownCoreId Int
    | PraosInvalidSig String (VerKeyKES (PraosKES c)) Natural (SigKES (PraosKES c))
    | PraosInvalidCert (VerKeyVRF (PraosVRF c)) Encoding Natural (CertVRF (PraosVRF c))
    | PraosInsufficientStake Double Natural

deriving instance PraosCrypto c => Show (PraosValidationError c)

-- TODO: This type definition belongs elsewhere.
newtype EpochNo = EpochNo { unEpochNo :: Word64 }
    deriving (Eq, Num, Ord, Serialise, ToCBOR)

data BlockInfo c = BlockInfo
    { biSlot  :: SlotNo
    , biRho   :: CertifiedVRF (PraosVRF c) (HList [Natural, SlotNo, VRFType])
    , biStake :: StakeDist
    }

deriving instance PraosCrypto c => Show (BlockInfo c)
deriving instance PraosCrypto c => Eq   (BlockInfo c)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data Praos c

-- | Praos parameters that are node independent
data PraosParams = PraosParams {
      praosLeaderF       :: Double
    , praosSecurityParam :: SecurityParam
    , praosSlotsPerEpoch :: Word64
    , praosLifetimeKES   :: Natural
    }

instance PraosCrypto c => OuroborosTag (Praos c) where
  data NodeConfig (Praos c) = PraosNodeConfig
    { praosParams        :: PraosParams
    , praosInitialEta    :: Natural
    , praosInitialStake  :: StakeDist
    , praosNodeId        :: NodeId
    , praosSignKeyVRF    :: SignKeyVRF (PraosVRF c)
    , praosVerKeys       :: IntMap (VerKeyKES (PraosKES c), VerKeyVRF (PraosVRF c))
    }

  protocolSecurityParam = praosSecurityParam . praosParams

  type NodeState       (Praos c) = SignKeyKES (PraosKES c)
  type LedgerView      (Praos c) = StakeDist
  type IsLeader        (Praos c) = PraosProof c
  type ValidationErr   (Praos c) = PraosValidationError c
  type SupportedHeader (Praos c) = HeaderSupportsPraos c
  type ChainState      (Praos c) = [BlockInfo c]

  checkIsLeader cfg@PraosNodeConfig{..} slot _u cs =
    case praosNodeId of
        RelayId _  -> return Nothing
        CoreId nid -> do
          let (rho', y', t) = rhoYT cfg cs slot nid
          rho <- evalCertified encode rho' praosSignKeyVRF
          y   <- evalCertified encode y'   praosSignKeyVRF
          return $ if fromIntegral (certifiedNatural y) < t
              then Just PraosProof {
                       praosProofRho  = rho
                     , praosProofY    = y
                     , praosLeader    = CoreNodeId nid
                     , praosProofSlot = slot
                     }
              else Nothing

  applyChainState cfg@PraosNodeConfig{..} sd b cs = do
    let PraosFields{..}      = headerPraosFields cfg b
        PraosExtraFields{..} = praosExtraFields
        toSign               = headerSigned b
        slot                 = blockSlot b
        CoreNodeId nid       = praosCreator

    -- check that the new block advances time
    case cs of
        (c : _)
            | biSlot c >= slot -> throwError $ PraosInvalidSlot slot (biSlot c)
        _                      -> return ()

    -- check that block creator is a known core node
    (vkKES, vkVRF) <- case IntMap.lookup nid praosVerKeys of
        Nothing  -> throwError $ PraosUnknownCoreId nid
        Just vks -> return vks

    -- verify block signature
    let proxy = Identity b
    case verifySignedKES
           (encodeSigned proxy)
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

    let (rho', y', t) = rhoYT cfg cs slot nid

    -- verify rho proof
    unless (verifyCertified encode vkVRF rho' praosRho) $
        throwError $ PraosInvalidCert
            vkVRF
            (encode rho')
            (certifiedNatural praosRho)
            (certifiedProof praosRho)

    -- verify y proof
    unless (verifyCertified encode vkVRF y' praosY) $
        throwError $ PraosInvalidCert
            vkVRF
            (encode y')
            (certifiedNatural praosY)
            (certifiedProof praosY)

    -- verify stake
    unless (fromIntegral (certifiedNatural praosY) < t) $
        throwError $ PraosInsufficientStake t $ certifiedNatural praosY

    let bi = BlockInfo
            { biSlot  = blockSlot b
            , biRho   = praosRho
            , biStake = sd
            }

    return $ bi : cs

  -- Rewind the chain state
  --
  -- At the moment, this implementation of Praos keeps the full history of the
  -- chain state since the dawn of time (#248). For this reason rewinding is
  -- very simple, and we can't get to a point where we can't roll back more
  -- (unless the slot number never occurred, but that would be a bug in the
  -- caller). Once we limit the history we keep, this function will become
  -- more complicated.
  --
  -- We don't roll back to the exact slot since that slot might not have been
  -- filled; instead we roll back the the block just before it.
  rewindChainState PraosNodeConfig{..} cs rewindTo =
      -- This may drop us back to the empty list if we go back to genesis
      Just $ dropWhile (\bi -> biSlot bi > rewindTo) cs

  -- NOTE: We redefine `preferCandidate` but NOT `compareCandidates`
  -- NOTE: See note regarding clock skew.
  preferCandidate PraosNodeConfig{..} ours cand =
      AF.forksAtMostKBlocks k ours cand &&
      AF.compareHeadBlockNo cand ours == GT
    where
      PraosParams{..} = praosParams

      k :: Word64
      k = maxRollbacks praosSecurityParam

slotEpoch :: NodeConfig (Praos c) -> SlotNo -> EpochNo
slotEpoch PraosNodeConfig{..} s =
    EpochNo $ 1 + div (unSlotNo s - 1) praosSlotsPerEpoch
  where
    PraosParams{..} = praosParams

blockInfoEpoch :: NodeConfig (Praos c) -> BlockInfo c -> EpochNo
blockInfoEpoch l = slotEpoch l . biSlot

epochStart :: NodeConfig (Praos c) -> EpochNo -> SlotNo
epochStart PraosNodeConfig{..} e = SlotNo $ (unEpochNo e - 1) * praosSlotsPerEpoch + 1
  where
    PraosParams{..} = praosParams

infosSlice :: SlotNo -> SlotNo -> [BlockInfo c] -> [BlockInfo c]
infosSlice from to xs = takeWhile (\b -> biSlot b >= from)
                      $ dropWhile (\b -> biSlot b > to) xs

infosEta :: forall c. (PraosCrypto c)
         => NodeConfig (Praos c)
         -> [BlockInfo c]
         -> EpochNo
         -> Natural
infosEta l _  1 = praosInitialEta l
infosEta l xs e =
    let e'   = e - 1
        eta' = infosEta l xs e'
        from = epochStart l e'
        n    = div (2 * praosSlotsPerEpoch) 3
        to   = SlotNo $ unSlotNo from + fromIntegral (n - 1)
        rhos = reverse [biRho b | b <- infosSlice from to xs]
    in  fromHash $ hash @(PraosHash c) $ eta' :* e :* rhos :* Nil
  where
    PraosParams{..} = praosParams l

infosStake :: NodeConfig (Praos c) -> [BlockInfo c] -> EpochNo -> StakeDist
infosStake s@PraosNodeConfig{..} xs e = case ys of
    []                  -> praosInitialStake
    (BlockInfo{..} : _) -> biStake
  where
    PraosParams{..} = praosParams

    e' = if e >= 2 then EpochNo (unEpochNo e - 2) else 0
    ys = dropWhile (\b -> blockInfoEpoch s b > e') xs

phi :: NodeConfig (Praos c) -> Rational -> Double
phi PraosNodeConfig{..} r = 1 - (1 - praosLeaderF) ** fromRational r
  where
    PraosParams{..} = praosParams

leaderThreshold :: forall c. PraosCrypto c
                => NodeConfig (Praos c)
                -> [BlockInfo c]
                -> SlotNo
                -> Int
                -> Double
leaderThreshold st xs s n =
    let a = stakeWithDefault 0 n $ infosStake st xs (slotEpoch st s)
    in  2 ^ (byteCount (Proxy :: Proxy (PraosHash c)) * 8) * phi st a

rhoYT :: PraosCrypto c
      => NodeConfig (Praos c)
      -> [BlockInfo c]
      -> SlotNo
      -> Int
      -> ( HList '[Natural, SlotNo, VRFType]
         , HList '[Natural, SlotNo, VRFType]
         , Double
         )
rhoYT st xs s n =
    let e   = slotEpoch st s
        eta = infosEta st xs e
        rho = eta :* s :* NONCE :* Nil
        y   = eta :* s :* TEST  :* Nil
        t   = leaderThreshold st xs s n
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
        -- TODO: For now we insist that everything must be signable
      , Signable (PraosKES c) ~ Empty
      ) => PraosCrypto (c :: *) where
  type family PraosKES  c :: *
  type family PraosVRF  c :: *
  type family PraosHash c :: *

data PraosStandardCrypto
data PraosMockCrypto

instance PraosCrypto PraosStandardCrypto where
  type PraosKES  PraosStandardCrypto = SimpleKES Ed448DSIGN
  type PraosVRF  PraosStandardCrypto = SimpleVRF
  type PraosHash PraosStandardCrypto = SHA256

instance PraosCrypto PraosMockCrypto where
  type PraosKES  PraosMockCrypto = MockKES
  type PraosVRF  PraosMockCrypto = MockVRF
  type PraosHash PraosMockCrypto = MD5

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance PraosCrypto c => Condense (PraosFields c toSign) where
   condense PraosFields{..} = condense praosSignature
