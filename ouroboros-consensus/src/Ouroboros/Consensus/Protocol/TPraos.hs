
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns            #-}

-- | Transitional Praos.
--
--   Transitional praos allows for the overlaying of Praos with an overlay
--   schedule determining slots to be produced by BFT
module Ouroboros.Consensus.Protocol.TPraos (
    TPraos
  , TPraosFields(..)
  , TPraosToSign(..)
  , TPraosLedgerView(..)
  , TPraosParams(..)
  , TPraosProof(..)
  , TPraosIsCoreNode(..)
  , forgeTPraosFields
    -- * Tags
  , TPraosCrypto(..)
  , TPraosStandardCrypto
  , TPraosMockCrypto
  , HeaderSupportsTPraos(..)
    -- * Type instances
  , NodeConfig(..)
  ) where

import           Control.Monad.Except (ExceptT(..))
import           Crypto.Random (MonadRandom)
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Numeric.Natural
import           Control.State.Transition (TRC(..), applySTS)
import           Cardano.Crypto.DSIGN.Class (VerKeyDSIGN)
import           Cardano.Crypto.Hash.Class (HashAlgorithm (..))
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.VRF.Class
import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..))
import           Ouroboros.Consensus.NodeId (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Signed
import qualified Ouroboros.Consensus.Protocol.TPraos.ChainState as ChainState
import           Ouroboros.Consensus.Protocol.TPraos.Crypto
import           Ouroboros.Consensus.Protocol.TPraos.Util
import           Ouroboros.Consensus.Util.Condense
import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF

import BaseTypes (Seed, UnitInterval)
import BlockChain (BHeader)
import Delegation.Certificates (PoolDistr(..))
import Keys (DiscVKey(..), Dms(..), GenKeyHash, KeyHash, hashKey)
import OCert (OCert(..))
import PParams (PParams)
import Slot (Slot(..))
import qualified STS.Prtcl as STS

{-------------------------------------------------------------------------------
  Fields required by TPraos in the header
-------------------------------------------------------------------------------}

data TPraosFields c toSign = TPraosFields
  { tpraosSignature :: SignedKES (TPraosKES c) toSign
  , tpraosToSign :: TPraosToSign c
  }
  deriving Generic

deriving instance TPraosCrypto c => Show (TPraosFields c toSign)

-- | Fields arising from transitional praos execution which must be included in
-- the block signature.
data TPraosToSign c = TPraosToSign
  { tptsIssuerVK :: VerKeyDSIGN (TPraosDSIGN c)
  , tptsVrfVk :: VerKeyVRF (TPraosVRF c)
  , tptsEta :: CertifiedVRF (TPraosVRF c) Seed
  , tptsLeader :: CertifiedVRF (TPraosVRF c) UnitInterval
  , tptsOCert :: OCert (TPraosDSIGN c) (TPraosKES c)
  }
  deriving Generic

deriving instance TPraosCrypto c => Show (TPraosToSign c)

class ( HasHeader hdr
      , SignedHeader hdr
      , Cardano.Crypto.KES.Class.Signable (TPraosKES c) (Signed hdr)
      ) => HeaderSupportsTPraos c hdr where

  -- Because we are using the executable spec, rather than implementing the
  -- protocol directly here, we have a fixed header type rather than an
  -- abstraction. So we must introduce this method.
  headerToBHeader
    :: proxy (TPraos c)
    -> hdr
    -> BHeader (TPraosHash c) (TPraosDSIGN c) (TPraosKES c)

forgeTPraosFields :: ( HasNodeState (TPraos c) m
                    , MonadRandom m
                    , TPraosCrypto c
                    , Cardano.Crypto.KES.Class.Signable (TPraosKES c) toSign
                    )
                 => NodeConfig (TPraos c)
                 -> TPraosProof c
                 -> (TPraosToSign c -> toSign)
                 -> m (TPraosFields c toSign)
forgeTPraosFields TPraosNodeConfig{..}  TPraosProof{..} mkToSign = do
    let icn@TPraosIsCoreNode{..} = tpraosIsCoreNode
        (DiscVKey issuerVK) = ocertVkCold tpraosOpCert
        signedFields = TPraosToSign {
          tptsIssuerVK = issuerVK
        , tptsVrfVk = deriveVerKeyVRF tpraosSignKeyVRF
        , tptsEta = tpraosEta
        , tptsLeader       = tpraosLeader
        , tptsOCert = tpraosOpCert
        }
    m <- signedKES
           (fromIntegral (unSlotNo tpraosProofSlot))
           (mkToSign signedFields)
           tpraosIsCoreNodeSKSHot
    case m of
      Nothing                  -> error "mkOutoborosPayload: signedKES failed"
      Just (signature, newKey) -> do
        putNodeState . Just $ icn { tpraosIsCoreNodeSKSHot = newKey }
        return $ TPraosFields {
            tpraosSignature    = signature
          , tpraosToSign       = signedFields
          }

{-------------------------------------------------------------------------------
  TPraos specific types
-------------------------------------------------------------------------------}

data TPraosProof c
  = TPraosProof
    { tpraosEta       :: CertifiedVRF (TPraosVRF c) Seed
    , tpraosLeader    :: CertifiedVRF (TPraosVRF c) UnitInterval
    , tpraosProofSlot :: SlotNo
    , tpraosOpCert    :: OCert (TPraosDSIGN c) (TPraosKES c)
    , tpraosIsCoreNode :: TPraosIsCoreNode c
    }

data TPraosLedgerView c = TPraosLedgerView {
    -- | Stake distribution
    tpraosLedgerViewPoolDistr :: PoolDistr (TPraosHash c) (TPraosDSIGN c)
  , tpraosLedgerViewProtParams :: PParams
  , tpraosLedgerViewDelegationMap :: Dms (TPraosHash c) (TPraosDSIGN c)
  , tpraosLedgerViewEpochNonce :: Seed
  , tpraosLedgerViewLeaderVal :: UnitInterval
  , tpraosLedgerViewOverlaySchedule :: Map.Map Slot (Maybe (GenKeyHash (TPraosHash c) (TPraosDSIGN c)))
  }

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data TPraos c

-- | TPraos parameters that are node independent
data TPraosParams = TPraosParams {
      tpraosLeaderF       :: Double
    , tpraosSecurityParam :: SecurityParam
    , tpraosSlotsPerEpoch :: Word64
    , tpraosLifetimeKES   :: Natural
    }

data TPraosIsCoreNode c = TPraosIsCoreNode
  { tpraosIsCoreNodeSKSHot :: SignKeyKES (TPraosKES c)
  , tpraosIsCoreNodeOpCert :: OCert (TPraosDSIGN c) (TPraosKES c)
  }

instance TPraosCrypto c => OuroborosTag (TPraos c) where
  data NodeConfig (TPraos c) = TPraosNodeConfig
    { tpraosParams        :: TPraosParams
    , tpraosInitialEta    :: Natural
    , tpraosInitialStake  :: PoolDistr (TPraosHash c) (TPraosDSIGN c)
    , tpraosNodeId        :: NodeId
    , tpraosSignKeyVRF    :: SignKeyVRF (TPraosVRF c)
    }

  protocolSecurityParam = tpraosSecurityParam . tpraosParams

  type NodeState       (TPraos c) = Maybe (TPraosIsCoreNode c)
  type LedgerView      (TPraos c) = TPraosLedgerView c
  type IsLeader        (TPraos c) = TPraosProof c
  type ValidationErr   (TPraos c) = [[STS.PredicateFailure (PRTCL c)]]
  type SupportedHeader (TPraos c) = HeaderSupportsTPraos c
  type ChainState      (TPraos c) = ChainState.TPraosChainState c

  checkIsLeader cfg@TPraosNodeConfig{..} slot lv _cs =
    getNodeState >>= \case
        Nothing -> return Nothing
        Just TPraosIsCoreNode{ tpraosIsCoreNodeOpCert} -> do
          let vkhCold = hashKey $ ocertVkCold tpraosIsCoreNodeOpCert
              (rho', y', t) = rhoYT cfg slot vkhCold lv
          rho <- evalCertified rho' tpraosSignKeyVRF
          y   <- evalCertified y'   tpraosSignKeyVRF
          -- First, check whether we're in the overlay schedule
          case (Map.lookup (convertSlot slot) $ tpraosLedgerViewOverlaySchedule lv) of
            Nothing -> return $
              -- Slot isn't in the overlay schedule, so we're in Praos
              if fromIntegral (certifiedNatural y) < t
                  then Just TPraosProof {
                          tpraosEta       = coerce rho
                        , tpraosLeader    = coerce y
                        , tpraosProofSlot = slot
                        , tpraosOpCert    = tpraosIsCoreNodeOpCert
                        }
                  else Nothing
            Just Nothing ->
              -- This is a non-active slot; nobody may produce a block
              return Nothing
            Just (Just gkhash) ->
              -- The given genesis key has authority to produce a block in this
              -- slot. Check whether we're its delegate.
              let Dms dlgMap = tpraosLedgerViewDelegationMap lv
              in do
                let verKey = ocertVkCold tpraosIsCoreNodeOpCert
                return $ case Map.lookup gkhash dlgMap of
                  Just dlgHash | dlgHash == hashKey verKey ->
                    Just TPraosProof
                        { tpraosEta = coerce rho
                          -- Note that this leader value is not checked for
                          -- slots in the overlay schedule, so we could set it
                          -- to whatever we want. We evaluate it as normal for
                          -- simplicity's sake.
                        , tpraosLeader = coerce y
                        , tpraosProofSlot = slot
                        , tpraosOpCert    = tpraosIsCoreNodeOpCert
                        }
                  _ -> Nothing

  applyChainState TPraosNodeConfig{..} lv b cs = do
    let slot = blockSlot b
        SecurityParam (fromIntegral -> k) = tpraosSecurityParam tpraosParams

    newCS <- ExceptT . return $ applySTS @(PRTCL c)
      $ TRC ( ( ( tpraosLedgerViewProtParams lv
                , tpraosLedgerViewOverlaySchedule lv
                , tpraosLedgerViewEpochNonce lv
                , tpraosLedgerViewPoolDistr lv
                , tpraosLedgerViewDelegationMap lv
                )
              , convertSlot slot
              )
            , ChainState.toPRTCLState cs
            , headerToBHeader (Proxy :: Proxy (TPraos c)) b
            )

    return . ChainState.prune k $ ChainState.appendState newCS cs

  -- Rewind the chain state
  --
  -- At the moment, this implementation of TPraos keeps the full history of the
  -- chain state since the dawn of time (#248). For this reason rewinding is
  -- very simple, and we can't get to a point where we can't roll back more
  -- (unless the slot number never occurred, but that would be a bug in the
  -- caller). Once we limit the history we keep, this function will become
  -- more complicated.
  --
  -- We don't roll back to the exact slot since that slot might not have been
  -- filled; instead we roll back the the block just before it.
  rewindChainState TPraosNodeConfig{..} cs rewindTo = ChainState.rewind rewindTo cs

  -- NOTE: We redefine `preferCandidate` but NOT `compareCandidates`
  -- NOTE: See note regarding clock skew.
  preferCandidate TPraosNodeConfig{..} ours cand =
      AF.forksAtMostKBlocks k ours cand &&
      AF.compareHeadBlockNo cand ours == GT
    where
      TPraosParams{..} = tpraosParams

      k :: Word64
      k = maxRollbacks tpraosSecurityParam

phi :: NodeConfig (TPraos c) -> Rational -> Double
phi TPraosNodeConfig{..} r = 1 - (1 - tpraosLeaderF) ** fromRational r
  where
    TPraosParams{..} = tpraosParams

leaderThreshold :: forall c. TPraosCrypto c
                => NodeConfig (TPraos c)
                -> LedgerView (TPraos c)
                -> KeyHash (TPraosHash c) (TPraosDSIGN c) -- ^ Key hash of the pool
                -> Double
leaderThreshold nc lv kh =
    let PoolDistr pd = tpraosLedgerViewPoolDistr lv
        a = maybe 0 fst $ Map.lookup kh pd
    in  2 ^ (byteCount (Proxy :: Proxy (TPraosHash c)) * 8) * phi nc a

-- | Calculate the rho, y, and T parameters as per Fig. 9 of the Praos paper.
--
--   - T is the relative stake proportion for the stakeholder in the current
--     epoch.
rhoYT :: TPraosCrypto c
      => NodeConfig (TPraos c)
      -> SlotNo
      -> KeyHash (TPraosHash c) (TPraosDSIGN c) -- ^ Pool key hash
      -> LedgerView (TPraos c)
      -> ( (Seed, SlotNo)
         , (UnitInterval, SlotNo)
         , Double
         )
rhoYT nc s kh lv =
    let eta = tpraosLedgerViewEpochNonce lv
        l   = tpraosLedgerViewLeaderVal lv
        rho = (eta, s)
        y   = (l, s)
        t   = leaderThreshold nc lv kh
    in  (rho, y, t)

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance (Condense c, Condense toSign) => Condense (TPraosFields c toSign)
