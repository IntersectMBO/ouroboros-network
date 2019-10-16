{-# LANGUAGE AllowAmbiguousTypes     #-}
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
{-# LANGUAGE StrictData              #-}
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
import           Control.State.Transition (TRC(..), applySTS)
import           Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm)
import           Cardano.Crypto.Hash.Class (HashAlgorithm (..))
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.VRF.Class
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..))
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Protocol.TPraos.ChainState as ChainState
import           Ouroboros.Consensus.Protocol.TPraos.Crypto
import           Ouroboros.Consensus.Protocol.TPraos.Util
import           Ouroboros.Consensus.Util.Condense
import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF

import BaseTypes (Nonce, UnitInterval)
import BlockChain (BHeader, HashHeader, mkSeed, seedEta, seedL)
import Delegation.Certificates (PoolDistr(..))
import Keys (GenDelegs(..), GenKeyHash, KeyHash, hashKey)
import OCert (OCert(..))
import PParams (PParams)
import Slot (Slot(..))
import qualified STS.Overlay as STS
import qualified STS.Prtcl as STS

{-------------------------------------------------------------------------------
  Fields required by TPraos in the header
-------------------------------------------------------------------------------}

data TPraosFields c toSign = TPraosFields
  { tpraosSignature :: SignedKES (TPraosKES c) toSign
  , tpraosToSign :: TPraosToSign c
  }
  deriving Generic

instance TPraosCrypto c => NoUnexpectedThunks (TPraosFields c toSign)
deriving instance TPraosCrypto c => Show (TPraosFields c toSign)

-- | Fields arising from transitional praos execution which must be included in
-- the block signature.
data TPraosToSign c = TPraosToSign
  { tptsVrfVk :: VerKeyVRF (TPraosVRF c)
    -- | Verifiable result containing the updated nonce value.
  , tptsEta :: CertifiedVRF (TPraosVRF c) Nonce
    -- | Verifiable proof of the leader value, used to determine whether the
    -- node has the right to issue a block in this slot.
    --
    -- We include a value here even for blocks forged under the BFT schedule. It
    -- is not required that such a value be verifiable (though by default it
    -- will be verifiably correct, but unused.)
  , tptsLeader :: CertifiedVRF (TPraosVRF c) UnitInterval
    -- Lightweight delegation certificate mapping the cold (DSIGN) key to the
    -- online KES key.
  , tptsOCert :: OCert (TPraosDSIGN c) (TPraosKES c)
  }
  deriving Generic

instance TPraosCrypto c => NoUnexpectedThunks (TPraosToSign c)
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
    -> BHeader (TPraosHash c) (TPraosDSIGN c) (TPraosKES c) (TPraosVRF c)

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
        signedFields = TPraosToSign {
          tptsVrfVk = deriveVerKeyVRF tpraosIsCoreNodeSignKeyVRF
        , tptsEta = tpraosEta
        , tptsLeader = tpraosLeader
        , tptsOCert = tpraosIsCoreNodeOpCert
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

-- | Assembled proof that the issuer has the right to issue a block in the
-- selected slot.
data TPraosProof c
  = TPraosProof
    { tpraosEta       :: CertifiedVRF (TPraosVRF c) Nonce
    , tpraosLeader    :: CertifiedVRF (TPraosVRF c) UnitInterval
    , tpraosProofSlot :: SlotNo
    , tpraosIsCoreNode :: TPraosIsCoreNode c
    } deriving Generic

instance TPraosCrypto c => NoUnexpectedThunks (TPraosProof c)

data TPraosLedgerView c = TPraosLedgerView {
    -- | Stake distribution
    tpraosLedgerViewPoolDistr :: PoolDistr (TPraosHash c) (TPraosDSIGN c) (TPraosVRF c)
  , tpraosLedgerViewProtParams :: PParams
  , tpraosLedgerViewDelegationMap :: GenDelegs (TPraosHash c) (TPraosDSIGN c)
  , tpraosLedgerViewEpochNonce :: Nonce
    -- | Determines which slots are considered to be part of the overlay
    -- schedule - that is, slots for whom block issuance is reserved to the core
    -- nodes operating under Ouborobos BFT rules. There are three cases:
    --
    -- - The slot is not in the overlay schedule. Then it is considered under
    --   Praos rules.
    -- - The slot is reserved in the schedule to a specific genesis keyholder
    --   ('Just keyHash'). Then the delegate of this stakeholder has the unique
    --   right to issue a block in this slot.
    -- - The slot is reserved in the schedule to 'Nothing'. Then nobody is
    --   eligible to issue a block in this slot.
    --
    -- The last situation is required to reconcile the presence of BFT (which
    -- issues blocks in every slot) in the overlay schedule with the shorter
    -- slot lengths to be used under Praos, which issues blocks only in some
    -- proportion (the 'active slot coefficient') of slots.
  , tpraosLedgerViewOverlaySchedule :: Map.Map Slot (Maybe (GenKeyHash (TPraosHash c) (TPraosDSIGN c)))
  } deriving Generic

instance NoUnexpectedThunks (TPraosLedgerView c)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data TPraos c

-- | TPraos parameters that are node independent
data TPraosParams = TPraosParams {
      -- | Active slots coefficient. This parameter represents the proportion of
      -- slots in which blocks should be issued. This can be interpreted as the
      -- probability that a party holding all the stake will be elected as
      -- leader for a given slot.
      tpraosLeaderF       :: Double
    , tpraosSecurityParam :: SecurityParam
    } deriving Generic

instance NoUnexpectedThunks TPraosParams

data TPraosIsCoreNode c = TPraosIsCoreNode
  { -- | Online KES key used to sign blocks.
    tpraosIsCoreNodeSKSHot :: SignKeyKES (TPraosKES c)
    -- | Certificate delegating rights from the stake pool cold key (or genesis
    -- stakeholder delegate cold key) to the online KES key.
  , tpraosIsCoreNodeOpCert :: OCert (TPraosDSIGN c) (TPraosKES c)
  , tpraosIsCoreNodeSignKeyVRF    :: SignKeyVRF (TPraosVRF c)
  } deriving Generic

instance
  ( DSIGNAlgorithm (TPraosDSIGN c)
  , KESAlgorithm (TPraosKES c)
  , VRFAlgorithm (TPraosVRF c)
  ) => NoUnexpectedThunks (TPraosIsCoreNode c)

instance TPraosCrypto c => OuroborosTag (TPraos c) where
  data NodeConfig (TPraos c) = TPraosNodeConfig
    { tpraosParams        :: TPraosParams
    } deriving Generic


  protocolSecurityParam = tpraosSecurityParam . tpraosParams

  type NodeState       (TPraos c) = Maybe (TPraosIsCoreNode c)
  type LedgerView      (TPraos c) = TPraosLedgerView c
  type IsLeader        (TPraos c) = TPraosProof c
  type ValidationErr   (TPraos c) = [[STS.PredicateFailure (PRTCL c)]]
  type SupportedHeader (TPraos c) = HeaderSupportsTPraos c
  type ChainState      (TPraos c) = ChainState.TPraosChainState c

  checkIsLeader cfg@TPraosNodeConfig{..} slot lv cs =
    getNodeState >>= \case
        Nothing -> return Nothing
        Just icn@TPraosIsCoreNode
              { tpraosIsCoreNodeOpCert
              , tpraosIsCoreNodeSignKeyVRF
              } -> do
          let mkSeed' = mkSeed @(TPraosHash c) @(TPraosDSIGN c) @(TPraosKES c) @(TPraosVRF c)
              vkhCold = hashKey $ ocertVkCold tpraosIsCoreNodeOpCert
              t = leaderThreshold cfg lv vkhCold
              eta0 = tpraosLedgerViewEpochNonce lv
              prevHash = prtclStateHash @c $ ChainState.toPRTCLState cs
              rho' = mkSeed' seedEta (convertSlot slot) eta0 prevHash
              y' = mkSeed' seedL (convertSlot slot) eta0 prevHash
          rho <- evalCertified rho' tpraosIsCoreNodeSignKeyVRF
          y   <- evalCertified y'   tpraosIsCoreNodeSignKeyVRF
          -- First, check whether we're in the overlay schedule
          case (Map.lookup (convertSlot slot) $ tpraosLedgerViewOverlaySchedule lv) of
            Nothing -> return $
              -- Slot isn't in the overlay schedule, so we're in Praos
              if fromIntegral (certifiedNatural y) < t
                  then Just TPraosProof {
                          tpraosEta       = coerce rho
                        , tpraosLeader    = coerce y
                        , tpraosProofSlot = slot
                        , tpraosIsCoreNode = icn
                        }
                  else Nothing
            Just Nothing ->
              -- This is a non-active slot; nobody may produce a block
              return Nothing
            Just (Just gkhash) ->
              -- The given genesis key has authority to produce a block in this
              -- slot. Check whether we're its delegate.
              let GenDelegs dlgMap = tpraosLedgerViewDelegationMap lv
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
                        , tpraosIsCoreNode = icn
                        }
                  _ -> Nothing

  applyChainState TPraosNodeConfig{..} lv b cs = do
    let slot = blockSlot b
        SecurityParam (fromIntegral -> k) = tpraosSecurityParam tpraosParams

    newCS <- ExceptT . return $ applySTS @(PRTCL c)
      $ TRC ( STS.PrtclEnv
                ( STS.OverlayEnv
                  (tpraosLedgerViewProtParams lv)
                  (tpraosLedgerViewOverlaySchedule lv)
                  (tpraosLedgerViewEpochNonce lv)
                  (tpraosLedgerViewPoolDistr lv)
                  (tpraosLedgerViewDelegationMap lv)
                )
                (convertSlot slot)
            , ChainState.toPRTCLState cs
            , headerToBHeader (Proxy :: Proxy (TPraos c)) b
            )

    return . ChainState.prune k $ ChainState.appendState newCS cs

  -- Rewind the chain state
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

-- Use generic instance
instance (VRFAlgorithm (TPraosVRF c)) => NoUnexpectedThunks (NodeConfig (TPraos c))

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

prtclStateHash
  :: STS.State (PRTCL c)
  -> BlockChain.HashHeader (TPraosHash c) (TPraosDSIGN c) (TPraosKES c) (TPraosVRF c)
prtclStateHash (STS.PrtclState _ h _ _ _) = h

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance (Condense c, Condense toSign, TPraosCrypto c)
  => Condense (TPraosFields c toSign) where
  -- TODO Nicer 'Condense' instance
  condense = show
