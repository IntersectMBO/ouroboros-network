{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Transitional Praos.
--
--   Transitional praos allows for the overlaying of Praos with an overlay
--   schedule determining slots to be produced by BFT
module Ouroboros.Consensus.Shelley.Protocol (
    TPraos
  , TPraosFields (..)
  , TPraosToSign (..)
  , TPraosValidateView
  , TPraosNodeState (..)
  , TPraosParams (..)
  , TPraosProof (..)
  , TPraosIsCoreNode (..)
  , TPraosIsCoreNodeOrNot (..)
  , forgeTPraosFields
  , mkShelleyGlobals
    -- * Crypto
  , Crypto
  , TPraosCrypto
  , TPraosStandardCrypto
    -- * Type instances
  , ConsensusConfig (..)
  ) where

import           Control.Monad.Reader (runReader)
import           Control.Monad.Trans.Except (except)
import           Crypto.Random (MonadRandom)
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Typeable (typeRep)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Crypto.DSIGN.Class (VerKeyDSIGN)
import           Cardano.Crypto.Hash.Class (HashAlgorithm (..))
import           Cardano.Crypto.KES.Class (SignKeyKES, SignedKES, signedKES)
import qualified Cardano.Crypto.KES.Class as KES
import           Cardano.Crypto.VRF.Class (CertifiedVRF, SignKeyVRF, VerKeyVRF,
                     deriveVerKeyVRF)
import qualified Cardano.Crypto.VRF.Class as VRF
import           Cardano.Prelude (Natural, NoUnexpectedThunks (..))
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Network.Block (pointSlot)

import qualified Ouroboros.Consensus.Node.State as NodeState
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense

import qualified Shelley.Spec.Ledger.API as SL
import           Control.State.Transition.Extended (applySTS)
import qualified Control.State.Transition.Extended as STS
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS

import           Ouroboros.Consensus.Shelley.Protocol.Crypto
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
    , tpraosToSignLeader   :: CertifiedVRF (VRF c) SL.UnitInterval
      -- | Lightweight delegation certificate mapping the cold (DSIGN) key to
      -- the online KES key.
    , tpraosToSignOCert    :: SL.OCert c
    }
  deriving (Generic)

instance TPraosCrypto c => NoUnexpectedThunks (TPraosToSign c)
deriving instance TPraosCrypto c => Show (TPraosToSign c)

-- | Because we are using the executable spec, rather than implementing the
-- protocol directly here, we have a fixed header type rather than an
-- abstraction. So our validate view is fixed to this.
type TPraosValidateView c = SL.BHeader c

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

data TPraosNodeState c =
    -- | The online KES key used to sign blocks is available
    TPraosKeyAvailable !(SignKeyKES (KES c))

    -- | The KES key is being evolved by another thread
    --
    -- Any thread that sees this value should back off and retry.
  | TPraosKeyEvolving

    -- | This node is not a core node, it doesn't have the capability to sign
    -- blocks.
    --
    -- The 'NodeState' of such a node will always be 'TPraosNoKey'.
  | TPraosNoKey
  deriving (Generic)

-- We override 'showTypeOf' to make sure to show @c@
instance TPraosCrypto c => NoUnexpectedThunks (TPraosNodeState c) where
  showTypeOf _ = show $ typeRep (Proxy @(TPraosNodeState c))

forgeTPraosFields :: ( MonadRandom m
                     , TPraosCrypto c
                     , KES.Signable (KES c) toSign
                     )
                  => NodeState.Update m (TPraosNodeState c)
                  -> IsLeader (TPraos c)
                  -> SL.KESPeriod
                  -> (TPraosToSign c -> toSign)
                  -> m (TPraosFields c toSign)
forgeTPraosFields updateNodeState TPraosProof{..} kesPeriod mkToSign = do
    hotKESKey   <- evolveKESKeyIfNecessary updateNodeState kesPeriod
    mbSignature <- signedKES
      ()
      kesPeriodNat
      (mkToSign signedFields)
      hotKESKey
    case mbSignature of
      Nothing        -> error "signedKES failed"
      Just signature ->
        return TPraosFields {
            tpraosSignature = signature
          , tpraosToSign    = mkToSign signedFields
          }
  where
    SL.KESPeriod kesPeriodNat = kesPeriod

    TPraosIsCoreNode{..} = tpraosIsCoreNode

    SL.DiscVKey issuerVK = tpraosIsCoreNodeColdVerKey

    signedFields = TPraosToSign {
        tpraosToSignIssuerVK = issuerVK
      , tpraosToSignVrfVK    = deriveVerKeyVRF tpraosIsCoreNodeSignKeyVRF
      , tpraosToSignEta      = tpraosEta
      , tpraosToSignLeader   = tpraosLeader
      , tpraosToSignOCert    = tpraosIsCoreNodeOpCert
      }

-- | Get the KES key from the node state, evolve if its KES period doesn't
-- match the given one.
evolveKESKeyIfNecessary
  :: forall m c. (MonadRandom m, TPraosCrypto c)
  => NodeState.Update m (TPraosNodeState c)
  -> SL.KESPeriod
  -> m (SignKeyKES (KES c))
evolveKESKeyIfNecessary updateNodeState (SL.KESPeriod kesPeriod) = do
    getOudatedKeyOrCurrentKey >>= \case
      Right currentKey -> return currentKey
      Left outdatedKey -> do
        newKey <- evolveKey outdatedKey
        saveNewKey newKey
        return newKey
  where
    -- | Return either (@Left@) an outdated key (setting the node state to
    -- 'TPraosKeyEvolving') or (@Right@) a key that's up-to-date w.r.t. the
    -- current KES period (leaving the node state to 'TPraosKeyAvailable').
    getOudatedKeyOrCurrentKey
      :: m (Either (SignKeyKES (KES c)) (SignKeyKES (KES c)))
    getOudatedKeyOrCurrentKey = NodeState.runUpdate updateNodeState $ \case
      TPraosKeyEvolving ->
        -- Another thread is currently evolving the key; wait
        Nothing
      TPraosKeyAvailable key
        | let kesPeriodOfKey = KES.currentPeriodKES () key
        , kesPeriodOfKey < kesPeriod
          -- Must evolve key
        -> return (TPraosKeyEvolving, Left key)
        | otherwise
        -> return (TPraosKeyAvailable key, Right key)
      TPraosNoKey ->
        error "no KES key available"

    -- | Evolve the given key so that its KES period matches @kesPeriod@.
    evolveKey :: SignKeyKES (KES c) -> m (SignKeyKES (KES c))
    evolveKey outdatedKey =
      KES.updateKES () outdatedKey kesPeriod >>= \case
        -- TODO
        Nothing         -> error "Could not update KES key"
        Just evolvedKey -> return evolvedKey

    -- | PRECONDITION: we're in the 'TPraosKeyEvolving' node state.
    saveNewKey :: SignKeyKES (KES c) -> m ()
    saveNewKey newKey = NodeState.runUpdate updateNodeState $ \case
      TPraosKeyEvolving -> Just (TPraosKeyAvailable newKey, ())
      _                 -> error "must be in evolving state"

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data TPraos c

-- | TPraos parameters that are node independent
data TPraosParams = TPraosParams {
      tpraosEpochInfo         :: !(EpochInfo Identity)
      -- | See 'Globals.slotsPerKESPeriod'.
    , tpraosSlotsPerKESPeriod :: !Word64
      -- | Active slots coefficient. This parameter represents the proportion
      -- of slots in which blocks should be issued. This can be interpreted as
      -- the probability that a party holding all the stake will be elected as
      -- leader for a given slot.
    , tpraosLeaderF           :: !Double
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
    }
  deriving (Generic, NoUnexpectedThunks)

data TPraosIsCoreNodeOrNot c
  = TPraosIsACoreNode !(TPraosIsCoreNode c)
  | TPraosIsNotACoreNode
  deriving (Generic, NoUnexpectedThunks)

data TPraosIsCoreNode c = TPraosIsCoreNode {
      -- | Certificate delegating rights from the stake pool cold key (or
      -- genesis stakeholder delegate cold key) to the online KES key.
      tpraosIsCoreNodeOpCert     :: !(SL.OCert c)
    -- | Stake pool cold key or genesis stakeholder delegate cold key.
    , tpraosIsCoreNodeColdVerKey :: !(SL.VKey c)
    , tpraosIsCoreNodeSignKeyVRF :: !(SignKeyVRF (VRF c))
    }
  deriving (Generic)

instance Crypto c => NoUnexpectedThunks (TPraosIsCoreNode c)

-- | Assembled proof that the issuer has the right to issue a block in the
-- selected slot.
data TPraosProof c = TPraosProof {
      tpraosEta        :: CertifiedVRF (VRF c) SL.Nonce
    , tpraosLeader     :: CertifiedVRF (VRF c) SL.UnitInterval
    , tpraosIsCoreNode :: TPraosIsCoreNode c
    }
  deriving (Generic)

instance TPraosCrypto c => NoUnexpectedThunks (TPraosProof c)

-- | Static configuration
data instance ConsensusConfig (TPraos c) = TPraosConfig {
      tpraosParams          :: !TPraosParams
    , tpraosIsCoreNodeOrNot :: !(TPraosIsCoreNodeOrNot c)
    }
  deriving (Generic)

-- Use generic instance
instance TPraosCrypto c => NoUnexpectedThunks (ConsensusConfig (TPraos c))

instance TPraosCrypto c => ConsensusProtocol (TPraos c) where

  type ConsensusState  (TPraos c) = TPraosState c
  type IsLeader        (TPraos c) = TPraosProof c
  type LedgerView      (TPraos c) = SL.LedgerView c
  type ValidationErr   (TPraos c) = [[STS.PredicateFailure (STS.PRTCL c)]]
  type ValidateView    (TPraos c) = TPraosValidateView c

  -- TODO override compareCandidates and preferCandidate to check the
  -- certificate number?

  protocolSecurityParam = tpraosSecurityParam . tpraosParams

  checkIfCanBeLeader TPraosConfig{tpraosIsCoreNodeOrNot} =
    case tpraosIsCoreNodeOrNot of
      TPraosIsACoreNode{}  -> True
      TPraosIsNotACoreNode -> False

  checkIsLeader cfg@TPraosConfig{..} slot lv cs =
    case tpraosIsCoreNodeOrNot of
      TPraosIsNotACoreNode          -> return Nothing
      TPraosIsACoreNode isACoreNode -> go isACoreNode
    where
      go :: MonadRandom m => TPraosIsCoreNode c -> m (Maybe (TPraosProof c))
      go icn = do
        let TPraosIsCoreNode {
                tpraosIsCoreNodeColdVerKey
              , tpraosIsCoreNodeSignKeyVRF
              } = icn
            prtclState = State.currentPRTCLState cs
            eta0       = prtclStateEta0 prtclState
            vkhCold    = SL.hashKey tpraosIsCoreNodeColdVerKey
            t          = leaderThreshold cfg lv vkhCold
            rho'       = SL.mkSeed SL.seedEta slot eta0
            y'         = SL.mkSeed SL.seedL   slot eta0
        rho <- VRF.evalCertified () rho' tpraosIsCoreNodeSignKeyVRF
        y   <- VRF.evalCertified () y'   tpraosIsCoreNodeSignKeyVRF
        -- First, check whether we're in the overlay schedule
        return $ case Map.lookup slot (SL.lvOverlaySched lv) of
          Nothing
            | fromIntegral (VRF.certifiedNatural y) < t
              -- Slot isn't in the overlay schedule, so we're in Praos
            -> Just TPraosProof {
                 tpraosEta        = coerce rho
               , tpraosLeader     = coerce y
               , tpraosIsCoreNode = icn
               }
            | otherwise
            -> Nothing

          -- This is a non-active slot; nobody may produce a block
          Just SL.NonActiveSlot -> Nothing

          -- The given genesis key has authority to produce a block in this
          -- slot. Check whether we're its delegate.
          Just (SL.ActiveSlot gkhash) -> case Map.lookup gkhash dlgMap of
              Just dlgHash | dlgHash == vkhCold -> Just TPraosProof {
                  tpraosEta        = coerce rho
                  -- Note that this leader value is not checked for slots in
                  -- the overlay schedule, so we could set it to whatever we
                  -- want. We evaluate it as normal for simplicity's sake.
                , tpraosLeader     = coerce y
                , tpraosIsCoreNode = icn
                }
              _ -> Nothing
            where
              SL.GenDelegs dlgMap = SL.lvGenDelegs lv

  updateConsensusState TPraosConfig{..} lv b cs = do
      newCS <- except . flip runReader shelleyGlobals $
        applySTS @(STS.PRTCL c) $ STS.TRC (prtclEnv, prtclState, b)
      return
        $ State.prune (fromIntegral k)
        $ State.append newCS cs
    where
      slot = SL.bheaderSlotNo $ SL.bhbody b
      epochInfo = tpraosEpochInfo tpraosParams
      SecurityParam k = tpraosSecurityParam tpraosParams
      shelleyGlobals = mkShelleyGlobals tpraosParams

      prtclEnv :: STS.PrtclEnv c
      prtclEnv = SL.mkPrtclEnv
        lv
        slot
        (isNewEpoch epochInfo slot (State.lastSlot cs))

      prtclState :: STS.PrtclState c
      prtclState = State.currentPRTCLState cs

  -- Rewind the chain state
  --
  -- We don't roll back to the exact slot since that slot might not have been
  -- filled; instead we roll back the the block just before it.
  rewindConsensusState TPraosConfig{..} cs rewindTo =
    State.rewind (pointSlot rewindTo) cs

mkShelleyGlobals :: TPraosParams -> SL.Globals
mkShelleyGlobals TPraosParams {..} = SL.Globals {
      epochInfo         = tpraosEpochInfo
    , slotsPerKESPeriod = tpraosSlotsPerKESPeriod
      -- TODO where does 3 * k come from?
    , slotsPrior        = 3 * k
    , startRewards      = 3 * k
    , securityParameter = k
    , maxKESEvo         = tpraosMaxKESEvo
    , quorum            = tpraosQuorum
    , maxMajorPV        = tpraosMaxMajorPV
    , maxLovelaceSupply = tpraosMaxLovelaceSupply
    }
  where
    SecurityParam k = tpraosSecurityParam

phi :: ConsensusConfig (TPraos c) -> Rational -> Double
phi TPraosConfig { tpraosParams } r =
    1 - (1 - tpraosLeaderF) ** fromRational r
  where
    TPraosParams { tpraosLeaderF } = tpraosParams

leaderThreshold :: forall c. TPraosCrypto c
                => ConsensusConfig (TPraos c)
                -> LedgerView (TPraos c)
                -> SL.KeyHash c  -- ^ Key hash of the pool
                -> Double
leaderThreshold cfg ledgerView keyHash =
    2 ^ (byteCount (Proxy @(HASH c)) * 8) * phi cfg a
  where
    SL.PoolDistr poolDistr = SL.lvPoolDistr ledgerView
    a = maybe 0 fst $ Map.lookup keyHash poolDistr

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance (Condense toSign, TPraosCrypto c)
  => Condense (TPraosFields c toSign) where
  -- TODO Nicer 'Condense' instance
  condense = condense . tpraosToSign
