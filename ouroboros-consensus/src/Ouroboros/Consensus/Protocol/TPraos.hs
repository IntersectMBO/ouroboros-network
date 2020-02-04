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

-- | Transitional Praos.
--
--   Transitional praos allows for the overlaying of Praos with an overlay
--   schedule determining slots to be produced by BFT
module Ouroboros.Consensus.Protocol.TPraos (
    TPraos
  , TPraosFields(..)
  , TPraosToSign(..)
  , TPraosParams(..)
  , TPraosProof(..)
  , TPraosIsCoreNode(..)
  , forgeTPraosFields
    -- * Tags
  , TPraosCrypto
  , TPraosStandardCrypto
  , TPraosMockCrypto
    -- * Type instances
  , NodeConfig(..)
  ) where

import           BaseTypes (Globals (..), Nonce, UnitInterval)
import           BlockChain (BHeader, HashHeader, mkSeed, seedEta, seedL, bheaderSlotNo, bhbody)
import           Cardano.Crypto.DSIGN.Class (VerKeyDSIGN)
import           Cardano.Crypto.Hash.Class (HashAlgorithm (..))
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.VRF.Class
import qualified Cardano.Ledger.Shelley.API as Shelley
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Cardano.Slotting.EpochInfo
import           Control.Monad.Except (ExceptT (..))
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.State.Transition.Extended (TRC (..), applySTS)
import           Crypto.Random (MonadRandom)
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           Delegation.Certificates (PoolDistr (..))
import           GHC.Generics (Generic)
import           Keys (DiscVKey (..), GenDelegs (..), KeyHash, hashKey)
import           OCert (OCert (..))
import           Ouroboros.Consensus.BlockchainTime (SlotLength,
                     singletonSlotLengths)
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Protocol.TPraos.ChainState as ChainState
import           Ouroboros.Consensus.Protocol.TPraos.Crypto
import           Ouroboros.Consensus.Protocol.TPraos.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Block (SlotNo (..),
                     pointSlot)
import qualified STS.Prtcl as STS

{-------------------------------------------------------------------------------
  Fields required by TPraos in the header
-------------------------------------------------------------------------------}

data TPraosFields c toSign = TPraosFields
  { tpraosSignature :: SignedKES (KES c) toSign
  , tpraosToSign    :: toSign
  }
  deriving Generic

instance (NoUnexpectedThunks toSign, TPraosCrypto c)
  => NoUnexpectedThunks (TPraosFields c toSign)
deriving instance (Show toSign, TPraosCrypto c)
  => Show (TPraosFields c toSign)

-- | Fields arising from transitional praos execution which must be included in
-- the block signature.
data TPraosToSign c = TPraosToSign
  { -- | Verification key for the issuer of this block. Note that unlike in Classic/BFT
    --   where we have a key for the genesis delegate on whose behalf we are
    --   issuing this block, this key corresponds to the stake pool/core node
    --   actually forging the block.
    tptsIssuerVK :: VerKeyDSIGN (DSIGN c)
  , tptsVrfVK    :: VerKeyVRF (VRF c)
    -- | Verifiable result containing the updated nonce value.
  , tptsEta      :: CertifiedVRF (VRF c) Nonce
    -- | Verifiable proof of the leader value, used to determine whether the
    -- node has the right to issue a block in this slot.
    --
    -- We include a value here even for blocks forged under the BFT schedule. It
    -- is not required that such a value be verifiable (though by default it
    -- will be verifiably correct, but unused.)
  , tptsLeader   :: CertifiedVRF (VRF c) UnitInterval
    -- Lightweight delegation certificate mapping the cold (DSIGN) key to the
    -- online KES key.
  , tptsOCert    :: OCert c
  }
  deriving Generic

instance TPraosCrypto c => NoUnexpectedThunks (TPraosToSign c)
deriving instance TPraosCrypto c => Show (TPraosToSign c)

-- Because we are using the executable spec, rather than implementing the
-- protocol directly here, we have a fixed header type rather than an
-- abstraction. So our validate view is fixed to this.
type TPraosValidateView c = BHeader c

forgeTPraosFields :: ( HasNodeState (TPraos cfg c) m
                    , MonadRandom m
                    , TPraosCrypto c
                    , Cardano.Crypto.KES.Class.Signable (KES c) toSign
                    )
                 => NodeConfig (TPraos cfg c)
                 -> TPraosProof c
                 -> (TPraosToSign c -> toSign)
                 -> m (TPraosFields c toSign)
forgeTPraosFields TPraosNodeConfig{..}  TPraosProof{..} mkToSign = do
    let icn@TPraosIsCoreNode{..} = tpraosIsCoreNode
        (DiscVKey issuerVK) = ocertVkCold tpraosIsCoreNodeOpCert
        signedFields = TPraosToSign {
          tptsIssuerVK = issuerVK
        , tptsVrfVK = deriveVerKeyVRF tpraosIsCoreNodeSignKeyVRF
        , tptsEta = tpraosEta
        , tptsLeader = tpraosLeader
        , tptsOCert = tpraosIsCoreNodeOpCert
        }
    m <- signedKES
          ()
          (fromIntegral (unSlotNo tpraosProofSlot))
          (mkToSign signedFields)
          tpraosIsCoreNodeSKSHot
    case m of
      Nothing                  -> error "mkOutoborosPayload: signedKES failed"
      Just signature ->
        return $ TPraosFields {
            tpraosSignature    = signature
          , tpraosToSign       = (mkToSign signedFields)
          }

{-------------------------------------------------------------------------------
  TPraos specific types
-------------------------------------------------------------------------------}

-- | Assembled proof that the issuer has the right to issue a block in the
-- selected slot.
data TPraosProof c
  = TPraosProof
    { tpraosEta        :: CertifiedVRF (VRF c) Nonce
    , tpraosLeader     :: CertifiedVRF (VRF c) UnitInterval
    , tpraosProofSlot  :: SlotNo
    , tpraosIsCoreNode :: TPraosIsCoreNode c
    } deriving Generic

instance TPraosCrypto c => NoUnexpectedThunks (TPraosProof c)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data TPraos cfg c

-- | TPraos parameters that are node independent
data TPraosParams = TPraosParams {
      -- | Active slots coefficient. This parameter represents the proportion of
      -- slots in which blocks should be issued. This can be interpreted as the
      -- probability that a party holding all the stake will be elected as
      -- leader for a given slot.
      tpraosLeaderF       :: Double
    , tpraosSecurityParam :: SecurityParam
    , tpraosEpochInfo     :: EpochInfo Identity
    , tpraosKESPeriod     :: Word64
    , tpraosSlotLength    :: SlotLength
    } deriving Generic

instance NoUnexpectedThunks TPraosParams

data TPraosIsCoreNode c = TPraosIsCoreNode
  { -- | Online KES key used to sign blocks.GHC.Generics
    tpraosIsCoreNodeSKSHot :: SignKeyKES (KES c)
    -- | Certificate delegating rights from the stake pool cold key (or genesis
    -- stakeholder delegate cold key) to the online KES key.
  , tpraosIsCoreNodeOpCert :: OCert c
  , tpraosIsCoreNodeSignKeyVRF    :: SignKeyVRF (VRF c)
  } deriving Generic

instance
  ( Crypto c
  ) => NoUnexpectedThunks (TPraosIsCoreNode c)

instance
  ( NoUnexpectedThunks cfg
  , Typeable cfg
  , TPraosCrypto c)
  => OuroborosTag (TPraos cfg c) where

  protocolSecurityParam = tpraosSecurityParam . tpraosParams
  protocolSlotLengths   = singletonSlotLengths . tpraosSlotLength . tpraosParams

  type NodeState       (TPraos cfg c) = Maybe (TPraosIsCoreNode c)
  type LedgerView      (TPraos cfg c) = Shelley.LedgerView c
  type IsLeader        (TPraos cfg c) = TPraosProof c
  type ValidationErr   (TPraos cfg c) = [[STS.PredicateFailure (STS.PRTCL c)]]
  type ChainState      (TPraos cfg c) = ChainState.TPraosChainState c
  type ValidateView    (TPraos cfg c) = TPraosValidateView c

  checkIsLeader cfg@TPraosNodeConfig{..} slot lv cs =
    getNodeState >>= \case
        Nothing -> return Nothing
        Just icn@TPraosIsCoreNode
            { tpraosIsCoreNodeOpCert
            , tpraosIsCoreNodeSignKeyVRF
            } -> do
          let mkSeed' = mkSeed @c
              vkhCold = hashKey $ ocertVkCold tpraosIsCoreNodeOpCert
              t = leaderThreshold cfg lv vkhCold
              eta0 = prtclEta0 $ ChainState.toPRTCLState cs
              prevHash = prtclStateHash @c $ ChainState.toPRTCLState cs
              rho' = mkSeed' seedEta slot eta0 prevHash
              y' = mkSeed' seedL slot eta0 prevHash
          rho <- evalCertified () rho' tpraosIsCoreNodeSignKeyVRF
          y   <- evalCertified () y'   tpraosIsCoreNodeSignKeyVRF
          -- First, check whether we're in the overlay schedule
          case (Map.lookup slot $ Shelley.lvOverlaySched lv) of
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
              let GenDelegs dlgMap = Shelley.lvGenDelegs lv
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
    let slot = bheaderSlotNo $ bhbody b
        ei = tpraosEpochInfo tpraosParams
        SecurityParam k = tpraosSecurityParam tpraosParams
        shelleyGlobs = Globals
          { epochInfo = ei
          , slotsPerKESPeriod = tpraosKESPeriod tpraosParams
          , securityParameter = k
          , startRewards = 3 * k
          , slotsPrior = 3 * k
          }

    newCS <- ExceptT . return . runIdentity . flip runReaderT shelleyGlobs $ applySTS @(STS.PRTCL c)
      $ TRC ( Shelley.mkPrtclEnv
                lv
                slot
                (isNewEpoch ei slot (ChainState.lastSlot cs))
            , ChainState.toPRTCLState cs
            , b
            )

    return . ChainState.prune (fromIntegral k) $ ChainState.appendState newCS cs

  -- Rewind the chain state
  --
  -- We don't roll back to the exact slot since that slot might not have been
  -- filled; instead we roll back the the block just before it.
  rewindChainState TPraosNodeConfig{..} cs rewindTo
    = ChainState.rewind (pointSlot rewindTo) cs

-- Use generic instance
instance (NoUnexpectedThunks cfg, VRFAlgorithm (VRF c)) => NoUnexpectedThunks (NodeConfig (TPraos cfg c))

data instance NodeConfig (TPraos cfg c) = TPraosNodeConfig
  { tpraosParams        :: TPraosParams
  , tpraosExtraConfig   :: cfg
  } deriving Generic


phi :: NodeConfig (TPraos cfg c) -> Rational -> Double
phi TPraosNodeConfig{..} r = 1 - (1 - tpraosLeaderF) ** fromRational r
  where
    TPraosParams{..} = tpraosParams

leaderThreshold :: forall cfg c. TPraosCrypto c
                => NodeConfig (TPraos cfg c)
                -> LedgerView (TPraos cfg c)
                -> KeyHash c -- ^ Key hash of the pool
                -> Double
leaderThreshold nc lv kh =
    let PoolDistr pd = Shelley.lvPoolDistr lv
        a = maybe 0 fst $ Map.lookup kh pd
    in  2 ^ (byteCount (Proxy :: Proxy (HASH c)) * 8) * phi nc a

prtclStateHash
  :: STS.State (STS.PRTCL c)
  -> BlockChain.HashHeader c
prtclStateHash (STS.PrtclState _ h _ _ _ _ _) = h

prtclEta0
  :: STS.State (STS.PRTCL c)
  -> Nonce
prtclEta0 (STS.PrtclState _ _ _ eta0 _ _ _) = eta0

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance (Condense c, Condense toSign, TPraosCrypto c)
  => Condense (TPraosFields c toSign) where
  -- TODO Nicer 'Condense' instance
  condense = condense . tpraosToSign
