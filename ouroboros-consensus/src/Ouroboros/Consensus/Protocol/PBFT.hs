{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Ouroboros.Consensus.Protocol.PBFT (
    PBft
  , PBftLedgerView(..)
  , PBftFields(..)
  , PBftParams(..)
  , PBftIsLeader(..)
  , PBftIsLeaderOrNot(..)
  , pbftWindowSize
    -- * Forging
  , forgePBftFields
    -- * Classes
  , PBftCrypto(..)
  , PBftMockCrypto
  , PBftValidateView(..)
  , pbftValidateRegular
  , pbftValidateBoundary
    -- * Type instances
  , ConsensusConfig(..)
    -- * Exported for testing
  , PBftValidationErr(..)
  ) where

import           Codec.Serialise (Serialise (..))
import qualified Control.Exception as Exn
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Crypto.DSIGN.Class
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (BlockNo, pattern BlockPoint,
                     pattern GenesisPoint, HasHeader (..), HeaderHash, Point,
                     SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT.Crypto
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import           Ouroboros.Consensus.Protocol.PBFT.State.HeaderHashBytes
                     (HeaderHashBytes, headerHashBytes)
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  Fields that PBFT requires present in a block
-------------------------------------------------------------------------------}

data PBftFields c toSign = PBftFields {
      -- | The actual issuer of a block
      pbftIssuer    :: VerKeyDSIGN (PBftDSIGN c)
      -- | The stakeholder on whose behalf the block is being issued
    , pbftGenKey    :: VerKeyDSIGN (PBftDSIGN c)
    , pbftSignature :: SignedDSIGN (PBftDSIGN c) toSign
    }
  deriving (Generic)

deriving instance PBftCrypto c => Show (PBftFields c toSign)
deriving instance PBftCrypto c => Eq   (PBftFields c toSign)

instance (PBftCrypto c, Typeable toSign) => NoUnexpectedThunks (PBftFields c toSign)
  -- use generic instance

-- | Part of the header that we validate
data PBftValidateView c =
     -- | Regular block
     --
     -- Regular blocks are signed, and so we need to validate them.
     -- We also need to know the slot number of the block
     forall signed. Signable (PBftDSIGN c) signed
                 => PBftValidateRegular
                      SlotNo
                      (PBftFields c signed)
                      signed
                      (ContextDSIGN (PBftDSIGN c))

     -- | Boundary block (EBB)
     --
     -- EBBs are not signed but do affect the consensus state.
   | PBftValidateBoundary SlotNo HeaderHashBytes

-- | Convenience constructor for 'PBftValidateView' for regular blocks
pbftValidateRegular :: ( HasHeader    hdr
                       , SignedHeader hdr
                       , Signable (PBftDSIGN c) (Signed hdr)
                       )
                    => ContextDSIGN (PBftDSIGN c)
                    -> (hdr -> PBftFields c (Signed hdr))
                    -> (hdr -> PBftValidateView c)
pbftValidateRegular contextDSIGN getFields hdr =
    PBftValidateRegular
      (blockSlot hdr)
      (getFields hdr)
      (headerSigned hdr)
      contextDSIGN

-- | Convenience constructor for 'PBftValidateView' for boundary blocks
pbftValidateBoundary :: forall hdr c. (
                          HasHeader hdr
                        , Serialise (HeaderHash hdr)
                        )
                     => hdr -> PBftValidateView c
pbftValidateBoundary hdr =
    PBftValidateBoundary
      (blockSlot hdr)
      (headerHashBytes (Proxy @hdr) (blockHash hdr))

-- | Part of the header required for chain selection
--
-- EBBs share a block number with regular blocks, and so for chain selection
-- we need to know if a block is an EBB or not (because a chain ending on an
-- EBB with a particular block number is longer than a chain on a regular
-- block with that same block number).
type PBftSelectView = (BlockNo, IsEBB)

{-------------------------------------------------------------------------------
  Block forging
-------------------------------------------------------------------------------}

forgePBftFields :: forall m c toSign. (
                       MonadRandom m
                     , PBftCrypto c
                     , Signable (PBftDSIGN c) toSign
                     )
                => (VerKeyDSIGN (PBftDSIGN c) -> ContextDSIGN (PBftDSIGN c))
                -- ^ Construct DSIGN context given 'pbftGenKey'
                -> IsLeader (PBft c)
                -> toSign
                -> m (PBftFields c toSign)
forgePBftFields contextDSIGN PBftIsLeader{..} toSign = do
    signature <- signedDSIGN ctxtDSIGN toSign pbftSignKey
    return $ Exn.assert (issuer == deriveVerKeyDSIGN pbftSignKey) $ PBftFields {
        pbftIssuer    = issuer
      , pbftGenKey    = genKey
      , pbftSignature = signature
      }
  where
    issuer    = dlgCertDlgVerKey pbftDlgCert
    genKey    = dlgCertGenVerKey pbftDlgCert
    ctxtDSIGN = contextDSIGN genKey

{-------------------------------------------------------------------------------
  Information PBFT requires from the ledger
-------------------------------------------------------------------------------}

data PBftLedgerView c = PBftLedgerView {
    -- | ProtocolParameters: map from genesis to delegate keys.
    pbftDelegates :: !(Bimap (PBftVerKeyHash c) (PBftVerKeyHash c))
  }
  deriving (Generic)

deriving instance PBftCrypto c => NoUnexpectedThunks (PBftLedgerView c)
  -- use generic instance

deriving instance Eq (PBftVerKeyHash c) => Eq (PBftLedgerView c)
deriving instance Show (PBftVerKeyHash c) => Show (PBftLedgerView c)

instance (Serialise (PBftVerKeyHash c), Ord (PBftVerKeyHash c))
      => Serialise (PBftLedgerView c) where
  encode (PBftLedgerView ds) = encode (Bimap.toList ds)
  decode = PBftLedgerView . Bimap.fromList <$> decode

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | Permissive BFT
--
-- As defined in https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec
data PBft c

-- | Protocol parameters
data PBftParams = PBftParams {
      -- | Security parameter
      --
      -- Although the protocol proper does not have such a security parameter,
      -- we insist on it.
      pbftSecurityParam      :: !SecurityParam

      -- | Number of core nodes
    , pbftNumNodes           :: !NumCoreNodes

      -- | Signature threshold
      --
      -- This bounds the proportion of the latest 'pbftSecurityParam'-many
      -- blocks which is allowed to be signed by any single key. The protocol
      -- proper is parameterized over the size of this window of recent blocks,
      -- but this implementation follows the specification by fixing that
      -- parameter to the ambient security parameter @k@.
    , pbftSignatureThreshold :: !Double
    }
  deriving (Generic, NoUnexpectedThunks, Show)

-- | If we are a core node (i.e. a block producing node) we know which core
-- node we are, and we have the operational key pair and delegation certificate.
--
data PBftIsLeader c = PBftIsLeader {
      pbftCoreNodeId :: !CoreNodeId
    , pbftSignKey    :: !(SignKeyDSIGN (PBftDSIGN c))
    , pbftDlgCert    :: !(PBftDelegationCert c)
    }
  deriving (Generic)

instance PBftCrypto c => NoUnexpectedThunks (PBftIsLeader c)
 -- use generic instance

data PBftIsLeaderOrNot c
  = PBftIsALeader !(PBftIsLeader c)
  | PBftIsNotALeader
  deriving (Generic, NoUnexpectedThunks)

-- | (Static) node configuration
data instance ConsensusConfig (PBft c) = PBftConfig {
      pbftParams   :: !PBftParams
    , pbftIsLeader :: !(PBftIsLeaderOrNot c)
    }
  deriving (Generic, NoUnexpectedThunks)

instance PBftCrypto c => ConsensusProtocol (PBft c) where
  type ValidationErr (PBft c) = PBftValidationErr c
  type ValidateView  (PBft c) = PBftValidateView  c
  type SelectView    (PBft c) = PBftSelectView

  -- | We require two things from the ledger state:
  --
  --   - Protocol parameters, for the signature window and threshold.
  --   - The delegation map.
  type LedgerView     (PBft c) = PBftLedgerView c
  type IsLeader       (PBft c) = PBftIsLeader   c
  type ConsensusState (PBft c) = PBftState      c

  protocolSecurityParam = pbftSecurityParam . pbftParams

  checkIfCanBeLeader PBftConfig{pbftIsLeader} =
      case pbftIsLeader of
        PBftIsALeader{}  -> True
        PBftIsNotALeader -> False

  checkIsLeader PBftConfig{pbftIsLeader, pbftParams} (SlotNo n) _l _cs =
      case pbftIsLeader of
        PBftIsNotALeader                           -> return Nothing

        -- We are the slot leader based on our node index, and the current
        -- slot number. Our node index depends which genesis key has delegated
        -- to us, see 'genesisKeyCoreNodeId'.
        PBftIsALeader credentials
          | n `mod` numCoreNodes == i -> return (Just credentials)
          | otherwise                 -> return Nothing
          where
            PBftIsLeader{pbftCoreNodeId = CoreNodeId i} = credentials
            PBftParams{pbftNumNodes = NumCoreNodes numCoreNodes} = pbftParams

  updateConsensusState cfg@PBftConfig{..} lv@(PBftLedgerView dms) toValidate state =
      case toValidate of
        PBftValidateBoundary slot hash ->
          return $! appendEBB cfg params slot hash state
        PBftValidateRegular slot PBftFields{..} signed contextDSIGN -> do
          -- Check that the issuer signature verifies, and that it's a delegate of a
          -- genesis key, and that genesis key hasn't voted too many times.
          case verifySignedDSIGN
                 contextDSIGN
                 pbftIssuer
                 signed
                 pbftSignature of
            Right () -> return ()
            Left err -> throwError $ PBftInvalidSignature (Text.pack err)

          -- FIXME confirm that non-strict inequality is ok in general.
          -- It's here because EBBs have the same slot as the first block of their
          -- epoch.
          unless (At slot >= S.lastSignedSlot state)
            $ throwError PBftInvalidSlot

          case Bimap.lookupR (hashVerKey pbftIssuer) dms of
            Nothing -> throwError $ PBftNotGenesisDelegate (hashVerKey pbftIssuer) lv
            Just gk -> do
              let state' = append cfg params (slot, gk) state
              case exceedsThreshold params state' gk of
                Nothing -> return $! state'
                Just n  -> throwError $ PBftExceededSignThreshold gk n
    where
      params = pbftWindowParams cfg

  rewindConsensusState cfg = flip (rewind cfg params)
    where
      params = pbftWindowParams cfg

  compareCandidates PBftConfig{..} (lBlockNo, lIsEBB) (rBlockNo, rIsEBB) =
      -- Prefer the highest block number, as it is a proxy for chain length
      case lBlockNo `compare` rBlockNo of
        LT -> LT
        GT -> GT
        -- If the block numbers are the same, check if one of them is an EBB.
        -- An EBB has the same block number as the block before it, so the
        -- chain ending with an EBB is actually longer than the one ending
        -- with a regular block.
        EQ -> score lIsEBB `compare` score rIsEBB
     where
       score :: IsEBB -> Int
       score IsEBB    = 1
       score IsNotEBB = 0

{-------------------------------------------------------------------------------
  Internal: thin wrapper on top of 'PBftState'
-------------------------------------------------------------------------------}

-- | Parameters for the window check
data PBftWindowParams = PBftWindowParams {
      -- | Window size
      windowSize :: S.WindowSize

      -- | Threshold (maximum number of slots anyone is allowed to sign)
    , threshold  :: Word64
    }

-- | Compute window check parameters from the node config
pbftWindowParams :: ConsensusConfig (PBft c) -> PBftWindowParams
pbftWindowParams PBftConfig{..} = PBftWindowParams {
      windowSize = winSize
    , threshold  = floor $ pbftSignatureThreshold * fromIntegral winSize
    }
  where
    PBftParams{..} = pbftParams
    winSize        = pbftWindowSize pbftSecurityParam

-- | Window size used by PBFT
--
-- We set the window size to be equal to k.
pbftWindowSize :: SecurityParam -> S.WindowSize
pbftWindowSize (SecurityParam k) = S.WindowSize k

-- | Does the number of blocks signed by this key exceed the threshold?
--
-- Returns @Just@ the number of blocks signed if exceeded.
exceedsThreshold :: PBftCrypto c
                 => PBftWindowParams
                 -> PBftState c -> PBftVerKeyHash c -> Maybe Word64
exceedsThreshold PBftWindowParams{..} st gk =
    if numSigned > threshold
      then Just numSigned
      else Nothing
  where
    numSigned = S.countSignedBy st gk

append :: PBftCrypto c
       => ConsensusConfig (PBft c)
       -> PBftWindowParams
       -> (SlotNo, PBftVerKeyHash c)
       -> PBftState c -> PBftState c
append PBftConfig{..} PBftWindowParams{..} =
    S.append pbftSecurityParam windowSize . uncurry S.PBftSigner
  where
    PBftParams{..} = pbftParams

appendEBB :: forall c. PBftCrypto c
          => ConsensusConfig (PBft c)
          -> PBftWindowParams
          -> SlotNo
          -> HeaderHashBytes
          -> PBftState c -> PBftState c
appendEBB PBftConfig{..} PBftWindowParams{..} =
    S.appendEBB pbftSecurityParam windowSize
  where
    PBftParams{..} = pbftParams

rewind :: forall c hdr. (PBftCrypto c, Serialise (HeaderHash hdr))
       => ConsensusConfig (PBft c)
       -> PBftWindowParams
       -> Point hdr
       -> PBftState c
       -> Maybe (PBftState c)
rewind PBftConfig{..} PBftWindowParams{..} p =
    S.rewind pbftSecurityParam windowSize p'
  where
    PBftParams{..} = pbftParams
    p' = case p of
      GenesisPoint    -> Origin
      BlockPoint s hh -> At (s, headerHashBytes (Proxy :: Proxy hdr) hh)

{-------------------------------------------------------------------------------
  PBFT specific types
-------------------------------------------------------------------------------}

-- | NOTE: this type is stored in the state, so it must be in normal form to
-- avoid space leaks.
data PBftValidationErr c
  = PBftInvalidSignature !Text
  | PBftNotGenesisDelegate !(PBftVerKeyHash c) !(PBftLedgerView c)
  -- | We record how many slots this key signed
  | PBftExceededSignThreshold !(PBftVerKeyHash c) !Word64
  | PBftInvalidSlot
  deriving (Generic, NoUnexpectedThunks)

deriving instance PBftCrypto c => Show (PBftValidationErr c)
deriving instance PBftCrypto c => Eq   (PBftValidationErr c)

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance PBftCrypto c => Condense (PBftFields c toSign) where
  condense PBftFields{..} = condense pbftSignature
