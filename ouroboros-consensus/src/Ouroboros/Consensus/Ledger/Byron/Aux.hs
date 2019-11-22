{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Auxiliary definitions to make working with the Byron ledger easier
--
-- NOTE: None of these definitions depend on @ouroboros-network@ or
-- @ouroboros-consensus@ and could probably be moved to @cardano-ledger@.
module Ouroboros.Consensus.Ledger.Byron.Aux (
    -- * Extract info from genesis config
    allowedDelegators
  , boundaryBlockSlot
    -- * Extract info from chain state
  , getDelegationMap
  , getProtocolParams
  , getScheduledDelegations
    -- * Applying blocks
  , applyEpochTransition
  , validateBlock
  , validateBoundary
  , applyScheduledDelegations
    -- * Applying transactions
  , ApplyMempoolPayloadErr(..)
  , applyMempoolPayload
  , mempoolPayloadRecoverBytes
  , mempoolPayloadReencode
    -- * Annotations
  , reAnnotateBlock
  , reAnnotateBoundary
  , reAnnotateUsing
    -- * Headers
  , ABlockOrBoundaryHdr(..)
  , aBlockOrBoundaryHdr
  , fromCBORABlockOrBoundaryHdr
  , abobHdrFromBlock
  , abobHdrSlotNo
  , abobHdrChainDifficulty
  , abobHdrHash
  , abobHdrPrevHash
  , abobMatchesBody
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Either (isRight)
import qualified Data.Foldable as Foldable
import           Data.List (intercalate)
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Binary
import           Cardano.Crypto.ProtocolMagic
import           Cardano.Prelude (NoUnexpectedThunks, cborError, wrapError)

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as D.Iface
import qualified Cardano.Chain.Delegation.Validation.Scheduling as D.Sched
import qualified Cardano.Chain.Genesis as Gen
import qualified Cardano.Chain.MempoolPayload as CC
import qualified Cardano.Chain.Slotting as CC
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Update.Validation.Interface as U.Iface
import qualified Cardano.Chain.UTxO as Utxo
import qualified Cardano.Chain.ValidationMode as CC

-- NOTE: NO dependencies on ouroboros-network or ouroboros-consensus here!
-- This stuff could/should eventually be moved to cardano-ledger.

{-------------------------------------------------------------------------------
  Extract info from genesis config
-------------------------------------------------------------------------------}

allowedDelegators :: Gen.Config -> Set CC.KeyHash
allowedDelegators =
      Gen.unGenesisKeyHashes
    . Gen.configGenesisKeyHashes

-- | Compute the slot number assigned to a boundary block
boundaryBlockSlot :: CC.EpochSlots -> Word64 -> CC.SlotNumber
boundaryBlockSlot (CC.EpochSlots epochSlots) epoch =
    CC.SlotNumber $ epochSlots * epoch

{-------------------------------------------------------------------------------
  Extract info from chain state
-------------------------------------------------------------------------------}

getDelegationMap :: CC.ChainValidationState -> Delegation.Map
getDelegationMap =
      D.Iface.delegationMap
    . CC.cvsDelegationState

getProtocolParams :: CC.ChainValidationState -> Update.ProtocolParameters
getProtocolParams =
      U.Iface.adoptedProtocolParameters
    . CC.cvsUpdateState

getScheduledDelegations :: CC.ChainValidationState
                        -> Seq D.Sched.ScheduledDelegation
getScheduledDelegations =
      D.Sched.scheduledDelegations
    . D.Iface.schedulingState
    . CC.cvsDelegationState

{-------------------------------------------------------------------------------
  Update parts of the chain state
-------------------------------------------------------------------------------}

setUTxO :: Utxo.UTxO
        -> CC.ChainValidationState -> CC.ChainValidationState
setUTxO newUTxO state = state { CC.cvsUtxo = newUTxO }

setDelegationState :: D.Iface.State
                   -> CC.ChainValidationState -> CC.ChainValidationState
setDelegationState newDlg state = state { CC.cvsDelegationState = newDlg }

setUpdateState :: U.Iface.State
               -> CC.ChainValidationState -> CC.ChainValidationState
setUpdateState newUpdate state = state { CC.cvsUpdateState = newUpdate }

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

mkEpochEnvironment :: Gen.Config
                   -> CC.ChainValidationState
                   -> CC.EpochEnvironment
mkEpochEnvironment cfg state = CC.EpochEnvironment {
      CC.protocolMagic     = reAnnotateMagicId $
                               Gen.configProtocolMagicId cfg
    , CC.k                 = Gen.configK cfg
    , CC.allowedDelegators = allowedDelegators cfg
    , CC.delegationMap     = delegationMap
      -- The 'currentEpoch' required by the epoch environment is the /old/
      -- epoch (i.e., the one in the ledger state), so that we can verify that
      -- the new epoch indeed is after the old.
    , CC.currentEpoch      = CC.slotNumberEpoch
                               (Gen.configEpochSlots cfg)
                               (CC.cvsLastSlot state)
    }
  where
    delegationMap :: Delegation.Map
    delegationMap = D.Iface.delegationMap $ CC.cvsDelegationState state

mkBodyState :: CC.ChainValidationState -> CC.BodyState
mkBodyState state = CC.BodyState {
      CC.utxo            = CC.cvsUtxo            state
    , CC.updateState     = CC.cvsUpdateState     state
    , CC.delegationState = CC.cvsDelegationState state
    }

-- TODO: Unlike 'mkEpochEnvironment' and 'mkDelegationEnvironment', for the
-- body processing we set 'currentEpoch' to the epoch of the block rather than
-- the current epoch (in the state). Is that deliberate?
mkBodyEnvironment :: Gen.Config
                  -> Update.ProtocolParameters
                  -> CC.SlotNumber
                  -> CC.BodyEnvironment
mkBodyEnvironment cfg params slotNo = CC.BodyEnvironment {
      CC.protocolMagic      = reAnnotateMagic $ Gen.configProtocolMagic cfg
    , CC.utxoConfiguration  = Gen.configUTxOConfiguration cfg
    , CC.k                  = Gen.configK cfg
    , CC.allowedDelegators  = allowedDelegators cfg
    , CC.protocolParameters = params
      -- The 'currentEpoch' for validating a block should be the /current/
      -- epoch (that is, the epoch of the block), /not/ the old epoch
      -- (from the ledger state). This is to make sure delegation certificates
      -- are for the /next/ epoch.
    , CC.currentEpoch       = CC.slotNumberEpoch
                                (Gen.configEpochSlots cfg)
                                slotNo
    }

applyEpochTransition :: Gen.Config
                     -> CC.SlotNumber
                     -> CC.ChainValidationState
                     -> CC.ChainValidationState
applyEpochTransition cfg slotNo state = state {
      CC.cvsUpdateState = CC.epochTransition
                            (mkEpochEnvironment cfg state)
                            (CC.cvsUpdateState state)
                            slotNo
    }

-- | Validate header
--
-- NOTE: Header validation does not produce any state changes; the only state
-- changes arising from processing headers come from 'applyEpochTransition'.
validateHeader :: MonadError CC.ChainValidationError m
               => CC.ValidationMode
               -> U.Iface.State -> CC.Header -> m ()
validateHeader validationMode updState hdr =
    flip runReaderT validationMode $
      CC.headerIsValid updState hdr

validateBody :: MonadError CC.ChainValidationError m
             => CC.ValidationMode
             -> CC.Block
             -> CC.BodyEnvironment -> CC.BodyState -> m CC.BodyState
validateBody validationMode block bodyEnv bodyState =
    flip runReaderT validationMode $
      CC.updateBody bodyEnv bodyState block

validateBlock :: MonadError CC.ChainValidationError m
              => Gen.Config
              -> CC.ValidationMode
              -> CC.Block
              -> CC.HeaderHash
              -> CC.ChainValidationState -> m CC.ChainValidationState
validateBlock cfg validationMode block blkHash state = do

    -- TODO: How come this check isn't done in 'updateBlock'
    -- (but it /is/ done in 'updateChainBoundary')?
    --
    -- TODO: It could be argued that hash checking isn't part of consensus /or/
    -- the ledger. If we take that point of view serious, we should think about
    -- what that third thing is precisely and what its responsibilities are.
    case ( CC.cvsPreviousHash state
         , CC.headerPrevHash (CC.blockHeader block)
         ) of
      (Left gh, hh) ->
         throwError $ CC.ChainValidationExpectedGenesisHash gh hh
      (Right expected, actual) ->
         unless (expected == actual) $
           throwError $ CC.ChainValidationInvalidHash expected actual

    validateHeader validationMode updState (CC.blockHeader block)
    bodyState' <- validateBody validationMode block bodyEnv bodyState
    return state {
          CC.cvsLastSlot        = CC.blockSlot block
        , CC.cvsPreviousHash    = Right blkHash
        , CC.cvsUtxo            = CC.utxo            bodyState'
        , CC.cvsUpdateState     = CC.updateState     bodyState'
        , CC.cvsDelegationState = CC.delegationState bodyState'
        }
  where
    updState  = CC.cvsUpdateState state
    bodyEnv   = mkBodyEnvironment
                  cfg
                  (getProtocolParams state)
                  (CC.blockSlot block)
    bodyState = mkBodyState state

-- | Apply a boundary block
--
-- NOTE: The `cvsLastSlot` calculation must match the one in 'abobHdrSlotNo'.
validateBoundary :: MonadError CC.ChainValidationError m
                 => Gen.Config
                 -> CC.BoundaryBlock
                 -> CC.ChainValidationState -> m CC.ChainValidationState
validateBoundary cfg blk state = do
    -- TODO: Unfortunately, 'updateChainBoundary' doesn't take a hash as an
    -- argument but recomputes it.
    state' <- CC.updateChainBoundary state blk
    -- TODO: For some reason 'updateChainBoundary' does not set the slot when
    -- applying an EBB, so we do it here. Could that cause problems??
    return state' {
        CC.cvsLastSlot = boundaryBlockSlot epochSlots (CC.boundaryEpoch hdr)
      }
  where
    hdr        = CC.boundaryHeader blk
    epochSlots = Gen.configEpochSlots cfg

applyScheduledDelegations :: Seq D.Sched.ScheduledDelegation
                          -> Delegation.Map -> Delegation.Map
applyScheduledDelegations update (Delegation.Map del) =
    -- The order in which we apply the updates does not matter, because the spec
    -- says: "Any given key can issue at most one certificate in a given slot."
    Delegation.Map $ Foldable.foldl' (flip applyOne) del update
  where
    applyOne :: D.Sched.ScheduledDelegation
             -> Bimap CC.KeyHash CC.KeyHash
             -> Bimap CC.KeyHash CC.KeyHash
    applyOne x = Bimap.insert (D.Sched.sdDelegator x)
                              (D.Sched.sdDelegate  x)

{-------------------------------------------------------------------------------
  Applying transactions
-------------------------------------------------------------------------------}

mkUtxoEnvironment :: Gen.Config
                  -> CC.ChainValidationState
                  -> Utxo.Environment
mkUtxoEnvironment cfg state = Utxo.Environment {
      Utxo.protocolMagic      = protocolMagic
    , Utxo.protocolParameters = U.Iface.adoptedProtocolParameters updateState
    , Utxo.utxoConfiguration  = Gen.configUTxOConfiguration cfg
    }
  where
    protocolMagic = reAnnotateMagic (Gen.configProtocolMagic cfg)
    updateState   = CC.cvsUpdateState state

mkDelegationEnvironment :: Gen.Config
                        -> CC.ChainValidationState
                        -> D.Iface.Environment
mkDelegationEnvironment cfg state = D.Iface.Environment {
      D.Iface.protocolMagic     = getAProtocolMagicId protocolMagic
    , D.Iface.allowedDelegators = allowedDelegators cfg
    , D.Iface.k                 = k
      -- By rights the 'currentEpoch' for checking a delegation certificate
      -- should be the epoch of the block in which the delegation certificate
      -- is included. However, we don't have such a block yet, and so we can
      -- only use the epoch from the ledger state. This does mean that we might
      -- say a transaction is valid now, but will become invalid by the time we
      -- actually include it in a block.
    , D.Iface.currentEpoch      = currentEpoch
    , D.Iface.currentSlot       = currentSlot
    }
  where
    k             = Gen.configK cfg
    protocolMagic = reAnnotateMagic (Gen.configProtocolMagic cfg)
    currentSlot   = CC.cvsLastSlot state
    currentEpoch  = CC.slotNumberEpoch (Gen.configEpochSlots cfg) currentSlot

mkUpdateEnvironment :: Gen.Config
                    -> CC.ChainValidationState
                    -> U.Iface.Environment
mkUpdateEnvironment cfg state = U.Iface.Environment {
      U.Iface.protocolMagic = getAProtocolMagicId protocolMagic
    , U.Iface.k             = k
    , U.Iface.currentSlot   = currentSlot
    , U.Iface.numGenKeys    = numGenKeys
    , U.Iface.delegationMap = delegationMap
    }
  where
    k             = Gen.configK cfg
    protocolMagic = reAnnotateMagic (Gen.configProtocolMagic cfg)
    currentSlot   = CC.cvsLastSlot state
    numGenKeys    = toNumGenKeys $ Set.size (allowedDelegators cfg)
    delegationMap = getDelegationMap state

    -- TODO: This function comes straight from cardano-ledger, which however
    -- does not export it. We should either export it, or -- preferably -- when
    -- all of the functions in this module are moved to cardano-ledger, the
    -- function can just be used directly.
    toNumGenKeys :: Int -> Word8
    toNumGenKeys n
      | n > fromIntegral (maxBound :: Word8) = error $
        "toNumGenKeys: Too many genesis keys"
      | otherwise = fromIntegral n

applyTxAux :: MonadError Utxo.UTxOValidationError m
           => CC.ValidationMode
           -> Gen.Config
           -> [Utxo.TxAux]
           -> CC.ChainValidationState -> m CC.ChainValidationState
applyTxAux validationMode cfg txs state =
    flip runReaderT validationMode $
      (`setUTxO` state) <$>
        Utxo.updateUTxO utxoEnv utxo txs
  where
    utxoEnv = mkUtxoEnvironment cfg state
    utxo    = CC.cvsUtxo state

applyCertificate :: MonadError D.Sched.Error m
                 => Gen.Config
                 -> [Delegation.Certificate]
                 -> CC.ChainValidationState -> m CC.ChainValidationState
applyCertificate cfg certs state =
    (`setDelegationState` state) <$>
      D.Iface.updateDelegation dlgEnv dlgState certs
  where
    dlgEnv   = mkDelegationEnvironment cfg state
    dlgState = CC.cvsDelegationState state

applyUpdateProposal :: MonadError U.Iface.Error m
                    => Gen.Config
                    -> Update.Proposal
                    -> CC.ChainValidationState -> m CC.ChainValidationState
applyUpdateProposal cfg proposal state =
    (`setUpdateState` state) <$>
      U.Iface.registerProposal updateEnv updateState proposal
  where
    updateEnv   = mkUpdateEnvironment cfg state
    updateState = CC.cvsUpdateState state

applyUpdateVote :: MonadError U.Iface.Error m
                => Gen.Config
                -> Update.Vote
                -> CC.ChainValidationState -> m CC.ChainValidationState
applyUpdateVote cfg vote state =
    (`setUpdateState` state) <$>
      U.Iface.registerVote updateEnv updateState vote
  where
    updateEnv   = mkUpdateEnvironment cfg state
    updateState = CC.cvsUpdateState state

{-------------------------------------------------------------------------------
  Apply any kind of transactions
-------------------------------------------------------------------------------}

-- | Errors that arise from applying an arbitrary mempool payload
--
-- Although @cardano-legder@ defines 'MempoolPayload', it does not define a
-- corresponding error type. We could 'ChainValidationError', but it's too
-- large, which is problematic because we actually sent encoded versions of
-- these errors across the wire.
data ApplyMempoolPayloadErr =
    MempoolTxErr             Utxo.UTxOValidationError
  | MempoolDlgErr            D.Sched.Error
  | MempoolUpdateProposalErr U.Iface.Error
  | MempoolUpdateVoteErr     U.Iface.Error
  deriving (Eq, Show)

instance ToCBOR ApplyMempoolPayloadErr where
  toCBOR (MempoolTxErr err) =
    CBOR.encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR err
  toCBOR (MempoolDlgErr err) =
    CBOR.encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR err
  toCBOR (MempoolUpdateProposalErr err) =
    CBOR.encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR err
  toCBOR (MempoolUpdateVoteErr err) =
    CBOR.encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR err

instance FromCBOR ApplyMempoolPayloadErr where
  fromCBOR = do
    enforceSize "ApplyMempoolPayloadErr" 2
    CBOR.decodeWord8 >>= \case
      0   -> MempoolTxErr             <$> fromCBOR
      1   -> MempoolDlgErr            <$> fromCBOR
      2   -> MempoolUpdateProposalErr <$> fromCBOR
      3   -> MempoolUpdateVoteErr     <$> fromCBOR
      tag -> cborError $ DecoderErrorUnknownTag "ApplyMempoolPayloadErr" tag

applyMempoolPayload :: MonadError ApplyMempoolPayloadErr m
                    => CC.ValidationMode
                    -> Gen.Config
                    -> CC.MempoolPayload
                    -> CC.ChainValidationState -> m CC.ChainValidationState
applyMempoolPayload validationMode cfg payload =
    case payload of
      CC.MempoolTx tx ->
        (`wrapError` MempoolTxErr) .
          applyTxAux validationMode cfg [tx]
      CC.MempoolDlg cert ->
        (`wrapError` MempoolDlgErr) .
          applyCertificate cfg [cert]
      CC.MempoolUpdateProposal proposal ->
        (`wrapError` MempoolUpdateProposalErr) .
          applyUpdateProposal cfg proposal
      CC.MempoolUpdateVote vote ->
        (`wrapError` MempoolUpdateVoteErr) .
          applyUpdateVote cfg vote

-- | The encoding of the mempool payload (without a 'AMempoolPayload' envelope)
mempoolPayloadRecoverBytes :: CC.MempoolPayload -> ByteString
mempoolPayloadRecoverBytes = go
  where
    go :: CC.MempoolPayload -> ByteString
    go (CC.MempoolTx             payload) = serialize' payload
    go (CC.MempoolDlg            payload) = serialize' payload
    go (CC.MempoolUpdateProposal payload) = serialize' payload
    go (CC.MempoolUpdateVote     payload) = serialize' payload

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

reAnnotateMagicId :: ProtocolMagicId -> Annotated ProtocolMagicId ByteString
reAnnotateMagicId pmi = reAnnotate $ Annotated pmi ()

reAnnotateMagic :: ProtocolMagic -> AProtocolMagic ByteString
reAnnotateMagic (AProtocolMagic a b) = AProtocolMagic (reAnnotate a) b

-- | Generalization of 'reAnnotate'
reAnnotateUsing :: forall f a. Functor f
                => (f a -> Encoding)
                -> (forall s. Decoder s (f ByteSpan))
                -> f a -> f ByteString
reAnnotateUsing encoder decoder =
      (\bs -> splice bs $ CBOR.deserialiseFromBytes decoder bs)
    . CBOR.toLazyByteString
    . encoder
  where
    splice :: Show err
           => Lazy.ByteString
           -> Either err (Lazy.ByteString, f ByteSpan)
           -> f ByteString
    splice bs (Right (left, fSpan))
       | Lazy.null left = (Lazy.toStrict . slice bs) <$> fSpan
       | otherwise      = roundtripFailure "leftover bytes"
    splice _ (Left err) = roundtripFailure $ show err

    roundtripFailure :: forall x. String -> x
    roundtripFailure err = error $ intercalate ": " $ [
          "annotateBoundary"
        , "serialization roundtrip failure"
        , show err
        ]

{-------------------------------------------------------------------------------
  Header of a regular block or EBB

  The ledger layer defines 'BlockOrBoundary', but no equivalent for headers.
-------------------------------------------------------------------------------}

data BlockOrBoundaryHdr  =
    BOBBlockHdr    !CC.Header
  | BOBBoundaryHdr !CC.BoundaryHeader
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

fromCBORABlockOrBoundaryHdr :: CC.EpochSlots
                            -> Decoder s BlockOrBoundaryHdr
fromCBORABlockOrBoundaryHdr epochSlots = do
    enforceSize "ABlockOrBoundaryHdr" 2
    fromCBOR @Word >>= \case
      0 -> BOBBoundaryHdr <$> fromCBORAnnotated'
      1 -> BOBBlockHdr    <$> CC.fromCBORHeader epochSlots
      t -> error $ "Unknown tag in encoded HeaderOrBoundary" <> show t

-- | The analogue of 'Data.Either.either'
aBlockOrBoundaryHdr :: (CC.Header         -> b)
                    -> (CC.BoundaryHeader -> b)
                    -> BlockOrBoundaryHdr -> b
aBlockOrBoundaryHdr f _ (BOBBlockHdr    hdr) = f hdr
aBlockOrBoundaryHdr _ g (BOBBoundaryHdr hdr) = g hdr

abobHdrFromBlock :: CC.BlockOrBoundary -> BlockOrBoundaryHdr
abobHdrFromBlock (CC.BOBBlock    blk) = BOBBlockHdr    $ CC.blockHeader    blk
abobHdrFromBlock (CC.BOBBoundary blk) = BOBBoundaryHdr $ CC.boundaryHeader blk

-- | Slot number of the header
--
-- NOTE: Epoch slot number calculation must match the one in 'applyBoundary'.
abobHdrSlotNo :: CC.EpochSlots -> BlockOrBoundaryHdr -> CC.SlotNumber
abobHdrSlotNo epochSlots =
    aBlockOrBoundaryHdr
      CC.headerSlot
      (boundaryBlockSlot epochSlots . CC.boundaryEpoch)

abobHdrChainDifficulty :: BlockOrBoundaryHdr -> CC.ChainDifficulty
abobHdrChainDifficulty =
    aBlockOrBoundaryHdr
      CC.headerDifficulty
      CC.boundaryDifficulty

abobHdrHash :: BlockOrBoundaryHdr -> CC.HeaderHash
abobHdrHash (BOBBoundaryHdr hdr) = CC.boundaryHeaderHashAnnotated hdr
abobHdrHash (BOBBlockHdr    hdr) = CC.hashHeader         hdr

abobHdrPrevHash :: BlockOrBoundaryHdr -> Maybe CC.HeaderHash
abobHdrPrevHash =
    aBlockOrBoundaryHdr
      (Just                        . CC.headerPrevHash)
      (either (const Nothing) Just . CC.boundaryPrevHash)

-- | Check if a block matches its header
--
-- For EBBs, we're currently being more permissive here and not performing any
-- header-body validation but only checking whether an EBB header and EBB block
-- were provided. This seems to be fine as it won't cause any loss of consensus
-- with the old `cardano-sl` nodes.
abobMatchesBody :: BlockOrBoundaryHdr
                -> CC.BlockOrBoundary
                -> Bool
abobMatchesBody hdr blk =
    case (hdr, blk) of
      (BOBBlockHdr hdr', CC.BOBBlock blk') -> matchesBody hdr' blk'
      (BOBBoundaryHdr _, CC.BOBBoundary _) -> True
      (BOBBlockHdr    _, CC.BOBBoundary _) -> False
      (BOBBoundaryHdr _, CC.BOBBlock    _) -> False
  where
    matchesBody :: CC.Header -> CC.Block -> Bool
    matchesBody hdr' blk' = isRight $
        CC.validateHeaderMatchesBody hdr' (CC.blockBody blk')
