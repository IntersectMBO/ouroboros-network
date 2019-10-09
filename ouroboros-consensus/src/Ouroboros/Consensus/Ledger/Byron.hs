{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Byron
  ( -- * Byron blocks and headers
    ByronBlock (..)
  , ByronHash (..)
  , annotateByronBlock
  , ByronGiven
  , ConfigContainsGenesis(..)
  , mkByronHeader
    -- * Mempool integration
  , GenTx (..)
  , GenTxId (..)
  , mkByronGenTx
    -- * Block Fetch integration
  , byronBlockOrEBBMatchesHeader
    -- * Ledger
  , LedgerState (..)
  , LedgerConfig (..)
    -- * Serialisation
  , encodeByronHeader
  , encodeByronBlock
  , encodeByronHeaderHash
  , encodeByronGenTx
  , encodeByronGenTxId
  , encodeByronLedgerState
  , encodeByronChainState
  , encodeByronApplyTxError
  , decodeByronHeader
  , decodeByronBlock
  , decodeByronHeaderHash
  , decodeByronGenTx
  , decodeByronGenTxId
  , decodeByronLedgerState
  , decodeByronChainState
  , decodeByronApplyTxError
    -- * EBBs
  , ByronBlockOrEBB (..)
  , pattern ByronHeaderRegular
  , pattern ByronHeaderBoundary
  , mkByronHeaderOrEBB
  , annotateBoundary
  , toCBORAHeaderOrBoundary
  , fromCBORAHeaderOrBoundary
  ) where

import           Cardano.Prelude (Word8, cborError, wrapError)

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Monad.Except
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Bifunctor (bimap)
import qualified Data.Bimap as Bimap
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce (coerce)
import           Data.Either (isRight)
import           Data.FingerTree.Strict (Measured (..))
import           Data.Foldable (find, foldl')
import           Data.Reflection (Given (..))
import qualified Data.Sequence.Strict as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Typeable
import           Formatting
import           GHC.Generics (Generic)

import           Cardano.Binary (Annotated (..), ByteSpan, Decoded (..),
                     DecoderError (..), FromCBOR (..), ToCBOR (..),
                     enforceSize, fromCBOR, reAnnotate, serialize', slice,
                     toCBOR)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as CC.Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as V.Interface
import qualified Cardano.Chain.Delegation.Validation.Scheduling as V.Scheduling
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.MempoolPayload as CC.Mempool
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Update.Proposal as CC.Update.Proposal
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import qualified Cardano.Chain.Update.Vote as CC.Update.Vote
import qualified Cardano.Chain.UTxO as CC.UTxO
import           Cardano.Chain.ValidationMode (ValidationMode (..),
                     fromBlockValidationMode)
import qualified Cardano.Crypto as Crypto
import           Cardano.Crypto.DSIGN
import           Cardano.Crypto.Hash
import           Cardano.Prelude (NoUnexpectedThunks (..),
                     UseIsNormalFormNamed (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.Point (WithOrigin (..))
import qualified Ouroboros.Network.Point as Point (block, origin)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron.Orphans ()
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Protocol.WithEBBs
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.SlotBounded (SlotBounded (..))
import qualified Ouroboros.Consensus.Util.SlotBounded as SB

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ByronHash = ByronHash { unByronHash :: CC.Block.HeaderHash }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, FromCBOR)
  deriving anyclass NoUnexpectedThunks

instance Condense ByronHash where
  condense = formatToString CC.Block.headerHashF . unByronHash

{-------------------------------------------------------------------------------
  Byron blocks and headers
-------------------------------------------------------------------------------}

-- | Unfortunate Byron requires some information that is morally static but
-- actually comes from configuration files on startup. We use reflection to
-- provide this to type class instances.
type ByronGiven = (
    Given Crypto.ProtocolMagicId
  , Given CC.Slot.EpochSlots
  )

-- | Newtype wrapper to avoid orphan instances
--
-- The phantom type parameter is there to record the additional information
-- we need to work with this block. Most of the code here does not care,
-- but we may need different additional information when running the chain.
newtype ByronBlock cfg = ByronBlock
  { unByronBlock :: CC.Block.ABlock ByteString
  }
  deriving (Eq, Show)

mkByronHeader :: CC.Block.AHeader ByteString -> Header (ByronBlock cfg)
mkByronHeader header = ByronHeader {
    -- This satisfies the format 'ByronHeader' desires.
    byronHeader     = header
  , byronHeaderHash = ByronHash $ CC.Block.headerHashAnnotated header
  }

instance GetHeader (ByronBlock cfg) where
  -- | The only acceptable format for constructing a 'ByronHeader' is
  --
  -- > ByronHeader {
  -- >     byronHeader     = x
  -- >   , byronHeaderHash = computeHash x
  -- >   }
  --
  -- for some variable (not expression) @x@.
  data Header (ByronBlock cfg) = ByronHeader {
      byronHeader     :: !(CC.Block.AHeader ByteString)
    , byronHeaderHash :: ByronHash
      -- ^ Cache the hash, but compute it lazily based on 'byronHeader'.
      --
      -- Since the thunk to compute the hash (see 'mkByronHeader') only refers
      -- to 'byronHeader', this thunk should not secretly hold on to
      -- more data than 'byronHeader' itself holds on to.
    } deriving (Eq, Show)

  getHeader (ByronBlock b) = mkByronHeader (CC.Block.blockHeader b)

instance Typeable cfg => NoUnexpectedThunks (Header (ByronBlock cfg)) where
  showTypeOf _ = show $ typeRep (Proxy @(Header (ByronBlock cfg)))

  -- We explicitly allow the hash to be a thunk
  whnfNoUnexpectedThunks ctxt (ByronHeader hdr _hash) =
      noUnexpectedThunks ctxt hdr

instance (ByronGiven, Typeable cfg, NoUnexpectedThunks cfg)
    => SupportedBlock (ByronBlock cfg)

{-------------------------------------------------------------------------------
  HasHeader instances
-------------------------------------------------------------------------------}

type instance HeaderHash (ByronBlock  cfg) = ByronHash

instance (ByronGiven, Typeable cfg) => HasHeader (ByronBlock cfg) where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance (ByronGiven, Typeable cfg) => HasHeader (Header (ByronBlock cfg)) where
  blockHash = byronHeaderHash

  blockPrevHash = BlockHash . ByronHash . CC.Block.headerPrevHash . byronHeader

  blockSlot      = convertSlot
                 . CC.Block.headerSlot
                 . byronHeader
  blockNo        = BlockNo
                 . CC.Common.unChainDifficulty
                 . CC.Block.headerDifficulty
                 . byronHeader
  blockInvariant = const True

instance (ByronGiven, Typeable cfg) => Measured BlockMeasure (ByronBlock cfg) where
  measure = blockMeasure

instance StandardHash (ByronBlock cfg)

{-------------------------------------------------------------------------------
  Ledger
-------------------------------------------------------------------------------}

class ConfigContainsGenesis cfg where
  genesisConfig :: cfg -> CC.Genesis.Config

instance (ByronGiven, Typeable cfg, ConfigContainsGenesis cfg, NoUnexpectedThunks cfg)
     => UpdateLedger (ByronBlock cfg) where

  data LedgerState (ByronBlock cfg) = ByronLedgerState
      { blsCurrent :: !CC.Block.ChainValidationState
        -- | Slot-bounded snapshots of the chain state
      , blsSnapshots :: !(Seq.StrictSeq (SlotBounded (PBftLedgerView PBftCardanoCrypto)))
      }
    deriving (Eq, Show, Generic)

  type LedgerError (ByronBlock cfg) = CC.Block.ChainValidationError

  newtype LedgerConfig (ByronBlock cfg) = ByronLedgerConfig CC.Genesis.Config

  ledgerConfigView EncNodeConfig{..} = ByronLedgerConfig $
    genesisConfig encNodeConfigExt

  applyChainTick (ByronLedgerConfig cfg) slotNo
                 (ByronLedgerState state snapshots) = do
      let updateState' = CC.Block.epochTransition
            epochEnv
            (CC.Block.cvsUpdateState state)
            (coerce slotNo)
      let state' = state { CC.Block.cvsUpdateState = updateState' }
      return $ ByronLedgerState state' snapshots
    where
      epochEnv = CC.Block.EpochEnvironment
        { CC.Block.protocolMagic = fixPMI $ CC.Genesis.configProtocolMagicId cfg
        , CC.Block.k             = CC.Genesis.configK cfg
        , CC.Block.allowedDelegators = allowedDelegators cfg
        , CC.Block.delegationMap = delegationMap
        , CC.Block.currentEpoch  = CC.Slot.slotNumberEpoch
                                     (CC.Genesis.configEpochSlots cfg)
                                     (CC.Block.cvsLastSlot state)
        }
      delegationMap = V.Interface.delegationMap
                    $ CC.Block.cvsDelegationState state

      fixPMI pmi = reAnnotate $ Annotated pmi ()

  applyLedgerBlock = applyByronLedgerBlock
    (fromBlockValidationMode CC.Block.BlockValidation)

  reapplyLedgerBlock cfg blk st =
    let validationMode = fromBlockValidationMode CC.Block.NoBlockValidation
    -- Given a 'BlockValidationMode' of 'NoBlockValidation', a call to
    -- 'applyByronLedgerBlock' shouldn't fail since the ledger layer won't be
    -- performing any block validation checks.
    -- However, because 'applyByronLedgerBlock' can fail in the event it is
    -- given a 'BlockValidationMode' of 'BlockValidation', it still /looks/
    -- like it can fail (since its type doesn't change based on the
    -- 'ValidationMode') and we must still treat it as such.
    in case runExcept (applyByronLedgerBlock validationMode cfg blk st) of
      Left  err -> error ("reapplyLedgerBlock: unexpected error: " <> show err)
      Right st' -> st'

  ledgerTipPoint (ByronLedgerState state _) = case CC.Block.cvsPreviousHash state of
      -- In this case there are no blocks in the ledger state. The genesis
      -- block does not occupy a slot, so its point is Origin.
      Left _genHash -> Point Point.origin
      Right hdrHash -> Point (Point.block slot (ByronHash hdrHash))
        where
          slot = convertSlot (CC.Block.cvsLastSlot state)

instance ByronGiven => NoUnexpectedThunks (LedgerState (ByronBlock cfg))
  -- use generic instance

pbftLedgerView :: CC.Block.ChainValidationState
               -> PBftLedgerView PBftCardanoCrypto
pbftLedgerView = PBftLedgerView
               . CC.Delegation.unMap
               . V.Interface.delegationMap
               . CC.Block.cvsDelegationState

applyByronLedgerBlock :: ValidationMode
                      -> LedgerConfig (ByronBlock cfg)
                      -> ByronBlock cfg
                      -> LedgerState (ByronBlock cfg)
                      -> Except (LedgerError (ByronBlock cfg))
                                (LedgerState (ByronBlock cfg))
applyByronLedgerBlock validationMode
                      (ByronLedgerConfig cfg)
                      (ByronBlock block)
                      (ByronLedgerState state snapshots) = do
    runReaderT
      (CC.Block.headerIsValid
        (CC.Block.cvsUpdateState state)
        (CC.Block.blockHeader block)
      )
      validationMode
    CC.Block.BodyState { CC.Block.utxo, CC.Block.updateState
                        , CC.Block.delegationState }
      <- runReaderT
          (CC.Block.updateBody bodyEnv bodyState block)
          validationMode
    let state' = state
          { CC.Block.cvsLastSlot        = CC.Block.blockSlot block
          , CC.Block.cvsPreviousHash    = Right $ CC.Block.blockHashAnnotated block
          , CC.Block.cvsUtxo            = utxo
          , CC.Block.cvsUpdateState     = updateState
          , CC.Block.cvsDelegationState = delegationState
          }
        snapshots'
            | CC.Block.cvsDelegationState state' ==
                CC.Block.cvsDelegationState state
            = snapshots
            | otherwise
            = snapshots Seq.|>
              SB.bounded startOfSnapshot slot (pbftLedgerView state')
          where
            startOfSnapshot = case snapshots of
              _ Seq.:|> a -> sbUpper a
              Seq.Empty   -> SlotNo 0
            slot = convertSlot $ CC.Block.blockSlot block
    return $ ByronLedgerState state' (trimSnapshots snapshots')
  where
    bodyState = CC.Block.BodyState
      { CC.Block.utxo            = CC.Block.cvsUtxo state
      , CC.Block.updateState     = CC.Block.cvsUpdateState state
      , CC.Block.delegationState = CC.Block.cvsDelegationState state
      }
    bodyEnv = CC.Block.BodyEnvironment
      { CC.Block.protocolMagic      = fixPM $ CC.Genesis.configProtocolMagic cfg
      , CC.Block.utxoConfiguration  = CC.Genesis.configUTxOConfiguration cfg
      , CC.Block.k                  = CC.Genesis.configK cfg
      , CC.Block.allowedDelegators  = allowedDelegators cfg
      , CC.Block.protocolParameters = protocolParameters
      , CC.Block.currentEpoch       = CC.Slot.slotNumberEpoch
                                        (CC.Genesis.configEpochSlots cfg)
                                        (CC.Block.blockSlot block)
      }

    protocolParameters = CC.UPI.adoptedProtocolParameters . CC.Block.cvsUpdateState
                        $ state

    fixPM (Crypto.AProtocolMagic a b) = Crypto.AProtocolMagic (reAnnotate a) b

    k = CC.Genesis.configK cfg

    trimSnapshots = Seq.dropWhileL $ \ss ->
      sbUpper ss < convertSlot (CC.Block.blockSlot block) - 2 * coerce k

allowedDelegators :: CC.Genesis.Config -> Set CC.Common.KeyHash
allowedDelegators
  = CC.Genesis.unGenesisKeyHashes
  . CC.Genesis.configGenesisKeyHashes

{-------------------------------------------------------------------------------
  Support for PBFT consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol (ByronBlock cfg) =
  ExtNodeConfig cfg (PBft PBftCardanoCrypto)

instance ByronGiven => SignedHeader (Header (ByronBlock cfg)) where
  type Signed (Header (ByronBlock cfg)) = Annotated CC.Block.ToSign ByteString
  headerSigned = CC.Block.recoverSignedBytes given
               . byronHeader

instance (ByronGiven, Typeable cfg)
      => HeaderSupportsPBft PBftCardanoCrypto (Header (ByronBlock cfg)) where
  headerPBftFields _ (ByronHeader hdr _) = PBftFields {
        pbftIssuer    = VerKeyCardanoDSIGN
                      . CC.Delegation.delegateVK
                      . CC.Block.delegationCertificate
                      . CC.Block.headerSignature
                      $ hdr
      , pbftGenKey    = VerKeyCardanoDSIGN
                      . CC.Block.headerGenesisKey
                      $ hdr
      , pbftSignature = SignedDSIGN
                      . SigCardanoDSIGN
                      . CC.Block.signature
                      . CC.Block.headerSignature
                      $ hdr
      }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

convertSlot :: CC.Slot.SlotNumber -> SlotNo
convertSlot = coerce

{-------------------------------------------------------------------------------
  Condense instances
-------------------------------------------------------------------------------}

instance Condense (ByronBlock cfg) where
  condense blk =
      "(header: " <> condensedHeader <>
      ", body: "  <> condensedBody   <>
      ")"
    where
      condensedHeader = condense
                      . getHeader
                      $ blk
      condensedBody = T.unpack
                    . sformat build
                    . CC.UTxO.txpTxs
                    . CC.Block.bodyTxPayload
                    . CC.Block.blockBody
                    . unByronBlock
                    $ blk

instance Condense (Header (ByronBlock cfg)) where
  condense hdr =
      "(hash: "          <> condensedHash        <>
      ", previousHash: " <> condensedPrevHash    <>
      ", slot: "         <> condensedSlot        <>
      ", issuer: "       <> condenseKey issuer   <>
      ", delegate: "     <> condenseKey delegate <>
      ")"
    where
      psigCert = CC.Block.delegationCertificate
               . CC.Block.headerSignature
               . byronHeader
               $ hdr
      issuer   = CC.Delegation.issuerVK   psigCert
      delegate = CC.Delegation.delegateVK psigCert

      condenseKey :: Crypto.VerificationKey -> String
      condenseKey = T.unpack . sformat build

      condensedHash
        = T.unpack
        . sformat CC.Block.headerHashF
        . unByronHash
        . byronHeaderHash
        $ hdr

      condensedPrevHash
        = T.unpack
        . sformat CC.Block.headerHashF
        . CC.Block.headerPrevHash
        . byronHeader
        $ hdr

      condensedSlot
        = T.unpack
        . sformat build
        . unAnnotated
        . CC.Block.aHeaderSlot
        . byronHeader
        $ hdr

{-------------------------------------------------------------------------------
  Epoch Boundary Blocks
-------------------------------------------------------------------------------}

newtype ByronBlockOrEBB cfg = ByronBlockOrEBB
  { unByronBlockOrEBB :: CC.Block.ABlockOrBoundary ByteString
  } deriving (Eq, Show)

mkByronHeaderOrEBB :: Either (CC.Block.ABoundaryHeader ByteString)
                             (CC.Block.AHeader         ByteString)
                   -> Header (ByronBlockOrEBB cfg)
mkByronHeaderOrEBB header = case header of
    -- By pattern-matching on the two cases, we satisfy the format
    -- 'ByronHeaderOrEBB' desires.
    Left ebb -> ByronHeaderBoundary ebb h
      where
        h = ByronHash $ CC.Block.boundaryHeaderHashAnnotated ebb
    Right mb -> ByronHeaderRegular mb h
      where
        h = ByronHash $ CC.Block.headerHashAnnotated mb

instance GetHeader (ByronBlockOrEBB cfg) where
  -- | The only acceptable formats for constructing a 'ByronHeaderOrEBB' are
  --
  -- > ByronHeaderRegular  x (computeHash x)
  -- > ByronHeaderBoundary x (computeHash x)
  --
  -- for some variable (not expression) @x@.
  --
  -- This guarantees that the lazily evaluated hash won't retain anything more
  -- than is retained /anyway/ by the header itself.
  data Header (ByronBlockOrEBB cfg) =
        ByronHeaderRegular  !(CC.Block.AHeader         ByteString) ByronHash
      | ByronHeaderBoundary !(CC.Block.ABoundaryHeader ByteString) ByronHash
      deriving (Eq, Show)

  getHeader (ByronBlockOrEBB (CC.Block.ABOBBlock    b)) =
    mkByronHeaderOrEBB . Right $ CC.Block.blockHeader b

  getHeader (ByronBlockOrEBB (CC.Block.ABOBBoundary b)) =
    mkByronHeaderOrEBB . Left $ CC.Block.boundaryHeader b

type instance HeaderHash (ByronBlockOrEBB  cfg) = ByronHash

instance Typeable cfg => NoUnexpectedThunks (Header (ByronBlockOrEBB cfg)) where
  showTypeOf _ = show $ typeRep (Proxy @(Header (ByronBlockOrEBB cfg)))

  -- We explicitly allow the hash to be a thunk
  whnfNoUnexpectedThunks ctxt (ByronHeaderRegular mb _) =
      noUnexpectedThunks ctxt mb
  whnfNoUnexpectedThunks ctxt (ByronHeaderBoundary ebb _) =
      noUnexpectedThunks ctxt ebb

instance (ByronGiven, Typeable cfg, NoUnexpectedThunks cfg)
      => SupportedBlock (ByronBlockOrEBB cfg)

instance (ByronGiven, Typeable cfg) => HasHeader (ByronBlockOrEBB cfg) where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance (ByronGiven, Typeable cfg) => HasHeader (Header (ByronBlockOrEBB cfg)) where
  blockHash (ByronHeaderRegular  _ h) = h
  blockHash (ByronHeaderBoundary _ h) = h

  blockPrevHash (ByronHeaderRegular mb _) =
      BlockHash . ByronHash . CC.Block.headerPrevHash $ mb
  blockPrevHash (ByronHeaderBoundary ebb _) =
      case CC.Block.boundaryPrevHash ebb of
        Left _  -> GenesisHash
        Right h -> BlockHash (ByronHash h)

  blockSlot (ByronHeaderRegular mb _) =
      convertSlot . CC.Block.headerSlot $ mb
  blockSlot (ByronHeaderBoundary ebb _) =
      SlotNo $ CC.Slot.unEpochSlots given * CC.Block.boundaryEpoch ebb

  blockNo (ByronHeaderRegular mb _) =
        BlockNo
      . CC.Common.unChainDifficulty
      . CC.Block.headerDifficulty
      $ mb
  blockNo (ByronHeaderBoundary ebb _) =
        BlockNo
      . CC.Common.unChainDifficulty
      . CC.Block.boundaryDifficulty
      $ ebb

  blockInvariant = const True

instance (ByronGiven, Typeable cfg) => Measured BlockMeasure (ByronBlockOrEBB cfg) where
  measure = blockMeasure

instance StandardHash (ByronBlockOrEBB cfg)

instance (ByronGiven, Typeable cfg)
  => HeaderSupportsWithEBB (ExtNodeConfig cfg (PBft PBftCardanoCrypto)) (Header (ByronBlockOrEBB cfg)) where
  type HeaderOfBlock (Header (ByronBlockOrEBB cfg)) = Header (ByronBlock cfg)
  type HeaderOfEBB (Header (ByronBlockOrEBB cfg)) = CC.Block.ABoundaryHeader ByteString

  eitherHeaderOrEbb _ (ByronHeaderRegular  mb _)  = Right (mkByronHeader mb)
  eitherHeaderOrEbb _ (ByronHeaderBoundary ebb _) = Left ebb

type instance BlockProtocol (ByronBlockOrEBB cfg) =
  WithEBBs (ExtNodeConfig cfg (PBft PBftCardanoCrypto))

instance ( ByronGiven
         , Typeable cfg
         , ConfigContainsGenesis cfg
         , NoUnexpectedThunks cfg
         )
     => UpdateLedger (ByronBlockOrEBB cfg) where

  newtype LedgerState (ByronBlockOrEBB cfg) = ByronEBBLedgerState (LedgerState (ByronBlock cfg))
    deriving (Eq, Show)
  type LedgerError (ByronBlockOrEBB cfg) = LedgerError (ByronBlock cfg)

  newtype LedgerConfig (ByronBlockOrEBB cfg) = ByronEBBLedgerConfig (LedgerConfig (ByronBlock cfg))

  ledgerConfigView = ByronEBBLedgerConfig . ledgerConfigView . unWithEBBNodeConfig

  applyChainTick (ByronEBBLedgerConfig cfg) slotNo (ByronEBBLedgerState state) =
    ByronEBBLedgerState <$> applyChainTick cfg slotNo state

  applyLedgerBlock = applyByronLedgerBlockOrEBB
    (fromBlockValidationMode CC.Block.BlockValidation)

  reapplyLedgerBlock cfg blk st =
    let validationMode = fromBlockValidationMode CC.Block.NoBlockValidation
    -- Given a 'BlockValidationMode' of 'NoBlockValidation', a call to
    -- 'applyByronLedgerBlockOrEBB' shouldn't fail since the ledger layer
    -- won't be performing any block validation checks.
    -- However, because 'applyByronLedgerBlockOrEBB' can fail in the event it
    -- is given a 'BlockValidationMode' of 'BlockValidation', it still /looks/
    -- like it can fail (since its type doesn't change based on the
    -- 'ValidationMode') and we must still treat it as such.
    in case runExcept (applyByronLedgerBlockOrEBB validationMode cfg blk st) of
      Left  err -> error ("reapplyLedgerBlock: unexpected error: " <> show err)
      Right st' -> st'

  ledgerTipPoint (ByronEBBLedgerState state) = castPoint $ ledgerTipPoint state

deriving newtype instance UpdateLedger (ByronBlock cfg) => NoUnexpectedThunks (LedgerState (ByronBlockOrEBB cfg))

applyByronLedgerBlockOrEBB :: Given CC.Slot.EpochSlots
                           => ValidationMode
                           -> LedgerConfig (ByronBlockOrEBB cfg)
                           -> ByronBlockOrEBB cfg
                           -> LedgerState (ByronBlockOrEBB cfg)
                           -> Except (LedgerError (ByronBlockOrEBB cfg))
                                     (LedgerState (ByronBlockOrEBB cfg))
applyByronLedgerBlockOrEBB validationMode
                           (ByronEBBLedgerConfig cfg)
                           (ByronBlockOrEBB block)
                           (ByronEBBLedgerState bs@(ByronLedgerState state snapshots)) =
  case block of
    CC.Block.ABOBBlock b ->
      ByronEBBLedgerState <$> applyByronLedgerBlock validationMode cfg (ByronBlock b) bs
    CC.Block.ABOBBoundary b ->
      mapExcept (fmap (\i -> ByronEBBLedgerState $ ByronLedgerState i snapshots)) $
        return $ state
          { CC.Block.cvsPreviousHash = Right $ CC.Block.boundaryHeaderHashAnnotated hdr
          , CC.Block.cvsLastSlot = CC.Slot.SlotNumber $ epochSlots * CC.Block.boundaryEpoch hdr
          }
      where
        hdr = CC.Block.boundaryHeader b
        CC.Slot.EpochSlots epochSlots = given

-- | Construct Byron block from unannotated 'CC.Block.Block'
--
-- This should be used only when forging blocks (not when receiving blocks
-- over the wire).
annotateByronBlock :: CC.Slot.EpochSlots -> CC.Block.Block -> ByronBlockOrEBB cfg
annotateByronBlock epochSlots = ByronBlockOrEBB . CC.Block.ABOBBlock . annotateBlock epochSlots

{-------------------------------------------------------------------------------
  Condense instances
-------------------------------------------------------------------------------}

instance Condense (ByronBlockOrEBB cfg) where
  condense (ByronBlockOrEBB (CC.Block.ABOBBlock    blk)) = condense (ByronBlock blk)
  condense (ByronBlockOrEBB (CC.Block.ABOBBoundary ebb)) = condenseABoundaryBlock ebb

condenseABoundaryBlock :: CC.Block.ABoundaryBlock ByteString -> String
condenseABoundaryBlock CC.Block.ABoundaryBlock{boundaryHeader} =
  condenseABoundaryHeader boundaryHeader

condenseABoundaryHeader :: CC.Block.ABoundaryHeader ByteString -> String
condenseABoundaryHeader hdr =
    "( ebb: true" <>
    ", hash: " <> condensedHash <>
    ", previousHash: " <> condensedPrevHash <>
    ")"
  where
    condensedHash
      = T.unpack
      . sformat CC.Block.headerHashF
      . coerce
      . Crypto.hashDecoded . fmap CC.Block.wrapBoundaryBytes
      $ hdr

    condensedPrevHash
      = T.unpack $ case CC.Block.boundaryPrevHash hdr of
          Left _  -> "Genesis"
          Right h -> sformat CC.Block.headerHashF h

instance Condense (Header (ByronBlockOrEBB cfg)) where
  condense (ByronHeaderRegular  hdr    _) = condense (mkByronHeader hdr)
  condense (ByronHeaderBoundary ebbhdr _) = condenseABoundaryHeader ebbhdr

instance Condense (ChainHash (ByronBlockOrEBB cfg)) where
  condense GenesisHash   = "genesis"
  condense (BlockHash h) = condense h

instance Condense (GenTx (ByronBlockOrEBB cfg)) where
    condense (ByronTx _ tx) =
      "byrontx: " <> T.unpack (sformat build (void tx))
    condense (ByronDlg _ cert) =
      "byrondlg: " <> T.unpack (sformat build (void cert))
    condense (ByronUpdateProposal _ p) =
      "byronupdateproposal: " <> T.unpack (sformat build (void p))
    condense (ByronUpdateVote _ vote) =
      "byronupdatevote: " <> T.unpack (sformat build (void vote))

instance Show (GenTx (ByronBlockOrEBB cfg)) where
    show tx = condense tx

instance Condense (GenTxId (ByronBlockOrEBB cfg)) where
  condense (ByronTxId i)             = "byrontxid: " <> condense i
  condense (ByronDlgId i)            = "byrondlgid: " <> condense i
  condense (ByronUpdateProposalId i) = "byronupdateproposalid: " <> condense i
  condense (ByronUpdateVoteId i)     = "byronupdatevoteid: " <> condense i

instance Show (GenTxId (ByronBlockOrEBB cfg)) where
  show = condense

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | Encode a block. A legacy Byron node (cardano-sl) would successfully
-- decode a block from these.
encodeByronBlock :: ByronBlockOrEBB cfg -> Encoding
encodeByronBlock blk =
    CBOR.encodeListLen 2
     <> case unByronBlockOrEBB blk of
          CC.Block.ABOBBoundary b ->
              CBOR.encodeWord 0
           <> CBOR.encodePreEncoded (CC.Block.boundaryAnnotation b)

          CC.Block.ABOBBlock b ->
              CBOR.encodeWord 1
           <> CBOR.encodePreEncoded (CC.Block.blockAnnotation b)

-- | Inversion of 'encodeByronBlock'. The annotation will be correct, because
-- the full bytes are passed to the decoded value.
decodeByronBlock :: CC.Slot.EpochSlots
                 -> Decoder s (Lazy.ByteString -> ByronBlockOrEBB cfg)
decodeByronBlock epochSlots =
    fillInByteString <$> CC.Block.fromCBORABlockOrBoundary epochSlots
  where
    fillInByteString it theBytes = ByronBlockOrEBB $
      Lazy.toStrict . slice theBytes <$> it

-- | Encode a header. A legacy Byron node (cardano-sl) would successfully
-- decode a header from these.
encodeByronHeader :: Header (ByronBlockOrEBB cfg) -> Encoding
encodeByronHeader (ByronHeaderBoundary ebb _) = mconcat [
      CBOR.encodeListLen 2
    , CBOR.encodeWord 0
    , CBOR.encodePreEncoded (CC.Block.boundaryHeaderAnnotation ebb)
    ]
encodeByronHeader (ByronHeaderRegular mb _) = mconcat [
      CBOR.encodeListLen 2
    , CBOR.encodeWord 1
    , CBOR.encodePreEncoded (CC.Block.headerAnnotation mb)
    ]

-- | Inversion of 'encodeByronHeader'.  The annotation will be correct, because
-- the full bytes are passed to the decoded value.
decodeByronHeader :: CC.Slot.EpochSlots
                  -> Decoder s (Lazy.ByteString -> Header (ByronBlockOrEBB cfg))
decodeByronHeader epochSlots =
    fillInByteString <$> fromCBORAHeaderOrBoundary epochSlots
  where
    fillInByteString it theBytes = mkByronHeaderOrEBB $ bimap
      (fmap (Lazy.toStrict . slice theBytes))
      (fmap (Lazy.toStrict . slice theBytes))
      it

encodeByronHeaderHash :: HeaderHash (ByronBlockOrEBB cfg) -> Encoding
encodeByronHeaderHash = toCBOR

encodeByronLedgerState :: LedgerState (ByronBlockOrEBB cfg) -> Encoding
encodeByronLedgerState (ByronEBBLedgerState ByronLedgerState{..}) = mconcat
    [ CBOR.encodeListLen 2
    , encode blsCurrent
    , encode blsSnapshots
    ]

encodeByronChainState :: ChainState (BlockProtocol (ByronBlockOrEBB cfg)) -> Encoding
encodeByronChainState = encodeChainStateWithEBBs encode

decodeByronHeaderHash :: Decoder s (HeaderHash (ByronBlockOrEBB cfg))
decodeByronHeaderHash = fromCBOR

encodeByronGenTx :: GenTx (ByronBlockOrEBB cfg) -> Encoding
encodeByronGenTx genTx = toCBOR $ void (mkMempoolPayload genTx)

encodeByronGenTxId :: GenTxId (ByronBlockOrEBB cfg) -> Encoding
encodeByronGenTxId genTxId = case genTxId of
  ByronTxId i ->
    CBOR.encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR i
  ByronDlgId i ->
    CBOR.encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR i
  ByronUpdateProposalId i ->
    CBOR.encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR i
  ByronUpdateVoteId i ->
    CBOR.encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR i

encodeByronApplyTxError :: ApplyTxErr (ByronBlockOrEBB cfg) -> Encoding
encodeByronApplyTxError = toCBOR

decodeByronGenTx :: Decoder s (GenTx (ByronBlockOrEBB cfg))
decodeByronGenTx =
    mkByronGenTx . annotate <$> fromCBOR
  where
    -- | 'slice' a strict 'ByteString'.
    slice' :: ByteString -> ByteSpan -> ByteString
    slice' bs = Lazy.toStrict . slice (Lazy.fromStrict bs)

    annotate :: CC.Mempool.AMempoolPayload ByteSpan
             -> CC.Mempool.AMempoolPayload ByteString
    annotate mp =
      -- TODO: After input-output-hk/cardano-base#41 is complete, we should be
      -- able to get rid of this 'serialize'' call as the new
      -- @AnnotatedDecoder@ will allow us to reference the original
      -- 'ByteString' from which the 'AMempoolPayload' was deserialized.
      let bs = serialize' (void mp)
      in case mp of
        CC.Mempool.MempoolTx tx ->
          CC.Mempool.MempoolTx (slice' bs <$> tx)

        CC.Mempool.MempoolDlg cert ->
          CC.Mempool.MempoolDlg (slice' bs <$> cert)

        CC.Mempool.MempoolUpdateProposal proposal ->
          CC.Mempool.MempoolUpdateProposal (slice' bs <$> proposal)

        CC.Mempool.MempoolUpdateVote vote ->
          CC.Mempool.MempoolUpdateVote (slice' bs <$> vote)

decodeByronGenTxId :: Decoder s (GenTxId (ByronBlockOrEBB cfg))
decodeByronGenTxId = do
  enforceSize "GenTxId (ByronBlockOrEBB cfg)" 2
  CBOR.decodeWord8 >>= \case
    0   -> ByronTxId             <$> fromCBOR
    1   -> ByronDlgId            <$> fromCBOR
    2   -> ByronUpdateProposalId <$> fromCBOR
    3   -> ByronUpdateVoteId     <$> fromCBOR
    tag -> cborError $ DecoderErrorUnknownTag "GenTxId (ByronBlockOrEBB cfg)" tag

decodeByronLedgerState :: Decoder s (LedgerState (ByronBlockOrEBB cfg))
decodeByronLedgerState = ByronEBBLedgerState <$> do
    CBOR.decodeListLenOf 2
    ByronLedgerState <$> decode <*> decode

decodeByronChainState :: Decoder s (ChainState (BlockProtocol (ByronBlockOrEBB cfg)))
decodeByronChainState = decodeChainStateWithEBBs decode

decodeByronApplyTxError :: Decoder s (ApplyTxErr (ByronBlockOrEBB cfg))
decodeByronApplyTxError = fromCBOR

{-------------------------------------------------------------------------------
  Internal auxiliary

  TODO: This should live in an upstream repo instead.
-------------------------------------------------------------------------------}

annotateBlock :: CC.Slot.EpochSlots
              -> CC.Block.ABlock ()
              -> CC.Block.ABlock ByteString
annotateBlock epochSlots =
      (\bs -> splice bs (CBOR.deserialiseFromBytes
                           (CC.Block.fromCBORABlock epochSlots)
                           bs))
    . CBOR.toLazyByteString
    . CC.Block.toCBORBlock epochSlots
  where
    splice :: Lazy.ByteString
           -> Either err (Lazy.ByteString, CC.Block.ABlock ByteSpan)
           -> CC.Block.ABlock ByteString
    splice _ (Left _err) =
      error "annotateBlock: serialization roundtrip failure"
    splice bs (Right (_leftover, txAux)) =
      (Lazy.toStrict . slice bs) <$> txAux

{-------------------------------------------------------------------------------
  Internal auxiliary

  Since we will not be creating further boundary blocks, these utilities do not
  exist in the cardano-ledger repo, but we need them for the genesis case in the
  demo.
  -------------------------------------------------------------------------------}

annotateBoundary :: Crypto.ProtocolMagicId
                 -> CC.Block.ABoundaryBlock ()
                 -> CC.Block.ABoundaryBlock ByteString
annotateBoundary pm =
      (\bs -> splice bs (CBOR.deserialiseFromBytes
                           CC.Block.fromCBORABoundaryBlock
                           bs))
    . CBOR.toLazyByteString
    . CC.Block.toCBORABoundaryBlock pm
  where
    splice :: Show err
           => Lazy.ByteString
           -> Either err (Lazy.ByteString, CC.Block.ABoundaryBlock ByteSpan)
           -> CC.Block.ABoundaryBlock ByteString
    splice _ (Left err) =
      error $ "annotateBoundary: serialization roundtrip failure: " <> show err
    splice bs (Right (_leftover, boundary)) =
      (Lazy.toStrict . slice bs) <$> boundary

fromCBORAHeaderOrBoundary
  :: CC.Slot.EpochSlots
  -> Decoder s (Either (CC.Block.ABoundaryHeader ByteSpan) (CC.Block.AHeader ByteSpan))
fromCBORAHeaderOrBoundary epochSlots = do
  enforceSize "Block" 2
  fromCBOR @Word >>= \case
    0 -> Left <$> CC.Block.fromCBORABoundaryHeader
    1 -> Right <$> CC.Block.fromCBORAHeader epochSlots
    t -> error $ "Unknown tag in encoded HeaderOrBoundary" <> show t

toCBORAHeaderOrBoundary
  :: Crypto.ProtocolMagicId
  -> CC.Slot.EpochSlots
  -> (Either (CC.Block.ABoundaryHeader a) (CC.Block.AHeader a))
  -> Encoding
toCBORAHeaderOrBoundary pm epochSlots abob =
  CBOR.encodeListLen 2 <>
  case abob of
    Right mh -> toCBOR (1 :: Word) <> (CC.Block.toCBORHeader epochSlots  . void $ mh)
    Left ebb -> toCBOR (0 :: Word) <> (CC.Block.toCBORABoundaryHeader pm . void $ ebb)

{-------------------------------------------------------------------------------
  Mempool integration
-------------------------------------------------------------------------------}

-- | An error type which represents either a UTxO, delegation, update proposal
-- registration, or update vote error in the Byron era.
data ByronApplyTxError
  = ByronApplyTxError !CC.UTxO.UTxOValidationError
  | ByronApplyDlgError !V.Scheduling.Error
  | ByronApplyUpdateProposalError !CC.UPI.Error
  | ByronApplyUpdateVoteError !CC.UPI.Error
  deriving (Show)

instance ToCBOR ByronApplyTxError where
  toCBOR (ByronApplyTxError err) =
    CBOR.encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR err
  toCBOR (ByronApplyDlgError err) =
    CBOR.encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR err
  toCBOR (ByronApplyUpdateProposalError err) =
    CBOR.encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR err
  toCBOR (ByronApplyUpdateVoteError err) =
    CBOR.encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR err

instance FromCBOR ByronApplyTxError where
  fromCBOR = do
    enforceSize "ByronApplyTxError" 2
    CBOR.decodeWord8 >>= \case
      0   -> ByronApplyTxError             <$> fromCBOR
      1   -> ByronApplyDlgError            <$> fromCBOR
      2   -> ByronApplyUpdateProposalError <$> fromCBOR
      3   -> ByronApplyUpdateVoteError     <$> fromCBOR
      tag -> cborError $ DecoderErrorUnknownTag "ByronApplyTxError" tag

instance (ByronGiven, Typeable cfg, ConfigContainsGenesis cfg, NoUnexpectedThunks cfg)
      => ApplyTx (ByronBlockOrEBB cfg) where
  -- | Generalized transactions in Byron
  --
  data GenTx (ByronBlockOrEBB cfg)
    = ByronTx
        CC.UTxO.TxId
        -- ^ This field is lazy on purpose so that the 'CC.UTxO.TxId' is
        -- computed on demand.
        !(CC.UTxO.ATxAux ByteString)
    | ByronDlg
        CC.Delegation.CertificateId
        -- ^ This field is lazy on purpose so that the
        -- 'CC.Delegation.CertificateId' is computed on demand.
        !(CC.Delegation.ACertificate ByteString)
    | ByronUpdateProposal
        CC.Update.Proposal.UpId
        -- ^ This field is lazy on purpose so that the 'CC.Update.UpId' is
        -- computed on demand.
        !(CC.Update.Proposal.AProposal ByteString)
    | ByronUpdateVote
        CC.Update.Vote.VoteId
        -- ^ This field is lazy on purpose so that the 'CC.Update.VoteId' is
        -- computed on demand.
        !(CC.Update.Vote.AVote ByteString)

  data GenTxId (ByronBlockOrEBB cfg)
    = ByronTxId !CC.UTxO.TxId
    | ByronDlgId !CC.Delegation.CertificateId
    | ByronUpdateProposalId !CC.Update.Proposal.UpId
    | ByronUpdateVoteId !CC.Update.Vote.VoteId
    deriving (Eq, Ord)

  txId (ByronTx txid _)             = ByronTxId txid
  txId (ByronDlg certHash _)        = ByronDlgId certHash
  txId (ByronUpdateProposal upid _) = ByronUpdateProposalId upid
  txId (ByronUpdateVote voteHash _) = ByronUpdateVoteId voteHash

  txSize (ByronTx _ atxaux) = fromIntegral txByteSize
    where
      -- TODO cardano-ledger#576 will provide a function for this
      txByteSize = 1 -- To account for @encodeListLen 2@
                 + (Strict.length . annotation . CC.UTxO.aTaTx      $ atxaux)
                 + (Strict.length . annotation . CC.UTxO.aTaWitness $ atxaux)

  txSize (ByronDlg _ cert) = fromIntegral $
    Strict.length (recoverBytes cert)

  txSize (ByronUpdateProposal _ prop) = fromIntegral $
    Strict.length (recoverBytes prop)

  txSize (ByronUpdateVote _ vote) = fromIntegral $
    Strict.length (recoverBytes vote)

  type ApplyTxErr (ByronBlockOrEBB cfg) = ByronApplyTxError

  applyTx = applyByronGenTx
    (ValidationMode CC.Block.BlockValidation CC.UTxO.TxValidation)

  reapplyTx = applyByronGenTx
    (ValidationMode CC.Block.NoBlockValidation CC.UTxO.TxValidationNoCrypto)

  reapplyTxSameState cfg tx st =
    let validationMode = ValidationMode CC.Block.NoBlockValidation CC.UTxO.NoTxValidation
    in case runExcept (applyByronGenTx validationMode cfg tx st) of
      Left  err -> error $ "unexpected error: " <> show err
      Right st' -> st'

-- | We intentionally ignore the hash
instance Typeable cfg => NoUnexpectedThunks (GenTx (ByronBlockOrEBB cfg)) where
  showTypeOf _ = show (typeRep (Proxy @(GenTx (ByronBlockOrEBB cfg))))
  whnfNoUnexpectedThunks ctxt gtx = case gtx of
      ByronTx _hash tx ->
        noUnexpectedThunks ctxt (UseIsNormalFormNamed @"AVote" tx)
      ByronDlg _hash cert ->
        noUnexpectedThunks ctxt (UseIsNormalFormNamed @"ACertificate" cert)
      ByronUpdateProposal _hash prop ->
        noUnexpectedThunks ctxt (UseIsNormalFormNamed @"AProposal" prop)
      ByronUpdateVote _hash vote ->
        noUnexpectedThunks ctxt (UseIsNormalFormNamed @"AVote" vote)

applyByronGenTx :: ValidationMode
                -> LedgerConfig (ByronBlockOrEBB cfg)
                -> GenTx (ByronBlockOrEBB cfg)
                -> LedgerState (ByronBlockOrEBB cfg)
                -> Except (ApplyTxErr (ByronBlockOrEBB cfg))
                          (LedgerState (ByronBlockOrEBB cfg))
applyByronGenTx validationMode
                (ByronEBBLedgerConfig (ByronLedgerConfig cfg))
                genTx
                (ByronEBBLedgerState st@ByronLedgerState{blsCurrent}) =
    (\x -> ByronEBBLedgerState $ st { blsCurrent = x })
      <$> go genTx blsCurrent
  where
    go :: (MonadError ByronApplyTxError m)
       => GenTx (ByronBlockOrEBB cfg)
       -> CC.Block.ChainValidationState
       -> m CC.Block.ChainValidationState
    go gtx cvs = case gtx of
        ByronTx             _ tx       -> applyByronTx tx
        ByronDlg            _ cert     -> applyByronDlg cert
        ByronUpdateProposal _ proposal -> applyByronUpdateProposal proposal
        ByronUpdateVote     _ vote     -> applyByronUpdateVote vote
      where
        protocolMagic = fixPM (CC.Genesis.configProtocolMagic cfg)

        k = CC.Genesis.configK cfg

        currentEpoch = CC.Slot.slotNumberEpoch
          (CC.Genesis.configEpochSlots cfg)
          currentSlot

        currentSlot = CC.Block.cvsLastSlot cvs

        utxo = CC.Block.cvsUtxo cvs

        dlgState = CC.Block.cvsDelegationState cvs

        updateState = CC.Block.cvsUpdateState cvs

        delegationMap =
          (V.Interface.delegationMap . CC.Block.cvsDelegationState) cvs

        utxoEnv = CC.UTxO.Environment
          { CC.UTxO.protocolMagic      = protocolMagic
          , CC.UTxO.protocolParameters = CC.UPI.adoptedProtocolParameters updateState
          , CC.UTxO.utxoConfiguration  = CC.Genesis.configUTxOConfiguration cfg
          }

        dlgEnv = V.Interface.Environment
          { V.Interface.protocolMagic     = Crypto.getAProtocolMagicId protocolMagic
          , V.Interface.allowedDelegators = allowedDelegators cfg
          , V.Interface.k                 = k
          , V.Interface.currentEpoch      = currentEpoch
          , V.Interface.currentSlot       = currentSlot
          }

        updateEnv = CC.UPI.Environment
          { CC.UPI.protocolMagic = Crypto.getAProtocolMagicId protocolMagic
          , CC.UPI.k             = k
          , CC.UPI.currentSlot   = currentSlot
          , CC.UPI.numGenKeys    = numGenKeys
          , CC.UPI.delegationMap = delegationMap
          }

        numGenKeys = toNumGenKeys $ Set.size (allowedDelegators cfg)

        toNumGenKeys :: Integral n => n -> Word8
        toNumGenKeys n
          | n > fromIntegral (maxBound :: Word8) = error $
            "toNumGenKeys: Too many genesis keys"
          | otherwise = fromIntegral n

        fixPM (Crypto.AProtocolMagic a b) =
          Crypto.AProtocolMagic (reAnnotate a) b

        wrapUTxO newUTxO = cvs { CC.Block.cvsUtxo = newUTxO }

        wrapDlg newDlg = cvs { CC.Block.cvsDelegationState = newDlg }

        wrapUpdate newUpdate = cvs { CC.Block.cvsUpdateState = newUpdate }

        applyByronTx tx = wrapUTxO <$>
            runReaderT (CC.UTxO.updateUTxO utxoEnv utxo [tx]) validationMode
              `wrapError` ByronApplyTxError

        applyByronDlg cert = wrapDlg <$>
            V.Interface.updateDelegation dlgEnv dlgState [cert]
              `wrapError` ByronApplyDlgError

        applyByronUpdateProposal proposal = wrapUpdate <$>
            CC.UPI.registerProposal updateEnv updateState proposal
              `wrapError` ByronApplyUpdateProposalError

        applyByronUpdateVote vote = wrapUpdate <$>
              CC.UPI.registerVote updateEnv updateState vote
                `wrapError` ByronApplyUpdateVoteError

mkByronGenTx :: CC.Mempool.AMempoolPayload ByteString
             -> GenTx (ByronBlockOrEBB cfg)
mkByronGenTx mp = case mp of
    CC.Mempool.MempoolTx tx ->
      ByronTx (Crypto.hash $ CC.UTxO.taTx tx)  -- TODO replace this with a
                                               -- function from cardano-ledger,
                                               -- see cardano-ledger#581
              tx

    CC.Mempool.MempoolDlg cert ->
      ByronDlg (Crypto.hash $ void cert) cert

    CC.Mempool.MempoolUpdateProposal proposal ->
      ByronUpdateProposal (CC.Update.Proposal.recoverUpId proposal) proposal

    CC.Mempool.MempoolUpdateVote vote ->
      ByronUpdateVote (Crypto.hash $ void vote) vote

mkMempoolPayload :: GenTx (ByronBlockOrEBB cfg)
                 -> CC.Mempool.AMempoolPayload ByteString
mkMempoolPayload genTx = case genTx of
  ByronTx             _ tx       -> CC.Mempool.MempoolTx tx
  ByronDlg            _ cert     -> CC.Mempool.MempoolDlg cert
  ByronUpdateProposal _ proposal -> CC.Mempool.MempoolUpdateProposal proposal
  ByronUpdateVote     _ vote     -> CC.Mempool.MempoolUpdateVote vote

{-------------------------------------------------------------------------------
  Block Fetch integration
-------------------------------------------------------------------------------}

-- | Check if a block matches its header
byronBlockOrEBBMatchesHeader :: Header (ByronBlockOrEBB cfg)
                             -> ByronBlockOrEBB cfg
                             -> Bool
byronBlockOrEBBMatchesHeader blkOrEbbHdr (ByronBlockOrEBB blkOrEbb) =
    case (blkOrEbbHdr, blkOrEbb) of
      (ByronHeaderRegular hdr _, CC.Block.ABOBBlock blk) -> isRight $
        CC.Block.validateHeaderMatchesBody hdr (CC.Block.blockBody blk)
      (ByronHeaderBoundary _ebbHdr _, CC.Block.ABOBBoundary _) ->
        -- For EBBs, we're currently being more permissive here and not
        -- performing any header-body validation but only checking whether an
        -- EBB header and EBB block were provided. This seems to be fine as it
        -- won't cause any loss of consensus with the old `cardano-sl` nodes.
        True
      (ByronHeaderRegular{}  , CC.Block.ABOBBoundary{}) -> False
      (ByronHeaderBoundary{} , CC.Block.ABOBBlock{})    -> False

{-------------------------------------------------------------------------------
  PBFT integration
-------------------------------------------------------------------------------}

instance (ByronGiven, Typeable cfg, ConfigContainsGenesis cfg, NoUnexpectedThunks cfg)
      => ProtocolLedgerView (ByronBlockOrEBB cfg) where
  protocolLedgerView _ns (ByronEBBLedgerState (ByronLedgerState ls _)) =
    pbftLedgerView ls

  -- There are two cases here:
  --
  -- - The view we want is in the past. In this case, we attempt to find a
  --   snapshot which contains the relevant slot, and extract the delegation
  --   map from that.
  --
  -- - The view we want is in the future. In this case, we need to check the
  --   upcoming delegations to see what new delegations will be made in the
  --   future, and update the current delegation map based on that.
  anachronisticProtocolLedgerView
    (WithEBBNodeConfig cfg)
    (ByronEBBLedgerState (ByronLedgerState ls ss)) slot =
      case find (containsSlot slot) ss of
        -- We can find a snapshot which supports this slot
        Just sb -> Right sb
        -- No snapshot - we could be in the past or in the future
        Nothing
          | slot < At lvLB -> Left TooFarBehind
          | slot > At lvUB -> Left TooFarAhead
          | otherwise
          -> Right $ PBftLedgerView <$>
             case intermediateUpdates of
                -- No updates to apply. So the current ledger state is valid
                -- from the end of the last snapshot to the first scheduled
                -- update.
               Seq.Empty              -> SB.bounded lb ub dsNow
                -- Updates to apply. So we must apply them, and then the ledger
                -- state is valid from the end of the last update until the next
                -- scheduled update in the future.
               toApply@(_ Seq.:|> la) ->
                 SB.bounded (convertSlot . V.Scheduling.sdSlot $ la) ub $
                 foldl'
                   (\acc x -> Bimap.insert (V.Scheduling.sdDelegator x)
                                           (V.Scheduling.sdDelegate x)
                                           acc)
                   dsNow toApply
    where
      lb = case ss of
        _ Seq.:|> s -> max lvLB (sbUpper s)
        Seq.Empty   -> lvLB
      ub = case futureUpdates of
        s Seq.:<| _ -> min lvUB (convertSlot $ V.Scheduling.sdSlot s)
        Seq.Empty   -> lvUB

      (intermediateUpdates, futureUpdates) = Seq.spanl
                    (\sd -> At (convertSlot (V.Scheduling.sdSlot sd)) <= slot)
                    dsScheduled

      SecurityParam paramK = pbftSecurityParam . pbftParams . encNodeConfigP $ cfg

      lvUB = SlotNo $ unSlotNo currentSlot + (2 * paramK)
      lvLB
        | 2 * paramK > unSlotNo currentSlot
        = SlotNo 0
        | otherwise
        = SlotNo $ unSlotNo currentSlot - (2 * paramK)

      dsNow = pbftDelegates $ pbftLedgerView ls
      dsScheduled = Seq.toStrict
                  . V.Scheduling.scheduledDelegations
                  . V.Interface.schedulingState
                  . CC.Block.cvsDelegationState
                  $ ls
      currentSlot = convertSlot $ CC.Block.cvsLastSlot ls
      containsSlot s sb = At (sbLower sb) <= s && At (sbUpper sb) >= s
