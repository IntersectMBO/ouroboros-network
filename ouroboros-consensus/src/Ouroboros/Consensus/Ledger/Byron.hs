{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTSyntax            #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wredundant-constraints -Wno-orphans #-}

module Ouroboros.Consensus.Ledger.Byron
  ( -- * Byron blocks and headers
    ByronBlock (..)
  , annotateByronBlock
  , ByronGiven
  , ConfigContainsGenesis(..)
    -- * Mempool integration
  , GenTx (..)
  , GenTxId (..)
  , mkByronTx
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
  , blockBytes
  , headerBytes
    -- * EBBs
  , ByronBlockOrEBB (..)
  , pattern ByronHeaderOrEBB
  , unByronHeaderOrEBB
  , annotateBoundary
  , toCBORAHeaderOrBoundary
  , fromCBORAHeaderOrBoundary
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (Serialise, decode, encode)
import           Control.Monad.Except
import           Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Bimap as Bimap
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce (coerce)
import           Data.FingerTree (Measured (..))
import           Data.Foldable (find, foldl')
import           Data.Reflection (Given (..))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Typeable
import           Formatting

import           Cardano.Binary (Annotated (..), ByteSpan,
                     decodeFullAnnotatedBytes, decodeNestedCborBytes,
                     encodeNestedCborBytes, enforceSize, fromCBOR, reAnnotate,
                     serializeEncoding, slice, toCBOR)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as CC.Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as V.Interface
import qualified Cardano.Chain.Delegation.Validation.Scheduling as V.Scheduling
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import qualified Cardano.Chain.UTxO as CC.UTxO
import           Cardano.Chain.ValidationMode (fromBlockValidationMode)
import qualified Cardano.Crypto as Crypto
import           Cardano.Crypto.DSIGN
import           Cardano.Crypto.Hash

import           Ouroboros.Network.Block
import           Ouroboros.Network.Point (WithOrigin (..))
import qualified Ouroboros.Network.Point as Point (block, origin)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Ledger.Abstract
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

instance GetHeader (ByronBlock cfg) where
  newtype Header (ByronBlock cfg) = ByronHeader {
      unByronHeader :: CC.Block.AHeader ByteString
    } deriving (Eq, Show)

  getHeader (ByronBlock b) = ByronHeader (CC.Block.blockHeader b)

instance (ByronGiven, Typeable cfg) => SupportedBlock (ByronBlock cfg)

{-------------------------------------------------------------------------------
  HasHeader instances
-------------------------------------------------------------------------------}

type instance HeaderHash (ByronBlock  cfg) = CC.Block.HeaderHash

instance (ByronGiven, Typeable cfg) => HasHeader (ByronBlock cfg) where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance (ByronGiven, Typeable cfg) => HasHeader (Header (ByronBlock cfg)) where
  blockHash = CC.Block.headerHashAnnotated . unByronHeader

  blockPrevHash (ByronHeader h) = BlockHash $ CC.Block.headerPrevHash h

  blockSlot      = convertSlot
                 . CC.Block.headerSlot
                 . unByronHeader
  blockNo        = BlockNo
                 . CC.Common.unChainDifficulty
                 . CC.Block.headerDifficulty
                 . unByronHeader
  blockInvariant = const True

instance (ByronGiven, Typeable cfg) => Measured BlockMeasure (ByronBlock cfg) where
  measure = blockMeasure

instance StandardHash (ByronBlock cfg)

{-------------------------------------------------------------------------------
  Ledger
-------------------------------------------------------------------------------}

class ConfigContainsGenesis cfg where
  genesisConfig :: cfg -> CC.Genesis.Config

instance (ByronGiven, Typeable cfg, ConfigContainsGenesis cfg)
     => UpdateLedger (ByronBlock cfg) where

  data LedgerState (ByronBlock cfg) = ByronLedgerState
      { blsCurrent :: !CC.Block.ChainValidationState
        -- | Slot-bounded snapshots of the chain state
      , blsSnapshots :: !(Seq.Seq (SlotBounded CC.Block.ChainValidationState))
      }
    deriving (Eq, Show)

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

  applyLedgerBlock (ByronLedgerConfig cfg) (ByronBlock block)
                   (ByronLedgerState state snapshots) = do
      runReaderT
        (CC.Block.headerIsValid
          (CC.Block.cvsUpdateState state)
          (CC.Block.blockHeader block)
        )
        (fromBlockValidationMode CC.Block.BlockValidation)
      CC.Block.BodyState { CC.Block.utxo, CC.Block.updateState
                         , CC.Block.delegationState }
        <- runReaderT
            (CC.Block.updateBody bodyEnv bodyState block)
            (fromBlockValidationMode CC.Block.BlockValidation)
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
              = snapshots Seq.|> SB.bounded startOfSnapshot slot state'
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

  ledgerTipPoint (ByronLedgerState state _) = case CC.Block.cvsPreviousHash state of
      -- In this case there are no blocks in the ledger state. The genesis
      -- block does not occupy a slot, so its point is Origin.
      Left _genHash -> Point Point.origin
      Right hdrHash -> Point (Point.block slot hdrHash)
        where
          slot = convertSlot (CC.Block.cvsLastSlot state)

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
               . unByronHeader

instance (ByronGiven, Typeable cfg)
      => HeaderSupportsPBft PBftCardanoCrypto (Header (ByronBlock cfg)) where
  headerPBftFields _ (ByronHeader hdr) = PBftFields {
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
               . unByronHeader
               $ hdr
      issuer   = CC.Delegation.issuerVK   psigCert
      delegate = CC.Delegation.delegateVK psigCert

      condenseKey :: Crypto.VerificationKey -> String
      condenseKey = T.unpack . sformat build

      condensedHash
        = T.unpack
        . sformat CC.Block.headerHashF
        . Crypto.hashDecoded . fmap CC.Block.wrapHeaderBytes
        . unByronHeader
        $ hdr

      condensedPrevHash
        = T.unpack
        . sformat CC.Block.headerHashF
        . CC.Block.headerPrevHash
        . unByronHeader
        $ hdr

      condensedSlot
        = T.unpack
        . sformat build
        . unAnnotated
        . CC.Block.aHeaderSlot
        . unByronHeader
        $ hdr

{-------------------------------------------------------------------------------
  Epoch Boundary Blocks
-------------------------------------------------------------------------------}

newtype ByronBlockOrEBB cfg = ByronBlockOrEBB
  { unByronBlockOrEBB :: CC.Block.ABlockOrBoundary ByteString
  } deriving (Eq, Show)

instance GetHeader (ByronBlockOrEBB cfg) where
  newtype Header (ByronBlockOrEBB cfg) = ByronHeaderOrEBB {
      unByronHeaderOrEBB :: Either (CC.Block.ABoundaryHeader ByteString) (CC.Block.AHeader ByteString)
    } deriving (Eq, Show)

  getHeader (ByronBlockOrEBB (CC.Block.ABOBBlock    b)) =
    ByronHeaderOrEBB . Right $ CC.Block.blockHeader b

  getHeader (ByronBlockOrEBB (CC.Block.ABOBBoundary b)) =
    ByronHeaderOrEBB . Left $ CC.Block.boundaryHeader b

type instance HeaderHash (ByronBlockOrEBB  cfg) = CC.Block.HeaderHash

instance (ByronGiven, Typeable cfg) => SupportedBlock (ByronBlockOrEBB cfg)

instance (ByronGiven, Typeable cfg) => HasHeader (ByronBlockOrEBB cfg) where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance (ByronGiven, Typeable cfg) => HasHeader (Header (ByronBlockOrEBB cfg)) where
  blockHash b = case unByronHeaderOrEBB b of
    Left ebb -> CC.Block.boundaryHeaderHashAnnotated ebb
    Right mb -> CC.Block.headerHashAnnotated mb

  blockPrevHash b = case unByronHeaderOrEBB b of
    Right mb -> BlockHash . CC.Block.headerPrevHash $ mb
    Left ebb -> case CC.Block.boundaryPrevHash ebb of
      Left _  -> GenesisHash
      Right h -> BlockHash h

  blockSlot b = case unByronHeaderOrEBB b of
    Right mb -> convertSlot . CC.Block.headerSlot $ mb
    Left ebb -> SlotNo $ CC.Slot.unEpochSlots given * CC.Block.boundaryEpoch ebb

  blockNo b = case unByronHeaderOrEBB b of
    Right mb -> BlockNo
      . CC.Common.unChainDifficulty
      . CC.Block.headerDifficulty
      $ mb
    Left ebb -> BlockNo
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

  eitherHeaderOrEbb _ = fmap ByronHeader . unByronHeaderOrEBB

type instance BlockProtocol (ByronBlockOrEBB cfg) =
  WithEBBs (ExtNodeConfig cfg (PBft PBftCardanoCrypto))

instance ( ByronGiven
         , Typeable cfg
         , ConfigContainsGenesis cfg
         )
     => UpdateLedger (ByronBlockOrEBB cfg) where

  newtype LedgerState (ByronBlockOrEBB cfg) = ByronEBBLedgerState (LedgerState (ByronBlock cfg))
    deriving (Eq, Show)
  type LedgerError (ByronBlockOrEBB cfg) = LedgerError (ByronBlock cfg)

  newtype LedgerConfig (ByronBlockOrEBB cfg) = ByronEBBLedgerConfig (LedgerConfig (ByronBlock cfg))

  ledgerConfigView = ByronEBBLedgerConfig . ledgerConfigView . unWithEBBNodeConfig

  applyChainTick (ByronEBBLedgerConfig cfg) slotNo (ByronEBBLedgerState state) =
    ByronEBBLedgerState <$> applyChainTick cfg slotNo state

  applyLedgerBlock (ByronEBBLedgerConfig cfg) (ByronBlockOrEBB block)
                   (ByronEBBLedgerState bs@(ByronLedgerState state snapshots)) =
    case block of
      CC.Block.ABOBBlock b -> ByronEBBLedgerState <$> applyLedgerBlock cfg (ByronBlock b) bs
      CC.Block.ABOBBoundary b ->
        mapExcept (fmap (\i -> ByronEBBLedgerState $ ByronLedgerState i snapshots)) $
          return $ state
            { CC.Block.cvsPreviousHash = Right $ CC.Block.boundaryHeaderHashAnnotated hdr
            , CC.Block.cvsLastSlot = CC.Slot.SlotNumber $ epochSlots * CC.Block.boundaryEpoch hdr
            }
        where
          hdr = CC.Block.boundaryHeader b
          CC.Slot.EpochSlots epochSlots = given

  ledgerTipPoint (ByronEBBLedgerState state) = castPoint $ ledgerTipPoint state

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
  condense (ByronHeaderOrEBB (Right   hdr)) = condense (ByronHeader hdr)
  condense (ByronHeaderOrEBB (Left ebbhdr)) = condenseABoundaryHeader ebbhdr

instance Condense CC.Block.HeaderHash where
  condense = formatToString CC.Block.headerHashF

instance Condense (ChainHash (ByronBlockOrEBB cfg)) where
  condense GenesisHash   = "genesis"
  condense (BlockHash h) = condense h

instance Condense (GenTx (ByronBlockOrEBB cfg)) where
    condense (ByronTx { byronTx = tx }) =
      "(tx: "       <> show (CC.UTxO.aTaTx tx) <>
      ", witness: " <> show (CC.UTxO.aTaWitness tx) <>
      ")"

instance Show (GenTx (ByronBlockOrEBB cfg)) where
    show tx = condense tx

instance Condense (GenTxId (ByronBlockOrEBB cfg)) where
  condense = condense . unByronTxId

instance Show (GenTxId (ByronBlockOrEBB cfg)) where
  show = condense

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise CC.Block.ChainValidationState where
  encode = toCBOR
  decode = fromCBOR

instance Serialise CC.Common.KeyHash where
  encode = toCBOR
  decode = fromCBOR

-- Codec for blocks and headers using CBOR-in-CBOR. This style is needed
-- because the cardano-ledger decoders drop information that must be
-- retained if we are to communicate with Byron peers.
-- Re-annotating in the cardano-ledger style, i.e. re-encoding then using those
-- bytes to annotate, does not work, because information is lost in the
-- initial decoding.
-- This codec must be used for network exchange _and_ for the database.

-- | Get the encoded bytes of a block. A legacy Byron node (cardano-sl) would
-- successfully decode a block from these.
blockBytes :: ByronBlockOrEBB cfg -> Lazy.ByteString
blockBytes blk =
    serializeEncoding $
        CBOR.encodeListLen 2
     <> case unByronBlockOrEBB blk of
          CC.Block.ABOBBoundary b ->
              CBOR.encodeWord 0
           <> CBOR.encodePreEncoded (CC.Block.boundaryAnnotation b)

          CC.Block.ABOBBlock b ->
              CBOR.encodeWord 1
           <> CBOR.encodePreEncoded (CC.Block.blockAnnotation b)

-- | Encode a block using CBOR-in-CBOR tag 24.
encodeByronBlock :: ByronBlockOrEBB cfg -> Encoding
encodeByronBlock = encodeNestedCborBytes . blockBytes

-- | Inversion of `encodeByronBlock`. The annotation will be correct, because
-- the full bytes are available thanks to the CBOR-in-CBOR encoding.
decodeByronBlock :: CC.Slot.EpochSlots -> Decoder s (ByronBlockOrEBB cfg)
decodeByronBlock epochSlots = do
  theBytes <- decodeNestedCborBytes
  case decodeFullAnnotatedBytes "Block" internalDecoder (Lazy.fromStrict theBytes) of
    Right it  -> pure $ ByronBlockOrEBB it
    -- FIXME
    --   err :: DecodeError
    -- but AFAICT the only way to make the decoder fail is to give a `String`
    -- to `fail`...
    Left  err -> fail (show err)
  where
  internalDecoder :: Decoder s (CC.Block.ABlockOrBoundary ByteSpan)
  internalDecoder = CC.Block.fromCBORABlockOrBoundary epochSlots

-- | Get the encoded bytes of a header. A legacy Byron node (cardano-sl) would
-- successfully decode a header from these.
headerBytes :: Header (ByronBlockOrEBB cfg) -> Lazy.ByteString
headerBytes blk =
    serializeEncoding $
        CBOR.encodeListLen 2
     <> case unByronHeaderOrEBB blk of
          Left ebb ->
              CBOR.encodeWord 0
           <> CBOR.encodePreEncoded (CC.Block.boundaryHeaderAnnotation ebb)

          Right hdr ->
              CBOR.encodeWord 1
           <> CBOR.encodePreEncoded (CC.Block.headerAnnotation hdr)

-- | Encode a header using CBOR-in-CBOR tag 24.
encodeByronHeader :: Header (ByronBlockOrEBB cfg) -> Encoding
encodeByronHeader = encodeNestedCborBytes . headerBytes

-- | Inversion of `encodeByronHeader`. The annotation will be correct, because
-- the full bytes are available thanks to the CBOR-in-CBOR encoding.
decodeByronHeader :: CC.Slot.EpochSlots -> Decoder s (Header (ByronBlockOrEBB cfg))
decodeByronHeader epochSlots = do
  theBytes <- decodeNestedCborBytes
  -- We can use decodeFullAnnotatedBytes by using it at the EitherF type, which
  -- only works for an f ByteSpan
  case decodeFullAnnotatedBytes "Header" internalDecoder (Lazy.fromStrict theBytes) of
    Right (LeftF ebb)  -> pure $ ByronHeaderOrEBB (Left ebb)
    Right (RightF hdr) -> pure $ ByronHeaderOrEBB (Right hdr)
    Left  err          -> fail (show err)
  where
  -- cardano-ledger does not export a decoder for genesis or main header with
  -- the list length and tag bytes. It _does_ export one for blocks, used in
  -- decodeByronBlock (fromCBORABlockOrBoundary)
  internalDecoder :: Decoder s (EitherF CC.Block.ABoundaryHeader CC.Block.AHeader ByteSpan)
  internalDecoder = fromEither <$> fromCBORAHeaderOrBoundary epochSlots

-- | Defined only for use by decodeHeader.
data EitherF g h t where
  LeftF  :: g t -> EitherF g h t
  RightF :: h t -> EitherF g h t

instance (Functor g, Functor h) => Functor (EitherF g h) where
  fmap f (LeftF g)  = LeftF  (fmap f g)
  fmap f (RightF h) = RightF (fmap f h)

fromEither :: Either (g t) (h t) -> EitherF g h t
fromEither (Left  g) = LeftF  g
fromEither (Right h) = RightF h

encodeByronHeaderHash :: HeaderHash (ByronBlockOrEBB cfg) -> Encoding
encodeByronHeaderHash = toCBOR

encodeByronLedgerState :: LedgerState (ByronBlockOrEBB cfg) -> Encoding
encodeByronLedgerState (ByronEBBLedgerState ByronLedgerState{..}) = mconcat
    [ CBOR.encodeListLen 2
    , encode blsCurrent
    , encode blsSnapshots
    ]

encodeByronChainState :: ChainState (BlockProtocol (ByronBlock cfg)) -> Encoding
encodeByronChainState = encode

decodeByronHeaderHash :: Decoder s (HeaderHash (ByronBlockOrEBB cfg))
decodeByronHeaderHash = fromCBOR

encodeByronGenTx :: GenTx (ByronBlockOrEBB cfg) -> Encoding
encodeByronGenTx (ByronTx {byronTx = tx }) = toCBOR (void tx)

encodeByronGenTxId :: GenTxId (ByronBlockOrEBB cfg) -> Encoding
encodeByronGenTxId (ByronTxId txid) = toCBOR txid

encodeByronApplyTxError :: ApplyTxErr (ByronBlockOrEBB cfg) -> Encoding
encodeByronApplyTxError = toCBOR

decodeByronGenTx :: Decoder s (GenTx (ByronBlockOrEBB cfg))
decodeByronGenTx =
    mkByronTx . annotate <$> fromCBOR
  where
    -- TODO #560: Re-annotation can be done but requires some rearranging in
    -- the codecs Original ByteSpan's refer to bytestring we don't have, so
    -- we'll ignore them
    annotate :: CC.UTxO.ATxAux ByteSpan -> CC.UTxO.ATxAux ByteString
    annotate CC.UTxO.ATxAux{aTaTx, aTaWitness} =
      CC.UTxO.ATxAux{
        aTaTx      = reAnnotate aTaTx,
        aTaWitness = reAnnotate aTaWitness
      }

decodeByronGenTxId :: Decoder s (GenTxId (ByronBlockOrEBB cfg))
decodeByronGenTxId = ByronTxId <$> fromCBOR

decodeByronLedgerState :: Decoder s (LedgerState (ByronBlockOrEBB cfg))
decodeByronLedgerState = ByronEBBLedgerState <$> do
    CBOR.decodeListLenOf 2
    ByronLedgerState <$> decode <*> decode

decodeByronChainState :: Decoder s (ChainState (BlockProtocol (ByronBlock cfg)))
decodeByronChainState = decode

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

instance (ByronGiven, Typeable cfg, ConfigContainsGenesis cfg)
      => ApplyTx (ByronBlockOrEBB cfg) where
  -- | Generalized transactions in Byron
  --
  -- TODO #514: This is still missing the other cases (this shouldn't be a
  -- newtype)
  data GenTx (ByronBlockOrEBB cfg) = ByronTx
    { byronTx   :: !(CC.UTxO.ATxAux ByteString)
    , byronTxId :: CC.UTxO.TxId
      -- ^ This field is lazy on purpose so that the TxId is computed on
      -- demand.
    }

  newtype GenTxId (ByronBlockOrEBB cfg) = ByronTxId { unByronTxId :: CC.UTxO.TxId }
    deriving (Eq, Ord)

  txId = ByronTxId . byronTxId

  txSize (ByronTx { byronTx = atxaux }) = fromIntegral txByteSize
    where
      -- TODO cardano-ledger#576 will provide a function for this
      txByteSize = 1 -- To account for @encodeListLen 2@
                 + (Strict.length . annotation . CC.UTxO.aTaTx      $ atxaux)
                 + (Strict.length . annotation . CC.UTxO.aTaWitness $ atxaux)

  type ApplyTxErr (ByronBlockOrEBB cfg) = CC.UTxO.UTxOValidationError

  applyTx   = applyByronGenTx False
  reapplyTx = applyByronGenTx True

  -- TODO #440: We need explicit support for this from the ledger
  -- (though during testing we might still want to actually verify that we
  -- didn't get any errors)
  reapplyTxSameState cfg tx st =
    case runExcept (applyByronGenTx True cfg tx st) of
      Left  err -> error $ "unexpected error: " <> show err
      Right st' -> st'

applyByronGenTx :: Bool -- ^ Have we verified this transaction previously?
                -> LedgerConfig (ByronBlockOrEBB cfg)
                -> GenTx (ByronBlockOrEBB cfg)
                -> LedgerState (ByronBlockOrEBB cfg)
                -> Except CC.UTxO.UTxOValidationError
                          (LedgerState (ByronBlockOrEBB cfg))
applyByronGenTx _reapply (ByronEBBLedgerConfig (ByronLedgerConfig cfg)) genTx (ByronEBBLedgerState st@ByronLedgerState{..}) =
    (\x -> ByronEBBLedgerState $ st { blsCurrent = x }) <$> go genTx blsCurrent
  where
    validationMode = fromBlockValidationMode CC.Block.BlockValidation
    go :: GenTx (ByronBlockOrEBB cfg)
       -> CC.Block.ChainValidationState
       -> Except CC.UTxO.UTxOValidationError CC.Block.ChainValidationState
    go (ByronTx { byronTx = tx }) cvs = wrapCVS <$>
        runReaderT (CC.UTxO.updateUTxO env utxo [tx]) validationMode
      where
        wrapCVS newUTxO = cvs { CC.Block.cvsUtxo = newUTxO }
        protocolMagic = fixPM $ CC.Genesis.configProtocolMagic cfg
        utxo = CC.Block.cvsUtxo cvs
        updateState = CC.Block.cvsUpdateState cvs
        env = CC.UTxO.Environment
          { CC.UTxO.protocolMagic = protocolMagic
          , CC.UTxO.protocolParameters = CC.UPI.adoptedProtocolParameters updateState
          }
        fixPM (Crypto.AProtocolMagic a b) = Crypto.AProtocolMagic (reAnnotate a) b

mkByronTx :: CC.UTxO.ATxAux ByteString -> GenTx (ByronBlockOrEBB cfg)
mkByronTx tx = ByronTx
    { byronTx   = tx
    , byronTxId = Crypto.hash $ CC.UTxO.taTx tx
      -- TODO replace this with a function from cardano-ledger, see
      -- cardano-ledger#581
    }

{-------------------------------------------------------------------------------
  PBFT integration
-------------------------------------------------------------------------------}

instance (ByronGiven, Typeable cfg, ConfigContainsGenesis cfg)
      => ProtocolLedgerView (ByronBlockOrEBB cfg) where
  protocolLedgerView _ns (ByronEBBLedgerState (ByronLedgerState ls _))
    = PBftLedgerView
    . CC.Delegation.unMap
    . V.Interface.delegationMap
    . CC.Block.cvsDelegationState
    $ ls

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
        Just sb -> Just
                 $  PBftLedgerView
                 .  CC.Delegation.unMap
                 .  V.Interface.delegationMap
                 .  CC.Block.cvsDelegationState
                <$> sb
        -- No snapshot - we could be in the past or in the future
        Nothing
          | slot >= At lvLB && slot <= At lvUB
          -> Just $ PBftLedgerView <$>
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
          | otherwise
          -> Nothing
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

      dsNow = CC.Delegation.unMap
            . V.Interface.delegationMap
            . CC.Block.cvsDelegationState
            $ ls
      dsScheduled = V.Scheduling.scheduledDelegations
                  . V.Interface.schedulingState
                  . CC.Block.cvsDelegationState
                  $ ls
      currentSlot = convertSlot $ CC.Block.cvsLastSlot ls
      containsSlot s sb = At (sbLower sb) <= s && At (sbUpper sb) >= s
