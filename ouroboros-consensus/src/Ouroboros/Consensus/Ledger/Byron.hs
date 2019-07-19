{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PatternSynonyms       #-}

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
  , computeGenTxId
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
  , decodeByronHeader
  , decodeByronBlock
  , decodeByronHeaderHash
  , decodeByronGenTx
  , decodeByronGenTxId
  , decodeByronLedgerState
  , decodeByronChainState
    -- * EBBs
  , ByronBlockOrEBB (..)
  , pattern ByronHeaderOrEBB
  , unByronHeaderOrEBB
  , annotateBoundary
  ) where

import           Codec.CBOR.Decoding (Decoder, decodeListLenOf)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (Serialise, decode, encode)
import           Control.Monad.Except
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Bifunctor (bimap)
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

import           Cardano.Binary (Annotated (..), ByteSpan, enforceSize,
                     fromCBOR, reAnnotate, slice, toCBOR)
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
import qualified Ouroboros.Network.Point as Point (block, origin)
import           Ouroboros.Network.Point (WithOrigin (..))

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
      { blsCurrent :: CC.Block.ChainValidationState
        -- | Slot-bounded snapshots of the chain state
      , blsSnapshots :: Seq.Seq (SlotBounded CC.Block.ChainValidationState)
      }
    deriving (Eq, Show)

  type LedgerError (ByronBlock cfg) = CC.Block.ChainValidationError

  newtype LedgerConfig (ByronBlock cfg) = ByronLedgerConfig CC.Genesis.Config

  ledgerConfigView EncNodeConfig{..} = ByronLedgerConfig $
    genesisConfig encNodeConfigExt

  applyLedgerBlock (ByronLedgerConfig cfg) (ByronBlock block)
                   (ByronLedgerState state snapshots) = do
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

  applyLedgerHeader (ByronLedgerConfig cfg) (ByronHeader hdr)
                    (ByronLedgerState state snapshots) =
      mapExcept (fmap (\i -> ByronLedgerState i snapshots)) $ do
        updateState <- runReaderT
          (CC.Block.updateHeader headerEnv (CC.Block.cvsUpdateState state) hdr)
          (fromBlockValidationMode CC.Block.BlockValidation)
        return $ state
          { CC.Block.cvsLastSlot     = CC.Block.headerSlot hdr
          , CC.Block.cvsPreviousHash = Right $ CC.Block.headerHashAnnotated hdr
          , CC.Block.cvsUpdateState  = updateState
          }
    where
      headerEnv = CC.Block.HeaderEnvironment
        { CC.Block.protocolMagic = fixPMI $ CC.Genesis.configProtocolMagicId cfg
        , CC.Block.k             = CC.Genesis.configK cfg
        , CC.Block.allowedDelegators = allowedDelegators cfg
        , CC.Block.delegationMap = delegationMap
        , CC.Block.lastSlot      = CC.Block.cvsLastSlot state
        }
      delegationMap = V.Interface.delegationMap
                    $ CC.Block.cvsDelegationState state

      fixPMI pmi = reAnnotate $ Annotated pmi ()

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
  type Signed (Header (ByronBlock cfg)) = CC.Block.ToSign
  encodeSigned = const toCBOR
  headerSigned = unAnnotated
               . CC.Block.recoverSignedBytes given
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
      unByronHeaderOrEBB :: Either (CC.Block.BoundaryValidationData ByteString) (CC.Block.AHeader ByteString)
    } deriving (Eq, Show)

  getHeader (ByronBlockOrEBB (CC.Block.ABOBBlock b)) = ByronHeaderOrEBB . Right $ CC.Block.blockHeader b
  getHeader (ByronBlockOrEBB (CC.Block.ABOBBoundary b)) = ByronHeaderOrEBB . Left $ b

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
    Left ebb -> CC.Block.boundaryHashAnnotated ebb
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
  type HeaderOfEBB (Header (ByronBlockOrEBB cfg)) = CC.Block.BoundaryValidationData ByteString

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

  applyLedgerBlock (ByronEBBLedgerConfig cfg) (ByronBlockOrEBB block)
                   (ByronEBBLedgerState state) =
    case block of
      CC.Block.ABOBBlock b -> ByronEBBLedgerState <$> applyLedgerBlock cfg (ByronBlock b) state
      CC.Block.ABOBBoundary _ -> return $ ByronEBBLedgerState state

  applyLedgerHeader (ByronEBBLedgerConfig cfg) (ByronHeaderOrEBB hdr)
                    (ByronEBBLedgerState bs@(ByronLedgerState state snapshots)) = case hdr of
    Left ebb ->
      mapExcept (fmap (\i -> ByronEBBLedgerState $ ByronLedgerState i snapshots)) $ do
        return $ state
          { CC.Block.cvsPreviousHash = Right $ CC.Block.boundaryHashAnnotated ebb
          , CC.Block.cvsLastSlot = CC.Slot.SlotNumber $ epochSlots * (CC.Block.boundaryEpoch ebb)
          }
      where
        CC.Slot.EpochSlots epochSlots = given

    Right h -> ByronEBBLedgerState <$> applyLedgerHeader cfg (ByronHeader h) bs

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
  condense (ByronBlockOrEBB (CC.Block.ABOBBlock blk)) = condense (ByronBlock blk)
  condense (ByronBlockOrEBB (CC.Block.ABOBBoundary bvd)) = condenseBVD bvd

condenseBVD :: CC.Block.BoundaryValidationData ByteString -> String
condenseBVD bvd =
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
        $ bvd

      condensedPrevHash
        = T.unpack $ case CC.Block.boundaryPrevHash bvd of
            Left _  -> "Genesis"
            Right h -> sformat CC.Block.headerHashF h

instance Condense (Header (ByronBlockOrEBB cfg)) where
  condense (ByronHeaderOrEBB (Right hdr)) = condense (ByronHeader hdr)
  condense (ByronHeaderOrEBB (Left bvd))  = condenseBVD bvd

instance Condense CC.Block.HeaderHash where
  condense = formatToString CC.Block.headerHashF

instance Condense (ChainHash (ByronBlockOrEBB cfg)) where
  condense GenesisHash   = "genesis"
  condense (BlockHash h) = condense h

instance Condense (GenTx (ByronBlockOrEBB cfg)) where
    condense (ByronTx tx) =
      "(tx: "       <> show (CC.UTxO.aTaTx tx) <>
      ", witness: " <> show (CC.UTxO.aTaWitness tx) <>
      ")"

instance Show (GenTx (ByronBlockOrEBB cfg)) where
    show tx = condense tx

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise CC.Block.ChainValidationState where
  encode = toCBOR
  decode = fromCBOR

instance Serialise CC.Common.KeyHash where
  encode = toCBOR
  decode = fromCBOR

encodeByronHeader
  :: Crypto.ProtocolMagicId
  -> CC.Slot.EpochSlots
  -> Header (ByronBlockOrEBB cfg)
  -> Encoding
encodeByronHeader pm epochSlots (ByronHeaderOrEBB h) = toCBORAHeaderOrBoundary pm epochSlots h

encodeByronBlock :: Crypto.ProtocolMagicId -> CC.Slot.EpochSlots -> ByronBlockOrEBB cfg -> Encoding
encodeByronBlock pm epochSlots bob = case unByronBlockOrEBB bob of
  CC.Block.ABOBBlock b      -> CC.Block.toCBORABOBBlock epochSlots . void $ b
  CC.Block.ABOBBoundary ebb -> CC.Block.toCBORABOBBoundary pm . void $ ebb

encodeByronHeaderHash :: HeaderHash (ByronBlockOrEBB cfg) -> Encoding
encodeByronHeaderHash = toCBOR

encodeByronLedgerState :: LedgerState (ByronBlockOrEBB cfg) -> Encoding
encodeByronLedgerState (ByronEBBLedgerState ByronLedgerState{..}) = mconcat
    [ encodeListLen 2
    , encode blsCurrent
    , encode blsSnapshots
    ]

encodeByronChainState :: ChainState (BlockProtocol (ByronBlock cfg)) -> Encoding
encodeByronChainState = encode

decodeByronHeader :: Crypto.ProtocolMagicId -> CC.Slot.EpochSlots -> Decoder s (Header (ByronBlockOrEBB cfg))
decodeByronHeader pm epochSlots =
    ByronHeaderOrEBB . bimap annotateBo annotateH <$> fromCBORAHeaderOrBoundary epochSlots
  where
    -- TODO #560: Re-annotation can be done but requires some rearranging in
    -- the codecs Original ByteSpan's refer to bytestring we don't have, so
    -- we'll ignore them
    annotateH :: CC.Block.AHeader a -> CC.Block.AHeader ByteString
    annotateH = annotateHeader epochSlots . void

    annotateBo :: CC.Block.BoundaryValidationData a -> CC.Block.BoundaryValidationData ByteString
    annotateBo = annotateBoundary pm . void

decodeByronBlock :: Crypto.ProtocolMagicId -> CC.Slot.EpochSlots -> Decoder s (ByronBlockOrEBB cfg)
decodeByronBlock pm epochSlots =
    ByronBlockOrEBB . mapABOB annotateBl annotateBo
      <$> CC.Block.fromCBORABlockOrBoundary epochSlots
  where
    mapABOB f g abob = case abob of
      CC.Block.ABOBBlock x    -> CC.Block.ABOBBlock $ f x
      CC.Block.ABOBBoundary x -> CC.Block.ABOBBoundary $ g x
    -- TODO #560: Re-annotation can be done but requires some rearranging in
    -- the codecs Original ByteSpan's refer to bytestring we don't have, so
    -- we'll ignore them
    annotateBl :: CC.Block.ABlock a -> CC.Block.ABlock ByteString
    annotateBl = annotateBlock epochSlots . void

    annotateBo :: CC.Block.BoundaryValidationData a -> CC.Block.BoundaryValidationData ByteString
    annotateBo = annotateBoundary pm . void

decodeByronHeaderHash :: Decoder s (HeaderHash (ByronBlockOrEBB cfg))
decodeByronHeaderHash = fromCBOR

encodeByronGenTx :: GenTx (ByronBlockOrEBB cfg) -> Encoding
encodeByronGenTx (ByronTx tx) = toCBOR (void tx)

encodeByronGenTxId :: GenTxId (ByronBlockOrEBB cfg) -> Encoding
encodeByronGenTxId (ByronTxId txid) = toCBOR txid

decodeByronGenTx :: Decoder s (GenTx (ByronBlockOrEBB cfg))
decodeByronGenTx =
    ByronTx . annotate <$> fromCBOR
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
    decodeListLenOf 2
    ByronLedgerState <$> decode <*> decode

decodeByronChainState :: Decoder s (ChainState (BlockProtocol (ByronBlock cfg)))
decodeByronChainState = decode

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

annotateHeader :: CC.Slot.EpochSlots
               -> CC.Block.AHeader ()
               -> CC.Block.AHeader ByteString
annotateHeader epochSlots =
      (\bs -> splice bs (CBOR.deserialiseFromBytes
                           (CC.Block.fromCBORAHeader epochSlots)
                           bs))
    . CBOR.toLazyByteString
    . CC.Block.toCBORHeader epochSlots
  where
    splice :: Lazy.ByteString
           -> Either err (Lazy.ByteString, CC.Block.AHeader ByteSpan)
           -> CC.Block.AHeader ByteString
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
                 -> CC.Block.BoundaryValidationData ()
                 -> CC.Block.BoundaryValidationData ByteString
annotateBoundary pm =
      (\bs -> splice bs (CBOR.deserialiseFromBytes
                           CC.Block.dropBoundaryBlock
                           bs))
    . CBOR.toLazyByteString
    . CC.Block.toCBORBoundaryBlock pm
  where
    splice :: Show err
           => Lazy.ByteString
           -> Either err (Lazy.ByteString, CC.Block.BoundaryValidationData ByteSpan)
           -> CC.Block.BoundaryValidationData ByteString
    splice _ (Left err) =
      error $ "annotateBoundary: serialization roundtrip failure: " <> show err
    splice bs (Right (_leftover, boundary)) =
      (Lazy.toStrict . slice bs) <$> boundary

fromCBORAHeaderOrBoundary
  :: CC.Slot.EpochSlots
  -> Decoder s (Either (CC.Block.BoundaryValidationData ByteSpan) (CC.Block.AHeader ByteSpan))
fromCBORAHeaderOrBoundary epochSlots = do
  enforceSize "Block" 2
  fromCBOR @Word >>= \case
    0 -> Left <$> CC.Block.dropBoundaryBlock
    1 -> Right <$> CC.Block.fromCBORAHeader epochSlots
    t -> error $ "Unknown tag in encoded HeaderOrBoundary" <> show t

toCBORAHeaderOrBoundary
  :: Crypto.ProtocolMagicId
  -> CC.Slot.EpochSlots
  -> (Either (CC.Block.BoundaryValidationData a) (CC.Block.AHeader a))
  -> Encoding
toCBORAHeaderOrBoundary pm epochSlots abob =
  encodeListLen 2 <>
  case abob of
    Right mh -> toCBOR (1 :: Word) <> (CC.Block.toCBORHeader epochSlots . void $ mh)
    Left ebb -> toCBOR (0 :: Word) <> (CC.Block.toCBORBoundaryBlock pm . void $ ebb)

{-------------------------------------------------------------------------------
  Mempool integration
-------------------------------------------------------------------------------}

instance (ByronGiven, Typeable cfg, ConfigContainsGenesis cfg)
      => ApplyTx (ByronBlockOrEBB cfg) where
  -- | Generalized transactions in Byron
  --
  -- TODO #514: This is still missing the other cases (this shouldn't be a
  -- newtype)
  data GenTx (ByronBlockOrEBB cfg) = ByronTx { unByronTx :: CC.UTxO.ATxAux ByteString }

  newtype GenTxId (ByronBlockOrEBB cfg) = ByronTxId { unByronTxId :: CC.UTxO.TxId }
    deriving (Eq, Ord)

  computeGenTxId = ByronTxId . Crypto.hash . CC.UTxO.taTx . unByronTx

  txSize (ByronTx atxaux) = fromIntegral txByteSize
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
    go (ByronTx tx) cvs = wrapCVS <$>
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
             case Seq.takeWhileL
                    (\sd -> At (convertSlot (V.Scheduling.sdSlot sd)) <= slot)
                    dsScheduled of
                -- No updates to apply. So the current ledger state is valid
                -- from the end of the last snapshot to the first scheduled
                -- update.
               Seq.Empty              -> SB.bounded lb ub dsNow
               toApply@(_ Seq.:|> la) ->
                 SB.bounded lb (convertSlot . V.Scheduling.sdSlot $ la) $
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
      ub = case dsScheduled of
        s Seq.:<| _ -> min lvUB (convertSlot $ V.Scheduling.sdSlot s)
        Seq.Empty   -> lvUB

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
