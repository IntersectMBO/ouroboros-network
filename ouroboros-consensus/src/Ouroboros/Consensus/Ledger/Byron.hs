{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

module Ouroboros.Consensus.Ledger.Byron
  ( -- * Byron blocks and headers
    ByronBlock (..)
  , annotateByronBlock
  , ByronGiven
  , ConfigContainsGenesis(..)
    -- * Mempool integration
  , GenTx (..)
    -- * Ledger
  , LedgerState (..)
  , LedgerConfig (..)
    -- * Serialisation
  , encodeByronHeader
  , encodeByronBlock
  , encodeByronHeaderHash
  , encodeByronGenTx
  , decodeByronHeader
  , decodeByronBlock
  , decodeByronHeaderHash
  , decodeByronGenTx
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Monad.Except
import qualified Data.Bimap as Bimap
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce (coerce)
import           Data.FingerTree (Measured (..))
import           Data.Foldable (find, foldl')
import           Data.Reflection (Given (..))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Typeable
import           Data.Word (Word8)
import           Formatting

import           Cardano.Binary (Annotated (..), ByteSpan, fromCBOR, reAnnotate,
                     slice, toCBOR)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as CC.Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as V.Interface
import qualified Cardano.Chain.Delegation.Validation.Scheduling as V.Scheduling
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import qualified Cardano.Chain.UTxO as CC.UTxO
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (genesisSlotNo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.SlotBounded (SlotBounded (..))
import qualified Ouroboros.Consensus.Util.SlotBounded as SB

{-------------------------------------------------------------------------------
  Byron blocks and headers
-------------------------------------------------------------------------------}

-- | Newtype wrapper to avoid orphan instances
--
-- The phantom type parameter is there to record the additional information
-- we need to work with this block. Most of the code here does not care,
-- but we may need different additional information when running the chain
-- for real as when we are running the demo.
newtype ByronBlock cfg = ByronBlock
  { unByronBlock :: CC.Block.ABlock ByteString
  }
  deriving (Eq, Show)

-- | Unfortunate Byron requires some information that is morally static but
-- actually comes from configuration files on startup. We use reflection to
-- provide this to type class instances.
type ByronGiven = (
    Given CC.Block.HeaderHash
  , Given Crypto.ProtocolMagicId
  , Given CC.Slot.EpochSlots
  )

instance GetHeader (ByronBlock cfg) where
  newtype Header (ByronBlock cfg) = ByronHeader {
      unByronHeader :: CC.Block.AHeader ByteString
    } deriving (Eq, Show)

  getHeader (ByronBlock b) = ByronHeader (CC.Block.blockHeader b)

instance (ByronGiven, Typeable cfg) => SupportedBlock (ByronBlock cfg)

-- | Construct Byron block from unannotated 'CC.Block.Block'
--
-- This should be used only when forging blocks (not when receiving blocks
-- over the wire).
annotateByronBlock :: CC.Slot.EpochSlots -> CC.Block.Block -> ByronBlock cfg
annotateByronBlock epochSlots = ByronBlock . annotateBlock epochSlots

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
  blockHash = headerHashAnnotated . unByronHeader

  -- We distinguish the genesis hash
  --
  -- Note that on the actual chain, for real blocks the previous hash will
  -- never be genesis, only for EBBs can it be genesis. We don't enforce that
  -- here (makes stuff like the demo easier, and also means we could for
  -- example run a shelley-only chain).
  blockPrevHash (ByronHeader h) = case CC.Block.headerPrevHash h of
    h' | h' == given -> GenesisHash
    _                -> BlockHash $ CC.Block.headerPrevHash h

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
        <- CC.Block.updateBody bodyEnv bodyState block
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
        , CC.Block.numGenKeys         = numGenKeys         cfg
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
        updateState <- CC.Block.updateHeader
          headerEnv
          (CC.Block.cvsUpdateState state)
          hdr
        return $ state
          { CC.Block.cvsLastSlot     = CC.Block.headerSlot hdr
          , CC.Block.cvsPreviousHash = Right $ headerHashAnnotated hdr
          , CC.Block.cvsUpdateState  = updateState
          }
    where
      headerEnv = CC.Block.HeaderEnvironment
        { CC.Block.protocolMagic = fixPMI $ CC.Genesis.configProtocolMagicId cfg
        , CC.Block.k             = CC.Genesis.configK cfg
        , CC.Block.numGenKeys    = numGenKeys         cfg
        , CC.Block.delegationMap = delegationMap
        , CC.Block.lastSlot      = CC.Block.cvsLastSlot state
        }
      delegationMap = V.Interface.delegationMap
                    $ CC.Block.cvsDelegationState state

      fixPMI pmi = reAnnotate $ Annotated pmi ()

  ledgerTipPoint (ByronLedgerState state _) = Point
    { pointSlot = convertSlot (CC.Block.cvsLastSlot state)
    , pointHash = case CC.Block.cvsPreviousHash state of
                    Left _genHash -> GenesisHash
                    Right hdrHash -> BlockHash hdrHash
    }

numGenKeys :: CC.Genesis.Config -> Word8
numGenKeys cfg = case length genKeys of
    n | n > fromIntegral (maxBound :: Word8)
      -> error "updateBody: Too many genesis keys"
      | otherwise
      -> fromIntegral n
  where
    genKeys :: Set CC.Common.KeyHash
    genKeys = CC.Genesis.unGenesisKeyHashes
            . CC.Genesis.configGenesisKeyHashes
            $ cfg

{-------------------------------------------------------------------------------
  Support for PBFT consensus algorithm
-------------------------------------------------------------------------------}

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
                      . Crypto.pskDelegateVK
                      . CC.Block.delegationCertificate
                      . CC.Block.headerSignature
                      $ hdr
      , pbftSignature = SignedDSIGN
                      . SigCardanoDSIGN
                      . CC.Block.signature
                      . CC.Block.headerSignature
                      $ hdr
      }

type instance BlockProtocol (ByronBlock cfg) =
  ExtNodeConfig cfg (PBft PBftCardanoCrypto)

instance (ByronGiven, Typeable cfg, ConfigContainsGenesis cfg)
      => ProtocolLedgerView (ByronBlock cfg) where
  protocolLedgerView _ns (ByronLedgerState ls _)
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
  anachronisticProtocolLedgerView cfg (ByronLedgerState ls ss) slot =
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
          | slot >= lvLB && slot <= lvUB
          -> Just $ PBftLedgerView <$>
             case Seq.takeWhileL
                    (\sd -> convertSlot (V.Scheduling.sdSlot sd) <= slot)
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
        = genesisSlotNo
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
      containsSlot s sb = sbLower sb <= s && sbUpper sb >= s

{-------------------------------------------------------------------------------
  Mempool integration
-------------------------------------------------------------------------------}

instance (ByronGiven, Typeable cfg, ConfigContainsGenesis cfg)
      => ApplyTx (ByronBlock cfg) where
  -- | Generalized transactions in Byron
  --
  -- TODO #514: This is still missing the other cases (this shouldn't be a
  -- newtype)
  data GenTx (ByronBlock cfg) = ByronTx { unByronTx :: CC.UTxO.ATxAux ByteString }

  type ApplyTxErr (ByronBlock cfg) = CC.UTxO.UTxOValidationError

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
                -> LedgerConfig (ByronBlock cfg)
                -> GenTx (ByronBlock cfg)
                -> LedgerState (ByronBlock cfg)
                -> Except CC.UTxO.UTxOValidationError
                          (LedgerState (ByronBlock cfg))
applyByronGenTx _reapply (ByronLedgerConfig cfg) genTx st@ByronLedgerState{..} =
    (\x -> st { blsCurrent = x }) <$> go genTx blsCurrent
  where
    go :: GenTx (ByronBlock cfg)
       -> CC.Block.ChainValidationState
       -> Except CC.UTxO.UTxOValidationError CC.Block.ChainValidationState
    go (ByronTx tx) cvs = wrapCVS <$> CC.UTxO.updateUTxO env utxo [tx]
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
  Auxiliary
-------------------------------------------------------------------------------}

convertSlot :: CC.Slot.FlatSlotId -> SlotNo
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
      psigPsk = CC.Block.delegationCertificate
              . CC.Block.headerSignature
              . unByronHeader
              $ hdr
      issuer   = Crypto.pskIssuerVK   psigPsk
      delegate = Crypto.pskDelegateVK psigPsk

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

instance Condense (ChainHash (ByronBlock cfg)) where
  condense GenesisHash   = "genesis"
  condense (BlockHash h) = show h

instance Condense (GenTx (ByronBlock cfg)) where
    condense (ByronTx tx) =
      "(tx: "       <> show (CC.UTxO.aTaTx tx) <>
      ", witness: " <> show (CC.UTxO.aTaWitness tx) <>
      ")"

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeByronHeader :: CC.Slot.EpochSlots -> Header (ByronBlock cfg) -> Encoding
encodeByronHeader epochSlots =
    CC.Block.toCBORHeader epochSlots . void . unByronHeader

encodeByronBlock :: CC.Slot.EpochSlots -> ByronBlock cfg -> Encoding
encodeByronBlock epochSlots =
    CC.Block.toCBORBlock epochSlots . void . unByronBlock

encodeByronHeaderHash :: HeaderHash (ByronBlock cfg) -> Encoding
encodeByronHeaderHash = toCBOR

encodeByronGenTx :: GenTx (ByronBlock cfg) -> Encoding
encodeByronGenTx (ByronTx tx) = toCBOR (void tx)

decodeByronHeader :: CC.Slot.EpochSlots -> Decoder s (Header (ByronBlock cfg))
decodeByronHeader epochSlots =
    ByronHeader . annotate <$> CC.Block.fromCBORAHeader epochSlots
  where
    -- TODO #560: Re-annotation can be done but requires some rearranging in
    -- the codecs Original ByteSpan's refer to bytestring we don't have, so
    -- we'll ignore them
    annotate :: CC.Block.AHeader a -> CC.Block.AHeader ByteString
    annotate = annotateHeader epochSlots . void

decodeByronBlock :: CC.Slot.EpochSlots -> Decoder s (ByronBlock cfg)
decodeByronBlock epochSlots =
    ByronBlock . annotate <$> CC.Block.fromCBORABlock epochSlots
  where
    -- TODO #560: Re-annotation can be done but requires some rearranging in
    -- the codecs Original ByteSpan's refer to bytestring we don't have, so
    -- we'll ignore them
    annotate :: CC.Block.ABlock a -> CC.Block.ABlock ByteString
    annotate = annotateBlock epochSlots . void

decodeByronHeaderHash :: Decoder s (HeaderHash (ByronBlock cfg))
decodeByronHeaderHash = fromCBOR

decodeByronGenTx :: Decoder s (GenTx (ByronBlock cfg))
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

-- Implementation of 'blockHash' derived from
--
-- > blockHashAnnotated :: ABlock ByteString -> HeaderHash
-- > blockHashAnnotated = hashDecoded . fmap wrapHeaderBytes . blockHeader
--
-- I couldn't find a version for headers
headerHashAnnotated :: CC.Block.AHeader ByteString
                    -> Crypto.Hash CC.Block.Header
headerHashAnnotated = Crypto.hashDecoded . fmap CC.Block.wrapHeaderBytes
