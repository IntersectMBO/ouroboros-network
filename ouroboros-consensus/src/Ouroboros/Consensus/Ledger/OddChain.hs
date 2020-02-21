{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Ledger.OddChain where

import           Prelude hiding (flip)

import           Cardano.Prelude (cborError)
import           Codec.CBOR.Decoding as D
import           Codec.Serialise (decode, encode)
import           Control.Monad.Except (throwError)
import           Crypto.Random (MonadRandom)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as Strict
import           Data.Either (isRight)
import           Data.FingerTree (Measured, measure)
import           Data.Map.Strict ((!))
import           Data.Maybe (catMaybes)
import           Data.Proxy (Proxy (Proxy))
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Binary (DecoderError (DecoderErrorUnknownTag),
                     Encoding, FromCBOR, ToCBOR, encodeInt, encodeListLen,
                     encodeWord, fromCBOR, serialize', toCBOR)
import           Cardano.Crypto (ProtocolMagicId (ProtocolMagicId))
import           Cardano.Crypto.DSIGN.Class (SigDSIGN,
                     SignedDSIGN (SignedDSIGN), VerKeyDSIGN, signDSIGN,
                     verifyDSIGN)
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Crypto.Hash.Class (hash)
import qualified Cardano.Crypto.Hash.Class as Crypto.Hash
import qualified Cardano.Crypto.Hash.Class
import           Cardano.Crypto.Hash.Short (ShortHash)
import           Cardano.Prelude (NoUnexpectedThunks)
import           Ouroboros.Consensus.Block (GetHeader, Header, SupportedBlock,
                     getHeader, validateView)
import           Ouroboros.Consensus.BlockchainTime.SlotLengths
                     (SystemStart (SystemStart))
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip,
                     ValidateEnvelope)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState,
                     ledgerState)
import           Ouroboros.Consensus.Mempool.API (ApplyTx, ApplyTxErr, GenTx,
                     HasTxId (TxId, txId), applyTx, reapplyTx, txSize)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (HasNetworkProtocolVersion)
import           Ouroboros.Consensus.Node.Run.Abstract (RunNode,
                     nodeBlockEncodingOverhead,
                     nodeBlockFetchSize, nodeBlockMatchesHeader,
                     nodeCheckIntegrity, nodeDecodeApplyTxError,
                     nodeDecodeBlock, nodeDecodeChainState, nodeDecodeGenTx,
                     nodeDecodeGenTxId, nodeDecodeHeader, nodeDecodeHeaderHash,
                     nodeDecodeLedgerState, nodeDecodeQuery, nodeDecodeResult,
                     nodeDecodeTipInfo, nodeDecodeWrappedHeader,
                     nodeEncodeApplyTxError, nodeEncodeBlockWithInfo,
                     nodeEncodeChainState, nodeEncodeGenTx, nodeEncodeGenTxId,
                     nodeEncodeHeader, nodeEncodeHeaderHash,
                     nodeEncodeLedgerState, nodeEncodeQuery, nodeEncodeResult,
                     nodeEncodeTipInfo, nodeEncodeWrappedHeader, nodeEpochSize,
                     nodeForgeBlock, nodeHashInfo, nodeIsEBB,
                     nodeMaxBlockSize, nodeNetworkMagic, nodeProtocolMagicId,
                     nodeStartTime)
import           Ouroboros.Consensus.Protocol.Abstract (HasNodeState,
                     NodeConfig, ValidateView)
import           Ouroboros.Consensus.Protocol.BFT (Bft, BftDSIGN,
                     BftFields (BftFields), BftMockCrypto, bftNodeId,
                     bftSignKey, bftValidateView, bftVerKeys)
import           Ouroboros.Consensus.Protocol.ExtConfig (ExtConfig,
                     extNodeConfig, extNodeConfigP)
import           Ouroboros.Consensus.Protocol.Signed (Signed,
                     SignedHeader (headerSigned))
import           Ouroboros.Network.Block (BlockMeasure, BlockNo,
                     ChainHash (BlockHash, GenesisHash), HasHeader, HeaderHash,
                     Point (Point), SlotNo, StandardHash, blockHash,
                     blockInvariant, blockMeasure, blockNo, blockPrevHash,
                     blockSlot, castHash, unSlotNo)
import           Ouroboros.Network.BlockFetch (SizeInBytes)
import           Ouroboros.Network.Magic (NetworkMagic (NetworkMagic))
import qualified Ouroboros.Network.Point as Point
import           Ouroboros.Storage.Common (BinaryInfo (BinaryInfo), EpochNo,
                     EpochSize (EpochSize), binaryBlob, headerOffset,
                     headerSize)
import           Ouroboros.Storage.ImmutableDB.Types (HashInfo (HashInfo),
                     getHash, hashSize, putHash)
import           Ouroboros.Consensus.Util.Condense (Condense (condense))


data OddBlock
  = OddBlock
    { oddBlockHeader  :: !(Header OddBlock)
    , oddBlockPayload :: ![Tx]
    } deriving (Eq, Show,  HasAnnTip, ValidateEnvelope, HasNetworkProtocolVersion)

type Hash a = Crypto.Hash.Hash ShortHash a

type instance HeaderHash OddBlock = Hash (Header OddBlock)

instance StandardHash OddBlock

instance Measured BlockMeasure OddBlock where
  measure = blockMeasure

instance HasHeader OddBlock where
  blockHash = blockHash . oddBlockHeader

  blockPrevHash b = castHash $ blockPrevHash (oddBlockHeader b)

  blockNo = blockNo . oddBlockHeader

  blockSlot = blockSlot . oddBlockHeader

  blockInvariant = blockInvariant . oddBlockHeader

data SignedPart
  = SignedPart
    { oddBlockIssuer      :: !(VerKeyDSIGN (BftDSIGN BftMockCrypto))
    , oddBlockPrevHash    :: !(ChainHash OddBlock)
    , oddBlockNo          :: !BlockNo
    , oddBlockSlot        :: !SlotNo
    , oddBlockPayloadHash :: !(Hash [Tx])
    , oddBlockSize        :: !SizeInBytes
    } deriving (Show, Eq, Generic, NoUnexpectedThunks)

instance GetHeader OddBlock where
  data Header OddBlock
    = OddHeader
      { oddBlockSignedPart :: !SignedPart
      , oddBlockSignature  :: !(SigDSIGN (BftDSIGN BftMockCrypto))
        -- ^ The signed data must correspond with the 'Signed' type instance.
      } deriving (Show, Eq, Generic, NoUnexpectedThunks)

  getHeader = oddBlockHeader

instance HasHeader (Header OddBlock) where
  blockHash = hash

  blockPrevHash = castHash . oddBlockPrevHash . oddBlockSignedPart

  blockNo = oddBlockNo . oddBlockSignedPart

  blockSlot = oddBlockSlot . oddBlockSignedPart

  blockInvariant = const True

-- | An odd chain has two phases:
--
-- In the 'Decrease' phase, all numbers in a block must be smaller than the
-- numbers seen in the previous block. The number that is passed to the
-- constructor represents the upper bound on the numbers of a block.
---
-- In the 'Increase' phase, all the numbers in a block must be larger than the
-- numbers seen in the previous block. The number that is passed to the
-- constructor represents the lower bound on the numbers of a block.
--
-- The odd chain can be initialized in any of the two phases. This is, when
-- there is no previous block any phase and either lower or upper bound values
-- can be chosen.
data Phase = Decrease Int | Increase Int
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

flip :: Phase -> Phase
flip (Decrease i) = Increase i
flip (Increase i) = Decrease i

bump :: [Tx] -> Phase -> Phase
bump xs (Decrease i) = Decrease $ minimum $ i:(unTx <$> xs)
bump xs (Increase i) = Increase $ maximum $ i:(unTx <$> xs)

data OddError
  = OddError
    { currentPhase :: !Phase
      -- ^ Phase that was active at when the error occurred.
    , errors       :: ![OddTxError]
    }
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance UpdateLedger OddBlock where
  data LedgerState OddBlock
    = LedgerState
      { stCurrentSlot :: !SlotNo
      , stPrevHash    :: !(ChainHash OddBlock)
      , phase         :: !Phase
      } deriving (Eq, Show, Generic, NoUnexpectedThunks)

  type LedgerError OddBlock = OddError

  data LedgerConfig OddBlock
    = OddConfig
    { slotsPerEpoch    :: !Word64
      -- ^ How many slots can you fit in an epoch. We use epochs to change phases.
    , cfgNodeStartTime :: !UTCTime
    } deriving (Show, Generic, NoUnexpectedThunks)

  applyChainTick cfg currentSlot st
    = TickedLedgerState
      { tickedSlotNo      = currentSlot
      , tickedLedgerState =
          -- Note that the current slot shouldn't be changed here since no block is applied.
          st { phase = changePhaseOnEpochBoundary cfg currentSlot (phase st) }
      }

  applyLedgerBlock cfg blk@(OddBlock { oddBlockPayload }) st
    = let st'@LedgerState { phase } = tickedLedgerState
                                    $ applyChainTick cfg (blockSlot blk) st
      in case catMaybes $ fmap (checkTx st') oddBlockPayload of
          -- Remember to update the current slot. I didn't do it and I got a
          -- failed assertion, in  Ouroboros.Storage.ChainDB.Impl.ChainSel
          --
          -- >     assert (castPoint (AF.headPoint c) == LgrDB.currentPoint l) $
          --
          -- the problem was that the ledger was not reporting the tip of the
          -- last applied block.
          --
          [] -> pure $! st' { stCurrentSlot = blockSlot blk
                            , stPrevHash    = BlockHash $ blockHash blk
                            , phase         = bump oddBlockPayload phase
                            }
          xs -> throwError $ OddError
                             { currentPhase = phase
                             , errors       = xs
                             }

  reapplyLedgerBlock _cfg blk@OddBlock { oddBlockHeader, oddBlockPayload } st@LedgerState { phase }
    = st { stPrevHash    = BlockHash $ blockHash blk
         , stCurrentSlot = blockSlot blk -- QUESTION: Is this needed here?
         , phase         = bump oddBlockPayload phase
         }

  ledgerTipPoint LedgerState { stPrevHash, stCurrentSlot }
    -- So here I realized that the ledger state needs a
    -- the header hash of the last applied block.
    = Point
    $ case stPrevHash of
        GenesisHash          -> Point.origin
        BlockHash headerHash -> Point.block stCurrentSlot headerHash

changePhaseOnEpochBoundary
  :: LedgerConfig OddBlock
  -> SlotNo
  -- ^ Current slot.
  -> Phase
  -> Phase
changePhaseOnEpochBoundary OddConfig { slotsPerEpoch } currentSlot phase
  = if unSlotNo currentSlot `mod` slotsPerEpoch == 0
    then flip phase
    else phase

--------------------------------------------------------------------------------
-- Defining a 'ProtocolLedgerView' instance ...
--------------------------------------------------------------------------------

-- | A specific type of block should be designed for a specific kind of
-- protocol. We will design our odd blocks to run the Bft consensus algorithm.
--
-- In addition, we need to specify the BftCrypto. We will use the
-- 'BftMockCrypto'.
--
-- Note that if we wanted to be parametric over crypto we would have had to
-- define:
--
-- > data OddBlock c = ...
--
-- However, for the purpose of this exercise this isn't necessary.
type instance BlockProtocol OddBlock = ExtConfig (Bft BftMockCrypto) (LedgerConfig OddBlock)

-- By choosing the block protocol to be ExtConfig (Bft BftMockCrypto) (LedgerConfig OddBlock), we have
--
-- > instance BftCrypto c => OuroborosTag (Bft c) where
-- >   type ValidationErr (ExtConfig ...) = BftValidationErr
-- >   type ValidateView  (ExtConfig ...) = BftValidateView c
-- >   type NodeState     (ExtConfig ...) = ()
-- >   type LedgerView    (ExtConfig ...) = ()
-- >   type IsLeader      (ExtConfig ...) = ()
-- >   type ChainState    (ExtConfig ...) = ()
--
-- This OuroborosTag instance is one of the instances we need for defining a
-- supported block instance.
--
-- Also, by default, the OuroborosTag class defines:
--
-- > type SelectView p = BlockNo
--
-- And this isn't overridden in the 'OuroborosTag (Bft c)' instance.

instance SupportedBlock OddBlock where

  -- Remember that
  --
  -- > type BlockConfig b = NodeConfig (BlockProtocol b)
  --
  -- And ValidateView is the data required to validate the given header
  -- according to the consensus rules.
  validateView
    :: NodeConfig  (ExtConfig (Bft BftMockCrypto) (LedgerConfig OddBlock))
    -> Header OddBlock
    -> ValidateView (ExtConfig (Bft BftMockCrypto) (LedgerConfig OddBlock))
  validateView _cfg = bftValidateView getBftFields

  -- We use the default implementation of 'selectView', which extracts the block
  -- number from the header.

getBftFields :: Header OddBlock -> BftFields BftMockCrypto (Signed (Header OddBlock))
getBftFields = BftFields . SignedDSIGN . oddBlockSignature

type instance Signed (Header OddBlock) = SignedPart

-- We need to specify how to extract the data to be signed from the block
-- header.
instance SignedHeader (Header OddBlock) where
  headerSigned = oddBlockSignedPart

instance ProtocolLedgerView OddBlock where

  ledgerConfigView
    :: NodeConfig  (ExtConfig (Bft BftMockCrypto) (LedgerConfig OddBlock))
    -> LedgerConfig OddBlock
  ledgerConfigView = extNodeConfig

  -- The ledger view for Bft is just ().
  protocolLedgerView
    :: NodeConfig (ExtConfig (Bft BftMockCrypto) (LedgerConfig OddBlock))
    -> LedgerState OddBlock
    -> ()
  protocolLedgerView _ _ = ()

  -- The anachronistic ledger view should return the ledger view at a particular
  -- time. However, since the ledger view is just '()' we can always return it.
  anachronisticProtocolLedgerView _ _ _ = Right ()

--------------------------------------------------------------------------------
-- Defining a 'RunNode' instance ...
--------------------------------------------------------------------------------

instance ApplyTx OddBlock where
  newtype GenTx OddBlock = OddTx { unOddTx :: Tx }
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass (NoUnexpectedThunks)

  -- A transaction is a 64 bits Word.
  txSize _ = 4

  -- We use the same error for applying a transaction as for applying a block.
  -- In this case, both 'outOfBound' and 'notOdd' fields will contain at most
  -- one element, which is the integer in the transaction.
  type ApplyTxErr OddBlock = OddTxError

  applyTx _cfg (OddTx i) st@TickedLedgerState { tickedLedgerState } =
    case checkTx tickedLedgerState i of
      Nothing  -> pure st
      Just err -> throwError err

  reapplyTx = applyTx

checkTx :: LedgerState OddBlock -> Tx -> Maybe OddTxError
checkTx LedgerState { phase } (Tx i) =
  if odd i
  then        (`OddBut` i) <$> checkPhase i phase
  else Just $  NotOdd (checkPhase i phase) i

data OddTxError
  = NotOdd (Maybe OutOfBoundError) Int
  | OddBut OutOfBoundError Int
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

-- | Errors caused by numbers that do not obey the ordering constraints that the
-- current phase dictates.
data OutOfBoundError = NotDecreasing | NotIncreasing
  deriving (Eq, Show, Generic, NoUnexpectedThunks, Enum)

checkPhase :: Int -> Phase -> Maybe OutOfBoundError
checkPhase i (Decrease j) =
  if i < j then Nothing else Just NotDecreasing
checkPhase i (Increase j) =
  if j < i then Nothing else Just NotIncreasing

newtype Tx = Tx { unTx :: Int }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NoUnexpectedThunks)
  deriving newtype (ToCBOR, FromCBOR)

instance HasTxId (GenTx OddBlock) where

  newtype TxId (GenTx OddBlock) = TxId Int
    deriving (Eq, Ord, Show)
    deriving newtype (ToCBOR, FromCBOR)

  txId = TxId . unTx . unOddTx

instance ShowQuery (Query OddBlock) where

  showResult = \case {}

instance QueryLedger OddBlock where

  data Query OddBlock result
    deriving (Show)

  answerQuery = \case {}

  eqQuery = \case {}

-- Run node requires:
--
-- > instance ApplyTx OddBlock
-- > instance HasTxId (GenTx OddBlock)
-- > instance QueryLedger OddBlock

instance RunNode OddBlock where

  nodeForgeBlock
    :: (HasNodeState (BlockProtocol OddBlock) m, MonadRandom m)
    => NodeConfig (ExtConfig (Bft BftMockCrypto) (LedgerConfig OddBlock))
    -> SlotNo
    -- ^ Current slot
    -> BlockNo
    -- ^ Current block number
    -> ExtLedgerState OddBlock
    -- ^ Current ledger state
    -> [GenTx OddBlock]
    -- ^ Txs to add in the block
    -> ()
    -- ^ IsLeader (ExtConfig (Bft BftMockCrypto) (LedgerConfig OddBlock))
    -> m OddBlock
  nodeForgeBlock cfg slotNo aBlockNo extLedgerState txs () = do
    let signedPart = mkSignedPart nodeVKey prevHash aBlockNo slotNo payload
        nodeVKey   = vkeysMap ! bftNodeId (extNodeConfigP cfg)
        vkeysMap   = bftVerKeys $ extNodeConfigP cfg
        prevHash   = stPrevHash $ ledgerState $ extLedgerState
        payload    = fmap unOddTx txs
    oddSignature <- signDSIGN () signedPart (bftSignKey $ extNodeConfigP cfg)
    let
        header     = OddHeader
                     { oddBlockSignedPart = signedPart
                     , oddBlockSignature  = oddSignature
                     }
    pure $! OddBlock
            { oddBlockHeader = header
            , oddBlockPayload = payload
            }


  nodeBlockMatchesHeader
    :: Header OddBlock -> OddBlock -> Bool
  nodeBlockMatchesHeader hdr OddBlock { oddBlockHeader } = hdr == oddBlockHeader

  -- Remember
  --
  -- > type SizeInBytes = Word
  --
  nodeBlockFetchSize
    :: Header OddBlock -> SizeInBytes
  -- Here I realized we need to incorporate the block size in the header.
  nodeBlockFetchSize = oddBlockSize . oddBlockSignedPart

  nodeIsEBB = const Nothing

  nodeEpochSize
    :: Monad m
    => Proxy OddBlock
    -> NodeConfig (ExtConfig (Bft BftMockCrypto) (LedgerConfig OddBlock))
    -> EpochNo
    -> m EpochSize
  nodeEpochSize _  cfg _ = pure $ EpochSize $ slotsPerEpoch $ extNodeConfig cfg

  -- The node start time determines when the __network__ started. This is used
  -- in combination with the blockchain time and slot duration to determine the
  -- current slot.
  nodeStartTime _ cfg = SystemStart $ cfgNodeStartTime $ extNodeConfig cfg

  -- Here I realized I should be able to extract a 'NetworkMagic' from the
  -- configuration. For this simple example we won't be running different
  -- networks, so we can simply return a constant. However, even though the node
  -- config is unaware of the 'RunNode', it seems that this typeclass
  -- constraints quite heavily what run node should contain ...
  nodeNetworkMagic _ _ = NetworkMagic 0

  nodeProtocolMagicId _ _ = ProtocolMagicId 0

  nodeHashInfo :: Proxy OddBlock -> HashInfo (Hash (Header OddBlock))
  --
  -- This means we need to return:
  --
  -- > data HashInfo hash = HashInfo
  -- > { hashSize :: !Word32
  -- >  -- ^ A fixed size
  -- > , getHash  :: !(Get hash)
  -- > , putHash  :: !(hash -> Put)
  -- > }
  --
  --
  nodeHashInfo _ =
    HashInfo
    -- NOTE: for some strange reason the 'byteCount' is supposed to return a
    -- Natural, instead of a 'Word'. I don't think we'd need more than 2^32
    -- bytes for encoding hashes, since that's about 4GB.
    { hashSize = hashSize'
    , getHash =
        Crypto.Hash.UnsafeHash <$> Get.getByteString (fromIntegral hashSize')
    , putHash = \h -> Put.putByteString (Crypto.Hash.getHash h)
    }
    where
      hashSize' = fromIntegral
                $ Cardano.Crypto.Hash.Class.byteCount (Proxy :: Proxy ShortHash)
  -- See:
  --
  --
  -- > byronHashInfo :: HashInfo ByronHash
  -- > byronHashInfo = HashInfo { hashSize, getHash, putHash }
  -- >   where
  -- >     hashSize :: Word32
  -- >     hashSize = fromIntegral $ CC.hashDigestSize' @Crypto.Blake2b_256
  -- >
  -- >     getHash :: Get ByronHash
  -- >     getHash = do
  -- >       bytes <- Get.getByteString (fromIntegral hashSize)
  -- >       case Crypto.digestFromByteString bytes of
  -- >         Nothing     -> fail "digestFromByteString failed"
  -- >         Just digest -> return $ ByronHash $ CC.AbstractHash digest
  -- >
  -- >     putHash :: ByronHash -> Put
  -- >     putHash (ByronHash (CC.AbstractHash digest)) =
  -- >       Put.putByteString $ ByteArray.convert digest
  -- >
  --
  -- in @module Ouroboros.Consensus.Ledger.Byron.Block@

  -- We don't have a variable block size for now. We use 1M blocks
  nodeMaxBlockSize  = const $ 2^(20 :: Int)

  -- We set this to more than twice Byron's overhead. Just to be on the safe
  -- side.
  nodeBlockEncodingOverhead = const 2048

  nodeCheckIntegrity
    :: NodeConfig (ExtConfig (Bft BftMockCrypto) (LedgerConfig OddBlock))
    -> OddBlock
    -> Bool
  -- Here I realized that to check the signature I need to have the issuer's
  -- vkey in the header.
  nodeCheckIntegrity _ blk
    =  -- Payload hash matches
       oddBlockPayloadHash signedPart == hash (oddBlockPayload blk)
       -- Block signature is correct
    && isRight (verifyDSIGN () vkey signedPart (oddBlockSignature hdr))
    where
      signedPart = oddBlockSignedPart hdr
      hdr        = oddBlockHeader blk
      vkey       = oddBlockIssuer signedPart

  ------------------------------------------------------------------------------
  -- Encoders
  ------------------------------------------------------------------------------

  -- From Edsko: the immutable database stores blocks as binary blobs when we
  -- are streaming them to clients we want to avoid deserializating those
  -- blocks. Only to serialize them again when we put them on the wire and so we
  -- just stream directly from disk. For the blocks that is easy, but we also
  -- want to stream headers in the same way and so ni order to do that, we need
  -- to know, given a block, what we need to "cut out "of that block to send
  -- over.
  nodeEncodeBlockWithInfo
    :: NodeConfig (BlockProtocol OddBlock)
    -> OddBlock
    -> BinaryInfo Encoding
  -- QUESTION: are there functions to help getting this right?
  nodeEncodeBlockWithInfo _ blk =
    BinaryInfo
    { binaryBlob   = toCBOR blk
    , headerOffset = 1 -- CBOR tag
                   + 1 -- 'encodeListLen' of the block
      -- NOTE: here we should check/assert that the length fits in a 'Word16'.
    , headerSize   = fromIntegral $ Strict.length $ serialize' $ getHeader blk
    }

  nodeEncodeHeader _ _ = toCBOR

  nodeEncodeWrappedHeader _ _ = encode

  nodeEncodeGenTx = toCBOR

  nodeEncodeGenTxId = toCBOR

  nodeEncodeHeaderHash _ = toCBOR

  nodeEncodeLedgerState _ = toCBOR

  nodeEncodeChainState _ _ = toCBOR

  nodeEncodeApplyTxError _ = toCBOR

  nodeEncodeTipInfo _ = toCBOR

  nodeEncodeQuery = \case {}

  nodeEncodeResult = \case {}

  ------------------------------------------------------------------------------
  -- Decoders
  ------------------------------------------------------------------------------

  nodeDecodeHeader _ _ = const <$> fromCBOR

  -- This is actually doing CBOR-in-CBOR for you. Reason for doing that: binary
  -- streaming of headers, i.e., server headers to nodes without deserialising
  -- and then reserialising them
  nodeDecodeWrappedHeader   = \_ _ -> decode

  nodeDecodeBlock _ = const <$> fromCBOR

  nodeDecodeGenTx = fromCBOR

  nodeDecodeGenTxId = fromCBOR

  nodeDecodeHeaderHash _ = fromCBOR

  nodeDecodeLedgerState _ = fromCBOR

  nodeDecodeChainState _ _ = fromCBOR

  nodeDecodeApplyTxError _ = fromCBOR

  nodeDecodeTipInfo _ = fromCBOR

  nodeDecodeQuery = error "OddChain.nodeDecodeQuery"

  nodeDecodeResult = \case {}


mkSignedPart
  :: VerKeyDSIGN MockDSIGN
  -> ChainHash OddBlock
  -> BlockNo
  -> SlotNo
  -> [Tx]
  -> SignedPart
mkSignedPart nodeVKey prevHash aBlockNo slotNo payload
  = SignedPart
    { oddBlockIssuer      = nodeVKey
    , oddBlockPrevHash    = prevHash
    , oddBlockNo          = aBlockNo
    , oddBlockSlot        = slotNo
    , oddBlockPayloadHash = hash payload
      -- We give an upper bound estimate of the block size
      --
      -- NOTE: encoding and computing the number of bytes would
      -- be a less error prone way of doing this. For the
      -- purposes of our prototype this method should suffice.
    , oddBlockSize        = tag 32 -- Prev hash
                          + tag 4  -- Block number
                          + tag 4  -- Slot number
                          + tag 4  -- Payload hash
                          + tag 4  -- Odd Block size
                          + tag (32 + 64) -- Signature
                          + tag (tag 4 * fromIntegral (length payload))
    }
  where
    tag x      = 1 + x -- Account for the CBOR tag when computing the upper
                       -- bound on the block size.

-- We need to define the OddChain protocol, with all the data that is needed to
-- run it. This is done in
--
-- > module Ouroboros.Consensus.Protocol
--
-- Once this is defined, we need a way to translate this protocol value to a
-- 'ProtocolInfo' value, which contains the data required to run a protocol:
--
-- > module Ouroboros.Consensus.Node.ProtocolInfo.Abstract
-- > ...
-- > data ProtocolInfo b = ProtocolInfo {
-- >        pInfoConfig     :: NodeConfig (BlockProtocol b)
-- >      , pInfoInitState  :: NodeState  (BlockProtocol b)
-- >        -- | The ledger state at genesis
-- >      , pInfoInitLedger :: ExtLedgerState b
-- >      }
--
--
-- The translation from Protocol values to ProtocolInfo happens in:
--
-- > module Ouroboros.Consensus.Node.ProtocolInfo
-- > ...
-- > protocolInfo :: Protocol blk -> ProtocolInfo blk
--
-- The extensions of 'Protocol' and 'protocolInfo' to support the OddChain are
-- defined in the modules where these symbols are declared, otherwise we have a
-- circular dependency. This is just a temporary solution while the refactoring
-- is on its way.
--

instance Condense OddBlock where
  condense = show

--------------------------------------------------------------------------------
-- Testing the odd chain ...
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ToCBOR instances
--------------------------------------------------------------------------------

instance ToCBOR (Header OddBlock) where
  toCBOR (OddHeader
            oddBlockSignedPart
            oddBlockSignature
         )
    = encodeListLen 2
    <> toCBOR oddBlockSignedPart
    <> toCBOR oddBlockSignature

instance ToCBOR SignedPart where
  toCBOR (SignedPart
            oddBlockIssuer
            oddBlockPrevHash
            oddBlockNo
            oddBlockSlot
            oddBlockPayloadHash
            oddBlockSize
         )
    = encodeListLen 6
    <> toCBOR oddBlockIssuer
    <> toCBOR oddBlockPrevHash
    <> toCBOR oddBlockNo
    <> toCBOR oddBlockSlot
    <> toCBOR oddBlockPayloadHash
    <> toCBOR oddBlockSize

instance ToCBOR (ChainHash OddBlock) where
  toCBOR GenesisHash   = encodeListLen 0
  toCBOR (BlockHash h) = encodeListLen 1 <> toCBOR h

instance ToCBOR OddBlock where
  toCBOR (OddBlock aHeader aPayload)
    =  encodeListLen 2
    <> toCBOR aHeader
    <> toCBOR aPayload

instance ToCBOR (LedgerState OddBlock) where
  toCBOR (LedgerState aCurrentSlot aHash aPhase)
    =  encodeListLen 3
    <> toCBOR aCurrentSlot
    <> toCBOR aHash
    <> toCBOR aPhase

instance ToCBOR Phase where
  toCBOR (Decrease i) = encodeListLen 2 <> encodeWord 0 <> toCBOR i
  toCBOR (Increase i) = encodeListLen 2 <> encodeWord 1 <> toCBOR i

instance ToCBOR OddTxError where
  toCBOR (NotOdd mobe i) =
    encodeListLen 3 <> encodeWord 0 <> toCBOR mobe <> toCBOR i
  toCBOR (OddBut obe i) =
    encodeListLen 3 <> encodeWord 1 <> toCBOR obe <> toCBOR i

instance ToCBOR OutOfBoundError where
  toCBOR = encodeInt . fromEnum

--------------------------------------------------------------------------------
-- FromCBOR instances
--------------------------------------------------------------------------------

instance FromCBOR (Header OddBlock) where
  fromCBOR = do
    D.decodeListLenOf 2
    !theSignedPart <- fromCBOR
    !theSignature  <- fromCBOR
    pure $! OddHeader
            { oddBlockSignedPart = theSignedPart
            , oddBlockSignature = theSignature
            }

instance FromCBOR SignedPart where
  fromCBOR = do
    D.decodeListLenOf 6
    !theIssuer      <- fromCBOR
    !thePrevHash    <- fromCBOR
    !theBlockNo     <- fromCBOR
    !theBlockSlot   <- fromCBOR
    !thePayloadHash <- fromCBOR
    !theBlockSize   <- fromCBOR
    pure $! SignedPart
            { oddBlockIssuer      = theIssuer
            , oddBlockPrevHash    = thePrevHash
            , oddBlockNo          = theBlockNo
            , oddBlockSlot        = theBlockSlot
            , oddBlockPayloadHash = thePayloadHash
            , oddBlockSize        = theBlockSize
            }

instance FromCBOR (ChainHash OddBlock) where
  fromCBOR = do
    n <- D.decodeListLen
    case n of
      0 -> pure $! GenesisHash
      1 -> do
        !h <- fromCBOR
        pure $! BlockHash h
      _ -> cborError $ DecoderErrorUnknownTag "ChainHash" (fromIntegral n)

instance FromCBOR OddBlock where
  fromCBOR = do
    D.decodeListLenOf 2
    !theHeader  <- fromCBOR
    !thePayload <- fromCBOR
    pure $! OddBlock
            { oddBlockHeader  = theHeader
            , oddBlockPayload = thePayload
            }

instance FromCBOR (LedgerState OddBlock) where
  fromCBOR = do
    D.decodeListLenOf 3
    !aSlot  <- fromCBOR
    !aHash  <- fromCBOR
    !aPhase <- fromCBOR
    pure $! LedgerState
            { stCurrentSlot = aSlot
            , stPrevHash    = aHash
            , phase         = aPhase
            }

instance FromCBOR Phase where
  fromCBOR = do
    D.decodeListLenOf 2
    t <- D.decodeWord
    case t of
      0 -> do
        !i <- fromCBOR
        pure $! Decrease i
      1 -> do
        !i <- fromCBOR
        pure $! Increase i
      _ -> cborError $ DecoderErrorUnknownTag "Phase" (fromIntegral t)

instance FromCBOR OddTxError where
  fromCBOR = do
    D.decodeListLenOf 3
    t <- D.decodeWord
    case t of
      0 -> do
        !mobe <- fromCBOR
        !i    <- fromCBOR
        pure $! NotOdd mobe i
      1 -> do
        !obe <- fromCBOR
        !i   <- fromCBOR
        pure $! OddBut obe i
      _ -> cborError $ DecoderErrorUnknownTag "OddTxError" (fromIntegral t)

instance FromCBOR OutOfBoundError where
  fromCBOR = do
    !i <- D.decodeInt
    pure $! toEnum i
