{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Header validation
module Ouroboros.Consensus.HeaderValidation (
    validateHeader
    -- * Annotated tips
  , AnnTip(..)
  , annTipPoint
  , castAnnTip
  , HasAnnTip(..)
  , getAnnTip
    -- * Header state
  , HeaderState(..)
  , headerStateTip
  , headerStatePush
  , genesisHeaderState
  , castHeaderState
  , rewindHeaderState
    -- * Validate header envelope
  , HeaderEnvelopeError(..)
  , castHeaderEnvelopeError
  , ValidateEnvelope(..)
  , defaultValidateEnvelope
    -- * Errors
  , HeaderError(..)
  , castHeaderError
    -- * Serialization
  , encodeAnnTip
  , decodeAnnTip
  , encodeHeaderState
  , decodeHeaderState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise (Serialise, decode, encode)
import           Control.Monad.Except
import           Data.Foldable (toList)
import           Data.Proxy
import           Data.Sequence.Strict (StrictSeq ((:<|), (:|>), Empty))
import qualified Data.Sequence.Strict as Seq
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           GHC.Generics (Generic)

import           Cardano.Binary (enforceSize)
import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot (WithOrigin (..))

import           Ouroboros.Network.Block hiding (Tip (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR

{-------------------------------------------------------------------------------
  Preliminary: annotated tip
-------------------------------------------------------------------------------}

-- | Annotated information about the tip of the chain
--
-- The annotation is the additional information we need to validate the
-- header envelope. Under normal circumstances no additional information is
-- required, but for instance for Byron we need to know if the previous header
-- was an EBB.
data AnnTip blk = AnnTip {
      annTipSlotNo  :: !SlotNo
    , annTipHash    :: !(HeaderHash blk)
    , annTipBlockNo :: !BlockNo
    , annTipInfo    :: !(TipInfo blk)
    }
  deriving (Generic)

deriving instance HasAnnTip blk => Show               (AnnTip blk)
deriving instance HasAnnTip blk => Eq                 (AnnTip blk)
deriving instance HasAnnTip blk => NoUnexpectedThunks (AnnTip blk)

annTipPoint :: AnnTip blk -> Point blk
annTipPoint AnnTip{..} = BlockPoint annTipSlotNo annTipHash

castAnnTip :: ( HeaderHash blk ~ HeaderHash blk'
              , TipInfo    blk ~ TipInfo    blk'
              )
           => AnnTip blk -> AnnTip blk'
castAnnTip AnnTip{..} = AnnTip{..}

class ( StandardHash blk
      , Show               (TipInfo blk)
      , Eq                 (TipInfo blk)
      , NoUnexpectedThunks (TipInfo blk)
      ) => HasAnnTip blk where
  type TipInfo blk :: *
  type TipInfo blk = ()

  getTipInfo :: Header blk -> TipInfo blk

  default getTipInfo :: TipInfo blk ~ () => Header blk -> TipInfo blk
  getTipInfo _ = ()

getAnnTip :: (HasHeader (Header blk), HasAnnTip blk)
          => Header blk -> AnnTip blk
getAnnTip hdr = AnnTip {
      annTipSlotNo  = blockSlot  hdr
    , annTipHash    = blockHash  hdr
    , annTipBlockNo = blockNo    hdr
    , annTipInfo    = getTipInfo hdr
    }

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

-- | State required to validate the header
--
-- See 'validateHeader' for details
data HeaderState blk = HeaderState {
      -- | Protocol-specific state
      headerStateConsensus :: !(ConsensusState (BlockProtocol blk))

      -- | The most recent @k@ tips
    , headerStateTips      :: !(StrictSeq (AnnTip blk))

      -- | Tip /before/ 'headerStateTips'
    , headerStateAnchor    :: !(WithOrigin (AnnTip blk))
    }
  deriving (Generic)

headerStateTip :: HeaderState blk -> WithOrigin (AnnTip blk)
headerStateTip HeaderState{..} =
    case headerStateTips of
      Empty     -> headerStateAnchor
      _ :|> tip -> At tip

headerStatePush :: forall blk.
                   SecurityParam
                -> ConsensusState (BlockProtocol blk)
                -> AnnTip blk
                -> HeaderState blk
                -> HeaderState blk
headerStatePush (SecurityParam k) state newTip HeaderState{..} =
    case trim pushed of
      Nothing                   -> HeaderState state pushed  headerStateAnchor
      Just (newAnchor, trimmed) -> HeaderState state trimmed (At newAnchor)
  where
    pushed :: StrictSeq (AnnTip blk)
    pushed = headerStateTips :|> newTip

    trim :: StrictSeq (AnnTip blk) -> Maybe (AnnTip blk, StrictSeq (AnnTip blk))
    trim (newAnchor :<| trimmed) | Seq.length trimmed >= fromIntegral k =
        Just (newAnchor, trimmed)
    trim _otherwise = Nothing

deriving instance (BlockSupportsProtocol blk, HasAnnTip blk)
                => Show (HeaderState blk)
deriving instance (BlockSupportsProtocol blk, HasAnnTip blk)
                => NoUnexpectedThunks (HeaderState blk)
deriving instance ( BlockSupportsProtocol blk
                  , HasAnnTip blk
                  , Eq (ConsensusState (BlockProtocol blk))
                  ) => Eq (HeaderState blk)

genesisHeaderState :: ConsensusState (BlockProtocol blk) -> HeaderState blk
genesisHeaderState state = HeaderState state Seq.Empty Origin

castHeaderState :: (   ConsensusState (BlockProtocol blk )
                     ~ ConsensusState (BlockProtocol blk')
                   ,   HeaderHash blk
                     ~ HeaderHash blk'
                   ,   TipInfo blk
                     ~ TipInfo blk'
                   )
                => HeaderState blk -> HeaderState blk'
castHeaderState HeaderState{..} = HeaderState{
      headerStateConsensus = headerStateConsensus
    , headerStateTips      = castSeq castAnnTip $ headerStateTips
    , headerStateAnchor    = fmap    castAnnTip $ headerStateAnchor
    }
  where
    -- This is unfortunate. We're doing busy-work on a strict-sequence,
    -- mapping a function that actually doesn't change anything :/
    castSeq :: (a -> b) -> StrictSeq a -> StrictSeq b
    castSeq f = Seq.fromList . map f . toList

-- | Rewind the header state
--
-- This involves 'rewindChainState', and so inherits its PRECONDITION that the
-- target point must have been previously applied.
--
rewindHeaderState :: forall blk.
                     ( BlockSupportsProtocol blk
                     , Serialise (HeaderHash blk)
                     )
                  => TopLevelConfig blk
                  -> Point blk
                  -> HeaderState blk -> Maybe (HeaderState blk)
rewindHeaderState cfg p HeaderState{..} = do
    consensusState' <- rewindConsensusState
                         (configConsensus cfg)
                         headerStateConsensus
                         p
    return $ HeaderState {
        headerStateConsensus = consensusState'
      , headerStateTips      = Seq.dropWhileR rolledBack headerStateTips
      , headerStateAnchor    = headerStateAnchor
      }
  where
    -- the precondition ensures that @p@ is either in the old 'headerStateTips'
    -- or the new 'headerStateTips' should indeed be empty
    rolledBack :: AnnTip blk -> Bool
    rolledBack t = annTipPoint t /= p

{-------------------------------------------------------------------------------
  Validate header envelope
-------------------------------------------------------------------------------}

data HeaderEnvelopeError blk =
    -- | Invalid block number
    --
    -- We record both the expected and actual block number
    UnexpectedBlockNo !BlockNo !BlockNo

    -- | Invalid slot number
    --
    -- We record both the expected (minimum) and actual slot number
  | UnexpectedSlotNo !SlotNo !SlotNo

    -- | Invalid hash (in the reference to the previous block)
    --
    -- We record both the expected and actual hash
  | UnexpectedPrevHash !(ChainHash blk) !(ChainHash blk)

    -- | Block specific envelope error
  | OtherHeaderEnvelopeError !(OtherHeaderEnvelopeError blk)
  deriving (Generic)

deriving instance (ValidateEnvelope blk) => Eq   (HeaderEnvelopeError blk)
deriving instance (ValidateEnvelope blk) => Show (HeaderEnvelopeError blk)
deriving instance (ValidateEnvelope blk, Typeable blk)
               => NoUnexpectedThunks (HeaderEnvelopeError blk)

castHeaderEnvelopeError :: ( HeaderHash blk ~ HeaderHash blk'
                           , OtherHeaderEnvelopeError blk ~ OtherHeaderEnvelopeError blk'
                           )
                        => HeaderEnvelopeError blk -> HeaderEnvelopeError blk'
castHeaderEnvelopeError = \case
    OtherHeaderEnvelopeError err       -> OtherHeaderEnvelopeError err
    UnexpectedBlockNo  expected actual -> UnexpectedBlockNo  expected  actual
    UnexpectedSlotNo   expected actual -> UnexpectedSlotNo   expected  actual
    UnexpectedPrevHash expected actual -> UnexpectedPrevHash expected' actual'
      where
        expected' = castHash expected
        actual'   = castHash actual

-- | Validate header envelope (block, slot, hash)
class ( HasAnnTip blk
      , Eq                 (OtherHeaderEnvelopeError blk)
      , Show               (OtherHeaderEnvelopeError blk)
      , NoUnexpectedThunks (OtherHeaderEnvelopeError blk))
   => ValidateEnvelope blk where

  -- | A block-specific error that 'validateEnvelope' can return.
  type OtherHeaderEnvelopeError blk :: *
  type OtherHeaderEnvelopeError blk = Void

  -- | Validate the header envelope
  validateEnvelope :: TopLevelConfig blk
                   -> LedgerView (BlockProtocol blk)
                   -> WithOrigin (AnnTip blk)
                   -> Header blk
                   -> Except (HeaderEnvelopeError blk) ()

  -- | The block number of the first block on the chain
  firstBlockNo :: Proxy blk -> BlockNo
  firstBlockNo _ = BlockNo 0

  -- | The smallest possible 'SlotNo'
  --
  -- NOTE: This does not affect the translation between 'SlotNo' and 'EpochNo'.
  -- "Ouroboros.Consensus.HardFork.History" for details.
  minimumPossibleSlotNo :: Proxy blk -> SlotNo
  minimumPossibleSlotNo _ = SlotNo 0

  default validateEnvelope :: HasHeader (Header blk)
                           => TopLevelConfig blk
                           -> LedgerView (BlockProtocol blk)
                           -> WithOrigin (AnnTip blk)
                           -> Header blk
                           -> Except (HeaderEnvelopeError blk) ()
  validateEnvelope _cfg _ledgerView = defaultValidateEnvelope

-- | Default implementation for 'validateEnvelope'.
--
-- Is provided as a separate function to make it easier to extend the
-- 'validateEnvelope' implemention with checks on top of the default ones.
--
-- Using the other members of 'ValidateEnvelope', it checks:
--
-- * whether block numbers are /strictly increasing/, starting from
--  'firstBlockNo'.
-- * whether slot number are /increasing/, starting from
--  'minimumPossibleSlotNo'.
-- * whether the previous hash matches the hash of the previous header
defaultValidateEnvelope
  :: forall blk. (HasHeader (Header blk), ValidateEnvelope blk)
  => WithOrigin (AnnTip blk)
  -> Header blk
  -> Except (HeaderEnvelopeError blk) ()
defaultValidateEnvelope oldTip hdr = do
    when (actualBlockNo /= expectedBlockNo) $
      throwError $ UnexpectedBlockNo expectedBlockNo actualBlockNo
    when (actualSlotNo < expectedSlotNo) $
      throwError $ UnexpectedSlotNo expectedSlotNo actualSlotNo
    when (actualPrevHash /= expectedPrevHash) $
      throwError $ UnexpectedPrevHash expectedPrevHash actualPrevHash
  where
    actualSlotNo   :: SlotNo
    actualBlockNo  :: BlockNo
    actualPrevHash :: ChainHash blk

    actualSlotNo   =            blockSlot     hdr
    actualBlockNo  =            blockNo       hdr
    actualPrevHash = castHash $ blockPrevHash hdr

    expectedSlotNo   :: SlotNo           -- Lower bound only
    expectedBlockNo  :: BlockNo
    expectedPrevHash :: ChainHash blk

    (expectedSlotNo, expectedBlockNo, expectedPrevHash) =
        case oldTip of
          At (AnnTip s h b _) -> ( succ s, succ b, BlockHash h )
          Origin              -> ( minimumPossibleSlotNo proxy
                                 , firstBlockNo          proxy
                                 , GenesisHash
                                 )
      where
        proxy = Proxy @blk


{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Invalid header
data HeaderError blk =
    -- | Invalid consensus protocol fields
    HeaderProtocolError !(ValidationErr (BlockProtocol blk))

    -- | Failed to validate the envelope
  | HeaderEnvelopeError !(HeaderEnvelopeError blk)
  deriving (Generic)

deriving instance (BlockSupportsProtocol blk, ValidateEnvelope blk)
               => Eq                 (HeaderError blk)
deriving instance (BlockSupportsProtocol blk, ValidateEnvelope blk)
               => Show               (HeaderError blk)
deriving instance (BlockSupportsProtocol blk, ValidateEnvelope blk)
               => NoUnexpectedThunks (HeaderError blk)

castHeaderError :: (   ValidationErr (BlockProtocol blk )
                     ~ ValidationErr (BlockProtocol blk')
                   ,   HeaderHash blk
                     ~ HeaderHash blk'
                   ,   OtherHeaderEnvelopeError blk
                     ~ OtherHeaderEnvelopeError blk'
                   )
                => HeaderError blk -> HeaderError blk'
castHeaderError (HeaderProtocolError e) = HeaderProtocolError e
castHeaderError (HeaderEnvelopeError e) = HeaderEnvelopeError $
                                            castHeaderEnvelopeError e

{-------------------------------------------------------------------------------
  Validation proper
-------------------------------------------------------------------------------}

-- | Header validation
--
-- Header validation (as opposed to block validation) is done by the chain sync
-- client: as we download headers from other network nodes, we validate those
-- headers before deciding whether or not to download the corresponding blocks.
--
-- Before we /adopt/ any blocks we have downloaded, however, we will do a full
-- block validation. As such, the header validation check can omit some checks
-- (provided that we do those checks when we do the full validation); at worst,
-- this would mean we might download some blocks that we will reject as being
-- invalid where we could have detected that sooner.
--
-- For this reason, the header validation currently only checks two things:
--
-- o It verifies the consensus part of the header.
--
--   For example, for Praos this means checking the VRF proofs.
--
-- o It verifies the 'HasHeader' part of the header.
--
--   By default, we verify that
--
--   x Block numbers are consecutive
--   x The block number of the first block is 'firstBlockNo'
--   x Slot numbers are strictly increasing
--   x The slot number of the first block is at least 'minimumPossibleSlotNo'
--   x Hashes line up
--
-- /If/ a particular ledger wants to verify additional fields in the header,
-- it will get the chance to do so in 'applyLedgerBlock', which is passed the
-- entire block (not just the block body).
validateHeader :: (BlockSupportsProtocol blk, ValidateEnvelope blk)
               => TopLevelConfig blk
               -> LedgerView (BlockProtocol blk)
               -> Header blk
               -> HeaderState blk
               -> Except (HeaderError blk) (HeaderState blk)
validateHeader cfg ledgerView hdr st = do
    withExcept HeaderEnvelopeError $
      validateEnvelope cfg ledgerView (headerStateTip st) hdr
    consensusState' <- withExcept HeaderProtocolError $
                         updateConsensusState
                           (configConsensus cfg)
                           ledgerView
                           (validateView (configBlock cfg) hdr)
                           (headerStateConsensus st)
    return $ headerStatePush
               (configSecurityParam cfg)
               consensusState'
               (getAnnTip hdr)
               st

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeAnnTip :: (HeaderHash blk -> Encoding)
             -> (TipInfo    blk -> Encoding)
             -> (AnnTip     blk -> Encoding)
encodeAnnTip encodeHash encodeInfo AnnTip{..} = mconcat [
      encodeListLen 4
    , encode     annTipSlotNo
    , encodeHash annTipHash
    , encode     annTipBlockNo
    , encodeInfo annTipInfo
    ]

decodeAnnTip :: (forall s. Decoder s (HeaderHash blk))
             -> (forall s. Decoder s (TipInfo    blk))
             -> (forall s. Decoder s (AnnTip     blk))
decodeAnnTip decodeHash decodeInfo = do
    enforceSize "AnnTip" 4
    annTipSlotNo  <- decode
    annTipHash    <- decodeHash
    annTipBlockNo <- decode
    annTipInfo    <- decodeInfo
    return AnnTip{..}

encodeHeaderState :: (ConsensusState (BlockProtocol blk) -> Encoding)
                  -> (HeaderHash  blk -> Encoding)
                  -> (TipInfo     blk -> Encoding)
                  -> (HeaderState blk -> Encoding)
encodeHeaderState encodeConsensusState
                  encodeHash
                  encodeInfo
                  HeaderState{..} = mconcat [
      encodeListLen 3
    , encodeConsensusState headerStateConsensus
    , Util.CBOR.encodeSeq        encodeAnnTip' headerStateTips
    , Util.CBOR.encodeWithOrigin encodeAnnTip' headerStateAnchor
    ]
  where
    encodeAnnTip' = encodeAnnTip encodeHash encodeInfo

decodeHeaderState :: (forall s. Decoder s (ConsensusState (BlockProtocol blk)))
                  -> (forall s. Decoder s (HeaderHash  blk))
                  -> (forall s. Decoder s (TipInfo     blk))
                  -> (forall s. Decoder s (HeaderState blk))
decodeHeaderState decodeConsensusState decodeHash decodeInfo = do
    enforceSize "HeaderState" 3
    headerStateConsensus <- decodeConsensusState
    headerStateTips      <- Util.CBOR.decodeSeq        decodeAnnTip'
    headerStateAnchor    <- Util.CBOR.decodeWithOrigin decodeAnnTip'
    return HeaderState{..}
  where
    decodeAnnTip' = decodeAnnTip decodeHash decodeInfo
