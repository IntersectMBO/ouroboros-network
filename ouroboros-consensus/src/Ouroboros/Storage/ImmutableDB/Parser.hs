{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Ouroboros.Storage.ImmutableDB.Parser
  ( -- * EpochFileParser
    EpochFileError (..)
  , epochFileParser
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash)

import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API (HasFS)

import           Ouroboros.Storage.ImmutableDB.Types


{-------------------------------------------------------------------------------
  EpochFileParser
-------------------------------------------------------------------------------}

data EpochFileError =
    EpochErrRead Util.CBOR.ReadIncrementalErr
  | EpochErrUnexpectedEBB
  deriving (Eq, Show)

epochFileParser :: forall m blk h. (IOLike m, HasHeader blk)
                => HasFS m h
                -> (forall s. Decoder s (BL.ByteString -> blk))
                -> (blk -> Maybe (HeaderHash blk))
                -> EpochFileParser
                     EpochFileError
                     (HeaderHash blk)
                     m
                     (Word64, SlotNo)
epochFileParser hasFS decodeBlock isEBB =
    EpochFileParser $
        fmap (processEpochs (Proxy @blk))
      . Util.CBOR.readIncrementalOffsets hasFS decoder'
  where
    -- It is important that we don't first parse all blocks, storing them all
    -- in memory, and only /then/ extract the information we need.
    decoder' :: forall s. Decoder s (BL.ByteString -> (SlotNo, CurrentEBB (HeaderHash blk)))
    decoder' = (extractSlotNoAndEbbHash .) <$> decodeBlock

    extractSlotNoAndEbbHash :: blk -> (SlotNo, CurrentEBB (HeaderHash blk))
    extractSlotNoAndEbbHash b =
      -- IMPORTANT: force the slot and the ebbHash, because otherwise we
      -- return thunks that refer to the whole block! See
      -- 'Ouroboros.Consensus.Util.CBOR.readIncrementalOffsets' where we need
      -- to force the decoded value in order to force this computation.
        case isEBB b of
          Nothing         -> (slot, NoCurrentEBB)
          Just (!ebbHash) -> (slot, CurrentEBB ebbHash)
      where
        !slot = blockSlot b

-- | Verify that there is at most one EBB in the epoch file and that it
-- lives at the start of the file
processEpochs :: forall blk.
                 Proxy blk
              -> ( [(Word64, (Word64, (SlotNo, CurrentEBB (HeaderHash blk))))]
                 , Maybe Util.CBOR.ReadIncrementalErr
                 )
              -> ( [(SlotOffset, (Word64, SlotNo))]
                 , CurrentEBB (HeaderHash blk)
                 , Maybe EpochFileError
                 )
processEpochs _ = \(bs, mErr) ->
    case bs of
      []    -> ([], NoCurrentEBB, EpochErrRead <$> mErr)
      b:bs' -> let (bOff, (bSz, (bSlot, bEBB))) = b
                   (slots, mErr') = go bs'
               in ((bOff, (bSz, bSlot)) : slots, bEBB, earlierError mErr mErr')
  where
    -- Check that the rest of the blocks are not EBBs
    go :: [(Word64, (Word64, (SlotNo, CurrentEBB (HeaderHash blk))))]
       -> ( [(SlotOffset, (Word64, SlotNo))]
          , Maybe EpochFileError
          )
    go []     = ( [], Nothing )
    go (b:bs) = let (bOff, (bSz, (bSlot, bEBB))) = b
                in case bEBB of
                     CurrentEBB _ -> ( [], Just EpochErrUnexpectedEBB )
                     NoCurrentEBB -> first ((bOff, (bSz, bSlot)) :) $ go bs

    -- Report the earlier error
    --
    -- The 'ReadIncrementalError' reported by the parser tells us that there
    -- were blocks /after/ the ones that were returned that failed to parse.
    -- If therefore /we/ find an error in those blocks that were returned, that
    -- error happens /earlier/ in the file.
    earlierError :: Maybe Util.CBOR.ReadIncrementalErr
                 -> Maybe EpochFileError
                 -> Maybe EpochFileError
    earlierError _parserErr (Just ourErr) = Just ourErr
    earlierError  parserErr Nothing       = EpochErrRead <$> parserErr
