{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | PBFT chain state
--
-- Intended for qualified import.
module Ouroboros.Consensus.Protocol.PBFT.ChainState (
    PBftChainState(..)
    -- * Construction
  , empty
  , insert
  , prune
  , rewind
    -- * Queries
  , size
  , countSignedBy
  , lastSlot
    -- * Support for tests
  , fromMap
  ) where

import           Codec.Serialise (Serialise (..))
import qualified Codec.Serialise.Decoding as Serialise
import qualified Codec.Serialise.Encoding as Serialise
import           Data.Foldable (toList)
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (..), withOriginFromMaybe,
                     withOriginToMaybe)

import           Ouroboros.Consensus.Protocol.PBFT.Crypto
import           Ouroboros.Consensus.Util (nTimes, repeatedly)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | PBFT chain state
--
-- The PBFT chain state records for the last @n@ slots who signed those slots
-- (in terms of genesis keys). It is used to check that a single node has not
-- signed more than a certain percentage threshold of the slots (that check
-- is not implemented here, however).
--
-- The window size @n@ can be set pretty much arbitrarily. However, in order for
-- the chain state to be able to roll back at least @k@ blocks, we must have
-- that @n >= k@.
--
-- The performance of this code is not that important during normal node
-- operation, as it will only be used when a new block comes in. However, it
-- can potentially become a bottleneck during syncing.
data PBftChainState c = PBftChainState {
      -- | Anchor
      --
      -- The anchor is the slot number of the block before the first block
      -- in the state.
      --
      -- Suppose we have a window size of 5 (i.e., we 'prune' the state with a
      -- maximum size of 5). Then the state evolves as follows (G = genesis):
      --
      -- > anchor | max rollback | blocks
      -- > --------------------------------------------
      -- > G      | 0 (G)        | []
      -- > G      | 1 (G)        | [B1]
      -- > G      | 2 (G)        | [B1, B2]
      -- > G      | 3 (G)        | [B1, B2, B3]
      -- > G      | 4 (G)        | [B1, B2, B3, B4]
      -- > G      | 4 (B1)       | [B1, B2, B3, B4, B5]
      -- > S1     | 4 (B2)       | [B2, B3, B4, B5, B6]       (S1 slot of B1)
      -- > S2     | 4 (B3)       | [B3, B4, B5, B6, B7]
      --
      -- The table above also shows the maximum rollback, as well as the oldest
      -- block we should be able to roll back to, given @k=4@.
      --
      -- Note what happens when we set the window size to be /equal/ to @k@:
      --
      -- > anchor | max rollback | blocks
      -- > ----------------------------------------
      -- > G      | 0 (G)        | []
      -- > G      | 1 (G)        | [B1]
      -- > G      | 2 (G)        | [B1, B2]
      -- > G      | 3 (G)        | [B1, B2, B3]
      -- > G      | 4 (G)        | [B1, B2, B3, B4]
      -- > S1     | 4 (B1)       | [B2, B3, B4, B5]
      -- > S2     | 4 (B2)       | [B3, B4, B5, B6]
      -- > S3     | 4 (B3)       | [B4, B5, B6, B7]
      --
      -- At this point the anchor becomes equal to the (slot number of) the
      -- oldest block that we can roll back to.
      anchor  :: !(WithOrigin SlotNo)

      -- | Sequence in increasing order of slots and corresponding genesis keys
    , signers :: !(Seq (PBftSigner c))

      -- | Count for each genesis key
      --
      -- Invariant:
      --
      -- > counts == fromSlots signers
      --
      -- We maintain it as an explicit value for improved performance.
    , counts  :: !(Map (PBftVerKeyHash c) Int)
    }
  deriving (Generic)

-- | Slot and corresponding genesis key
data PBftSigner c = PBftSigner {
      pbftSignerSlotNo     :: !SlotNo
    , pbftSignerGenesisKey :: !(PBftVerKeyHash c)
    }
  deriving (Generic)

deriving instance PBftCrypto c => Show (PBftChainState c)
deriving instance PBftCrypto c => Eq   (PBftChainState c)
deriving instance PBftCrypto c => NoUnexpectedThunks (PBftChainState c)

deriving instance PBftCrypto c => Show (PBftSigner c)
deriving instance PBftCrypto c => Eq   (PBftSigner c)
deriving instance PBftCrypto c => NoUnexpectedThunks (PBftSigner c)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Total number of signed slots
--
-- This is in terms of /blocks/, not slots.
--
-- Pruning limits the size:
--
-- > size (prune n st) <= n
-- > size (prune n st) == n  if  size st >= n
size :: PBftChainState c -> Int
size = Seq.length . signers

-- | The number of blocks signed by the specified genesis key
countSignedBy :: PBftCrypto c => PBftChainState c -> PBftVerKeyHash c -> Int
countSignedBy st gk = Map.findWithDefault 0 gk (counts st)

-- | The last (most recent) signed slot in the window
--
-- Any new blocks should be in slots /after/ the 'lastSlot'.
--
-- Returns the anchor if empty.
lastSlot :: PBftChainState c -> WithOrigin SlotNo
lastSlot st =
    case signers st of
      _ :|> PBftSigner slot _ -> At slot
      _otherwise              -> anchor st

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Empty PBFT chain state
--
-- In other words, the PBFT chain state corresponding to genesis.
empty :: PBftChainState c
empty = PBftChainState {
      anchor  = Origin
    , signers = Seq.empty
    , counts  = Map.empty
    }

-- | Insert new signed block into the state
insert :: PBftCrypto c
       => PBftVerKeyHash c -> SlotNo -> PBftChainState c -> PBftChainState c
insert gk slot st = PBftChainState {
      anchor  = anchor st -- anchor doesn't change because first block doesn't
    , signers = signers st |> PBftSigner slot gk
    , counts  = incrementKey gk (counts st)
    }

-- | Internal: drop the oldest slot (if one exists)
--
-- Precondition: state is not empty.
--
-- (Used in 'prune', which establishes the precondition.)
dropOldest :: PBftCrypto c => PBftChainState c -> PBftChainState c
dropOldest st =
    case signers st of
      PBftSigner slot gk :<| signers' ->
        PBftChainState {
            anchor  = At slot
          , signers = signers'
          , counts  = decrementKey gk (counts st)
          }
      _otherwise ->
        error "dropOldest: empty PBftChainState"

-- | Prune to the given maximum size
prune :: forall c. PBftCrypto c => Int -> PBftChainState c -> PBftChainState c
prune maxSize st
  | size st > maxSize = nTimes dropOldest toDrop st
  | otherwise         = st
  where
    toDrop :: Word64
    toDrop = fromIntegral (size st - maxSize)

-- | Rewind the state to the specified slot
--
-- This matches the semantics of 'rewindChainState' in 'OuroborosTag', in that
-- this should be the state at the /end/ of the specified slot (i.e., after the
-- block in that slot, if any, has been applied).
--
-- NOTE: It only makes sense to rewind to a slot containing a block that we have
-- previously applied (the "genesis block" can be understood as having been
-- implicitly applied). This will be implicitly assumed in the properies below.
--
-- = Properties
--
-- * Rewinding to the last applied block is an identity
--
--   > rewind (lastSlot st) st == Just st
--
-- * Rewinding to the same slot should be idempotent:
--
--   > rewind s st == Just st'  ==>  rewind s st' == Just st'
--
-- * Rewinding fails only if the slot is before the anchor
--
--   > rewind s st == Nothing  iff  s < anchor st
--
--   (See documentation of 'anchor' for motivation)
--
-- * If rewinding to a particular slot fails, rewinding to any slot prior to
--   that will also fail
--
--   > rewind s st == Nothing, s' < s  ==>  rewind s' st == Nothing
--
-- * Rewinding to the anchor will leave the state empty
--
--   > size (rewind (anchor s) st) == 0
--
-- * But rewinding to a more recent block will not
--
--   > s > anchor st  ==>  size (rewind s st) > 0
rewind :: PBftCrypto c
       => WithOrigin SlotNo -> PBftChainState c -> Maybe (PBftChainState c)
rewind slot st
  | slot < anchor st = Nothing
  | otherwise = Just $ PBftChainState {
        anchor  = anchor st
      , signers = keep
      -- Typically we only drop a few blocks (and keep most), so updating
      -- the counts based on what we drop will in most cases be faster than
      -- recomputing it from the blocks we keep.
      , counts  = repeatedly (decrementKey . pbftSignerGenesisKey)
                             (toList discard)
                             (counts st)
      }
  where
    (keep, discard) = Seq.spanl (\(PBftSigner slot' _) -> At slot' <= slot) (signers st)

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

fromSlots :: Ord (PBftVerKeyHash c)
          => Seq (PBftSigner c) -> Map (PBftVerKeyHash c) Int
fromSlots slots = repeatedly (incrementKey . pbftSignerGenesisKey)
                             (toList slots)
                             Map.empty

incrementKey :: Ord gk => gk -> Map gk Int -> Map gk Int
incrementKey = Map.alter inc
  where
    inc :: Maybe Int -> Maybe Int
    inc Nothing  = Just 1
    inc (Just n) = Just (n + 1)

decrementKey :: Ord gk => gk -> Map gk Int -> Map gk Int
decrementKey = Map.alter dec
  where
    dec :: Maybe Int -> Maybe Int
    dec Nothing  = error "decrementKey: key does not exist"
    dec (Just 1) = Nothing
    dec (Just n) = Just (n - 1)

{-------------------------------------------------------------------------------
  Support for tests
-------------------------------------------------------------------------------}

-- | Construct PBFT chain state from map listing signed slots per key
fromMap :: forall c. PBftCrypto c
        => WithOrigin SlotNo -- ^ Anchor
        -> Map (PBftVerKeyHash c) [SlotNo]
        -> PBftChainState c
fromMap anchor perKey = PBftChainState {
      anchor  = anchor
    , signers = perSlot
    , counts  = fromSlots perSlot
    }
  where
    perSlot :: Seq (PBftSigner c)
    perSlot = Seq.fromList
            . sortOn pbftSignerSlotNo
            . distrib
            . Map.toList
            $ perKey

    distrib :: [(PBftVerKeyHash c, [SlotNo])] -> [PBftSigner c]
    distrib = concatMap $ \(k, ss) -> map (\s -> PBftSigner s k) ss

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

-- | Don't require PBftCrypto (we don't need the 'Given' constraint here)
instance ( Ord       (PBftVerKeyHash c)
         , Serialise (PBftVerKeyHash c)
         ) => Serialise (PBftChainState c) where
  encode PBftChainState{..} = mconcat [
        Serialise.encodeListLen 2
      , encode (withOriginToMaybe anchor)
      , encode signers
      ]

  decode = do
      Serialise.decodeListLenOf 2
      anchor  <- withOriginFromMaybe <$> decode
      signers <- decode
      let counts = fromSlots signers
      return $ PBftChainState{..}

instance Serialise (PBftVerKeyHash c) => Serialise (PBftSigner c) where
  encode = encode . toPair
    where
      toPair (PBftSigner{..}) = (pbftSignerSlotNo, pbftSignerGenesisKey)

  decode = fromPair <$> decode
    where
      fromPair (slotNo, genesisKey) = PBftSigner slotNo genesisKey

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- TODO: This will be obsolete once we have a strict Seq type
(|>) :: Seq a -> a -> Seq a
(|>) xs !x = xs Seq.|> x
