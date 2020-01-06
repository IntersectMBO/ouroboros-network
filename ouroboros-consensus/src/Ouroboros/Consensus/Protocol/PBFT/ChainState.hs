{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | PBFT chain state
--
-- Intended for qualified import.
module Ouroboros.Consensus.Protocol.PBFT.ChainState (
    PBftChainState(..)
  , WindowSize(..)
    -- * Construction
  , empty
  , append
  , appendEBB
  , rewind
    -- * Queries
  , countSignatures
  , countInWindow
  , countSignedBy
  , lastSignedSlot
    -- * Support for tests
  , MaybeEbbInfo (..)
  , EbbInfo (..)
  , PBftSigner(..)
  , invariant
  , fromList
  , toList
    -- ** Serialization
  , encodePBftChainState
  , decodePBftChainState
  ) where

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Serialise
import           Codec.Serialise.Encoding (Encoding)
import qualified Codec.Serialise.Encoding as Serialise
import qualified Control.Exception as Exn
import           Control.Monad.Except
import qualified Data.Foldable as Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence.Strict (StrictSeq ((:<|), (:|>), Empty), (|>))
import qualified Data.Sequence.Strict as Seq
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (..), withOriginFromMaybe,
                     withOriginToMaybe)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT.ChainState.HeaderHashBytes
import           Ouroboros.Consensus.Protocol.PBFT.Crypto
import           Ouroboros.Consensus.Util (repeatedly)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | PBFT chain state
--
-- For a window size of @n@ and a security parameter @k@, the PBFT chain state
-- is a sequence of signatures over the last @k+n@ slots
--
-- > +------------------------------------------------|
-- > |                signatures                      |
-- > |-------------------------------+----------------|
-- >           <-n->                 ^      <-k->
-- >                               anchor
-- > ^^^^^^^^^^^^^^|^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- >    k extra                window of n
--
-- We need the last @n@ signatures to verify that no single key has signed more
-- than a certain threshold percentage of the slots. The anchor indicates the
-- maximum rollback; we need the signatures /before/ the anchor point because
-- if we have a rollback, and drop part of the history
--
-- > +----------------------------------------|
-- > |              signatures                | <dropped>
-- > |-------------------------------+--------|
-- >                                 ^
-- >                               anchor
-- >        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- >                   window of n
--
-- the signatures of the blocks that get appended /after the rollback/ should
-- still be evaluated with respect to a full window of size @n@ (note that the
-- position of the anchor does not change after a rollback: rolling back does
-- not change the maximum rollback point).
--
-- This means that unless we are near genesis, we will at least have @n@
-- signatures in the history (even after a maximum rollback of @k@), and under
-- normal circumstances (i.e., when not halfway a switch to a fork), @k+n@
-- signatures.
--
-- The window size itself is pretty much arbitrary and will be fixed by a
-- particular blockchain specification (e.g., Byron).
--
-- The performance of this code is not critical during normal node operation, as
-- it will only be used when a new block comes in. However, it can become a
-- bottleneck during syncing.
data PBftChainState c = PBftChainState {
      -- | Signatures at or before the anchor
      --
      -- We should always have at least one signature (the anchor), unless the
      -- anchor is genesis.
      preAnchor  :: !(StrictSeq (PBftSigner c))

      -- | Signatures after the anchor
      --
      -- We should have precisely @k@ signatures after the anchor, unless
      --
      -- 1. We are near genesis, or
      -- 2. After a rollback (during a switch-to-fork)
    , postAnchor :: !(StrictSeq (PBftSigner c))

      -- | Signatures before the window
    , preWindow  :: !(StrictSeq (PBftSigner c))

      -- | Signatures in the window
      --
      -- We should have precisely @n@ signatures in the window, unless we are
      -- near genesis.
      --
      -- INVARIANT Empty if and only if we are exactly at genesis.
    , inWindow   :: !(StrictSeq (PBftSigner c))

      -- | Cached counts of the signatures in the window
    , counts     :: !(Map (PBftVerKeyHash c) Word64)

      -- | Info about a relevant EBB, if any
      --
      -- EBBs are not signed, so the 'preAnchor', 'postAnchor', 'preWindow',
      -- and 'postWindow' fields are unaffected by EBBs. However, EBBs must
      -- also be valid targets for 'rewind', so this field maps a slot that
      -- contains an EBB to the preceding signed slot, /if/ that signed slot is
      -- still a valid target for 'rewind'.
      --
      -- By assumption, there can be at most one EBB relevant to the @n + k@
      -- window. Current choices that justify this assumption:
      --
      --   * The real nodes currently pervasively require that every chain
      --     includes at least @k@ signed blocks in every span of @2k@ slots.
      --
      --   * The real nodes are currently configured such that epochs to have a
      --     duration of @10k@ slots (so EBBs are @10k@ slots apart).
      --
      --   * The PBFT window size @n@ is currently @k@.
      --
      --   * Thus we crucially have that @n + k < 10k@.
      --
      -- See INVARIANTs on 'MaybeEbbInfo'.
      --
      -- INVARIANT For all @EbbInfo{eiSlot, eiPrevSlot)@ in @'ebbs' (cs ::
      -- 'ChainState')@,
      --
      --  * @eiPrevSlot >= anchorSlot cs@ or @At eiSlot == anchorSlot cs@; see
      --    'pruneEBBsLT'
      --
      --  * @'At' eiSlot <= tgt@ if @cs@ is the result of a 'rewind' to @tgt@;
      --    see 'pruneEBBsGT'
      --
      --  * @and [ At s <= eiPrevSlot | s <- precedingSignedSlots ]@
      --
      --  * @'rewind' k n ('At' eiSlot) cs = 'rewind' k n eiPrevSlot cs@
      --
      --   where
      --
      --  * @precedingSignedSlots = filter (< eiSlot) signedSlots@
      --
      --  * @signedSlots = 'pbftSignerSlotNo' <$> ('preAnchor' <> 'postAnchor')@
    , ebbs       :: !MaybeEbbInfo
    }
  deriving (Generic)

{-------------------------------------------------------------------------------
  Invariant
-------------------------------------------------------------------------------}

size :: Num b => StrictSeq a -> b
size = fromIntegral . Seq.length

-- | Re-compute cached counts
computeCounts :: PBftCrypto c
              => StrictSeq (PBftSigner c)  -> Map (PBftVerKeyHash c) Word64
computeCounts inWindow =
    repeatedly (incrementKey . pbftSignerGenesisKey)
               (Foldable.toList inWindow)
               Map.empty

invariant :: PBftCrypto c
          => SecurityParam -> WindowSize -> PBftChainState c -> Either String ()
invariant (SecurityParam k)
          (WindowSize n)
          st@PBftChainState{..}
        = runExcept $ do
    unless (size postAnchor <= k) $
      failure "Too many post-anchor signatures"

    unless (size preAnchor <= n) $
      failure "Too many pre-anchor signatures"

    unless (size inWindow <= n) $
      failure "Too many in-window signatures"

    unless (size preWindow <= k) $
      failure "Too many pre-window signatures"

    unless (size preWindow + size inWindow <= k + n) $
      failure "Too many signatures"

    unless (preAnchor <> postAnchor == preWindow <> inWindow) $
      failure "Inconsistent signature split"

    unless (computeCounts inWindow == counts) $
      failure "Cached counts incorrect"

    unless (allEbbs $ \slot mSlot -> At slot == anchorSlot st ||
                                     mSlot >= anchorSlot st) $
      failure "EBB mapped to slot before anchor"

    unless (allEbbs $ \slot mSlot ->
               let signedSlots          =
                     fmap pbftSignerSlotNo $
                     Foldable.toList $ preAnchor <> postAnchor
                   precedingSignedSlots = filter (< slot) signedSlots
               in all (\s -> At s <= mSlot) precedingSignedSlots
           ) $
      failure "EBB does not map to the preceding signature"

    -- 'MaybeEbbInfo''s "Key greater"
    unless (allEbbs $ \slot mSlot -> At slot > mSlot) $
      failure "EBB mapped to a simultaneous or future slot"
  where
    failure :: String -> Except String ()
    failure err = throwError $ err ++ ": " ++ show st

    allEbbs p = case ebbs of
      NothingEbbInfo          -> True
      JustEbbInfo EbbInfo{..} -> p eiSlot eiPrevSlot

-- | The 'PBftChainState' tests don't rely on this flag but check the
-- invariant manually. This flag is here so that the invariant checks could be
-- enabled while running other consensus tests, just as a sanity check.
--
-- TODO: Make this a CPP flag, see #1248.
enableInvariant :: Bool
enableInvariant = False

assertInvariant :: (HasCallStack, PBftCrypto c)
                => SecurityParam
                -> WindowSize
                -> PBftChainState c -> PBftChainState c
assertInvariant k n st
  | enableInvariant =
      case invariant k n st of
        Right () -> st
        Left err -> error $ "Invariant violation: " ++ err
  | otherwise = st

-- | Slot and corresponding genesis key
data PBftSigner c = PBftSigner {
      pbftSignerSlotNo     :: !SlotNo
    , pbftSignerGenesisKey :: !(PBftVerKeyHash c)
    }
  deriving (Generic)

-- | Window size
--
-- See 'PBftChainState' itself for a detailed discussion on the window size
-- versus the number of signatures.
newtype WindowSize = WindowSize { getWindowSize :: Word64 }
  deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral)

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
countSignatures :: PBftChainState c -> Word64
countSignatures PBftChainState{..} = size preWindow + size inWindow

-- | Number of signatures in the window
--
-- This will be equal to the specified window size, unless near genesis
countInWindow :: PBftChainState c -> Word64
countInWindow PBftChainState{..} = size inWindow

-- | The number of blocks signed by the specified genesis key
--
-- This only considers the signatures within the window, not in the pre-window;
-- see 'PBftChainState' for detailed discussion.
countSignedBy :: PBftCrypto c => PBftChainState c -> PBftVerKeyHash c -> Word64
countSignedBy PBftChainState{..} gk = Map.findWithDefault 0 gk counts

-- | The last (most recent) signed slot in the window
--
-- Returns 'Origin' if there are no signatures in the window (this will happen
-- exactly at genesis only).
--
-- Unaffected by EBBs, since they're not signed.
lastSignedSlot :: PBftChainState c -> WithOrigin SlotNo
lastSignedSlot PBftChainState{..} =
    case inWindow of
      _ :|> signer -> At (pbftSignerSlotNo signer)
      _otherwise   -> Origin

-- | The anchor slot
--
-- Returns 'Origin' if there are no signatures in the window (this will happen
-- exactly at genesis only).
--
-- Unaffected by EBBs, since they're not signed.
anchorSlot :: PBftChainState c -> WithOrigin SlotNo
anchorSlot PBftChainState{..} =
    case preAnchor of
      _ :|> signer -> At (pbftSignerSlotNo signer)
      _otherwise   -> Origin

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Empty PBFT chain state
--
-- In other words, the PBFT chain state corresponding to genesis.
empty :: PBftChainState c
empty = PBftChainState {
      preAnchor  = Empty
    , postAnchor = Empty
    , preWindow  = Empty
    , inWindow   = Empty
    , counts     = Map.empty
    , ebbs       = ebbsEmpty
    }

-- | Append new signature
--
-- Drops the oldest signature, provided we have reached the required number.
append :: forall c. PBftCrypto c
       => SecurityParam
       -> WindowSize
       -> PBftSigner c
       -> PBftChainState c -> PBftChainState c
append k n signer@(PBftSigner _ gk) PBftChainState{..} =
    assertInvariant k n $
    pruneEBBsLT $
    PBftChainState {
        preAnchor  = preAnchor'
      , postAnchor = postAnchor'
      , preWindow  = preWindow'
      , inWindow   = inWindow'
      , counts     = updateCounts counts
        -- Will be pruned by the enclosing call to 'pruneEBBsLT'
      , ebbs       = ebbs
      }
  where
    (preAnchor', postAnchor') =
      case postAnchor of
        x :<| xs | size postAnchor == maxRollbacks k ->
          prune k n (preAnchor |> x, xs |> signer)
        _otherwise ->
          (preAnchor, postAnchor |> signer) -- We assume k >= 1

    ((preWindow', inWindow'), updateCounts) =
      case inWindow of
        x :<| xs | size inWindow == getWindowSize n ->
          ( prune k n (preWindow |> x, xs |> signer)
          , incrementKey gk . decrementKey (pbftSignerGenesisKey x)
          )
        _otherwise ->
          ( (preWindow, inWindow |> signer)
          , incrementKey gk
          )

-- | Rewind the state to the specified slot
--
-- This matches the semantics of 'rewindChainState' in 'OuroborosTag', in that
-- this should be the state after the given point.
--
-- NOTE: It only makes sense to rewind to a slot containing a block that we
-- have previously applied (the "genesis block" can be understood as having
-- been implicitly applied). HOWEVER, this function does not check this
-- precondition: it only uses the provided header hash to check if the
-- requested point is an EBB that was previously applied. If the header hash is
-- just random bytes, then the function will assume the target is a signed
-- block in the slot, without trying to confirm the signed block's header hash
-- matches that of the request.
--
-- In addition to preserving the invariant, we also have the guarantee that
-- rolling back to a point (within @k@) and then reapplying the blocks that were
-- rolled back results in the original state.
rewind :: forall c. PBftCrypto c
       => SecurityParam
       -> WindowSize
       -> WithOrigin (SlotNo, HeaderHashBytes)
          -- ^ the target \"point\"; see 'EbbInfo'
       -> PBftChainState c -> Maybe (PBftChainState c)
rewind k n p cs@PBftChainState{..} = case p of
    Origin               -> go Origin
    At (slot, hashBytes) -> case ebbsLookup slot ebbs of
      Just EbbInfo{..}
        | hashBytes == eiHashBytes -> go eiPrevSlot
      _                            -> go (At slot)
  where
    go mSlot = pruneEBBsGT (fst <$> p) <$> rewind_ k n mSlot cs

-- | Internal worker for 'rewind'
rewind_ :: forall c. PBftCrypto c
       => SecurityParam
       -> WindowSize
       -> WithOrigin SlotNo
       -> PBftChainState c -> Maybe (PBftChainState c)
rewind_ k n mSlot PBftChainState{..} =
    case mSlot of
      At slot ->
        -- We scan from the right, since block to roll back to likely at end
        case Seq.spanr (\(PBftSigner slot' _) -> slot' > slot) postAnchor of

          -- We found the slot to roll back to post-anchor. Discard everything
          -- after that slot.
          (toDiscard, toKeep@(_ :|> x)) ->
            if slot == pbftSignerSlotNo x
              then Just $ go toDiscard toKeep
              else notFound slot

          -- The slot was not found post-anchor. If the slot matches the last
          -- slot pre-anchor, all is well, discarding everything post-anchor.
          -- Otherwise, the rollback is too far.
          (toDiscard, Empty) ->
            case preAnchor of
              _ :|> x
                | slot == pbftSignerSlotNo x -> Just $ go toDiscard Empty
                | slot <  pbftSignerSlotNo x -> rollbackTooFar
                | otherwise                  -> notFound slot
              Empty                          -> notFound slot

      -- We can only roll back to origin if there are no signatures
      -- pre-anchor. Rolling back to origin would leave the chain empty. This
      -- is only possible if we have at most @k@ blocks in the chain. If we
      -- have more than @k@ blocks, the pre-anchor will not be empty.
      Origin ->
        case preAnchor of
          Empty      -> Just $ go postAnchor Empty
          _otherwise -> rollbackTooFar
  where
    -- If we didn't find a non-EBB, check if the slot was known to have an EBB.
    -- If so, recur (just once, as long as 'ebbs' is well-formed).
    notFound :: forall a. SlotNo -> a
    notFound slot =
      error $ "rewind: rollback to block not previously applied, "
          ++ show slot

    rollbackTooFar :: Maybe y
    rollbackTooFar = Nothing

    -- Construct new state, given the remaining post-anchor signatures
    --
    -- NOTE: we don't optimize this case as rollbacks in Byron are exceedingly
    -- rare.
    go :: StrictSeq (PBftSigner c)
       -> StrictSeq (PBftSigner c)
       -> PBftChainState c
    go postAnchorDiscard postAnchorKeep = assertInvariant k n $ PBftChainState {
          preAnchor  = preAnchor -- can't change by definition
        , postAnchor = postAnchorKeep
        , preWindow  = preWindow'
        , inWindow   = inWindow'
        , counts     = computeCounts inWindow' -- for simplicity, just recount
        , ebbs       = ebbs   -- NB this needs to be pruned
        }
      where
        -- Reconstruct the window
        (preWindow', inWindow') =
          Seq.splitAtEnd (fromIntegral n) $
            Seq.dropLast (Seq.length postAnchorDiscard) (preWindow <> inWindow)

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

incrementKey :: Ord gk => gk -> Map gk Word64 -> Map gk Word64
incrementKey = Map.alter inc
  where
    inc :: Maybe Word64 -> Maybe Word64
    inc Nothing  = Just 1
    inc (Just n) = Just (n + 1)

decrementKey :: Ord gk => gk -> Map gk Word64 -> Map gk Word64
decrementKey = Map.alter dec
  where
    dec :: Maybe Word64 -> Maybe Word64
    dec Nothing  = error "decrementKey: key does not exist"
    dec (Just 1) = Nothing
    dec (Just n) = Just (n - 1)

-- | Internal: drop elements from the first list, keeping max size at most @k+n@
prune :: SecurityParam
      -> WindowSize
      -> (StrictSeq a, StrictSeq a)
      -> (StrictSeq a, StrictSeq a)
prune (SecurityParam n) (WindowSize k) (xs, ys) =
    (Seq.drop (fromIntegral toDrop) xs, ys)
  where
    totalSize, maxSize, toDrop :: Word64
    totalSize = size xs + size ys
    maxSize   = n + k
    toDrop    = if totalSize > maxSize
                  then totalSize - maxSize
                  else 0

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

toList :: PBftChainState c -> (WithOrigin SlotNo, StrictSeq (PBftSigner c), MaybeEbbInfo)
toList PBftChainState{..} = (
      case preAnchor of
        Empty   -> Origin
        _ :|> x -> At (pbftSignerSlotNo x)
    , preWindow <> inWindow
    , ebbs
    )

fromList :: PBftCrypto c
         => SecurityParam
         -> WindowSize
         -> (WithOrigin SlotNo, StrictSeq (PBftSigner c), MaybeEbbInfo)
         -> PBftChainState c
fromList k n (anchor, signers, ebbs) =
    assertInvariant k n $
    pruneEBBsLT $
    PBftChainState {..}
  where
    inPreAnchor :: PBftSigner c -> Bool
    inPreAnchor (PBftSigner slot _) = At slot <= anchor

    (preAnchor, postAnchor) = Seq.spanl inPreAnchor signers
    (preWindow, inWindow)   = Seq.splitAtEnd (fromIntegral n) signers
    counts                  = computeCounts inWindow

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

serializationFormatVersion1 :: Word8
serializationFormatVersion1 = 1

serializationFormatVersion2 :: Word8
serializationFormatVersion2 = 2
  -- CHANGELOG
  --
  -- Version 0 is 2 fields, the anchor and the window. Note that it does not
  -- have the version marker.
  --
  -- Version 1 has 4 fields, the version marker, anchor, window, and @~(Map
  -- SlotNo (WithOrigin SlotNo))@.
  --
  -- Version 2 has 4 fields, the version marker, anchor, window, and @~(Maybe EbbInfo)@.

encodePBftChainState :: (PBftCrypto c, Serialise (PBftVerKeyHash c))
                     => PBftChainState c -> Encoding
encodePBftChainState st@PBftChainState{..} = mconcat [
      Serialise.encodeListLen 4
    , encode serializationFormatVersion2
    , encode (withOriginToMaybe anchor)
    , encode signers
    , encode ebbs'
    ]
  where
    (anchor, signers, ebbs') = toList st

decodePBftChainState :: (PBftCrypto c, Serialise (PBftVerKeyHash c), HasCallStack)
                     => SecurityParam
                     -> WindowSize
                     -> Decoder s (PBftChainState c)
decodePBftChainState k n = Serialise.decodeListLen >>= \case
   2 -> do -- Version is 0
      anchor  <- withOriginFromMaybe <$> decode
      signers <- decode
      return $ fromList k n (anchor, signers, ebbsEmpty)
   4 -> decode >>= \v -> if
      | v == serializationFormatVersion1 -> do
        anchor  <- withOriginFromMaybe <$> decode
        signers <- decode
        ebbs'   <- decode
        let _ = ebbs' :: Map SlotNo (WithOrigin SlotNo)
        -- NB we discard ebbs'
        return $ fromList k n (anchor, signers, ebbsEmpty)
      | v == serializationFormatVersion2 -> do
        anchor  <- withOriginFromMaybe <$> decode
        signers <- decode
        ebbs'   <- decode
        return $ fromList k n (anchor, signers, ebbs')
      | otherwise ->
        error $ "unexpected serialisation format version: " <> show v
   o -> error $ "unexpected list length: " <> show o

instance Serialise (PBftVerKeyHash c) => Serialise (PBftSigner c) where
  encode = encode . toPair
    where
      toPair (PBftSigner{..}) = (pbftSignerSlotNo, pbftSignerGenesisKey)

  decode = fromPair <$> decode
    where
      fromPair (slotNo, genesisKey) = PBftSigner slotNo genesisKey

{-------------------------------------------------------------------------------
  EBB Map
-------------------------------------------------------------------------------}

-- | Append an EBB
--
-- Its slot will be mapped to the 'lastSignedSlot'.
appendEBB :: forall c. (PBftCrypto c, HasCallStack)
          => SecurityParam
          -> WindowSize
          -> SlotNo
          -> HeaderHashBytes
          -> PBftChainState c -> PBftChainState c
appendEBB k n newEbbSlot hashBytes cs@PBftChainState{..} =
    assertInvariant k n $
    Exn.assert valid $
    cs{ebbs = JustEbbInfo EbbInfo
        { eiSlot      = newEbbSlot
        , eiHashBytes = hashBytes
        , eiPrevSlot  = latestNonEbbSlot
        }}
  where
    latestEbbSlot    = ebbsMax ebbs
    latestNonEbbSlot = lastSignedSlot cs

    valid = At newEbbSlot > max latestEbbSlot latestNonEbbSlot

-- | Discard 'ebbs' mappings whose 'eiPrevSlot' is before the anchor, except
-- if its 'eiSlot' is equal to the anchor's slot
--
-- Called by 'append', since 'ebbs' do not increase how far back the chain
-- state can rewind. However, we must retain the EBB that shares a slot with
-- the anchor so that we can fail if we attempt to rewind to it -- if we forget
-- about that EBB, then we won't be able to recognize its hash in the requested
-- rewind point.
pruneEBBsLT :: PBftChainState c -> PBftChainState c
pruneEBBsLT cs@PBftChainState{..} =
  cs{ ebbs = ebbsFilter ebbs $ \EbbInfo{..} ->
        eiPrevSlot >= anchorSlot cs ||
        At eiSlot == anchorSlot cs }
-- NOTE: this INLINE seems redundant but we add it here to avoid a strange
-- space leak that also goes away with @-O0@, see #1356.
{-# INLINE pruneEBBsLT #-}

-- | Discard 'ebbs' mappings whose 'eiSlot' is after the given slot
--
-- Called by 'rewind', since 'rewind'ing to a slot should forget the EBBs it
-- precedes.
pruneEBBsGT :: WithOrigin SlotNo -> PBftChainState c -> PBftChainState c
pruneEBBsGT mSlot cs@PBftChainState{..} =
  cs{ ebbs = ebbsFilter ebbs $ \EbbInfo{..} -> At eiSlot <= mSlot }

-- | Info about the latest EBB, if there is one recent enough to be relevant to
-- the chain state
--
data MaybeEbbInfo
  = NothingEbbInfo
  | JustEbbInfo !EbbInfo
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NoUnexpectedThunks, Serialise)

-- | Info about an EBB
--
-- The serialised bytes of the EBB's header hash and its latest previous signed
-- slot. We use 'HeaderHashBytes' instead of the EBB's actual @HeaderHash@
-- because the 'ChainState' type family (which we instantiate as
-- 'PBftChainState') does not take a type argument that to which we can apply
-- @HeaderHash@. This is a compromise.
--
-- INVARIANT @At 'eiSlot' > 'eiPrevSlot'@
data EbbInfo = EbbInfo
  { eiSlot      :: !SlotNo
    -- ^ the slot of the EBB
  , eiHashBytes :: !HeaderHashBytes
    -- ^ the bytes of the serialised header hash of the EBB
  , eiPrevSlot  :: !(WithOrigin SlotNo)
    -- ^ the slot of the latest non-EBB that precedes the EBB
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NoUnexpectedThunks, Serialise)

ebbsEmpty :: MaybeEbbInfo
ebbsEmpty = NothingEbbInfo

ebbsMax :: MaybeEbbInfo -> WithOrigin SlotNo
ebbsMax = \case
  NothingEbbInfo          -> Origin
  JustEbbInfo EbbInfo{..} -> At eiSlot

ebbsLookup :: SlotNo -> MaybeEbbInfo -> Maybe EbbInfo
ebbsLookup k = \case
  NothingEbbInfo             -> Nothing
  JustEbbInfo ei@EbbInfo{..} -> if eiSlot == k then Just ei else Nothing

ebbsFilter :: MaybeEbbInfo -> (EbbInfo -> Bool) -> MaybeEbbInfo
ebbsFilter x f = case x of
  NothingEbbInfo -> NothingEbbInfo
  JustEbbInfo ei -> if f ei then x else NothingEbbInfo
