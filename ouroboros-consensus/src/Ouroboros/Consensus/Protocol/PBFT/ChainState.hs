{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | PBFT chain state
--
-- Intended for qualified import.
module Ouroboros.Consensus.Protocol.PBFT.ChainState (
    PBftChainState
  , empty
  , prune
  , size
  , insert
  , rewind
  , lastSlot
  , countSignedBy
    -- * Primarily for tests
  , fromMap
  ) where

import           Codec.Serialise (Serialise)
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Protocol.PBFT.Crypto

-- | PBFT chain satte
--
-- The PBFT chain state consists of a map from genesis keys to the list of
-- blocks which they have issued.
newtype PBftChainState c = PBftChainState (Map (PBftVerKeyHash c) (Seq SlotNo))

-- | Construct chain state from a map
--
-- Used for testing only.
fromMap :: Map (PBftVerKeyHash c) (Seq SlotNo) -> PBftChainState c
fromMap = PBftChainState

deriving instance PBftCrypto c => Show (PBftChainState c)
deriving instance PBftCrypto c => Eq   (PBftChainState c)

-- | Don't require PBftCrypto (we don't need the 'Given' constraint here)
deriving instance ( Ord       (PBftVerKeyHash c)
                  , Serialise (PBftVerKeyHash c)
                  ) => Serialise (PBftChainState c)

empty :: PBftChainState c
empty = PBftChainState Map.empty

-- | Prune the chain state to the given size by dropping the signers in the
-- oldest slots.
prune :: forall c. PBftCrypto c => Int -> PBftChainState c -> PBftChainState c
prune toSize st@(PBftChainState cs) = PBftChainState $ go
    cs
    (sortOn snd . Map.toAscList . Map.mapMaybe (Seq.lookup 0) $ cs)
    (max 0 $ size st - toSize)
  where
    go :: (Ord k, Ord v)
       => Map k (Seq v)  -- ^ The chain state to prune
       -> [(k, v)]       -- ^ Index: for each @k@ in the chain state, its
                         -- oldest @v@ (slot).
                         --
                         -- INVARIANT: the @k@s in the chain state match the
                         -- @k@s in the index.
       -> Int            -- ^ How many elements left to drop
       -> Map k (Seq v)
    go fromCS idx toDrop = if toDrop <= 0 then fromCS else case idx of
      [] -> fromCS
      (gk,_):xs@((_,nextLowest):_) ->
        let (newSeq, numDropped) = dropWhileL (< nextLowest) $ fromCS Map.! gk
            newIdx = case newSeq of
              x Seq.:<| _ -> sortOn snd $ (gk, x) : xs
              _           -> xs
        in go (Map.insert gk newSeq fromCS) newIdx (toDrop - numDropped)
      -- Only one genesis key
      (gk,_):[] ->
        let newSeq = Seq.drop toDrop $ fromCS Map.! gk
        in Map.insert gk newSeq fromCS

size :: PBftChainState c -> Int
size (PBftChainState cs) = sum $ Seq.length <$> Map.elems cs

-- | Insert a signatory into the chain state.
insert
  :: PBftCrypto c
  => PBftVerKeyHash c
  -> SlotNo
  -> PBftChainState c
  -> PBftChainState c
insert gk s (PBftChainState cs) = PBftChainState $
  Map.alter (\case
      Just es -> Just $ es Seq.|> s
      Nothing -> Just $ Seq.singleton s
    ) gk cs

rewind :: WithOrigin SlotNo -> PBftChainState c -> Maybe (PBftChainState c)
rewind mSlot (PBftChainState cs) = PBftChainState <$> case mSlot of
    Origin
        -> Just Map.empty
    At slot
        | all Seq.null $ Map.elems oldCs
        -> Nothing
        | otherwise
        -> Just oldCs
      where
        oldCs = Seq.takeWhileL (<= slot) <$> cs

-- We always include slot number 0 in case there are no signers yet.
lastSlot :: PBftChainState c -> SlotNo
lastSlot (PBftChainState cs) =
        maximum
     .  (SlotNo 0 :)
     $  lastSlotOfSigner
    <$> Map.elems cs
  where
    lastSlotOfSigner :: Seq SlotNo -> SlotNo
    lastSlotOfSigner = \case
      _ Seq.:|> s -> s
      _           -> SlotNo 0

countSignedBy :: PBftCrypto c => PBftChainState c -> PBftVerKeyHash c -> Int
countSignedBy (PBftChainState cs) gk = maybe 0 Seq.length $ Map.lookup gk cs

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Variant of 'dropWhileL' which also returns the number of elements dropped
dropWhileL :: (a -> Bool) -> Seq a -> (Seq a, Int)
dropWhileL f s = let res = Seq.dropWhileL f s in
  (res, Seq.length s - Seq.length res)
