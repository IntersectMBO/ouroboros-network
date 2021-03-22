{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | PBFT chain state
--
-- Intended for qualified import.
module Ouroboros.Consensus.Protocol.PBFT.State (
    PBftSigner (..)
  , PBftState (..)
  , Ticked (..)
  , WindowSize (..)
    -- * Construction
  , append
  , empty
    -- * Queries
  , countSignatures
  , countSignedBy
  , lastSignedSlot
    -- * Conversion
  , fromList
  , toList
    -- * Serialization
  , decodePBftState
  , encodePBftState
  ) where

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except
import qualified Data.Foldable as Foldable
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence.Strict (StrictSeq (Empty, (:<|), (:|>)), (|>))
import qualified Data.Sequence.Strict as Seq
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.PBFT.Crypto
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.Versioned

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | PBFT state
--
-- For a window size of @n@, the PBFT chain state
-- is a sequence of signatures over the last @n@ slots
--
-- > +-------------------------------------------+
-- > |                signatures                 |
-- > +-------------------------------------------+
-- >
-- > ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- >                  window of n
--
-- We need the last @n@ signatures to verify that no single key has signed more
-- than a certain threshold percentage of the slots.
--
-- When near genesis, we will have less than @n@ signatures in the history.
--
-- The window size itself is pretty much arbitrary and will be fixed by a
-- particular blockchain specification (e.g., Byron).
data PBftState c = PBftState {
      -- | Signatures in the window
      --
      -- We should have precisely @n@ signatures in the window, unless we are
      -- near genesis.
      --
      -- INVARIANT Empty if and only if we are exactly at genesis.
      inWindow :: !(StrictSeq (PBftSigner c))

      -- | Cached counts of the signatures in the window
    , counts   :: !(Map (PBftVerKeyHash c) Word64)
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
          => WindowSize -> PBftState c -> Either String ()
invariant (WindowSize n) st@PBftState{..} = runExcept $ do
    unless (size inWindow <= n) $
      failure "Too many in-window signatures"

    unless (computeCounts inWindow == counts) $
      failure "Cached counts incorrect"
  where
    failure :: String -> Except String ()
    failure err = throwError $ err ++ ": " ++ show st

-- | The 'PBftState' tests don't rely on this flag but check the
-- invariant manually. This flag is here so that the invariant checks could be
-- enabled while running other consensus tests, just as a sanity check.
--
-- TODO: Make this a CPP flag, see #1248.
enableInvariant :: Bool
enableInvariant = False

assertInvariant ::
     (HasCallStack, PBftCrypto c)
  => WindowSize
  -> PBftState c -> PBftState c
assertInvariant n st
  | enableInvariant =
      case invariant n st of
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
-- See 'PBftState' itself for a detailed discussion on the window size
-- versus the number of signatures.
newtype WindowSize = WindowSize { getWindowSize :: Word64 }
  deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral)

deriving instance PBftCrypto c => Show     (PBftState c)
deriving instance PBftCrypto c => Eq       (PBftState c)
deriving instance PBftCrypto c => NoThunks (PBftState c)

deriving instance PBftCrypto c => Show     (PBftSigner c)
deriving instance PBftCrypto c => Eq       (PBftSigner c)
deriving instance PBftCrypto c => NoThunks (PBftSigner c)

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Number of signatures in the window
--
-- This will be equal to the specified window size, unless near genesis
countSignatures :: PBftState c -> Word64
countSignatures PBftState{..} = size inWindow

-- | The number of blocks signed by the specified genesis key
--
-- This only considers the signatures within the window, not in the pre-window;
-- see 'PBftState' for detailed discussion.
countSignedBy :: PBftCrypto c => PBftState c -> PBftVerKeyHash c -> Word64
countSignedBy PBftState{..} gk = Map.findWithDefault 0 gk counts

-- | The last (most recent) signed slot in the window
--
-- Returns 'Origin' if there are no signatures in the window (this will happen
-- exactly at genesis only).
--
-- Unaffected by EBBs, since they're not signed.
lastSignedSlot :: PBftState c -> WithOrigin SlotNo
lastSignedSlot PBftState{..} =
    case inWindow of
      _ :|> signer -> NotOrigin (pbftSignerSlotNo signer)
      _otherwise   -> Origin

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Empty PBFT chain state
--
-- In other words, the PBFT chain state corresponding to genesis.
empty :: PBftState c
empty = PBftState {
      inWindow   = Empty
    , counts     = Map.empty
    }

-- | Append new signature
--
-- Drops the oldest signature, provided we have reached the required number.
append ::
     forall c. PBftCrypto c
  => WindowSize
  -> PBftSigner c
  -> PBftState c -> PBftState c
append n signer@(PBftSigner _ gk) PBftState{..} =
    assertInvariant n $ PBftState {
        inWindow = trimmedWindow
      , counts   = trimmedCounts
      }
  where
    -- First append the signature to the right,
    (appendedWindow, appendedCounts) =
        (inWindow |> signer, incrementKey gk counts)
    -- then trim the oldest from the left, if needed.
    (trimmedWindow, trimmedCounts) = case appendedWindow of
        x :<| xs | size inWindow == getWindowSize n ->
          (xs, decrementKey (pbftSignerGenesisKey x) appendedCounts)
        _otherwise ->
          (appendedWindow, appendedCounts)

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

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

toList :: PBftState c -> [PBftSigner c]
toList = Foldable.toList . inWindow

-- | Note: we are not checking the invariants because we don't want to require
-- the 'WindowSize' to be in the context, see #2383. When assertions are
-- enabled, we would notice the invariant violation as soon as we 'append'.
--
-- PRECONDITION: the slots of the signers are in ascending order.
fromList :: PBftCrypto c => [PBftSigner c] -> PBftState c
fromList signers = PBftState {
      inWindow = inWindow
    , counts   = computeCounts inWindow
    }
  where
    inWindow = Seq.fromList signers

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

-- | Version 0 supported rollback, removed in #2575.
serializationFormatVersion1 :: VersionNumber
serializationFormatVersion1 = 1

invert :: PBftCrypto c => PBftState c -> Map (PBftVerKeyHash c) [SlotNo]
invert =
      Foldable.foldl'
        (\acc (PBftSigner slot key) -> Map.insertWith (<>) key [slot] acc)
        Map.empty
    . inWindow

uninvert :: PBftCrypto c => Map (PBftVerKeyHash c) [SlotNo] -> PBftState c
uninvert =
      fromList
    . sortOn pbftSignerSlotNo
    . concatMap (\(key, slots) -> map (`PBftSigner` key) slots)
    . Map.toList

encodePBftState ::
     (PBftCrypto c, Serialise (PBftVerKeyHash c))
  => PBftState c -> Encoding
encodePBftState st =
    encodeVersion serializationFormatVersion1 $
      encode (invert st)

decodePBftState ::
     forall c. (PBftCrypto c, Serialise (PBftVerKeyHash c))
  => forall s. Decoder s (PBftState c)
decodePBftState = decodeVersion
    [(serializationFormatVersion1, Decode decodePBftState1)]
  where
    decodePBftState1 :: forall s. Decoder s (PBftState c)
    decodePBftState1 = uninvert <$> decode

instance Serialise (PBftVerKeyHash c) => Serialise (PBftSigner c) where
  encode = encode . toPair
    where
      toPair (PBftSigner{..}) = (pbftSignerSlotNo, pbftSignerGenesisKey)

  decode = fromPair <$> decode
    where
      fromPair (slotNo, genesisKey) = PBftSigner slotNo genesisKey
