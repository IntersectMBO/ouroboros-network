{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.HardFork.History.Summary (
    -- * Bounds
    Bound (..)
  , initBound
  , mkUpperBound
  , slotToEpochBound
    -- * Per-era summary
  , EraEnd (..)
  , EraSummary (..)
  , mkEraEnd
    -- * Overall summary
  , Summary (..)
    -- ** Construction
  , neverForksSummary
  , summaryWithExactly
    -- *** Summarize
  , Shape (..)
  , Transitions (..)
  , invariantShape
  , invariantSummary
  , singletonShape
  , summarize
  , transitionsUnknown
    -- ** Query
  , summaryBounds
  , summaryInit
  ) where

import           Codec.CBOR.Decoding (TokenType (TypeNull), decodeNull,
                     peekTokenType)
import           Codec.CBOR.Encoding (encodeListLen, encodeNull)
import           Codec.Serialise
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Foldable (toList)
import           Data.Kind (Type)
import           Data.Proxy
import           Data.SOP.Strict (SListI, lengthSList)
import           Data.Time hiding (UTCTime)
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack
import           NoThunks.Class (InspectHeapNamed (..), NoThunks)

import           Cardano.Binary (enforceSize)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Util.Counting

import           Ouroboros.Consensus.HardFork.History.EraParams
import           Ouroboros.Consensus.HardFork.History.Util

{-------------------------------------------------------------------------------
  Bounds
-------------------------------------------------------------------------------}

-- | Detailed information about the time bounds of an era
data Bound = Bound {
      boundTime  :: !RelativeTime
    , boundSlot  :: !SlotNo
    , boundEpoch :: !EpochNo
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoThunks)

initBound :: Bound
initBound = Bound {
      boundTime  = RelativeTime 0
    , boundSlot  = SlotNo       0
    , boundEpoch = EpochNo      0
    }

-- | Version of 'mkUpperBound' when the upper bound may not be known
--
-- If passed 'Nothing', assumes 'EraUnbounded'. This is /NOT/
-- suitable for eras where the transition is simply unknown.
mkEraEnd :: EraParams
         -> Bound          -- ^ Lower bound
         -> Maybe EpochNo  -- ^ Upper bound
         -> EraEnd
mkEraEnd params lo = maybe EraUnbounded (EraEnd . mkUpperBound params lo)

-- | Compute upper bound given just the epoch number and era parameters
mkUpperBound :: HasCallStack
             => EraParams
             -> Bound    -- ^ Lower bound
             -> EpochNo  -- ^ Upper bound
             -> Bound
mkUpperBound EraParams{..} lo hiEpoch = Bound {
      boundTime  = addRelTime inEraTime  $ boundTime lo
    , boundSlot  = addSlots   inEraSlots $ boundSlot lo
    , boundEpoch = hiEpoch
    }
  where
    inEraEpochs, inEraSlots :: Word64
    inEraEpochs = countEpochs hiEpoch (boundEpoch lo)
    inEraSlots  = inEraEpochs * unEpochSize eraEpochSize

    inEraTime :: NominalDiffTime
    inEraTime = fromIntegral inEraSlots * getSlotLength eraSlotLength

-- Given the 'SlotNo' of the first /slot/ in which a transition could take
-- place, compute the first /epoch/ in which this could happen (since
-- transitions only take place at epoch boundaries). If the 'SlotNo' happens
-- to be the first slot in an epoch, it will be that 'EpochNo'; if it isn't,
-- however, it will be the /next/ epoch.
slotToEpochBound :: EraParams -> Bound -> SlotNo -> EpochNo
slotToEpochBound EraParams{eraEpochSize = EpochSize epochSize} lo hiSlot =
    addEpochs
      (if inEpoch == 0 then epochs else epochs + 1)
      (boundEpoch lo)
  where
    slots             = countSlots hiSlot (boundSlot lo)
    (epochs, inEpoch) = slots `divMod` epochSize

{-------------------------------------------------------------------------------
  Summary

  This is what we use internally for all translations.
-------------------------------------------------------------------------------}

-- | Information about a specific era
--
-- The 'eraEnd' of the final era in the summary will be determined by the
-- safe zone considerations discussed above.
--
-- Let the start of the summary be @(t, s, e)@ (time, slot epoch), and the
-- end of the summary be @(t', s', e')@. We have one invariant relating
-- epochs and slots:
--
-- > INV-1a  e' == e + ((s' - s) / epochSize)
-- > INV-1b: s' == s + ((e' - e) * epochSize)
--
-- And another invariant relating time and slots:
--
-- > INV-2a: s' == s + ((t' - t) / slotLen)
-- > INV-2b: t' == t + ((s' - s) * slotLen)
--
-- Note that these aren't really two sets of independent invariants. @INV-1a@
-- follows from @INV-1b@:
--
-- >       s'                   == s + ((e' - e) * epochSize)
-- >       s' - s               ==     ((e' - e) * epochSize)
-- >      (s' - s) / epochSize  ==       e' - e
-- > e + ((s' - s) / epochSize) ==       e'
--
-- Similarly, @INV-2a@ follows from @INV-2b@:
--
-- >       t'                 == t + ((s' - s) * slotLen)
-- >       t' - t             ==     ((s' - s) * slotLen)
-- >      (t' - t) / slotLen  ==       s' - s
-- > s + ((t' - t) / slotLen) ==       s'
data EraSummary = EraSummary {
      eraStart  :: !Bound     -- ^ Inclusive lower bound
    , eraEnd    :: !EraEnd    -- ^ Exclusive upper bound
    , eraParams :: !EraParams -- ^ Active parameters
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoThunks)

-- | Exclusive upper bound on the era
data EraEnd =
    -- | Bounded era
    EraEnd !Bound

    -- | Unbounded era
    --
    -- This arises from the use of 'UnsafeIndefiniteSafeZone'.
  | EraUnbounded
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoThunks)

-- | Summary of the /confirmed/ part of the ledger
--
-- The summary zips 'Shape' with 'Forks', and provides detailed information
-- about the start and end of each era.
--
-- We have at most one summary for each era, and at least one
newtype Summary xs = Summary { getSummary :: NonEmpty xs EraSummary }
  deriving (Eq, Show)
  deriving NoThunks via InspectHeapNamed "Summary" (Summary xs)

{-------------------------------------------------------------------------------
  Trivial summary
-------------------------------------------------------------------------------}

-- | 'Summary' for a ledger that never forks
neverForksSummary :: EpochSize -> SlotLength -> Summary '[x]
neverForksSummary epochSize slotLen = Summary $ NonEmptyOne $ EraSummary {
      eraStart  = initBound
    , eraEnd    = EraUnbounded
    , eraParams = EraParams {
          eraEpochSize  = epochSize
        , eraSlotLength = slotLen
        , eraSafeZone   = UnsafeIndefiniteSafeZone
        }
    }

{-------------------------------------------------------------------------------
  Basic API for 'Summary'
-------------------------------------------------------------------------------}

-- | Outer bounds of the summary
summaryBounds :: Summary xs -> (Bound, EraEnd)
summaryBounds (Summary summary) =
    (eraStart (nonEmptyHead summary), eraEnd (nonEmptyLast summary))

-- | Analogue of 'Data.List.init' for 'Summary' (i.e., split off the final era)
--
-- This is primarily useful for tests.
summaryInit :: Summary xs -> (Maybe (Summary xs), EraSummary)
summaryInit (Summary summary) = first (fmap Summary) $ nonEmptyInit summary

-- | Construct 'Summary' with an exact number of 'EraSummary'
--
-- Primarily useful for tests.
summaryWithExactly :: Exactly (x ': xs) EraSummary -> Summary (x ': xs)
summaryWithExactly = Summary . exactlyWeakenNonEmpty

{-------------------------------------------------------------------------------
  Shape and Transitions

  This is used only for 'summarize'.
-------------------------------------------------------------------------------}

-- | The shape of the chain (old to new)
--
-- The shape determines how many hard forks we expect as well as the parameters
-- for each era. The type argument is a type-level list containing one entry
-- per era, emphasizing that this information is statically known.
--
-- The entry indices themselves are not used here, but the idea is that they
-- look something like @'[ByronBlock, ShelleyBlock, GoguenBlock]@ and do affect
-- the hard fork combinator. So far this is a list of block types, since most
-- of consensus is indexed by block types.
newtype Shape xs = Shape { getShape :: Exactly xs EraParams }
  deriving (Show)
  deriving NoThunks via InspectHeapNamed "Shape" (Shape xs)

-- | There is only one era
singletonShape :: EraParams -> Shape '[x]
singletonShape params = Shape (exactlyOne params)

-- | The exact point of each confirmed hard fork transition (old to new)
--
-- Unlike the 'Shape' of the chain, which is statically known, the 'Transitions'
-- are derived from the state of the ledger (hard fork transition points only
-- become known after a voting procedure).
--
-- Any transition listed here must be "certain". How certainty is established is
-- ledger dependent, but it should imply that this is no longer subject to
-- rollback.
data Transitions :: [Type] -> Type where
  -- | If the indices are, say, @'[Byron, Shelley, Goguen]@, then we can have
  -- have at most two transitions: one to Shelley, and one to Goguen. There
  -- cannot be a transition /to/ the initial ledger.
  Transitions :: AtMost xs EpochNo -> Transitions (x ': xs)

deriving instance Show (Transitions xs)

-- | No known transitions yet
transitionsUnknown :: Transitions (x ': xs)
transitionsUnknown = Transitions AtMostNil

{-------------------------------------------------------------------------------
  Constructing the summary

  NOTE: In practice, when using the hard fork combinator, we never ever call
  'summarize', and instead read off a summary from the 'HardForkState'. In
  that case, this serves primarily as a reference implementation.
-------------------------------------------------------------------------------}

-- | Construct hard fork 'Summary'
--
-- NOTE (on epoch to slot translation). In order to translate 'SlotNo' to
-- 'EpochNo', we simply "line up" all slots. For example, suppose we have
-- an initial 'EpochSize' of 10, and then an 'EpochSize' of 20 from 'EpochNo'
-- 3 onwards. We end up with something like
--
-- > Epoch | 0      | 1        | 2        | 3        | 4        | ..
-- > Slot  | 0 .. 9 | 10 .. 19 | 20 .. 29 | 30 .. 49 | 50 .. 69 | ..
--
-- We do this translation /independent/ from the 'minimumPossibleSlotNo'
-- for a particular ledger. This means that for ledgers where the
-- 'minimumPossibleSlotNo' is not zero (e.g., some ledgers might set it to 1),
-- the maximum number of blocks (aka filled slots) in an epoch is just 1 (or
-- more) less than the other epochs.
summarize :: WithOrigin SlotNo -- ^ Slot at the tip of the ledger
          -> Shape       xs
          -> Transitions xs
          -> Summary     xs
summarize ledgerTip = \(Shape shape) (Transitions transitions) ->
    Summary $ go initBound shape transitions
  where
    go :: Bound                          -- Lower bound for current era
       -> Exactly  (x ': xs) EraParams   -- params for all eras
       -> AtMost         xs  EpochNo     -- transitions
       -> NonEmpty (x ': xs) EraSummary
    -- CASE (ii) from 'EraParams' Haddock
    -- NOTE: Ledger tip might be close to the end of this era (or indeed past
    -- it) but this doesn't matter for the summary of /this/ era.
    go lo (ExactlyCons params ss) (AtMostCons epoch fs) =
        NonEmptyCons (EraSummary lo (EraEnd hi) params) $ go hi ss fs
      where
        hi = mkUpperBound params lo epoch
    -- CASE (i) or (iii) from 'EraParams' Haddock
    go lo (ExactlyCons params@EraParams{..} _) AtMostNil =
        NonEmptyOne (EraSummary lo hi params)
      where
        hi :: EraEnd
        hi = case eraSafeZone of
               UnsafeIndefiniteSafeZone ->
                   EraUnbounded
               StandardSafeZone safeFromTip ->
                   EraEnd
                 . mkUpperBound params lo
                 . slotToEpochBound params lo
                 . addSlots safeFromTip
                   -- If the tip is already in this era, safe zone applies from the
                   -- ledger tip (CASE (i) from 'EraParams' Haddock). If the ledger
                   -- tip is in the /previous/ era, but the transition to /this/ era
                   -- is already known, the safe zone applies from the start of this
                   -- era (CASE (iii) from 'EraParams' Haddock).
                   --
                   -- NOTE: The upper bound is /exclusive/:
                   --
                   -- o Suppose the ledger tip is at slot 10, and 'safeFromTip' is 2.
                   --   Then we should be able to make accurate predictions for slots
                   --   10 (of course), as well as (the safe zone) slots 11 and 12.
                   --   Since the upper bound is /exclusive/, this means that the
                   --   upper bound becomes 13. (Case i)
                   -- o If the ledger tip is in the previous era (case iii), and the
                   --   start of this era is slot 100, then we should be able to
                   --   give accurate predictions for the first two slots in this era
                   --   (100 and 101), and the upper bound becomes 102.
                   --
                   -- This explains the use of the extra addition ('next') for
                   -- case (i) but not for case (iii).
                 $ max (next ledgerTip) (boundSlot lo)

    -- Upper bound is exclusive, so we count from the /next/ ledger tip
    next :: WithOrigin SlotNo -> SlotNo
    next Origin        = SlotNo 0
    next (NotOrigin s) = succ s

{-------------------------------------------------------------------------------
  Invariants
-------------------------------------------------------------------------------}

-- | Check 'Shape' invariants
--
-- The only part of the 'Shape' that must make sense is the 'safeBeforeEpoch'
-- values (they must be strictly increasing).
--
-- NOTE: We assume eras cannot be empty. This will be satisfied by any ledger
-- we are interested in since transitions must be voted on (safe zones will
-- be non-empty).
invariantShape :: Shape xs -> Except String ()
invariantShape = \(Shape shape) ->
    go (EpochNo 0) shape
  where
    go :: EpochNo -- Lower bound on the start of the era
       -> Exactly xs EraParams -> Except String ()
    go _           ExactlyNil                    = return ()
    go lowerBound (ExactlyCons _ shape') =
        let nextLowerBound = addEpochs 1 lowerBound
        in go nextLowerBound shape'

-- | Check 'Summary' invariants
invariantSummary :: Summary xs -> Except String ()
invariantSummary = \(Summary summary) ->
    -- Pretend the start of the first era is the "end of the previous" one
    go (eraStart (nonEmptyHead summary)) (toList summary)
  where
    go :: Bound   -- ^ End of the previous era
       -> [EraSummary] -> Except String ()
    go _       []                  = return ()
    go prevEnd (curSummary : next) = do
        unless (curStart == prevEnd) $
          throwError $ mconcat [
              "Bounds don't line up: end of previous era "
            , show prevEnd
            , " /= start of current era "
            , show curStart
            ]

        case mCurEnd of
          EraUnbounded ->
            unless (null next) $
              throwError "Unbounded non-final era"
          EraEnd curEnd -> do
            -- Check the invariants mentioned at 'EraSummary'
            --
            -- o @epochsInEra@ corresponds to @e' - e@
            -- o @slotsInEra@ corresponds to @(e' - e) * epochSize)@
            -- o @timeInEra@ corresponds to @((e' - e) * epochSize * slotLen@
            --   which, if INV-1b holds, equals @(s' - s) * slotLen@
            let epochsInEra, slotsInEra :: Word64
                epochsInEra = countEpochs (boundEpoch curEnd) (boundEpoch curStart)
                slotsInEra  = epochsInEra * unEpochSize (eraEpochSize curParams)

                timeInEra :: NominalDiffTime
                timeInEra = fromIntegral slotsInEra
                          * getSlotLength (eraSlotLength curParams)

            unless (boundEpoch curEnd > boundEpoch curStart) $
              throwError "Empty era"

            unless (boundSlot curEnd == addSlots slotsInEra (boundSlot curStart)) $
              throwError $ mconcat [
                  "Invalid final boundSlot in "
                , show curSummary
                , " (INV-1b)"
                ]

            unless (boundTime curEnd == addRelTime timeInEra (boundTime curStart)) $
              throwError $ mconcat [
                  "Invalid final boundTime in "
                , show curSummary
                , " (INV-2b)"
                ]

            go curEnd next
      where
        curStart  :: Bound
        mCurEnd   :: EraEnd
        curParams :: EraParams
        EraSummary curStart mCurEnd curParams = curSummary

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise Bound where
  encode Bound{..} = mconcat [
        encodeListLen 3
      , encode boundTime
      , encode boundSlot
      , encode boundEpoch
      ]

  decode = do
      enforceSize "Bound" 3
      boundTime  <- decode
      boundSlot  <- decode
      boundEpoch <- decode
      return Bound{..}

instance Serialise EraEnd where
  encode EraUnbounded   = encodeNull
  encode (EraEnd bound) = encode bound

  decode = peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        return EraUnbounded
      _ -> EraEnd <$> decode

instance Serialise EraSummary where
  encode EraSummary{..} = mconcat [
        encodeListLen 3
      , encode eraStart
      , encode eraEnd
      , encode eraParams
      ]

  decode = do
      enforceSize "EraSummary" 3
      eraStart  <- decode
      eraEnd    <- decode
      eraParams <- decode
      return EraSummary{..}

instance SListI xs => Serialise (Summary xs) where
  encode (Summary eraSummaries) = encode (toList eraSummaries)

  -- @xs@ is the list of eras that is statically known to us; the server has a
  -- similar list @ys@ of eras that /it/ statically knows about. We do not know
  -- what @ys@ is here, but we can nonetheless reason about how @|xs|@ and
  -- @|ys|@ might relate:
  --
  -- - @|xs| == |ys|@: this is the normal case; we and the server know about the
  --   same (number of) eras. No special care needs to be taken.
  --
  -- - @|xs| > |ys|@: we know about more eras than the server does. The server
  --   will send us era summaries for @1 <= n <= |ys|@ eras. For sure @n <
  --   |xs|@, so decoding will be unproblematic. The only slightly strange case
  --   is when @n == |ys|@: in this case, the server thinks we are in the final
  --   era, whereas in fact that era isn't actually final; consequently, the
  --   server will give us an unbounded summary for that "final" era. However,
  --   if we are following that particular server, treating that era as
  --   unbounded is okay, since we anyway won't be transitioning to the next
  --   era.
  --
  --   [TODO: Won't we be making any decisions that we might regret if we do
  --   eventually switch server..?]
  --
  -- - @|xs| < |ys|@: we know about fewer eras than the server does. This will
  --   happen when the server has been upgraded for the next hard fork, but the
  --   client hasn't yet. Pattern match on the number @n@ of eras that the
  --   server sends us summaries for:
  --
  --   o @n < |xs|@: Although the server knows about more eras than us, they
  --     actually only send us era summaries for fewer eras than we know about.
  --     This means that the transition to what _we_ believe is the final era is
  --     not yet known; the summary sent to us by the server is fine as is.
  --
  --   o @n == |xs|@: The server does not yet know about the transition out of
  --     what (we believe to be) the final era. In principle we could decide to
  --     leave the era summaries as-is; however, since _we_ consider that era to
  --     be the final one, we should regard it as unbounded (it does not make
  --     sense to have a bounded final era). We must therefore modify the final
  --     era summary. Of course this will mean that we will make some incorrect
  --     decisions; but as long as we aren't updated, we will anyway be unable
  --     to deal with the next era.
  --
  --   o @n > |xs|@: the server already knows about the transition to the next
  --     era after our final era. In this case we must drop all eras that we
  --     don't know about, and then modify again the final era to be unbounded,
  --     just like in the case above.
  --
  -- Since we do not know @|ys|@, we cannot actually implement the outermost
  -- case statement. However:
  --
  -- - If @|xs| > |ys|@, by definition @n < |xs|@, and hence we will not modify
  --   the era summary: this is what we wanted.
  --
  -- - If @|xs| == |ys|@, then at most @n == |xs|@, in which case we might
  --   "modify" the final era to be unbounded. But in this case, the server will
  --   consider that era to be final as well, and so it will _already_ be
  --   unbounded: effectively this means that this means we will leave the
  --   summary unchanged.
  decode = do
      -- Drop all eras we don't know about
      eraSummaries <- take nbXs <$> decode

      let n = length eraSummaries
          go
            -- @n == |xs|@
            | n == nbXs = fixEndBound
            -- @n <  |xs|@
            | otherwise = id

      case Summary . go <$> nonEmptyFromList eraSummaries of
        Just summary -> return summary
        Nothing      -> fail "Summary: expected at least one era summary"
    where
      -- @|xs|@
      nbXs :: Int
      nbXs = lengthSList (Proxy @xs)

      -- | Make the last era's end bound unbounded.
      fixEndBound :: NonEmpty xs' EraSummary -> NonEmpty xs' EraSummary
      fixEndBound (NonEmptyCons e es) = NonEmptyCons e (fixEndBound es)
      fixEndBound (NonEmptyOne  e)    = NonEmptyOne  e { eraEnd = EraUnbounded }
