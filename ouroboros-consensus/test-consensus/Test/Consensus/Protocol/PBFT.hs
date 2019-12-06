{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Test.Consensus.Protocol.PBFT (
    tests
    -- * Used in the roundtrip tests
  , TestChainState(..)
  ) where

import qualified Control.Exception as Exn
import           Data.Coerce (coerce)
import           Data.Functor ((<&>))
import           Data.List (inits, tails)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import           Data.Word

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Crypto.DSIGN
import qualified Cardano.Prelude

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT.ChainState (EbbMap (..),
                     PBftChainState)
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS
import           Ouroboros.Consensus.Protocol.PBFT.Crypto
import           Ouroboros.Consensus.Util (lastMaybe, repeatedly)

import           Test.Util.Split (spanLeft, splitAtJust)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "PBftChainState" $
    [ testProperty "validGenerator"
               prop_validGenerator
    , testProperty "directABb"
               prop_directABb
    , testProperty "appendIsMonoidAction"
               prop_appendIsMonoidAction
    , testProperty "appendPreservesInvariant"
               prop_appendPreservesInvariant
    , testProperty "rewindPreservesInvariant"
               prop_rewindPreservesInvariant
    , testProperty "rewindReappendId"
               prop_rewindReappendId
    , testProperty "appendOldStatePreservesInvariant"
               prop_appendOldStatePreservesInvariant
    , testProperty "appendOldStateRestoresPreWindow"
               prop_appendOldStateRestoresPreWindow
    ]

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

-- | A generated test scenario
--
-- We don't want to make too many assumptions here, so instead we model what
-- happens on an actual chain: we build chain states by starting from genesis
-- and iterating amongst the following actions.
--
--   * Append an actual block signed by a key, via 'CS.append'.
--   * Append an epoch boundary block (EBB), via 'CS.appendEBB'.
--   * Rewind the chain state to a previous slot, via 'CS.rewind'.
--
-- A rewind is expected to fail in two scenarios.
--
--   * When the slot is so far back that we would be discarding more than @k@
--     signed blocks.
--
--   * When no block (neither actual nor EBB) was appended in that exact slot.
--
-- Rewinding to a slot @s2@ that only contained an EBB should behave similarly
-- to rewinding to the latest /signed slot/ @s1 < s2@ or to 'Origin' if there
-- isn't one. More specifically, it should discard the same signed blocks
-- (those in slots @> s1@) but still retain information about the subsequent
-- EBBs in slots in the inclusive range from @s1@ to @s2@.
--
-- NOTE: PBftChainState assumes @k >= 1@.
--
-- * 'testChainInputsA', 'testChainInputsB', and 'testChainInputsC':
--
-- We first generate three segments of 'Inputs'.
--
-- >                  B
-- >              ---------
-- >       A     /
-- >   ---------<
-- >             \
-- >              -------------------
-- >                       C
--
-- The slot of the first input of B will be greater than the slot of the last
-- input of A. The segment B has @<= k@ signed blocks, since we intend to
-- successfully rewind past it. The segment C has exactly @n + k@ signed
-- blocks, since that's the most we'd need to append to ensure an incomplete
-- window arising pre-#1307 will then be filled. The segment A has an
-- effectively arbitrary number of signed blocks.
--
-- * 'testChainState':
--
-- The test properties themselves focus on the state @csABb@ defined by:
--
-- > csAB       = appendInputs (inA <> inB) empty
-- > Just csABb = rewind (slotLatestInput (lastInput inA)) csAB
--
-- Segment A could be empty or have several times @k@ signed blocks interleaved
-- with any number of EBBs. Segment B has @<= k@ signed blocks interleaved with
-- any number of EBBs. Thus @csABb@, the result of appending both and the
-- rewinding back to the end of A, should be a representative sample of the
-- space of chain states.
--
-- * 'testChainOldState':
--
-- We also test that the state analogous to @csABb@ that would have arisen
-- before PR #1307 is supported as an input to the current code as expected.
-- (This exercises the legacy-support burden.)
--
-- In particular, appending a sufficient prefix of segment C will always
-- restore the invariants that were not ensured before PR #1307.
--
-- * 'testChainRewind':
--
-- Since @csABb@ is fully representative of a realistic state, we will test an
-- arbitrary rewind from it.
data TestChainState = TestChainState {
      testChainStateK        :: SecurityParam
    , testChainStateN        :: CS.WindowSize
    , testChainStateNumKeys  :: Int

      -- | The segment A
    , testChainInputsA       :: Inputs PBftMockCrypto

      -- | The segment B
      --
      -- INVARIANT this segment has @<= k@ signed blocks
    , testChainInputsB       :: Inputs PBftMockCrypto

      -- | The segment C
      --
      -- INVARIANT this segment has exactly @n + k@ signed blocks
    , testChainInputsC       :: Inputs PBftMockCrypto

      -- | @csABb@
    , testChainState         :: PBftChainState PBftMockCrypto

      -- | The @csABb@ we would have deserialised before #1307: this state does
      -- not include the @n@ pre-anchor signatures.
    , testChainOldState      :: PBftChainState PBftMockCrypto

      -- | The slot of some input within segment A
    , testChainRewind        :: WithOrigin SlotNo

      -- | The inputs in segment A occupying slots @> 'testChainRewind'@
    , testChainRewoundInputs :: Inputs PBftMockCrypto
    }
  deriving (Show)

instance Arbitrary TestChainState where
  arbitrary = genTestChainState

genTestChainState :: Gen TestChainState
genTestChainState = do
    k       <- choose (1, 4)    -- security parameter
    n       <- choose (1, 10)   -- PBFT window size
    numKeys <- choose (1, 4)    -- number of core nodes

    let paramK = SecurityParam k
        paramN = CS.WindowSize n
        genKey = genMockKey numKeys

    -- Pick number of signed slots
    --
    -- We don't try to be too clever here; we need to test the various edge
    -- cases near genesis more carefully, but also want to test where we are
    -- well past genesis
    numSignersAB <- oneof [
        choose (0, k + 1)
      , choose (0, n + 1)
      , choose (0, k + n + 1)
      , choose (0, 5 * (k + n))
      ]

    -- Divide those signers between the A and B segments
    (numSignersA, numSignersB) <- do
        -- since we plan to (successfully) rewind B, it cannot contain more
        -- than k signers
        numSignersB <- choose (0, min k numSignersAB)
        pure (numSignersAB - numSignersB, numSignersB)

    -- Generate all the inputs

    (inA, inB, inC) <- do
        inA <- generateInputs genKey numSignersA (LatestInput Nothing)

        -- Segment B must not begin with the same slot that A ended with,
        -- because rewinds can't split a slot and our tests focus on the result
        -- of a rewind that can be undone by re-applying B.
        let lastInputOfA' = LatestInput $ case unLatestInput $ toLastInput inA of
                Nothing              -> Nothing
                Just (InputSigner x) -> Just $ InputSigner x
                Just (InputEBB x)    -> Just $ InputEBB $ tick x
              where
                tick (PBftEBB prev slot) = PBftEBB prev (succ slot)

        inB <- generateInputs genKey numSignersB lastInputOfA'
        inC <- generateInputs genKey (n + k)     (toLastInput inA)

        pure (inA, inB, inC)

    -- Calculate @csABb@ directly

    -- The @anchor@ is the earliest slot to which we can rewind after appending
    -- A and B; any earlier would violate the security parameter k.
    let (mbPre, _) = splitAtSigner (numSignersAB .- k) $ inA <> inB
        anchor = case mbPre of
                   Nothing     -> Origin
                   Just (_, x) -> At $ CS.pbftSignerSlotNo x

    let newABb = fromInputs paramK paramN anchor
                   (signatureInputs inps)
                   (ebbInputs inps)
          where
            inps = snd $ splitAtSigner (numSignersA .- ((n + k) .- numSignersB)) inA

        -- the state before PR #1307 didn't retain as many signatures and
        -- didn't track EBBs at all
        oldABb = fromInputs paramK paramN anchor
                   (signatureInputs inps)
                   mempty
          where
            inps = snd $ splitAtSigner (numSignersA .- (n .- numSignersB)) inA

    -- Generate a second rewind that drops a suffix of A

    numSignersSuffixA <- oneof [
        choose (0, k .- numSignersB) -- rollback that will succeed
      , choose (0, numSignersA) -- rollback that might fail (too far)
      ]

    let (mbAPrefix, inSuffixA) = splitAtSigner (numSignersA .- numSignersSuffixA) inA
        -- if the rollback succeeds, its new window ends with this signed slot
        signedTarget = case mbAPrefix of
                         Nothing     -> Origin
                         Just (_, x) -> At $ CS.pbftSignerSlotNo x

    -- Pick a slot to rewind to
    --
    -- appending the @rewoundInputs@ will undo the rewind to @rewindSlot@
    (rewindSlot, rewoundInputs) <- elements $
        [ Exn.assert (unInputs inSuffixA == int <> tal) $
          ( maybe signedTarget (At . slotInput) $ lastMaybe int
          , Inputs tal
          )
        | (int, tal) <-
            inits (unInputs inSuffixA) `zip` tails (unInputs inSuffixA)
          -- this is ultimately picking a /slot/, so don't split between two
          -- inputs that have the same slot
        , case ( slotInput <$> lastMaybe int
               , slotInput <$> Cardano.Prelude.head tal
               ) of
            (Just l, Just r) -> l /= r
            _                -> True
        ]

    pure TestChainState {
        testChainStateK        = paramK
      , testChainStateN        = paramN
      , testChainStateNumKeys  = numKeys
      , testChainState         = newABb
      , testChainOldState      = oldABb
      , testChainInputsA       = inA
      , testChainInputsB       = inB
      , testChainInputsC       = inC
      , testChainRewind        = rewindSlot
      , testChainRewoundInputs = rewoundInputs
      }

{-------------------------------------------------------------------------------
  Generating append inputs
-------------------------------------------------------------------------------}

genMockKey :: Int -> Gen (VerKeyDSIGN MockDSIGN)
genMockKey numKeys = VerKeyMockDSIGN <$> choose (1, numKeys)

-- | Generate a possibly empty sequence of 'InputEBB's and 'InputSigner's
--
-- POSTCONDITION The output contains exactly the specified number of
-- 'InputSigner's.
generateInputs
  :: forall c.
     Gen (PBftVerKeyHash c)
  -> Word64
  -> LatestInput c
  -> Gen (Inputs c)
generateInputs genKey = go []
  where
    go :: [Input c] -> Word64 -> LatestInput c -> Gen (Inputs c)
    go acc n lastInput = do
        inp <- frequency [(1, genEBB lastInput), (9, genSigner lastInput)]

        let loop n' = go (coerce (inp:) acc) n' (LatestInput $ Just inp)
        case inp of
            InputEBB{}                -> loop n
            InputSigner{} | 0 == n    -> pure $ Inputs $ reverse $ acc
                          | otherwise -> loop $ n - 1

    -- generate a slot >= the given slot
    genSlot slot = choose (0, 3) <&> \w -> slot + SlotNo w

    -- an EBB never has the same slot as its predecessor
    genEBB lastInput = do
      slot <- genSlot $ case unLatestInput lastInput of
          Nothing  -> 0
          Just inp -> succ $ slotInput inp
      pure $ InputEBB $ PBftEBB (signedSlotLatestInput lastInput) slot

    -- a signed slot can share the slot with an immediately preceding EBB
    genSigner lastInput = do
      slot <- genSlot $ case unLatestInput lastInput of
          Nothing                -> 0
          Just inp@InputEBB{}    -> slotInput inp
          Just inp@InputSigner{} -> succ $ slotInput inp
      key  <- genKey
      pure $ InputSigner $ CS.PBftSigner slot key

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

data ClassifyWindow =
    WindowEmpty
  | WindowNotFull
  | WindowFull
  deriving (Show)

data ClassifyRewind =
    RewindImpossible
  | RewindLimited
  | RewindMaximum
  deriving (Show)

data ClassifyLastA =
    EbbLastA
  | OriginLastA
  | SignedLastA
  deriving (Show)

classifyWindow :: TestChainState -> ClassifyWindow
classifyWindow TestChainState{..}
  | size inWindow == (0 :: Int)                       = WindowEmpty
  | size inWindow <  CS.getWindowSize testChainStateN = WindowNotFull
  | otherwise                                         = WindowFull
  where
    CS.PBftChainState{..} = testChainState

classifyRewind :: TestChainState -> ClassifyRewind
classifyRewind TestChainState{..}
  | size postAnchor == (0 :: Int)                   = RewindImpossible
  | size postAnchor <  maxRollbacks testChainStateK = RewindLimited
  | otherwise                                       = RewindMaximum
  where
    CS.PBftChainState{..} = testChainState

classifyLastA :: TestChainState -> ClassifyLastA
classifyLastA TestChainState{..} =
  case unLatestInput $ toLastInput testChainInputsA of
    Nothing            -> OriginLastA
    Just InputEBB{}    -> EbbLastA
    Just InputSigner{} -> SignedLastA

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Check that we are producing valid 'PBftChainState's
prop_validGenerator :: TestChainState -> Property
prop_validGenerator st@TestChainState{..} =
    collect (classifyWindow st) $
    collect (classifyRewind st) $
    collect (classifyLastA  st) $
    Right () === CS.invariant
                   testChainStateK
                   testChainStateN
                   testChainState
    .&&.
    Right () === CS.invariant
                   testChainStateK
                   testChainStateN
                   testChainOldState
    .&&.
    Seq.null (CS.preWindow testChainOldState)
  where
    CS.PBftChainState{..} = testChainState

-- | Our direct calculation of @csABb@ matches the result of the corresponding
-- appends and a rewind.
prop_directABb :: TestChainState -> Property
prop_directABb TestChainState{..} =
    let k = testChainStateK
        n = testChainStateN
        state0 = CS.empty
        state1 = appendInputs k n
                   (testChainInputsA <> testChainInputsB)
                   state0
        mbState2 = CS.rewind k n
                     (slotLatestInput $ toLastInput testChainInputsA)
                     state1
    in Just testChainState === mbState2

-- | 'appendInputs' realizes 'Inputs' as a monoid action on chain states.
prop_appendIsMonoidAction :: TestChainState -> Property
prop_appendIsMonoidAction TestChainState{..} =
    let act = appendInputs testChainStateK testChainStateN
        inA = testChainInputsA
        inB = testChainInputsB

        fo = flip (.)

        a1 = act $     inA  <>      inB
        a2 =       act inA `fo` act inB
    in
      a1 CS.empty === a2 CS.empty
      .&&.
      act mempty CS.empty === CS.empty

-- | Appending preserves the invariant.
prop_appendPreservesInvariant :: TestChainState -> Property
prop_appendPreservesInvariant TestChainState{..} =
    let state' = headInput testChainInputsC <&> \inp -> appendInput
                   testChainStateK
                   testChainStateN
                   inp
                   testChainState
    in (Just (Right ()) ===) $
       CS.invariant testChainStateK testChainStateN <$> state'

-- | Rewinding either fails or preserves the invariant.
prop_rewindPreservesInvariant :: TestChainState -> Property
prop_rewindPreservesInvariant TestChainState{..} =
    let rewound = CS.rewind
                    testChainStateK
                    testChainStateN
                    testChainRewind
                    testChainState
    in case rewound of
         Nothing     -> label "rollback too far in the past" True
         Just state' -> label "rollback succeeded" $
           Right () === CS.invariant
                          testChainStateK
                          testChainStateN
                          state'

-- | If we successfully rewind a chain state to before a segment of inputs and
-- then reapply those inputs, we should get that original chain state back.
prop_rewindReappendId :: TestChainState -> Property
prop_rewindReappendId TestChainState{..} =
    let rewound = CS.rewind
                    testChainStateK
                    testChainStateN
                    testChainRewind
                    testChainState
    in case rewound of
         Nothing     -> label "rollback too far in the past" True
         Just state' -> label "rollback succeeded" $
           counterexample ("Rewound: " <> show state') $
           testChainState === appendInputs
                                testChainStateK
                                testChainStateN
                                testChainRewoundInputs
                                state'

-- | 'prop_appendPreservesInvariant' holds for the old chain state too
prop_appendOldStatePreservesInvariant :: TestChainState -> Property
prop_appendOldStatePreservesInvariant TestChainState{..} =
    let state' = headInput testChainInputsC <&> \inp -> appendInput
                   testChainStateK
                   testChainStateN
                   inp
                   testChainOldState
    in (Just (Right ()) ===) $
       CS.invariant testChainStateK testChainStateN <$> state'

-- | The old pre-#1307 state will have a 'CS.preWindow' of @k@ again once we
-- add a sufficient number signed blocks (and any number of EBBs).
prop_appendOldStateRestoresPreWindow :: TestChainState -> Property
prop_appendOldStateRestoresPreWindow TestChainState{..} =
    let missing = fromIntegral
                $ maxRollbacks       testChainStateK
                + CS.getWindowSize   testChainStateN
                - CS.countSignatures testChainOldState
        inps = pre' <> post'
          where
            (mbPre, post) = splitAtSigner missing testChainInputsC

            -- the prefix with @missing@-many signers
            pre' = case mbPre of
              Nothing       -> mempty
              Just (pre, b) -> pre <> Inputs [InputSigner b]

            -- the immediately subsequent EBBs
            post' = Inputs $ map InputEBB $ fst $ spanEBBs post
        state' = appendInputs
                   testChainStateK
                   testChainStateN
                   inps
                   testChainOldState
    in Right () === CS.invariant
                      testChainStateK
                      testChainStateN
                      state'
       .&&.
       size (CS.preWindow state') === maxRollbacks testChainStateK

{-------------------------------------------------------------------------------
  ChainState "Inputs"
-------------------------------------------------------------------------------}

data PBftEBB = PBftEBB
  { pbftEbbPrev   :: !(WithOrigin SlotNo)
    -- ^ the preceding signed slot
  , pbftEbbSlotNo :: !SlotNo
    -- ^ the EBB's own slot
  }
  deriving (Eq, Show)

data Input c
  = InputEBB    !PBftEBB
  | InputSigner !(CS.PBftSigner c)
  deriving (Eq, Show)

newtype Inputs c = Inputs {unInputs :: [Input c]}
  deriving (Eq, Monoid, Semigroup, Show)

signatureInputs :: Inputs c -> [CS.PBftSigner c]
signatureInputs (Inputs inps) = [ x | InputSigner x <- inps ]

ebbInputs :: Inputs c -> [PBftEBB]
ebbInputs (Inputs inps) = [ x | InputEBB x <- inps ]

slotInput :: Input c -> SlotNo
slotInput = \case
  InputEBB x    -> pbftEbbSlotNo x
  InputSigner x -> CS.pbftSignerSlotNo x

headInput :: Inputs c -> Maybe (Input c)
headInput = Cardano.Prelude.head . unInputs

appendInput
  :: PBftCrypto c
  => SecurityParam
  -> CS.WindowSize
  -> Input c
  -> CS.PBftChainState c -> CS.PBftChainState c
appendInput k n = \case
  InputEBB ebb       -> CS.appendEBB k n (pbftEbbSlotNo ebb)
  InputSigner signer -> CS.append k n signer

appendInputs
  :: PBftCrypto c
  => SecurityParam
  -> CS.WindowSize
  -> Inputs c
  -> CS.PBftChainState c -> CS.PBftChainState c
appendInputs k n = repeatedly (appendInput k n) . unInputs

splitAtSigner :: Word64 -> Inputs c -> (Maybe (Inputs c, CS.PBftSigner c), Inputs c)
splitAtSigner n (Inputs inps) =
    coerce $ splitAtJust prjSigner n inps
  where
    prjSigner :: Input c -> Maybe (CS.PBftSigner c)
    prjSigner = \case
      InputEBB{}    -> Nothing
      InputSigner x -> Just x

spanEBBs :: Inputs c -> ([PBftEBB], Inputs c)
spanEBBs (Inputs inps0) =
    (pre, post)
  where
    (pre, mbPost) = spanLeft prj inps0
      where
        prj = \case
          InputEBB x        -> Left x
          inp@InputSigner{} -> Right inp

    post = case mbPost of
      Nothing          -> mempty
      Just (inp, inps) -> Inputs $ inp : inps

-- | Wrapper around 'CS.fromList'
fromInputs
  :: PBftCrypto c
  => SecurityParam
  -> CS.WindowSize
  -> WithOrigin SlotNo
  -> [CS.PBftSigner c]
     -- ^ determines the window of signers
  -> [PBftEBB]
     -- ^ determines 'CS.ebbs'
  -> CS.PBftChainState c
fromInputs k n anchor signers ebbs =
    CS.fromList k n (anchor, Seq.fromList signers, EbbMap m)
  where
    m = Map.fromList [ (slot, mSlot) | PBftEBB mSlot slot <- ebbs ]

{-------------------------------------------------------------------------------
  "The previous input"
-------------------------------------------------------------------------------}

-- | Summary of an 'Inputs' /that/ /begins/ /at/ 'Origin'
newtype LatestInput c = LatestInput {unLatestInput :: Maybe (Input c)}

toLastInput :: Inputs c -> LatestInput c
toLastInput = LatestInput . lastMaybe . unInputs

-- | The slot of the latest block
slotLatestInput :: LatestInput c -> WithOrigin SlotNo
slotLatestInput (LatestInput mbInp) = case mbInp of
  Nothing  -> Origin
  Just inp -> At $ slotInput inp

-- | The slot of the latest /signed/ block
signedSlotLatestInput :: LatestInput c -> WithOrigin SlotNo
signedSlotLatestInput (LatestInput inp) = case inp of
  Nothing              -> Origin
  Just (InputEBB x)    -> pbftEbbPrev x
  Just (InputSigner x) -> At $ CS.pbftSignerSlotNo x

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | <https://en.wikipedia.org/wiki/Monus>
(.-) :: (Num a, Ord a) => a -> a -> a
a .- b = if a > b then a - b else 0

size :: Num b => Seq.StrictSeq a -> b
size = fromIntegral . Seq.length
