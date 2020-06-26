{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Consensus.Protocol.PBFT (
    tests
  ) where

import qualified Control.Exception as Exn
import           Data.Coerce (coerce)
import           Data.Functor ((<&>))
import           Data.List (inits, tails)
import           Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as Seq
import           Data.Word

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Crypto.DSIGN
import qualified Cardano.Prelude

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Protocol.PBFT.Crypto
import           Ouroboros.Consensus.Protocol.PBFT.State (EbbInfo (..),
                     MaybeEbbInfo (..), PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import           Ouroboros.Consensus.Protocol.PBFT.State.HeaderHashBytes
import           Ouroboros.Consensus.Util (lastMaybe, repeatedly)
import           Ouroboros.Consensus.Util.Orphans ()

import           Test.Util.Roundtrip (roundtrip)
import           Test.Util.Split (spanLeft, splitAtJust)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "PBftState" $
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
    , testProperty "serialisation roundtrip"
               prop_serialisation_roundtrip
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
-- NOTE: PBftState assumes @k >= 1@.
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
-- * 'TestPBftState':
--
-- The test properties themselves focus on the state @csABb@ defined by:
--
-- > csAB       = appendInputs (inA <> inB) empty
-- > Just csABb = rewind (pointLatestInput (lastInput inA)) csAB
--
-- Segment A could be empty or have several times @k@ signed blocks interleaved
-- with any number of EBBs. Segment B has @<= k@ signed blocks interleaved with
-- any number of EBBs. Thus @csABb@, the result of appending both and the
-- rewinding back to the end of A, should be a representative sample of the
-- space of chain states.
--
-- * 'testOldPBftState':
--
-- We also test that the state analogous to @csABb@ that would have arisen
-- before PR #1307 is supported as an input to the current code as expected.
-- (This exercises the legacy-support burden.)
--
-- In particular, appending a sufficient prefix of segment C will always
-- restore the invariants that were not ensured before PR #1307.
--
-- * 'testChainRewindPoint':
--
-- Since @csABb@ is fully representative of a realistic state, we will test an
-- arbitrary rewind from it.
data TestPBftState = TestPBftState {
      testPBftStateK         :: SecurityParam
    , testPBftStateN         :: S.WindowSize
    , testPBftStateNumKeys   :: Int

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
    , testPBftState          :: PBftState PBftMockCrypto

      -- | The @csABb@ we would have deserialised before #1307: this state does
      -- not include the @n@ pre-anchor signatures.
    , testOldPBftState       :: PBftState PBftMockCrypto

      -- | The slot of some input within segment A
    , testChainRewindPoint   :: WithOrigin (SlotNo, HeaderHashBytes)

      -- | The inputs in segment A after 'testChainRewindPoint'
    , testChainRewoundInputs :: Inputs PBftMockCrypto
    }
  deriving (Show)

instance Arbitrary TestPBftState where
  arbitrary = genTestPBftState

genTestPBftState :: Gen TestPBftState
genTestPBftState = do
    k       <- choose (1, 4)    -- security parameter
    n       <- choose (1, 10)   -- PBFT window size
    numKeys <- choose (1, 4)    -- number of core nodes

    let paramK = SecurityParam k
        paramN = S.WindowSize n
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
        inA <- generateInputs paramK paramN genKey numSignersA (LatestInput Nothing)

        -- Segment B must not begin with the same slot that A ended with,
        -- because rewinds can't split a slot and our tests focus on the result
        -- of a rewind that can be undone by re-applying B.
        let lastInputOfA' = LatestInput $ case unLatestInput $ toLastInput inA of
                Nothing              -> Nothing
                Just (InputSigner x) -> Just $ InputSigner x
                Just (InputEBB x)    -> Just $ InputEBB $ tick x
              where
                tick (PBftEBB prev slot) = PBftEBB prev (succ slot)

        inB <- generateInputs paramK paramN genKey numSignersB lastInputOfA'
        inC <- generateInputs paramK paramN genKey (n + k)     (toLastInput inA)

        pure (inA, inB, inC)

    -- Calculate @csABb@ directly

    -- The @anchor@ is the earliest slot to which we can rewind after appending
    -- A and B; any earlier would violate the security parameter k.
    let (mbPre, _) = splitAtSigner (numSignersAB .- k) $ inA <> inB
        anchor = case mbPre of
                   Nothing     -> Origin
                   Just (_, x) -> NotOrigin $ S.pbftSignerSlotNo x

    let newABb = fromInputs paramK paramN anchor
                   (signatureInputs inps)
                   (ebbInputs inps)
          where
            inps =
              snd $ splitAtSigner (numSignersA .- ((n + k) .- numSignersB)) inA

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

    let (mbAPrefix, inSuffixA) =
          splitAtSigner (numSignersA .- numSignersSuffixA) inA
        -- if the rollback succeeds, its new window ends with this signed point
        signedPoint = case mbAPrefix of
          Nothing     -> Origin
          Just (_, x) ->
            NotOrigin (S.pbftSignerSlotNo x, headerHashBytesInput (InputSigner x))

    -- Pick a point to rewind to
    --
    -- appending the @rewoundInputs@ will undo the rewind to @rewindSlot@
    (rewindPoint, rewoundInputs) <- elements $
        [ Exn.assert (unInputs inSuffixA == int <> tal) $
          ( case lastMaybe int of
              Nothing -> signedPoint
              Just x  -> NotOrigin (slotInput x, headerHashBytesInput x)
          , Inputs tal
          )
        | (int, tal) <-
            inits (unInputs inSuffixA) `zip` tails (unInputs inSuffixA)
        ]

    pure TestPBftState {
        testPBftStateK         = paramK
      , testPBftStateN         = paramN
      , testPBftStateNumKeys   = numKeys
      , testPBftState          = newABb
      , testOldPBftState       = oldABb
      , testChainInputsA       = inA
      , testChainInputsB       = inB
      , testChainInputsC       = inC
      , testChainRewindPoint   = rewindPoint
      , testChainRewoundInputs = rewoundInputs
      }

{-------------------------------------------------------------------------------
  Generating append inputs
-------------------------------------------------------------------------------}

genMockKey :: Int -> Gen (VerKeyDSIGN MockDSIGN)
genMockKey numKeys = VerKeyMockDSIGN <$> choose (1, fromIntegral numKeys)

-- | Generate a possibly empty sequence of 'InputEBB's and 'InputSigner's
--
-- POSTCONDITION The output contains exactly the specified number of
-- 'InputSigner's.
--
-- POSTCONDITION The 'InputEBB's in the output are separated by at least @n +
-- k@ signed blocks.
generateInputs
  :: forall c.
     SecurityParam
  -> S.WindowSize
  -> Gen (PBftVerKeyHash c)
  -> Word64
  -> LatestInput c
  -> Gen (Inputs c)
generateInputs paramK paramN genKey =
    \n lastInput -> post <$> go [] n lastInput
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
      pure $ InputSigner $ S.PBftSigner slot key

    -- remove EBBs that come too soon
    post :: Inputs c -> Inputs c
    post = Inputs . go2 0 . unInputs
      where
        lim = n + k
          where
            S.WindowSize  n = paramN
            SecurityParam k = paramK

        go2 numSigned = \case
          []       -> []
          inp:inps -> case inp of
            InputEBB{}
              | numSigned < lim ->       go2 numSigned        inps
              | otherwise       -> inp : go2 0                inps
            InputSigner{}       -> inp : go2 (succ numSigned) inps

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

classifyWindow :: TestPBftState -> ClassifyWindow
classifyWindow TestPBftState{..}
  | size inWindow == (0 :: Int)                     = WindowEmpty
  | size inWindow <  S.getWindowSize testPBftStateN = WindowNotFull
  | otherwise                                       = WindowFull
  where
    S.PBftState{..} = testPBftState

classifyRewind :: TestPBftState -> ClassifyRewind
classifyRewind TestPBftState{..}
  | size postAnchor == (0 :: Int)                  = RewindImpossible
  | size postAnchor <  maxRollbacks testPBftStateK = RewindLimited
  | otherwise                                      = RewindMaximum
  where
    S.PBftState{..} = testPBftState

classifyLastA :: TestPBftState -> ClassifyLastA
classifyLastA TestPBftState{..} =
  case unLatestInput $ toLastInput testChainInputsA of
    Nothing            -> OriginLastA
    Just InputEBB{}    -> EbbLastA
    Just InputSigner{} -> SignedLastA

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Check that we are producing valid 'PBftState's
prop_validGenerator :: TestPBftState -> Property
prop_validGenerator st@TestPBftState{..} =
    collect (classifyWindow st) $
    collect (classifyRewind st) $
    collect (classifyLastA  st) $
    Right () === S.invariant
                   testPBftStateK
                   testPBftStateN
                   testPBftState
    .&&.
    Right () === S.invariant
                   testPBftStateK
                   testPBftStateN
                   testOldPBftState
    .&&.
    Seq.null (S.preWindow testOldPBftState)
  where
    S.PBftState{..} = testPBftState

-- | Our direct calculation of @csABb@ matches the result of the corresponding
-- appends and a rewind.
prop_directABb :: TestPBftState -> Property
prop_directABb TestPBftState{..} =
    let k = testPBftStateK
        n = testPBftStateN
        state0 = S.empty
        state1 = appendInputs k n
                   (testChainInputsA <> testChainInputsB)
                   state0
        mbState2 = S.rewind k n
                     (pointLatestInput $ toLastInput testChainInputsA)
                     state1
    in Just testPBftState === mbState2

-- | 'appendInputs' realizes 'Inputs' as a monoid action on chain states.
prop_appendIsMonoidAction :: TestPBftState -> Property
prop_appendIsMonoidAction TestPBftState{..} =
    let act = appendInputs testPBftStateK testPBftStateN
        inA = testChainInputsA
        inB = testChainInputsB

        fo = flip (.)

        a1 = act $     inA  <>      inB
        a2 =       act inA `fo` act inB
    in
      a1 S.empty === a2 S.empty
      .&&.
      act mempty S.empty === S.empty

-- | Appending preserves the invariant.
prop_appendPreservesInvariant :: TestPBftState -> Property
prop_appendPreservesInvariant TestPBftState{..} =
    let state' = headInput testChainInputsC <&> \inp -> appendInput
                   testPBftStateK
                   testPBftStateN
                   inp
                   testPBftState
    in (Just (Right ()) ===) $
       S.invariant testPBftStateK testPBftStateN <$> state'

-- | Rewinding either fails or preserves the invariant.
prop_rewindPreservesInvariant :: TestPBftState -> Property
prop_rewindPreservesInvariant TestPBftState{..} =
    let rewound = S.rewind
                    testPBftStateK
                    testPBftStateN
                    testChainRewindPoint
                    testPBftState
    in case rewound of
         Nothing     -> label "rollback too far in the past" True
         Just state' -> label "rollback succeeded" $
           Right () === S.invariant
                          testPBftStateK
                          testPBftStateN
                          state'

-- | If we successfully rewind a chain state to before a segment of inputs and
-- then reapply those inputs, we should get that original chain state back.
prop_rewindReappendId :: TestPBftState -> Property
prop_rewindReappendId TestPBftState{..} =
    let rewound = S.rewind
                    testPBftStateK
                    testPBftStateN
                    testChainRewindPoint
                    testPBftState
    in case rewound of
         Nothing     -> label "rollback too far in the past" True
         Just state' -> label "rollback succeeded" $
           counterexample ("Rewound: " <> show state') $
           testPBftState === appendInputs
                                testPBftStateK
                                testPBftStateN
                                testChainRewoundInputs
                                state'

-- | 'prop_appendPreservesInvariant' holds for the old chain state too
prop_appendOldStatePreservesInvariant :: TestPBftState -> Property
prop_appendOldStatePreservesInvariant TestPBftState{..} =
    let state' = headInput testChainInputsC <&> \inp -> appendInput
                   testPBftStateK
                   testPBftStateN
                   inp
                   testOldPBftState
    in (Just (Right ()) ===) $
       S.invariant testPBftStateK testPBftStateN <$> state'

-- | The old pre-#1307 state will have a 'CS.preWindow' of @k@ again once we
-- add a sufficient number signed blocks (and any number of EBBs).
prop_appendOldStateRestoresPreWindow :: TestPBftState -> Property
prop_appendOldStateRestoresPreWindow TestPBftState{..} =
    let missing = maxRollbacks      testPBftStateK
                + S.getWindowSize   testPBftStateN
                - S.countSignatures testOldPBftState
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
                   testPBftStateK
                   testPBftStateN
                   inps
                   testOldPBftState
    in Right () === S.invariant
                      testPBftStateK
                      testPBftStateN
                      state'
       .&&.
       size (S.preWindow state') === maxRollbacks testPBftStateK

{-------------------------------------------------------------------------------
  Serialisation roundtrip
-------------------------------------------------------------------------------}

-- | We test the roundtrip using mock crypto
prop_serialisation_roundtrip :: TestPBftState -> Property
prop_serialisation_roundtrip TestPBftState{..} =
    roundtrip
      (S.encodePBftState)
      (S.decodePBftState testPBftStateK testPBftStateN)
      testPBftState

{-------------------------------------------------------------------------------
  PBftState "Inputs"
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
  | InputSigner !(S.PBftSigner c)
  deriving (Eq, Show)

newtype Inputs c = Inputs {unInputs :: [Input c]}
  deriving (Eq, Monoid, Semigroup, Show)

signatureInputs :: Inputs c -> [S.PBftSigner c]
signatureInputs (Inputs inps) = [ x | InputSigner x <- inps ]

ebbInputs :: Inputs c -> [PBftEBB]
ebbInputs (Inputs inps) = [ x | InputEBB x <- inps ]

slotInput :: Input c -> SlotNo
slotInput = \case
  InputEBB x    -> pbftEbbSlotNo x
  InputSigner x -> S.pbftSignerSlotNo x

headInput :: Inputs c -> Maybe (Input c)
headInput = Cardano.Prelude.head . unInputs

appendInput
  :: PBftCrypto c
  => SecurityParam
  -> S.WindowSize
  -> Input c
  -> S.PBftState c -> S.PBftState c
appendInput k n inp = case inp of
  InputEBB{}         ->
    S.appendEBB k n (slotInput inp) (headerHashBytesInput inp)
  InputSigner signer -> S.append k n signer

appendInputs
  :: PBftCrypto c
  => SecurityParam
  -> S.WindowSize
  -> Inputs c
  -> S.PBftState c -> S.PBftState c
appendInputs k n = repeatedly (appendInput k n) . unInputs

splitAtSigner
  :: Word64 -> Inputs c -> (Maybe (Inputs c, S.PBftSigner c), Inputs c)
splitAtSigner n (Inputs inps) =
    coerce $ splitAtJust prjSigner n inps
  where
    prjSigner :: Input c -> Maybe (S.PBftSigner c)
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

-- | Wrapper around 'S.fromList'
fromInputs
  :: PBftCrypto c
  => SecurityParam
  -> S.WindowSize
  -> WithOrigin SlotNo
  -> [S.PBftSigner c]
     -- ^ determines the window of signers
  -> [PBftEBB]
     -- ^ determines 'S.ebbs'
  -> S.PBftState c
fromInputs k n anchor signers ebbs0 =
    S.fromList k n (anchor, Seq.fromList signers, ebbs2)
  where
    ebbs1 =
        [ mkEbbInfo slot mSlot
        | PBftEBB mSlot slot <- ebbs0
        , NotOrigin slot == anchor || mSlot >= anchor
        ]
    ebbs2 = case lastMaybe ebbs1 of
      Nothing -> NothingEbbInfo
      Just ei -> JustEbbInfo ei

    mkEbbInfo slot mSlot = EbbInfo
      { eiSlot      = slot
      , eiHashBytes = headerHashBytesInput $ InputEBB $ PBftEBB mSlot slot
      , eiPrevSlot  = mSlot
      }

type instance HeaderHash (Input c) = (Bool, SlotNo)

headerHashBytesInput :: forall c. Input c -> HeaderHashBytes
headerHashBytesInput inp =
    headerHashBytes (Proxy :: Proxy (Input c)) (flag, slotInput inp)
  where
    flag = case inp of
      InputEBB{}    -> True
      InputSigner{} -> False

{-------------------------------------------------------------------------------
  "The previous input"
-------------------------------------------------------------------------------}

-- | Summary of an 'Inputs' /that/ /begins/ /at/ 'Origin'
newtype LatestInput c = LatestInput {unLatestInput :: Maybe (Input c)}

toLastInput :: Inputs c -> LatestInput c
toLastInput = LatestInput . lastMaybe . unInputs

-- | The slot of the latest block
pointLatestInput :: LatestInput c -> WithOrigin (SlotNo, HeaderHashBytes)
pointLatestInput (LatestInput mbInp) = case mbInp of
  Nothing  -> Origin
  Just inp -> NotOrigin (slotInput inp, headerHashBytesInput inp)

-- | The slot of the latest /signed/ block
signedSlotLatestInput :: LatestInput c -> WithOrigin SlotNo
signedSlotLatestInput (LatestInput inp) = case inp of
  Nothing              -> Origin
  Just (InputEBB x)    -> pbftEbbPrev x
  Just (InputSigner x) -> NotOrigin $ S.pbftSignerSlotNo x

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | <https://en.wikipedia.org/wiki/Monus>
(.-) :: (Num a, Ord a) => a -> a -> a
a .- b = if a > b then a - b else 0

size :: Num b => Seq.StrictSeq a -> b
size = fromIntegral . Seq.length
