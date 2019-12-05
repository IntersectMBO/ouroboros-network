{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Test.Consensus.Protocol.PBFT (
    tests
    -- * Used in the roundtrip tests
  , TestChainState(..)
  ) where

import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe)
import qualified Data.Sequence.Strict as Seq
import           Data.Word

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Crypto.DSIGN

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT.ChainState (EbbMap (..), PBftChainState)
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS
import           Ouroboros.Consensus.Protocol.PBFT.Crypto
import           Ouroboros.Consensus.Util (dropLast, repeatedly, takeLast)


{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "PBftChainState" [
      testProperty "validGenerator"                   prop_validGenerator
    , testProperty "appendPreservesInvariant"         prop_appendPreservesInvariant
    , testProperty "rewindPreservesInvariant"         prop_rewindPreservesInvariant
    , testProperty "rewindReappendId"                 prop_rewindReappendId
    , testProperty "appendOldStatePreservesInvariant" prop_appendOldStatePreservesInvariant
    , testProperty "appendOldStateRestoresPreWindow"  prop_appendOldStateRestoresPreWindow
    ]

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data TestChainState = TestChainState {
      testChainStateK         :: SecurityParam
    , testChainStateN         :: CS.WindowSize
    , testChainStateNumKeys   :: Int

      -- | The generated chain state
    , testChainState          :: PBftChainState PBftMockCrypto

      -- | The corresponding chain state we would have deserialised before
      -- #1307: this state does not include the @n@ pre-anchor signatures.
    , testChainOldState       :: PBftChainState PBftMockCrypto

      -- | The slots that were kept (i.e., not rolled back to create
      -- 'testChainState')
    , testChainKeptInputs     :: Inputs PBftMockCrypto

      -- | The slots that were dropped (rolled back to create 'testChainState')
    , testChainDroppedInputs  :: Inputs PBftMockCrypto

      -- | Possible next inputs that can be added to 'testChainState' and to
      -- 'testChainOldState'
      --
      -- INVARIANT: the length of this list will be @n + k@.
    , testChainNextInputs     :: Inputs PBftMockCrypto

      -- | Possible next rollback from 'testChainState'
    , testChainRollback       :: WithOrigin SlotNo

      -- | The inputs that 'testChainRollback' would roll back
    , testChainRollbackInputs :: Inputs PBftMockCrypto
    }
  deriving (Show)

-- | Generate chain state
--
-- We don't want to make too many assumptions here, so instead we model what
-- happens on an actual chain: we have a bunch of slots, starting from genesis,
-- signed by various keys, some slots are empty. We then compute the maximum
-- rollback point (@k@ from the tip), and finally drop (d @<= k@) slots,
-- and take the final @n + k - d@ slots.
--
-- Returns both the chain before rollback as well as the chain state.
--
-- NOTE: PBftChainState assumes @k >= 1@.
instance Arbitrary TestChainState where
  arbitrary = do
      k       <- choose (1, 4)
      n       <- choose (1, 10)
      numKeys <- choose (1, 4)

      -- Pick number of signed slots
      --
      -- We don't try to be too clever here; we need to test the various edge
      -- cases near genesis more carefully, but also want to test where we are
      -- well past genesis
      numSigners <- oneof [
          choose (0, k)
        , choose (0, n)
        , choose (0, k + n)
        , choose (0, 5 * (k + n))
        ]

      let getFirstInputSlot = SlotNo <$> choose (0, 3)

      -- Generate all the inputs
      inputs <- getFirstInputSlot >>= generateInputs (genMockKey numKeys) numSigners . GenerateInputsState Origin

      -- The chain state after appending all inputs to the empty chain state
      let _X = appendInputs
                 (SecurityParam k)
                 (CS.WindowSize n)
                 inputs
                 CS.empty

          -- The suffix of inputs we could \"undo\" from _X without violating k
      let droppableInputs = takeLastSigners k inputs
      let maxDrop = lengthInputs droppableInputs

          -- The earliest slot to which we could rewind _X, as limited by k (it
          -- is necessarily a signed slot)
          anchor = case safeLastSigner (dropLastInputs maxDrop inputs) of
                     Nothing -> Origin
                     Just x  -> At (CS.pbftSignerSlotNo x)

      -- Pick a number of inputs to drop; they'll include at most k signers
      toDrop <- choose (0, maxDrop)

      let (originalKept, originalDropped) = splitAtLastInputs toDrop inputs

      -- Directly compute the state equivalent to rewinding _X enough to drop
      -- @toDrop@ many inputs
      let newInputs = dropLastInputs toDrop $ takeLastSigners (n + k) inputs
          newState  = fromInputs
                        (SecurityParam k)
                        (CS.WindowSize n)
                        (anchor, newInputs)

      -- Directly compute the analogous state that we would have deserialised
      -- before #1307: this state does not include the @k@ extra signatures
      -- before the window.
      let oldInputs = dropLastInputs toDrop $ takeLastSigners n inputs
          oldState = fromInputs
                      (SecurityParam k)
                      (CS.WindowSize n)
                      (anchor, oldInputs)

      -- Create potential next @n + k@ signers to be added
      resumeInputGenState <- case reverse $ unInputs $ originalKept of
          []                   -> GenerateInputsState Origin <$> getFirstInputSlot
          InputEBB prev slot:_ -> pure $ GenerateInputsState prev slot
          InputSigner x:_      -> pure $ GenerateInputsState (At s) (succ s)
            where
              s = CS.pbftSignerSlotNo x
      nextInputs <- generateInputs (genMockKey numKeys) (n + k) resumeInputGenState

      -- Create potential rollback
      toDrop2 <- oneof [
          choose (0, maxDrop - toDrop) -- rollback that will succeed
        , choose (0, lengthInputs inputs) -- rollback that might fail (too far)
        ]

      let rollback = case reverse $ slotInputs $ dropLastInputs toDrop2 originalKept of
                       []     -> Origin
                       slot:_ -> At slot

      return TestChainState {
          testChainStateK         = SecurityParam k
        , testChainStateN         = CS.WindowSize n
        , testChainStateNumKeys   = numKeys
        , testChainState          = newState
        , testChainOldState       = oldState
        , testChainKeptInputs     = originalKept
        , testChainDroppedInputs  = originalDropped
        , testChainNextInputs     = nextInputs
        , testChainRollback       = rollback
        , testChainRollbackInputs =
            dropWhileNotAfterInputs rollback $  -- If we rolled back to a slot
                                                -- that had both an EBB and a
                                                -- signer, then we must ensure
                                                -- the signer is not in
                                                -- testChainRollbackInputs
            takeLastInputs toDrop2 originalKept
        }

data GenerateInputsState = GenerateInputsState !(WithOrigin SlotNo) !SlotNo
     -- ^ (slot of latest signature, slot of next input)

-- | The output contains the specified number of 'InputSigner's and also some
-- additional 'InputEBB's
generateInputs ::
  forall c.
     Gen (PBftVerKeyHash c)
  -> Word64
  -> GenerateInputsState
  -> Gen (Inputs c)
     -- ^ inputs, slot of latest signature, slot of next input
generateInputs genKey = go
  where
    plus slot w = slot + SlotNo w
    advance slot = plus slot <$> choose (0, 3)

    go :: Word64 -> GenerateInputsState -> Gen (Inputs c)
    go 0 _                               = return (Inputs [])
    go n (GenerateInputsState prev slot) = do
      let genEBB = do
            pure
              ( n
              , prev
                -- an EBB's successor may have the same slot
              , advance (slot + 1)
                -- an EBB never has the same slot as its predecessor
              , InputEBB prev (slot + 1)
              )
          genSigner = do
            key <- genKey
            pure
              ( n - 1
              , At slot
                -- an non-EBB's successor cannot have the same slot
              , advance (slot + 1)
                -- an non-EBB may have the same slot as its predecessor
              , InputSigner (CS.PBftSigner slot key)
              )

      (n', prev', genSlot', inp) <- frequency [(1, genEBB), (9, genSigner)]
      slot' <- genSlot'
      (\inps -> (inp:) `coerce` inps) <$> go n' (GenerateInputsState prev' slot')

genMockKey :: Int -> Gen (VerKeyDSIGN MockDSIGN)
genMockKey numKeys = VerKeyMockDSIGN <$> choose (1, numKeys)

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

data ClassifyWindow =
    WindowEmpty
  | WindowNotFull
  | WindowFull
  deriving (Show)

data ClassifyRollback =
    RollbackImpossible
  | RollbackLimited
  | RollbackMaximum
  deriving (Show)

classifyWindow :: TestChainState -> ClassifyWindow
classifyWindow TestChainState{..}
  | size inWindow == (0 :: Int)                       = WindowEmpty
  | size inWindow <  CS.getWindowSize testChainStateN = WindowNotFull
  | otherwise                                         = WindowFull
  where
    CS.PBftChainState{..} = testChainState

classifyRollback :: TestChainState -> ClassifyRollback
classifyRollback TestChainState{..}
  | size postAnchor == (0 :: Int)                   = RollbackImpossible
  | size postAnchor <  maxRollbacks testChainStateK = RollbackLimited
  | otherwise                                       = RollbackMaximum
  where
    CS.PBftChainState{..} = testChainState

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- Check that we are producing valid chain tests
prop_validGenerator :: TestChainState -> Property
prop_validGenerator st@TestChainState{..} =
    collect (classifyWindow   st) $
    collect (classifyRollback st) $
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

prop_appendPreservesInvariant :: TestChainState -> Property
prop_appendPreservesInvariant TestChainState{..} =
    let state' = CS.append
                   testChainStateK
                   testChainStateN
                   (headSigner testChainNextInputs)
                   testChainState
    in Right () === CS.invariant
                      testChainStateK
                      testChainStateN
                      state'

prop_rewindPreservesInvariant :: TestChainState -> Property
prop_rewindPreservesInvariant TestChainState{..} =
    let rewound = CS.rewind
                    testChainStateK
                    testChainStateN
                    testChainRollback
                    testChainState
    in case rewound of
         Nothing     -> label "rollback too far in the past" True
         Just state' -> label "rollback succeeded" $
           Right () === CS.invariant
                          testChainStateK
                          testChainStateN
                          state'

-- | If we rewind and then reapply the same blocks, we should get back to
-- our original test
prop_rewindReappendId :: TestChainState -> Property
prop_rewindReappendId TestChainState{..} =
    let rewound = CS.rewind
                    testChainStateK
                    testChainStateN
                    testChainRollback
                    testChainState
    in case rewound of
         Nothing     -> label "rollback too far in the past" True
         Just state' -> label "rollback succeeded" $
           counterexample ("Rewound: " <> show state') $
           testChainState === appendInputs
                                testChainStateK
                                testChainStateN
                                testChainRollbackInputs
                                state'

-- This property holds for the old chain state too
prop_appendOldStatePreservesInvariant :: TestChainState -> Property
prop_appendOldStatePreservesInvariant TestChainState{..} =
    let state' = CS.append
                   testChainStateK
                   testChainStateN
                   (headSigner testChainNextInputs)
                   testChainOldState
    in Right () === CS.invariant
                      testChainStateK
                      testChainStateN
                      state'

-- | After appending the missing signatures, we should have a 'CS.preWindow'
-- of @k@ again.
prop_appendOldStateRestoresPreWindow :: TestChainState -> Property
prop_appendOldStateRestoresPreWindow TestChainState{..} =
    let missing = fromIntegral
                $ maxRollbacks       testChainStateK
                + CS.getWindowSize   testChainStateN
                - CS.countSignatures testChainOldState
        state' = appendInputs
                   testChainStateK
                   testChainStateN
                   (takeSigners missing testChainNextInputs)
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

data Input c
  = InputEBB !(WithOrigin SlotNo) !SlotNo
    -- ^ the preceding signed slot, the EBB's slot
  | InputSigner !(CS.PBftSigner c)
  deriving (Eq, Show)

newtype Inputs c = Inputs {unInputs :: [Input c]}
  deriving (Eq, Show)

signatureInputs :: Inputs c -> [CS.PBftSigner c]
signatureInputs (Inputs inps) = [ signer | InputSigner signer <- inps ]

ebbInputs :: Inputs c -> [(WithOrigin SlotNo, SlotNo)]
ebbInputs (Inputs inps) = [ (prev, slot) | InputEBB prev slot <- inps ]

slotInput :: Input c -> SlotNo
slotInput = \case
  InputEBB _ slot -> slot
  InputSigner   x -> CS.pbftSignerSlotNo x

slotInputs :: Inputs c -> [SlotNo]
slotInputs = map slotInput . unInputs

lengthInputs :: Num a => Inputs c -> a
lengthInputs = fromIntegral . length . unInputs

headSigner :: Inputs c -> CS.PBftSigner c
headSigner = head . signatureInputs

safeLastSigner :: Inputs c -> Maybe (CS.PBftSigner c)
safeLastSigner = listToMaybe . reverse . signatureInputs

appendInput ::
     PBftCrypto c
  => SecurityParam
  -> CS.WindowSize
  -> Input c
  -> CS.PBftChainState c -> CS.PBftChainState c
appendInput k n = \case
  InputEBB _ slot    -> CS.appendEBB k n slot
  InputSigner signer -> CS.append k n signer

appendInputs ::
     PBftCrypto c
  => SecurityParam
  -> CS.WindowSize
  -> Inputs c
  -> CS.PBftChainState c -> CS.PBftChainState c
appendInputs k n = repeatedly (appendInput k n) . unInputs

-- | May include EBBs before the first signer and after the last signer
takeSigners :: Word64 -> Inputs c -> Inputs c
takeSigners = \n0 -> Inputs . go n0 . unInputs
  where
    go !n = \case
      [] -> []
      inp:inps -> case inp of
        InputEBB{}    -> inp : go n inps
        InputSigner{}
          | n == 0    -> []
          | otherwise -> inp : go (n - 1) inps

-- | May include EBBs after the last signer *BUT* *NOT* before the first signer
takeLastSigners :: Word64 -> Inputs c -> Inputs c
takeLastSigners = \n0 -> Inputs . reverse . go n0 . reverse . unInputs
  where
    go 0 = const []
    go n = \case
      []       -> []
      inp:inps -> inp : go n' inps
        where
          n' = case inp of
            InputEBB{}    -> n
            InputSigner{} -> n - 1

-- | Wrapper around 'CS.fromList' that also sets the 'ebbs' field
fromInputs ::
     PBftCrypto c
  => SecurityParam
  -> CS.WindowSize
  -> (WithOrigin SlotNo, Inputs c)
  -> CS.PBftChainState c
fromInputs k n (anchor, inputs) =
    CS.fromList k n (anchor, Seq.fromList $ signatureInputs inputs, EbbMap m)
  where
    m = Map.fromList [ (slot, mSlot) | (mSlot, slot) <- ebbInputs inputs ]

splitAtLastInputs :: Word64 -> Inputs c -> (Inputs c, Inputs c)
splitAtLastInputs n (Inputs inps) = (Inputs l, Inputs r)
  where
    (l, r) = splitAt (length inps - fromIntegral n) inps

dropLastInputs :: Word64 -> Inputs c -> Inputs c
dropLastInputs n = Inputs . dropLast (fromIntegral n) . unInputs

dropWhileNotAfterInputs :: WithOrigin SlotNo -> Inputs c -> Inputs c
dropWhileNotAfterInputs mSlot =
  Inputs . dropWhile ((<= mSlot) . At . slotInput) . unInputs

takeLastInputs :: Word64 -> Inputs c -> Inputs c
takeLastInputs n = Inputs . takeLast (fromIntegral n) . unInputs

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

size :: Num b => Seq.StrictSeq a -> b
size = fromIntegral . Seq.length
