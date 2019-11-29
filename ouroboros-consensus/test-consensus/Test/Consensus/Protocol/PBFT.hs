{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Consensus.Protocol.PBFT (
    tests
    -- * Used in the roundtrip tests
  , TestChainState(..)
  ) where

import qualified Data.Sequence.Strict as Seq
import           Data.Word

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Crypto.DSIGN

import           Ouroboros.Network.Block (SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT.ChainState (PBftChainState)
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

      -- | The slots that were dropped (rollback before this chain state)
    , testChainDropped        :: [CS.PBftSigner PBftMockCrypto]

      -- | Possible next signers that can be added
      --
      -- INVARIANT: the length of this list will be @n + k@.
    , testChainNextSigners    :: [CS.PBftSigner PBftMockCrypto]

      -- | Possible next rollback
    , testChainRollback       :: WithOrigin SlotNo

      -- | The blocks that 'testChainRollback' would roll back
    , testChainRollbackBlocks :: [CS.PBftSigner PBftMockCrypto]
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

      -- Pick number of slots
      --
      -- We don't try to be too clever here; we need to test the various edge
      -- cases near genesis more carefully, but also want to test where we are
      -- well past genesis
      numSlots <- oneof [
          choose (0, k)
        , choose (0, n)
        , choose (0, k + n)
        , choose (0, 5 * (k + n))
        ]

      -- Generate all the signatures
      slots <- generateSigners (genMockKey numKeys) numSlots Origin

      -- Compute max rollback point
      let anchor = case drop (fromIntegral k) (reverse slots) of
                     []  -> Origin
                     x:_ -> At (CS.pbftSignerSlotNo x)

      -- Pick a number of blocks to drop
      toDrop <- choose (0, k)
      let signers = Seq.fromList $ takeLast (n + k - toDrop) (dropLast toDrop slots)
          state   = CS.fromList
                      (SecurityParam k)
                      (CS.WindowSize n)
                      (anchor, signers)

      -- Compute the state that we would have deserialised before #1307: this
      -- state does not include the @k@ extra signatures before the window.
      let oldSigners = Seq.fromList $ takeLast (n - toDrop) (dropLast toDrop slots)
          oldState = CS.fromList
                      (SecurityParam k)
                      (CS.WindowSize n)
                      (anchor, oldSigners)

      -- Create potential next @k@ signers to be added
      let lastSlot = case signers of
                       -- Can only be empty if near genesis, because @toDrop@ is
                       -- at most @k@ and hence @signers@ must be at least @n@
                       -- long, unless @slots@ is near genesis.
                       Seq.Empty                   -> Origin
                       _ Seq.:|> CS.PBftSigner s _ -> At s
      nextSigners <- generateSigners (genMockKey numKeys) (n + k) lastSlot

      -- Create potential rollback
      numRollback <- oneof [
          choose (0, k - toDrop) -- rollback that will succeed
        , choose (0, numSlots)   -- rollback that might fail (too far)
        ]
      let rollback = case drop (fromIntegral (toDrop + numRollback)) (reverse slots) of
                       []  -> Origin
                       x:_ -> At (CS.pbftSignerSlotNo x)

      return TestChainState {
          testChainStateK         = SecurityParam k
        , testChainStateN         = CS.WindowSize n
        , testChainStateNumKeys   = numKeys
        , testChainState          = state
        , testChainOldState       = oldState
        , testChainDropped        = takeLast toDrop slots
        , testChainNextSigners    = nextSigners
        , testChainRollback       = rollback
        , testChainRollbackBlocks = takeLast numRollback (dropLast toDrop slots)
        }

generateSigners :: Gen (PBftVerKeyHash c)
                -> Word64
                -> WithOrigin SlotNo
                -> Gen [CS.PBftSigner c]
generateSigners genKey = go
  where
    go 0        _    = return []
    go numSlots prev = do
      slot :: SlotNo <- case prev of
                          Origin ->
                            SlotNo <$> choose (0, 3)
                          At (SlotNo s) -> do
                            skip <- choose (1, 3)
                            return $ SlotNo (s + skip)
      signer <- CS.PBftSigner slot <$> genKey
      (signer :) <$> go (numSlots - 1) (At slot)

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
                   (head testChainNextSigners)
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
           testChainState === CS.appendMany
                                testChainStateK
                                testChainStateN
                                testChainRollbackBlocks
                                state'

-- This property holds for the old chain state too
prop_appendOldStatePreservesInvariant :: TestChainState -> Property
prop_appendOldStatePreservesInvariant TestChainState{..} =
    let state' = CS.append
                   testChainStateK
                   testChainStateN
                   (head testChainNextSigners)
                   testChainOldState
    in Right () === CS.invariant
                      testChainStateK
                      testChainStateN
                      state'

-- | After appending the missing signatures, we should have a 'CS.preWindow'
-- of @k again.
prop_appendOldStateRestoresPreWindow :: TestChainState -> Property
prop_appendOldStateRestoresPreWindow TestChainState{..} =
    let missing = fromIntegral
                $ maxRollbacks       testChainStateK
                + CS.getWindowSize   testChainStateN
                - CS.countSignatures testChainOldState
        state' = repeatedly
                   (CS.append
                     testChainStateK
                     testChainStateN)
                   (take missing testChainNextSigners)
                   testChainOldState
    in Right () === CS.invariant
                      testChainStateK
                      testChainStateN
                      state'
       .&&.
       size (CS.preWindow state') === maxRollbacks testChainStateK

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

size :: Num b => Seq.StrictSeq a -> b
size = fromIntegral . Seq.length
