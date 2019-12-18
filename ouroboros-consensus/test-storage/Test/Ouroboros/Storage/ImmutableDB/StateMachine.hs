{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans      #-}
module Test.Ouroboros.Storage.ImmutableDB.StateMachine
    ( tests
    , showLabelledExamples
    ) where

import           Prelude hiding (elem, notElem)

import           Codec.CBOR.Write (toBuilder)
import           Codec.Serialise (decode)
import           Control.Monad (forM_, void, when)
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Control.Monad.State.Strict (MonadState, State, evalState, gets,
                     modify, put, runState)
import           Data.Bifunctor (first)
import           Data.ByteString.Lazy (ByteString)
import           Data.Coerce (Coercible, coerce)
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.Functor.Classes (Eq1, Show1)
import           Data.Functor.Identity
import           Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, isNothing, listToMaybe)
import           Data.Proxy (Proxy (..))
import           Data.TreeDiff (Expr (App))
import           Data.TreeDiff.Class (ToExpr (..))
import           Data.Typeable (Typeable)
import           Data.Word (Word64)

import           Control.Monad.Class.MonadThrow hiding (try)

import qualified Generics.SOP as SOP

import           GHC.Generics (Generic, Generic1)
import           GHC.Stack (HasCallStack)

import           System.Random (getStdRandom, randomR)

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Random (mkQCGen)
import           Test.StateMachine
import qualified Test.StateMachine.Sequential as QSM
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Text.Show.Pretty (ppShow)

import           Ouroboros.Consensus.Block (IsEBB (..), fromIsEBB)
import qualified Ouroboros.Consensus.Util.Classify as C
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Network.Block (BlockNo (..), HasHeader (..),
                     HeaderHash, SlotNo (..))
import qualified Ouroboros.Network.Block as Block

import           Ouroboros.Storage.Common hiding (Tip (..))
import qualified Ouroboros.Storage.Common as C
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API (HasFS (..))
import           Ouroboros.Storage.FS.API.Types (FsError (..), FsPath)
import           Ouroboros.Storage.ImmutableDB hiding (BlockOrEBB (..))
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Storage.ImmutableDB.Impl as ImmDB (Internal (..))
import           Ouroboros.Storage.ImmutableDB.Impl.Util (renderFile, tryImmDB)
import           Ouroboros.Storage.ImmutableDB.Layout
import           Ouroboros.Storage.ImmutableDB.Parser (epochFileParser)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Util.FS.Sim.Error (Errors, mkSimErrorHasFS, withErrors)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.Orphans.Arbitrary (genSmallSlotNo)
import           Test.Util.RefEnv (RefEnv)
import qualified Test.Util.RefEnv as RE
import           Test.Util.SOP
import           Test.Util.Tracer (recordingTracerIORef)

import           Test.Ouroboros.Storage.ImmutableDB.Model
import           Test.Ouroboros.Storage.TestBlock
import           Test.Ouroboros.Storage.Util (collects)

{-------------------------------------------------------------------------------
  Abstract model
-------------------------------------------------------------------------------}

-- | Commands
--
-- 'Cmd' will be instantiated to:
--
-- > Cmd (IterRef m r)
-- > Cmd (Iterator m)
--
-- Where @m@ can be 'PureM', 'RealM', or 'RealErrM', and @r@ can be 'Symbolic'
-- or 'Concrete'.
data Cmd it
  = GetBlock            SlotNo
  | GetBlockHeader      SlotNo
  | GetBlockHash        SlotNo
  | GetEBB              EpochNo
  | GetEBBHeader        EpochNo
  | GetEBBHash          EpochNo
  | GetBlockOrEBB       SlotNo  Hash
  | GetBlockOrEBBHeader SlotNo  Hash
  | AppendBlock         SlotNo  Hash TestBlock
  | AppendEBB           EpochNo Hash TestBlock
  | StreamBlocks        (Maybe (SlotNo, Hash)) (Maybe (SlotNo, Hash))
  | StreamHeaders       (Maybe (SlotNo, Hash)) (Maybe (SlotNo, Hash))
  | IteratorNext        it
  | IteratorPeek        it
  | IteratorHasNext     it
  | IteratorClose       it
  | Reopen              ValidationPolicy
  | DeleteAfter         ImmTip
  | Corruption          Corruption
  deriving (Generic, Show, Functor, Foldable, Traversable)

deriving instance SOP.Generic         (Cmd it)
deriving instance SOP.HasDatatypeInfo (Cmd it)

-- | Simulate corruption of some files of the database.
newtype Corruption = MkCorruption Corruptions
  deriving (Generic, Show)

-- | A 'Cmd' together with 'Errors'.
--
-- When executing the 'Cmd', these 'Errors' are passed to 'SimErrorFS' to
-- simulate file system errors thrown at the 'HasFS' level. When 'Nothing', no
-- errors will be thrown.
data CmdErr it = CmdErr
  { _cmdErr   :: Maybe Errors
  , _cmd      :: Cmd it
  , _cmdIters :: [it]
    -- ^ A list of all open iterators. For some commands, e.g., corrupting the
    -- database or simulating errors, we need to close and reopen the
    -- database, which almost always requires truncation of the database.
    -- During truncation we might need to delete a file that is still opened
    -- by an iterator. As this is not allowed by the MockFS implementation, we
    -- first close all open iterators in these cases.
    --
    -- See #328
  } deriving (Show, Generic, Functor, Foldable, Traversable)

type Hash = TestHeaderHash

-- | Return type for successful database operations.
data Success it
  = Unit             ()
  | Block            (Maybe (Hash, ByteString))
  | EBB              (Maybe (Hash, ByteString))
  | BlockHeader      (Maybe (Hash, ByteString))
  | EBBHeader        (Maybe (Hash, ByteString))
  | BlockHash        (Maybe Hash)
  | EBBHash          (Maybe Hash)
  | BlockOrEBB       (Maybe (Either EpochNo SlotNo, ByteString))
  | BlockOrEBBHeader (Maybe (Either EpochNo SlotNo, ByteString))
  | EpochNo          EpochNo
  | Iter             (Either (WrongBoundError Hash) it)
  | IterResult       (IteratorResult Hash ByteString)
  | IterHasNext      Bool
  | Tip              (ImmTipWithHash Hash)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | How to run a 'Corruption' command.
type RunCorruption m =
     ImmutableDB    Hash m
  -> ImmDB.Internal Hash m
  -> Corruption
  -> m (Success (Iterator Hash m ByteString))

-- | Run the command against the given database.
run :: (HasCallStack, Monad m)
    => RunCorruption m
    -> [Iterator Hash m ByteString]
    -> ImmutableDB    Hash m
    -> ImmDB.Internal Hash m
    -> Cmd (Iterator Hash m ByteString)
    -> m (Success (Iterator Hash m ByteString))
run runCorruption its db internal cmd = case cmd of
  GetBlock            s     -> Block            <$> getBlock            db s
  GetEBB              e     -> EBB              <$> getEBB              db e
  GetBlockHeader      s     -> BlockHeader      <$> getBlockHeader      db s
  GetEBBHeader        e     -> EBBHeader        <$> getEBBHeader        db e
  GetBlockHash        s     -> BlockHash        <$> getBlockHash        db s
  GetEBBHash          e     -> EBBHash          <$> getEBBHash          db e
  GetBlockOrEBB       s h   -> BlockOrEBB       <$> getBlockOrEBB       db s h
  GetBlockOrEBBHeader s h   -> BlockOrEBBHeader <$> getBlockOrEBBHeader db s h
  AppendBlock         s h b -> Unit             <$> appendBlock         db s h (toBuilder <$> testBlockToBinaryInfo b)
  AppendEBB           e h b -> Unit             <$> appendEBB           db e h (toBuilder <$> testBlockToBinaryInfo b)
  StreamBlocks        s e   -> Iter             <$> streamBlocks        db s e
  StreamHeaders       s e   -> Iter             <$> streamHeaders       db s e
  IteratorNext        it    -> IterResult       <$> iteratorNext        it
  IteratorPeek        it    -> IterResult       <$> iteratorPeek        it
  IteratorHasNext     it    -> IterHasNext      <$> iteratorHasNext     it
  IteratorClose       it    -> Unit             <$> iteratorClose       it
  DeleteAfter tip           -> do
    mapM_ iteratorClose its
    Unit <$> deleteAfter internal tip
  Reopen valPol             -> do
    mapM_ iteratorClose its
    closeDB db
    reopen db valPol
    Tip <$> getTip db
  Corruption corr           -> do
    mapM_ iteratorClose its
    runCorruption db internal corr

{-------------------------------------------------------------------------------
  Instantiating the semantics
-------------------------------------------------------------------------------}

-- | Responses are either successful termination or an error.
newtype Resp it = Resp { getResp :: Either ImmutableDBError (Success it) }
  deriving (Functor, Foldable, Traversable, Show)

-- | The 'Eq' instance for 'Resp' uses 'sameImmutableDBError'.
instance Eq it => Eq (Resp it) where
  Resp (Left  e) == Resp (Left  e') = sameImmutableDBError e e'
  Resp (Right a) == Resp (Right a') = a == a'
  _              == _               = False

-- | The monad used to run pure model/mock implementation of the database.
type PureM = ExceptT ImmutableDBError (State (DBModel Hash))

-- | The type of the pure model/mock implementation of the database.
type ModelDBPure = ImmutableDB Hash PureM

-- | The type of the pure model/mock implementation of the interal API of the
-- database.
type ModelDBInternalPure = ImmDB.Internal Hash PureM

-- | Run a command against the pure model
runPure :: DBModel Hash
        -> ModelDBPure
        -> ModelDBInternalPure
        -> CmdErr (Iterator Hash PureM ByteString)
        -> (Resp (Iterator Hash PureM ByteString), DBModel Hash)
runPure dbm mdb minternal (CmdErr mbErrors cmd its) =
    first Resp $ flip runState dbm $ do
      resp <- runExceptT $ run runCorruption its mdb minternal cmd
      case (mbErrors, resp) of
        -- No simulated errors, just step
        (Nothing, _) -> return resp
        -- An error will be simulated and thrown (not here, but in the real
        -- implementation). To mimic what the implementation will do, we only
        -- have to close the iterators, as the truncation during the reopening
        -- of the database will erase any changes.
        (Just _, _) -> do
          -- We ignore the updated dbm (in the StateT), because we have to
          -- roll back to the state before executing cmd.
          --
          -- As the implementation closes all iterators, we do the same.
          put $ dbm { dbmIterators = mempty }
          -- The only exception is the DeleteAfter cmd, in which case we have
          -- to roll back to the requested tip.
          case cmd of
            DeleteAfter tip ->
              fmap (either (error . prettyImmutableDBError) id) $
              runExceptT $ deleteAfter minternal tip
            _               -> return ()
          gets (Right . Tip . dbmTip)
  where
    runCorruption :: ModelDBPure
                  -> ModelDBInternalPure
                  -> Corruption
                  -> PureM (Success (Iterator Hash PureM ByteString))
    runCorruption _ _ (MkCorruption corrs) = do
      modify $ simulateCorruptions corrs
      gets (Tip . dbmTip)

{-------------------------------------------------------------------------------
  Collect arguments
-------------------------------------------------------------------------------}

-- | Collect all iterators created. For example, @t@ could be 'Cmd' or 'CmdErr'.
iters :: Traversable t => t it -> [it]
iters = toList


{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

-- | Concrete or symbolic references to a real (or model) iterator
type IterRef m = Reference (Opaque (Iterator Hash m ByteString))

-- | Mapping between iterator references and mocked iterators
type KnownIters m = RefEnv (Opaque (Iterator Hash m     ByteString))
                                   (Iterator Hash PureM ByteString)

-- | Execution model
data Model m r = Model
  { dbModel      :: DBModel Hash
    -- ^ A model of the database, used as state for the 'HasImmutableDB'
    -- instance of 'ModelDB'.
  , mockDB       :: ModelDBPure
    -- ^ A handle to the mocked database.
  , mockInternal :: ModelDBInternalPure
    -- ^ A handle to the internal API of the mocked database.
  , knownIters   :: KnownIters m r
    -- ^ Store a mapping between iterator references and mocked iterators.
  } deriving (Show, Generic)

-- | Initial model
initModel :: DBModel Hash -> ModelDBPure -> ModelDBInternalPure -> Model m r
initModel dbModel mockDB mockInternal = Model
    { knownIters  = RE.empty
    , ..
    }

-- | Key property of the model is that we can go from real to mock responses
toMock :: (Functor t, Eq1 r)
       => Model m r -> At t m r -> t (Iterator Hash PureM ByteString)
toMock Model {..} (At t) = fmap (knownIters RE.!) t

-- | Step the mock semantics
step :: Eq1 r
     => Model m r
     -> At CmdErr m r
     -> (Resp (Iterator Hash PureM ByteString), DBModel Hash)
step model@Model{..} cmdErr = runPure dbModel mockDB mockInternal (toMock model cmdErr)


{-------------------------------------------------------------------------------
  Wrapping in quickcheck-state-machine references
-------------------------------------------------------------------------------}

-- | Instantiate functor @t@ to @t ('IterRef' m r)@.
--
-- Needed because we need to (partially) apply @'At' t m@ to @r@.
newtype At t m r = At { unAt :: t (IterRef m r) }
  deriving (Generic)

deriving instance Show (t (IterRef m r)) => Show (At t m r)

deriving instance Eq1 r => Eq (At Resp m r)

deriving instance Generic1          (At Cmd m)
deriving instance Rank2.Foldable    (At Cmd m)
deriving instance Rank2.Functor     (At Cmd m)
deriving instance Rank2.Traversable (At Cmd m)

deriving instance Generic1          (At CmdErr m)
deriving instance Rank2.Foldable    (At CmdErr m)
deriving instance Rank2.Functor     (At CmdErr m)
deriving instance Rank2.Traversable (At CmdErr m)

deriving instance Generic1          (At Resp m)
deriving instance Rank2.Foldable    (At Resp m)


{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

-- | An event records the model before and after a command along with the
-- command itself, and a mocked version of the response.
data Event m r = Event
  { eventBefore   :: Model     m r
  , eventCmdErr   :: At CmdErr m r
  , eventAfter    :: Model     m r
  , eventMockResp :: Resp (Iterator Hash PureM ByteString)
  } deriving (Show)

eventCmd :: Event m r -> At Cmd m r
eventCmd = At . _cmd . unAt . eventCmdErr

eventMockCmd :: Eq1 r => Event m r -> Cmd (Iterator Hash PureM ByteString)
eventMockCmd ev@Event {..} = toMock eventBefore (eventCmd ev)


-- | Construct an event
lockstep :: (Show1 r, Eq1 r)
         => Model     m r
         -> At CmdErr m r
         -> At Resp   m r
         -> Event     m r
lockstep model@Model {..} cmdErr (At resp) = Event
    { eventBefore   = model
    , eventCmdErr   = cmdErr
    , eventAfter    = model'
    , eventMockResp = mockResp
    }
  where
    (mockResp, dbModel') = step model cmdErr
    newIters = RE.fromList $ zip (iters resp) (iters mockResp)
    model' = model
      { dbModel    = dbModel'
      , knownIters = knownIters `RE.union` newIters
      }

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

-- | Generate a 'CmdErr'
generator :: Model m Symbolic -> Gen (At CmdErr m Symbolic)
generator m@Model {..} = do
    _cmd    <- unAt <$> generateCmd m
    _cmdErr <- if errorFor _cmd
       then frequency
          -- We want to make some progress
          [ (4, return Nothing)
          , (1, Just <$> arbitrary)
          ]
       else return Nothing
    let _cmdIters = RE.keys knownIters
    return $ At CmdErr {..}
  where
    -- Don't simulate an error during corruption, because we don't want an
    -- error to happen while we corrupt a file.
    errorFor Corruption {} = False
    errorFor _             = True

-- | Generate a 'Cmd'.
generateCmd :: Model m Symbolic -> Gen (At Cmd m Symbolic)
generateCmd Model {..} = At <$> frequency
    [ -- Block
      (1, elements [GetBlock, GetBlockHeader, GetBlockHash] <*> genGetBlockSlot)
      -- EBB
    , (1, elements [GetEBB, GetEBBHeader, GetEBBHash] <*> genGetEBB)
    , (1, (uncurry <$> elements [GetBlockOrEBB, GetBlockOrEBBHeader]) <*> genSlotAndHash)
    , (3, do
            let mbPrevBlock = dbmTipBlock dbModel
            slotNo  <- frequency
              [ -- Slot in the past -> invalid
                (1, chooseSlot (0, lastSlot))
                -- If the previous block is an EBB, make a regular block in
                -- the same slot number. The slot can still be empty, though.
              , (if maybe False (fromIsEBB . testBlockIsEBB) mbPrevBlock
                 then 7 else 0,
                 return lastSlot)
                -- Slots not too far in the future
              , (4, chooseSlot (lastSlot, lastSlot + 10))
                -- Slots in some future epoch
              , (1, chooseSlot (lastSlot + epochSize',
                                lastSlot + epochSize' * 4))
              ]
            let block = (maybe firstBlock mkNextBlock mbPrevBlock)
                        slotNo (TestBody 0 True)
            return $ AppendBlock slotNo (blockHash block) block)
    , (1, do
            (epoch, ebb) <- case dbmTipBlock dbModel of
              Nothing        -> return (0, firstEBB (TestBody 0 True))
              Just prevBlock -> do
                epoch <- frequency
                -- Epoch in the past -> invalid
                  [ (1, chooseEpoch (0, currentEpoch))
                  , (3, chooseEpoch (currentEpoch, currentEpoch + 5))
                  ]
                let slotNo = SlotNo (unEpochNo epoch) * epochSize'
                return (epoch, mkNextEBB prevBlock slotNo (TestBody 0 True))
            return $ AppendEBB epoch (blockHash ebb) ebb)
    , (4, frequency
            -- An iterator with a random and likely invalid range,
            [ (1, elements [StreamBlocks, StreamHeaders]
                    <*> (Just <$> genRandomBound)
                    <*> (Just <$> genRandomBound))
            -- A valid iterator
            , (if empty then 0 else 2, do
                 start <- genBound
                 let startSlot = maybe 0 fst start
                 end   <- genBound `suchThat` \case
                     -- NOTE: say @start@ refers to the only block in the DB,
                     -- which is the EBB of the current epoch, then there is
                     -- no regular block >= @start@, only the EBB itself (=
                     -- @start@). So we must make sure that we can generate a
                     -- slot that refers to this EBB for @end@, otherwise we
                     -- may end up in an infinite loop if we're only
                     -- generating slots referring to regular blocks.
                     Nothing           -> True
                     Just (endSlot, _) -> endSlot >= startSlot

                 streamBlocksOrHeaders <- elements [StreamBlocks, StreamHeaders]
                 return $ streamBlocksOrHeaders start end)
            ])
      -- Only if there are iterators can we generate commands that manipulate
      -- them.
    , (if Map.null dbmIterators then 0 else 8, do
         iter <- elements $ RE.keys knownIters
         frequency [ (4, return $ IteratorNext    iter)
                   , (4, return $ IteratorPeek    iter)
                   , (4, return $ IteratorHasNext iter)
                   , (1, return $ IteratorClose   iter) ])
    , (1, Reopen <$> genValPol)

    , (4, DeleteAfter <$> genTip)

      -- Only if there are files on disk can we generate commands that corrupt
      -- them.
    , (if null dbFiles then 0 else 2, Corruption <$> genCorruption)
    ]
  where
    DBModel {..} = dbModel

    currentEpoch = dbmCurrentEpoch dbModel

    lastSlot :: SlotNo
    lastSlot = fromIntegral $ length dbmChain

    -- Useful when adding to another 'SlotNo'
    epochSize' :: SlotNo
    epochSize' = SlotNo $ unEpochSize fixedEpochSize

    empty = dbmTip == C.TipGen

    noBlocks = all isNothing dbmChain

    noEBBs = Map.null dbmEBBs

    genGetBlockSlot :: Gen SlotNo
    genGetBlockSlot = frequency
      [ (if empty then 0 else 10, genSlotInThePast)
      , (1,  genSlotInTheFuture)
      , (1,  genSmallSlotNo) ]

    genGetEBB :: Gen EpochNo
    genGetEBB = frequency
      [ (if noEBBs then 0 else 5, elements $ Map.keys dbmEBBs)
      , (1, chooseEpoch (0, 5))
      ]

    chooseWord64 :: Coercible a Word64 => (a, a) -> Gen a
    chooseWord64 (start, end) = coerce $ choose @Word64 (coerce start, coerce end)

    chooseSlot :: (SlotNo, SlotNo) -> Gen SlotNo
    chooseSlot = chooseWord64

    chooseEpoch :: (EpochNo, EpochNo) -> Gen EpochNo
    chooseEpoch = chooseWord64

    genSlotInThePast :: Gen SlotNo
    genSlotInThePast = chooseSlot (0, lastSlot)

    genSlotInTheFuture :: Gen SlotNo
    genSlotInTheFuture = chooseSlot (succ lastSlot, maxBound)

    genSlotAndHash :: Gen (SlotNo, Hash)
    genSlotAndHash = frequency
      [ (if noBlocks then 0 else 5,
         (\b -> (blockSlot b, blockHash b)) <$> genBlockInThePast)
      , (if noEBBs then 0 else 5,
         (\b -> (blockSlot b, blockHash b)) <$> genEBBInThePast)
      , (1, genRandomBound)
      ]

    -- Generates random hashes, will seldomly correspond to real blocks. Used
    -- to test error handling.
    genRandomBound :: Gen (SlotNo, Hash)
    genRandomBound = (,) <$> arbitrary <*> (TestHeaderHash <$> arbitrary)

    genBlockInThePast :: Gen TestBlock
    genBlockInThePast =
      elements $ map (testBlockFromBinaryInfo . snd) $ catMaybes dbmChain

    genEBBInThePast :: Gen TestBlock
    genEBBInThePast =
      elements $ map (testBlockFromBinaryInfo . snd) $ Map.elems dbmEBBs

    genBound = frequency
      [ (1,
         return Nothing)
      , (if noBlocks then 0 else 1,
         (\b   -> Just (blockSlot b,   blockHash b))   <$> genBlockInThePast)
      , (if noEBBs then 0 else 1,
         (\ebb -> Just (blockSlot ebb, blockHash ebb)) <$> genEBBInThePast)
      ]

    genCorruption = MkCorruption <$> generateCorruptions (NE.fromList dbFiles)

    dbFiles = getDBFiles dbModel

    genValPol = elements [ValidateMostRecentEpoch, ValidateAllEpochs]

    genTip :: Gen ImmTip
    genTip = elements $ NE.toList $ tips dbModel

-- | Return the files that the database with the given model would have
-- created. For each epoch an epoch, primary index, and secondary index file.
getDBFiles :: DBModel Hash -> [FsPath]
getDBFiles dbm =
    [ renderFile fileType epoch
    | epoch <- [0..dbmCurrentEpoch dbm]
    , fileType <- ["epoch", "primary", "secondary"]
    ]

{-------------------------------------------------------------------------------
  Shrinking
-------------------------------------------------------------------------------}

-- | Shrinker
shrinker :: Model m Symbolic -> At CmdErr m Symbolic -> [At CmdErr m Symbolic]
shrinker m@Model {..} (At (CmdErr mbErrors cmd _)) = fmap At $
    [ CmdErr mbErrors' cmd  its' | mbErrors' <- shrink mbErrors ] ++
    [ CmdErr mbErrors  cmd' its' | At cmd'   <- shrinkCmd m (At cmd) ]
  where
    its' = RE.keys knownIters

-- | Shrink a 'Cmd'.
shrinkCmd :: Model m Symbolic -> At Cmd m Symbolic -> [At Cmd m Symbolic]
shrinkCmd Model {..} (At cmd) = fmap At $ case cmd of
    AppendBlock _slot  _hash _b       -> []
    AppendEBB   _epoch _hash _ebb     -> []
    StreamBlocks  _mbStart _mbEnd     -> []
    StreamHeaders _mbStart _mbEnd     -> []
    GetBlock slot                     ->
      [GetBlock slot' | slot' <- shrink slot]
    GetEBB epoch                      ->
      [GetEBB epoch' | epoch' <- shrink epoch]
    GetBlockHeader slot               ->
      [GetBlockHeader slot' | slot' <- shrink slot]
    GetEBBHeader epoch                ->
      [GetEBBHeader epoch' | epoch' <- shrink epoch]
    GetBlockHash slot                 ->
      [GetBlockHash slot' | slot' <- shrink slot]
    GetEBBHash epoch                  ->
      [GetEBBHash epoch' | epoch' <- shrink epoch]
    GetBlockOrEBB _slot _hash         -> []
    GetBlockOrEBBHeader _slot _hash   -> []
    IteratorNext    {}                -> []
    IteratorPeek    {}                -> []
    IteratorHasNext {}                -> []
    IteratorClose   {}                -> []
    DeleteAfter tip                   ->
      [DeleteAfter tip' | tip' <- shrinkTip tip]
    Reopen {}                         -> []
    Corruption corr                   ->
      [Corruption corr' | corr' <- shrinkCorruption corr]
  where
    DBModel {..} = dbModel

    currentEpoch = dbmCurrentEpoch dbModel

    lastSlot :: SlotNo
    lastSlot = fromIntegral $ length dbmChain

    shrinkCorruption (MkCorruption corrs) =
      [ MkCorruption corrs'
      | corrs' <- shrinkCorruptions corrs]

    -- Return tips that are closer to the current tip. If the tip is after the
    -- current tip, return the tips between the current tip and the tip. If
    -- the tip is before the current tip, return the tips between the tip and
    -- the current tip.
    --
    -- For simplicity, we only shrink to TipEBBs if the tip is an TipEBB,
    -- similarly for TipBlock. Otherwise we have to check whether a TipEBB is
    -- before or after a TipBlock.
    shrinkTip C.TipGen =
      map (C.Tip . ImmDB.Block) [0..lastSlot] ++ map (C.Tip . ImmDB.EBB) [0..currentEpoch]
    shrinkTip (C.Tip (ImmDB.Block slot))
      | slot > lastSlot = map (C.Tip . ImmDB.Block) [lastSlot..slot - 1]
      | otherwise       = map (C.Tip . ImmDB.Block) [slot + 1..lastSlot]
    shrinkTip (C.Tip (ImmDB.EBB epoch))
      | epoch > currentEpoch = map (C.Tip . ImmDB.EBB) [currentEpoch..epoch - 1]
      | otherwise            = map (C.Tip . ImmDB.EBB) [epoch + 1..currentEpoch]


{-------------------------------------------------------------------------------
  The final state machine
-------------------------------------------------------------------------------}


-- | Mock a response
--
-- We do this by running the pure semantics and then generating mock
-- references for any new handles.
mock :: Typeable m
     => Model           m Symbolic
     -> At CmdErr       m Symbolic
     -> GenSym (At Resp m Symbolic)
mock model cmdErr = At <$> traverse (const genSym) resp
  where
    (resp, _dbm) = step model cmdErr

precondition :: Model m Symbolic -> At CmdErr m Symbolic -> Logic
precondition Model {..} (At (CmdErr { _cmd = cmd })) =
   forall (iters cmd) (`elem` RE.keys knownIters) .&&
    case cmd of
      AppendBlock    _ _ b -> fitsOnTip b
      AppendEBB      _ _ b -> fitsOnTip b
      DeleteAfter tip      -> tip `elem` NE.toList (tips dbModel)
      Corruption corr ->
        forall (corruptionFiles corr) (`elem` getDBFiles dbModel)
      _ -> Top
  where
    corruptionFiles (MkCorruption corrs) = map snd $ NE.toList corrs

    fitsOnTip :: TestBlock -> Logic
    fitsOnTip b = case dbmTipBlock dbModel of
      Nothing    -> blockPrevHash b .== Block.GenesisHash
      Just bPrev -> blockPrevHash b .== Block.BlockHash (blockHash bPrev)

transition :: (Show1 r, Eq1 r)
           => Model m r -> At CmdErr m r -> At Resp m r -> Model m r
transition model cmdErr = eventAfter . lockstep model cmdErr

postcondition :: Model m Concrete
              -> At CmdErr m Concrete
              -> At Resp m Concrete
              -> Logic
postcondition model cmdErr resp =
    toMock (eventAfter ev) resp .== eventMockResp ev
  where
    ev = lockstep model cmdErr resp

semantics :: StrictTVar IO Errors
          -> HasFS IO h
          -> ImmutableDB   Hash IO
          -> ImmDB.Internal Hash IO
          -> At CmdErr IO Concrete
          -> IO (At Resp IO Concrete)
semantics errorsVar hasFS db internal (At cmdErr) =
    At . fmap (reference . Opaque) . Resp <$> case opaque <$> cmdErr of

      CmdErr Nothing       cmd its -> try $
        run (semanticsCorruption hasFS) its db internal cmd

      CmdErr (Just errors) cmd its -> do
        tipBefore <- fmap forgetHash <$> getTip db
        res       <- withErrors errorsVar errors $ try $
          run (semanticsCorruption hasFS) its db internal cmd
        case res of
          -- If the command resulted in a 'UserError', we didn't even get the
          -- chance to run into a simulated error. Note that we still
          -- truncate, because we can't predict whether we'll get a
          -- 'UserError' or an 'UnexpectedError', as it depends on the
          -- simulated error.
          Left (UserError {})       ->
            truncateAndReopen cmd its tipBefore

          -- We encountered a simulated error
          Left (UnexpectedError {}) -> do
            open <- isOpen db
            when open $
              fail "Database still open while it should have been closed"
            truncateAndReopen cmd its tipBefore

          -- TODO track somewhere which/how many errors were *actually* thrown

          -- By coincidence no error was thrown, try to mimic what would have
          -- happened if the error was thrown, so that we stay in sync with
          -- the model.
          Right suc                 ->
            truncateAndReopen cmd (its <> iters suc) tipBefore
            -- Note that we might have created an iterator, make sure to close
            -- it as well
  where
    try = tryImmDB EH.monadCatch EH.monadCatch

    truncateAndReopen cmd its tipBefore = try $ do
      -- Close all open iterators as we will perform truncation
      mapM_ iteratorClose its
      -- Close the database in case no errors occurred and it wasn't
      -- closed already. This is idempotent anyway.
      closeDB db
      reopen db ValidateAllEpochs
      deleteAfter internal tipBefore
      -- If the cmd deleted things, we must do it here to have a deterministic
      -- outcome and to stay in sync with the model. If no error was thrown,
      -- these things will have been deleted. If an error was thrown, they
      -- might not have been deleted or only part of them.
      case cmd of
        DeleteAfter tip -> deleteAfter internal tip
        _               -> return ()
      Tip <$> getTip db

semanticsCorruption :: MonadCatch m
                    => HasFS m h
                    -> ImmutableDB    Hash m
                    -> ImmDB.Internal Hash m
                    -> Corruption
                    -> m (Success (Iterator Hash m ByteString))
semanticsCorruption hasFS db _internal (MkCorruption corrs) = do
    closeDB db
    forM_ corrs $ \(corr, file) -> corruptFile hasFS corr file
    reopen db ValidateAllEpochs
    Tip <$> getTip db

-- | The state machine proper
sm :: StrictTVar IO Errors
   -> HasFS IO h
   -> ImmutableDB    Hash IO
   -> ImmDB.Internal Hash IO
   -> DBModel Hash
   -> ModelDBPure
   -> ModelDBInternalPure
   -> StateMachine (Model IO) (At CmdErr IO) IO (At Resp IO)
sm errorsVar hasFS db internal dbm mdb minternal = StateMachine
  { initModel     = initModel dbm mdb minternal
  , transition    = transition
  , precondition  = precondition
  , postcondition = postcondition
  , generator     = Just . generator
  , shrinker      = shrinker
  , semantics     = semantics errorsVar hasFS db internal
  , mock          = mock
  , invariant     = Nothing
  , distribution  = Nothing
  }

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

validate :: forall m. Monad m
         => Model m Concrete -> ImmutableDB Hash m
         -> QC.PropertyM m Property
validate Model {..} realDB = do
    dbContents    <- QC.run   $ getDBContents realDB
    modelContents <- runModel $ getDBContents mockDB
    -- This message is clearer than the one produced by (===)
    let msg = "Mismatch between database (" <> show dbContents <>
              ") and model (" <> show modelContents <> ")"
    return $ counterexample msg (dbContents == modelContents)
  where
    getDBContents db = streamBlocks db Nothing Nothing >>= \case
      -- This should never happen
      Left e   -> error (show e)
      Right it -> iteratorToList it

    runModel :: PureM a -> QC.PropertyM m a
    runModel m = case evalState (runExceptT m) dbModel of
      Left e  -> fail $ prettyImmutableDBError e
      Right a -> return a

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

data Tag
  = TagGetBlockJust

  | TagGetBlockNothing

  | TagGetEBBJust

  | TagGetEBBNothing

  | TagGetBlockHeaderJust

  | TagGetBlockHeaderNothing

  | TagGetEBBHeaderJust

  | TagGetEBBHeaderNothing

  | TagGetBlockHashJust

  | TagGetBlockHashNothing

  | TagGetEBBHashJust

  | TagGetEBBHashNothing

  | TagAppendToSlotInThePastError

  | TagReadFutureSlotError

  | TagInvalidIteratorRangeError

  | TagIteratorStreamedN Int

  | TagIteratorWithoutBounds

  | TagCorruption

  | TagErrorDuringAppendBlock

  | TagErrorDuringAppendEBB

  | TagErrorDuringGetBlock

  | TagErrorDuringGetEBB

  | TagErrorDuringGetBlockHeader

  | TagErrorDuringGetEBBHeader

  | TagErrorDuringGetBlockHash

  | TagErrorDuringGetEBBHash

  | TagErrorDuringStreamBlocks

  | TagErrorDuringStreamHeaders

  | TagErrorDuringIteratorNext

  | TagErrorDuringIteratorPeek

  | TagErrorDuringIteratorClose

  deriving (Show, Eq)


-- | Predicate on events
type EventPred m = C.Predicate (Event m Symbolic) Tag

-- | Convenience combinator for creating classifiers for successful commands
successful :: (    Event m Symbolic
                -> Success (Iterator Hash PureM ByteString)
                -> Either Tag (EventPred m)
              )
           -> EventPred m
successful f = C.predicate $ \ev -> case eventMockResp ev of
    Resp (Left  _ ) -> Right $ successful f
    Resp (Right ok) -> f ev ok

-- | Convenience combinator for creating classifiers for failed commands
failed :: (    Event m Symbolic
            -> ImmutableDBError
            -> Either Tag (EventPred m)
          )
       -> EventPred m
failed f = C.predicate $ \ev -> case eventMockResp ev of
    Resp (Left  e) -> f ev e
    Resp (Right _) -> Right $ failed f

-- | Convenience combinator for creating classifiers for commands failed with
-- a @UserError@.
failedUserError :: (    Event m Symbolic
                     -> UserError
                     -> Either Tag (EventPred m)
                   )
                -> EventPred m
failedUserError f = failed $ \ev e -> case e of
    UserError ue _ -> f ev ue
    _              -> Right $ failedUserError f

-- | Convenience combinator for creating classifiers for commands for which an
-- error is simulated.
simulatedError :: (Event m Symbolic -> Either Tag (EventPred m))
               -> EventPred m
simulatedError f = C.predicate $ \ev ->
    case (_cmdErr (unAt (eventCmdErr ev)), getResp (eventMockResp ev)) of
      (Just _, Right _) -> f ev
      _                 -> Right $ simulatedError f


-- | Tag commands
--
-- Tagging works on symbolic events, so that we can tag without doing real IO.
tag :: forall m. [Event m Symbolic] -> [Tag]
tag = C.classify
    [ tagGetBlockJust
    , tagGetBlockNothing
    , tagGetEBBJust
    , tagGetEBBNothing
    , tagGetBlockHeaderJust
    , tagGetBlockHeaderNothing
    , tagGetEBBHeaderJust
    , tagGetEBBHeaderNothing
    , tagGetBlockHashJust
    , tagGetBlockHashNothing
    , tagGetEBBHashJust
    , tagGetEBBHashNothing
    , tagAppendToSlotInThePastError
    , tagReadFutureSlotError
    , tagInvalidIteratorRangeError
    , tagIteratorStreamedN Map.empty
    , tagIteratorWithoutBounds
    , tagCorruption
    , tagErrorDuring TagErrorDuringAppendBlock $ \case
      { At (AppendBlock {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringAppendEBB $ \case
      { At (AppendEBB {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringGetBlock $ \case
      { At (GetBlock {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringGetEBB $ \case
      { At (GetEBB {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringGetBlockHeader $ \case
      { At (GetBlockHeader {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringGetEBBHeader $ \case
      { At (GetEBBHeader {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringGetBlockHash $ \case
      { At (GetBlockHash {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringGetEBBHash $ \case
      { At (GetEBBHash {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringStreamBlocks $ \case
      { At (StreamBlocks {}) -> True ; _ -> False }
    , tagErrorDuring TagErrorDuringStreamHeaders $ \case
      { At (StreamHeaders {}) -> True ; _ -> False }
    , tagErrorDuring TagErrorDuringIteratorNext $ \case
      { At (IteratorNext {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringIteratorPeek $ \case
      { At (IteratorPeek {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringIteratorClose $ \case
       { At (IteratorClose {}) -> True; _ -> False }
    ]
  where
    tagGetBlockJust :: EventPred m
    tagGetBlockJust = successful $ \ev r -> case r of
      Block (Just _) | GetBlock {} <- unAt $ eventCmd ev ->
        Left TagGetBlockJust
      _ -> Right tagGetBlockJust

    tagGetBlockNothing :: EventPred m
    tagGetBlockNothing = successful $ \ev r -> case r of
      Block Nothing | GetBlock {} <- unAt $ eventCmd ev ->
        Left TagGetBlockNothing
      _ -> Right tagGetBlockNothing

    tagGetEBBJust :: EventPred m
    tagGetEBBJust = successful $ \ev r -> case r of
      EBB (Just _) | GetEBB {} <- unAt $ eventCmd ev ->
        Left TagGetEBBJust
      _ -> Right tagGetEBBJust

    tagGetEBBNothing :: EventPred m
    tagGetEBBNothing = successful $ \ev r -> case r of
      EBB Nothing | GetEBB {} <- unAt $ eventCmd ev ->
        Left TagGetEBBNothing
      _ -> Right tagGetEBBNothing

    tagGetBlockHeaderJust :: EventPred m
    tagGetBlockHeaderJust = successful $ \ev r -> case r of
      BlockHeader (Just _) | GetBlockHeader {} <- unAt $ eventCmd ev ->
        Left TagGetBlockHeaderJust
      _ -> Right tagGetBlockHeaderJust

    tagGetBlockHeaderNothing :: EventPred m
    tagGetBlockHeaderNothing = successful $ \ev r -> case r of
      BlockHeader Nothing | GetBlockHeader {} <- unAt $ eventCmd ev ->
        Left TagGetBlockHeaderNothing
      _ -> Right tagGetBlockHeaderNothing

    tagGetEBBHeaderJust :: EventPred m
    tagGetEBBHeaderJust = successful $ \ev r -> case r of
      EBBHeader (Just _) | GetEBBHeader {} <- unAt $ eventCmd ev ->
        Left TagGetEBBHeaderJust
      _ -> Right tagGetEBBHeaderJust

    tagGetEBBHeaderNothing :: EventPred m
    tagGetEBBHeaderNothing = successful $ \ev r -> case r of
      EBBHeader Nothing | GetEBBHeader {} <- unAt $ eventCmd ev ->
        Left TagGetEBBHeaderNothing
      _ -> Right tagGetEBBHeaderNothing

    tagGetBlockHashJust :: EventPred m
    tagGetBlockHashJust = successful $ \ev r -> case r of
      BlockHash (Just _) | GetBlockHash {} <- unAt $ eventCmd ev ->
        Left TagGetBlockHashJust
      _ -> Right tagGetBlockHashJust

    tagGetBlockHashNothing :: EventPred m
    tagGetBlockHashNothing = successful $ \ev r -> case r of
      BlockHash Nothing | GetBlockHash {} <- unAt $ eventCmd ev ->
        Left TagGetBlockHashNothing
      _ -> Right tagGetBlockHashNothing

    tagGetEBBHashJust :: EventPred m
    tagGetEBBHashJust = successful $ \ev r -> case r of
      EBBHash (Just _) | GetEBBHash {} <- unAt $ eventCmd ev ->
        Left TagGetEBBHashJust
      _ -> Right tagGetEBBHashJust

    tagGetEBBHashNothing :: EventPred m
    tagGetEBBHashNothing = successful $ \ev r -> case r of
      EBBHash Nothing | GetEBBHash {} <- unAt $ eventCmd ev ->
        Left TagGetEBBHashNothing
      _ -> Right tagGetEBBHashNothing

    tagAppendToSlotInThePastError :: EventPred m
    tagAppendToSlotInThePastError = failedUserError $ \_ e -> case e of
      AppendToSlotInThePastError {} -> Left TagAppendToSlotInThePastError
      _                             -> Right tagAppendToSlotInThePastError

    tagReadFutureSlotError :: EventPred m
    tagReadFutureSlotError = failedUserError $ \_ e -> case e of
      ReadFutureSlotError {} -> Left TagReadFutureSlotError
      _                      -> Right tagReadFutureSlotError

    tagInvalidIteratorRangeError :: EventPred m
    tagInvalidIteratorRangeError = failedUserError $ \_ e -> case e of
      InvalidIteratorRangeError {} -> Left TagInvalidIteratorRangeError
      _                            -> Right tagInvalidIteratorRangeError

    tagIteratorStreamedN :: Map (Iterator Hash PureM ByteString) Int
                         -> EventPred m
    tagIteratorStreamedN streamedPerIterator = C.Predicate
      { C.predApply = \ev -> case eventMockResp ev of
          Resp (Right (IterResult (IteratorResult {})))
            | IteratorNext it <- eventMockCmd ev
            -> Right $ tagIteratorStreamedN $
               Map.insertWith (+) it 1 streamedPerIterator
          _ -> Right $ tagIteratorStreamedN streamedPerIterator
      , C.predFinish = do
          -- Find the entry with the highest value, i.e. the iterator that has
          -- streamed the most blocks/headers
          (_, longestStream) <- listToMaybe $ sortBy (flip compare `on` snd) $
            Map.toList streamedPerIterator
          return $ TagIteratorStreamedN longestStream
      }

    tagIteratorWithoutBounds :: EventPred m
    tagIteratorWithoutBounds = successful $ \ev _ -> case eventCmd ev of
      At (StreamBlocks  Nothing Nothing) -> Left TagIteratorWithoutBounds
      At (StreamHeaders Nothing Nothing) -> Left TagIteratorWithoutBounds
      _                                  -> Right tagIteratorWithoutBounds

    tagCorruption :: EventPred m
    tagCorruption = C.Predicate
      { C.predApply = \ev -> case eventCmd ev of
          At (Corruption {}) -> Left  TagCorruption
          _                  -> Right tagCorruption
      , C.predFinish = Nothing
      }

    tagErrorDuring :: Tag -> (At Cmd m Symbolic -> Bool) -> EventPred m
    tagErrorDuring t isErr = simulatedError $ \ev ->
      if isErr (eventCmd ev) then Left t else Right $ tagErrorDuring t isErr


-- | Step the model using a 'QSM.Command' (i.e., a command associated with
-- an explicit set of variables)
execCmd :: Model m Symbolic
        -> QSM.Command (At CmdErr m) (At Resp m)
        -> Event m Symbolic
execCmd model (QSM.Command cmdErr resp _vars) = lockstep model cmdErr resp

-- | 'execCmds' is just the repeated form of 'execCmd'
execCmds :: forall m
          . Model m Symbolic
         -> QSM.Commands (At CmdErr m) (At Resp m) -> [Event m Symbolic]
execCmds model = \(QSM.Commands cs) -> go model cs
  where
    go :: Model m Symbolic -> [QSM.Command (At CmdErr m) (At Resp m)]
       -> [Event m Symbolic]
    go _ []       = []
    go m (c : cs) = let ev = execCmd m c in ev : go (eventAfter ev) cs


{-------------------------------------------------------------------------------
  Required instances

  The 'ToExpr' constraints come from "Data.TreeDiff".
-------------------------------------------------------------------------------}

instance CommandNames (At Cmd m) where
  cmdName (At cmd) = constrName cmd
  cmdNames (_ :: Proxy (At Cmd m r)) =
    constrNames (Proxy @(Cmd (IterRef m r)))

instance CommandNames (At CmdErr m) where
  cmdName (At (CmdErr { _cmd = cmd }) ) = constrName cmd
  cmdNames (_ :: Proxy (At CmdErr m r)) =
    constrNames (Proxy @(Cmd (IterRef m r)))

instance Show (Iterator hash m a) where
  show it = "<iterator " <> show (iteratorID it) <> ">"

instance Show ModelDBPure where
  show _ = "<ModelDB>"

instance Show ModelDBInternalPure where
  show _ = "<ModelDBInternal>"

instance ToExpr SlotNo where
  toExpr (SlotNo w) = App "SlotNo" [toExpr w]

instance ToExpr EpochNo
instance ToExpr EpochSize
instance ToExpr EpochSlot
instance ToExpr RelativeSlot
instance ToExpr BlockNo
instance ToExpr BaseIteratorID
instance ToExpr IteratorID
instance ToExpr (IteratorResult Hash ByteString)
instance ToExpr (IteratorModel Hash)
instance ToExpr (HeaderHash h) => ToExpr (Block.ChainHash h)
instance ToExpr IsEBB
instance ToExpr TestHeaderHash
instance ToExpr TestBodyHash
instance ToExpr TestHeader
instance ToExpr TestBody
instance ToExpr TestBlock
instance ToExpr ImmDB.BlockOrEBB
instance (ToExpr a, ToExpr hash) => ToExpr (ImmDB.WithHash hash a)
instance ToExpr r => ToExpr (C.Tip r)
instance ToExpr b => ToExpr (BinaryInfo b)
instance ToExpr (DBModel Hash)

instance ToExpr FsError where
  toExpr fsError = App (show fsError) []

instance ToExpr ModelDBPure where
  toExpr db = App (show db) []

instance ToExpr ModelDBInternalPure where
  toExpr internal = App (show internal) []

instance ToExpr (Iterator hash m a) where
  toExpr it = App (show it) []

instance ToExpr (EpochInfo Identity) where
  toExpr it = App "fixedSizeEpochInfo" [App (show (runIdentity $ epochInfoSize it 0)) []]

instance ToExpr (Model m Concrete)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

-- | Show minimal examples for each of the generated tags
--
showLabelledExamples'
  :: Maybe Int
  -- ^ Seed
  -> Int
  -- ^ Number of tests to run to find examples
  -> (Tag -> Bool)
  -- ^ Tag filter (can be @const True@)
  -> (   DBModel Hash
      -> ModelDBPure
      -> ModelDBInternalPure
      -> StateMachine (Model m) (At CmdErr m) m (At Resp m)
     )
  -> IO ()
showLabelledExamples' mbReplay numTests focus stateMachine = do
    replaySeed <- case mbReplay of
      Nothing   -> getStdRandom (randomR (1,999999))
      Just seed -> return seed

    labelledExamplesWith (stdArgs { replay     = Just (mkQCGen replaySeed, 0)
                                  , maxSuccess = numTests
                                  }) $
      forAllShrinkShow (QSM.generateCommands smUnused Nothing)
                       (QSM.shrinkCommands   smUnused)
                       ppShow $ \cmds ->
        collects (filter focus . tag . execCmds (QSM.initModel smUnused) $ cmds) $
          property True
  where
    (dbm, mdb, minternal) = mkDBModel
    smUnused = stateMachine dbm mdb minternal

showLabelledExamples :: IO ()
showLabelledExamples = showLabelledExamples' Nothing 1000 (const True) $
    sm (error "errorsVar unused") hasFsUnused dbUnused internalUnused

prop_sequential :: Property
prop_sequential = forAllCommands smUnused Nothing $ \cmds -> QC.monadicIO $ do
    let test :: StrictTVar IO Mock.MockFS
             -> StrictTVar IO Errors
             -> HasFS IO h
             -> QC.PropertyM IO (
                    QSM.History (At CmdErr IO) (At Resp IO)
                  , Property
                  )
        test fsVar errorsVar hasFS = do
          let parser = epochFileParser hasFS (const <$> decode) isEBB
                getBinaryInfo testBlockIsValid
          (tracer, getTrace) <- QC.run recordingTracerIORef
          (db, internal) <- QC.run $ openDBInternal hasFS EH.monadCatch
            (fixedSizeEpochInfo fixedEpochSize) testHashInfo
            ValidateMostRecentEpoch parser tracer
          let sm' = sm errorsVar hasFS db internal dbm mdb minternal
          (hist, model, res) <- runCommands sm' cmds
          trace <- QC.run getTrace
          QC.monitor $ counterexample ("Trace: " <> unlines (map show trace))
          fsDump <- QC.run $ Mock.pretty <$> atomically (readTVar fsVar)
          QC.monitor $ counterexample ("FS: " <> fsDump)
          QC.run $ closeDB db >> reopen db ValidateAllEpochs
          validation <- validate model db
          dbTip <- QC.run $ getTip db <* closeDB db

          let modelTip = dbmTip $ dbModel model
          QC.monitor $ counterexample ("dbTip:    " <> show dbTip)
          QC.monitor $ counterexample ("modelTip: " <> show modelTip)

          return (hist, res === Ok .&&. dbTip === modelTip .&&. validation)

    fsVar     <- QC.run $ uncheckedNewTVarM Mock.empty
    errorsVar <- QC.run $ uncheckedNewTVarM mempty
    (hist, prop) <-
      test fsVar errorsVar (mkSimErrorHasFS EH.monadCatch fsVar errorsVar)
    prettyCommands smUnused hist
      $ tabulate "Tags" (map show $ tag (execCmds (QSM.initModel smUnused) cmds))
      $ prop
  where
    (dbm, mdb, minternal) = mkDBModel
    smUnused = sm (error "errorsVar unused") hasFsUnused dbUnused
      internalUnused dbm mdb minternal
    isEBB = testBlockEpochNoIfEBB fixedEpochSize
    getBinaryInfo = void . testBlockToBinaryInfo

tests :: TestTree
tests = testGroup "ImmutableDB q-s-m"
    [ testProperty "sequential" prop_sequential
    ]

fixedEpochSize :: EpochSize
fixedEpochSize = 10

mkDBModel :: MonadState (DBModel Hash) m
          =>  (DBModel Hash
             , ImmutableDB    Hash (ExceptT ImmutableDBError m)
             , ImmDB.Internal Hash (ExceptT ImmutableDBError m)
             )
mkDBModel = openDBModel EH.exceptT (const fixedEpochSize)

dbUnused :: ImmutableDB Hash m
dbUnused = error "semantics and DB used during command generation"

internalUnused :: ImmDB.Internal Hash m
internalUnused = error "semantics and internal DB API used during command generation"

hasFsUnused :: HasFS m h
hasFsUnused = error "HasFS only used during execution"
