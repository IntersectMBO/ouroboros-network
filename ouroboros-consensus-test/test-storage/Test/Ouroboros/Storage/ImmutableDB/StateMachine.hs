{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans      #-}
module Test.Ouroboros.Storage.ImmutableDB.StateMachine (
    showLabelledExamples
  , tests
  ) where

import           Prelude hiding (elem, notElem)

import           Control.Monad (forM_, void)
import           Data.Bifunctor (first)
import           Data.ByteString.Lazy (ByteString)
import           Data.Coerce (Coercible, coerce)
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.Functor.Classes (Eq1, Show1)
import           Data.Functor.Identity (Identity)
import           Data.List (delete, partition, sortBy)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe)
import           Data.TreeDiff (Expr (App), defaultExprViaShow)
import           Data.Typeable (Typeable)
import           Data.Word (Word16, Word32, Word64)
import           GHC.Generics (Generic, Generic1)
import           GHC.Stack (HasCallStack)
import qualified Generics.SOP as SOP
import           NoThunks.Class (AllowThunk (..))
import           System.Random (getStdRandom, randomR)
import           Text.Show.Pretty (ppShow)

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Random (mkQCGen)
import           Test.StateMachine hiding (showLabelledExamples,
                     showLabelledExamples')
import qualified Test.StateMachine.Sequential as QSM
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (HasFS (..), SomeHasFS (..))
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError (..), FsPath,
                     mkFsPath)
import           Ouroboros.Consensus.Storage.ImmutableDB hiding (streamAll)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (unsafeChunkNoToEpochNo)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
                     (CacheConfig (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util

import           Test.Util.ChunkInfo
import qualified Test.Util.Classify as C
import           Test.Util.FS.Sim.Error (Errors, mkSimErrorHasFS, withErrors)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.Orphans.Slotting.Arbitrary ()
import           Test.Util.Orphans.ToExpr ()
import           Test.Util.QuickCheck (collects)
import           Test.Util.RefEnv (RefEnv)
import qualified Test.Util.RefEnv as RE
import           Test.Util.SOP
import           Test.Util.Tracer (recordingTracerIORef)
import           Test.Util.WithEq

import           Test.Ouroboros.Storage.ImmutableDB.Model
import           Test.Ouroboros.Storage.Orphans ()
import           Test.Ouroboros.Storage.TestBlock

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
data Cmd it =
    GetTip
  | GetBlockComponent      (RealPoint TestBlock)
  | AppendBlock            TestBlock
  | Stream                 (StreamFrom TestBlock) (StreamTo TestBlock)
  | StreamAll
  | IteratorNext           it
  | IteratorHasNext        it
  | IteratorClose          it
  | Reopen                 ValidationPolicy
  | Migrate                ValidationPolicy
  | DeleteAfter            (WithOrigin (Tip TestBlock))
  | Corruption             Corruption
  deriving (Generic, Show, Functor, Foldable, Traversable)

deriving instance SOP.Generic         (Cmd it)
deriving instance SOP.HasDatatypeInfo (Cmd it)

-- | Simulate corruption of some files of the database.
newtype Corruption = MkCorruption { getCorruptions :: Corruptions }
  deriving (Generic, Show)

-- | A 'Cmd' together with 'Errors'.
--
-- When executing the 'Cmd', these 'Errors' are passed to 'SimErrorFS' to
-- simulate file system errors thrown at the 'HasFS' level. When 'Nothing', no
-- errors will be thrown.
data CmdErr it = CmdErr {
      cmdErr :: Maybe Errors
    , cmd    :: Cmd it
    }
  deriving (Show, Generic, Functor, Foldable, Traversable)

-- | Return type for successful database operations.
data Success it =
    Unit            ()
  | ErAllComponents (Either (MissingBlock TestBlock) (AllComponents TestBlock))
  | Iter            (Either (MissingBlock TestBlock) it)
  | IterResult      (IteratorResult (AllComponents TestBlock))
  | IterHasNext     (Maybe (RealPoint TestBlock))
  | IterResults     [AllComponents TestBlock]
  | ImmTip          (WithOrigin (Tip TestBlock))
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Product of all 'BlockComponent's. As this is a GADT, generating random
-- values of it (and combinations!) is not so simple. Therefore, we just
-- always request all block components.
allComponents :: BlockComponent blk (AllComponents blk)
allComponents = (,,,,,,,,,,)
    <$> GetVerifiedBlock
    <*> GetBlock
    <*> GetRawBlock
    <*> GetHeader
    <*> GetRawHeader
    <*> GetHash
    <*> GetSlot
    <*> GetIsEBB
    <*> GetBlockSize
    <*> GetHeaderSize
    <*> GetNestedCtxt

-- | A list of all the 'BlockComponent' indices (@b@) we are interested in.
type AllComponents blk =
  ( blk
  , blk
  , ByteString
  , Header blk
  , ByteString
  , HeaderHash blk
  , SlotNo
  , IsEBB
  , Word32
  , Word16
  , SomeSecond (NestedCtxt Header) blk
  )

-- | Short-hand
type TestIterator m = WithEq (Iterator m TestBlock (AllComponents TestBlock))

closeOpenIterators :: StrictTVar IO [TestIterator IO] -> IO ()
closeOpenIterators varIters = do
    its <- atomically $ readTVar varIters <* writeTVar varIters []
    mapM_ iteratorClose (unWithEq <$> its)

open :: ImmutableDbArgs Identity IO TestBlock -> IO ImmutableDBState
open args = do
    (db, internal) <- openDBInternal args
    return ImmutableDBState { db, internal }

-- | Opens a new ImmutableDB and stores it in 'varDB'.
--
-- Does not close the current VolatileDB stored in 'varDB'.
reopen :: ImmutableDBEnv -> ValidationPolicy -> IO ()
reopen ImmutableDBEnv { varDB, args } valPol = do
    immutableDbState <- open args { immValidationPolicy = valPol }
    void $ swapMVar varDB immutableDbState

-- | Run the command against the given database.
run ::
     HasCallStack
  => ImmutableDBEnv
  -> Cmd (TestIterator IO)
  -> IO (Success (TestIterator IO))
run env@ImmutableDBEnv {
        varDB
      , varNextId
      , varIters
      , args = ImmutableDbArgs {
            immRegistry = registry
          , immHasFS = SomeHasFS hasFS
          }
      } cmd =
    readMVar varDB >>= \ImmutableDBState { db, internal } -> case cmd of
      GetTip               -> ImmTip          <$> atomically (getTip            db)
      GetBlockComponent pt -> ErAllComponents <$>             getBlockComponent db allComponents pt
      AppendBlock blk      -> Unit            <$>             appendBlock       db blk
      Stream f t           -> iter            =<<             stream            db registry allComponents f t
      StreamAll            -> IterResults     <$>             streamAll         db
      IteratorNext    it   -> IterResult      <$>             iteratorNext      (unWithEq it)
      IteratorHasNext it   -> IterHasNext     <$> atomically (iteratorHasNext   (unWithEq it))
      IteratorClose   it   -> Unit            <$>             iteratorClose'              it
      DeleteAfter tip -> do
        closeOpenIterators varIters
        Unit <$> deleteAfter internal tip
      Reopen valPol -> do
        closeOpenIterators varIters
        closeDB db
        reopen env valPol
        db' <- getImmutableDB env
        ImmTip <$> atomically (getTip db')
      Migrate valPol -> do
        closeOpenIterators varIters
        closeDB db
        unmigrate hasFS
        reopen env valPol
        db' <- getImmutableDB env
        ImmTip <$> atomically (getTip db')
      Corruption (MkCorruption corrs) -> do
        closeOpenIterators varIters
        closeDB db
        forM_ corrs $ \(corr, file) -> corruptFile hasFS corr file
        reopen env ValidateAllChunks
        db' <- getImmutableDB env
        ImmTip <$> atomically (getTip db')
  where
    -- Store the iterator in 'varIters'
    iter ::
         Either
           (MissingBlock TestBlock)
           (Iterator IO TestBlock (AllComponents TestBlock))
      -> IO (Success (TestIterator IO))
    iter (Left e)   = return (Iter (Left e))
    iter (Right it) = do
      it' <- giveWithEq it
      atomically $ modifyTVar varIters (it':)
      return (Iter (Right it'))

    -- Remove the iterator from 'varIters'
    iteratorClose' :: TestIterator IO -> IO ()
    iteratorClose' it = do
      atomically $ modifyTVar varIters (delete it)
      iteratorClose (unWithEq it)

    giveWithEq :: a -> IO (WithEq a)
    giveWithEq a =
      fmap (`WithEq` a) $ atomically $ stateTVar varNextId $ \i -> (i, succ i)

    streamAll :: ImmutableDB IO TestBlock -> IO [AllComponents TestBlock]
    streamAll db =
      bracket
        (ImmutableDB.streamAll db registry allComponents)
        iteratorClose
        iteratorToList

-- | To test migration from "XXXXX.epoch" to "XXXXX.chunk" do the opposite
-- renaming, i.e., /unmigrate/ while the database is closed. When the database
-- is reopened, it should trigger the migration code.
unmigrate :: Monad m => HasFS m h -> m ()
unmigrate HasFS { listDirectory, renameFile } = do
    (chunkFiles, _, _) <- dbFilesOnDisk <$> listDirectory (mkFsPath [])
    forM_ chunkFiles $ \chunk ->
      renameFile (fsPathChunkFile chunk) (renderFile "epoch" chunk)

{-------------------------------------------------------------------------------
  Instantiating the semantics
-------------------------------------------------------------------------------}

-- | Responses are either successful termination or an error.
newtype Resp it = Resp { getResp :: Either (ImmutableDBError TestBlock) (Success it) }
  deriving (Eq, Functor, Foldable, Traversable, Show)

-- | Run the pure command against the given database.
runPure ::
     Cmd IteratorId
  -> DBModel TestBlock
  -> (Resp IteratorId, DBModel TestBlock)
runPure = \case
    GetTip               -> ok ImmTip          $ query     getTipModel
    GetBlockComponent pt -> ok ErAllComponents $ query    (getBlockComponentModel allComponents pt)
    AppendBlock blk      -> ok Unit            $ updateE_ (appendBlockModel blk)
    Stream f t           -> ok Iter            $ updateEE (streamModel f t)
    StreamAll            -> ok IterResults     $ query    (streamAllModel allComponents)
    IteratorNext    it   -> ok IterResult      $ update   (iteratorNextModel it allComponents)
    IteratorHasNext it   -> ok IterHasNext     $ query    (iteratorHasNextModel it)
    IteratorClose   it   -> ok Unit            $ update_  (iteratorCloseModel it)
    DeleteAfter tip      -> ok Unit            $ update_  (deleteAfterModel tip)
    Corruption corr      -> ok ImmTip          $ update   (simulateCorruptions (getCorruptions corr))
    Reopen _             -> ok ImmTip          $ update    reopenModel
    Migrate _            -> ok ImmTip          $ update    reopenModel
  where
    query  f m = (Right (f m), m)

    update   f m = first Right (f m)
    update_  f m = (Right (), f m)
    updateE_ f m = case f m of
      Left  e  -> (Left e, m)
      Right m' -> (Right (), m')
    updateEE ::
         (DBModel TestBlock -> Either (ImmutableDBError TestBlock) (Either e (a, DBModel TestBlock)))
      -> DBModel TestBlock
      -> (Either (ImmutableDBError TestBlock) (Either e a), DBModel TestBlock)
    updateEE f m = case f m of
      Left e                -> (Left e, m)
      Right (Left e)        -> (Right (Left e), m)
      Right (Right (a, m')) -> (Right (Right a), m')

    ok ::
         (a -> Success IteratorId)
      -> (DBModel TestBlock -> (Either (ImmutableDBError TestBlock) a, DBModel TestBlock))
      -> DBModel TestBlock
      -> (Resp IteratorId, DBModel TestBlock)
    ok toSuccess f m = first (Resp . fmap toSuccess) $ f m

-- | Run a command against the pure model
runPureErr :: DBModel TestBlock
           -> CmdErr IteratorId
           -> (Resp IteratorId, DBModel TestBlock)
runPureErr dbm (CmdErr mbErrors cmd) =
    case (mbErrors, runPure cmd dbm) of
      -- No simulated errors, just step
      (Nothing, (resp, dbm')) -> (resp, dbm')
      -- An error will be simulated and thrown (not here, but in the real
      -- implementation). To mimic what the implementation will do, we only
      -- have to close the iterators, as the truncation during the reopening
      -- of the database will erase any changes.
      (Just _, (_resp, dbm')) ->
        -- We ignore the updated @dbm'@, because we have to roll back to the
        -- state before executing cmd. The only exception is the DeleteAfter
        -- cmd, in which case we have to roll back to the requested tip.
        --
        -- As the implementation closes all iterators, we do the same.
        let dbm'' = closeAllIterators $ case cmd of
              DeleteAfter _ -> dbm'
              _             -> dbm
        in (Resp $ Right $ ImmTip $ dbmTip dbm'', dbm'')

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
type IterRef m = Reference (Opaque (TestIterator m))

-- | Mapping between iterator references and mocked iterators
type KnownIters m = RefEnv (Opaque (TestIterator m))
                           IteratorId

-- | Execution model
data Model m r = Model
  { dbModel    :: DBModel TestBlock
    -- ^ A model of the database, used as state for the 'HasImmutableDB'
    -- instance of 'ModelDB'.
  , knownIters :: KnownIters m r
    -- ^ Store a mapping between iterator references and mocked iterators.
  } deriving (Show, Generic)

nbOpenIterators :: Model m r -> Int
nbOpenIterators model = length (RE.toList (knownIters model))

-- | Initial model
initModel :: DBModel TestBlock -> Model m r
initModel dbModel = Model { knownIters  = RE.empty, dbModel }

-- | Key property of the model is that we can go from real to mock responses
toMock :: (Functor t, Eq1 r)
       => Model m r -> At t m r -> t IteratorId
toMock Model {..} (At t) = fmap (knownIters RE.!) t

-- | Step the mock semantics
step :: Eq1 r
     => Model m r
     -> At CmdErr m r
     -> (Resp IteratorId, DBModel TestBlock)
step model@Model{..} cmdErr = runPureErr dbModel (toMock model cmdErr)

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
  , eventMockResp :: Resp IteratorId
  } deriving (Show)

eventCmdNoErr :: Event m r -> At Cmd m r
eventCmdNoErr = At . cmd . unAt . eventCmdErr

eventMockCmdNoErr :: Eq1 r => Event m r -> Cmd IteratorId
eventMockCmdNoErr ev@Event {..} = toMock eventBefore (eventCmdNoErr ev)


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
generator m = do
    cmd    <- unAt <$> generateCmd m
    cmdErr <-
      if errorFor cmd then
        frequency [
            -- We want to make some progress
            (4, return Nothing)
          , (1, Just <$> arbitrary)
          ]
       else
         return Nothing
    return $ At CmdErr {..}
  where
    -- Don't simulate an error during corruption, because we don't want an
    -- error to happen while we corrupt a file.
    errorFor Corruption {} = False
    errorFor _             = True

-- | Generate a 'Cmd'.
generateCmd :: forall m. Model m Symbolic -> Gen (At Cmd m Symbolic)
generateCmd Model {..} = At <$> frequency
    [ -- Block
      (1, GetBlockComponent <$> genGetBlock)
    , (5, AppendBlock <$> genAppendRegularBlock)
    , (if modelSupportsEBBs then 2 else 0, AppendBlock <$> genAppendEBB)
    , (if empty then 1 else 4, return StreamAll)
    , (if empty then 1 else 4, uncurry Stream <$> genBounds)
    , (if noIters then 0 else 5, IteratorNext    <$> pickIter)
    , (if noIters then 0 else 5, IteratorHasNext <$> pickIter)
    , (if noIters then 0 else 1, IteratorClose   <$> pickIter)
    , (1, Reopen <$> genValPol)
    , (1, Migrate <$> genValPol)
    , (1, DeleteAfter <$> genTip)
    , (if null dbFiles then 0 else 1, Corruption <$> genCorruption)
    ]
  where
    DBModel {..} = dbModel
    modelSupportsEBBs = chunkInfoSupportsEBBs dbmChunkInfo
    currentEpoch      = unsafeChunkNoToEpochNo $ dbmCurrentChunk dbModel
    canContainEBB     = const modelSupportsEBBs -- TODO: we could be more precise

    -- Construct a 'SlotNo' @n@ chunks later
    inLaterChunk :: Word -> SlotNo -> SlotNo
    inLaterChunk 0 s = s
    inLaterChunk n s = inLaterChunk (n - 1) $
                         SlotNo (unSlotNo s + numRegularBlocks size)
      where
        chunk = chunkIndexOfSlot dbmChunkInfo s
        size  = getChunkSize     dbmChunkInfo chunk

    lastBlock :: WithOrigin TestBlock
    lastBlock = dbmTipBlock dbModel

    lastSlot :: SlotNo
    lastSlot = withOrigin (SlotNo 0) blockSlot lastBlock

    lastBlockIsEBB :: Bool
    lastBlockIsEBB = withOrigin False (fromIsEBB . testBlockIsEBB) lastBlock

    dbFiles :: [FsPath]
    dbFiles = getDBFiles dbModel

    blocks :: [TestBlock]
    blocks = dbmBlocks dbModel

    ebbs, regularBlocks :: [TestBlock]
    (ebbs, regularBlocks) = partition (fromIsEBB . blockToIsEBB) blocks

    empty, noRegularBlocks, noEBBs :: Bool
    empty           = null blocks
    noRegularBlocks = null regularBlocks
    noEBBs          = null ebbs

    noIters :: Bool
    noIters = Map.null dbmIterators

    -- Randomly pick one of the open iterators
    --
    -- PRECONDITION: there is at least one open iterator
    pickIter :: Gen (IterRef m Symbolic)
    pickIter = elements (RE.keys knownIters)

    genRandomPoint :: Gen (RealPoint TestBlock)
    genRandomPoint =
        RealPoint
          <$> arbitrary
          <*> (TestHeaderHash <$> arbitrary)

    genGetBlock :: Gen (RealPoint TestBlock)
    genGetBlock = frequency [
          (if noRegularBlocks then 0 else 4, elements (map blockRealPoint regularBlocks))
        , (if noEBBs          then 0 else 2, elements (map blockRealPoint ebbs))
        , (1, genRandomPoint)
        ]

    genAppendRegularBlock :: Gen TestBlock
    genAppendRegularBlock = do
        slotNo <- frequency [
            -- Slot in the past -> invalid
            (1, chooseSlot (0, lastSlot))
            -- If the previous block is an EBB, make a regular block in
            -- the same slot number. The slot can still be empty, though.
          , (if lastBlockIsEBB then 7 else 0, return lastSlot)
            -- Slots not too far in the future
          , (4, chooseSlot (lastSlot, lastSlot + 10))
            -- Slots in some future chunk
          , (1, chooseSlot (inLaterChunk 1 lastSlot,
                            inLaterChunk 4 lastSlot))
          ]
        return $
          (withOrigin firstBlock mkNextBlock lastBlock)
            slotNo
            (TestBody 0 True)

    genAppendEBB :: Gen TestBlock
    genAppendEBB = case lastBlock of
        Origin              -> return $ firstEBB canContainEBB (TestBody 0 True)
        NotOrigin prevBlock -> do
          epoch <- frequency
          -- Epoch in the past -> invalid
            [ (1, chooseEpoch (0, currentEpoch))
            , (3, chooseEpoch (currentEpoch, currentEpoch + 5))
            ]
          let slotNo = slotNoOfEBB dbmChunkInfo epoch
          return $ mkNextEBB canContainEBB prevBlock slotNo epoch (TestBody 0 True)

    -- Both random points and existing points
    genRandomOrExisting :: Gen (RealPoint TestBlock)
    genRandomOrExisting = frequency [
          (1, genRandomPoint)
        , (if empty then 0 else 1, elements (map blockRealPoint blocks))
        ]

    genStreamFromWith :: Gen (RealPoint blk) -> Gen (StreamFrom blk)
    genStreamFromWith genPoint = oneof [
          StreamFromExclusive <$> frequency [
              (1, return GenesisPoint)
            , (4, realPointToPoint <$> genPoint)
            ]
        , StreamFromInclusive <$> genPoint
        ]

    genRandomOrExistingStreamFrom :: Gen (StreamFrom TestBlock)
    genRandomOrExistingStreamFrom = genStreamFromWith genRandomOrExisting

    genRandomOrExistingStreamTo :: Gen (StreamTo TestBlock)
    genRandomOrExistingStreamTo =
        StreamToInclusive <$> genRandomOrExisting

    -- PRECONDITION: not empty
    genStreamFrom :: Gen (StreamFrom TestBlock)
    genStreamFrom = genStreamFromWith (elements (map blockRealPoint blocks))

    -- Tries to generate an upper bound /after/ the lower bound. This can fail,
    -- i.e., when the lower bound is an exclusive bound corresponding to the
    -- last block. In that case, we give up and use the same block as the upper
    -- bound, resulting in invalid bounds.
    --
    -- PRECONDITION: not empty and the given block exists
    genStreamTo :: StreamFrom TestBlock -> Gen (StreamTo TestBlock)
    genStreamTo = fmap StreamToInclusive . \case
        StreamFromExclusive pt -> case pointToWithOriginRealPoint pt of
          Origin        -> elements (map blockRealPoint blocks)
          NotOrigin pt' -> genPointAfter pt'
        StreamFromInclusive pt -> genPointAfter pt
      where
        -- Can generate the given point itself
        genPointAfter :: RealPoint TestBlock -> Gen (RealPoint TestBlock)
        genPointAfter pt =
              elements
            . dropWhile (/= pt)
            . map blockRealPoint
            $ blocks

    genBounds :: Gen (StreamFrom TestBlock, StreamTo TestBlock)
    genBounds = frequency [
        -- Likely an invalid range
          (1, (,) <$> genRandomOrExistingStreamFrom <*> genRandomOrExistingStreamTo)
        -- A valid iterator
        , (if empty then 0 else 3, do
             from <- genStreamFrom
             to   <- genStreamTo from
             return (from, to))
        ]

    chooseWord64 :: Coercible a Word64 => (a, a) -> Gen a
    chooseWord64 (start, end) = coerce $ choose @Word64 (coerce start, coerce end)

    chooseSlot :: (SlotNo, SlotNo) -> Gen SlotNo
    chooseSlot = chooseWord64

    chooseEpoch :: (EpochNo, EpochNo) -> Gen EpochNo
    chooseEpoch = chooseWord64

    genCorruption :: Gen Corruption
    genCorruption = MkCorruption <$> generateCorruptions (NE.fromList dbFiles)

    genValPol :: Gen ValidationPolicy
    genValPol = elements [ValidateMostRecentChunk, ValidateAllChunks]

    genTip :: Gen (WithOrigin (Tip TestBlock))
    genTip = elements $ NE.toList $ tips dbModel

-- | Return the files that the database with the given model would have
-- created. For each epoch an epoch, primary index, and secondary index file.
getDBFiles :: DBModel TestBlock -> [FsPath]
getDBFiles dbm =
    [ file
    | chunk <- chunksBetween firstChunkNo (dbmCurrentChunk dbm)
    , file  <-
      [ fsPathChunkFile chunk
      , fsPathPrimaryIndexFile chunk
      , fsPathSecondaryIndexFile chunk
      ]
    ]

{-------------------------------------------------------------------------------
  Shrinking
-------------------------------------------------------------------------------}

-- | Shrinker
shrinker :: Model m Symbolic -> At CmdErr m Symbolic -> [At CmdErr m Symbolic]
shrinker m (At (CmdErr mbErrors cmd)) = fmap At $
    [CmdErr mbErrors' cmd  | mbErrors' <- shrink mbErrors] ++
    [CmdErr mbErrors  cmd' | At cmd'   <- shrinkCmd m (At cmd)]

-- | Shrink a 'Cmd'.
shrinkCmd :: Model m Symbolic -> At Cmd m Symbolic -> [At Cmd m Symbolic]
shrinkCmd _ (At cmd) = fmap At $ case cmd of
    Corruption corr -> [Corruption corr' | corr' <- shrinkCorruption corr]
    _otherwise      -> []
  where
    shrinkCorruption (MkCorruption corrs) =
        [ MkCorruption corrs'
        | corrs' <- shrinkCorruptions corrs
        ]

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
precondition Model {..} (At (CmdErr { cmd })) =
   forall (iters cmd) (`member` RE.keys knownIters) .&&
    case cmd of
      AppendBlock blk -> fitsOnTip blk
      DeleteAfter tip -> tip `member` NE.toList (tips dbModel)
      Corruption corr ->
        forall
          (corruptionFiles (getCorruptions corr))
          (`member` getDBFiles dbModel)
      _ -> Top
  where
    fitsOnTip :: TestBlock -> Logic
    fitsOnTip b = case dbmTipBlock dbModel of
      Origin          -> blockPrevHash b .== GenesisHash
      NotOrigin bPrev -> blockPrevHash b .== BlockHash (blockHash bPrev)

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

data ImmutableDBState = ImmutableDBState {
      db       :: ImmutableDB          IO TestBlock
    , internal :: ImmutableDB.Internal IO TestBlock
    }
  deriving NoThunks via AllowThunk ImmutableDBState

-- | Environment to run commands against the real ImmutableDB implementation.
data ImmutableDBEnv = ImmutableDBEnv {
      varErrors :: StrictTVar IO Errors
    , varNextId :: StrictTVar IO Id
    , varIters  :: StrictTVar IO [TestIterator IO]
      -- ^ A list of all open iterators. For some commands, e.g., corrupting the
      -- database or simulating errors, we need to close and reopen the
      -- database, which almost always requires truncation of the database.
      -- During truncation we might need to delete a file that is still opened
      -- by an iterator. As this is not allowed by the MockFS implementation, we
      -- first close all open iterators in these cases.
    , varDB     :: StrictMVar IO ImmutableDBState
    , args      :: ImmutableDbArgs Identity IO TestBlock
    }

getImmutableDB :: ImmutableDBEnv -> IO (ImmutableDB IO TestBlock)
getImmutableDB = fmap db . readMVar . varDB

getInternal :: ImmutableDBEnv -> IO (ImmutableDB.Internal IO TestBlock)
getInternal = fmap internal . readMVar . varDB

semantics ::
     ImmutableDBEnv
  -> At CmdErr IO Concrete
  -> IO (At Resp IO Concrete)
semantics env@ImmutableDBEnv {..} (At cmdErr) =
    At . fmap (reference . Opaque) . Resp <$> case opaque <$> cmdErr of

      CmdErr Nothing       cmd -> tryImmutableDB (Proxy @TestBlock) $ run env cmd

      CmdErr (Just errors) cmd -> do
        tipBefore <- getImmutableDB env >>= atomically . getTip
        res       <- withErrors varErrors errors $
                       tryImmutableDB (Proxy @TestBlock) $ run env cmd
        case res of
          -- If the command resulted in a 'ApiMisuse', we didn't even get the
          -- chance to run into a simulated error. Note that we still
          -- truncate, because we can't predict whether we'll get a
          -- 'ApiMisuse' or an 'UnexpectedFailure', as it depends on the
          -- simulated error.
          Left (ApiMisuse {})       ->
            truncateAndReopen cmd tipBefore

          -- We encountered a simulated error
          Left (UnexpectedFailure {}) ->
            truncateAndReopen cmd tipBefore

          -- TODO track somewhere which/how many errors were *actually* thrown

          -- By coincidence no error was thrown, try to mimic what would have
          -- happened if the error was thrown, so that we stay in sync with
          -- the model.
          Right _suc                 ->
            truncateAndReopen cmd tipBefore
            -- Note that we might have created an iterator, make sure to close
            -- it as well
  where
    ImmutableDbArgs { immRegistry } = args

    truncateAndReopen cmd tipBefore = tryImmutableDB (Proxy @TestBlock) $ do
      -- Close all open iterators as we will perform truncation
      closeOpenIterators varIters
      -- Close the database in case no errors occurred and it wasn't
      -- closed already. This is idempotent anyway.
      getImmutableDB env >>= closeDB
      -- Release any handles that weren't closed because of a simulated error.
      releaseAll immRegistry
      reopen env ValidateAllChunks
      getInternal env >>= flip deleteAfter tipBefore
      -- If the cmd deleted things, we must do it here to have a deterministic
      -- outcome and to stay in sync with the model. If no error was thrown,
      -- these things will have been deleted. If an error was thrown, they
      -- might not have been deleted or only part of them.
      case cmd of
        DeleteAfter tip -> getInternal env >>= flip deleteAfter tip
        _               -> return ()
      ImmTip <$> (getImmutableDB env >>= atomically . getTip)

-- | The state machine proper
sm ::
     ImmutableDBEnv
  -> DBModel TestBlock
  -> StateMachine (Model IO) (At CmdErr IO) IO (At Resp IO)
sm env dbm = StateMachine {
      initModel     = initModel dbm
    , transition    = transition
    , precondition  = precondition
    , postcondition = postcondition
    , generator     = Just . generator
    , shrinker      = shrinker
    , semantics     = semantics env
    , mock          = mock
    , invariant     = Nothing
    , cleanup       = noCleanup
    }

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

data Tag =
    TagGetBlockComponentFound

  | TagGetBlockComponentFoundEBB

  | TagGetBlockComponentEmptySlot

  | TagGetBlockComponentWrongHash

  | TagGetBlockComponentNewerThanTip

  | TagAppendBlockNotNewerThanTipError

  | TagInvalidIteratorRangeError

  | TagIteratorStreamedN Int

  | TagCorruption

  | TagMigrate

  | TagErrorDuringAppendBlock

  | TagErrorDuringGetBlockComponent

  | TagErrorDuringStream

  | TagErrorDuringIteratorNext

  | TagErrorDuringIteratorClose

  deriving (Show, Eq)


-- | Predicate on events
type EventPred m = C.Predicate (Event m Symbolic) Tag

-- | Convenience combinator for creating classifiers for successful commands
successful :: (    Event m Symbolic
                -> Success IteratorId
                -> Either Tag (EventPred m)
              )
           -> EventPred m
successful f = C.predicate $ \ev -> case eventMockResp ev of
    Resp (Left  _ ) -> Right $ successful f
    Resp (Right ok) -> f ev ok

-- | Convenience combinator for creating classifiers for failed commands
failed :: (    Event m Symbolic
            -> ImmutableDBError TestBlock
            -> Either Tag (EventPred m)
          )
       -> EventPred m
failed f = C.predicate $ \ev -> case eventMockResp ev of
    Resp (Left  e) -> f ev e
    Resp (Right _) -> Right $ failed f

-- | Convenience combinator for creating classifiers for commands failed with
-- a @ApiMisuse@.
failedApiMisuse :: (    Event m Symbolic
                     -> ApiMisuse TestBlock
                     -> Either Tag (EventPred m)
                   )
                -> EventPred m
failedApiMisuse f = failed $ \ev e -> case e of
    ApiMisuse am _ -> f ev am
    _              -> Right $ failedApiMisuse f

-- | Convenience combinator for creating classifiers for commands for which an
-- error is simulated.
simulatedError :: (Event m Symbolic -> Either Tag (EventPred m))
               -> EventPred m
simulatedError f = C.predicate $ \ev ->
    case (cmdErr (unAt (eventCmdErr ev)), getResp (eventMockResp ev)) of
      (Just _, Right _) -> f ev
      _                 -> Right $ simulatedError f


-- | Tag commands
--
-- Tagging works on symbolic events, so that we can tag without doing real IO.
tag :: forall m. [Event m Symbolic] -> [Tag]
tag = C.classify
    [ tagGetBlockComponentFound
    , tagGetBlockComponentFoundEBB
    , tagGetBlockComponentEmptySlot
    , tagGetBlockComponentWrongHash
    , tagGetBlockComponentNewerThanTip
    , tagAppendBlockNotNewerThanTipError
    , tagInvalidIteratorRangeError
    , tagIteratorStreamedN Map.empty
    , tagCorruption
    , tagMigrate
    , tagErrorDuring TagErrorDuringAppendBlock $ \case
      { At (AppendBlock {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringGetBlockComponent $ \case
      { At (GetBlockComponent {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringStream $ \case
      { At (Stream {}) -> True ; _ -> False }
    , tagErrorDuring TagErrorDuringIteratorNext $ \case
      { At (IteratorNext {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringIteratorClose $ \case
       { At (IteratorClose {}) -> True; _ -> False }
    ]
  where
    tagGetBlockComponentFound :: EventPred m
    tagGetBlockComponentFound = successful $ \ev r -> case r of
      ErAllComponents (Right _) | GetBlockComponent {} <- unAt $ eventCmdNoErr ev ->
        Left TagGetBlockComponentFound
      _ -> Right tagGetBlockComponentFound

    tagGetBlockComponentFoundEBB :: EventPred m
    tagGetBlockComponentFoundEBB = successful $ \ev r -> case r of
      ErAllComponents (Right (_, _, _, _, _, _, _, IsEBB, _, _, _))
        | GetBlockComponent {} <- unAt $ eventCmdNoErr ev
        -> Left TagGetBlockComponentFoundEBB
      _ -> Right tagGetBlockComponentFoundEBB

    tagGetBlockComponentEmptySlot :: EventPred m
    tagGetBlockComponentEmptySlot = successful $ \ev r -> case r of
      ErAllComponents (Left (EmptySlot {})) | GetBlockComponent {} <- unAt $ eventCmdNoErr ev ->
        Left TagGetBlockComponentEmptySlot
      _ -> Right tagGetBlockComponentEmptySlot

    tagGetBlockComponentWrongHash :: EventPred m
    tagGetBlockComponentWrongHash = successful $ \ev r -> case r of
      ErAllComponents (Left (WrongHash {})) | GetBlockComponent {} <- unAt $ eventCmdNoErr ev ->
        Left TagGetBlockComponentWrongHash
      _ -> Right tagGetBlockComponentWrongHash

    tagGetBlockComponentNewerThanTip :: EventPred m
    tagGetBlockComponentNewerThanTip = successful $ \ev r -> case r of
      ErAllComponents (Left (NewerThanTip {})) | GetBlockComponent {} <- unAt $ eventCmdNoErr ev ->
        Left TagGetBlockComponentNewerThanTip
      _ -> Right tagGetBlockComponentNewerThanTip

    tagAppendBlockNotNewerThanTipError :: EventPred m
    tagAppendBlockNotNewerThanTipError = failedApiMisuse $ \_ e -> case e of
      AppendBlockNotNewerThanTipError {} -> Left TagAppendBlockNotNewerThanTipError
      _                                  -> Right tagAppendBlockNotNewerThanTipError

    tagInvalidIteratorRangeError :: EventPred m
    tagInvalidIteratorRangeError = failedApiMisuse $ \_ e -> case e of
      InvalidIteratorRangeError {} -> Left TagInvalidIteratorRangeError
      _                            -> Right tagInvalidIteratorRangeError

    tagIteratorStreamedN :: Map IteratorId Int
                         -> EventPred m
    tagIteratorStreamedN streamedPerIterator = C.Predicate
      { C.predApply = \ev -> case eventMockResp ev of
          Resp (Right (IterResult (IteratorResult {})))
            | IteratorNext it <- eventMockCmdNoErr ev
            -> Right $ tagIteratorStreamedN $
               Map.insertWith (+) it 1 streamedPerIterator
          _ -> Right $ tagIteratorStreamedN streamedPerIterator
      , C.predFinish = do
          -- Find the entry with the highest value, i.e. the iterator that has
          (_, longestStream) <- listToMaybe $ sortBy (flip compare `on` snd) $
            Map.toList streamedPerIterator
          return $ TagIteratorStreamedN longestStream
      }

    tagCorruption :: EventPred m
    tagCorruption = C.Predicate
      { C.predApply = \ev -> case eventCmdNoErr ev of
          At (Corruption {}) -> Left  TagCorruption
          _                  -> Right tagCorruption
      , C.predFinish = Nothing
      }

    tagMigrate :: EventPred m
    tagMigrate = C.Predicate
      { C.predApply = \ev -> case eventCmdNoErr ev of
          At (Migrate {}) -> Left  TagMigrate
          _               -> Right tagMigrate
      , C.predFinish = Nothing
      }

    tagErrorDuring :: Tag -> (At Cmd m Symbolic -> Bool) -> EventPred m
    tagErrorDuring t isErr = simulatedError $ \ev ->
      if isErr (eventCmdNoErr ev) then Left t else Right $ tagErrorDuring t isErr


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
  cmdName (At (CmdErr { cmd }) ) = constrName cmd
  cmdNames (_ :: Proxy (At CmdErr m r)) =
    constrNames (Proxy @(Cmd (IterRef m r)))

instance ToExpr EpochNo
instance ToExpr EpochSize
instance ToExpr ChunkSize
instance ToExpr ChunkNo
instance ToExpr ChunkSlot
instance ToExpr RelativeSlot
instance (ToExpr a, ToExpr b, ToExpr c, ToExpr d, ToExpr e, ToExpr f, ToExpr g,
          ToExpr h, ToExpr i, ToExpr j)
      => ToExpr (a, b, c, d, e, f, g, h, i, j) where
    toExpr (a, b, c, d, e, f, g, h, i, j) = App "_×_×_×_×_×_×_×_×_x_"
      [ toExpr a, toExpr b, toExpr c, toExpr d, toExpr e, toExpr f, toExpr g
      , toExpr h, toExpr i, toExpr j
      ]
instance ToExpr (IteratorModel TestBlock)
instance ToExpr EBB
instance ToExpr IsEBB
instance ToExpr ChainLength
instance ToExpr TestHeaderHash
instance ToExpr TestBodyHash
instance ToExpr (ChainHash TestHeader)
instance ToExpr TestHeader
instance ToExpr TestBody
instance ToExpr TestBlock
instance ToExpr (Tip TestBlock)
instance ToExpr (InSlot TestBlock)
instance ToExpr (CodecConfig TestBlock)
instance ToExpr (DBModel TestBlock)

instance ToExpr FsError where
  toExpr fsError = App (show fsError) []

instance ToExpr ChunkInfo where
  toExpr = defaultExprViaShow

instance ToExpr (Model m Concrete)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

-- | Show minimal examples for each of the generated tags
showLabelledExamples'
  :: Maybe Int
  -- ^ Seed
  -> Int
  -- ^ Number of tests to run to find examples
  -> (Tag -> Bool)
  -- ^ Tag filter (can be @const True@)
  -> ChunkInfo
  -> IO ()
showLabelledExamples' mbReplay numTests focus chunkInfo = do
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
    smUnused = sm unusedEnv $ initDBModel chunkInfo TestBlockCodecConfig

showLabelledExamples :: ChunkInfo -> IO ()
showLabelledExamples = showLabelledExamples' Nothing 1000 (const True)

prop_sequential :: Index.CacheConfig -> SmallChunkInfo -> Property
prop_sequential cacheConfig (SmallChunkInfo chunkInfo) =
    forAllCommands smUnused Nothing $ \cmds -> QC.monadicIO $ do
      (hist, prop) <- QC.run $ test cacheConfig chunkInfo cmds
      prettyCommands smUnused hist
        $ tabulate "Tags" (map show $ tag (execCmds (QSM.initModel smUnused) cmds))
        $ prop
  where
    smUnused = sm unusedEnv $ initDBModel chunkInfo TestBlockCodecConfig

test :: Index.CacheConfig
     -> ChunkInfo
     -> QSM.Commands (At CmdErr IO) (At Resp IO)
     -> IO (QSM.History (At CmdErr IO) (At Resp IO), Property)
test cacheConfig chunkInfo cmds = do
    fsVar              <- uncheckedNewTVarM Mock.empty
    varErrors          <- uncheckedNewTVarM mempty
    varNextId          <- uncheckedNewTVarM 0
    varIters           <- uncheckedNewTVarM []
    (tracer, getTrace) <- recordingTracerIORef

    withRegistry $ \registry -> do
      let hasFS = mkSimErrorHasFS fsVar varErrors
          args  = ImmutableDbArgs {
              immCacheConfig      = cacheConfig
            , immCheckIntegrity   = testBlockIsValid
            , immChunkInfo        = chunkInfo
            , immCodecConfig      = TestBlockCodecConfig
            , immHasFS            = SomeHasFS hasFS
            , immRegistry         = registry
            , immTracer           = tracer
            , immValidationPolicy = ValidateMostRecentChunk
            }

      (hist, model, res, trace) <- bracket
        (open args >>= newMVar)
        -- Note: we might be closing a different ImmutableDB than the one we
        -- opened, as we can reopen it the ImmutableDB, swapping the
        -- ImmutableDB in the MVar.
        (\varDB -> readMVar varDB >>= closeDB . db)
        $ \varDB -> do
          let env = ImmutableDBEnv
                { varErrors
                , varNextId
                , varIters
                , varDB
                , args
                }
              sm' = sm env (initDBModel chunkInfo TestBlockCodecConfig)

          (hist, model, res) <- QSM.runCommands' sm' cmds

          trace <- getTrace
          return (hist, model, res, trace)

      fs <- atomically $ readTVar fsVar

      let modelTip = dbmTip $ dbModel model
          prop =
            counterexample ("Trace: " <> unlines (map show trace)) $
            counterexample ("FS: " <> Mock.pretty fs)              $
            counterexample ("modelTip: " <> show modelTip)         $
            res === Ok .&&. openHandlesProp fs model

      return (hist, prop)
  where
    openHandlesProp fs model
        | openHandles <= maxExpectedOpenHandles
        = property True
        | otherwise
        = counterexample
          ("open handles: " <> show openHandles <>
           " > max expected open handles: " <>
           show maxExpectedOpenHandles) False
      where
        openHandles = Mock.numOpenHandles fs
        -- We're appending to the epoch, so a handle for each of the
        -- three files, plus a handle for the epoch file (to read the
        -- blocks) per open iterator.
        maxExpectedOpenHandles = 1 {- epoch file -}
                               + 1 {- primary index file -}
                               + 1 {- secondary index file -}
                               + nbOpenIterators model

tests :: TestTree
tests = testGroup "ImmutableDB q-s-m"
    [ testProperty "sequential" prop_sequential
    ]

unusedEnv :: ImmutableDBEnv
unusedEnv = error "ImmutableDBEnv used during command generation"

instance Arbitrary Index.CacheConfig where
  arbitrary = do
    pastChunksToCache <- frequency
      -- Pick small values so that we exercise cache eviction
      [ (1, return 1)
      , (1, return 2)
      , (1, choose (3, 10))
      ]
    -- TODO create a Cmd that advances time, so this is being exercised too.
    expireUnusedAfter <- (fromIntegral :: Int -> DiffTime) <$> choose (1, 100)
    return Index.CacheConfig {..}
