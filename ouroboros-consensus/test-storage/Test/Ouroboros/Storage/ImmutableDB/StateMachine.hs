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
module Test.Ouroboros.Storage.ImmutableDB.StateMachine
    ( tests
    , showLabelledExamples
    ) where

import           Prelude hiding (elem, notElem)

import           Codec.CBOR.Write (toBuilder)
import           Codec.Serialise (decode)
import           Control.Monad (forM_, void)
import           Data.Bifunctor (first)
import           Data.ByteString.Lazy (ByteString)
import           Data.Coerce (Coercible, coerce)
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.Functor.Classes (Eq1, Show1)
import           Data.List (delete, sortBy)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, isNothing, listToMaybe)
import           Data.Proxy (Proxy (..))
import           Data.TreeDiff (Expr (App), defaultExprViaShow)
import           Data.TreeDiff.Class (ToExpr (..))
import           Data.Typeable (Typeable)
import           Data.Word (Word16, Word32, Word64)
import qualified Generics.SOP as SOP
import           GHC.Generics (Generic, Generic1)
import           GHC.Stack (HasCallStack)

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.StateMachine hiding (showLabelledExamples,
                     showLabelledExamples')
import           Test.StateMachine.Labelling
import qualified Test.StateMachine.Labelling as QSM
import qualified Test.StateMachine.Sequential as QSM
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Cardano.Prelude (AllowThunk (..), NoUnexpectedThunks)
import           Cardano.Slotting.Slot hiding (At)
import qualified Cardano.Slotting.Slot as S

import           Ouroboros.Consensus.Block (IsEBB (..), fromIsEBB, getHeader)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Network.Block (BlockNo (..), HasHeader (..),
                     HeaderHash, SlotNo (..))
import qualified Ouroboros.Network.Block as Block

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (HasFS (..))
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError (..), FsPath,
                     mkFsPath)
import           Ouroboros.Consensus.Storage.ImmutableDB hiding
                     (BlockOrEBB (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (unsafeChunkNoToEpochNo)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl as ImmDB
                     (Internal (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
                     (CacheConfig (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Consensus.Storage.ImmutableDB.Parser (ChunkFileError,
                     chunkFileParser)

import           Test.Util.ChunkInfo
import           Test.Util.FS.Sim.Error (Errors, mkSimErrorHasFS, withErrors)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.Orphans.Arbitrary (genSmallSlotNo)
import           Test.Util.RefEnv (RefEnv)
import qualified Test.Util.RefEnv as RE
import           Test.Util.SOP
import           Test.Util.Tracer (recordingTracerIORef)
import           Test.Util.WithEq

import           Test.Ouroboros.Storage.ImmutableDB.Model
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
data Cmd it
  = GetBlockComponent      SlotNo
  | GetEBBComponent        EpochNo
  | GetBlockOrEBBComponent SlotNo  Hash
  | AppendBlock            SlotNo  Hash TestBlock
  | AppendEBB              EpochNo Hash TestBlock
  | Stream                 (Maybe (SlotNo, Hash)) (Maybe (SlotNo, Hash))
  | StreamAll
  | IteratorNext           it
  | IteratorHasNext        it
  | IteratorClose          it
  | Reopen                 ValidationPolicy
  | Migrate                ValidationPolicy
  | DeleteAfter            (ImmTipWithInfo Hash)
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
data CmdErr it = CmdErr
  { cmdErr :: Maybe Errors
  , cmd    :: Cmd it
  } deriving (Show, Generic, Functor, Foldable, Traversable)

type Hash = TestHeaderHash

-- | Return type for successful database operations.
data Success it
  = Unit            ()
  | MbAllComponents (Maybe AllComponents)
  | EpochNo         EpochNo
  | Iter            (Either (WrongBoundError Hash) it)
  | IterResult      (IteratorResult AllComponents)
  | IterHasNext     (Maybe (Either EpochNo SlotNo, Hash))
  | IterResults     [AllComponents]
  | Tip             (ImmTipWithInfo Hash)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Product of all 'BlockComponent's. As this is a GADT, generating random
-- values of it (and combinations!) is not so simple. Therefore, we just
-- always request all block components.
allComponents :: BlockComponent (ImmutableDB Hash m) AllComponents
allComponents = (,,,,,,,,)
    <$> GetBlock
    <*> GetRawBlock
    <*> GetHeader
    <*> GetRawHeader
    <*> GetHash
    <*> GetSlot
    <*> GetIsEBB
    <*> GetBlockSize
    <*> GetHeaderSize

-- | A list of all the 'BlockComponent' indices (@b@) we are interested in.
type AllComponents =
  ( ()
  , ByteString
  , ()
  , ByteString
  , Hash
  , SlotNo
  , IsEBB
  , Word32
  , Word16
  )

-- | Short-hand
type TestIterator m = WithEq (Iterator Hash m AllComponents)

closeOpenIterators :: StrictTVar IO [TestIterator IO] -> IO ()
closeOpenIterators varIters = do
    its <- atomically $ readTVar varIters <* writeTVar varIters []
    mapM_ iteratorClose (unWithEq <$> its)

open
  :: Eq h
  => ImmutableDbArgs IO h Hash (ChunkFileError Hash)
  -> IO ImmutableDBState
open args = do
    (db, internal) <- openDBInternal args
    return ImmutableDBState { db, internal }

-- | Opens a new ImmutableDB and stores it in 'varDB'.
--
-- Does not close the current VolatileDB stored in 'varDB'.
reopen :: Eq h => ImmutableDBEnv h -> ValidationPolicy -> IO ()
reopen ImmutableDBEnv { varDB, args } valPol = do
    immutableDbState <- open args { valPol = valPol }
    void $ swapMVar varDB immutableDbState

-- | Run the command against the given database.
run :: (Eq h, HasCallStack)
    => ImmutableDBEnv h
    -> Cmd (TestIterator IO)
    -> IO (Success (TestIterator IO))
run env@ImmutableDBEnv { varDB, varNextId, varIters, args } cmd =
    readMVar varDB >>= \ImmutableDBState { db, internal } -> case cmd of
      GetBlockComponent      s   -> MbAllComponents <$> getBlockComponent      db allComponents s
      GetEBBComponent        e   -> MbAllComponents <$> getEBBComponent        db allComponents e
      GetBlockOrEBBComponent s h -> MbAllComponents <$> getBlockOrEBBComponent db allComponents s h
      AppendBlock         s h b  -> Unit            <$> appendBlock            db s (blockNo b) h (toBuilder <$> testBlockToBinaryInfo b)
      AppendEBB           e h b  -> Unit            <$> appendEBB              db e (blockNo b) h (toBuilder <$> testBlockToBinaryInfo b)
      Stream              s e    -> iter            =<< stream                 db registry allComponents s e
      StreamAll                  -> IterResults     <$> streamAll              db
      IteratorNext        it     -> IterResult      <$> iteratorNext           (unWithEq it)
      IteratorHasNext     it     -> IterHasNext     <$> iteratorHasNext        (unWithEq it)
      IteratorClose       it     -> Unit            <$> iteratorClose'         it
      DeleteAfter tip            -> do
        closeOpenIterators varIters
        Unit <$> deleteAfter internal tip
      Reopen valPol              -> do
        closeOpenIterators varIters
        closeDB db
        reopen env valPol
        Tip <$> getTip db
      Migrate valPol -> do
        closeOpenIterators varIters
        closeDB db
        unmigrate hasFS
        reopen env valPol
        Tip <$> getTip db
      Corruption (MkCorruption corrs) -> do
        closeOpenIterators varIters
        closeDB db
        forM_ corrs $ \(corr, file) -> corruptFile hasFS corr file
        reopen env ValidateAllChunks
        Tip <$> getTip db
  where
    ImmutableDbArgs { registry, hasFS } = args

    -- Store the iterator in 'varIters'
    iter :: Either (WrongBoundError Hash) (Iterator Hash IO AllComponents)
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
      fmap (`WithEq` a) $ atomically $ updateTVar varNextId $ \i -> (succ i, i)

    streamAll :: ImmutableDB Hash IO -> IO [AllComponents]
    streamAll db =
      bracket
        (noWrongBoundError <$> stream db registry allComponents Nothing Nothing)
        iteratorClose
        iteratorToList

    noWrongBoundError :: Either (WrongBoundError Hash) a -> a
    noWrongBoundError (Left e)  = error ("impossible: " <> show e)
    noWrongBoundError (Right a) = a

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
newtype Resp it = Resp { getResp :: Either ImmutableDBError (Success it) }
  deriving (Functor, Foldable, Traversable, Show)

-- | The 'Eq' instance for 'Resp' uses 'sameImmutableDBError'.
instance Eq it => Eq (Resp it) where
  Resp (Left  e) == Resp (Left  e') = sameImmutableDBError e e'
  Resp (Right a) == Resp (Right a') = a == a'
  _              == _               = False

-- | Run the pure command against the given database.
runPure :: Cmd IteratorId
        -> DBModel Hash
        -> (Resp IteratorId, DBModel Hash)
runPure = \case
    GetBlockComponent      s   -> ok MbAllComponents $ queryE   (getBlockComponentModel allComponents s)
    GetEBBComponent        e   -> ok MbAllComponents $ queryE   (getEBBComponentModel allComponents e)
    GetBlockOrEBBComponent s h -> ok MbAllComponents $ queryE   (getBlockOrEBBComponentModel allComponents s h)
    AppendBlock         s h b  -> ok Unit            $ updateE_ (appendBlockModel s (blockNo b) h (toBuilder <$> testBlockToBinaryInfo b))
    AppendEBB           e h b  -> ok Unit            $ updateE_ (appendEBBModel   e (blockNo b) h (toBuilder <$> testBlockToBinaryInfo b))
    Stream              s e    -> ok Iter            $ updateEE (streamModel s e)
    StreamAll                  -> ok IterResults     $ query    (streamAllModel allComponents)
    IteratorNext        it     -> ok IterResult      $ update   (iteratorNextModel it allComponents)
    IteratorHasNext     it     -> ok IterHasNext     $ query    (iteratorHasNextModel it)
    IteratorClose       it     -> ok Unit            $ update_  (iteratorCloseModel it)
    DeleteAfter tip            -> ok Unit            $ update_  (deleteAfterModel tip)
    Corruption corr            -> ok Tip             $ update   (simulateCorruptions (getCorruptions corr))
    Reopen _                   -> ok Tip             $ update    reopenModel
    Migrate _                  -> ok Tip             $ update    reopenModel
  where
    query  f m = (Right (f m), m)
    queryE f m = (f m, m)

    update   f m = first Right (f m)
    update_  f m = (Right (), f m)
    updateE_ f m = case f m of
      Left  e  -> (Left e, m)
      Right m' -> (Right (), m')
    updateEE :: (DBModel Hash -> Either ImmutableDBError (Either e (a, DBModel Hash)))
             -> DBModel Hash
             -> (Either ImmutableDBError (Either e a), DBModel Hash)
    updateEE f m = case f m of
      Left e                -> (Left e, m)
      Right (Left e)        -> (Right (Left e), m)
      Right (Right (a, m')) -> (Right (Right a), m')

    ok :: (a -> Success IteratorId)
       -> (DBModel Hash -> (Either ImmutableDBError a, DBModel Hash))
       -> DBModel Hash
       -> (Resp IteratorId, DBModel Hash)
    ok toSuccess f m = first (Resp . fmap toSuccess) $ f m

-- | Run a command against the pure model
runPureErr :: DBModel Hash
           -> CmdErr IteratorId
           -> (Resp IteratorId, DBModel Hash)
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
        in (Resp $ Right $ Tip $ dbmTip dbm'', dbm'')

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
  { dbModel    :: DBModel Hash
    -- ^ A model of the database, used as state for the 'HasImmutableDB'
    -- instance of 'ModelDB'.
  , knownIters :: KnownIters m r
    -- ^ Store a mapping between iterator references and mocked iterators.
  } deriving (Show, Generic)

nbOpenIterators :: Model m r -> Int
nbOpenIterators model = length (RE.toList (knownIters model))

-- | Initial model
initModel :: DBModel Hash -> Model m r
initModel dbModel = Model { knownIters  = RE.empty, dbModel }

-- | Key property of the model is that we can go from real to mock responses
toMock :: (Functor t, Eq1 r)
       => Model m r -> At t m r -> t IteratorId
toMock Model {..} (At t) = fmap (knownIters RE.!) t

-- | Step the mock semantics
step :: Eq1 r
     => Model m r
     -> At CmdErr m r
     -> (Resp IteratorId, DBModel Hash)
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
-- command itself, and the response.
type ImmDBEvent m = Event (Model m) (At CmdErr m) (At Resp m)

eventCmdErr :: ImmDBEvent m r -> At CmdErr m r
eventCmdErr = eventCmd

eventCmdNoErr :: ImmDBEvent m r -> At Cmd m r
eventCmdNoErr = At . cmd . unAt . eventCmdErr

eventMockCmd :: Eq1 r => ImmDBEvent m r -> Cmd IteratorId
eventMockCmd ev@Event {..} = toMock eventBefore (eventCmdNoErr ev)

eventMockResp :: Eq1 r => ImmDBEvent m r -> Resp IteratorId
eventMockResp Event {..} = toMock eventAfter eventResp

-- | Construct an event
lockstep :: (Show1 r, Eq1 r)
         => Model      m r
         -> At CmdErr  m r
         -> At Resp    m r
         -> ImmDBEvent m r
lockstep model@Model {..} cmdErr (At resp) = Event
    { eventBefore = model
    , eventCmd    = cmdErr
    , eventAfter  = model'
    , eventResp   = At resp
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
    cmd    <- unAt <$> generateCmd m
    cmdErr <- if errorFor cmd
       then frequency
          -- We want to make some progress
          [ (4, return Nothing)
          , (1, Just <$> arbitrary)
          ]
       else return Nothing
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
      (1, GetBlockComponent <$> genGetBlockSlot)
      -- EBB
    , (if modelSupportsEBBs then 1 else 0, GetEBBComponent <$> genGetEBB)
    , (1, uncurry GetBlockOrEBBComponent <$> genSlotAndHash)
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
              , (1, chooseSlot (inLaterChunk 1 lastSlot,
                                inLaterChunk 4 lastSlot))
              ]
            let block = (maybe (firstBlock  canContainEBB)
                               (mkNextBlock canContainEBB)
                               mbPrevBlock)
                          slotNo
                          (TestBody 0 True)
            return $ AppendBlock slotNo (blockHash block) block)
    , (if modelSupportsEBBs then 1 else 0, do
            (epoch, ebb) <- case dbmTipBlock dbModel of
              Nothing        -> return (0, firstEBB canContainEBB (TestBody 0 True))
              Just prevBlock -> do
                epoch <- frequency
                -- Epoch in the past -> invalid
                  [ (1, chooseEpoch (0, currentEpoch))
                  , (3, chooseEpoch (currentEpoch, currentEpoch + 5))
                  ]
                let slotNo = slotNoOfEBB dbmChunkInfo epoch
                return (epoch, mkNextEBB canContainEBB prevBlock slotNo (TestBody 0 True))
            return $ AppendEBB epoch (blockHash ebb) ebb)
    , (4, return StreamAll)
    , (4, frequency
            -- An iterator with a random and likely invalid range,
            [ (1, Stream
                    <$> (Just <$> genRandomBound)
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

                 return $ Stream start end)
            ])
      -- Only if there are iterators can we generate commands that manipulate
      -- them.
    , (if Map.null dbmIterators then 0 else 8, do
         iter <- elements $ RE.keys knownIters
         frequency [ (4, return $ IteratorNext    iter)
                   , (4, return $ IteratorHasNext iter)
                   , (1, return $ IteratorClose   iter) ])
    , (1, Reopen <$> genValPol)

    , (1, Migrate <$> genValPol)

    , (4, DeleteAfter <$> genTip)

      -- Only if there are files on disk can we generate commands that corrupt
      -- them.
    , (if null dbFiles then 0 else 2, Corruption <$> genCorruption)
    ]
  where
    DBModel {..} = dbModel
    modelSupportsEBBs = chunkInfoSupportsEBBs dbmChunkInfo
    currentEpoch      = unsafeChunkNoToEpochNo $ dbmCurrentChunk dbModel
    canContainEBB     = const modelSupportsEBBs -- TODO: we could be more precise

    lastSlot :: SlotNo
    lastSlot = fromIntegral $ length (dbmRegular dbModel)

    -- Construct a 'SlotNo' @n@ chunks later
    inLaterChunk :: Word -> SlotNo -> SlotNo
    inLaterChunk 0 s = s
    inLaterChunk n s = inLaterChunk (n - 1) $
                         SlotNo (unSlotNo s + numRegularBlocks size)
      where
        chunk = chunkIndexOfSlot dbmChunkInfo s
        size  = getChunkSize     dbmChunkInfo chunk

    empty = dbmTip dbModel == S.Origin

    noBlocks = all isNothing (dbmRegular dbModel)

    noEBBs = Map.null (dbmEBBs dbModel)

    genGetBlockSlot :: Gen SlotNo
    genGetBlockSlot = frequency
      [ (if empty then 0 else 10, genSlotInThePast)
      , (1,  genSlotInTheFuture)
      , (1,  genSmallSlotNo) ]

    genGetEBB :: Gen EpochNo
    genGetEBB = frequency
      [ (if noEBBs then 0 else 5,
           elements $ map unsafeChunkNoToEpochNo $ Map.keys (dbmEBBs dbModel))
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
      elements $ map (testBlockFromBinaryInfo . snd) $ catMaybes (dbmRegular dbModel)

    genEBBInThePast :: Gen TestBlock
    genEBBInThePast =
      elements $ map (testBlockFromBinaryInfo . snd) $ Map.elems (dbmEBBs dbModel)

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

    genValPol = elements [ValidateMostRecentChunk, ValidateAllChunks]

    genTip :: Gen (ImmTipWithInfo Hash)
    genTip = elements $ NE.toList $ tips dbModel

-- | Return the files that the database with the given model would have
-- created. For each epoch an epoch, primary index, and secondary index file.
getDBFiles :: DBModel Hash -> [FsPath]
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
shrinker m@Model {..} (At (CmdErr mbErrors cmd)) = fmap At $
    [CmdErr mbErrors' cmd  | mbErrors' <- shrink mbErrors] ++
    [CmdErr mbErrors  cmd' | At cmd'   <- shrinkCmd m (At cmd)]

-- | Shrink a 'Cmd'.
shrinkCmd :: Model m Symbolic -> At Cmd m Symbolic -> [At Cmd m Symbolic]
shrinkCmd Model {..} (At cmd) = fmap At $ case cmd of
    AppendBlock _slot  _hash _b        -> []
    AppendEBB   _epoch _hash _ebb      -> []
    Stream  _mbStart _mbEnd            -> []
    StreamAll                          -> []
    GetBlockComponent slot             ->
      [GetBlockComponent slot' | slot' <- shrink slot]
    GetEBBComponent epoch              ->
      [GetEBBComponent epoch' | epoch' <- shrink epoch]
    GetBlockOrEBBComponent _slot _hash -> []
    IteratorNext    {}                 -> []
    IteratorHasNext {}                 -> []
    IteratorClose   {}                 -> []
    DeleteAfter tip                    ->
      [DeleteAfter tip' | tip' <- shrinkTip tip]
    Reopen {}                          -> []
    Migrate {}                         -> []
    Corruption corr                    ->
      [Corruption corr' | corr' <- shrinkCorruption corr]
  where
    DBModel {..} = dbModel

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
    --
    -- TODO: Re-enable shrinker
    -- We could shrink the tip by asking for `tips` and selecting some.
    shrinkTip :: ImmTipWithInfo Hash -> [ImmTipWithInfo Hash]
    shrinkTip _ = []

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
      AppendBlock    _ _ b -> fitsOnTip b
      AppendEBB      _ _ b -> fitsOnTip b
      DeleteAfter tip      -> tip `member` NE.toList (tips dbModel)
      Corruption corr ->
        forall
          (corruptionFiles (getCorruptions corr))
          (`member` getDBFiles dbModel)
      _ -> Top
  where
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

data ImmutableDBState = ImmutableDBState
    { db       :: ImmutableDB    Hash IO
    , internal :: ImmDB.Internal Hash IO
    }
  deriving NoUnexpectedThunks via AllowThunk ImmutableDBState

-- | Environment to run commands against the real ImmutableDB implementation.
data ImmutableDBEnv h = ImmutableDBEnv
  { varErrors :: StrictTVar IO Errors
  , varNextId :: StrictTVar IO Id
  , varIters  :: StrictTVar IO [TestIterator IO]
    -- ^ A list of all open iterators. For some commands, e.g., corrupting the
    -- database or simulating errors, we need to close and reopen the
    -- database, which almost always requires truncation of the database.
    -- During truncation we might need to delete a file that is still opened
    -- by an iterator. As this is not allowed by the MockFS implementation, we
    -- first close all open iterators in these cases.
  , varDB     :: StrictMVar IO ImmutableDBState
  , args      :: ImmutableDbArgs IO h Hash (ChunkFileError Hash)
  }

getImmDB :: ImmutableDBEnv h -> IO (ImmutableDB Hash IO)
getImmDB = fmap db . readMVar . varDB

getInternal :: ImmutableDBEnv h -> IO (ImmDB.Internal Hash IO)
getInternal = fmap internal . readMVar . varDB

semantics :: Eq h
          => ImmutableDBEnv h
          -> At CmdErr IO Concrete
          -> IO (At Resp IO Concrete)
semantics env@ImmutableDBEnv {..} (At cmdErr) =
    At . fmap (reference . Opaque) . Resp <$> case opaque <$> cmdErr of

      CmdErr Nothing       cmd -> tryImmDB $ run env cmd

      CmdErr (Just errors) cmd -> do
        tipBefore <- getImmDB env >>= getTip
        res       <- withErrors varErrors errors $ tryImmDB $ run env cmd
        case res of
          -- If the command resulted in a 'UserError', we didn't even get the
          -- chance to run into a simulated error. Note that we still
          -- truncate, because we can't predict whether we'll get a
          -- 'UserError' or an 'UnexpectedError', as it depends on the
          -- simulated error.
          Left (UserError {})       ->
            truncateAndReopen cmd tipBefore

          -- We encountered a simulated error
          Left (UnexpectedError {}) ->
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
    ImmutableDbArgs { registry } = args

    truncateAndReopen cmd tipBefore = tryImmDB $ do
      -- Close all open iterators as we will perform truncation
      closeOpenIterators varIters
      -- Close the database in case no errors occurred and it wasn't
      -- closed already. This is idempotent anyway.
      getImmDB env >>= closeDB
      -- Release any handles that weren't closed because of a simulated error.
      releaseAll registry
      reopen env ValidateAllChunks
      getInternal env >>= flip deleteAfter tipBefore
      -- If the cmd deleted things, we must do it here to have a deterministic
      -- outcome and to stay in sync with the model. If no error was thrown,
      -- these things will have been deleted. If an error was thrown, they
      -- might not have been deleted or only part of them.
      case cmd of
        DeleteAfter tip -> getInternal env >>= flip deleteAfter tip
        _               -> return ()
      Tip <$> (getImmDB env >>= getTip)

-- | The state machine proper
sm :: Eq h
   => ImmutableDBEnv h
   -> DBModel Hash
   -> StateMachine (Model IO) (At CmdErr IO) IO (At Resp IO)
sm env dbm = StateMachine
  { initModel     = initModel dbm
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

data Tag
  = TagGetBlockComponentJust

  | TagGetBlockComponentNothing

  | TagGetEBBComponentJust

  | TagGetEBBComponentNothing

  | TagGetBlockOrEBBComponentJust

  | TagGetBlockOrEBBComponentNothing

  | TagAppendToSlotInThePastError

  | TagReadFutureSlotError

  | TagInvalidIteratorRangeError

  | TagIteratorStreamedN Int

  | TagIteratorWithoutBounds

  | TagCorruption

  | TagMigrate

  | TagErrorDuringAppendBlock

  | TagErrorDuringAppendEBB

  | TagErrorDuringGetBlockComponent

  | TagErrorDuringGetEBBComponent

  | TagErrorDuringGetBlockOrEBBComponent

  | TagErrorDuringStream

  | TagErrorDuringIteratorNext

  | TagErrorDuringIteratorClose

  deriving (Show, Eq)


-- | Predicate on events
type EventPred m = Predicate (ImmDBEvent m Symbolic) Tag

-- | Convenience combinator for creating classifiers for successful commands
successful :: (    ImmDBEvent m Symbolic
                -> Success IteratorId
                -> Either Tag (EventPred m)
              )
           -> EventPred m
successful f = predicate $ \ev -> case eventMockResp ev of
    Resp (Left  _ ) -> Right $ successful f
    Resp (Right ok) -> f ev ok

-- | Convenience combinator for creating classifiers for failed commands
failed :: (    ImmDBEvent m Symbolic
            -> ImmutableDBError
            -> Either Tag (EventPred m)
          )
       -> EventPred m
failed f = predicate $ \ev -> case eventMockResp ev of
    Resp (Left  e) -> f ev e
    Resp (Right _) -> Right $ failed f

-- | Convenience combinator for creating classifiers for commands failed with
-- a @UserError@.
failedUserError :: (    ImmDBEvent m Symbolic
                     -> UserError
                     -> Either Tag (EventPred m)
                   )
                -> EventPred m
failedUserError f = failed $ \ev e -> case e of
    UserError ue _ -> f ev ue
    _              -> Right $ failedUserError f

-- | Convenience combinator for creating classifiers for commands for which an
-- error is simulated.
simulatedError :: (ImmDBEvent m Symbolic -> Either Tag (EventPred m))
               -> EventPred m
simulatedError f = predicate $ \ev ->
    case (cmdErr (unAt (eventCmdErr ev)), getResp (eventMockResp ev)) of
      (Just _, Right _) -> f ev
      _                 -> Right $ simulatedError f


-- | Tag commands
--
-- Tagging works on symbolic events, so that we can tag without doing real IO.
tag :: forall m. [ImmDBEvent m Symbolic] -> [Tag]
tag = QSM.classify
    [ tagGetBlockComponentJust
    , tagGetBlockComponentNothing
    , tagGetEBBComponentJust
    , tagGetEBBComponentNothing
    , tagGetBlockOrEBBComponentJust
    , tagGetBlockOrEBBComponentNothing
    , tagAppendToSlotInThePastError
    , tagReadFutureSlotError
    , tagInvalidIteratorRangeError
    , tagIteratorStreamedN Map.empty
    , tagIteratorWithoutBounds
    , tagCorruption
    , tagMigrate
    , tagErrorDuring TagErrorDuringAppendBlock $ \case
      { At (AppendBlock {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringAppendEBB $ \case
      { At (AppendEBB {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringGetBlockComponent $ \case
      { At (GetBlockComponent {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringGetEBBComponent $ \case
      { At (GetEBBComponent {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringStream $ \case
      { At (Stream {}) -> True ; _ -> False }
    , tagErrorDuring TagErrorDuringIteratorNext $ \case
      { At (IteratorNext {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringIteratorClose $ \case
       { At (IteratorClose {}) -> True; _ -> False }
    ]
  where
    tagGetBlockComponentJust :: EventPred m
    tagGetBlockComponentJust = successful $ \ev r -> case r of
      MbAllComponents (Just _) | GetBlockComponent {} <- unAt $ eventCmdNoErr ev ->
        Left TagGetBlockComponentJust
      _ -> Right tagGetBlockComponentJust

    tagGetBlockComponentNothing :: EventPred m
    tagGetBlockComponentNothing = successful $ \ev r -> case r of
      MbAllComponents Nothing | GetBlockComponent {} <- unAt $ eventCmdNoErr ev ->
        Left TagGetBlockComponentNothing
      _ -> Right tagGetBlockComponentNothing

    tagGetEBBComponentJust :: EventPred m
    tagGetEBBComponentJust = successful $ \ev r -> case r of
      MbAllComponents (Just _) | GetEBBComponent {} <- unAt $ eventCmdNoErr ev ->
        Left TagGetEBBComponentJust
      _ -> Right tagGetEBBComponentJust

    tagGetEBBComponentNothing :: EventPred m
    tagGetEBBComponentNothing = successful $ \ev r -> case r of
      MbAllComponents Nothing | GetEBBComponent {} <- unAt $ eventCmdNoErr ev ->
        Left TagGetEBBComponentNothing
      _ -> Right tagGetEBBComponentNothing

    tagGetBlockOrEBBComponentJust :: EventPred m
    tagGetBlockOrEBBComponentJust = successful $ \ev r -> case r of
      MbAllComponents (Just _) | GetBlockOrEBBComponent {} <- unAt $ eventCmdNoErr ev ->
        Left TagGetBlockOrEBBComponentJust
      _ -> Right tagGetBlockOrEBBComponentJust

    tagGetBlockOrEBBComponentNothing :: EventPred m
    tagGetBlockOrEBBComponentNothing = successful $ \ev r -> case r of
      MbAllComponents Nothing | GetBlockOrEBBComponent {} <- unAt $ eventCmdNoErr ev ->
        Left TagGetBlockOrEBBComponentNothing
      _ -> Right tagGetBlockOrEBBComponentNothing

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

    tagIteratorStreamedN :: Map IteratorId Int
                         -> EventPred m
    tagIteratorStreamedN streamedPerIterator = Predicate
      { predApply = \ev -> case eventMockResp ev of
          Resp (Right (IterResult (IteratorResult {})))
            | IteratorNext it <- eventMockCmd ev
            -> Right $ tagIteratorStreamedN $
               Map.insertWith (+) it 1 streamedPerIterator
          _ -> Right $ tagIteratorStreamedN streamedPerIterator
      , predFinish = do
          -- Find the entry with the highest value, i.e. the iterator that has
          -- streamed the most blocks/headers
          (_, longestStream) <- listToMaybe $ sortBy (flip compare `on` snd) $
            Map.toList streamedPerIterator
          return $ TagIteratorStreamedN longestStream
      }

    tagIteratorWithoutBounds :: EventPred m
    tagIteratorWithoutBounds = successful $ \ev _ -> case eventCmdNoErr ev of
      At (Stream Nothing Nothing) -> Left TagIteratorWithoutBounds
      _                           -> Right tagIteratorWithoutBounds

    tagCorruption :: EventPred m
    tagCorruption = Predicate
      { predApply = \ev -> case eventCmdNoErr ev of
          At (Corruption {}) -> Left  TagCorruption
          _                  -> Right tagCorruption
      , predFinish = Nothing
      }

    tagMigrate :: EventPred m
    tagMigrate = Predicate
      { predApply = \ev -> case eventCmdNoErr ev of
          At (Migrate {}) -> Left  TagMigrate
          _               -> Right tagMigrate
      , predFinish = Nothing
      }

    tagErrorDuring :: Tag -> (At Cmd m Symbolic -> Bool) -> EventPred m
    tagErrorDuring t isErr = simulatedError $ \ev ->
      if isErr (eventCmdNoErr ev) then Left t else Right $ tagErrorDuring t isErr

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

instance ToExpr SlotNo where
  toExpr (SlotNo w) = App "SlotNo" [toExpr w]

instance ToExpr EpochNo
instance ToExpr EpochSize
instance ToExpr ChunkSize
instance ToExpr ChunkNo
instance ToExpr ChunkSlot
instance ToExpr RelativeSlot
instance ToExpr BlockNo
instance (ToExpr a, ToExpr b, ToExpr c, ToExpr d, ToExpr e, ToExpr f, ToExpr g,
          ToExpr h, ToExpr i)
      => ToExpr (a, b, c, d, e, f, g, h, i) where
    toExpr (a, b, c, d, e, f, g, h, i) = App "_×_×_×_×_×_×_×_×_"
      [ toExpr a, toExpr b, toExpr c, toExpr d, toExpr e, toExpr f, toExpr g
      , toExpr h, toExpr i
      ]
instance ToExpr (IteratorResult AllComponents)
instance ToExpr (IteratorModel Hash)
instance ToExpr (HeaderHash h) => ToExpr (Block.ChainHash h)
instance ToExpr IsEBB
instance ToExpr TestHeaderHash
instance ToExpr TestBodyHash
instance ToExpr TestHeader
instance ToExpr TestBody
instance ToExpr TestBlock
instance ToExpr ImmDB.BlockOrEBB
instance (ToExpr a, ToExpr hash) => ToExpr (ImmDB.TipInfo hash a)
instance ToExpr r => ToExpr (S.WithOrigin r)
instance ToExpr b => ToExpr (BinaryInfo b)
instance ToExpr hash => ToExpr (InSlot hash)
instance ToExpr (DBModel Hash)

instance ToExpr FsError where
  toExpr fsError = App (show fsError) []

instance ToExpr ChunkInfo where
  toExpr = defaultExprViaShow

instance ToExpr (Model m Concrete)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

-- | Show minimal examples for each of the generated tags
showLabelledExamples' :: Maybe Int
                      -- ^ Seed
                      -> Int
                      -- ^ Number of tests to run to find examples
                      -> (Tag -> Bool)
                      -- ^ Tag filter (can be @const True@)
                      -> ChunkInfo -> IO ()
showLabelledExamples' mReplay numTests focus chunkInfo =
    QSM.showLabelledExamples' smUnused mReplay numTests tag focus
  where
    smUnused = sm (unusedEnv @()) $ initDBModel chunkInfo

showLabelledExamples :: ChunkInfo -> IO ()
showLabelledExamples = showLabelledExamples' Nothing 1000 (const True)

prop_sequential :: Index.CacheConfig -> SmallChunkInfo -> Property
prop_sequential cacheConfig (SmallChunkInfo chunkInfo) =
    forAllCommands smUnused Nothing $ \cmds -> QC.monadicIO $ do
      (hist, prop) <- QC.run $ test cacheConfig chunkInfo cmds
      prettyCommands smUnused hist
        $ tabulate "Tags" (map show $ tag (execCmds smUnused cmds))
        $ prop
  where
    smUnused = sm (unusedEnv @()) $ initDBModel chunkInfo

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

    let hasFS  = mkSimErrorHasFS fsVar varErrors
        parser = chunkFileParser hasFS (const <$> decode) isEBB
          getBinaryInfo testBlockIsValid
    withRegistry $ \registry -> do
      let args = ImmutableDbArgs
            { registry
            , hasFS
            , chunkInfo
            , hashInfo = testHashInfo
            , tracer
            , cacheConfig
            , valPol   = ValidateMostRecentChunk
            , parser
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
              sm' = sm env (initDBModel chunkInfo)

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
    isEBB = testHeaderEpochNoIfEBB chunkInfo . getHeader
    getBinaryInfo = void . testBlockToBinaryInfo

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

unusedEnv :: ImmutableDBEnv h
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
