{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
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
import           Control.Monad (forM_, void, when)
import           Data.Bifunctor (first)
import           Data.ByteString.Lazy (ByteString)
import           Data.Coerce (Coercible, coerce)
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.Functor.Classes (Eq1, Show1)
import           Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, isNothing, listToMaybe)
import           Data.Proxy (Proxy (..))
import           Data.TreeDiff (Expr (App))
import           Data.TreeDiff.Class (ToExpr (..), defaultExprViaShow)
import           Data.Typeable (Typeable)
import           Data.Word (Word16, Word32, Word64)
import qualified Generics.SOP as SOP
import           GHC.Generics (Generic, Generic1)
import           GHC.Stack (HasCallStack)
import           System.Random (getStdRandom, randomR)
import           Text.Show.Pretty (ppShow)

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Random (mkQCGen)
import           Test.StateMachine
import qualified Test.StateMachine.Sequential as QSM
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Block (IsEBB (..), fromIsEBB, getHeader)
import           Ouroboros.Consensus.BlockchainTime.Mock
                     (settableBlockchainTime)
import qualified Ouroboros.Consensus.Util.Classify as C
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Network.Block (BlockNo (..), HasHeader (..),
                     HeaderHash, SlotNo (..))
import qualified Ouroboros.Network.Block as Block

import           Ouroboros.Consensus.Storage.Common hiding (Tip (..))
import qualified Ouroboros.Consensus.Storage.Common as C
import           Ouroboros.Consensus.Storage.FS.API (HasFS (..))
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError (..), FsPath)
import           Ouroboros.Consensus.Storage.ImmutableDB hiding
                     (BlockOrEBB (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl as ImmDB
                     (Internal (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
                     (CacheConfig (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util (renderFile,
                     tryImmDB)
import           Ouroboros.Consensus.Storage.ImmutableDB.Layout
import           Ouroboros.Consensus.Storage.ImmutableDB.Parser
                     (epochFileParser)
import qualified Ouroboros.Consensus.Storage.Util.ErrorHandling as EH

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
  = GetBlockComponent      SlotNo
  | GetEBBComponent        EpochNo
  | GetBlockOrEBBComponent SlotNo  Hash
  | AppendBlock            SlotNo  Hash TestBlock
  | AppendEBB              EpochNo Hash TestBlock
  | Stream                 (Maybe (SlotNo, Hash)) (Maybe (SlotNo, Hash))
  | IteratorNext           it
  | IteratorPeek           it
  | IteratorHasNext        it
  | IteratorClose          it
  | Reopen                 ValidationPolicy
  | ReopenInThePast        ValidationPolicy SlotNo
    -- ^ New current slot, will be in the past
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
  = Unit            ()
  | MbAllComponents (Maybe AllComponents)
  | EpochNo         EpochNo
  | Iter            (Either (WrongBoundError Hash) it)
  | IterResult      (IteratorResult AllComponents)
  | IterHasNext     (Maybe (Either EpochNo SlotNo, Hash))
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

-- | How to run a 'Corruption' command.
type RunCorruption m =
     ImmutableDB    Hash m
  -> ImmDB.Internal Hash m
  -> Corruption
  -> m (Success (TestIterator m))

-- | Run the command against the given database.
run :: HasCallStack
    => ImmutableDBEnv h
    -> RunCorruption IO
    -> [TestIterator IO]
    -> Cmd (TestIterator IO)
    -> IO (Success (TestIterator IO))
run ImmutableDBEnv { db, internal, registry, varNextId, varCurSlot } runCorruption its cmd = case cmd of
    GetBlockComponent      s   -> MbAllComponents <$> getBlockComponent      db allComponents s
    GetEBBComponent        e   -> MbAllComponents <$> getEBBComponent        db allComponents e
    GetBlockOrEBBComponent s h -> MbAllComponents <$> getBlockOrEBBComponent db allComponents s h
    AppendBlock         s h b  -> Unit            <$> appendBlock            db s (blockNo b) h (toBuilder <$> testBlockToBinaryInfo b)
    AppendEBB           e h b  -> Unit            <$> appendEBB              db e (blockNo b) h (toBuilder <$> testBlockToBinaryInfo b)
    Stream              s e    -> iter            =<< stream                 db registry allComponents s e
    IteratorNext        it     -> IterResult      <$> iteratorNext           (unWithEq it)
    IteratorPeek        it     -> IterResult      <$> iteratorPeek           (unWithEq it)
    IteratorHasNext     it     -> IterHasNext     <$> iteratorHasNext        (unWithEq it)
    IteratorClose       it     -> Unit            <$> iteratorClose          (unWithEq it)
    DeleteAfter tip            -> do
      mapM_ iteratorClose (unWithEq <$> its)
      Unit <$> deleteAfter internal tip
    Reopen valPol              -> do
      mapM_ iteratorClose (unWithEq <$> its)
      closeDB db
      reopen db valPol
      Tip <$> getTip db
    ReopenInThePast valPol curSlot -> do
      -- The @curSlot@ will be in the past, so there /will/ be truncation
      mapM_ iteratorClose (unWithEq <$> its)
      closeDB db
      -- We only change the current slot here, we leave it set to 'maxBound'
      -- in all other places so that we don't accidentally truncate blocks
      -- from the \"future\" when reopening.
      bracket_
        (atomically $ writeTVar varCurSlot curSlot)
        (atomically $ writeTVar varCurSlot maxBound)
        (reopen db valPol)
      Tip <$> getTip db
    Corruption corr            -> do
      mapM_ iteratorClose (unWithEq <$> its)
      runCorruption db internal corr
  where
    iter = fmap Iter . traverse giveWithEq

    giveWithEq :: a -> IO (WithEq a)
    giveWithEq a =
      fmap (`WithEq` a) $ atomically $ updateTVar varNextId $ \i -> (succ i, i)


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
    IteratorNext        it     -> ok IterResult      $ update   (iteratorNextModel it allComponents)
    IteratorPeek        it     -> ok IterResult      $ query    (iteratorPeekModel it allComponents)
    IteratorHasNext     it     -> ok IterHasNext     $ query    (iteratorHasNextModel it)
    IteratorClose       it     -> ok Unit            $ update_  (iteratorCloseModel it)
    DeleteAfter tip            -> ok Unit            $ update_  (deleteAfterModel tip)
    Corruption corr            -> ok Tip             $ update   (simulateCorruptions (getCorruptions corr))
    Reopen _                   -> ok Tip             $ update    reopenModel
    ReopenInThePast _ s        -> ok Tip             $ update   (reopenInThePastModel s)
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
runPureErr dbm (CmdErr mbErrors cmd _its) =
    case (mbErrors, runPure cmd dbm) of
      -- No simulated errors, just step
      (Nothing, (resp, dbm')) -> (resp, dbm')
      -- An error will be simulated and thrown (not here, but in the real
      -- implementation). To mimic what the implementation will do, we only
      -- have to close the iterators, as the truncation during the reopening
      -- of the database will erase any changes.
      (Just _, (_resp, dbm')) ->
        -- We ignore the updated @dbm'@, because we have to roll back to the
        -- state before executing cmd. Exception: DeleteAfter and
        -- ReopenInThePast cmd, in which case we have may have to truncate the
        -- tip.
        --
        -- As the implementation closes all iterators, we do the same.
        let dbm'' = closeAllIterators $ case cmd of
              DeleteAfter {}     -> dbm'
              ReopenInThePast {} -> dbm'
              _                  -> dbm
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
-- command itself, and a mocked version of the response.
data Event m r = Event
  { eventBefore   :: Model     m r
  , eventCmdErr   :: At CmdErr m r
  , eventAfter    :: Model     m r
  , eventMockResp :: Resp IteratorId
  } deriving (Show)

eventCmd :: Event m r -> At Cmd m r
eventCmd = At . _cmd . unAt . eventCmdErr

eventMockCmd :: Eq1 r => Event m r -> Cmd IteratorId
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
    errorFor Corruption {}      = False
    errorFor ReopenInThePast {} = False  -- TODO #1567
    errorFor _                  = True

-- | Generate a 'Cmd'.
generateCmd :: Model m Symbolic -> Gen (At Cmd m Symbolic)
generateCmd Model {..} = At <$> frequency
    [ -- Block
      (1, GetBlockComponent <$> genGetBlockSlot)
      -- EBB
    , (1, GetEBBComponent <$> genGetEBB)
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
                   , (4, return $ IteratorPeek    iter)
                   , (4, return $ IteratorHasNext iter)
                   , (1, return $ IteratorClose   iter) ])
    , (1, Reopen <$> genValPol)

    , (1, ReopenInThePast <$> genValPol <*> chooseSlot (0, lastSlot))

    , (4, DeleteAfter <$> genTip)

      -- Only if there are files on disk can we generate commands that corrupt
      -- them.
    , (if null dbFiles then 0 else 2, Corruption <$> genCorruption)
    ]
  where
    DBModel {..} = dbModel

    currentEpoch = dbmCurrentEpoch dbModel

    lastSlot :: SlotNo
    lastSlot = fromIntegral $ length (dbmRegular dbModel)

    -- Useful when adding to another 'SlotNo'
    epochSize' :: SlotNo
    epochSize' = SlotNo $ unEpochSize fixedEpochSize

    empty = dbmTip dbModel == C.TipGen

    noBlocks = all isNothing (dbmRegular dbModel)

    noEBBs = Map.null (dbmEBBs dbModel)

    genGetBlockSlot :: Gen SlotNo
    genGetBlockSlot = frequency
      [ (if empty then 0 else 10, genSlotInThePast)
      , (1,  genSlotInTheFuture)
      , (1,  genSmallSlotNo) ]

    genGetEBB :: Gen EpochNo
    genGetEBB = frequency
      [ (if noEBBs then 0 else 5, elements $ Map.keys (dbmEBBs dbModel))
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

    genValPol = elements [ValidateMostRecentEpoch, ValidateAllEpochs]

    genTip :: Gen (ImmTipWithInfo Hash)
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
    AppendBlock _slot  _hash _b        -> []
    AppendEBB   _epoch _hash _ebb      -> []
    Stream  _mbStart _mbEnd            -> []
    GetBlockComponent slot             ->
      [GetBlockComponent slot' | slot' <- shrink slot]
    GetEBBComponent epoch              ->
      [GetEBBComponent epoch' | epoch' <- shrink epoch]
    GetBlockOrEBBComponent _slot _hash -> []
    IteratorNext    {}                 -> []
    IteratorPeek    {}                 -> []
    IteratorHasNext {}                 -> []
    IteratorClose   {}                 -> []
    DeleteAfter tip                    ->
      [DeleteAfter tip' | tip' <- shrinkTip tip]
    Reopen {}                          -> []
    ReopenInThePast {}                 -> []
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
precondition Model {..} (At (CmdErr { _cmd = cmd })) =
   forall (iters cmd) (`elem` RE.keys knownIters) .&&
    case cmd of
      AppendBlock    _ _ b -> fitsOnTip b
      AppendEBB      _ _ b -> fitsOnTip b
      DeleteAfter tip      -> tip `elem` NE.toList (tips dbModel)
      Corruption corr ->
        forall
          (corruptionFiles (getCorruptions corr))
          (`elem` getDBFiles dbModel)
      ReopenInThePast _ curSlot ->
        0 .<= curSlot .&& curSlot .<= lastSlot
      _ -> Top
  where
    fitsOnTip :: TestBlock -> Logic
    fitsOnTip b = case dbmTipBlock dbModel of
      Nothing    -> blockPrevHash b .== Block.GenesisHash
      Just bPrev -> blockPrevHash b .== Block.BlockHash (blockHash bPrev)

    lastSlot :: SlotNo
    lastSlot = fromIntegral $ length $ dbmRegular dbModel

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

-- | Environment to run commands against the real ImmutableDB implementation.
data ImmutableDBEnv h = ImmutableDBEnv
  { varErrors  :: StrictTVar IO Errors
  , varNextId  :: StrictTVar IO Id
  , varCurSlot :: StrictTVar IO SlotNo
    -- ^ Always 'maxBound'. Only when executing 'ReopenInThePast', it will
    -- temporarily be reset to a slot in the past.
  , registry   :: ResourceRegistry IO
  , hasFS      :: HasFS IO h
  , db         :: ImmutableDB    Hash IO
  , internal   :: ImmDB.Internal Hash IO
  }

semantics :: ImmutableDBEnv h
          -> At CmdErr IO Concrete
          -> IO (At Resp IO Concrete)
semantics env@ImmutableDBEnv {..} (At cmdErr) =
    At . fmap (reference . Opaque) . Resp <$> case opaque <$> cmdErr of

      CmdErr Nothing       cmd its -> tryDB $
        run env (semanticsCorruption hasFS) its cmd

      CmdErr (Just errors) cmd its -> do
        tipBefore <- getTip db
        res       <- withErrors varErrors errors $ tryDB $
          run env (semanticsCorruption hasFS) its cmd
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
    tryDB = tryImmDB EH.monadCatch EH.monadCatch

    truncateAndReopen cmd its tipBefore = tryDB $ do
      -- Close all open iterators as we will perform truncation
      mapM_ iteratorClose (unWithEq <$> its)
      -- Close the database in case no errors occurred and it wasn't
      -- closed already. This is idempotent anyway.
      closeDB db
      -- Release any handles that weren't closed because of a simulated error.
      releaseAll registry
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
                    -> m (Success (TestIterator m))
semanticsCorruption hasFS db _internal (MkCorruption corrs) = do
    closeDB db
    forM_ corrs $ \(corr, file) -> corruptFile hasFS corr file
    reopen db ValidateAllEpochs
    Tip <$> getTip db

-- | The state machine proper
sm :: ImmutableDBEnv h
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
  , distribution  = Nothing
  }

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

validate :: forall m. IOLike m
         => Model m Concrete -> ImmutableDB Hash m
         -> QC.PropertyM m Property
validate Model {..} realDB = do
    dbContents <- QC.run $ getDBContents realDB
    -- This message is clearer than the one produced by (===)
    let msg = "Mismatch between database (" <> show dbContents <>
              ") and model (" <> show modelContents <> ")"
    return $ counterexample msg (dbContents == modelContents)
  where
    modelContents = dbmBlockList dbModel

    getDBContents db = withRegistry $ \registry ->
      stream db registry GetRawBlock Nothing Nothing >>= \case
        -- This should never happen
        Left e   -> error (show e)
        Right it -> iteratorToList it

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

  | TagReopenInThePast

  | TagErrorDuringAppendBlock

  | TagErrorDuringAppendEBB

  | TagErrorDuringGetBlockComponent

  | TagErrorDuringGetEBBComponent

  | TagErrorDuringGetBlockOrEBBComponent

  | TagErrorDuringStream

  | TagErrorDuringIteratorNext

  | TagErrorDuringIteratorPeek

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
    , tagReopenInThePast
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
    , tagErrorDuring TagErrorDuringIteratorPeek $ \case
      { At (IteratorPeek {}) -> True; _ -> False }
    , tagErrorDuring TagErrorDuringIteratorClose $ \case
       { At (IteratorClose {}) -> True; _ -> False }
    ]
  where
    tagGetBlockComponentJust :: EventPred m
    tagGetBlockComponentJust = successful $ \ev r -> case r of
      MbAllComponents (Just _) | GetBlockComponent {} <- unAt $ eventCmd ev ->
        Left TagGetBlockComponentJust
      _ -> Right tagGetBlockComponentJust

    tagGetBlockComponentNothing :: EventPred m
    tagGetBlockComponentNothing = successful $ \ev r -> case r of
      MbAllComponents Nothing | GetBlockComponent {} <- unAt $ eventCmd ev ->
        Left TagGetBlockComponentNothing
      _ -> Right tagGetBlockComponentNothing

    tagGetEBBComponentJust :: EventPred m
    tagGetEBBComponentJust = successful $ \ev r -> case r of
      MbAllComponents (Just _) | GetEBBComponent {} <- unAt $ eventCmd ev ->
        Left TagGetEBBComponentJust
      _ -> Right tagGetEBBComponentJust

    tagGetEBBComponentNothing :: EventPred m
    tagGetEBBComponentNothing = successful $ \ev r -> case r of
      MbAllComponents Nothing | GetEBBComponent {} <- unAt $ eventCmd ev ->
        Left TagGetEBBComponentNothing
      _ -> Right tagGetEBBComponentNothing

    tagGetBlockOrEBBComponentJust :: EventPred m
    tagGetBlockOrEBBComponentJust = successful $ \ev r -> case r of
      MbAllComponents (Just _) | GetBlockOrEBBComponent {} <- unAt $ eventCmd ev ->
        Left TagGetBlockOrEBBComponentJust
      _ -> Right tagGetBlockOrEBBComponentJust

    tagGetBlockOrEBBComponentNothing :: EventPred m
    tagGetBlockOrEBBComponentNothing = successful $ \ev r -> case r of
      MbAllComponents Nothing | GetBlockOrEBBComponent {} <- unAt $ eventCmd ev ->
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
      At (Stream Nothing Nothing) -> Left TagIteratorWithoutBounds
      _                           -> Right tagIteratorWithoutBounds

    tagCorruption :: EventPred m
    tagCorruption = C.Predicate
      { C.predApply = \ev -> case eventCmd ev of
          At (Corruption {}) -> Left  TagCorruption
          _                  -> Right tagCorruption
      , C.predFinish = Nothing
      }

    tagReopenInThePast :: EventPred m
    tagReopenInThePast = C.Predicate
      { C.predApply = \ev -> case eventCmd ev of
          At (ReopenInThePast {}) -> Left  TagReopenInThePast
          _                       -> Right tagReopenInThePast
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

instance ToExpr SlotNo where
  toExpr (SlotNo w) = App "SlotNo" [toExpr w]

instance ToExpr EpochNo
instance ToExpr EpochSize
instance ToExpr EpochSlot
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
instance ToExpr r => ToExpr (C.Tip r)
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
--
showLabelledExamples'
  :: Maybe Int
  -- ^ Seed
  -> Int
  -- ^ Number of tests to run to find examples
  -> (Tag -> Bool)
  -- ^ Tag filter (can be @const True@)
  -> (   DBModel Hash
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
    smUnused = stateMachine mkDBModel

showLabelledExamples :: IO ()
showLabelledExamples = showLabelledExamples' Nothing 1000 (const True) $
    sm unusedEnv

prop_sequential :: Index.CacheConfig -> Property
prop_sequential cacheConfig = forAllCommands smUnused Nothing $ \cmds -> QC.monadicIO $ do
    (hist, prop) <- test cacheConfig cmds
    prettyCommands smUnused hist
      $ tabulate "Tags" (map show $ tag (execCmds (QSM.initModel smUnused) cmds))
      $ prop
  where
    dbm = mkDBModel
    smUnused = sm unusedEnv dbm

test :: Index.CacheConfig
     -> QSM.Commands (At CmdErr IO) (At Resp IO)
     -> QC.PropertyM IO (QSM.History (At CmdErr IO) (At Resp IO), Property)
test cacheConfig cmds = do
    fsVar              <- QC.run $ uncheckedNewTVarM Mock.empty
    varErrors          <- QC.run $ uncheckedNewTVarM mempty
    varNextId          <- QC.run $ uncheckedNewTVarM 0
    varCurSlot         <- QC.run $ uncheckedNewTVarM maxBound
    (tracer, getTrace) <- QC.run $ recordingTracerIORef
    registry           <- QC.run $ unsafeNewRegistry

    let hasFS  = mkSimErrorHasFS EH.monadCatch fsVar varErrors
        parser = epochFileParser hasFS (const <$> decode) isEBB
          getBinaryInfo testBlockIsValid
        btime  = settableBlockchainTime varCurSlot

    (db, internal) <- QC.run $ openDBInternal registry hasFS
      EH.monadCatch (simpleChunkInfo $ unEpochSize fixedEpochSize) testHashInfo
      ValidateMostRecentEpoch parser tracer cacheConfig btime

    let env = ImmutableDBEnv
          { varErrors, varNextId, varCurSlot, registry, hasFS, db, internal }
        sm' = sm env dbm

    (hist, model, res) <- runCommands sm' cmds

    trace <- QC.run $ getTrace
    fs    <- QC.run $ atomically $ readTVar fsVar

    QC.monitor $ counterexample ("Trace: " <> unlines (map show trace))
    QC.monitor $ counterexample ("FS: " <> Mock.pretty fs)

    let prop = res === Ok .&&. openHandlesProp fs model

    case res of
      Ok -> do
        QC.run $ closeDB db >> reopen db ValidateAllEpochs
        validation <- validate model db
        dbTip <- QC.run $ getTip db <* closeDB db <* closeRegistry registry

        let modelTip = dbmTip $ dbModel model
        QC.monitor $ counterexample ("dbTip:    " <> show dbTip)
        QC.monitor $ counterexample ("modelTip: " <> show modelTip)

        return (hist, prop .&&. dbTip === modelTip .&&. validation)
      -- If something went wrong, don't try to reopen the database
      _ -> do
        QC.run $ closeRegistry registry
        return (hist, prop)
  where
    dbm = mkDBModel
    isEBB = testHeaderEpochNoIfEBB fixedEpochSize . getHeader
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

fixedEpochSize :: EpochSize
fixedEpochSize = 10

mkDBModel :: DBModel Hash
mkDBModel = initDBModel fixedEpochSize

unusedEnv :: ImmutableDBEnv h
unusedEnv = error "ImmutableDBEnv used during command generation"

instance Arbitrary Index.CacheConfig where
  arbitrary = do
    pastEpochsToCache <- frequency
      -- Pick small values so that we exercise cache eviction
      [ (1, return 1)
      , (1, return 2)
      , (1, choose (3, 10))
      ]
    -- TODO create a Cmd that advances time, so this is being exercised too.
    expireUnusedAfter <- (fromIntegral :: Int -> DiffTime) <$> choose (1, 100)
    return Index.CacheConfig {..}
