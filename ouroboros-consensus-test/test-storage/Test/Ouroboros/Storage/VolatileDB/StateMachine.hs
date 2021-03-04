{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.VolatileDB.StateMachine
    ( tests
    , showLabelledExamples
    ) where

import           Prelude hiding (elem)

import           Control.Monad (forM_, void)
import           Data.Bifunctor (first)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Classes
import           Data.Functor.Identity (Identity)
import           Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics
import           GHC.Stack
import qualified Generics.SOP as SOP
import           System.Random (getStdRandom, randomR)
import           Text.Show.Pretty (ppShow)

import           Ouroboros.Network.Block (MaxSlotNo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API (SomeHasFS (..), hPutAll,
                     withFile)
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.VolatileDB
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Types (FileId)
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Util

import           Test.QuickCheck hiding (elements)
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Random (mkQCGen)
import           Test.StateMachine hiding (showLabelledExamples,
                     showLabelledExamples')
import qualified Test.StateMachine.Sequential as QSM
import           Test.StateMachine.Types
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified Test.Util.Classify as C
import           Test.Util.FS.Sim.Error hiding (null)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.Orphans.Slotting.Arbitrary ()
import           Test.Util.QuickCheck
import           Test.Util.SOP
import           Test.Util.Tracer (recordingTracerIORef)

import           Test.Ouroboros.Storage.Orphans ()
import           Test.Ouroboros.Storage.TestBlock
import           Test.Ouroboros.Storage.VolatileDB.Model


type Block = TestBlock

newtype At t (r :: Type -> Type) = At {unAt :: t}
  deriving (Generic)

-- | Alias for 'At'
type (:@) t r = At t r

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

data Cmd
    = Close
    | ReOpen
    | GetBlockComponent (HeaderHash Block)
    | PutBlock Block
    | GarbageCollect SlotNo
    | FilterByPredecessor [ChainHash Block]
    | GetBlockInfo [HeaderHash Block]
    | GetMaxSlotNo
    | Corruption Corruptions
    | DuplicateBlock FileId Block
    deriving (Show, Generic)

data CmdErr = CmdErr {
      cmd :: Cmd
    , err :: Maybe Errors
    } deriving Show

-- | We compare two functions based on their results on a list of inputs
-- (functional extensionality).
data Success
    = Unit            ()
    | MbAllComponents (Maybe (AllComponents Block))
    | Bool            Bool
    | Successors      [Set (HeaderHash Block)]
    | BlockInfos      [Maybe (BlockInfo Block)]
    | MaxSlot         MaxSlotNo
    deriving (Show, Eq)

newtype Resp = Resp {
      getResp :: Either (VolatileDBError Block) Success
    }
  deriving (Eq, Show)

deriving instance Generic1          (At Cmd)
deriving instance Generic1          (At CmdErr)
deriving instance Rank2.Foldable    (At Cmd)
deriving instance Rank2.Foldable    (At CmdErr)
deriving instance Rank2.Functor     (At Cmd)
deriving instance Rank2.Functor     (At CmdErr)
deriving instance Rank2.Traversable (At CmdErr)
deriving instance Show1 r => Show   (CmdErr :@ r)
deriving instance Show1 r => Show   (Cmd :@ r)

deriving instance SOP.Generic         Cmd
deriving instance SOP.HasDatatypeInfo Cmd

deriving instance Generic1        (At Resp)
deriving instance Rank2.Foldable  (At Resp)
deriving instance Show1 r => Show (Resp :@ r)

deriving instance ToExpr EpochNo
deriving instance ToExpr SlotNo
deriving instance ToExpr FsPath
deriving instance ToExpr MaxSlotNo
deriving instance ToExpr IsEBB
deriving instance ToExpr BlocksPerFile
deriving instance ToExpr (ChainHash Block)
deriving instance ToExpr (BlockInfo Block)
deriving instance ToExpr (BlocksInFile Block)
deriving instance ToExpr (CodecConfig Block)
deriving instance ToExpr (DBModel Block)
deriving instance ToExpr (Model r)
deriving instance ToExpr TestHeaderHash
deriving instance ToExpr TestBodyHash
deriving instance ToExpr EBB
deriving instance ToExpr ChainLength
deriving instance ToExpr TestHeader
deriving instance ToExpr TestBody
deriving instance ToExpr TestBlock
deriving instance ToExpr BinaryBlockInfo
deriving instance ToExpr (ChainHash TestHeader)
deriving instance ToExpr BlockNo

instance CommandNames (At Cmd) where
  cmdName (At cmd) = constrName cmd
  cmdNames (_ :: Proxy (At Cmd r)) = constrNames (Proxy @Cmd)

instance CommandNames (At CmdErr) where
  cmdName (At (CmdErr { cmd }) ) = constrName cmd
  cmdNames (_ :: Proxy (At CmdErr r)) = constrNames (Proxy @Cmd)

newtype Model (r :: Type -> Type) = Model {
      dbModel :: DBModel Block
      -- ^ A model of the database.
    }
  deriving (Generic, Show)

-- | An event records the model before and after a command along with the
-- command itself, and a mocked version of the response.
data Event r = Event {
      eventBefore   :: Model     r
    , eventCmd      :: At CmdErr r
    , eventAfter    :: Model     r
    , eventMockResp :: Resp
    }
  deriving (Show)

lockstep :: forall r.
            Model     r
         -> At CmdErr r
         -> Event     r
lockstep model cmdErr = Event {
      eventBefore   = model
    , eventCmd      = cmdErr
    , eventAfter    = model'
    , eventMockResp = mockResp
    }
  where
    (mockResp, dbModel') = step model cmdErr
    model' = Model dbModel'

-- | Key property of the model is that we can go from real to mock responses.
toMock :: Model r -> At t r -> t
toMock _ (At t) = t

step :: Model r -> At CmdErr r -> (Resp, DBModel Block)
step model@Model{..} cmderr = runPureErr dbModel (toMock model cmderr)

runPure :: Cmd -> DBModel Block -> (Resp, DBModel Block)
runPure = \case
    GetBlockComponent h    -> ok MbAllComponents          $ queryE   (getBlockComponentModel allComponents h)
    FilterByPredecessor hs -> ok (Successors  . (<$> hs)) $ queryE    filterByPredecessorModel
    GetBlockInfo hs        -> ok (BlockInfos  . (<$> hs)) $ queryE    getBlockInfoModel
    GarbageCollect slot    -> ok Unit                     $ updateE_ (garbageCollectModel slot)
    Close                  -> ok Unit                     $ update_   closeModel
    ReOpen                 -> ok Unit                     $ update_   reOpenModel
    GetMaxSlotNo           -> ok MaxSlot                  $ queryE    getMaxSlotNoModel
    PutBlock blk           -> ok Unit                     $ updateE_ (putBlockModel blk)
    Corruption cors        -> ok Unit                     $ update_  (withClosedDB (runCorruptionsModel cors))
    DuplicateBlock {}      -> ok Unit                     $ update_  (withClosedDB noop)
  where
    queryE f m = (f m, m)

    update_ f m = (Right (), f m)
    updateE_ f m = case f m of
      Left  e  -> (Left e, m)
      Right m' -> (Right (), m')

    ok :: (a -> Success)
       -> (DBModel Block -> (Either (VolatileDBError Block) a, DBModel Block))
       -> DBModel Block
       -> (Resp, DBModel Block)
    ok toSuccess f m = first (Resp . fmap toSuccess) $ f m

    withClosedDB action = reOpenModel . action . closeModel

    noop = id

-- | When simulating an error in the real implementation, we reopen the
-- VolatileDB and run the command again, as each command is idempotent. In the
-- model, we can just run the command once.
runPureErr :: DBModel Block -> CmdErr -> (Resp, DBModel Block)
runPureErr dbm (CmdErr cmd _mbErrors) = runPure cmd dbm

sm ::
     VolatileDBEnv
  -> DBModel Block
  -> StateMachine Model (At CmdErr) IO (At Resp)
sm env dbm = StateMachine {
      initModel     = initModelImpl dbm
    , transition    = transitionImpl
    , precondition  = preconditionImpl
    , postcondition = postconditionImpl
    , generator     = generatorImpl
    , shrinker      = shrinkerImpl
    , semantics     = semanticsImpl env
    , mock          = mockImpl
    , invariant     = Nothing
    , cleanup       = noCleanup
    }

initModelImpl :: DBModel Block -> Model r
initModelImpl = Model

transitionImpl :: Model r -> At CmdErr r -> At Resp r -> Model r
transitionImpl model cmd _ = eventAfter $ lockstep model cmd

preconditionImpl :: Model Symbolic -> At CmdErr Symbolic -> Logic
preconditionImpl Model{..} (At (CmdErr cmd mbErrors)) =
    compatibleWithError .&& case cmd of
      Corruption cors ->
        forall (corruptionFiles cors) (`member` getDBFiles dbModel)

      -- When duplicating a block by appending it to some other file, make
      -- sure that both the file and the block exists, and that we're adding
      -- it /after/ the original block, so that truncating that the duplicated
      -- block brings us back in the original state.
      DuplicateBlock fileId blk -> case fileIdContainingBlock (blockHash blk) of
        Nothing      -> Bot
        Just fileId' -> fileId .>= fileId'
      _ -> Top
  where

    -- | Corruption commands are not allowed to have errors.
    compatibleWithError :: Logic
    compatibleWithError
      | not (allowErrorFor cmd), Just _ <- mbErrors
      = Bot
      | otherwise
      = Top

    fileIdContainingBlock :: HeaderHash Block -> Maybe FileId
    fileIdContainingBlock hash = listToMaybe
        [ fileId
        | (fileId, BlocksInFile blocks) <- Map.toList $ fileIndex dbModel
        , blk <- blocks
        , blockHash blk == hash
        ]

postconditionImpl :: Model Concrete
                  -> At CmdErr Concrete
                  -> At Resp Concrete
                  -> Logic
postconditionImpl model cmdErr resp =
    toMock (eventAfter ev) resp .== eventMockResp ev
  where
    ev = lockstep model cmdErr

generatorCmdImpl :: Model Symbolic -> Gen Cmd
generatorCmdImpl Model {..} = frequency
    [ (3, PutBlock <$> genTestBlock)
    , (1, return Close)
      -- When the DB is closed, we try to reopen it ASAP.
    , (if open dbModel then 1 else 5, return ReOpen)
    , (2, GetBlockComponent <$> genHash)
    , (2, GarbageCollect <$> genGCSlot)
    , (2, GetBlockInfo <$> listOf genHash)
    , (2, FilterByPredecessor <$> listOf genPrevHash)
    , (2, return GetMaxSlotNo)

    , (if null dbFiles then 0 else 1,
       Corruption <$> generateCorruptions (NE.fromList dbFiles))
    , (if isEmpty then 0 else 1, genDuplicateBlock)
    ]
  where
    blockIdx      = blockIndex dbModel
    dbFiles       = getDBFiles dbModel
    isEmpty       = Map.null blockIdx
    canContainEBB = const True

    getSlot :: HeaderHash Block -> Maybe SlotNo
    getSlot hash = blockSlot <$> Map.lookup hash blockIdx

    genSlotStartingFrom :: SlotNo -> Gen SlotNo
    genSlotStartingFrom slot = chooseSlot slot (slot + 20)

    mbMinMaxSlotInModel :: Maybe (SlotNo, SlotNo)
    mbMinMaxSlotInModel = do
      minSlot <- blockSlot . fst <$> Map.minView blockIdx
      maxSlot <- blockSlot . fst <$> Map.maxView blockIdx
      return (minSlot, maxSlot)

    -- Blocks don't have to be valid, i.e., they don't have to satisfy the
    -- invariants checked in MockChain and AnchoredFragment, etc. EBBs don't have
    -- to have a particular slot number, etc.
    genTestBlock :: Gen Block
    genTestBlock = frequency
      [ (4, genHash >>= genSuccessor)
      , (1, genRandomBlock)
      ]

    genSuccessor :: HeaderHash Block -> Gen Block
    genSuccessor prevHash = do
      b    <- genRandomBlock
      slot <- genSlotStartingFrom $ maybe 0 succ (getSlot prevHash)
      let body  = testBody b
          th    = testHeader b
          no    = thBlockNo th
          clen  = thChainLength th
          ebb   = blockIsEBB b
      return $ mkBlock canContainEBB body (BlockHash prevHash) slot no clen ebb

    genRandomBlock :: Gen Block
    genRandomBlock = do
      body     <- TestBody <$> arbitrary <*> arbitrary
      prevHash <- frequency
        [ (1, return GenesisHash)
        , (6, BlockHash . TestHeaderHash <$> arbitrary)
        ]
      slot     <- genSlotStartingFrom 0
      -- We don't care about block numbers in the VolatileDB
      no       <- BlockNo <$> arbitrary
      -- We don't care about epoch numbers in the VolatileDB
      ebb      <- arbitrary
      -- We don't care about chain length in the VolatileDB
      let clen = ChainLength (fromIntegral (unBlockNo no))
      return $ mkBlock canContainEBB body prevHash slot no clen ebb

    genHash :: Gen (HeaderHash Block)
    genHash = frequency
      [ (if isEmpty then 0 else 5, elements $ Map.keys blockIdx)
      , (1, TestHeaderHash <$> arbitrary)
      , (4, blockHash <$> genTestBlock)
      ]

    genPrevHash :: Gen (ChainHash Block)
    genPrevHash = frequency
      [ (1, return GenesisHash)
      , (8, BlockHash <$> genHash)
      ]

    -- In general, we only want to GC part of the blocks, not all of them
    genGCSlot :: Gen SlotNo
    genGCSlot = case mbMinMaxSlotInModel of
      Nothing                 -> chooseSlot 0 10
      Just (minSlot, maxSlot) ->
        -- Sometimes GC a slot lower than @minSlot@ and GC a slot higher than
        -- @maxSlot@ (i.e., nothing and everything).
        chooseSlot (subtractNoUnderflow minSlot 3) (maxSlot + 3)

    subtractNoUnderflow :: (Num a, Ord a) => a -> a -> a
    subtractNoUnderflow x y
      | x >= y    = x - y
      | otherwise = 0

    chooseSlot :: SlotNo -> SlotNo -> Gen SlotNo
    chooseSlot (SlotNo start) (SlotNo end) = SlotNo <$> choose (start, end)

    genDuplicateBlock = do
      (originalFileId, blk) <- elements
        [ (fileId, blk)
        | (fileId, BlocksInFile blks) <- Map.toList $ fileIndex dbModel
        , blk <- blks
        ]
      fileId <- elements (getDBFileIds dbModel) `suchThat` (>= originalFileId)
      return $ DuplicateBlock fileId blk

generatorImpl :: Model Symbolic -> Maybe (Gen (At CmdErr Symbolic))
generatorImpl m@Model {..} = Just $ do
    cmd <- generatorCmdImpl m
    err <- frequency
      [ (9, return Nothing)
      , (if allowErrorFor cmd && open dbModel then 1 else 0,
         -- Don't simulate errors while closed, because they won't have any
         -- effect, but also because we would reopen, which would not be
         -- idempotent.
         Just <$> genErrors True True)
      ]
    return $ At $ CmdErr cmd err

allowErrorFor :: Cmd -> Bool
allowErrorFor Corruption {}     = False
allowErrorFor DuplicateBlock {} = False
allowErrorFor _                 = True

shrinkerImpl :: Model Symbolic -> At CmdErr Symbolic -> [At CmdErr Symbolic]
shrinkerImpl m (At (CmdErr cmd mbErr)) = fmap At $
    [ CmdErr cmd mbErr' | mbErr' <- shrink mbErr ] ++
    [ CmdErr cmd' mbErr | cmd'   <- shrinkCmd m cmd ]

shrinkCmd :: Model Symbolic -> Cmd -> [Cmd]
shrinkCmd _ cmd = case cmd of
    GetBlockInfo   bids       -> GetBlockInfo <$> shrinkList (const []) bids
    FilterByPredecessor preds -> FilterByPredecessor <$> shrinkList (const []) preds
    Corruption cors           -> Corruption <$> shrinkCorruptions cors
    _                         -> []

-- | Environment to run commands against the real VolatileDB implementation.
data VolatileDBEnv = VolatileDBEnv
  { varErrors :: StrictTVar IO Errors
  , varDB     :: StrictMVar IO (VolatileDB IO Block)
  , args      :: VolatileDbArgs Identity IO Block
  }

-- | Opens a new VolatileDB and stores it in 'varDB'.
--
-- Does not close the current VolatileDB stored in 'varDB'.
reopenDB :: VolatileDBEnv -> IO ()
reopenDB VolatileDBEnv { varDB, args } = do
    db <- openDB args
    void $ swapMVar varDB db

semanticsImpl :: VolatileDBEnv -> At CmdErr Concrete -> IO (At Resp Concrete)
semanticsImpl env@VolatileDBEnv { varDB, varErrors }  (At (CmdErr cmd mbErrors)) =
    At . Resp <$> case mbErrors of
      Nothing     -> tryVolatileDB (Proxy @Block) (runDB env cmd)
      Just errors -> do
        _ <- withErrors varErrors errors $
          tryVolatileDB (Proxy @Block) (runDB env cmd)
        -- As all operations on the VolatileDB are idempotent, close (not
        -- idempotent by default!), reopen it, and run the command again.
        readMVar varDB >>= idemPotentCloseDB
        reopenDB env
        tryVolatileDB (Proxy @Block) (runDB env cmd)

idemPotentCloseDB :: VolatileDB IO Block -> IO ()
idemPotentCloseDB db =
    catchJust
      isClosedDBError
      (closeDB db)
      (const (return ()))
  where
    isClosedDBError :: VolatileDBError Block -> Maybe ()
    isClosedDBError (ApiMisuse (ClosedDBError _)) = Just ()
    isClosedDBError _                             = Nothing

runDB :: HasCallStack => VolatileDBEnv -> Cmd -> IO Success
runDB env@VolatileDBEnv { varDB, args = VolatileDbArgs { volHasFS = SomeHasFS hasFS } } cmd = readMVar varDB >>= \db -> case cmd of
    GetBlockComponent hash          -> MbAllComponents           <$> getBlockComponent db allComponents hash
    PutBlock blk                    -> Unit                      <$> putBlock db blk
    FilterByPredecessor hashes      -> Successors . (<$> hashes) <$> atomically (filterByPredecessor db)
    GetBlockInfo   hashes           -> BlockInfos . (<$> hashes) <$> atomically (getBlockInfo db)
    GarbageCollect slot             -> Unit                      <$> garbageCollect db slot
    GetMaxSlotNo                    -> MaxSlot                   <$> atomically (getMaxSlotNo db)
    Close                           -> Unit                      <$> closeDB db
    ReOpen                          -> do
        readMVar varDB >>= idemPotentCloseDB
        Unit <$> reopenDB env
    Corruption corrs                ->
      withClosedDB $
        forM_ corrs $ \(corr, file) -> corruptFile hasFS corr file
    DuplicateBlock fileId blk       -> do
      withClosedDB $
        withFile hasFS (filePath fileId) (AppendMode AllowExisting) $ \hndl ->
          void $ hPutAll hasFS hndl (testBlockToLazyByteString blk)
  where
    withClosedDB :: IO () -> IO Success
    withClosedDB action = do
      readMVar varDB >>= closeDB
      action
      reopenDB env
      return $ Unit ()

mockImpl :: Model Symbolic -> At CmdErr Symbolic -> GenSym (At Resp Symbolic)
mockImpl model cmdErr = At <$> return mockResp
  where
    (mockResp, _dbModel') = step model cmdErr


prop_sequential :: Property
prop_sequential = forAllCommands smUnused Nothing $ \cmds -> monadicIO $ do
    (hist, prop) <- run $ test cmds
    let events = execCmds (initModel smUnused) cmds
    prettyCommands smUnused hist
        $ tabulate "Tags"
          (map show $ tag events)
        $ tabulate "Commands"
          (cmdName . eventCmd <$> events)
        $ tabulate "Error Tags"
          (tagSimulatedErrors events)
        $ tabulate "GetBlockInfo: total number of Just's"
          [groupIsMember $ isMemberTrue events]
        $ tabulate "Successors"
          (tagFilterByPredecessor events)
        $ prop
  where
    dbm = initDBModel testMaxBlocksPerFile TestBlockCodecConfig
    smUnused = sm unusedEnv dbm

    groupIsMember n
      | n < 5     = show n
      | n < 20    = "5-19"
      | n < 100   = "20-99"
      | otherwise = ">=100"

test :: Commands (At CmdErr) (At Resp)
     -> IO (History (At CmdErr) (At Resp), Property)
test cmds = do
    varErrors          <- uncheckedNewTVarM mempty
    varFs              <- uncheckedNewTVarM Mock.empty
    (tracer, getTrace) <- recordingTracerIORef

    let hasFS = mkSimErrorHasFS varFs varErrors
        args = VolatileDbArgs {
                   volCheckIntegrity   = testBlockIsValid
                 , volCodecConfig      = TestBlockCodecConfig
                 , volHasFS            = SomeHasFS hasFS
                 , volMaxBlocksPerFile = testMaxBlocksPerFile
                 , volTracer           = tracer
                 , volValidationPolicy = ValidateAll
                 }

    (hist, res, trace) <- bracket
      (openDB args >>= newMVar)
      -- Note: we might be closing a different VolatileDB than the one we
      -- opened, as we can reopen it the VolatileDB, swapping the VolatileDB
      -- in the MVar.
      (\varDB -> readMVar varDB >>= closeDB)
      $ \varDB -> do
        let env = VolatileDBEnv { varErrors, varDB, args }
            sm' = sm env dbm
        (hist, _model, res) <- QSM.runCommands' sm' cmds
        trace <- getTrace
        return (hist, res, trace)

    fs <- atomically $ readTVar varFs

    let prop =
          counterexample ("Trace: " <> unlines (map show trace)) $
          counterexample ("FS: " <> Mock.pretty fs)              $
          res === Ok
    return (hist, prop)
  where
    dbm = initDBModel testMaxBlocksPerFile TestBlockCodecConfig

testMaxBlocksPerFile :: BlocksPerFile
testMaxBlocksPerFile = mkBlocksPerFile 3

unusedEnv :: VolatileDBEnv
unusedEnv = error "VolatileDBEnv used during command generation"

tests :: TestTree
tests = testGroup "VolatileDB q-s-m" [
      testProperty "sequential" prop_sequential
    ]

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

-- | Predicate on events
type EventPred = C.Predicate (Event Symbolic) Tag

-- | Convenience combinator for creating classifiers for successful commands
successful :: (    Event Symbolic
                -> Success
                -> Either Tag EventPred
              )
           -> EventPred
successful f = C.predicate $ \ev -> case (eventMockResp ev, eventCmd ev) of
    (Resp (Right ok), At (CmdErr _ Nothing)) -> f ev ok
    _                                        -> Right $ successful f

-- | Tag commands
--
-- Tagging works on symbolic events, so that we can tag without doing real IO.
tag :: [Event Symbolic] -> [Tag]
tag [] = [TagEmpty]
tag ls = C.classify
    [ tagGetBlockComponentNothing
    , tagGetJust $ Left TagGetJust
    , tagGetReOpenGet
    , tagReOpenJust
    , tagGarbageCollect True Set.empty Nothing
    , tagCorruptWriteFile
    , tagIsClosedError
    , tagGarbageCollectThenReOpen
    ] ls
  where

    tagGetBlockComponentNothing :: EventPred
    tagGetBlockComponentNothing = successful $ \ev r -> case r of
      MbAllComponents Nothing | GetBlockComponent {} <- getCmd ev ->
        Left TagGetNothing
      _ -> Right tagGetBlockComponentNothing

    tagReOpenJust :: EventPred
    tagReOpenJust = tagReOpen False $ Right $ tagGetJust $ Left TagReOpenGet

    tagGetReOpenGet :: EventPred
    tagGetReOpenGet = tagGetJust $ Right $ tagReOpen False $
                        Right $ tagGetJust $ Left TagGetReOpenGet

    -- This rarely succeeds. I think this is because the last part
    -- (get -> Nothing) rarely succeeds. This happens because when a blockId is
    -- deleted is very unlikely to be requested.
    tagGarbageCollect ::
         Bool
      -> Set (HeaderHash Block)
      -> Maybe SlotNo
      -> EventPred
    tagGarbageCollect keep bids mbGCed = successful $ \ev suc ->
      if not keep then Right $ tagGarbageCollect keep bids mbGCed
      else case (mbGCed, suc, getCmd ev) of
        (Nothing, _, PutBlock TestBlock{testHeader = TestHeader{..}})
          -> Right $ tagGarbageCollect
                       True
                       (Set.insert thHash bids)
                       Nothing
        (Nothing, _, GarbageCollect sl)
          -> Right $ tagGarbageCollect True bids (Just sl)
        (Just _gced, MbAllComponents Nothing, GetBlockComponent bid)
          | (Set.member bid bids)
          -> Left TagGarbageCollect
        (_, _, Corruption _)
          -> Right $ tagGarbageCollect False bids mbGCed
        _ -> Right $ tagGarbageCollect True bids mbGCed

    tagGetJust :: Either Tag EventPred -> EventPred
    tagGetJust next = successful $ \_ev suc -> case suc of
      MbAllComponents (Just _) -> next
      _                        -> Right $ tagGetJust next

    tagReOpen :: Bool -> Either Tag EventPred -> EventPred
    tagReOpen hasClosed next = successful $ \ev _ ->
      case (hasClosed, getCmd ev) of
        (True, ReOpen)        -> next
        (False, Close)        -> Right $ tagReOpen True next
        (False, Corruption _) -> Right $ tagReOpen True next
        _                     -> Right $ tagReOpen hasClosed next

    tagCorruptWriteFile :: EventPred
    tagCorruptWriteFile = successful $ \ev _ -> case getCmd ev of
        Corruption cors
          | let currentFile = getCurrentFile $ dbModel $ eventBefore ev
          , any (currentFileGotCorrupted currentFile) cors
          -> Left TagCorruptWriteFile
        _ -> Right tagCorruptWriteFile
      where
        currentFileGotCorrupted currentFile (cor, file)
          | DeleteFile <- cor
          = file == currentFile
          | otherwise
          = False

    tagIsClosedError :: EventPred
    tagIsClosedError = C.predicate $ \ev -> case eventMockResp ev of
      Resp (Left (ApiMisuse (ClosedDBError _))) -> Left TagClosedError
      _                                         -> Right tagIsClosedError

    tagGarbageCollectThenReOpen :: EventPred
    tagGarbageCollectThenReOpen = successful $ \ev _ -> case getCmd ev of
      GarbageCollect _ -> Right $ tagReOpen False $
                            Left TagGarbageCollectThenReOpen
      _                -> Right $ tagGarbageCollectThenReOpen

getCmd :: Event r -> Cmd
getCmd ev = cmd $ unAt (eventCmd ev)

isMemberTrue :: [Event Symbolic] -> Int
isMemberTrue events = sum $ count <$> events
  where
    count :: Event Symbolic -> Int
    count e = case eventMockResp e of
      Resp (Left _)                -> 0
      Resp (Right (BlockInfos ls)) -> length $ catMaybes ls
      Resp (Right _)               -> 0

data Tag =
    -- | Request a block successfully
    --
    -- > GetBlockComponent (returns Just)
      TagGetJust

    -- | Try to get a non-existant block
    --
    -- > GetBlockComponent (returns Nothing)
    | TagGetNothing

    -- | Make a request, close, re-open and do another request.
    --
    -- > GetBlockComponent (returns Just)
    -- > CloseDB or Corrupt
    -- > ReOpen
    -- > GetBlockComponent (returns Just)
    | TagGetReOpenGet

    -- | Close, re-open and do a request.
    --
    -- > CloseDB or Corrupt
    -- > ReOpen
    -- > GetBlockComponent (returns Just)
    | TagReOpenGet

    -- | Test Garbage Collect.
    --
    -- > PutBlock
    -- > GarbageColect
    -- > GetBlockComponent (returns Nothing)
    | TagGarbageCollect

    -- | Try to delete the current active file.
    --
    -- > Corrupt Delete
    | TagCorruptWriteFile

    -- | A test with zero commands.
    | TagEmpty

    -- | Returns ClosedDBError (whatever Command)
    | TagClosedError

    -- | Gc then Close then Open
    --
    -- > GarbageCollect
    -- > CloseDB
    -- > ReOpen
    | TagGarbageCollectThenReOpen

    deriving Show

tagSimulatedErrors :: [Event Symbolic] -> [String]
tagSimulatedErrors events = fmap tagError events
  where
    tagError :: Event Symbolic -> String
    tagError ev = case eventCmd ev of
      At (CmdErr _ Nothing) -> "NoError"
      At (CmdErr cmd _)     -> cmdName (At cmd) <> " Error"

tagFilterByPredecessor :: [Event Symbolic] -> [String]
tagFilterByPredecessor = mapMaybe f
  where
    f :: Event Symbolic -> Maybe String
    f ev = case (getCmd ev, eventMockResp ev) of
        (FilterByPredecessor _pid, Resp (Right (Successors st))) ->
            if all Set.null st then Just "Empty Successors"
            else Just "Non empty Successors"
        _otherwise -> Nothing

execCmd :: Model Symbolic
        -> Command (At CmdErr) (At Resp)
        -> Event Symbolic
execCmd model (Command cmdErr _resp _vars) = lockstep model cmdErr

execCmds :: Model Symbolic -> Commands (At CmdErr) (At Resp) -> [Event Symbolic]
execCmds model (Commands cs) = go model cs
  where
    go :: Model Symbolic -> [Command (At CmdErr) (At Resp)] -> [Event Symbolic]
    go _ []        = []
    go m (c : css) = let ev = execCmd m c in ev : go (eventAfter ev) css

showLabelledExamples :: IO ()
showLabelledExamples = showLabelledExamples' Nothing 1000

showLabelledExamples' :: Maybe Int
                      -- ^ Seed
                      -> Int
                      -- ^ Number of tests to run to find examples
                      -> IO ()
showLabelledExamples' mReplay numTests = do
    replaySeed <- case mReplay of
        Nothing   -> getStdRandom (randomR (1,999999))
        Just seed -> return seed

    labelledExamplesWith (stdArgs { replay     = Just (mkQCGen replaySeed, 0)
                                  , maxSuccess = numTests
                                  }) $
        forAllShrinkShow (QSM.generateCommands smUnused Nothing)
                         (QSM.shrinkCommands   smUnused)
                         ppShow $ \cmds ->
            collects (tag . execCmds (initModel smUnused) $ cmds) $
                property True
  where
    dbm      = initDBModel testMaxBlocksPerFile TestBlockCodecConfig
    smUnused = sm unusedEnv dbm
