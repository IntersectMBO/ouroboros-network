{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ouroboros.Storage.VolatileDB.StateMachine
    ( tests
    , showLabelledExamples
    ) where

import           Prelude hiding (elem)

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Builder as BL
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Classes
import           Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Maybe (fromJust, isJust, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.TreeDiff (ToExpr (..), defaultExprViaShow)
import           Data.Word
import           GHC.Generics
import           GHC.Stack
import           System.Random (getStdRandom, randomR)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Random (mkQCGen)
import           Test.StateMachine
import           Test.StateMachine.Sequential
import           Test.StateMachine.Types
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Show.Pretty (ppShow)

import           Ouroboros.Network.Point (WithOrigin)
import qualified Ouroboros.Network.Point as WithOrigin

import           Ouroboros.Consensus.Block (IsEBB (..))
import qualified Ouroboros.Consensus.Util.Classify as C
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common (BlockComponent (..))
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal hiding (openDB)
import           Ouroboros.Storage.VolatileDB.Util

import           Test.Util.FS.Sim.Error hiding (null)
import qualified Test.Util.FS.Sim.MockFS as Mock

import           Test.Ouroboros.Storage.Util
import           Test.Ouroboros.Storage.VolatileDB.Model
import           Test.Ouroboros.Storage.VolatileDB.TestBlock

newtype At t (r :: Type -> Type) = At {unAt :: t}
  deriving (Generic)

-- | Alias for 'At'
type (:@) t r = At t r

-- | Product of all 'BlockComponent's. As this is a GADT, generating random
-- values of it (and combinations!) is not so simple. Therefore, we just
-- always request all block components.
allComponents :: BlockComponent (VolatileDB BlockId m) AllComponents
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
  , BlockId
  , SlotNo
  , IsEBB
  , Word32
  , Word16
  )

data Success
    = Unit            ()
    | MbAllComponents (Maybe AllComponents)
    | Blocks          [BlockId]
    | Bl              Bool
    | IsMember        [Bool] -- ^ We compare two functions based on their
                      -- results on a list of inputs.
    | SimulatedError  (Either VolatileDBError Success)
    | Successors      [Set BlockId]
    | Predecessor     [Predecessor]
    | MaxSlot         MaxSlotNo
    deriving Show

instance Eq Success where
    Unit ()             == Unit ()              = True
    Unit _              == _                    = False
    MbAllComponents mbc == MbAllComponents mbc' = mbc == mbc'
    MbAllComponents _   == _                    = False
    Blocks ls           == Blocks ls'           = S.fromList ls == S.fromList ls'
    Blocks _            == _                    = False
    Bl bl               == Bl bl'               = bl == bl'
    Bl _                == _                    = False
    IsMember ls         == IsMember ls'         = ls == ls'
    IsMember _          == _                    = False
    SimulatedError _    == SimulatedError _     = True
    SimulatedError _    == _                    = False
    Successors st1      == Successors st2       = st1 == st2
    Successors _        == _                    = False
    Predecessor p1      == Predecessor p2       = p1 == p2
    Predecessor _       == _                    = False
    MaxSlot ms1         == MaxSlot ms2          = ms1 == ms2
    MaxSlot _           == _                    = False


newtype Resp = Resp {getResp :: Either VolatileDBError Success}
    deriving (Eq, Show)

data Cmd
    = IsOpen
    | Close
    | ReOpen
    | GetBlockComponent BlockId
    | GetBlockIds
    | PutBlock BlockId Predecessor IsEBB ByteString -- ^ @ByteString@ is the header.
    | GarbageCollect BlockId
    | AskIfMember [BlockId]
    | Corrupt Corruptions
    | CreateFile
    | CreateInvalidFile
    | DuplicateBlock FsPath BlockId Predecessor
    | GetSuccessors [Predecessor]
    | GetPredecessor [BlockId]
    | GetMaxSlotNo
    deriving Show

data CmdErr = CmdErr {
      cmd :: Cmd
    , err :: Maybe Errors
    } deriving Show

deriving instance Generic1          (At Cmd)
deriving instance Generic1          (At CmdErr)
deriving instance Rank2.Foldable    (At Cmd)
deriving instance Rank2.Foldable    (At CmdErr)
deriving instance Rank2.Functor     (At Cmd)
deriving instance Rank2.Functor     (At CmdErr)
deriving instance Rank2.Traversable (At CmdErr)
deriving instance Show1 r => Show   (CmdErr :@ r)
deriving instance Show1 r => Show   (Cmd :@ r)

deriving instance Generic1        (At Resp)
deriving instance Rank2.Foldable  (At Resp)
deriving instance Show1 r => Show (Resp :@ r)

deriving instance Generic (DBModel BlockId)
deriving instance ToExpr SlotNo
deriving instance Generic BlockId
deriving instance ToExpr FsPath
deriving instance ToExpr MaxSlotNo
deriving instance ToExpr IsEBB
deriving instance ToExpr (DBModel BlockId)
deriving instance ToExpr (Model r)
deriving instance ToExpr (WithOrigin BlockId)

instance ToExpr ParserError where
  toExpr = defaultExprViaShow

instance CommandNames (At Cmd) where
    cmdName (At cmd) = case cmd of
      GetBlockComponent {} -> "GetBlockComponent"
      GetBlockIds          -> "GetBlockIds"
      PutBlock {}          -> "PutBlock"
      GarbageCollect {}    -> "GarbageCollect"
      IsOpen               -> "IsOpen"
      Close                -> "Close"
      ReOpen               -> "ReOpen"
      AskIfMember {}       -> "AskIfMember"
      Corrupt {}           -> "Corrupt"
      CreateFile {}        -> "CreateFile"
      CreateInvalidFile {} -> "CreateInvalidFile"
      DuplicateBlock {}    -> "DuplicateBlock"
      GetSuccessors {}     -> "GetSuccessors"
      GetPredecessor {}    -> "GetPredecessor"
      GetMaxSlotNo {}      -> "GetMaxSlotNo"
    cmdNames _ = ["not", "suported", "yet"]

instance CommandNames (At CmdErr) where
    cmdName (At (CmdErr cmd _)) = cmdName (At cmd)
    cmdNames _ = ["not", "suported", "yet"]

data Model (r :: Type -> Type) = Model {
      dbModel   :: DBModel BlockId
      -- ^ A model of the database.
    , shouldEnd :: Bool
      -- ^ Indicates that no further commands should be generated.
    } deriving (Generic, Show)

type PureM = ExceptT VolatileDBError (State (DBModel BlockId))

-- | An event records the model before and after a command along with the
-- command itself, and a mocked version of the response.
data Event r = Event {
      eventBefore   :: Model  r
    , eventCmd      :: At CmdErr r
    , eventAfter    :: Model  r
    , eventMockResp :: Resp
    } deriving (Show)

lockstep :: forall r.
            Model     r
         -> At CmdErr r
         -> At Resp   r
         -> Event     r
lockstep model@Model {..} cmdErr (At resp) = Event {
      eventBefore   = model
    , eventCmd      = cmdErr
    , eventAfter    = model'
    , eventMockResp = mockResp
    }
  where
    (mockResp, dbModel') = step model cmdErr
    ends = case resp of
      Resp (Left (UnexpectedError (ParserError _))) -> True
      _                                             -> False
    model' = model {
        dbModel   = dbModel'
      , shouldEnd = ends
      }

-- | Key property of the model is that we can go from real to mock responses.
toMock :: Model r -> At t r -> t
toMock _ (At t) = t

step :: Model r -> At CmdErr r -> (Resp, DBModel BlockId)
step model@Model{..} cmderr = runPure dbModel (toMock model cmderr)

runPure :: DBModel BlockId
        -> CmdErr
        -> (Resp, DBModel BlockId)
runPure dbm (CmdErr cmd err) =
    bimap Resp id $ flip runState dbm $ do
      resp <- runExceptT go
      case (err, resp) of
        (Nothing, _) -> return resp
        (Just _ , Left (UserError ClosedDBError))
                     -> return resp
        (Just _, _)  -> do
          modify $ \dbm' -> dbm' {open = False}
          return $ Right $ SimulatedError resp
  where
    tnc :: EH.ThrowCantCatch VolatileDBError PureM
    tnc = EH.throwCantCatch EH.exceptT

    go :: PureM Success
    go = case cmd of
      GetBlockComponent bid  ->
        MbAllComponents <$> getBlockComponentModel tnc allComponents bid
      GetBlockIds           -> Blocks <$> getBlockIdsModel tnc
      PutBlock b pb e h     -> do
        let blockInfo = BlockInfo {
                bbid          = b
              , bslot         = guessSlot b
              , bpreBid       = pb
              , bisEBB        = e
              , bheaderOffset = headerOffset
              , bheaderSize   = headerSize
              }
            blob = BL.lazyByteString $ toBinary (b, pb, h)
        Unit <$> putBlockModel tnc err blockInfo blob
      GetSuccessors bids    -> do
        successors <- getSuccessorsModel tnc
        return $ Successors $ successors <$> bids
      GetPredecessor bids   -> do
        predecessor <- getPredecessorModel tnc
        return $ Predecessor $ predecessor <$> bids
      GarbageCollect bid    -> 
        Unit <$> garbageCollectModel tnc err (guessSlot bid)
      IsOpen                -> Bl <$> isOpenModel
      Close                 -> Unit <$> closeModel
      ReOpen                -> Unit <$> reOpenModel tnc
      AskIfMember bids      -> do
        isMember <- getIsMemberModel tnc
        return $ IsMember $ isMember <$> bids
      GetMaxSlotNo          -> MaxSlot <$> getMaxSlotNoModel tnc
      Corrupt cors          -> withClosedDB $ runCorruptionModel guessSlot cors
      CreateFile            -> withClosedDB createFileModel
      CreateInvalidFile     -> withClosedDB $
        createInvalidFileModel (mkFsPath ["invalidFileName.dat"])
      DuplicateBlock file bid _pbid 
                            -> withClosedDB $ duplicateBlockModel (file, bid)

    withClosedDB :: PureM () -> PureM Success
    withClosedDB act = do
      closeModel
      act
      reOpenModel tnc
      return $ Unit ()

sm :: IOLike m
   => Bool
   -> StrictTVar m Errors
   -> VolatileDB BlockId m
   -> Internal.VolatileDBEnv m BlockId
   -> DBModel BlockId
   -> StateMachine Model (At CmdErr) m (At Resp)
sm terminatingCmd errorsVar db env dbm = StateMachine {
      initModel     = initModelImpl dbm
    , transition    = transitionImpl
    , precondition  = preconditionImpl
    , postcondition = postconditionImpl
    , generator     = generatorImpl True terminatingCmd
    , shrinker      = shrinkerImpl
    , semantics     = semanticsImpl errorsVar db env
    , mock          = mockImpl
    , invariant     = Nothing
    , distribution  = Nothing
    }

stateMachine :: IOLike m
             => DBModel BlockId
             -> StateMachine Model (At CmdErr) m (At Resp)
stateMachine = sm
                True
                (error "errorsVar unused")
                (error "semantics and DB used during command generation")
                (error "env used during command generation")

initModelImpl :: DBModel BlockId -> Model r
initModelImpl dbm = Model {
      dbModel   = dbm
    , shouldEnd = False
    }

transitionImpl :: Model r -> At CmdErr r -> At Resp r -> Model r
transitionImpl model cmd = eventAfter . lockstep model cmd

preconditionImpl :: Model Symbolic -> At CmdErr Symbolic -> Logic
preconditionImpl Model{..} (At (CmdErr cmd err)) =
    Not (Boolean shouldEnd)
    .&& compatibleWithError
    .&& case cmd of
      GetBlockComponent bid -> Boolean $ afterGC $ guessSlot bid
      GetPredecessor bids   ->
        forall bids (`elem` bidsInModel)
      PutBlock bid _ _ _    ->
        Boolean $ afterGC $ guessSlot bid
      AskIfMember bids      -> Boolean $ and
        (afterGC . guessSlot <$> bids)
      Corrupt cors          ->
        isOpen
        .&& forall (corruptionFiles cors) (`elem` getDBFiles dbModel)
      CreateFile            -> isOpen
      CreateInvalidFile     -> isOpen
      DuplicateBlock file bid _ ->
        case fmap fst . snd <$> M.lookup file (index dbModel) of
          Nothing   -> Bot
          Just bids -> isOpen .&& bid `elem` bids
      _                     -> Top
  where
    -- | Checks if the given block is bigger then the biggest gced slot.
    afterGC :: SlotNo -> Bool
    afterGC slot = case latestGarbaged dbModel of
      Nothing    -> True
      Just slot' -> slot' <=  slot

    -- | Is the db open?
    isOpen :: Logic
    isOpen = Boolean $ open $ dbModel

    -- | All the 'BlockId' in the db.
    bidsInModel :: [BlockId]
    bidsInModel = filter (newer (latestGarbaged dbModel) . guessSlot)
                    $ M.keys
                    $ mp dbModel

    -- | Corruption commands are not allowed to have errors.
    compatibleWithError :: Logic
    compatibleWithError = case (allowErrorFor cmd, isJust err) of
      (False, True) -> Bot
      _             -> Top

postconditionImpl :: Model Concrete 
                  -> At CmdErr Concrete  
                  -> At Resp Concrete 
                  -> Logic
postconditionImpl model cmdErr resp =
    toMock (eventAfter ev) resp .== eventMockResp ev
  where
    ev = lockstep model cmdErr resp

generatorCmdImpl :: Bool -> Model Symbolic -> Maybe (Gen (At Cmd Symbolic))
generatorCmdImpl terminatingCmd m@Model {..} =
    if shouldEnd then Nothing else Just $ do
    block <- blockIdGenerator m
    pblock <- predecessorGenerator
    blocks <- listOf $ blockIdGenerator m
    header :: Word8 <- arbitrary
    duplicate <- mkDuplicate' pblock
    let allowDuplication = terminatingCmd && isJust duplicate
        isEBB = IsNotEBB  -- TODO we need a proper TestBlock
    At <$> frequency [
        (150, return $ GetBlockComponent block)
      , (100, return $ GetBlockIds)
      , (150, return $ PutBlock block (WithOrigin.At pblock) isEBB
                                (BL.singleton header))
      , (100, return $ GetSuccessors $ WithOrigin.At block : 
          (WithOrigin.At <$> possiblePredecessors))
      , (100, return $ GetPredecessor blocks)
      , (100, return $ GetMaxSlotNo)
      , (50, return $ GarbageCollect block)
      , (50, return $ IsOpen)
      , (50, return $ Close)
      , (30, return CreateFile)
      -- When the db is Closed, we try to ReOpen it asap.
      -- This helps minimize TagClosedError and create more
      -- interesting tests.
      , (if open dbModel then 10 else 1000, return $ ReOpen)
      , (if null blocks then 0 else 30, return $ AskIfMember blocks)
      , (if null dbFiles then 0 else 30, Corrupt <$> generateCorruptions dbFiles)
      , (if terminatingCmd then 1 else 0, return CreateInvalidFile)
      , (if allowDuplication then 1 else 0, return $ fromJust duplicate)
      ]
  where
    dbFiles = getDBFiles dbModel
    mkDuplicate' pblock =
      case mkDuplicate pblock <$> getDBBlocksWithFiles dbModel of
        []   -> return Nothing
        bids -> Just <$> elements bids
    mkDuplicate pblock = \(f, b) -> DuplicateBlock f b $ WithOrigin.At pblock

generatorImpl :: Bool 
              -> Bool 
              -> Model Symbolic 
              -> Maybe (Gen (At CmdErr Symbolic))
generatorImpl mkErr terminatingCmd m@Model {..} = do
    genCmd <- generatorCmdImpl terminatingCmd m
    Just $ do
      At cmd <- genCmd
      err <- if not (allowErrorFor cmd) then return Nothing
        else frequency
          [ (8, return Nothing)
            -- TODO use the full generator for 'Errors', i.e.
            -- 'arbitrary'. Now we disable partial writes and
            -- 'SubstituteWithJunk' corruptions because we cannot
            -- predict what the model should do with those.
          , (if mkErr then 1 else 0, Just <$> genErrors False False)]
      return $ At $ CmdErr cmd err

allowErrorFor :: Cmd -> Bool
allowErrorFor CreateInvalidFile {} = False
allowErrorFor CreateFile {}        = False
allowErrorFor Corrupt {}           = False
allowErrorFor DuplicateBlock {}    = False
allowErrorFor _                    = True

blockIdGenerator :: Model Symbolic -> Gen BlockId
blockIdGenerator Model {..} = do
    sl <- arbitrary
    elements $ sl : (fst <$> getBlockId dbModel)

predecessorGenerator :: Gen BlockId
predecessorGenerator = elements possiblePredecessors

possiblePredecessors :: [BlockId]
possiblePredecessors = [0,1,2]

newer :: Ord a => Maybe a -> a -> Bool
newer Nothing _   = True
newer (Just a) a' = a' >= a

shrinkerImpl :: Model Symbolic -> At CmdErr Symbolic -> [At CmdErr Symbolic]
shrinkerImpl m (At (CmdErr cmd mbErr)) = fmap At $
    [ CmdErr cmd mbErr' | mbErr' <- shrink mbErr ] ++
    [ CmdErr cmd' mbErr | cmd'   <- shrinkCmd m cmd ]

shrinkCmd :: Model Symbolic -> Cmd -> [Cmd]
shrinkCmd Model{..} cmd = case cmd of
    AskIfMember bids    -> AskIfMember <$> shrink bids
    GetPredecessor bids -> GetPredecessor <$> shrink bids
    Corrupt cors        -> Corrupt . NE.fromList <$>
                             shrinkList shrinkNothing (NE.toList cors)
    _                   -> []

semanticsImpl :: IOLike m
              => StrictTVar m Errors
              -> VolatileDB BlockId m
              -> Internal.VolatileDBEnv m BlockId -- ^ Used only for corruptions
              -> At CmdErr Concrete
              -> m (At Resp Concrete)
semanticsImpl errorsVar m env (At cmderr) = At . Resp <$> case cmderr of
    CmdErr cmd Nothing -> try (runDB m cmd env)
    CmdErr cmd (Just errors) -> do
      res <- withErrors errorsVar errors $
        try (runDB m cmd env)
      case res of
        Left (UserError ClosedDBError) -> return res
        _                              -> do
          closeDB m
          return $ Right $ SimulatedError res
  where
    try = tryVolDB EH.monadCatch EH.monadCatch

runDB :: forall m. (HasCallStack, IOLike m)
      => VolatileDB BlockId m
      -> Cmd
      -> Internal.VolatileDBEnv m BlockId
      -> m Success
runDB db cmd env@Internal.VolatileDBEnv{..} = case cmd of
    GetBlockComponent bid -> 
      MbAllComponents <$> getBlockComponent db allComponents bid
    GetBlockIds           -> Blocks <$> getBlockIds db
    PutBlock b pb e h     -> Unit <$> putBlock db
      (BlockInfo b (guessSlot b) pb e headerOffset headerSize)
      (BL.lazyByteString $ toBinary (b, pb, h))
    GetSuccessors bids    -> do
      successors <- atomically $ getSuccessors db
      return $ Successors $ successors <$> bids
    GetPredecessor bids   -> do
      predecessor <- atomically $ getPredecessor db
      return $ Predecessor $ predecessor <$> bids
    GarbageCollect bid    -> Unit <$> garbageCollect db (guessSlot bid)
    IsOpen                -> Bl <$> isOpenDB db
    Close                 -> Unit <$> closeDB db
    ReOpen                -> Unit <$> reOpenDB db
    AskIfMember bids      -> do
      isMember <- atomically $ getIsMember db
      return $ IsMember $ isMember <$> bids
    GetMaxSlotNo          -> atomically $ MaxSlot <$> getMaxSlotNo db
        -- Corruption.
    Corrupt corrs         ->
      withClosedDB $
        forM_ corrs $ \(corr,file) -> corruptFile _dbHasFS corr file
    CreateFile            -> do
      createFile env
      withClosedDB $ return ()
    CreateInvalidFile     ->
      withClosedDB $
        withFile _dbHasFS (mkFsPath ["invalidFileName.dat"])
          (AppendMode MustBeNew) $ \_hndl -> return ()
    DuplicateBlock _file  bid preBid -> do
        let specialEnc = toBinary (bid, preBid, "a")
        _ <- withDBState $ \hasFS st ->
          hPut hasFS (Internal._currentWriteHandle st)
                          (BL.lazyByteString specialEnc)
        withClosedDB $ return ()
  where
    withClosedDB action = do
      closeDB db
      action
      reOpenDB db
      return $ Unit ()

    -- This will soon be replaced by @withState@.
    withDBState :: forall r
              . (  forall h
                .  HasFS m h
                -> Internal.InternalState BlockId h
                -> m r
                )
              -> m r
    withDBState action = Internal.modifyState env $
      \hasFS st -> (st,) <$> action hasFS st

mockImpl :: Model Symbolic -> At CmdErr Symbolic -> GenSym (At Resp Symbolic)
mockImpl model cmdErr = At <$> return mockResp
  where
    (mockResp, _dbModel') = step model cmdErr

prop_sequential :: Property
prop_sequential =
    forAllCommands smUnused Nothing $ \cmds -> monadicIO $ do
        let test :: StrictTVar IO Errors
                 -> HasFS IO h
                 -> PropertyM IO (History (At CmdErr) (At Resp), Reason)
            test errorsVar hasFS = do
              let ec = EH.throwCantCatch EH.monadCatch
              (db, env) <- run $
                Internal.openDBFull hasFS EH.monadCatch ec (myParser hasFS) 3
              let sm' = sm True errorsVar db env dbm
              (hist, _model, res) <- runCommands sm' cmds
              run $ closeDB db
              return (hist, res)
        errorsVar <- run $ uncheckedNewTVarM mempty
        fsVar <- run $ uncheckedNewTVarM Mock.empty
        let simHasFS = mkSimErrorHasFS EH.monadCatch fsVar errorsVar
        (hist, res) <- test errorsVar simHasFS
        let events = execCmds (initModel smUnused) cmds
        prettyCommands smUnused hist
            $ tabulate "Tags" (map show $ tag events)
            $ tabulate "Commands" (cmdName . eventCmd <$> events)
            $ tabulate "Error Tags" (tagSimulatedErrors events)
            $ tabulate "IsMember: Total number of True's" 
                [groupIsMember $ isMemberTrue events]
            $ tabulate "IsMember: At least one True" 
                [show $ isMemberTrue' events]
            $ tabulate "Successors" (tagGetSuccessors events)
            $ tabulate "Predecessor" (tagGetPredecessor events)
            $ res === Ok
    where
        dbm = initDBModel 3
        smUnused = stateMachine
                    dbm

        groupIsMember n =
          if n<5 then show n
          else if n < 20 then "5-19"
          else if n < 100 then "20-99" else ">=100"

tests :: TestTree
tests = testGroup "VolatileDB-q-s-m" [
      testProperty "q-s-m-Errors" $ withMaxSuccess 3000 prop_sequential
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
-- TODO(kde) This needs extending as part of #251
tag :: [Event Symbolic] -> [Tag]
tag [] = [TagEmpty]
tag ls = C.classify
    [ tagGetBlockComponentNothing
    , tagGetJust $ Left TagGetJust
    , tagGetReOpenGet
    , tagReOpenJust
    , tagGarbageCollect True S.empty Nothing
    , tagCorruptWriteFile
    , tagAppendRecover
    , tagIsClosedError
    , tagGarbageCollectThenReOpen
    , tagImpossibleToRecover
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
    -- (get -> Nothing) rarelly succeeds. This happens because when a blockId is
    -- deleted is very unlikely to be requested.
    tagGarbageCollect :: Bool -> Set BlockId -> Maybe BlockId -> EventPred
    tagGarbageCollect keep bids mgced = successful $ \ev suc ->
        if not keep then Right $ tagGarbageCollect keep bids mgced
        else case (mgced, suc, getCmd ev) of
          (Nothing, _, PutBlock bid _pbid _isEBB _h)
            -> Right $ tagGarbageCollect True (S.insert bid bids) Nothing
          (Nothing, _, GarbageCollect bid)
            -> Right $ tagGarbageCollect True bids (Just bid)
          (Just _gced, MbAllComponents Nothing, GetBlockComponent bid) | (S.member bid bids)
            -> Left TagGarbageCollect
          (_, _, Corrupt _) 
            -> Right $ tagGarbageCollect False bids mgced
          _ -> Right $ tagGarbageCollect True bids mgced

    tagGetJust :: Either Tag EventPred -> EventPred
    tagGetJust next = successful $ \_ev suc -> case suc of
      MbAllComponents (Just _) -> next
      _                        -> Right $ tagGetJust next

    tagReOpen :: Bool -> Either Tag EventPred -> EventPred
    tagReOpen hasClosed next = successful $ \ev _ -> 
      case (hasClosed, getCmd ev) of
        (True, ReOpen)     -> next
        (False, Close)     -> Right $ tagReOpen True next
        (False, Corrupt _) -> Right $ tagReOpen True next
        _                  -> Right $ tagReOpen hasClosed next

    tagCorruptWriteFile :: EventPred
    tagCorruptWriteFile = successful $ \ev _ -> case getCmd ev of
      Corrupt cors -> if any (\(cor,file) -> (cor == DeleteFile)
          && (file == (getCurrentFile $ dbModel $ eventBefore ev)))
          (NE.toList cors)
        then Left TagCorruptWriteFile
        else Right tagCorruptWriteFile
      _            -> Right tagCorruptWriteFile

    tagAppendRecover :: EventPred
    tagAppendRecover =
        successful $ \ev _ -> case getCmd ev of
          Corrupt cors | any (\(cor,_file) -> doesAppend cor) (NE.toList cors)
              -> Left TagAppendRecover
          _ -> Right tagAppendRecover
      where
        doesAppend (AppendBytes 0) = False
        doesAppend (AppendBytes _) = True
        doesAppend _               = False

    tagIsClosedError :: EventPred
    tagIsClosedError = C.predicate $ \ev -> case eventMockResp ev of
      Resp (Left (UserError ClosedDBError)) -> Left TagClosedError
      _                                     -> Right tagIsClosedError

    tagGarbageCollectThenReOpen :: EventPred
    tagGarbageCollectThenReOpen = successful $ \ev _ -> case getCmd ev of
      GarbageCollect _ -> Right $ tagReOpen False $ 
                            Left TagGarbageCollectThenReOpen
      _                -> Right $ tagGarbageCollectThenReOpen

    tagImpossibleToRecover :: EventPred
    tagImpossibleToRecover = C.predicate $ \ev ->
      if shouldEnd (eventBefore ev) || shouldEnd (eventAfter ev)
      then Left TagImpossibleToRecover
      else Right tagImpossibleToRecover

getCmd :: Event r -> Cmd
getCmd ev = cmd $ unAt (eventCmd ev)

isMemberTrue :: [Event Symbolic] -> Int
isMemberTrue events = sum $ count <$> events
  where
    count :: Event Symbolic -> Int
    count e = case eventMockResp e of
      Resp (Left _)              -> 0
      Resp (Right (IsMember ls)) -> length $ filter id ls
      Resp (Right _)             -> 0

isMemberTrue' :: [Event Symbolic] -> Int
isMemberTrue' events = sum $ count <$> events
  where
    count :: Event Symbolic -> Int
    count e = case eventMockResp e of
        Resp (Left _)              -> 0
        Resp (Right (IsMember ls)) -> if length (filter id ls) > 0 then 1 else 0
        Resp (Right _)             -> 0

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
    -- TODO(kde): This is actually a limitation, since we never
    -- actually request gced blocks.
    | TagGarbageCollect

    -- | Try to delete the current active file.
    --
    -- > Corrupt Delete
    | TagCorruptWriteFile

    -- | Recover after writing garbage to a file.
    --
    -- > Corrupt Append
    | TagAppendRecover

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

    -- | Command which irreversibly corrupt the db.
    | TagImpossibleToRecover
    deriving Show

tagSimulatedErrors :: [Event Symbolic] -> [String]
tagSimulatedErrors events = fmap tagError events
  where
    tagError :: Event Symbolic -> String
    tagError ev = case eventCmd ev of
      At (CmdErr _ Nothing) -> "NoError"
      At (CmdErr cmd _)     -> cmdName (At cmd) <> " Error"

tagGetSuccessors :: [Event Symbolic] -> [String]
tagGetSuccessors = mapMaybe f
  where
    f :: Event Symbolic -> Maybe String
    f ev = case (getCmd ev, eventMockResp ev) of
        (GetSuccessors _pid, Resp (Right (Successors st))) ->
            if all S.null st then Just "Empty Successors"
            else Just "Non empty Successors"
        _otherwise -> Nothing

tagGetPredecessor :: [Event Symbolic] -> [String]
tagGetPredecessor = mapMaybe f
  where
    f :: Event Symbolic -> Maybe String
    f ev = case (getCmd ev, eventMockResp ev) of
        (GetPredecessor _pid, Resp (Right (Predecessor _))) ->
            Just "Predecessor"
        _otherwise -> Nothing

execCmd :: Model Symbolic
        -> Command (At CmdErr) (At Resp)
        -> Event Symbolic
execCmd model (Command cmdErr resp _vars) = lockstep model cmdErr resp

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
        forAllShrinkShow (generateCommands smUnused Nothing)
                         (shrinkCommands   smUnused)
                         ppShow $ \cmds ->
            collects (tag . execCmds (initModel smUnused) $ cmds) $
                property True
  where
    dbm = initDBModel 3
    smUnused :: StateMachine Model (At CmdErr) IO (At Resp)
    smUnused = stateMachine
                dbm
