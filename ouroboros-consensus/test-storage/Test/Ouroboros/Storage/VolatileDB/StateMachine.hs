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
{-# LANGUAGE TypeApplications    #-}
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
import qualified Data.Binary as Binary
import qualified Data.ByteString.Builder as BL
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Classes
import           Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Maybe (fromJust, isJust, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.TreeDiff (ToExpr)
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

import           Ouroboros.Consensus.Util (SomePair (..))
import qualified Ouroboros.Consensus.Util.Classify as C
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal hiding (openDB)
import           Ouroboros.Storage.VolatileDB.Util

import           Test.Ouroboros.Storage.FS.Sim.Error hiding (null)
import           Test.Ouroboros.Storage.Util
import           Test.Ouroboros.Storage.VolatileDB.Model
import           Test.Ouroboros.Storage.VolatileDB.TestBlock

newtype At t (r :: Type -> Type) = At {unAt :: t}
  deriving (Generic)

-- | Alias for 'At'
type (:@) t r = At t r

data Success
  = Unit     ()
  | Blob     (Maybe ByteString)
  | Blocks   [BlockId]
  | Bl       Bool
  | IsMember [Bool] -- We compare two functions based on their results on a list of inputs.
  | SimulatedError (Either (VolatileDBError BlockId) Success)
  | Successors [Set BlockId]
  | Predecessor [Predecessor]
  | MaxSlot  MaxSlotNo
  deriving Show

instance Eq Success where
    Unit ()          == Unit ()          = True
    Unit _           == _                = False
    Blob mbs         == Blob mbs'        = mbs == mbs'
    Blob _           == _                = False
    Blocks ls        == Blocks ls'       = S.fromList ls == S.fromList ls'
    Blocks _         == _                = False
    Bl bl            == Bl bl'           = bl == bl'
    Bl _             == _                = False
    IsMember ls      == IsMember ls'     = ls == ls'
    IsMember _       == _                = False
    SimulatedError _ == SimulatedError _ = True
    SimulatedError _ == _                = False
    Successors st1   == Successors st2   = st1 == st2
    Successors _     == _                = False
    Predecessor p1   == Predecessor p2   = p1 == p2
    Predecessor _    == _                = False
    MaxSlot ms1      == MaxSlot ms2      = ms1 == ms2
    MaxSlot _        == _                = False


newtype Resp = Resp {getResp :: Either (VolatileDBError BlockId) Success}
    deriving (Eq, Show)

data Cmd
    = IsOpen
    | Close
    | ReOpen
    | GetBlock BlockId
    | GetBlockIds
    | PutBlock BlockId Predecessor
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

data CmdErr = CmdErr
    {
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
deriving instance Generic (ParserError BlockId)
deriving instance ToExpr FsPath
deriving instance ToExpr (ParserError BlockId)
deriving instance ToExpr MaxSlotNo
deriving instance ToExpr (DBModel BlockId)
deriving instance ToExpr (Model r)

instance CommandNames (At Cmd) where
    cmdName (At cmd) = case cmd of
        GetBlock {}          -> "GetBlock"
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

data Model (r :: Type -> Type) = Model
  { dbModel   :: DBModel BlockId
    -- ^ A model of the database.
  , shouldEnd :: Bool
  } deriving (Generic, Show)

type PureM = ExceptT (VolatileDBError BlockId) (State (DBModel BlockId))

-- | An event records the model before and after a command along with the
-- command itself, and a mocked version of the response.
data Event r = Event
  { eventBefore   :: Model  r
  , eventCmd      :: At CmdErr r
  , eventAfter    :: Model  r
  , eventMockResp :: Resp
  } deriving (Show)

lockstep :: forall r.
            Model     r
         -> At CmdErr r
         -> At Resp   r
         -> Event     r
lockstep model@Model {..} cmdErr (At resp) = Event
    { eventBefore   = model
    , eventCmd      = cmdErr
    , eventAfter    = model'
    , eventMockResp = mockResp
    }
  where
    (mockResp, dbModel') = step model cmdErr
    model' = model {
              dbModel = dbModel'
            , shouldEnd = case resp of
                    Resp (Left (UnexpectedError (ParserError _))) -> True
                    _                                             -> False
            }

-- | Key property of the model is that we can go from real to mock responses
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
            (Nothing, _)                             -> return resp
            (Just _ , Left (UserError ClosedDBError)) -> return resp
            (Just _, _) -> do
                modify $ \dbm' -> dbm' {open = False}
                return $ Right $ SimulatedError resp
    where
        tnc :: EH.ThrowCantCatch (VolatileDBError BlockId) PureM = EH.throwCantCatch EH.exceptT
        go :: PureM Success
        go = case cmd of
            GetBlock bid       -> do
                Blob <$> getBlockModel tnc bid
            GetBlockIds        ->
                Blocks <$> getBlockIdsModel tnc
            PutBlock b pb      ->
                Unit <$> putBlockModel tnc err (BlockInfo b (guessSlot b) pb) (BL.lazyByteString $ Binary.encode $ toBlock (b, pb))
            GetSuccessors bids -> do
                successors <- getSuccessorsModel tnc
                return $ Successors $ successors <$> bids
            GetPredecessor bids   -> do
                predecessor <- getPredecessorModel tnc
                return $ Predecessor $ predecessor <$> bids
            GarbageCollect bid -> Unit <$> garbageCollectModel tnc err (guessSlot bid)
            IsOpen             -> Bl <$> isOpenModel
            Close              -> Unit <$> closeModel
            ReOpen             -> Unit <$> reOpenModel tnc
            AskIfMember bids   -> do
                isMember <- getIsMemberModel tnc
                return $ IsMember $ isMember <$> bids
            GetMaxSlotNo       -> MaxSlot <$> getMaxSlotNoModel tnc
            Corrupt cors       -> do
                closeModel
                runCorruptionModel guessSlot cors
                reOpenModel tnc
                return $ Unit ()
            CreateFile         -> do
                closeModel
                createFileModel
                reOpenModel tnc
                return $ Unit ()
            CreateInvalidFile  -> do
                closeModel
                createInvalidFileModel (mkFsPath ["invalidFileName.dat"])
                reOpenModel tnc
                return $ Unit ()
            DuplicateBlock file bid _pbid -> do
                closeModel
                duplicateBlockModel (file, bid)
                reOpenModel tnc
                return $ Unit ()

runDB :: (HasCallStack, IOLike m)
      => (VolatileDB BlockId m -> Cmd -> m Success)
      -> VolatileDB BlockId m
      -> Cmd
      -> m Success
runDB restCmd db cmd = case cmd of
    GetBlock bid       -> Blob <$> getBlock db bid
    GetBlockIds        -> Blocks <$> getBlockIds db
    PutBlock b pb      -> Unit <$> putBlock db (BlockInfo b (guessSlot b) pb) (BL.lazyByteString $ Binary.encode $ toBlock (b, pb))
    GetSuccessors bids -> do
        successors <- atomically $ getSuccessors db
        return $ Successors $ successors <$> bids
    GetPredecessor bids -> do
        predecessor <- atomically $ getPredecessor db
        return $ Predecessor $ predecessor <$> bids
    GarbageCollect bid -> Unit <$> garbageCollect db (guessSlot bid)
    IsOpen             -> Bl <$> isOpenDB db
    Close              -> Unit <$> closeDB db
    ReOpen             -> Unit <$> reOpenDB db
    Corrupt {}         -> restCmd db cmd
    CreateFile         -> restCmd db cmd
    CreateInvalidFile  -> restCmd db cmd
    DuplicateBlock {}  -> restCmd db cmd
    AskIfMember bids   -> do
        isMember <- atomically $ getIsMember db
        return $ IsMember $ isMember <$> bids
    GetMaxSlotNo       -> atomically $ MaxSlot <$> getMaxSlotNo db

sm :: IOLike m
   => Bool
   -> StrictTVar m Errors
   -> HasFS m h
   -> VolatileDB BlockId m
   -> Internal.VolatileDBEnv m BlockId
   -> DBModel BlockId
   -> StateMachine Model (At CmdErr) m (At Resp)
sm terminatingCmd errorsVar hasFS db env dbm = StateMachine {
     initModel     = initModelImpl dbm
   , transition    = transitionImpl
   , precondition  = preconditionImpl
   , postcondition = postconditionImpl
   , generator     = generatorImpl True terminatingCmd
   , shrinker      = shrinkerImpl
   , semantics     = semanticsImplErr errorsVar hasFS db env
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
                (error "hasFS used during command generation")
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
preconditionImpl m@Model {..} (At (CmdErr cmd _)) =
    Not (knownLimitation m (At cmd))
    .&& case cmd of
        Corrupt cors ->
            forall (corruptionFiles cors) (`elem` getDBFiles dbModel)
            .&& (Boolean $ open dbModel)
        DuplicateBlock file bid pbid ->
            let bids = concat $ (\(f,(_, _, bs)) -> ((\(b,bp)->(f,b,bp)) <$> bs)) <$> (M.toList $ index dbModel)
            in (file, bid, pbid) `elem` bids
        GetPredecessor bids ->
            forall bids (`elem` bidsInModel)
        _ -> Top
  where
      corruptionFiles = map snd . NE.toList
      bidsInModel = filter (newer (latestGarbaged dbModel) . guessSlot)
                  $ M.keys
                  $ mp dbModel

postconditionImpl :: Model Concrete -> At CmdErr Concrete -> At Resp Concrete -> Logic
postconditionImpl model cmdErr resp =
    toMock (eventAfter ev) resp .== eventMockResp ev
  where
    ev = lockstep model cmdErr resp

generatorCmdImpl :: Bool -> Model Symbolic -> Maybe (Gen (At Cmd Symbolic))
generatorCmdImpl terminatingCmd m@Model {..} =
    if shouldEnd then Nothing else Just $ do
    sl <- blockIdGenerator m
    psl <- predecessorGenerator m
    let lastGC = latestGarbaged dbModel
    let dbFiles :: [FsPath] = getDBFiles dbModel
    ls <- filter (newer lastGC . guessSlot) <$> (listOf $ blockIdGenerator m)
    bid <- do
        let bids = concat $ (\(f,(_, _, bs)) -> map (\(b, pb) -> (f, b, pb)) bs) <$> (M.toList $ index dbModel)
        case bids of
            [] -> return Nothing
            _  -> Just <$> elements bids
    let duplicate = (\(f, b, pb) -> DuplicateBlock f b pb) <$> bid
    cmd <- frequency
        [ (150, return $ GetBlock sl)
        , (100, return $ GetBlockIds)
        , (150, return $ PutBlock sl $ Just psl)
        , (100, return $ GetSuccessors $ Just sl : (Just <$> possiblePredecessors))
        , (100, return $ GetPredecessor ls)
        , (100, return $ GetMaxSlotNo)
        , (50, return $ GarbageCollect sl)
        , (50, return $ IsOpen)
        , (50, return $ Close)
        , (30, return CreateFile)
        -- When the db is Closed, we try to ReOpen it asap.
        -- This helps minimize TagClosedError and create more
        -- interesting tests.
        , (if open dbModel then 10 else 1000, return $ ReOpen)
        , (if null ls then 0 else 30, return $ AskIfMember ls)
        , (if null dbFiles then 0 else 30, Corrupt <$> generateCorruptions (NE.fromList dbFiles))
        , (if terminatingCmd then 1 else 0, return CreateInvalidFile)
        , (if terminatingCmd && isJust duplicate then 1 else 0, return $ fromJust duplicate)
        ]
    return $ At cmd

generatorImpl :: Bool -> Bool -> Model Symbolic -> Maybe (Gen (At CmdErr Symbolic))
generatorImpl mkErr terminatingCmd m@Model {..} = do
    genCmd <- generatorCmdImpl terminatingCmd m
    Just $ do
        At cmd <- genCmd
        err <- if noErrorFor cmd then return Nothing
           else frequency
                [ (8, return Nothing)
                  -- TODO use the full generator for 'Errors', i.e.
                  -- 'arbitrary'. Now we disable partial writes and
                  -- 'SubstituteWithJunk' corruptions because we cannot
                  -- predict what the model should do with those.
                , (if mkErr then 1 else 0, Just <$> genErrors False False)]
        return $ At $ CmdErr cmd err
    where
        noErrorFor GetBlock {}          = False
        noErrorFor GetBlockIds          = False
        noErrorFor ReOpen {}            = False
        noErrorFor IsOpen {}            = False
        noErrorFor Close {}             = False
        noErrorFor AskIfMember {}       = False
        noErrorFor GarbageCollect {}    = False
        noErrorFor PutBlock {}          = False
        noErrorFor GetSuccessors {}     = False
        noErrorFor GetPredecessor {}    = False
        noErrorFor GetMaxSlotNo {}      = False
        noErrorFor CreateInvalidFile {} = True
        noErrorFor CreateFile {}        = True
        noErrorFor Corrupt {}           = True
        noErrorFor DuplicateBlock {}    = True

blockIdGenerator :: Model Symbolic -> Gen BlockId
blockIdGenerator Model {..} = do
    sl <- arbitrary
    -- list must be non empty
    elements $ sl : (M.keys $ mp dbModel)

predecessorGenerator :: Model Symbolic -> Gen BlockId
predecessorGenerator Model {..} = elements possiblePredecessors

possiblePredecessors :: [BlockId]
possiblePredecessors = [0,1,2]

getDBFiles :: DBModel BlockId -> [FsPath]
getDBFiles DBModel {..} = M.keys index

newer :: Ord a => Maybe a -> a -> Bool
newer Nothing _   = True
newer (Just a) a' = a' >= a

shrinkerImpl :: Model Symbolic -> At CmdErr Symbolic -> [At CmdErr Symbolic]
shrinkerImpl _ _ = []

semanticsImplErr :: IOLike m
                 => StrictTVar m Errors
                 -> HasFS m h
                 -> VolatileDB BlockId m
                 -> Internal.VolatileDBEnv m blockId
                 -> At CmdErr Concrete
                 -> m (At Resp Concrete)
semanticsImplErr errorsVar hasFS m env (At cmderr) = At . Resp <$> case cmderr of
    CmdErr cmd Nothing ->
        try (runDB (semanticsRestCmd hasFS env) m cmd)
    CmdErr cmd (Just errors) -> do
        res <- withErrors errorsVar errors $
            try (runDB (semanticsRestCmd hasFS env) m cmd)
        case res of
            Left (UserError ClosedDBError) -> return res
            _                              -> do
                closeDB m
                return $ Right $ SimulatedError res
  where
    try = tryVolDB EH.monadCatch EH.monadCatch

semanticsRestCmd :: IOLike m
                 => HasFS m h
                 -> Internal.VolatileDBEnv m blockId
                 -> VolatileDB BlockId m
                 -> Cmd
                 -> m Success
semanticsRestCmd hasFS env db cmd = case cmd of
    Corrupt corrs -> do
        closeDB db
        forM_ corrs $ \(corr,file) -> corruptFile hasFS corr file
        reOpenDB db
        return $ Unit ()
    CreateFile -> do
        createFileImpl hasFS env
        closeDB db
        reOpenDB db
        return $ Unit ()
    CreateInvalidFile -> do
        closeDB db
        withFile hasFS (mkFsPath ["invalidFileName.dat"]) (AppendMode MustBeNew) $ \_hndl -> do
            return ()
        reOpenDB db
        return $ Unit ()
    DuplicateBlock _file  bid preBid -> do
        let specialEnc = Binary.encode $ toBlock (bid, preBid)
        SomePair stHasFS st <- Internal.getInternalState env
        let hndl = Internal._currentWriteHandle st
        _ <- hPut stHasFS hndl (BL.lazyByteString specialEnc)
        closeDB db
        reOpenDB db
        return $ Unit ()
    _ -> error "invalid cmd"

mockImpl :: Model Symbolic -> At CmdErr Symbolic -> GenSym (At Resp Symbolic)
mockImpl model cmdErr = At <$> return mockResp
    where
        (mockResp, _dbModel') = step model cmdErr

knownLimitation :: Model Symbolic -> Cmd :@ Symbolic -> Logic
knownLimitation model (At cmd) = case cmd of
    GetBlock bid       -> Boolean $ isLimitation (latestGarbaged $ dbModel model) (guessSlot bid)
    GetBlockIds        -> Bot
    GetSuccessors {}   -> Bot
    GetPredecessor {}  -> Bot
    GetMaxSlotNo {}    -> Bot
    PutBlock bid _pbid -> Boolean $ isLimitation (latestGarbaged $ dbModel model) (guessSlot bid)
    GarbageCollect _sl -> Bot
    IsOpen             -> Bot
    Close              -> Bot
    ReOpen             -> Bot
    Corrupt _          -> Bot
    CreateFile         -> Boolean $ not $ open $ dbModel model
    AskIfMember bids   -> exists ((\b -> isLimitation (latestGarbaged $ dbModel model) (guessSlot b)) <$> bids) Boolean
    CreateInvalidFile  -> Boolean $ not $ open $ dbModel model
    DuplicateBlock {}  -> Boolean $ not $ open $ dbModel model
    where
        isLimitation :: (Ord slot) => Maybe slot -> slot -> Bool
        isLimitation Nothing _sl       = False
        isLimitation (Just slot') slot = slot' >  slot

prop_sequential :: Property
prop_sequential =
    forAllCommands smUnused Nothing $ \cmds -> monadicIO $ do
        let test :: StrictTVar IO Errors
                 -> HasFS IO h
                 -> PropertyM IO (History (At CmdErr) (At Resp), Reason)
            test errorsVar hasFS = do
              (db, env) <- run $ Internal.openDBFull hasFS EH.monadCatch (EH.throwCantCatch EH.monadCatch) (myParser hasFS) 3
              let sm' = sm True errorsVar hasFS db env dbm
              (hist, _model, res) <- runCommands sm' cmds
              run $ closeDB db
              return (hist, res)
        errorsVar <- run $ uncheckedNewTVarM mempty
        fsVar <- run $ uncheckedNewTVarM Mock.empty
        (hist, res) <- test errorsVar (mkSimErrorHasFS EH.monadCatch fsVar errorsVar)
        let events = execCmds (initModel smUnused) cmds
        let myshow n = if n<5 then show n else if n < 20 then "5-19" else if n < 100 then "20-99" else ">=100"
        prettyCommands smUnused hist
            $ tabulate "Tags" (map show $ tag events)
            $ tabulate "Commands" (cmdName . eventCmd <$> events)
            $ tabulate "Error Tags" (tagSimulatedErrors events)
            $ tabulate "IsMember: Total number of True's" [myshow $ isMemberTrue events]
            $ tabulate "IsMember: At least one True" [show $ isMemberTrue' events]
            $ tabulate "Successors" (tagGetSuccessors events)
            $ tabulate "Predecessor" (tagGetPredecessor events)
            $ res === Ok
    where
        -- we use that: MonadState (DBModel BlockId) (State (DBModel BlockId))
        dbm = initDBModel 3
        smUnused = stateMachine
                    dbm

tests :: TestTree
tests = testGroup "VolatileDB-q-s-m" [
      testProperty "q-s-m-Errors" $ prop_sequential
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
    [ tagGetBinaryBlobNothing
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

    tagGetBinaryBlobNothing :: EventPred
    tagGetBinaryBlobNothing = successful $ \ev r -> case r of
        Blob Nothing | GetBlock {} <- getCmd ev ->
            Left TagGetNothing
        _ -> Right tagGetBinaryBlobNothing

    tagReOpenJust :: EventPred
    tagReOpenJust = tagReOpen False $ Right $ tagGetJust $ Left TagReOpenGet

    tagGetReOpenGet :: EventPred
    tagGetReOpenGet = tagGetJust $ Right $ tagReOpen False $ Right $ tagGetJust $ Left TagGetReOpenGet

    -- This rarely succeeds. I think this is because the last part (get -> Nothing) rarelly succeeds.
    -- This happens because when a blockId is deleted is very unlikely to be requested.
    tagGarbageCollect :: Bool -> Set BlockId -> Maybe BlockId -> EventPred
    tagGarbageCollect keep bids mgced = successful $ \ev suc ->
        if not keep then Right $ tagGarbageCollect keep bids mgced
        else case (mgced, suc, getCmd ev) of
            (Nothing, _, PutBlock bid _pbid)
                -> Right $ tagGarbageCollect True (S.insert bid bids) Nothing
            (Nothing, _, GarbageCollect bid)
                -> Right $ tagGarbageCollect True bids (Just bid)
            (Just _gced, Blob Nothing, GetBlock bid) | (S.member bid bids)
                -> Left TagGarbageCollect
            (_, _, Corrupt _) -> Right $ tagGarbageCollect False bids mgced
            _ -> Right $ tagGarbageCollect True bids mgced

    tagGetJust :: Either Tag EventPred -> EventPred
    tagGetJust next = successful $ \_ev suc -> case suc of
        Blob (Just _) -> next
        _             -> Right $ tagGetJust next

    tagReOpen :: Bool -> Either Tag EventPred -> EventPred
    tagReOpen hasClosed next = successful $ \ev _ -> case (hasClosed, getCmd ev) of
        (True, ReOpen)     -> next
        (False, Close)     -> Right $ tagReOpen True next
        (False, Corrupt _) -> Right $ tagReOpen True next
        _                  -> Right $ tagReOpen hasClosed next

    tagCorruptWriteFile :: EventPred
    tagCorruptWriteFile = successful $ \ev _ -> case getCmd ev of
        Corrupt cors -> if any (\(cor,file) -> (cor == DeleteFile) && (file == (currentFile $ dbModel $ eventBefore ev))) (NE.toList cors)
                        then Left TagCorruptWriteFile
                        else Right tagCorruptWriteFile
        _            -> Right tagCorruptWriteFile

    tagAppendRecover :: EventPred
    tagAppendRecover =
        let
            doesAppend (AppendBytes 0) = False
            doesAppend (AppendBytes _) = True
            doesAppend _               = False
        in  successful $ \ev _ -> case getCmd ev of
            Corrupt cors | any (\(cor,_file) -> doesAppend cor) (NE.toList cors)
              -> Left TagAppendRecover
            _ -> Right tagAppendRecover

    tagIsClosedError :: EventPred
    tagIsClosedError = C.predicate $ \ev -> case eventMockResp ev of
        Resp (Left (UserError ClosedDBError)) -> Left TagClosedError
        _                                     -> Right tagIsClosedError

    tagGarbageCollectThenReOpen :: EventPred
    tagGarbageCollectThenReOpen = successful $ \ev _ -> case getCmd ev of
        GarbageCollect _ -> Right $ tagReOpen False $ Left TagGarbageCollectThenReOpen
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
isMemberTrue' events = sum $ f <$> events
    where
        f :: Event Symbolic -> Int
        f e = case eventMockResp e of
            Resp (Left _)              -> 0
            Resp (Right (IsMember ls)) -> if length (filter id ls) > 0 then 1 else 0
            Resp (Right _)             -> 0

data Tag =
    -- | Request a block successfully
    --
    -- > GetBlock (returns Just)
      TagGetJust

    -- | Try to get a non-existant block
    --
    -- > GetBlock (returns Nothing)
    | TagGetNothing

    -- | Make a request, close, re-open and do another request.
    --
    -- > GetBlock (returns Just)
    -- > CloseDB or Corrupt
    -- > ReOpen
    -- > GetBlock (returns Just)
    | TagGetReOpenGet

    -- | Close, re-open and do a request.
    --
    -- > CloseDB or Corrupt
    -- > ReOpen
    -- > GetBlock (returns Just)
    | TagReOpenGet

    -- | Test Garbage Collect.
    --
    -- > PutBlock
    -- > GarbageColect
    -- > GetBlock (returns Nothing)
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
tagSimulatedErrors events = fmap f events
    where
        f :: Event Symbolic -> String
        f ev = case eventCmd ev of
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
