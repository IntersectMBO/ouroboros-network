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

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor (bimap)
import qualified Data.Binary as Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BL
import           Data.Functor.Classes
import           Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Maybe (fromJust, isJust)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.TreeDiff (ToExpr)
import           Data.TreeDiff.Class
import           GHC.Generics
import           GHC.Stack
import qualified System.IO as IO
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
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API (HasFS (..))
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal hiding (openDB)

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
  deriving Show

instance Eq Success where
    Unit () == Unit () = True
    Blob mbs == Blob mbs' = mbs == mbs'
    Blocks ls == Blocks ls' = S.fromList ls == S.fromList ls'
    Bl bl == Bl bl' = bl == bl'
    IsMember ls == IsMember ls' = ls == ls'
    SimulatedError _ == SimulatedError _ = True
    _ == _ = False


newtype Resp = Resp {getResp :: Either (VolatileDBError BlockId) Success}
    deriving (Eq, Show)

data Cmd
    = IsOpen
    | Close
    | ReOpen
    | GetBlock BlockId
    | GetBlockIds
    | PutBlock BlockId
    | GarbageCollect BlockId
    | AskIfMember [BlockId]
    | Corrupt Corruptions
    | CreateFile
    | CreateInvalidFile
    | DuplicateBlock (String, BlockId)
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

instance Show (Model r) where
    show = show . toShow

deriving instance Show (ModelShow r)
deriving instance Generic (DBModel BlockId)
deriving instance Generic (ModelShow r)
deriving instance ToExpr SlotNo
deriving instance Generic BlockId
deriving instance Generic (ParserError BlockId)
deriving instance ToExpr (ParserError BlockId)
deriving instance ToExpr (DBModel BlockId)
deriving instance ToExpr (ModelShow r)


instance ToExpr (Model r) where
    toExpr = toExpr . toShow

instance CommandNames (At Cmd) where
    cmdName (At cmd) = case cmd of
        GetBlock _        -> "GetBlock"
        GetBlockIds       -> "GetBlockIds"
        PutBlock _        -> "PutBlock"
        GarbageCollect _  -> "GarbageCollect"
        IsOpen            -> "IsOpen"
        Close             -> "Close"
        ReOpen            -> "ReOpen"
        AskIfMember _     -> "AskIfMember"
        Corrupt _         -> "Corrupt"
        CreateFile        -> "CreateFile"
        CreateInvalidFile -> "CreateInvalidFile"
        DuplicateBlock _  -> "DuplicateBlock"
    cmdNames _ = ["not", "suported", "yet"]

instance CommandNames (At CmdErr) where
    cmdName (At (CmdErr cmd _)) = cmdName (At cmd)
    cmdNames _ = ["not", "suported", "yet"]


data Model (r :: Type -> Type) = Model
  { dbModel   :: DBModel BlockId
    -- ^ A model of the database.
  , mockDB    :: ModelDBPure
    -- ^ A handle to the mocked database.
  , shouldEnd :: Bool
  } deriving (Generic)

data ModelShow (r :: Type -> Type) = Model'
  { msdbModel   :: DBModel BlockId
  , msShouldEnd :: Bool
  }

toShow :: Model r -> ModelShow r
toShow (Model dbm _ se) = Model' dbm se

type PureM = ExceptT (VolatileDBError BlockId) (State (DBModel BlockId, Maybe Errors))
type ModelDBPure = VolatileDB BlockId PureM

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
                    Resp (Left (VParserError _)) -> True
                    _                            -> False
            }

-- | Key property of the model is that we can go from real to mock responses
toMock :: Model r -> At t r -> t
toMock _ (At t) = t

step :: Model r -> At CmdErr r -> (Resp, DBModel BlockId)
step model@Model{..} cmderr = runPure dbModel mockDB (toMock model cmderr)

runPure :: DBModel BlockId
        -> ModelDBPure
        -> CmdErr
        -> (Resp, DBModel BlockId)
runPure dbm mdb (CmdErr cmd err) =
    bimap Resp fst $ flip runState (dbm, err) $ do
        resp <- runExceptT $ runDB runRest mdb cmd
        case (err, resp) of
            (Nothing, _)                       -> return resp
            (Just _ , Left ClosedDBError)      -> return resp
            (Just _, _) -> do
                modify $ \(dbm', cErr) -> (dbm' {open = False}, cErr)
                return $ Right $ SimulatedError resp
    where
        runRest :: ModelDBPure -> Cmd -> PureM Success
        runRest db cmd' = case cmd' of
            Corrupt cors -> do
                closeDB db
                runCorruptionModel toSlot cors
                reOpenDB db
                return $ Unit ()
            CreateFile -> do
                closeDB db
                createFileModel
                reOpenDB db
                return $ Unit ()
            CreateInvalidFile -> do
                closeDB db
                createInvalidFileModel "invalidFileName.dat"
                reOpenDB db
                return $ Unit ()
            DuplicateBlock bid -> do
                closeDB db
                duplicateBlockModel bid
                reOpenDB db
                return $ Unit ()
            _ -> error "invalid cmd"

runDB :: (HasCallStack, Monad m)
      => (VolatileDB BlockId m -> Cmd -> m Success)
      -> VolatileDB BlockId m
      -> Cmd
      -> m Success
runDB restCmd db cmd = case cmd of
    GetBlock bid       -> Blob <$> getBlock db bid
    GetBlockIds        -> Blocks <$> getBlockIds db
    PutBlock bid       -> Unit <$> putBlock db bid (BL.lazyByteString $ Binary.encode $ toBlock bid)
    GarbageCollect bid -> Unit <$> garbageCollect db (toSlot bid)
    IsOpen             -> Bl <$> isOpenDB db
    Close              -> Unit <$> closeDB db
    ReOpen             -> Unit <$> reOpenDB db
    Corrupt _          -> restCmd db cmd
    CreateFile         -> restCmd db cmd
    CreateInvalidFile  -> restCmd db cmd
    DuplicateBlock _   -> restCmd db cmd
    AskIfMember bids   -> do
        isMember <- getIsMember db
        return $ IsMember $ isMember <$> bids

smErr :: (MonadCatch m, MonadSTM m)
      => Bool
      -> TVar m Errors
      -> HasFS m h
      -> VolatileDB BlockId m
      -> Internal.VolatileDBEnv m blockId
      -> DBModel BlockId
      -> ModelDBPure
      -> StateMachine Model (At CmdErr) m (At Resp)
smErr terminatingCmd errorsVar hasFS db env dbm vdb = StateMachine {
     initModel     = initModelImpl dbm vdb
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

stateMachine :: (MonadCatch m, MonadSTM m)
             => DBModel BlockId
             -> ModelDBPure
             -> StateMachine Model (At CmdErr) m (At Resp)
stateMachine = smErr
                True
                (error "errorsVar unused")
                (error "hasFS used during command generation")
                (error "semantics and DB used during command generation")
                (error "env used during command generation")

initModelImpl :: DBModel BlockId -> ModelDBPure -> Model r
initModelImpl dbm vdm = Model {
      dbModel   = dbm
    , mockDB    = vdm
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
        DuplicateBlock (_file, bid) ->
            let bids = concat $ (\(_,(_, _, bs)) -> bs) <$> (M.toList $ index dbModel)
            in bid `elem` bids
        _ -> Top
  where
      corruptionFiles = map snd . NE.toList

postconditionImpl :: Model Concrete -> At CmdErr Concrete -> At Resp Concrete -> Logic
postconditionImpl model cmdErr resp =
    toMock (eventAfter ev) resp .== eventMockResp ev
  where
    ev = lockstep model cmdErr resp

generatorCmdImpl :: Bool -> Model Symbolic -> Maybe (Gen (At Cmd Symbolic))
generatorCmdImpl terminatingCmd m@Model {..} =
    if shouldEnd then Nothing else Just $ do
    sl <- blockIdgenerator m
    let lastGC = latestGarbaged dbModel
    let dbFiles :: [String] = getDBFiles dbModel
    ls <- filter (newer lastGC . toSlot) <$> (listOf $ blockIdgenerator m)
    bid <- do
        let bids = concat $ (\(f,(_, _, bs)) -> map (\b -> (f,b)) bs) <$> (M.toList $ index dbModel)
        case bids of
            [] -> return Nothing
            _  -> Just <$> elements bids
    cmd <- frequency
        [ (150, return $ GetBlock sl)
        , (100, return $ GetBlockIds)
        , (150, return $ PutBlock sl)
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
        , (if terminatingCmd && isJust bid then 1 else 0, return $ DuplicateBlock $ fromJust bid)
        ]
    return $ At cmd

generatorImpl :: Bool -> Bool -> Model Symbolic -> Maybe (Gen (At CmdErr Symbolic))
generatorImpl mkErr terminatingCmd m@Model {..} = do
    genCmd <- generatorCmdImpl terminatingCmd m
    Just $ do
        At cmd <- genCmd
        err' <- if noErrorFor cmd then return Nothing
           else frequency
                [ (8, return Nothing)
                , (if mkErr then 1 else 0, Just <$> arbitrary)]
        let err = erasePutCorruptions err'
        return $ At $ CmdErr cmd err
    where
        -- This doesn't reduce the power of tests. This is because we have partial
        -- writes as part of file Corruption and not as part of simulated errors.
        eraseCorruptions str = (\(fsErr, _) -> (fsErr, Nothing)) <$> str
        erasePutCorruptions mErr = do
            err <- mErr
            -- we don't use corruptions as part of simulated errors.
            return err {_hPut = eraseCorruptions $ _hPut err}
        noErrorFor GetBlock {}          = False
        noErrorFor GetBlockIds          = False
        noErrorFor ReOpen {}            = False
        noErrorFor IsOpen {}            = False
        noErrorFor Close {}             = False
        noErrorFor AskIfMember {}       = False
        noErrorFor GarbageCollect {}    = False
        noErrorFor PutBlock {}          = False
        noErrorFor CreateInvalidFile {} = True
        noErrorFor CreateFile {}        = True
        noErrorFor Corrupt {}           = True
        noErrorFor DuplicateBlock {}    = True

blockIdgenerator :: Model Symbolic -> Gen BlockId
blockIdgenerator Model {..} = do
    sl <- arbitrary
    -- list must be non empty
    elements $ sl : (M.keys $ mp dbModel)

getDBFiles :: DBModel BlockId -> [String]
getDBFiles DBModel {..} = M.keys index

newer :: Ord a => Maybe a -> a -> Bool
newer Nothing _   = True
newer (Just a) a' = a' >= a

shrinkerImpl :: Model Symbolic -> At CmdErr Symbolic -> [At CmdErr Symbolic]
shrinkerImpl _ _ = []

semanticsImplErr :: (MonadCatch m, MonadSTM m)
                 => TVar m Errors
                 -> HasFS m h
                 -> VolatileDB BlockId m
                 -> Internal.VolatileDBEnv m blockId
                 -> At CmdErr Concrete
                 -> m (At Resp Concrete)
semanticsImplErr errorsVar hasFS m env (At cmderr) = At . Resp <$> case cmderr of
    CmdErr cmd Nothing ->
        tryVolDB (runDB (semanticsRestCmd hasFS env) m cmd)
    CmdErr cmd (Just errors) -> do
        res <- withErrors errorsVar errors $
            tryVolDB (runDB (semanticsRestCmd hasFS env) m cmd)
        case res of
            Left ClosedDBError -> return res
            _                  -> do
                closeDB m
                return $ Right $ SimulatedError res

semanticsRestCmd :: (MonadSTM m, MonadCatch m)
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
        withFile hasFS ["invalidFileName.dat"] IO.AppendMode $ \_hndl -> do
            return ()
        reOpenDB db
        return $ Unit ()
    DuplicateBlock (_file, bid) -> do
        let specialEnc = Binary.encode $ toBlock bid
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
    GetBlock bid       -> Boolean $ isLimitation (latestGarbaged $ dbModel model) (toSlot bid)
    GetBlockIds        -> Bot
    PutBlock bid       -> Boolean $ isLimitation (latestGarbaged $ dbModel model) (toSlot bid)
    GarbageCollect _sl -> Bot
    IsOpen             -> Bot
    Close              -> Bot
    ReOpen             -> Bot
    Corrupt _          -> Bot
    CreateFile         -> Boolean $ not $ open $ dbModel model
    AskIfMember bids   -> exists ((\b -> isLimitation (latestGarbaged $ dbModel model) (toSlot b)) <$> bids) Boolean
    CreateInvalidFile  -> Boolean $ not $ open $ dbModel model
    DuplicateBlock _   -> Boolean $ not $ open $ dbModel model
    where
        isLimitation :: (Ord slot) => Maybe slot -> slot -> Bool
        isLimitation Nothing _sl       = False
        isLimitation (Just slot') slot = slot' >  slot

mkDBModel :: MonadState (DBModel BlockId, Maybe Errors) m
          => Int
          -> (BlockId -> SlotNo)
          -> (DBModel BlockId, VolatileDB BlockId (ExceptT (VolatileDBError BlockId) m))
mkDBModel = openDBModel EH.exceptT

prop_sequential :: Property
prop_sequential = withMaxSuccess 1000 $
    forAllCommands smUnused Nothing $ \cmds -> monadicIO $ do
        let test :: TVar IO Errors
                 -> HasFS IO h
                 -> PropertyM IO (History (At CmdErr) (At Resp), Reason)
            test errorsVar hasFS = do
              (db, env) <- run $ Internal.openDBFull hasFS EH.monadCatch (myParser hasFS EH.monadCatch) 3 toSlot
              let sm' = smErr True errorsVar hasFS db env dbm vdb
              (hist, _model, res) <- runCommands sm' cmds
              run $ closeDB db
              return (hist, res)
        errorsVar <- run $ atomically (newTVar mempty)
        fsVar <- run $ atomically (newTVar Mock.empty)
        (hist, res) <- test errorsVar (mkSimErrorHasFS EH.monadCatch fsVar errorsVar)
        let events = execCmds (initModel smUnused) cmds
        let myshow n = if n<5 then show n else if n < 20 then "5-19" else if n < 100 then "20-99" else ">=100"
        prettyCommands smUnused hist
            $ tabulate "Tags" (map show $ tag events)
            $ tabulate "Commands" (cmdName . eventCmd <$> events)
            $ tabulate "Error Tags" (tagSimulatedErrors events)
            $ tabulate "IsMember: Total number of True's" [myshow $ isMemberTrue events]
            $ tabulate "IsMember: At least one True" [show $ isMemberTrue' events]
            $ res === Ok
    where
        -- we use that: MonadState (DBModel BlockId) (State (DBModel BlockId))
        (dbm, vdb) = mkDBModel 3 toSlot
        smUnused = stateMachine
                    dbm
                    vdb

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
            (Nothing, _, PutBlock bid)
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
        Resp (Left ClosedDBError) -> Left TagClosedError
        _                         -> Right tagIsClosedError

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
    (dbm, vdb) = mkDBModel 3 toSlot
    smUnused :: StateMachine Model (At CmdErr) IO (At Resp)
    smUnused = stateMachine
                dbm
                vdb
