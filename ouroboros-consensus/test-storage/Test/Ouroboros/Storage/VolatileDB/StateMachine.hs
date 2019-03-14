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

module Test.Ouroboros.Storage.VolatileDB.StateMachine (tests) where

import           Prelude hiding (elem)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor (bimap)
import qualified Data.Binary as Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BS
import           Data.Functor.Classes
import           Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.TreeDiff (ToExpr)
import           Data.TreeDiff.Class
import           GHC.Generics
import           GHC.Stack
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.StateMachine
import           Test.StateMachine.Types
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified Ouroboros.Consensus.Util.Classify as C
import           Ouroboros.Storage.FS.API (HasFS (..))
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import           Ouroboros.Storage.VolatileDB.Impl (openDB)

import           Test.Ouroboros.Storage.FS.Sim.Error
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
  | Bl       Bool
  | IsMember [Bool] -- We compare two functions based on their results on a list of inputs.
  | SimulatedError (Either (VolatileDBError BlockId) Success)
  deriving Show

instance Eq Success where
    Unit () == Unit () = True
    Blob mbs == Blob mbs' = mbs == mbs'
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
    | PutBlock BlockId
    | GarbageCollect BlockId
    | AskIfMember [BlockId]
    | Corrupt Corruptions
    deriving (Show)

data CmdErr = CmdErr
    {
      cmd :: Cmd
    , err :: Maybe Errors
    } deriving (Show)

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
deriving instance Generic SlotNo
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
        GetBlock _       -> "GetBlock"
        PutBlock _       -> "PutBlock"
        GarbageCollect _ -> "GarbageCollect"
        IsOpen           -> "IsOpen"
        Close            -> "Close"
        ReOpen           -> "ReOpen"
        AskIfMember _    -> "AskIfMember"
        Corrupt _        -> "Corrupt"
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
  , eventCmd      :: At Cmd r
  , eventAfter    :: Model  r
  , eventMockResp :: Resp
  } deriving (Show)

lockstep :: forall r.
            Model     r
         -> At CmdErr r
         -> At Resp   r
         -> Event     r
lockstep model@Model {..} cmdErr@(At (CmdErr cmd _)) (At resp) = Event
    { eventBefore   = model
    , eventCmd      = At cmd
    , eventAfter    = model'
    , eventMockResp = mockResp
    }
  where
    (mockResp, dbModel') = step model cmdErr
    model' = model {
              dbModel = dbModel'
            , shouldEnd = case resp of
                    Resp (Left (VParserError (DecodeFailed _ _ _))) -> True
                    _                                               -> False
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
        resp <- runExceptT $ runDB runCorruptions mdb cmd
        case (err, resp) of
            (Nothing, _)                       -> return resp
            (Just _ , Left ClosedDBError)      -> return resp
            (Just _, _) -> do
                modify $ \(dbm, err) -> (dbm {open = False}, err)
                return $ Right $ SimulatedError resp
    where
        runCorruptions :: ModelDBPure -> Corruptions -> PureM Success
        runCorruptions db cors = do
            closeDB db
            runCorruptionModel toSlot cors
            reOpenDB db
            return $ Unit ()

runDB :: (HasCallStack, Monad m)
      => (VolatileDB BlockId m -> Corruptions -> m Success)
      -> VolatileDB BlockId m
      -> Cmd
      -> m Success
runDB runCorruptions db cmd = case cmd of
    GetBlock bid       -> Blob <$> getBlock db bid
    PutBlock bid       -> Unit <$> putBlock db bid (BS.lazyByteString $ Binary.encode $ toBlock bid)
    GarbageCollect bid -> Unit <$> garbageCollect db (toSlot bid)
    IsOpen             -> Bl <$> isOpenDB db
    Close              -> Unit <$> closeDB db
    ReOpen             -> Unit <$> reOpenDB db
    Corrupt cors       -> runCorruptions db cors
    AskIfMember bids   -> do
        isMember <- getIsMember db
        return $ IsMember $ isMember <$> bids


sm :: MonadCatch m
   => HasFS m h
   -> VolatileDB BlockId m
   -> DBModel BlockId
   -> ModelDBPure
   -> StateMachine Model (At CmdErr) m (At Resp)
sm hasFS db dbm vdb = StateMachine {
        initModel     = initModelImpl dbm vdb
      , transition    = transitionImpl
      , precondition  = preconditionImpl
      , postcondition = postconditionImpl
      , generator     = generatorImpl False
      , shrinker      = shrinkerImpl
      , semantics     = semanticsImpl hasFS db
      , mock          = mockImpl
      , invariant     = Nothing
      , distribution  = Nothing
    }

smErr :: (MonadCatch m, MonadSTM m)
      => TVar m Errors
      -> HasFS m h
      -> VolatileDB BlockId m
      -> DBModel BlockId
      -> ModelDBPure
      -> StateMachine Model (At CmdErr) m (At Resp)
smErr errorsVar hasFS db dbm vdb = StateMachine {
     initModel     = initModelImpl dbm vdb
   , transition    = transitionImpl
   , precondition  = preconditionImpl
   , postcondition = postconditionImpl
   , generator     = generatorImpl True
   , shrinker      = shrinkerImpl
   , semantics     = semanticsImplErr errorsVar hasFS db
   , mock          = mockImpl
   , invariant     = Nothing
   , distribution  = Nothing
 }

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
        _ -> Top
  where
      corruptionFiles = map snd . NE.toList

postconditionImpl :: Model Concrete -> At CmdErr Concrete -> At Resp Concrete -> Logic
postconditionImpl model cmdErr resp =
    toMock (eventAfter ev) resp .== eventMockResp ev
  where
    ev = lockstep model cmdErr resp

generatorCmdImpl :: Model Symbolic -> Maybe (Gen (At Cmd Symbolic))
generatorCmdImpl m@Model {..} =
    if shouldEnd then Nothing else Just $ do
    sl <- blockIdgenerator m
    let lastGC = latestGarbaged dbModel
    let dbFiles :: [String] = getDBFiles dbModel
    ls <- filter (newer lastGC . toSlot) <$> (listOf $ blockIdgenerator m)
    cmd <- frequency
        [ (15, return $ GetBlock sl)
        , (15, return $ PutBlock sl)
        , (5, return $ GarbageCollect sl)
        , (5, return $ IsOpen)
        , (5, return $ Close)
        -- When the db is Closed, we try to ReOpen it asap.
        -- This helps minimize TagClosedError and create more
        -- interesting tests.
        , (if open dbModel then 1 else 100, return $ ReOpen)
        , (if null ls then 0 else 3, return $ AskIfMember ls)
        , (if null dbFiles then 0 else 3, Corrupt <$> generateCorruptions (NE.fromList dbFiles))
        ]
    return $ At cmd

generatorImpl :: Bool -> Model Symbolic -> Maybe (Gen (At CmdErr Symbolic))
generatorImpl mkErr m@Model {..} = do
    genCmd <- generatorCmdImpl m
    Just $ do
        At cmd <- genCmd
        err' <- if noErrorFor cmd then return Nothing
           else frequency
                [ (4, return Nothing)
                , (if mkErr then 1 else 0, Just <$> arbitrary)]
        let err = erasePutCorruptions err'
        return $ At $ CmdErr cmd err
    where
        eraseCorruptions str = (\(fsErr, _) -> (fsErr, Nothing)) <$> str
        erasePutCorruptions mErr = do
            err <- mErr
            return err {_hPut = eraseCorruptions $ _hPut err}
        noErrorFor GetBlock {}       = False
        noErrorFor ReOpen {}         = False
        noErrorFor IsOpen {}         = False
        noErrorFor Close {}          = False
        noErrorFor AskIfMember {}    = False
        noErrorFor GarbageCollect {} = True
        noErrorFor PutBlock {}       = False
        noErrorFor Corrupt {}        = True

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

semanticsImpl :: MonadCatch m => HasFS m h -> VolatileDB BlockId m -> At CmdErr Concrete -> m (At Resp Concrete)
semanticsImpl hasFS m (At (CmdErr cmd _)) = At . Resp <$> tryVolDB (runDB (semanticsCorruption hasFS) m cmd)

semanticsImplErr :: (MonadCatch m, MonadSTM m)
                 => TVar m Errors
                 -> HasFS m h
                 -> VolatileDB BlockId m
                 -> At CmdErr Concrete
                 -> m (At Resp Concrete)
semanticsImplErr errorsVar hasFS m (At cmderr) = At . Resp <$> case cmderr of
    CmdErr cmd Nothing ->
        tryVolDB (runDB (semanticsCorruption hasFS) m cmd)
    CmdErr cmd (Just errors) -> do
        res <- withErrors errorsVar errors $
            tryVolDB (runDB (semanticsCorruption hasFS) m cmd)
        case res of
            Left ClosedDBError -> return res
            _                  -> do
                closeDB m
                -- return res
                return $ Right $ SimulatedError res

semanticsCorruption :: MonadCatch m
                    => HasFS m h
                    -> VolatileDB BlockId m
                    -> Corruptions
                    -> m Success
semanticsCorruption hasFS db corrs = do
    closeDB db
    forM_ corrs $ \(corr,file) -> corruptFile hasFS corr file
    reOpenDB db
    return $ Unit ()

mockImpl :: Model Symbolic -> At CmdErr Symbolic -> GenSym (At Resp Symbolic)
mockImpl model cmdErr = At <$> return mockResp
    where
        (mockResp, _dbModel') = step model cmdErr

knownLimitation :: Model Symbolic -> Cmd :@ Symbolic -> Logic
knownLimitation model (At cmd) = case cmd of
    GetBlock bid       -> Boolean $ isLimitation (latestGarbaged $ dbModel model) (toSlot bid)
    PutBlock bid       -> Boolean $ isLimitation (latestGarbaged $ dbModel model) (toSlot bid)
    GarbageCollect _sl -> Bot
    IsOpen             -> Bot
    Close              -> Bot
    ReOpen             -> Bot
    Corrupt _          -> Bot
    AskIfMember bids   -> exists ((\b -> isLimitation (latestGarbaged $ dbModel model) (toSlot b)) <$> bids) Boolean
    where
        isLimitation :: (Ord slot) => Maybe slot -> slot -> Bool
        isLimitation Nothing _sl       = False
        isLimitation (Just slot') slot = slot' >  slot

mkDBModel :: MonadState (DBModel BlockId, Maybe Errors) m
          => Int
          -> (BlockId -> Slot)
          -> Bool
          -> (DBModel BlockId, VolatileDB BlockId (ExceptT (VolatileDBError BlockId) m))
mkDBModel = openDBModel EH.exceptT

-- | Predicate on events
type EventPred = C.Predicate (Event Symbolic) Tag

-- | Convenience combinator for creating classifiers for successful commands
successful :: (    Event Symbolic
                -> Success
                -> Either Tag EventPred
              )
           -> EventPred
successful f = C.predicate $ \ev -> case eventMockResp ev of
    Resp (Left  _ ) -> Right $ successful f
    Resp (Right ok) -> f ev ok

-- | Tag commands
--
-- Tagging works on symbolic events, so that we can tag without doing real IO.
-- TODO(kde) This needs extending as part of #251
tag :: [Event Symbolic] -> [Tag]
tag [] = [TagEmpty]
tag ls = C.classify
    [ tagGetBinaryBlobNothing
    , tagGetJust $ Left TagGetJust
    , tagJustReOpenJust
    , tagReOpenJust
    , tagGarbageCollect Nothing False
    , tagCorruptWriteFile
    , tagAppendRecover
    , tagIsClosedError
    ] ls
  where

    tagGetBinaryBlobNothing :: EventPred
    tagGetBinaryBlobNothing = successful $ \ev r -> case r of
        Blob Nothing | GetBlock {} <- unAt $ eventCmd ev ->
            Left TagGetNothing
        _ -> Right tagGetBinaryBlobNothing

    tagReOpenJust :: EventPred
    tagReOpenJust = tagGetJust $ Right $ tagGetJust $ Left TagReOpenGet

    tagJustReOpenJust :: EventPred
    tagJustReOpenJust = tagGetJust $ Right $ reOpen $ Right $ tagGetJust $ Left TagGetReOpenGet

    tagGarbageCollect :: Maybe BlockId -> Bool -> EventPred
    tagGarbageCollect msl bl = C.predicate $ \ev -> case (msl, bl, eventMockResp ev, eventCmd ev) of
        (Nothing, False, Resp (Right (Blob (Just _))), At (GetBlock bid))
            -> Right $ tagGarbageCollect (Just bid) False
        (Just bid, False, Resp (Right (Unit ())), At (GarbageCollect bid'))
            -> Right $ tagGarbageCollect (Just bid) (toSlot bid < toSlot bid')
        (Just bid, True, Resp (Right (Blob Nothing)), At (GetBlock bid'))
            -> if bid == bid' then Left TagGarbageCollect
                              else Right $ tagGarbageCollect msl bl
        _ -> Right $ tagGarbageCollect msl bl

    tagGetJust :: Either Tag EventPred -> EventPred
    tagGetJust next = C.predicate $ \ev -> case eventMockResp ev of
        Resp (Right (Blob (Just _))) -> next
        _                            -> Right $ tagGetJust next

    reOpen :: Either Tag EventPred -> EventPred
    reOpen next = C.predicate $ \ev -> case (unAt $ eventCmd ev, eventMockResp ev) of
        (ReOpen, Resp (Right (Unit ()))) -> next
        _                                -> Right $ reOpen next

    tagCorruptWriteFile :: EventPred
    tagCorruptWriteFile = C.predicate $ \ev -> case unAt (eventCmd ev) of
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
        in  C.predicate $ \ev -> case unAt (eventCmd ev) of
            Corrupt cors -> if any (\(cor,_file) -> doesAppend cor) (NE.toList cors)
                            then Left TagAppendRecover
                            else Right tagAppendRecover
            _            -> Right tagAppendRecover

    tagIsClosedError :: EventPred
    tagIsClosedError = C.predicate $ \ev -> case eventMockResp ev of
        Resp (Left ClosedDBError) -> Left TagClosedError
        _                         -> Right tagIsClosedError

tagParser :: [Event Symbolic] -> String
tagParser [] = "TagEmpty"
tagParser ls = if shouldEnd (eventAfter (last ls))
               then "TagParseErrorEnd"
               else "TagNormalEnd"


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

data Tag = TagGetJust
         | TagGetNothing
         | TagGetReOpenGet
         | TagReOpenGet
         | TagGarbageCollect
         | TagCorruptWriteFile
         | TagAppendRecover
         | TagEmpty
         | TagClosedError
         deriving Show

prop_sequential :: Property
prop_sequential =
    forAllCommands smUnused Nothing $ \cmds -> monadicIO $ do
        let test :: HasFS IO h -> PropertyM IO (History (At CmdErr) (At Resp), Reason)
            test hasFS = do
              db <- run $ openDB hasFS EH.monadCatch myParser 7 toSlot True LenientFillOnlyLast
              let sm' = sm hasFS db dbm vdb
              (hist, _model, res) <- runCommands sm' cmds
              run $ closeDB db
              return (hist, res)
        fsVar <- run $ atomically (newTVar Mock.empty)
        (hist, res) <- test (simHasFS EH.monadCatch fsVar)
        let events = execCmds (initModel smUnused) cmds
        let myshow n = if n<5 then show n else if n < 20 then "5-19" else if n < 100 then "20-99" else ">=100"
        prettyCommands smUnused hist
            $ tabulate "Tags" (map show $ tag events)
            $ tabulate "Commands" (cmdName . eventCmd <$> events)
            $ tabulate "IsMember: Total number of True's" [myshow $ isMemberTrue events]
            $ tabulate "IsMember: At least one True" [show $ isMemberTrue' events]
            $ res === Ok
    where
        -- we use that: MonadState (DBModel BlockId) (State (DBModel BlockId))
        (dbm, vdb) = mkDBModel 7 toSlot True
        smUnused = sm (error "hasFS used during command generation") (error "semantics and DB used during command generation") dbm vdb

prop_sequential_strict_parser :: Property
prop_sequential_strict_parser =
    forAllCommands smUnused Nothing $ \cmds -> monadicIO $ do
        let test :: HasFS IO h
                 -> PropertyM IO (History (At CmdErr) (At Resp), Reason)
            test hasFS = do
                db <- run $ openDB hasFS EH.monadCatch myParser 7 toSlot False LenientFillOnlyLast
                let sm' = sm hasFS db dbm vdb
                (hist, _model, res) <- runCommands sm' cmds
                run $ closeDB db
                return (hist, res)
        fsVar <- run $ atomically (newTVar Mock.empty)
        (hist, res) <- test (simHasFS EH.monadCatch fsVar)
        let events = execCmds (initModel smUnused) cmds
        prettyCommands smUnused hist
            $ tabulate "Tags" [tagParser events]
            $ res === Ok
    where
        -- we use that: MonadState (DBModel BlockId) (State (DBModel BlockId))
        (dbm, vdb) = mkDBModel 7 toSlot False
        smUnused = sm (error "hasFS used during command generation") (error "semantics and DB used during command generation") dbm vdb

prop_sequential_errors :: Property
prop_sequential_errors = withMaxSuccess 50000 $
    forAllCommands smUnused Nothing $ \cmds -> monadicIO $ do
        let test :: TVar IO Errors
                 -> HasFS IO h
                 -> PropertyM IO (History (At CmdErr) (At Resp), Reason)
            test errorsVar hasFS = do
              db <- run $ openDB hasFS EH.monadCatch myParser 7 toSlot True LenientFillOnlyLast
              let sm' = smErr errorsVar hasFS db dbm vdb
              (hist, _model, res) <- runCommands sm' cmds
              run $ closeDB db
              return (hist, res)
        errorsVar <- run $ atomically (newTVar mempty)
        fsVar <- run $ atomically (newTVar Mock.empty)
        (hist, res) <- test errorsVar (mkSimErrorHasFS EH.monadCatch fsVar errorsVar)
        let events = execCmds (initModel smUnused) cmds
        prettyCommands smUnused hist
            $ res === Ok
    where
        -- we use that: MonadState (DBModel BlockId) (State (DBModel BlockId))
        (dbm, vdb) = mkDBModel 7 toSlot True
        smUnused = smErr (error "errorsVar unused") (error "hasFS used during command generation") (error "semantics and DB used during command generation") dbm vdb


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


tests :: TestTree
tests = testGroup "VolatileDB" [
      testProperty "q-s-m" $ prop_sequential
    , testProperty "q-s-m-Parser" $ prop_sequential_strict_parser
    , testProperty "q-s-m-Errors" $ prop_sequential_errors
    ]
