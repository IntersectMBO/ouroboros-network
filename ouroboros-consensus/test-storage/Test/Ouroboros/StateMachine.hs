{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Test.Ouroboros.StateMachine (tests) where

import           Prelude hiding (elem)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BL
import           Data.Kind (Type)
import qualified Data.Map as Map
import           Data.Map (Map, (!))
import           Data.Set (fromList)
import           Data.TreeDiff (ToExpr, defaultExprViaShow)
import           Data.TreeDiff.Class
import           Data.Word (Word64)
import           Control.Monad.State.Lazy
import           Control.Monad.Except
import           GHC.Generics (Generic, Generic1)
import           GHC.Stack
import           System.IO (IOMode (..), SeekMode (..))
import qualified System.Directory as Dir

import           Test.Tasty
                   (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck (arbitrary, Gen, Property, (===),
                     choose, elements, frequency, sublistOf, suchThat)
import           Test.QuickCheck.Monadic (monadicIO, run)
import           Test.StateMachine
import qualified Test.StateMachine.Types.Rank2 as Rank2

import           Ouroboros.Storage.FS.Class
import           Ouroboros.Storage.FS.IO
import           Ouroboros.Storage.FS.Sim


-- Fd here is assigned by the Model for each new file. It saves us from
-- implementing Eq instances on OpenFile which needs using pointer equality.
type Fd = Int

-- Our wraper around an open file. When we run with r = Concrete, the handle can
-- be accessed using @opaque@.
--
-- Opaque adds a Show instance to sth that does not have.
-- Reference is used so that we are able to use symbolic references while generating.
type OpenFile r = Reference (Opaque (FsHandle IOFSE)) r

data Model (r :: Type -> Type) = Model {
          mockFs    :: MockFS -- The FS tree.
        , counter   :: Int    -- Number steps that have been taken from the initial model state.
        , nextFd    :: Fd     -- This is increased every time a new file is opened.
        , openFiles :: Map Fd (MockHandle2, OpenFile r) -- The open files.
        } deriving (Show, Generic)

instance ToExpr MockFS
instance ToExpr (FsTree ByteString)
instance ToExpr (Model Symbolic)
instance ToExpr (Model Concrete)
instance ToExpr IOMode where
    toExpr = defaultExprViaShow
instance ToExpr MockHandle2

data Command (r :: Type -> Type)
    = Open                      FsPath IOMode
    | Close                     (Fd, OpenFile r)
    | Seek                      (Fd, OpenFile r) SeekMode Word64
    | Get                       (Fd, OpenFile r) Int
    | Put                       (Fd, OpenFile r) BL.ByteString
    | CreateDirectory           FsPath
    | CreateDirectoryIfMissing  Bool FsPath
    | ListDirectory             FsPath
    | DoesDirectoryExist        FsPath
    | DoesFileExist             FsPath
    | Dummy  -- A command which should always be  discarded from precondition.
      deriving (Generic1, Rank2.Foldable, Rank2.Traversable, Rank2.Functor)

deriving instance Show (Command Symbolic)
deriving instance Show (Command Concrete)

data Response (r :: Type -> Type)
  = OpenR                            (Either FsError (OpenFile r))
  | CloseR                           (Either FsError ())
  | SeekR                            (Either FsError Word64)
  | GetR                             (Either FsError ByteString)
  | PutR                             (Either FsError Word64)
  | CreateDirectoryR                 (Either FsError ())
  | CreateDirectoryIfMissingR        (Either FsError ())
  | ListDirectoryR                   (Either FsError [String])
  | DoesDirectoryExistR              (Either FsError Bool)
  | DoesFileExistR                   (Either FsError Bool)
  | DummyR
  deriving (Generic1, Rank2.Foldable)

deriving instance Show (Response Symbolic)
deriving instance Show (Response Concrete)

initModel :: Model r
initModel = Model newEmptyMockFS 0 0 Map.empty

-- In this Model we try to keep preconditions minimal. We handle failures,
-- at the form of Left in the postconditions.
preconditions :: Model Symbolic -> Command Symbolic -> Logic
preconditions (Model fs _ _ mp) cmd = case cmd of
    Open _ _ ->
        -- frequency of 'opens' reduces when openfds rises
        -- and becomes 0 when there are 200 files to avoid EMFILE.
        Map.size mp .< 100
    Seek (fd, _) seekMode offset ->
        if seekMode == SeekFromEnd && offset > 0
        then Bot .// "we only allow SeekFromEnd with 0 offset."
        else
            case Map.lookup fd mp of
                Nothing -> Bot .// "fd does not exist in open files Map."
                Just ((MockHandle2 fp _mode currentOffset), _) ->
                    case index fp (getMockFS fs) of
                        Nothing -> Top
                        Just (FolderOnDisk _) -> Top
                        Just (FileOnDisk block) ->
                            let offset' = case seekMode of
                                    AbsoluteSeek -> offset
                                    RelativeSeek -> currentOffset + offset
                                    SeekFromEnd  -> (toEnum $ BS.length block) + offset
                            in (toEnum $ BS.length block) .>= offset' -- we don't allow overflow.
    Get (fd, _) _ ->
        case Map.lookup fd mp of
            Nothing -> Bot .// "fd does not exist in open files Map."
            Just (_mockHandle, _) -> Top
    Put (fd, _) _ ->
        case Map.lookup fd mp of
            Nothing -> Bot .// "fd does not exist in open files Map."
            Just (_mockHandle, _) -> Top
    -- TODO(kde) until the bug is fixed. In IO this throws an exception when the path
    -- does not exist. In Sim it is a no op. Not sure which we want to follow.
    CreateDirectoryIfMissing b _path -> Boolean b
    Dummy -> Bot
    _ -> Top

postconditions :: Model Concrete -> Command Concrete -> Response Concrete -> Logic
postconditions (Model fs _ _nextFd mp) cmd resp = case (cmd,resp) of
    (Open fsPath ioMode, OpenR r1) ->
        let (r2, _) = runState (runExceptT $ mockOpen2 fsPath ioMode) fs
        in standardPostCondition allTop r1 r2
    (Close (fd, _), CloseR r1) ->
        let (r2, _) = runState (runExceptT $ mockClose2 $ fst $ mp Map.! fd) fs
        in standardPostCondition (.==) r1 r2
    (Seek (fd, _) seekMode w, SeekR r1) ->
        let (r2, _) = runState (runExceptT $ mockSeek2 (fst $ mp Map.! fd) seekMode w) fs
        in standardPostCondition (.==) r1 (fst <$> r2)
    (Get (fd, _) n, GetR r1) ->
        let (r2, _) = runState (runExceptT $ mockGet2 (fst $ mp Map.! fd) n) fs
        in standardPostCondition (.==) r1 (fst <$> r2)
    (Put (fd, _) bytes, PutR r1) ->
        let (r2, _) = runState (runExceptT $ mockPut2 (fst $ mp Map.! fd) (BL.lazyByteString bytes)) fs
        in standardPostCondition (.==) r1 (fst <$> r2)
    (CreateDirectory path , CreateDirectoryR r1) ->
        let (r2, _) = runState (runExceptT $ mockCreateDirectory2 path) fs
        in standardPostCondition (.==) r1 r2
    (CreateDirectoryIfMissing b path, CreateDirectoryIfMissingR r1) ->
        let (r2, _) = runState (runExceptT $ mockCreateDirectoryIfMissing2 b path) fs
        in standardPostCondition (.==) r1 r2
    (ListDirectory path , ListDirectoryR r1) ->
        let (r2, _) = runState (runExceptT $ mockListDirectory2 path) fs
        in standardPostCondition (\ls1 ls2 -> fromList ls1 .== fromList ls2) r1 r2 -- compare as Set.
    (DoesDirectoryExist path , DoesDirectoryExistR r1) ->
        let (r2, _) = runState (runExceptT $ mockDoesDirectoryExist2 path) fs
        in standardPostCondition (.==) r1 r2
    (DoesFileExist path , DoesFileExistR r1) ->
        let (r2, _) = runState (runExceptT $ mockDoesFileExist2 path) fs
        in standardPostCondition (.==) r1 r2
    (Dummy, DummyR) -> Bot
    _ -> Bot


standardPostCondition :: (a -> b -> Logic) -> Either FsError a -> Either FsError b -> Logic
standardPostCondition _ (Left e1) (Left e2) =
    if e1 `sameFsError` e2 then Top
    else Bot .// ("IO returned error " ++ show e1 ++ " while Sim returned " ++ show e2)
standardPostCondition _ (Left e1) (Right _) = Bot .// "IO returned " ++ (show e1)
standardPostCondition _ (Right _) (Left e2) = Bot .// "Sim returned " ++ (show e2)
standardPostCondition checkEq (Right a) (Right b) = checkEq a b

allTop :: a -> b -> Logic
allTop _ _ = Top

generator :: Model Symbolic -> Gen (Command Symbolic)
generator (Model _ _ _ mp) =
    let openfds = Map.size mp
        -- we want 0 probability when there are no open files.
        fClose = if openfds == 0 then 0 else 1 + (div (max (openfds) 0) 25)
        fFileExist = if openfds == 0 then 0 else 1
        genFD :: Gen (Fd, Reference (Opaque (FsHandle IOFSE)) Symbolic)
        genFD = do
            (fd :: Fd) <- elements $ Map.keys mp
            let x = snd (mp ! fd)
            pure (fd, x)
    in
        frequency
        [(3, Open <$> genFile <*> genMode)
        ,(fClose, Close <$> genFD)
        ,(fFileExist, Seek <$> genFD <*> genSeekMode <*> genOffset)
        ,(fFileExist, Get <$> genFD <*> suchThat arbitrary (>=0))
        ,(fFileExist, Put <$> genFD <*> (BL.pack <$> arbitrary))
        ,(1, CreateDirectory <$> genPath)
        ,(1, ListDirectory <$> genPath)
        ,(1, DoesDirectoryExist <$> genPath)
        ,(1, DoesFileExist <$> genPath)
        ,(1, return Dummy)
        ]

genPath :: Gen FsPath
genPath =
    let ls = ["xx", "yy", "zz", "ww"]
    in suchThat (sublistOf ls) (\x -> Prelude.length x > 0)

genFile :: Gen FsPath
genFile =
    let ls = [["aa"], ["bb"], ["cc"], ["dd"]]
    in elements ls

genMode :: Gen IOMode
genMode = elements [ReadMode, AppendMode, ReadWriteMode]

genSeekMode :: Gen SeekMode
genSeekMode = elements [AbsoluteSeek, RelativeSeek, SeekFromEnd]

-- Keeping the offset completely arbitrary, ends up in offset which gets
-- rejected by precondition almost always.
genOffset :: Gen Word64
genOffset = frequency
     [ (2, return 0)
     , (3, choose (1, 10))
     , (1, arbitrary)
     ]

shrinker :: Command r -> [Command r]
shrinker _ = []

semantics :: FilePath -> Command Concrete -> IO (Response Concrete)
semantics tmpDir cmd = do
    case cmd of
        Open fsPath ioMode ->
            let
                act = hOpen fsPath ioMode
                script :: IOFS (Either FsError (FsHandle IOFSE)) = runExceptT act
                io :: IO (Either FsError (FsHandle IOFSE)) = runIOFS script tmpDir
            in
                (OpenR . (fmap $ reference . Opaque)) <$> io
        Close (_, fshandle) ->
            let
                act = hClose (opaque fshandle)
                script = runExceptT act
                io = runIOFS script tmpDir
            in
                CloseR <$> io
        Seek (_, fshandle) seekMode sz ->
            let
                act = hSeek (opaque fshandle) seekMode sz
                script = runExceptT act
                io = runIOFS script tmpDir
            in
                SeekR <$> io
        Get (_, fshandle) n ->
            let
                act = hGet (opaque fshandle) n
                script = runExceptT act
                io = runIOFS script tmpDir
            in
                GetR <$> io
        Put (_, fshandle) bytes ->
            let
                act = hPut (opaque fshandle) (BL.lazyByteString bytes)
                script = runExceptT act
                io = runIOFS script tmpDir
            in
                PutR <$> io
        CreateDirectory path ->
            let
                act = createDirectory path
                script = runExceptT act
                io = runIOFS script tmpDir
            in
                CreateDirectoryR <$> io
        CreateDirectoryIfMissing b path ->
            let
                act = createDirectoryIfMissing b path
                script = runExceptT act
                io = runIOFS script tmpDir
            in
                CreateDirectoryIfMissingR <$> io
        ListDirectory path ->
            let
                act = listDirectory path
                script = runExceptT act
                io = runIOFS script tmpDir
            in
                ListDirectoryR <$> io
        DoesDirectoryExist path ->
            let
                act = doesDirectoryExist path
                script = runExceptT act
                io = runIOFS script tmpDir
            in
                DoesDirectoryExistR <$> io
        DoesFileExist path ->
            let
                act = doesFileExist path
                script = runExceptT act
                io = runIOFS script tmpDir
            in
                DoesFileExistR <$> io
        Dummy -> return DummyR

next :: Model r -> Model r
next m@Model{..} = m{ counter = counter + 1}

transitions :: Model r -> Command r -> Response r -> Model r
transitions m@(Model fs n nextFd mp) cmd resp = case (cmd,resp) of
    (Open fsPath ioMode, OpenR r1) ->
        let (r2, fs') = runState (runExceptT $ mockOpen2 fsPath ioMode) fs
        in case (r1,r2) of
            (Left _, Left _) -> next m
            (Right _, Left _)  -> error "Right-Left"
            (Left _, Right _)  -> error "Left-Right"
            (Right realHandle, Right simHandle) ->
                Model fs' (n + 1) (nextFd + 1) (Map.insert nextFd (simHandle, realHandle) mp)
    (Close (fd, _), CloseR r1) ->
        let Just handle = fst <$> Map.lookup fd mp
            (r2, fs') = runState (runExceptT $ mockClose2 handle) fs
        in standardTranstion r1 r2 m $ Model fs' (n + 1) nextFd (Map.delete fd mp)
    (Seek (fd, _) seekMode w, SeekR r1) ->
        let Just (handle, ref) = Map.lookup fd mp
            (r2, fs') = runState (runExceptT $ mockSeek2 handle seekMode w) fs
        in transitionWithHandle r1 r2 m
                $ \(_w, handle') -> Model fs' (n + 1) nextFd (Map.insert fd (handle', ref) mp)
    (Get (fd, _) size, GetR r1) ->
        let Just (handle, ref) = Map.lookup fd mp
            (r2, fs') = runState (runExceptT $ mockGet2 handle size) fs
        in transitionWithHandle r1 r2 m
                $ \(_bytes, handle') -> Model fs' (n + 1) nextFd (Map.insert fd (handle', ref) mp)
    (Put (fd, _) bytes, PutR r1) ->
        let Just (handle, ref) = Map.lookup fd mp
            (r2, fs') = runState (runExceptT $ mockPut2 handle (BL.lazyByteString bytes)) fs
        in transitionWithHandle r1 r2 m
                $ \(_w, handle') -> Model fs' (n + 1) nextFd (Map.insert fd (handle', ref) mp)
    (CreateDirectory path, CreateDirectoryR r1) ->
        let (r2, fs') = runState (runExceptT $ mockCreateDirectory2 path) fs
        in standardTranstion r1 r2 m $ Model fs' (n + 1) nextFd mp
    (CreateDirectoryIfMissing b path, CreateDirectoryIfMissingR r1) ->
        let (r2, fs') = runState (runExceptT $ mockCreateDirectoryIfMissing2 b path) fs
        in standardTranstion r1 r2 m $ Model fs' (n + 1) nextFd mp
    (ListDirectory path, ListDirectoryR r1) ->
        let (r2, fs') = runState (runExceptT $ mockListDirectory2 path) fs
        in standardTranstion r1 r2 m $ Model fs' (n + 1) nextFd mp
    (DoesDirectoryExist path, DoesDirectoryExistR r1) ->
        let (r2, fs') = runState (runExceptT $ mockDoesDirectoryExist2 path) fs
        in standardTranstion r1 r2 m $ Model fs' (n + 1) nextFd mp
    (DoesFileExist path, DoesFileExistR r1) ->
        let (r2, fs') = runState (runExceptT $ mockDoesFileExist2 path) fs
        in standardTranstion r1 r2 m $ Model fs' (n + 1) nextFd mp
    (Dummy, DummyR) -> next m
    _ -> error "transition impossible"

standardTranstion :: Either e a -> Either e b -> Model r -> Model r -> Model r
standardTranstion a b startingModel model' = case (a,b) of
    (Left _, Left _)   -> next startingModel
    (Right _, Left _)  -> error "Right-Left"
    (Left _, Right _)  -> error "Left-Right"
    (Right _, Right _) -> model'

-- the difference with the standard one is that in case of Right result, we
-- need the mockHandle to create the new Model.
transitionWithHandle :: Either e a -> Either e b -> Model r -> (b -> Model r ) -> Model r
transitionWithHandle a b startModel mkModel' = case (a,b) of
    (Left _, Left _)   -> next startModel
    (Right _, Left _)  -> error "Right-Left"
    (Left _, Right _)  -> error "Left-Right"
    (Right _, Right x) -> mkModel' x

mock :: Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock (Model fs n _nextFd mp) cmd = case cmd of
    Open fsPath ioMode ->
        let res = evalState (runExceptT $ mockOpen2 fsPath ioMode) fs
        in case res of
            Left e -> return $ OpenR $ Left e
            Right _ -> OpenR . Right <$> genSym
    Close (fd, _) ->
        let Just handle = fst <$> Map.lookup fd mp
            res = evalState (runExceptT $ mockClose2 handle) fs
        in return $ CloseR res
    Seek (fd, _) seekMode w ->
        let Just handle = fst <$> Map.lookup fd mp
            res = evalState (runExceptT $ mockSeek2 handle seekMode w) fs
        in return $ SeekR (fst <$> res)
    Get (fd, _) size ->
        let Just handle = fst <$> Map.lookup fd mp
            res = evalState (runExceptT $ mockGet2 handle size) fs
        in return $ GetR (fst <$> res)
    Put (fd, _) bytes ->
        let Just handle = fst <$> Map.lookup fd mp
            res = evalState (runExceptT $ mockPut2 handle $ BL.lazyByteString bytes) fs
        in return $ PutR (fst <$> res)
    CreateDirectory path ->
        let res = evalState (runExceptT $ mockCreateDirectory2 path) fs
        in return $ CreateDirectoryR res
    CreateDirectoryIfMissing b path ->
        let res = evalState (runExceptT $ mockCreateDirectoryIfMissing2 b path) fs
        in return $ CreateDirectoryIfMissingR res
    ListDirectory path ->
        let res = evalState (runExceptT $ mockListDirectory2 path) fs
        in return $ ListDirectoryR res
    DoesDirectoryExist path ->
        let res = evalState (runExceptT $ mockDoesDirectoryExist2 path) fs
        in return $ DoesDirectoryExistR res
    DoesFileExist path ->
        let res = evalState (runExceptT $ mockDoesFileExist2 path) fs
        in return $ DoesFileExistR res
    Dummy -> return DummyR

sm :: FilePath -> StateMachine Model Command IO Response
sm tmpDir = StateMachine initModel transitions preconditions postconditions
       Nothing generator Nothing shrinker (semantics tmpDir) mock


newtype HideProperty = HideProperty {unProperty :: Property}

-- we keep this hidden here to avoid using it easily with quickcheck
-- from ghci, since it would write on a random file path.
propertyImmutableStorage :: FilePath -> HideProperty
propertyImmutableStorage tmpDir = HideProperty $ do
    forAllCommands (sm tmpDir) Nothing $ \cmds -> monadicIO $ do
        ls <- run $ Dir.listDirectory tmpDir
        let lsn :: [Int] = read <$> ls
            newn = 1 + Prelude.foldl max 0 lsn
            tmpDir' = tmpDir ++ "/" ++ show newn
        run $ Dir.createDirectory tmpDir'
        -- run $ putStrLn $ "Test On " ++ show newn
        -- run $ print cmds
        (hist, model, res) <- runCommands (sm tmpDir') cmds
        prettyCommands (sm tmpDir') hist $
            checkCommandNames cmds (res === Ok)
        run $ garbageCollect model

-- That's important to do after each test, or we may end up with too
-- many open files. This can result in us getting a EMFILE. Depending
-- on the GC to close these fd's would be a bad idea.
garbageCollect :: Model Concrete -> IO ()
garbageCollect (Model _ _ _ mp) = do
    let f :: (MockHandle2, Reference (Opaque (FsHandle IOFSE)) Concrete)
          -> IO (Either FsError ())
        f (_, ref) = do
            let handle = opaque ref
            -- we provide a dummy fs, since closing files does not use them.
            runIOFS (runExceptT $ hClose handle) undefined
    forM_ (Map.elems mp) f


tests :: HasCallStack => FilePath -> TestTree
tests tmpDir = testGroup "Immutable State Machine Tests"
    [ testProperty "" (unProperty $ propertyImmutableStorage tmpDir)]

