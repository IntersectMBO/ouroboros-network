{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Ouroboros.Storage.FS.StateMachine (
    tests
  , showLabelledExamples
  ) where

import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.Except (runExcept)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bifoldable
import           Data.Bifunctor
import qualified Data.Bifunctor.TH as TH
import           Data.Bitraversable
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Classes
import           Data.Int (Int64)
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.TreeDiff (ToExpr)
import           Data.Word (Word64)
import qualified Generics.SOP as SOP
import           GHC.Generics
import           GHC.Stack
import           System.Directory (removeDirectoryRecursive)
import           System.IO (IOMode, SeekMode)
import qualified System.IO as IO
import           System.IO.Temp (createTempDirectory)
import           System.Random (getStdRandom, randomR)
import           Text.Read (readMaybe)
import           Text.Show.Pretty (ppShow)

import           Test.QuickCheck
import           Test.QuickCheck.Monadic (monadicIO)
import           Test.QuickCheck.Random (mkQCGen)
import           Test.StateMachine (Concrete, Symbolic)
import qualified Test.StateMachine as QSM
import qualified Test.StateMachine.Sequential as QSM
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.IO
import           Ouroboros.Storage.FS.Sim.FsTree (FsTree (..))
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.Pure
import qualified Ouroboros.Storage.IO as F
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import qualified Ouroboros.Consensus.Util.Classify as C
import           Ouroboros.Consensus.Util.Condense

import           Test.Ouroboros.Storage.Util (collects)

import           Test.Util.RefEnv (RefEnv)
import qualified Test.Util.RefEnv as RE

{-------------------------------------------------------------------------------
  Path expressions
-------------------------------------------------------------------------------}

data PathExpr fp =
    PExpPath     FsPath
  | PExpRef      fp
  | PExpParentOf fp
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

evalPathExpr :: PathExpr FsPath -> FsPath
evalPathExpr (PExpPath     fp) = fp
evalPathExpr (PExpRef      fp) = fp
evalPathExpr (PExpParentOf fp) = init fp

{-------------------------------------------------------------------------------
  Abstract model
-------------------------------------------------------------------------------}

-- | Commands
--
-- We will be interested in three different instantiations of @h@:
--
-- > Cmd Mock.Handle
-- > Cmd (Reference (Opaque (FsHandle IOFSE)) Concrete)
-- > Cmd (Reference (Opaque (FsHandle IOFSE)) Symbolic)
--
-- Key idea is that all this infrastructure will be applicable both to
-- the model and to the system under test.
--
-- TODO: Program such as "copy what you read" is currently not expressible
-- in our language. Does this matter?
data Cmd fp h =
    Open               (PathExpr fp) IOMode
  | Close              h
  | Seek               h SeekMode Int64
  | Get                h Int
  | Put                h BL.ByteString
  | Truncate           h Word64
  | GetSize            h
  | CreateDir          (PathExpr fp)
  | CreateDirIfMissing Bool (PathExpr fp)
  | ListDirectory      (PathExpr fp)
  | DoesDirectoryExist (PathExpr fp)
  | DoesFileExist      (PathExpr fp)
  | RemoveFile         (PathExpr fp)
  deriving (Generic, Show, Functor, Foldable, Traversable)

deriving instance SOP.Generic         (Cmd fp h)
deriving instance SOP.HasDatatypeInfo (Cmd fp h)

-- | Successful result
data Success fp h =
    WHandle    fp h
  | RHandle    h
  | Unit       ()
  | Path       fp ()
  | Word64     Word64
  | ByteString BL.ByteString
  | Strings    (Set String)
  | Bool       Bool
  deriving (Eq, Show, Functor, Foldable)

-- | Successful semantics
run :: forall m h. Monad m
    => HasFS m h
    -> Cmd FsPath h
    -> m (Success FsPath h)
run hasFS@HasFS{..} = go
  where
    go :: Cmd FsPath h -> m (Success FsPath h)
    go (Open pe mode) =
        case mode of
          IO.ReadMode -> withPE pe (\_ -> RHandle) $ \fp -> hOpen fp mode
          _otherwise  -> withPE pe WHandle         $ \fp -> hOpen fp mode

    go (CreateDir            pe) = withPE pe Path   $ createDirectory
    go (CreateDirIfMissing b pe) = withPE pe Path   $ createDirectoryIfMissing b
    go (Close    h             ) = Unit       <$> hClose    h
    go (Seek     h mode sz     ) = Unit       <$> hSeek     h mode sz
    -- Note: we're not using 'hGetSome' and 'hPutSome' that may produce
    -- partial reads/writes, but wrappers around them that handle partial
    -- reads/writes, see #502.
    go (Get      h n           ) = ByteString <$> hGetExactly hasFS h n
    go (Put      h bs          ) = Word64     <$> hPutAll     hasFS h bs
    go (Truncate h sz          ) = Unit       <$> hTruncate h sz
    go (GetSize  h             ) = Word64     <$> hGetSize  h
    go (ListDirectory      pe  ) = withPE pe (const Strings) $ listDirectory
    go (DoesDirectoryExist pe  ) = withPE pe (const Bool)    $ doesDirectoryExist
    go (DoesFileExist      pe  ) = withPE pe (const Bool)    $ doesFileExist
    go (RemoveFile         pe  ) = withPE pe (const Unit)    $ removeFile

    withPE :: PathExpr FsPath
           -> (FsPath -> a -> Success FsPath h)
           -> (FsPath -> m a)
           -> m (Success FsPath h)
    withPE pe r f = let fp = evalPathExpr pe in r fp <$> f fp

{-------------------------------------------------------------------------------
  Instantiating the semantics
-------------------------------------------------------------------------------}

-- | Responses are either successful termination or an error
newtype Resp fp h = Resp { getResp :: Either FsError (Success fp h) }
  deriving (Show, Functor, Foldable)

-- | The 'Eq' instance for 'Resp' uses 'sameFsError'
instance (Eq fp, Eq h) => Eq (Resp fp h) where
  -- TODO(532) True should be reverted to sameFsError.
  Resp (Left  _) == Resp (Left  _)  = True
  Resp (Right a) == Resp (Right a') = a == a'
  _              == _               = False

runPure :: Cmd FsPath Mock.Handle -> MockFS -> (Resp FsPath Mock.Handle, MockFS)
runPure cmd mockFS =
    aux $ runExcept $ runPureSimFS (run (pureHasFS EH.exceptT) cmd) mockFS
  where
    aux :: Either FsError (Success FsPath Mock.Handle, MockFS)
        -> (Resp FsPath Mock.Handle, MockFS)
    aux (Left e)             = (Resp (Left e), mockFS)
    aux (Right (r, mockFS')) = (Resp (Right r), mockFS')

runIO :: MountPoint -> Cmd FsPath HandleIO -> IO (Resp FsPath HandleIO)
runIO mount cmd = Resp <$> E.try (run (ioHasFS mount) cmd)

{-------------------------------------------------------------------------------
  Bitraversable instances
-------------------------------------------------------------------------------}

TH.deriveBifunctor     ''Cmd
TH.deriveBifoldable    ''Cmd
TH.deriveBitraversable ''Cmd

TH.deriveBifunctor     ''Success
TH.deriveBifoldable    ''Success
TH.deriveBitraversable ''Success

TH.deriveBifunctor     ''Resp
TH.deriveBifoldable    ''Resp
TH.deriveBitraversable ''Resp

{-------------------------------------------------------------------------------
  Collect arguments
-------------------------------------------------------------------------------}

paths :: Bitraversable t => t fp h -> [fp]
paths = bifoldMap (:[]) (const [])

handles :: Bitraversable t => t fp h -> [h]
handles = bifoldMap (const []) (:[])

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

-- | Concrete or symbolic reference to a path
type PathRef = QSM.Reference FsPath

-- | Concrete or symbolic reference to an IO file handle
type HandleRef = QSM.Reference (QSM.Opaque HandleIO)

-- | Mapping between real IO file handles and mock file handles
type KnownHandles = RefEnv (QSM.Opaque HandleIO) Mock.Handle

-- | Mapping between path references and paths
type KnownPaths = RefEnv FsPath FsPath

resolvePathExpr :: Eq1 r => KnownPaths r -> PathExpr (PathRef r) -> FsPath
resolvePathExpr knownPaths = evalPathExpr . fmap (knownPaths RE.!)

-- | Execution model
data Model r = Model {
      mockFS       :: MockFS
    , knownPaths   :: KnownPaths   r
    , knownHandles :: KnownHandles r
    }
  deriving (Show, Generic)

-- | Initial model
initModel :: Model r
initModel = Model Mock.empty RE.empty RE.empty

-- | Key property of the model is that we can go from real to mock responses
toMock :: (Bifunctor t, Eq1 r) => Model r -> t :@ r -> t FsPath Mock.Handle
toMock Model{..} (At r) = bimap (knownPaths RE.!) (knownHandles RE.!) r

-- | Step the mock semantics
--
-- We cannot step the whole Model here (see 'event', below)
step :: Eq1 r => Model r -> Cmd :@ r -> (Resp FsPath Mock.Handle, MockFS)
step model@Model{..} cmd = runPure (toMock model cmd) mockFS

-- | Pair a handle with the path it points to
resolveHandle :: MockFS -> Mock.Handle -> (Mock.Handle, FsPath)
resolveHandle mockFS h = (h, Mock.handleFsPath mockFS h)

-- | Open read handles and the files they point to
openHandles :: Model r -> [(Mock.Handle, FsPath)]
openHandles Model{..} =
    map (resolveHandle mockFS) $ filter isOpen (RE.elems knownHandles)
  where
    isOpen :: Mock.Handle -> Bool
    isOpen h = isJust $ Mock.handleIOMode mockFS h

{-------------------------------------------------------------------------------
  Wrapping in quickcheck-state-machine references
-------------------------------------------------------------------------------}

-- | Instantiate functor @f@ to @f (PathRef r) (HRef r)@
--
-- > Cmd :@ Concrete ~ Cmd (PathRef Concrete) (HandleRef Concrete)
newtype At t r = At (t (PathRef r) (HandleRef r))
  deriving (Generic)

-- | Alias for 'At'
type (:@) t r = At t r

deriving instance Show1 r => Show (Cmd  :@ r)
deriving instance Show1 r => Show (Resp :@ r)
deriving instance Eq1   r => Eq   (Resp :@ r)

instance Bifoldable t => Rank2.Foldable (At t) where
  foldMap = \f (At x) -> bifoldMap (app f) (app f) x
    where
      app :: (r x -> m) -> QSM.Reference x r -> m
      app f (QSM.Reference x) = f x

instance Bifunctor t => Rank2.Functor (At t) where
  fmap = \f (At x) -> At (bimap (app f) (app f) x)
    where
      app :: (r x -> r' x) -> QSM.Reference x r -> QSM.Reference x r'
      app f (QSM.Reference x) = QSM.Reference (f x)

instance Bitraversable t => Rank2.Traversable (At t) where
  traverse = \f (At x) -> At <$> bitraverse (app f) (app f) x
    where
      app :: Functor f
          => (r x -> f (r' x)) -> QSM.Reference x r -> f (QSM.Reference x r')
      app f (QSM.Reference x) = QSM.Reference <$> f x

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

-- | An event records the model before and after a command along with the
-- command itself and its response
data Event r = Event {
      eventBefore   :: Model  r
    , eventCmd      :: Cmd :@ r
    , eventAfter    :: Model  r
    , eventMockResp :: Resp FsPath Mock.Handle
    }
  deriving (Show)

eventMockCmd :: Eq1 r => Event r -> Cmd FsPath Mock.Handle
eventMockCmd Event{..} = toMock eventBefore eventCmd

-- | Bundle handles with the paths they refer to
resolveCmd :: Eq1 r => Event r -> Cmd FsPath (Mock.Handle, FsPath)
resolveCmd ev@Event{..} = resolveHandle (mockFS eventBefore) <$> eventMockCmd ev

-- | Construct an event
--
-- When we execute both the model and the real implementation in lockstep,
-- we get two responses: this suffices to update the model.
lockstep :: forall r. (Show1 r, Ord1 r, HasCallStack)
         => Model r
         -> Cmd :@ r
         -> Resp :@ r
         -> Event r
lockstep model@Model{..} cmd (At resp) = Event {
      eventBefore   = model
    , eventCmd      = cmd
    , eventAfter    = Model {
                          mockFS       = mockFS'
                        , knownPaths   = knownPaths   `RE.union` newPaths
                        , knownHandles = knownHandles `RE.union` newHandles
                        }
    , eventMockResp = resp'
    }
  where
    (resp', mockFS') = step model cmd
    newPaths   = RE.fromList $ zip (paths   resp) (paths   resp')
    newHandles = RE.fromList $ zip (handles resp) (handles resp')

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

generator :: Model Symbolic -> Gen (Cmd :@ Symbolic)
generator Model{..} = oneof $ concat [
      withoutHandle
    , if RE.null knownHandles then [] else withHandle
    ]
  where
    withoutHandle :: [Gen (Cmd :@ Symbolic)]
    withoutHandle = [
          fmap At $ Open               <$> genPathExpr <*> genMode
        , fmap At $ CreateDir          <$> genPathExpr
        , fmap At $ CreateDirIfMissing <$> arbitrary <*> genPathExpr
        , fmap At $ ListDirectory      <$> genPathExpr
        , fmap At $ DoesDirectoryExist <$> genPathExpr
        , fmap At $ DoesFileExist      <$> genPathExpr
        , fmap At $ RemoveFile         <$> genPathExpr
        ]

    withHandle :: [Gen (Cmd :@ Symbolic)]
    withHandle = [
          fmap At $ Close    <$> genHandle
        , fmap At $ Seek     <$> genHandle <*> genSeekMode <*> genOffset
        , fmap At $ Get      <$> genHandle <*> (getNonNegative <$> arbitrary)
        , fmap At $ Put      <$> genHandle <*> (BL.pack <$> arbitrary)
        , fmap At $ Truncate <$> genHandle <*> (getSmall . getNonNegative <$> arbitrary)
        , fmap At $ GetSize  <$> genHandle
        ]

    -- Wrap path in a simple path expression
    -- (References are generated during shrinking only)
    genPathExpr :: Gen (PathExpr fp)
    genPathExpr = PExpPath <$> genPath

    -- We choose from a small list of names so that we reuse names often
    -- We use the same set of files and directories so that we can test
    -- things like trying to open a directory as if it were a file
    genPath :: Gen FsPath
    genPath = choose (0, 3) >>= \n -> replicateM n (elements ["x", "y", "z"])

    genHandle :: Gen (HandleRef Symbolic)
    genHandle = elements (RE.keys knownHandles)

    genMode :: Gen IOMode
    genMode = elements [
          IO.ReadMode
        , IO.WriteMode
        , IO.AppendMode
        , IO.ReadWriteMode
        ]

    genSeekMode :: Gen SeekMode
    genSeekMode = elements [
          IO.AbsoluteSeek
        , IO.RelativeSeek
        , IO.SeekFromEnd
        ]

    genOffset :: Gen Int64
    genOffset = oneof
         [ return 0
         , choose (1, 10)
         , choose (-1, -10)
         ]

{-------------------------------------------------------------------------------
  Temporary files (used in shrinking)
-------------------------------------------------------------------------------}

-- | Temp files are numbered from 1
newtype TempFile = TempFile Int
  deriving (Show)

instance Condense TempFile where -- basically GNTD
  condense (TempFile n) = condense n

tempToExpr :: TempFile -> PathExpr fp
tempToExpr (TempFile n) = PExpPath ['t' : show n]

tempFromPath :: FsPath -> Maybe TempFile
tempFromPath ['t' : suf] = do n <- readMaybe suf
                              guard (n >= 1)
                              return $ TempFile n
tempFromPath _otherwise  = Nothing

{-------------------------------------------------------------------------------
  Shrinking

  When we replace one reference with another, we are careful to impose an order
  so that we don't end up flipping between references. Since shrinking is greedy
  this does mean that the choice of reference may influence how much we can
  shrink later. This is hard to avoid in greedy algorithms.
-------------------------------------------------------------------------------}

shrinker :: Model Symbolic -> Cmd :@ Symbolic -> [Cmd :@ Symbolic]
shrinker Model{..} (At cmd) =
    case cmd of
      Open pe mode -> concat [
            case tempFromPath fp of
              Just n ->
                map (\n' -> At $ Open (tempToExpr n') mode)
                  $ shrinkTempFile n
              Nothing ->
                let mode' = case mode of
                             IO.ReadMode -> IO.ReadWriteMode
                             _otherwise  -> mode
                in [At $ Open (tempToExpr (TempFile numTempFiles)) mode']
          , case mode of
              IO.ReadWriteMode -> [
                  At $ Open pe IO.ReadMode
                , At $ Open pe IO.WriteMode
                ]
              _otherwise ->
                []
          , map (\pe' -> At $ Open pe' mode) $
              replaceWithRef pe (== fp) PExpRef
          ]
        where
          fp :: FsPath
          fp = resolvePathExpr knownPaths pe

      ListDirectory pe -> concat [
            map (At . ListDirectory) $
              replaceWithRef pe ((== fp) . init) PExpParentOf
          ]
        where
          fp :: FsPath
          fp = resolvePathExpr knownPaths pe

      Get      h n  -> At . Get      h <$> shrink n
      Put      h bs -> At . Put      h <$> shrinkBytes bs
      Truncate h n  -> At . Truncate h <$> shrink n

      _otherwise ->
          []
  where
    -- Replace path with reference
    --
    -- If we are replacing one reference with another, be careful to impose
    -- an ordering so that we don't end up toggling between references.
    replaceWithRef :: PathExpr (PathRef Symbolic)
                   -- current
                   -> (FsPath -> Bool)
                   -- evaluate candidate
                   -> (PathRef Symbolic -> PathExpr (PathRef Symbolic))
                   -- construct replacement
                   -> [PathExpr (PathRef Symbolic)]
    replaceWithRef pe p f =
        filter (canReplace pe) $ map f $ (RE.reverseLookup p knownPaths)
      where
        canReplace :: PathExpr (PathRef Symbolic)  -- current
                   -> PathExpr (PathRef Symbolic)  -- candidate
                   -> Bool
        canReplace (PExpRef      ref) (PExpRef      ref') = ref' < ref
        canReplace (PExpParentOf ref) (PExpParentOf ref') = ref' < ref
        canReplace _                  _                   = True

    shrinkTempFile :: TempFile -> [TempFile]
    shrinkTempFile (TempFile n) = TempFile . getPositive <$> shrink (Positive n)

    shrinkBytes :: BL.ByteString -> [BL.ByteString]
    shrinkBytes = map BL.pack . shrink . BL.unpack

    numTempFiles :: Int
    numTempFiles = 100

{-------------------------------------------------------------------------------
  Limitations/known bugs
-------------------------------------------------------------------------------}

-- | Known limitations/bugs that we don't want to test for
--
-- NOTE: Can assume all used handles are in known in the model.
knownLimitation :: Model Symbolic -> Cmd :@ Symbolic -> QSM.Logic
knownLimitation model cmd =
    case getResp resp of
      Left FsError{..} -> QSM.Boolean fsLimitation
      _otherwise       -> QSM.Bot
  where
    (resp, _mockFS') = step model cmd

{-------------------------------------------------------------------------------
  The final state machine
-------------------------------------------------------------------------------}

-- | Mock a response
--
-- We do this by running the pure semantics and then generating mock
-- references for any new handles.
mock :: Model               Symbolic
     -> Cmd              :@ Symbolic
     -> QSM.GenSym (Resp :@ Symbolic)
mock model cmd = At <$> bitraverse (const QSM.genSym) (const QSM.genSym) resp
  where
    (resp, _mockFS') = step model cmd

precondition :: Model Symbolic -> Cmd :@ Symbolic -> QSM.Logic
precondition m@Model{..} (At cmd) =
            QSM.forall (handles cmd) (`QSM.elem` RE.keys knownHandles)
    QSM.:&& QSM.Boolean (Mock.numOpenHandles mockFS < maxNumOpenHandles)
    QSM.:&& QSM.Not (knownLimitation m (At cmd))
  where
    -- Limit number of open handles to avoid exceeding OS limits
    maxNumOpenHandles = 100

-- | Step the model
--
-- NOTE: This function /must/ be polymorphic in @r@.
transition :: (Show1 r, Ord1 r) => Model r -> Cmd :@ r -> Resp :@ r -> Model r
transition model cmd = eventAfter . lockstep model cmd

postcondition :: Model   Concrete
              -> Cmd  :@ Concrete
              -> Resp :@ Concrete
              -> QSM.Logic
postcondition model cmd resp =
    toMock (eventAfter ev) resp QSM..== eventMockResp ev
  where
    ev = lockstep model cmd resp

semantics :: MountPoint -> Cmd :@ Concrete -> IO (Resp :@ Concrete)
semantics mount (At cmd) =
    At . bimap QSM.reference (QSM.reference . QSM.Opaque) <$>
      runIO mount (bimap QSM.concrete QSM.opaque cmd)

-- | The state machine proper
sm :: MountPoint -> QSM.StateMachine Model (At Cmd) IO (At Resp)
sm mount = QSM.StateMachine {
               initModel     = initModel
             , transition    = transition
             , precondition  = precondition
             , postcondition = postcondition
             , generator     = Just . generator
             , shrinker      = shrinker
             , semantics     = semantics mount
             , mock          = mock
             , invariant     = Nothing
             , distribution  = Nothing
             }

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

data Tag =
    -- | Create directory then list its parent
    --
    -- > CreateDir [x, .., y, z]
    -- > ListDirectory [x, .., y]
    TagCreateDirThenListDir

    -- | Create a directory with its parents, then list its parents
    --
    -- > CreateDirIfMissing True [x, .., y, z]
    -- > ListDirectory [x, .., y]
    --
    -- Note that this implies all directories must have been created.
  | TagCreateDirWithParentsThenListDir

    -- | Have a least N open files
    --
    -- > Open ..
    -- > .. --
    -- > Open ..
    --
    -- (with not too many Close calls in between).
  | TagAtLeastNOpenFiles Int

    -- | Write, then truncate, then write again
    --
    -- > Put ..
    -- > Truncate .. (deleting some but not all of the bytes already written)
    -- > Put         (write some different bytes)
    --
    -- Verifies that we correctly modify the file pointer.
  | TagPutTruncatePut

    -- | Concurrent writer and reader
    --
    -- > h1 <- Open fp WriteMode ..
    -- > h2 <- Open fp ReadMode ..
    -- > Put h1 ..
    -- > Get h2 ..
  | TagConcurrentWriterReader

    -- | Writing many times should append the bytes.
    --
    -- > h1 <- Open fp WriteMode ..   |    > h2 <- Open fp ReadMode ..
    -- > Put h1 ..                    |
    -- > Put h1 ..                    |

    -- > Get h2 ..
  | TagWriteWriteRead

  -- | Try to open a directory
  --
  -- > CreateDirectoryIfMissing True fp
  -- > Open hp IO.WriteMode
  | TagOpenDirectory

  -- | Write to a file
  --
  -- > Put h1
  | TagWrite

  -- | Seek from end of a file
  --
  -- > Seek h IO.SeekFromEnd n (n<0)
  | TagSeekFromEnd

  -- | Create a directory
  --
  -- > CreateDirIfMissing True ..
  | TagCreateDirectory

  -- | DoesFileExistOK returns True
  | TagDoesFileExistOK

  -- | DoesFileExistOK returns False
  | TagDoesFileExistKO

  -- | DoesDirectoryExistOK returns True
  | TagDoesDirectoryExistOK

  -- | DoesDirectoryExistOK returns False
  | TagDoesDirectoryExistKO

  -- | Remove a file
  --
  -- > RemoveFile fe
  -- > DoesFileExist fe
  | TagRemoveFile

  -- | Put truncate and Get
  --
  -- > Put ..
  -- > Truncate ..
  -- > Get ..
  | TagPutTruncateGet

  -- Close a handle 2 times
  --
  -- > h <- Open ..
  -- > close h
  -- > close h
  | TagClosedTwice

  -- Open an existing file with ReadMode and then with WriteMode
  --
  -- > open fp ReadMode
  -- > open fp Write
  | TagOpenReadThenWrite

  -- Open 2 Readers of a file.
  --
  -- > open fp ReadMode
  -- > open fp ReadMode
  | TagOpenReadThenRead

  -- ListDir on a non empty dirextory.
  --
  -- > CreateDirIfMissing True a/b
  -- > ListDirectory a
  | TagCreateDirWithParentsThenListDirNotNull

  -- Read from an AppendMode file
  --
  -- > h <- Open fp AppendMode
  -- > Read h ..
  | TagReadInvalid

  -- Write to a read only file
  --
  -- > h <- Open fp ReadMode
  -- > Put h ..
  | TagWriteInvalid

  -- Put Seek and Get
  --
  -- > Put ..
  -- > Seek ..
  -- > Get ..
  | TagPutSeekGet

  -- Put Seek (negative) and Get
  --
  -- > Put ..
  -- > Seek .. (negative)
  -- > Get ..
  | TagPutSeekNegGet
  deriving (Show, Eq)

-- | Predicate on events
type EventPred = C.Predicate (Event Symbolic) Tag

-- | Convenience combinator for creating classifiers for successful commands
--
-- For convenience we pair handles with the paths they refer to
successful :: (    Event Symbolic
                -> Success FsPath Mock.Handle
                -> Either Tag EventPred
              )
           -> EventPred
successful f = C.predicate $ \ev ->
                 case eventMockResp ev of
                   Resp (Left  _ ) -> Right $ successful f
                   Resp (Right ok) -> f ev ok

-- | Tag commands
--
-- Tagging works on symbolic events, so that we can tag without doing real IO.
tag :: [Event Symbolic] -> [Tag]
tag = C.classify [
      tagCreateDirThenListDir Set.empty
    , tagCreateDirWithParentsThenListDir Set.empty
    , tagAtLeastNOpenFiles 0
    , tagPutTruncatePut Map.empty Map.empty Map.empty
    , tagConcurrentWriterReader Map.empty
    , tagWriteWriteRead Map.empty
    , tagOpenDirectory Set.empty
    , tagWrite
    , tagSeekFromEnd
    , tagCreateDirectory
    , tagDoesFileExistOK
    , tagDoesFileExistKO
    , tagDoesDirectoryExistOK
    , tagDoesDirectoryExistKO
    , tagRemoveFile Set.empty
    , tagPutTruncateGet Map.empty Set.empty
    , tagClosedTwice Set.empty
    , tagOpenReadThenWrite Set.empty
    , tagOpenReadThenRead Set.empty
    , tagCreateDirWithParentsThenListDirNotNull Set.empty
    , tagReadInvalid Set.empty
    , tagWriteInvalid Set.empty
    , tagPutSeekGet Set.empty Set.empty
    , tagPutSeekNegGet Set.empty Set.empty
    ]
  where
    tagCreateDirThenListDir :: Set FsPath -> EventPred
    tagCreateDirThenListDir created = successful $ \ev _ ->
        case eventMockCmd ev of
          CreateDir fe ->
              Right $ tagCreateDirThenListDir (Set.insert fp created)
            where
              fp = evalPathExpr fe
          ListDirectory fe | fp `Set.member` (Set.map init created) ->
              Left TagCreateDirThenListDir
            where
              fp = evalPathExpr fe
          _otherwise ->
            Right $ tagCreateDirThenListDir created

    tagCreateDirWithParentsThenListDir :: Set FsPath -> EventPred
    tagCreateDirWithParentsThenListDir created = successful $ \ev _ ->
        case eventMockCmd ev of
          CreateDirIfMissing True fe | length fp > 1 ->
              Right $ tagCreateDirWithParentsThenListDir (Set.insert fp created)
            where
              fp = evalPathExpr fe
          ListDirectory fe | fp `Set.member` (Set.map init created) ->
              Left TagCreateDirWithParentsThenListDir
            where
              fp = evalPathExpr fe
          _otherwise ->
            Right $ tagCreateDirWithParentsThenListDir created

    tagCreateDirWithParentsThenListDirNotNull :: Set FsPath -> EventPred
    tagCreateDirWithParentsThenListDirNotNull created = successful $ \ev suc ->
        case (eventMockCmd ev, suc) of
          (CreateDirIfMissing True fe, _) | length fp > 1 ->
              Right $ tagCreateDirWithParentsThenListDirNotNull (Set.insert fp created)
            where
              fp = evalPathExpr fe
          (ListDirectory fe, Strings set) | fp `Set.member` (Set.map init created) && not (Set.null set) ->
              Left TagCreateDirWithParentsThenListDirNotNull
            where
              fp = evalPathExpr fe
          _otherwise ->
            Right $ tagCreateDirWithParentsThenListDirNotNull created


    -- TODO: It turns out we never hit the 10 (or higher) open handles case
    -- Not sure if this is a problem or not.
    tagAtLeastNOpenFiles :: Int -> EventPred
    tagAtLeastNOpenFiles maxNumOpen = C.Predicate {
          predApply = \ev ->
            let maxNumOpen' = max maxNumOpen (countOpen (eventAfter ev))
            in Right $ tagAtLeastNOpenFiles maxNumOpen'
        , predFinish = case maxNumOpen of
                         0 -> Nothing
                         1 -> Just $ TagAtLeastNOpenFiles 1
                         2 -> Just $ TagAtLeastNOpenFiles 2
                         n | n < 10 -> Just $ TagAtLeastNOpenFiles 3
                         n -> Just $ TagAtLeastNOpenFiles (n `div` 10 * 10)
        }
      where
        countOpen :: Model r -> Int
        countOpen = Mock.numOpenHandles . mockFS

    tagPutTruncateGet :: Map (Mock.Handle, FsPath) Int64
                      -> Set (Mock.Handle, FsPath)
                      -> EventPred
    tagPutTruncateGet put truncated = successful $ \ev _ ->
      case resolveCmd  ev of
        Put (h, fp) bs | BL.length bs /= 0 ->
          let
              f Nothing  = Just $ BL.length bs
              f (Just n) = Just $ (BL.length bs) + n
              put' = Map.alter f (h, fp) put
          in Right $ tagPutTruncateGet put' truncated
        Truncate (h, fp) sz | sz > 0 -> case Map.lookup (h, fp) put of
          Just p | fromIntegral sz < p ->
            let truncated' = Set.insert (h, fp) truncated
            in Right $ tagPutTruncateGet put truncated'
          _otherwise -> Right $ tagPutTruncateGet put truncated
        Get (h, fp) n | n > 0 && (not $ Set.null $ Set.filter (\(hRead, fp') -> fp' == fp && not (hRead == h)) truncated) ->
              Left TagPutTruncateGet
        _otherwise -> Right $ tagPutTruncateGet put truncated

    tagPutTruncatePut :: Map Mock.Handle BL.ByteString
                      -> Map Mock.Handle BL.ByteString
                      -> Map Mock.Handle BL.ByteString
                      -> EventPred
    tagPutTruncatePut before truncated after = successful $ \ev _ ->
        case eventMockCmd ev of
          Put h bs | BL.length bs /= 0 ->
            case Map.lookup h truncated of
              Nothing -> -- not yet truncated
                let before' = Map.alter (appTo bs) h before in
                Right $ tagPutTruncatePut before' truncated after
              Just deleted ->
                let putAfter = Map.findWithDefault mempty h after <> bs
                    after'   = Map.insert h putAfter after in
                if deleted /= BL.take (BL.length deleted) putAfter
                  then Left  $ TagPutTruncatePut
                  else Right $ tagPutTruncatePut before truncated after'
          Truncate h sz | sz > 0 ->
            let putBefore  = Map.findWithDefault mempty h before
                (putBefore', deleted) = BL.splitAt (fromIntegral sz) putBefore
                before'    = Map.insert h putBefore' before
                truncated' = Map.insert h deleted    truncated
                after'     = Map.delete h            after
            in Right $ tagPutTruncatePut before' truncated' after'
          _otherwise ->
            Right $ tagPutTruncatePut before truncated after
      where
        appTo :: Monoid a => a -> Maybe a -> Maybe a
        appTo b Nothing  = Just b
        appTo b (Just a) = Just (a <> b)

    tagConcurrentWriterReader :: Map Mock.Handle (Set Mock.Handle) -> EventPred
    tagConcurrentWriterReader put = successful $ \ev@Event{..} _ ->
        case resolveCmd ev of
          Put (h, fp) bs | BL.length bs > 0 ->
            -- Remember the other handles to the same file open at this time
            let readHs :: Set Mock.Handle
                readHs = Set.fromList
                       $ map fst
                       $ filter (\(h', fp') -> h /= h' && fp == fp')
                       $ openHandles eventBefore

                put' :: Map Mock.Handle (Set Mock.Handle)
                put' = Map.alter (Just . maybe readHs (Set.union readHs)) h put

            in Right $ tagConcurrentWriterReader put'
          Close (h, _) ->
            Right $ tagConcurrentWriterReader (Map.delete h put)
          Get (h, _) n | h `elem` Set.unions (Map.elems put), n > 0 ->
            Left TagConcurrentWriterReader
          _otherwise ->
            Right $ tagConcurrentWriterReader put

    tagOpenReadThenWrite :: Set FsPath -> EventPred
    tagOpenReadThenWrite readOpen = successful $ \ev@Event{..} _ ->
      case eventMockCmd ev of
        Open (PExpPath fp) IO.ReadMode ->
          Right $ tagOpenReadThenWrite $ Set.insert fp readOpen
        Open (PExpPath fp) IO.WriteMode | Set.member fp readOpen ->
          Left TagOpenReadThenWrite
        _otherwise -> Right $ tagOpenReadThenWrite readOpen

    tagOpenReadThenRead :: Set FsPath -> EventPred
    tagOpenReadThenRead readOpen = successful $ \ev@Event{..} _ ->
      case eventMockCmd ev of
        Open (PExpPath fp) IO.ReadMode | Set.member fp readOpen ->
          Left TagOpenReadThenRead
        Open (PExpPath fp) IO.ReadMode ->
          Right $ tagOpenReadThenRead $ Set.insert fp readOpen
        _otherwise -> Right $ tagOpenReadThenRead readOpen

    tagWriteWriteRead :: Map (Mock.Handle, FsPath) Int -> EventPred
    tagWriteWriteRead wr = successful $ \ev@Event{..} _ ->
      case resolveCmd ev of
        Put (h, fp) bs | BL.length bs > 0 ->
          let f Nothing  = Just 0
              f (Just x) = Just $ x + 1
          in Right $ tagWriteWriteRead $ Map.alter f (h, fp) wr
        Get (hRead, fp) n | n > 1 ->
          if not $ Map.null $ Map.filterWithKey (\(hWrite, fp') times -> fp' == fp && times > 1 && not (hWrite == hRead)) wr
          then Left TagWriteWriteRead
          else Right $ tagWriteWriteRead wr
        _otherwise ->
          Right $ tagWriteWriteRead wr

    -- this never succeeds because of an fsLimitation
    tagOpenDirectory :: Set FsPath -> EventPred
    tagOpenDirectory created = C.predicate $ \ev ->
        case (eventMockCmd ev, eventMockResp ev) of
          (CreateDir fe, Resp (Right _)) ->
            Right $ tagOpenDirectory (Set.insert fp created)
              where
                fp = evalPathExpr fe
          (CreateDirIfMissing True fe, Resp (Right _)) ->
            Right $ tagOpenDirectory (Set.insert fp created)
              where
                fp = evalPathExpr fe
          (Open fe _mode, _) | Set.member (evalPathExpr fe) created ->
            Left TagOpenDirectory
          _otherwise ->
            Right $ tagOpenDirectory created

    tagWrite :: EventPred
    tagWrite = successful $ \ev@Event{..} _ ->
      case eventMockCmd ev of
        Put _ bs | BL.length bs > 0 ->
          Left TagWrite
        _otherwise -> Right tagWrite

    tagSeekFromEnd :: EventPred
    tagSeekFromEnd = successful $ \ev@Event{..} _ ->
      case eventMockCmd ev of
        Seek _ IO.SeekFromEnd n | n < 0 -> Left TagSeekFromEnd
        _otherwise                      -> Right tagSeekFromEnd

    tagCreateDirectory :: EventPred
    tagCreateDirectory = successful $ \ev@Event{..} _ ->
      case resolveCmd ev of
        CreateDirIfMissing True (PExpPath fp) | length fp > 1 ->
          Left TagCreateDirectory
        _otherwise ->
          Right tagCreateDirectory

    tagDoesFileExistOK :: EventPred
    tagDoesFileExistOK = successful $ \ev@Event{..} suc ->
      case (eventMockCmd ev, suc) of
        (DoesFileExist _, Bool True) -> Left TagDoesFileExistOK
        _otherwise                   -> Right tagDoesFileExistOK

    tagDoesFileExistKO :: EventPred
    tagDoesFileExistKO = successful $ \ev@Event{..} suc ->
      case (eventMockCmd ev, suc) of
        (DoesFileExist _, Bool False) -> Left TagDoesFileExistKO
        _otherwise                    -> Right tagDoesFileExistKO

    tagDoesDirectoryExistOK :: EventPred
    tagDoesDirectoryExistOK = successful $ \ev@Event{..} suc ->
      case (eventMockCmd ev, suc) of
        (DoesDirectoryExist (PExpPath fp), Bool True) | not (fp == ["/"])
                   -> Left TagDoesDirectoryExistOK
        _otherwise -> Right tagDoesDirectoryExistOK

    tagDoesDirectoryExistKO :: EventPred
    tagDoesDirectoryExistKO = successful $ \ev@Event{..} suc ->
      case (eventMockCmd ev, suc) of
        (DoesDirectoryExist _, Bool False) -> Left TagDoesDirectoryExistKO
        _otherwise                         -> Right tagDoesDirectoryExistKO

    tagRemoveFile :: Set FsPath -> EventPred
    tagRemoveFile removed = successful $ \ev@Event{..} _suc ->
      case resolveCmd ev of
        RemoveFile fe -> Right $ tagRemoveFile $ Set.insert fp removed
          where
            fp = evalPathExpr fe
        DoesFileExist fe -> if Set.member fp removed
          then Left TagRemoveFile
          else Right $ tagRemoveFile removed
            where
              fp = evalPathExpr fe
        _otherwise -> Right $ tagRemoveFile removed

    tagClosedTwice :: Set Mock.Handle -> EventPred
    tagClosedTwice closed = successful $ \ev@Event{..} _suc ->
      case eventMockCmd ev of
        Close h | Set.member h closed -> Left TagClosedTwice
        Close h -> Right $ tagClosedTwice $ Set.insert h closed
        _otherwise -> Right $ tagClosedTwice closed

    -- this never succeeds because of an fsLimitation
    tagReadInvalid :: Set Mock.Handle -> EventPred
    tagReadInvalid openAppend = C.predicate $ \ev ->
      case (eventMockCmd ev, eventMockResp ev) of
        (Open _ IO.AppendMode, Resp (Right (WHandle _ h))) ->
          Right $ tagReadInvalid $ Set.insert h openAppend
        (Close h, Resp (Right _)) ->
          Right $ tagReadInvalid $ Set.delete h openAppend
        (Get h _, Resp (Left _)) | Set.member h openAppend ->
          Left TagReadInvalid
        _otherwise -> Right $ tagReadInvalid openAppend

    -- never succeeds, not sure why.
    tagWriteInvalid :: Set Mock.Handle -> EventPred
    tagWriteInvalid openRead = C.predicate $ \ev ->
      case (eventMockCmd ev, eventMockResp ev) of
        (Open _ IO.ReadMode, Resp (Right (WHandle _ h))) ->
          Right $ tagWriteInvalid $ Set.insert h openRead
        (Close h, Resp (Right _)) ->
          Right $ tagWriteInvalid $ Set.delete h openRead
        (Put h _, _) | Set.member h openRead ->
          Left TagWriteInvalid
        _otherwise -> Right $ tagWriteInvalid openRead

    tagPutSeekGet :: Set Mock.Handle -> Set Mock.Handle -> EventPred
    tagPutSeekGet put seek = successful $ \ev@Event{..} _suc ->
      case eventMockCmd ev of
        Put h bs | BL.length bs > 0 ->
          Right $ tagPutSeekGet (Set.insert h put) seek
        Seek h IO.RelativeSeek n | n > 0 && Set.member h put->
          Right $ tagPutSeekGet put (Set.insert h seek)
        Get h n | n > 0 && Set.member h seek ->
          Left TagPutSeekGet
        _otherwise -> Right $ tagPutSeekGet put seek

    tagPutSeekNegGet :: Set Mock.Handle -> Set Mock.Handle -> EventPred
    tagPutSeekNegGet put seek = successful $ \ev@Event{..} _suc ->
      case eventMockCmd ev of
        Put h bs | BL.length bs > 0 ->
          Right $ tagPutSeekNegGet (Set.insert h put) seek
        Seek h IO.RelativeSeek n | n < 0 && Set.member h put->
          Right $ tagPutSeekNegGet put (Set.insert h seek)
        Get h n | n > 0 && Set.member h seek ->
          Left TagPutSeekNegGet
        _otherwise -> Right $ tagPutSeekNegGet put seek

-- | Step the model using a 'QSM.Command' (i.e., a command associated with
-- an explicit set of variables)
execCmd :: Model Symbolic -> QSM.Command (At Cmd) (At Resp) -> Event Symbolic
execCmd model (QSM.Command cmd resp _vars) = lockstep model cmd resp

-- | 'execCmds' is just the repeated form of 'execCmd'
execCmds :: QSM.Commands (At Cmd) (At Resp) -> [Event Symbolic]
execCmds = \(QSM.Commands cs) -> go initModel cs
  where
    go :: Model Symbolic -> [QSM.Command (At Cmd) (At Resp)] -> [Event Symbolic]
    go _ []       = []
    go m (c : cs) = let ev = execCmd m c in ev : go (eventAfter ev) cs

{-------------------------------------------------------------------------------
  Required instances

  The 'ToExpr' constraints come from "Data.TreeDiff".
-------------------------------------------------------------------------------}

instance QSM.CommandNames (At Cmd) where
  cmdName  (At cmd) = constrName cmd
  cmdNames _        = constrNames (Proxy @(Cmd () ()))

deriving instance ToExpr a => ToExpr (FsTree a)
deriving instance ToExpr fp => ToExpr (PathExpr fp)

deriving instance ToExpr MockFS
deriving instance ToExpr Mock.Handle
deriving instance ToExpr Mock.HandleState
deriving instance ToExpr Mock.OpenHandleState
deriving instance ToExpr Mock.ClosedHandleState
deriving instance ToExpr Mock.FilePtr

deriving instance ToExpr (Model Concrete)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

-- | Show minimal examples for each of the generated tags
--
-- TODO: The examples listed are not always minimal. I'm not entirely sure why.
showLabelledExamples' :: Maybe Int
                      -- ^ Seed
                      -> Int
                      -- ^ Number of tests to run to find examples
                      -> (Tag -> Bool)
                      -- ^ Tag filter (can be @const True@)
                      -> IO ()
showLabelledExamples' mReplay numTests focus = do
    replaySeed <- case mReplay of
      Nothing   -> getStdRandom (randomR (1,999999))
      Just seed -> return seed

    labelledExamplesWith (stdArgs { replay     = Just (mkQCGen replaySeed, 0)
                                  , maxSuccess = numTests
                                  }) $
      forAllShrinkShow (QSM.generateCommands sm' Nothing)
                       (QSM.shrinkCommands   sm')
                       pp $ \cmds ->
        collects (filter focus . tag . execCmds $ cmds) $
          property True

    putStrLn $ "Used replaySeed " ++ show replaySeed
  where
    sm' = sm mountUnused
    pp  = \x -> ppShow x ++ "\n" ++ condense x

showLabelledExamples :: IO ()
showLabelledExamples = showLabelledExamples' Nothing 1000 (const True)

prop_sequential :: FilePath -> Property
prop_sequential tmpDir =
    QSM.forAllCommands (sm mountUnused) Nothing $ \cmds -> monadicIO $ do
      tstTmpDir <- liftIO $ createTempDirectory tmpDir "HasFS"
      let mount = MountPoint tstTmpDir
          sm'   = sm mount

      (hist, model, res) <- QSM.runCommands sm' cmds

      -- | Close all open handles and delete the temp directory
      liftIO $ do
        forM_ (RE.keys (knownHandles model)) $ F.close . QSM.opaque
        removeDirectoryRecursive tstTmpDir

      QSM.prettyCommands sm' hist
        $ tabulate "Tags" (map show $ tag (execCmds cmds))
        $ counterexample ("Mount point: " ++ tstTmpDir)
        $ res === QSM.Ok

tests :: FilePath -> TestTree
tests tmpDir = testGroup "HasFS" [
      testProperty "q-s-m" $ prop_sequential tmpDir
    ]

-- | Unused mount mount
--
-- 'forAllCommands' wants the entire state machine as argument, but we
-- need the mount point only when /executing/ the commands in IO. We can
-- therefore generate the commands with a dummy mount point, and then
-- inside the property construct a temporary directory which we can use
-- for execution.
mountUnused :: MountPoint
mountUnused = error "mount point not used during command generation"

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Debugging: show @n@ levels of shrink steps (with some required tags)
--
-- This can be useful when debugging the shrinker
_showTaggedShrinks :: ([Tag] -> Bool)  -- ^ Required tags
                   -> Int              -- ^ Number of shrink steps
                   -> QSM.Commands (At Cmd) (At Resp)
                   -> IO ()
_showTaggedShrinks hasRequiredTags numLevels = go 0
  where
    go :: Int -> QSM.Commands (At Cmd) (At Resp) -> IO ()
    go n _ | n == numLevels = return ()
    go n cmds = do
      if hasRequiredTags tags then do
        putStrLn $ replicate n '\t' ++ condense (cmds, tags)
        forM_ shrinks $ go (n + 1)
      else
        return ()
      where
        tags    = tag $ execCmds cmds
        shrinks = QSM.shrinkCommands (sm mountUnused) cmds

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Condense fp => Condense (PathExpr fp) where
  condense (PExpPath     fp) = show $ "/" ++ L.intercalate "/" fp
  condense (PExpRef      fp) = condense fp
  condense (PExpParentOf fp) = condense fp ++ "/.."

instance (Condense fp, Condense h) => Condense (Cmd fp h) where
  condense = L.intercalate " " . go
    where
      go (Open fp mode)            = ["open", condense fp, condense mode]
      go (Close h)                 = ["close", condense h]
      go (Seek h mode o)           = ["seek", condense h, condense mode, condense o]
      go (Get h n)                 = ["get", condense h, condense n]
      go (Put h bs)                = ["put", condense h, condense bs]
      go (Truncate h sz)           = ["truncate", condense h, condense sz]
      go (GetSize h)               = ["getSize", condense h]
      go (CreateDir fp)            = ["createDir", condense fp]
      go (CreateDirIfMissing p fp) = ["createDirIfMissing", condense p, condense fp]
      go (ListDirectory fp)        = ["listDirectory", condense fp]
      go (DoesDirectoryExist fp)   = ["doesDirectoryExist", condense fp]
      go (DoesFileExist fp)        = ["doesFileExist", condense fp]
      go (RemoveFile fp)           = ["removeFile", condense fp]

instance Condense1 r => Condense (Cmd :@ r) where
  condense (At cmd) = condense cmd

instance Condense Tag where
  condense = show

{-------------------------------------------------------------------------------
  (Orphan) condense instance for QSM types
-------------------------------------------------------------------------------}

instance Condense QSM.Var where
  condense (QSM.Var i) = "x" ++ condense i

instance Condense1 Symbolic where
  liftCondense _ (QSM.Symbolic a) = condense a

instance Condense (QSM.Opaque a) where
  condense _ = "<>"

instance (Condense1 r, Condense a) => Condense (QSM.Reference a r) where
  condense (QSM.Reference ra) = condense1 ra

instance Condense (cmd Symbolic) => Condense (QSM.Command cmd resp) where
  condense = \(QSM.Command cmd _resp vars) ->
      L.intercalate " " $ go cmd vars
    where
      go :: cmd Symbolic -> [QSM.Var] -> [String]
      go cmd [] = [condense cmd]
      go cmd xs = [condense xs, "<-", condense cmd]

instance Condense (cmd Symbolic) => Condense (QSM.Commands cmd resp) where
  condense (QSM.Commands cmds) = unlines $ "do" : map (indent . condense) cmds
    where
      indent :: String -> String
      indent = ("  " ++)

{-------------------------------------------------------------------------------
  generics-sop auxiliary
-------------------------------------------------------------------------------}

cmdConstrInfo :: Proxy (Cmd fp h)
              -> SOP.NP SOP.ConstructorInfo (SOP.Code (Cmd fp h))
cmdConstrInfo = SOP.constructorInfo . SOP.datatypeInfo

constrName :: forall fp h. Cmd fp h -> String
constrName a =
    SOP.hcollapse $ SOP.hliftA2 go (cmdConstrInfo p) (SOP.unSOP (SOP.from a))
  where
    go :: SOP.ConstructorInfo a -> SOP.NP SOP.I a -> SOP.K String a
    go nfo _ = SOP.K $ SOP.constructorName nfo

    p = Proxy @(Cmd fp h)

constrNames :: Proxy (Cmd fp h) -> [String]
constrNames p =
    SOP.hcollapse $ SOP.hmap go (cmdConstrInfo p)
  where
    go :: SOP.ConstructorInfo a -> SOP.K String a
    go nfo = SOP.K $ SOP.constructorName nfo
