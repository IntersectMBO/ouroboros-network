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

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Storage.FS.StateMachine (
    tests
  , showLabelledExamples
  ) where

import qualified Control.Exception as E
import           Control.Monad
import           Data.Bifoldable
import           Data.Bifunctor
import qualified Data.Bifunctor.TH as TH
import           Data.Bitraversable
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Functor.Classes
import           Data.Int (Int64)
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.TreeDiff (ToExpr (..), defaultExprViaShow)
import           Data.Word (Word64)
import           GHC.Generics
import           GHC.Stack
import qualified Generics.SOP as SOP
import           System.IO.Temp (withTempDirectory)
import           System.Random (getStdRandom, randomR)
import           Text.Read (readMaybe)
import           Text.Show.Pretty (ppShow)

import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Random (mkQCGen)
import           Test.StateMachine (Concrete, Symbolic)
import qualified Test.StateMachine as QSM
import qualified Test.StateMachine.Sequential as QSM
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Storage.FS.API (HasFS (..))
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.FS.IO
import qualified Ouroboros.Consensus.Storage.IO as F

import           Ouroboros.Consensus.Util.Condense

import qualified Test.Util.Classify as C
import           Test.Util.FS.Sim.FsTree (FsTree (..))
import           Test.Util.FS.Sim.MockFS (HandleMock, MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.Pure
import           Test.Util.QuickCheck (collects)
import           Test.Util.RefEnv (RefEnv)
import qualified Test.Util.RefEnv as RE
import           Test.Util.SOP

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
evalPathExpr (PExpParentOf fp) = fsPathInit fp

{-------------------------------------------------------------------------------
  Abstract model
-------------------------------------------------------------------------------}

-- | Commands
--
-- We will be interested in three different instantiations of @h@:
--
-- > Cmd HandleMock
-- > Cmd (Reference (Opaque (FsHandle IOFSE)) Concrete)
-- > Cmd (Reference (Opaque (FsHandle IOFSE)) Symbolic)
--
-- Key idea is that all this infrastructure will be applicable both to
-- the model and to the system under test.
--
-- TODO: Program such as "copy what you read" is currently not expressible
-- in our language. Does this matter?
data Cmd fp h =
    Open               (PathExpr fp) OpenMode
  | Close              h
  | IsOpen             h
  | Seek               h SeekMode Int64
  | Get                h Word64
  | GetAt              h Word64 AbsOffset
  | Put                h ByteString
  | Truncate           h Word64
  | GetSize            h
  | CreateDir          (PathExpr fp)
  | CreateDirIfMissing Bool (PathExpr fp)
  | ListDirectory      (PathExpr fp)
  | DoesDirectoryExist (PathExpr fp)
  | DoesFileExist      (PathExpr fp)
  | RemoveFile         (PathExpr fp)
  | RenameFile         (PathExpr fp) (PathExpr fp)
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
  | ByteString ByteString
  | Strings    (Set String)
  | Bool       Bool
  deriving (Eq, Show, Functor, Foldable)

-- | Successful semantics
run :: forall m h. Monad m
    => HasFS m h
    -> Cmd FsPath (Handle h)
    -> m (Success FsPath (Handle h))
run hasFS@HasFS{..} = go
  where
    go :: Cmd FsPath (Handle h) -> m (Success FsPath (Handle h))
    go (Open pe mode) =
        case mode of
          ReadMode   -> withPE pe (\_ -> RHandle) $ \fp -> hOpen fp mode
          _otherwise -> withPE pe WHandle         $ \fp -> hOpen fp mode

    go (CreateDir            pe) = withPE pe Path   $ createDirectory
    go (CreateDirIfMissing b pe) = withPE pe Path   $ createDirectoryIfMissing b
    go (IsOpen   h             ) = Bool       <$> hIsOpen   h
    go (Close    h             ) = Unit       <$> hClose    h
    go (Seek     h mode sz     ) = Unit       <$> hSeek     h mode sz
    -- Note: we're not using 'hGetSome', 'hGetSomeAt' and 'hPutSome' that may
    -- produce partial reads/writes, but wrappers around them that handle
    -- partial reads/writes, see #502.
    go (Get      h n           ) = ByteString <$> hGetSomeChecked hasFS h n
    go (GetAt    h n o         ) = ByteString <$> hGetSomeAtChecked hasFS h n o
    go (Put      h bs          ) = Word64     <$> hPutSomeChecked hasFS h bs
    go (Truncate h sz          ) = Unit       <$> hTruncate h sz
    go (GetSize  h             ) = Word64     <$> hGetSize  h
    go (ListDirectory      pe  ) = withPE  pe      (const Strings) $ listDirectory
    go (DoesDirectoryExist pe  ) = withPE  pe      (const Bool)    $ doesDirectoryExist
    go (DoesFileExist      pe  ) = withPE  pe      (const Bool)    $ doesFileExist
    go (RemoveFile         pe  ) = withPE  pe      (const Unit)    $ removeFile
    go (RenameFile     pe1 pe2 ) = withPEs pe1 pe2 (\_ _ -> Unit)  $ renameFile

    withPE :: PathExpr FsPath
           -> (FsPath -> a -> Success FsPath (Handle h))
           -> (FsPath -> m a)
           -> m (Success FsPath (Handle h))
    withPE pe r f = let fp = evalPathExpr pe in r fp <$> f fp

    withPEs :: PathExpr FsPath
            -> PathExpr FsPath
            -> (FsPath -> FsPath -> a -> Success FsPath (Handle h))
            -> (FsPath -> FsPath -> m a)
            -> m (Success FsPath (Handle h))
    withPEs pe1 pe2 r f =
      let fp1 = evalPathExpr pe1
          fp2 = evalPathExpr pe2
      in r fp1 fp2 <$> f fp1 fp2


{-------------------------------------------------------------------------------
  Detecting partial reads/writes of the tested IO implementation
-------------------------------------------------------------------------------}

-- The functions 'hGetSome', 'hGetSomeAt' and 'hPutSome' might perform partial
-- reads/writes, depending on the underlying implementation, see #277. While
-- the model will always perform complete reads/writes, the real IO
-- implementation we are testing /might/ actually perform partial reads/writes.
-- This testsuite will fail when such a partial read or write is performed in
-- the real IO implementation, as these are undeterministic and the model will
-- no longer correspond to the real implementation. See #502 were we track this
-- issue.
--
-- So far, on all systems the tests have been run on, no partial reads/writes
-- have ever been noticed. However, we cannot be sure that the tests will
-- never be run on a system or file-system that might result in partial
-- reads/writes. Therefore, we use checked variants of 'hGetSome', 'hGetSomeAt'
-- and 'hPutSome' that detect partial reads/writes and that will signal an
-- error so that the developer noticing the failing test doesn't waste any time
-- debugging the implementation while the failing test was actually due to an
-- unexpected partial read/write.
--
-- While using the wrappers 'hGetExactly' and 'hPutAll' instead of 'hGetSome',
-- 'hGetSomeAt' and 'hPut' in the implementation of 'run' will opaquely handle
-- any potential partial reads/writes, it is not a good solution. The problem
-- is that to run a single 'Cmd', we now have to run multiple primitive 'HasFS'
-- functions. Each of those primitive functions might update the state of the
-- model and the real world. Now when the second, third, ..., or n-th
-- primitive functions fails (while running a single 'Cmd'), the whole 'Cmd'
-- failed and the model is not updated. This means that we continue with the
-- model as it was /before/ running the 'Cmd'. However, these primitive
-- functions might have changed the model /and/ the state of the real
-- implementation. In that case, we can no longer guarantee that the model and
-- the real implementation are in sync.

hGetSomeChecked :: (Monad m, HasCallStack)
                => HasFS m h -> Handle h -> Word64 -> m ByteString
hGetSomeChecked HasFS{..} h n = do
    bytes <- hGetSome h n
    when (fromIntegral (BS.length bytes) /= n) $ do
      moreBytes <- hGetSome h 1
      -- If we can actually read more bytes, the last read was partial. If we
      -- cannot, we really were at EOF.
      unless (BS.null moreBytes) $
        error "Unsupported partial read detected, see #502"
    return bytes

hGetSomeAtChecked :: (Monad m, HasCallStack)
                  => HasFS m h -> Handle h -> Word64 -> AbsOffset -> m ByteString
hGetSomeAtChecked HasFS{..} h n o = do
    bytes <- hGetSomeAt h n o
    when (fromIntegral (BS.length bytes) /= n) $ do
      moreBytes <- hGetSomeAt h 1 $ o + fromIntegral (BS.length bytes)
      -- If we can actually read more bytes, the last read was partial. If we
      -- cannot, we really were at EOF.
      unless (BS.null moreBytes) $
        error "Unsupported partial read detected, see #502"
    return bytes

hPutSomeChecked :: (Monad m, HasCallStack)
                => HasFS m h -> Handle h -> ByteString -> m Word64
hPutSomeChecked HasFS{..} h bytes = do
    n <- hPutSome h bytes
    if fromIntegral (BS.length bytes) /= n
      then error "Unsupported partial write detected, see #502"
      else return n

{-------------------------------------------------------------------------------
  Instantiating the semantics
-------------------------------------------------------------------------------}

-- | Responses are either successful termination or an error
newtype Resp fp h = Resp { getResp :: Either FsError (Success fp h) }
  deriving (Show, Functor, Foldable)

-- | The 'Eq' instance for 'Resp' uses 'F.sameError'
instance (Eq fp, Eq h) => Eq (Resp fp h) where
  Resp (Left  e) == Resp (Left  e') = F.sameError e e'
  Resp (Right a) == Resp (Right a') = a == a'
  _              == _               = False

runPure :: Cmd FsPath (Handle HandleMock)
        -> MockFS -> (Resp FsPath (Handle HandleMock), MockFS)
runPure cmd mockFS =
    aux $ runPureSimFS (run pureHasFS cmd) mockFS
  where
    aux :: Either FsError (Success FsPath (Handle HandleMock), MockFS)
        -> (Resp FsPath (Handle HandleMock), MockFS)
    aux (Left e)             = (Resp (Left e), mockFS)
    aux (Right (r, mockFS')) = (Resp (Right r), mockFS')

runIO :: MountPoint
      -> Cmd FsPath (Handle HandleIO) -> IO (Resp FsPath (Handle HandleIO))
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
type HandleRef = QSM.Reference (Handle HandleIO)

-- | Mapping between real IO file handles and mock file handles
type KnownHandles = RefEnv (Handle HandleIO) (Handle HandleMock)

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
toMock :: (Bifunctor t, Eq1 r)
       => Model r -> t :@ r -> t FsPath (Handle HandleMock)
toMock Model{..} (At r) = bimap (knownPaths RE.!) (knownHandles RE.!) r

-- | Step the mock semantics
--
-- We cannot step the whole Model here (see 'event', below)
step :: Eq1 r
     => Model r -> Cmd :@ r -> (Resp FsPath (Handle HandleMock), MockFS)
step model@Model{..} cmd = runPure (toMock model cmd) mockFS

-- | Open read handles
openHandles :: Model r -> [Handle HandleMock]
openHandles Model{..} =
    filter isOpen (RE.elems knownHandles)
  where
    isOpen :: Handle HandleMock -> Bool
    isOpen (Handle h _) = Mock.handleIsOpen mockFS h

{-------------------------------------------------------------------------------
  Wrapping in quickcheck-state-machine references
-------------------------------------------------------------------------------}

-- | Instantiate functor @f@ to @f (PathRef r) (HRef r)@
--
-- > Cmd :@ Concrete ~ Cmd (PathRef Concrete) (HandleRef Concrete)
newtype At t r = At {unAt :: (t (PathRef r) (HandleRef r))}
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
    , eventMockResp :: Resp FsPath (Handle HandleMock)
    }
  deriving (Show)

eventMockCmd :: Eq1 r => Event r -> Cmd FsPath (Handle HandleMock)
eventMockCmd Event{..} = toMock eventBefore eventCmd

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
          fmap At $                        genOpen
        , fmap At $ CreateDir          <$> genPathExpr
        , fmap At $ CreateDirIfMissing <$> arbitrary <*> genPathExpr
        , fmap At $ ListDirectory      <$> genPathExpr
        , fmap At $ DoesDirectoryExist <$> genPathExpr
        , fmap At $ DoesFileExist      <$> genPathExpr
        , fmap At $ RemoveFile         <$> genPathExpr
        , fmap At $ RenameFile         <$> genPathExpr <*> genPathExpr
        ]

    withHandle :: [Gen (Cmd :@ Symbolic)]
    withHandle = [
          fmap At $ Close    <$> genHandle
        , fmap At $ IsOpen   <$> genHandle
        , fmap At $ Seek     <$> genHandle <*> genSeekMode <*> genOffset
        , fmap At $ Get      <$> genHandle <*> (getSmall <$> arbitrary)
        , fmap At $ GetAt    <$> genHandle <*> (getSmall <$> arbitrary) <*> arbitrary
        , fmap At $ Put      <$> genHandle <*> (BS.pack <$> arbitrary)
        , fmap At $ Truncate <$> genHandle <*> (getSmall . getNonNegative <$> arbitrary)
        , fmap At $ GetSize  <$> genHandle
        ]

    genOpen :: Gen (Cmd (PathRef Symbolic) (HandleRef Symbolic))
    genOpen = do
      path <- genPath
      mode <- genMode $ elem path (RE.elems knownPaths)
      return $ Open (PExpPath path) mode

    -- Wrap path in a simple path expression
    -- (References are generated during shrinking only)
    genPathExpr :: Gen (PathExpr fp)
    genPathExpr = PExpPath <$> genPath

    -- We choose from a small list of names so that we reuse names often
    -- We use the same set of files and directories so that we can test
    -- things like trying to open a directory as if it were a file
    genPath :: Gen FsPath
    genPath = do
        n <- choose (0, 3)
        mkFsPath <$> replicateM n (elements ["x", "y", "z"])

    genHandle :: Gen (HandleRef Symbolic)
    genHandle = elements (RE.keys knownHandles)

    genMode :: Bool -> Gen OpenMode
    genMode fileExists = frequency [
          (rf, return ReadMode)
        , (wf, WriteMode     <$> genAllowExisting)
        , (wf, AppendMode    <$> genAllowExisting)
        , (wf, ReadWriteMode <$> genAllowExisting)
        ]
      where
        -- we try to avoid 'ReadMode' when the file does not exist.
        (rf, wf) = if fileExists then (10,3) else (1,3)

    genAllowExisting :: Gen AllowExisting
    genAllowExisting = elements [AllowExisting, MustBeNew]

    genSeekMode :: Gen SeekMode
    genSeekMode = elements [
          AbsoluteSeek
        , RelativeSeek
        , SeekFromEnd
        ]

    genOffset :: Gen Int64
    genOffset = oneof
         [ return 0
         , choose (1, 10)
         , choose (-1, -10)
         ]

instance Arbitrary AbsOffset where
  arbitrary = AbsOffset . getSmall <$> arbitrary
  shrink ao = AbsOffset <$> shrink (unAbsOffset ao)

{-------------------------------------------------------------------------------
  Temporary files (used in shrinking)
-------------------------------------------------------------------------------}

-- | Temp files are numbered from 1
newtype TempFile = TempFile Int
  deriving (Show)

instance Condense TempFile where -- basically GNTD
  condense (TempFile n) = condense n

tempToExpr :: TempFile -> PathExpr fp
tempToExpr (TempFile n) = PExpPath (mkFsPath ['t' : show n])

tempFromPath :: FsPath -> Maybe TempFile
tempFromPath fp =
    case map Text.unpack (fsPathToList fp) of
      ['t' : suf] -> do n <- readMaybe suf
                        guard (n >= 1)
                        return $ TempFile n
      _otherwise  -> Nothing

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
                             ReadMode   -> ReadWriteMode AllowExisting
                             _otherwise -> mode
                in [At $ Open (tempToExpr (TempFile numTempFiles)) mode']
          , case mode of
              ReadWriteMode ex -> [
                  At $ Open pe ReadMode
                , At $ Open pe (WriteMode ex)
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
              replaceWithRef pe ((== fp) . fsPathInit) PExpParentOf
          ]
        where
          fp :: FsPath
          fp = resolvePathExpr knownPaths pe

      Get      h n   -> At . Get      h <$> shrink n
      GetAt    h n o -> At <$>
        [GetAt h n o' | o' <- shrink o] <>
        [GetAt h n' o | n' <- shrink n]
      Put      h bs  -> At . Put      h <$> shrinkBytes bs
      Truncate h n   -> At . Truncate h <$> shrink n

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

    shrinkBytes :: ByteString -> [ByteString]
    shrinkBytes = map BS.pack . shrink . BS.unpack

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
            QSM.forall (handles cmd) (`QSM.member` RE.keys knownHandles)
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
    QSM..&& errorHasMountPoint (getResp $ unAt resp)
  where
    ev = lockstep model cmd resp

    errorHasMountPoint :: Either FsError a -> QSM.Logic
    errorHasMountPoint (Right _)      = QSM.Top
    errorHasMountPoint (Left fsError) = QSM.Boolean $ hasMountPoint fsError

semantics :: MountPoint -> Cmd :@ Concrete -> IO (Resp :@ Concrete)
semantics mount (At cmd) =
    At . bimap QSM.reference QSM.reference <$>
      runIO mount (bimap QSM.concrete QSM.concrete cmd)

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
             , cleanup       = QSM.noCleanup
             , invariant     = Nothing
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

  -- | Rename a file
  --
  -- > _ <- Open fe1 WriteMode
  -- > RenameFile fe2 fe2
  | TagRenameFile

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

  -- Open with MustBeNew (O_EXCL flag), but the file already existed.
  --
  -- > h <- Open fp (AppendMode _)
  -- > Close h
  -- > Open fp (AppendMode MustBeNew)
  | TagExclusiveFail


  -- Reading returns an empty bytestring when EOF
  --
  -- > h <- open fp ReadMode
  -- > Get h 1 == ""
  | TagReadEOF


  -- GetAt
  --
  -- > GetAt ...
  | TagPread
  deriving (Show, Eq)

-- | Predicate on events
type EventPred = C.Predicate (Event Symbolic) Tag

-- | Convenience combinator for creating classifiers for successful commands
--
-- For convenience we pair handles with the paths they refer to
successful :: (    Event Symbolic
                -> Success FsPath (Handle HandleMock)
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
    , tagRenameFile
    , tagPutTruncateGet Map.empty Set.empty
    , tagClosedTwice Set.empty
    , tagOpenReadThenWrite Set.empty
    , tagOpenReadThenRead Set.empty
    , tagCreateDirWithParentsThenListDirNotNull Set.empty
    , tagReadInvalid Set.empty
    , tagWriteInvalid Set.empty
    , tagPutSeekGet Set.empty Set.empty
    , tagPutSeekNegGet Set.empty Set.empty
    , tagExclusiveFail
    , tagReadEOF
    , tagPread
    ]
  where
    tagCreateDirThenListDir :: Set FsPath -> EventPred
    tagCreateDirThenListDir created = successful $ \ev _ ->
        case eventMockCmd ev of
          CreateDir fe ->
              Right $ tagCreateDirThenListDir (Set.insert fp created)
            where
              fp = evalPathExpr fe
          ListDirectory fe | fp `Set.member` (Set.map fsPathInit created) ->
              Left TagCreateDirThenListDir
            where
              fp = evalPathExpr fe
          _otherwise ->
            Right $ tagCreateDirThenListDir created

    tagCreateDirWithParentsThenListDir :: Set FsPath -> EventPred
    tagCreateDirWithParentsThenListDir created = successful $ \ev _ ->
        case eventMockCmd ev of
          CreateDirIfMissing True fe | length (fsPathToList fp) > 1 ->
              Right $ tagCreateDirWithParentsThenListDir (Set.insert fp created)
            where
              fp = evalPathExpr fe
          ListDirectory fe | fp `Set.member` (Set.map fsPathInit created) ->
              Left TagCreateDirWithParentsThenListDir
            where
              fp = evalPathExpr fe
          _otherwise ->
            Right $ tagCreateDirWithParentsThenListDir created

    tagCreateDirWithParentsThenListDirNotNull :: Set FsPath -> EventPred
    tagCreateDirWithParentsThenListDirNotNull created = successful $ \ev suc ->
        case (eventMockCmd ev, suc) of
          (CreateDirIfMissing True fe, _) | length (fsPathToList fp) > 1 ->
              Right $ tagCreateDirWithParentsThenListDirNotNull (Set.insert fp created)
            where
              fp = evalPathExpr fe
          (ListDirectory fe, Strings set) | fp `Set.member` (Set.map fsPathInit created)
                                         && not (Set.null set) ->
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

    tagPutTruncateGet :: Map (HandleMock, FsPath) Int
                      -> Set (HandleMock, FsPath)
                      -> EventPred
    tagPutTruncateGet put truncated = successful $ \ev _ ->
      case eventMockCmd  ev of
        Put (Handle h fp) bs | BS.length bs /= 0 ->
          let
              f Nothing  = Just $ BS.length bs
              f (Just n) = Just $ (BS.length bs) + n
              put' = Map.alter f (h, fp) put
          in Right $ tagPutTruncateGet put' truncated
        Truncate (Handle h fp) sz | sz > 0 -> case Map.lookup (h, fp) put of
          Just p | fromIntegral sz < p ->
            let truncated' = Set.insert (h, fp) truncated
            in Right $ tagPutTruncateGet put truncated'
          _otherwise -> Right $ tagPutTruncateGet put truncated
        Get (Handle h fp) n | n > 0 && (not $ Set.null $
          Set.filter (\(hRead, fp') -> fp' == fp && not (hRead == h)) truncated) ->
            Left TagPutTruncateGet
        GetAt (Handle h fp) n _ | n > 0 && (not $ Set.null $
          Set.filter (\(hRead, fp') -> fp' == fp && not (hRead == h)) truncated) ->
            Left TagPutTruncateGet
        _otherwise -> Right $ tagPutTruncateGet put truncated

    tagPutTruncatePut :: Map HandleMock ByteString
                      -> Map HandleMock ByteString
                      -> Map HandleMock ByteString
                      -> EventPred
    tagPutTruncatePut before truncated after = successful $ \ev _ ->
        case eventMockCmd ev of
          Put (Handle h _) bs | BS.length bs /= 0 ->
            case Map.lookup h truncated of
              Nothing -> -- not yet truncated
                let before' = Map.alter (appTo bs) h before in
                Right $ tagPutTruncatePut before' truncated after
              Just deleted ->
                let putAfter = Map.findWithDefault mempty h after <> bs
                    after'   = Map.insert h putAfter after in
                if deleted /= BS.take (BS.length deleted) putAfter
                  then Left  $ TagPutTruncatePut
                  else Right $ tagPutTruncatePut before truncated after'
          Truncate (Handle h _) sz | sz > 0 ->
            let putBefore  = Map.findWithDefault mempty h before
                (putBefore', deleted) = BS.splitAt (fromIntegral sz) putBefore
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

    tagConcurrentWriterReader :: Map HandleMock (Set HandleMock) -> EventPred
    tagConcurrentWriterReader put = successful $ \ev@Event{..} _ ->
        case eventMockCmd ev of
          Put (Handle h fp) bs | BS.length bs > 0 ->
            -- Remember the other handles to the same file open at this time
            let readHs :: Set HandleMock
                readHs = Set.fromList
                       $ map handleRaw
                       $ filter (\(Handle h' fp') -> h /= h' && fp == fp')
                       $ openHandles eventBefore

                put' :: Map HandleMock (Set HandleMock)
                put' = Map.alter (Just . maybe readHs (Set.union readHs)) h put

            in Right $ tagConcurrentWriterReader put'
          Close (Handle h _) ->
            Right $ tagConcurrentWriterReader (Map.delete h put)
          Get (Handle h _) n | h `elem` Set.unions (Map.elems put), n > 0 ->
            Left TagConcurrentWriterReader
          GetAt (Handle h _) n _ | h `elem` Set.unions (Map.elems put), n > 0 ->
            Left TagConcurrentWriterReader
          _otherwise ->
            Right $ tagConcurrentWriterReader put

    tagOpenReadThenWrite :: Set FsPath -> EventPred
    tagOpenReadThenWrite readOpen = successful $ \ev _ ->
      case eventMockCmd ev of
        Open (PExpPath fp) ReadMode ->
          Right $ tagOpenReadThenWrite $ Set.insert fp readOpen
        Open (PExpPath fp) (WriteMode _) | Set.member fp readOpen ->
          Left TagOpenReadThenWrite
        _otherwise -> Right $ tagOpenReadThenWrite readOpen

    tagOpenReadThenRead :: Set FsPath -> EventPred
    tagOpenReadThenRead readOpen = successful $ \ev _ ->
      case eventMockCmd ev of
        Open (PExpPath fp) ReadMode | Set.member fp readOpen ->
          Left TagOpenReadThenRead
        Open (PExpPath fp) ReadMode ->
          Right $ tagOpenReadThenRead $ Set.insert fp readOpen
        _otherwise -> Right $ tagOpenReadThenRead readOpen

    tagWriteWriteRead :: Map (HandleMock, FsPath) Int -> EventPred
    tagWriteWriteRead wr = successful $ \ev _ ->
      case eventMockCmd ev of
        Put (Handle h fp) bs | BS.length bs > 0 ->
          let f Nothing  = Just 0
              f (Just x) = Just $ x + 1
          in Right $ tagWriteWriteRead $ Map.alter f (h, fp) wr
        Get (Handle hRead fp) n | n > 1 ->
          if not $ Map.null $ Map.filterWithKey (\(hWrite, fp') times -> fp' == fp && times > 1 && not (hWrite == hRead)) wr
          then Left TagWriteWriteRead
          else Right $ tagWriteWriteRead wr
        GetAt (Handle hRead fp) n _ | n > 1 ->
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
    tagWrite = successful $ \ev _ ->
      case eventMockCmd ev of
        Put _ bs | BS.length bs > 0 ->
          Left TagWrite
        _otherwise -> Right tagWrite

    tagSeekFromEnd :: EventPred
    tagSeekFromEnd = successful $ \ev _ ->
      case eventMockCmd ev of
        Seek _ SeekFromEnd n | n < 0 -> Left TagSeekFromEnd
        _otherwise                   -> Right tagSeekFromEnd

    tagCreateDirectory :: EventPred
    tagCreateDirectory = successful $ \ev _ ->
      case eventMockCmd ev of
        CreateDirIfMissing True (PExpPath fp) | length (fsPathToList fp) > 1 ->
          Left TagCreateDirectory
        _otherwise ->
          Right tagCreateDirectory

    tagDoesFileExistOK :: EventPred
    tagDoesFileExistOK = successful $ \ev suc ->
      case (eventMockCmd ev, suc) of
        (DoesFileExist _, Bool True) -> Left TagDoesFileExistOK
        _otherwise                   -> Right tagDoesFileExistOK

    tagDoesFileExistKO :: EventPred
    tagDoesFileExistKO = successful $ \ev suc ->
      case (eventMockCmd ev, suc) of
        (DoesFileExist _, Bool False) -> Left TagDoesFileExistKO
        _otherwise                    -> Right tagDoesFileExistKO

    tagDoesDirectoryExistOK :: EventPred
    tagDoesDirectoryExistOK = successful $ \ev suc ->
      case (eventMockCmd ev, suc) of
        (DoesDirectoryExist (PExpPath fp), Bool True) | not (fp == mkFsPath ["/"])
                   -> Left TagDoesDirectoryExistOK
        _otherwise -> Right tagDoesDirectoryExistOK

    tagDoesDirectoryExistKO :: EventPred
    tagDoesDirectoryExistKO = successful $ \ev suc ->
      case (eventMockCmd ev, suc) of
        (DoesDirectoryExist _, Bool False) -> Left TagDoesDirectoryExistKO
        _otherwise                         -> Right tagDoesDirectoryExistKO

    tagRemoveFile :: Set FsPath -> EventPred
    tagRemoveFile removed = successful $ \ev _suc ->
      case eventMockCmd ev of
        RemoveFile fe -> Right $ tagRemoveFile $ Set.insert fp removed
          where
            fp = evalPathExpr fe
        DoesFileExist fe -> if Set.member fp removed
          then Left TagRemoveFile
          else Right $ tagRemoveFile removed
            where
              fp = evalPathExpr fe
        _otherwise -> Right $ tagRemoveFile removed

    tagRenameFile :: EventPred
    tagRenameFile = successful $ \ev _suc ->
      case eventMockCmd ev of
        RenameFile {} -> Left TagRenameFile
        _otherwise    -> Right tagRenameFile

    tagClosedTwice :: Set HandleMock -> EventPred
    tagClosedTwice closed = successful $ \ev _suc ->
      case eventMockCmd ev of
        Close (Handle h _) | Set.member h closed -> Left TagClosedTwice
        Close (Handle h _) -> Right $ tagClosedTwice $ Set.insert h closed
        _otherwise -> Right $ tagClosedTwice closed

    -- this never succeeds because of an fsLimitation
    tagReadInvalid :: Set HandleMock -> EventPred
    tagReadInvalid openAppend = C.predicate $ \ev ->
      case (eventMockCmd ev, eventMockResp ev) of
        (Open _ (AppendMode _), Resp (Right (WHandle _ (Handle h _)))) ->
          Right $ tagReadInvalid $ Set.insert h openAppend
        (Close (Handle h _), Resp (Right _)) ->
          Right $ tagReadInvalid $ Set.delete h openAppend
        (Get (Handle h _) _, Resp (Left _)) | Set.member h openAppend ->
          Left TagReadInvalid
        (GetAt (Handle h _) _ _, Resp (Left _)) | Set.member h openAppend ->
          Left TagReadInvalid
        _otherwise -> Right $ tagReadInvalid openAppend

    tagWriteInvalid :: Set HandleMock -> EventPred
    tagWriteInvalid openRead = C.predicate $ \ev ->
      case (eventMockCmd ev, eventMockResp ev) of
        (Open _ ReadMode, Resp (Right (RHandle (Handle h _)))) ->
          Right $ tagWriteInvalid $ Set.insert h openRead
        (Close (Handle h _), Resp (Right _)) ->
          Right $ tagWriteInvalid $ Set.delete h openRead
        (Put (Handle h _) _, _) | Set.member h openRead ->
          Left TagWriteInvalid
        _otherwise -> Right $ tagWriteInvalid openRead

    tagPutSeekGet :: Set HandleMock -> Set HandleMock -> EventPred
    tagPutSeekGet put seek = successful $ \ev _suc ->
      case eventMockCmd ev of
        Put (Handle h _) bs | BS.length bs > 0 ->
          Right $ tagPutSeekGet (Set.insert h put) seek
        Seek (Handle h _) RelativeSeek n | n > 0 && Set.member h put ->
          Right $ tagPutSeekGet put (Set.insert h seek)
        Get (Handle h _) n | n > 0 && Set.member h seek ->
          Left TagPutSeekGet
        GetAt (Handle h _) n _ | n > 0 && Set.member h seek ->
          Left TagPutSeekGet
        _otherwise -> Right $ tagPutSeekGet put seek

    tagPutSeekNegGet :: Set HandleMock -> Set HandleMock -> EventPred
    tagPutSeekNegGet put seek = successful $ \ev _suc ->
      case eventMockCmd ev of
        Put (Handle h _) bs | BS.length bs > 0 ->
          Right $ tagPutSeekNegGet (Set.insert h put) seek
        Seek (Handle h _) RelativeSeek n | n < 0 && Set.member h put ->
          Right $ tagPutSeekNegGet put (Set.insert h seek)
        Get (Handle h _) n | n > 0 && Set.member h seek ->
          Left TagPutSeekNegGet
        GetAt (Handle h _) n _ | n > 0 && Set.member h seek ->
          Left TagPutSeekNegGet
        _otherwise -> Right $ tagPutSeekNegGet put seek

    tagExclusiveFail :: EventPred
    tagExclusiveFail = C.predicate $ \ev ->
      case (eventMockCmd ev, eventMockResp ev) of
        (Open _ mode, Resp (Left fsError))
          | MustBeNew <- allowExisting mode
          , fsErrorType fsError == FsResourceAlreadyExist ->
            Left TagExclusiveFail
        _otherwise -> Right tagExclusiveFail

    tagReadEOF :: EventPred
    tagReadEOF = successful $ \ev suc ->
      case (eventMockCmd ev, suc) of
        (Get _ n, ByteString bl)
          | n > 0, BS.null bl -> Left  TagReadEOF
        _otherwise            -> Right tagReadEOF

    tagPread :: EventPred
    tagPread = successful $ \ev _ ->
      case eventMockCmd ev of
        GetAt{}    -> Left  TagPread
        _otherwise -> Right tagPread

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

deriving instance ToExpr HandleMock
deriving instance ToExpr MockFS
deriving instance ToExpr Mock.HandleState
deriving instance ToExpr Mock.OpenHandleState
deriving instance ToExpr Mock.ClosedHandleState
deriving instance ToExpr Mock.FilePtr
deriving instance ToExpr FsPath

instance ToExpr (Handle h) where
  toExpr = defaultExprViaShow

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
    QSM.forAllCommands (sm mountUnused) Nothing $ \cmds -> QC.monadicIO $ do
      (tstTmpDir, hist, res) <- QC.run $
        withTempDirectory tmpDir "HasFS" $ \tstTmpDir -> do
          let mount = MountPoint tstTmpDir
              sm'   = sm mount

          (hist, model, res) <- QSM.runCommands' sm' cmds

          -- Close all open handles
          forM_ (RE.keys (knownHandles model)) $ F.close . handleRaw . QSM.concrete

          return (tstTmpDir, hist, res)

      QSM.prettyCommands (sm mountUnused) hist
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
  condense (PExpPath     fp) = condense fp
  condense (PExpRef      fp) = condense fp
  condense (PExpParentOf fp) = condense fp ++ "/.."

instance (Condense fp, Condense h) => Condense (Cmd fp h) where
  condense = L.intercalate " " . go
    where
      go (Open fp mode)            = ["open", condense fp, condense mode]
      go (Close h)                 = ["close", condense h]
      go (IsOpen h)                = ["isOpen", condense h]
      go (Seek h mode o)           = ["seek", condense h, condense mode, condense o]
      go (Get h n)                 = ["get", condense h, condense n]
      go (GetAt h n o)             = ["getAt", condense h, condense n, condense o]
      go (Put h bs)                = ["put", condense h, condense bs]
      go (Truncate h sz)           = ["truncate", condense h, condense sz]
      go (GetSize h)               = ["getSize", condense h]
      go (CreateDir fp)            = ["createDir", condense fp]
      go (CreateDirIfMissing p fp) = ["createDirIfMissing", condense p, condense fp]
      go (ListDirectory fp)        = ["listDirectory", condense fp]
      go (DoesDirectoryExist fp)   = ["doesDirectoryExist", condense fp]
      go (DoesFileExist fp)        = ["doesFileExist", condense fp]
      go (RemoveFile fp)           = ["removeFile", condense fp]
      go (RenameFile fp1 fp2)      = ["renameFile", condense fp1, condense fp2]

instance Condense1 r => Condense (Cmd :@ r) where
  condense (At cmd) = condense cmd

instance Condense Tag where
  condense = show

instance Condense AbsOffset where
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
