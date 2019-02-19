{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | 'HasFS' instance wrapping 'SimFS' that generates errors, suitable for
-- testing error handling.
module Test.Ouroboros.Storage.FS.Sim.Error
  ( -- * Simulate Errors monad
    SimErrorFS
  , runSimErrorFS
  , mkSimErrorHasFS
  , withErrors
  , Buffer(MockBufferUnused)
    -- * Streams
  , Stream(..)
  , streamShrink
  , mkStream
  , runStream
  , always
  , mkStreamGen
  , ErrorStream
    -- * Generating corruption for 'hPut'
  , PutCorruption(..)
  , corrupt
    -- * Error streams for 'HasFS'
  , Errors(..)
  , simpleErrors
    -- * Testing examples
  , mockErrorDemo
  ) where

import           Control.Monad (replicateM, void)
import           Control.Monad.Catch (MonadCatch(..), MonadMask(..),
                                      MonadThrow(..))
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSTM (MonadSTM(..))
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Unlift (MonadIO(liftIO),
                                          MonadUnliftIO(withRunInIO),
                                          wrappedWithRunInIO)
import           Control.Monad.Reader (ReaderT(ReaderT), ask, runReaderT)
import           Control.Monad.State (MonadState(state, get))
import           Control.Monad.Trans (MonadTrans(lift))

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import           Data.List (intercalate, dropWhileEnd)
import           Data.Maybe (isNothing, catMaybes)
import           Data.Proxy (Proxy(..))
import           Data.Type.Coercion (Coercion(..))
import           Data.Word (Word64)

import           GHC.Stack (HasCallStack, callStack)

import           Test.QuickCheck (Gen, Arbitrary(..))
import qualified Test.QuickCheck as QC

import           Ouroboros.Consensus.Util (whenJust)

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Example (example)
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.Sim.MockFS (MockFS, Handle, handleFsPath)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import           Ouroboros.Storage.FS.Sim.STM (SimFS)
import qualified Ouroboros.Storage.FS.Sim.STM as Sim
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Ouroboros.Storage.Util (Blob(..))



-- TODO what about FsUnexpectedException? Should we generate those too?


{-------------------------------------------------------------------------------
  Streams
-------------------------------------------------------------------------------}

-- | A 'Stream' is a possibly infinite stream of @'Maybe' a@s.
newtype Stream a = Stream { getStream :: [Maybe a] }
    deriving (Show, Functor)

instance Semigroup (Stream a) where
  Stream s1 <> Stream s2 = Stream (zipWith pickLast s1 s2)
    where
      pickLast (Just x) Nothing = Just x
      pickLast _        mbY     = mbY

instance Monoid (Stream a) where
  mempty  = Stream (repeat Nothing)
  mappend = (<>)

-- | Shrink the given 'Stream' by shrinking the stream itself, not the
-- individual elements of the stream.
--
-- __Note:__ Only works with finite 'Stream's.
streamShrink :: Stream a -> [Stream a]
streamShrink (Stream es) = Stream <$> QC.shrinkList (const []) es

-- | Create a 'Stream' based on the given possibly infinite list of @'Maybe'
-- a@s.
mkStream :: [Maybe a] -> Stream a
mkStream = Stream

-- | Advance the 'Stream'. Return the @'Maybe' a@ and the remaining 'Stream'.
runStream :: Stream a -> (Maybe a, Stream a)
runStream s@(Stream [])     = (Nothing, s)
runStream   (Stream (a:as)) = (a, Stream as)

-- | Make a 'Stream' that always generates the given @a@.
always :: a -> Stream a
always a = Stream (repeat (Just a))

-- | Make a 'Stream' generator based on a @a@ generator.
--
-- The generator generates a finite stream of 10 elements, where each element
-- has a chance of being either 'Nothing' or an element generated with the
-- given @a@ generator (wrapped in a 'Just').
--
-- The first argument is the likelihood (as used by 'QC.frequency') of a
-- 'Just' where 'Nothing' has likelihood 2.
mkStreamGen :: Int -> Gen a -> Gen (Stream a)
mkStreamGen justLikelihood genA =
    mkStream . dropWhileEnd isNothing <$> replicateM 10 mbGenA
  where
    mbGenA = QC.frequency
      [ (2, return Nothing)
      , (justLikelihood, Just <$> genA)
      ]

-- | An 'ErrorStream' is a possibly infinite 'Stream' of (@Maybe@)
-- @'FsErrorType'@s.
--
-- 'Nothing' indicates that there is no error.
--
-- Each time the 'ErrorStream' is used (see 'runErrorStream'), the first
-- element ('Nothing' in case the list is empty) is taken from the list and an
-- 'ErrorStream' with the remainder of the list is returned. The first element
-- represents whether an error should be returned or not.
--
-- An 'FsError' consists of a number of fields: 'fsErrorType', a
-- 'fsErrorPath', etc. Only the first fields is interesting. Therefore, we
-- only generate the 'FsErrorType'. The 'FsErrorType' will be used to
-- construct the actual 'FsError'.
type ErrorStream = Stream FsErrorType


{-------------------------------------------------------------------------------
  Generating corruption for hPut
-------------------------------------------------------------------------------}

-- | Model possible corruptions that could happen to a 'hPut' call.
data PutCorruption
    = SubstituteWithJunk Blob
      -- ^ The blob to write is substituted with corrupt junk
    | DropRatioLastBytes Int
      -- ^ Drop a number of bytes from the end of a blob where the number is
      -- @n/10@ of the total number bytes the blob is long.
      --
      -- As we don't know how long the blobs will be, using an absolute number
      -- makes less sense, as we might end up dropping everything from each
      -- blob.

-- When used as part of a QuickCheck tag, it's better to not show the
-- Blob/Int, otherwise we get a separate tag for each Blob/Int.
instance Show PutCorruption where
  show (SubstituteWithJunk _) = "SubstituteWithJunk"
  show (DropRatioLastBytes _) = "DropRatioLastBytes"

instance Arbitrary PutCorruption where
  arbitrary = QC.oneof
      [ SubstituteWithJunk <$> arbitrary
      , DropRatioLastBytes <$> QC.choose (0, 10)
      ]
  shrink (SubstituteWithJunk blob) =
      [SubstituteWithJunk blob' | blob' <- shrink blob]
  shrink (DropRatioLastBytes tenths) =
      [DropRatioLastBytes tenths'  | tenths' <- shrink tenths]

-- | Apply the 'PutCorruption' to the 'Builder'.
corrupt :: Builder -> PutCorruption -> Builder
corrupt bld pc = case pc of
    SubstituteWithJunk blob   -> getBlob blob
    DropRatioLastBytes tenths -> BS.lazyByteString bs'
      where
        bs = BS.toLazyByteString bld
        toTake :: Double
        toTake = fromIntegral (BL.length bs) * fromIntegral (10 - tenths) / 10.0
        bs' = BL.take (round toTake) bs

-- | 'ErrorStream' with possible 'PutCorruption'.
--
-- We use a @Maybe@ because not every error needs to be accompanied with data
-- corruption.
type ErrorStreamWithCorruption = Stream (FsErrorType, Maybe PutCorruption)

{-------------------------------------------------------------------------------
  Simulated errors
-------------------------------------------------------------------------------}

-- | Error streams for the methods of the 'HasFS' type class.
--
-- An 'ErrorStream' is provided for each method of the 'HasFS' type class.
-- This 'ErrorStream' will be used to generate potential errors that will be
-- thrown by the corresponding method.
--
-- For 'hPut', an 'ErrorStreamWithCorruption' is provided to simulate
-- corruption.
--
-- An 'ErrorStreams' is used in conjunction with 'SimErrorFS', which is a layer
-- on top of 'SimFS' that simulates methods throwing 'FsError's.
data Errors = Errors
  { _newBuffer :: ErrorStream
  , _dumpState :: ErrorStream

    -- Operations on files
  , _hOpen      :: ErrorStream
  , _hClose     :: ErrorStream
  , _hSeek      :: ErrorStream
  , _hGet       :: ErrorStream
  , _hPut       :: ErrorStreamWithCorruption
  , _hPutBuffer :: ErrorStream
  , _hTruncate  :: ErrorStream
  , _hGetSize   :: ErrorStream

    -- Operations on directories
  , _createDirectory          :: ErrorStream
  , _createDirectoryIfMissing :: ErrorStream
  , _listDirectory            :: ErrorStream
  , _doesDirectoryExist       :: ErrorStream
  , _doesFileExist            :: ErrorStream
  , _removeFile               :: ErrorStream
  }

instance Show Errors where
  show Errors {..} =
      "Errors {"  <> intercalate ", " streams <> "}"
    where
      -- | Show a stream unless it is empty
      s :: Show a => String -> Stream a -> Maybe String
      s _   (Stream []) = Nothing
      -- While the testsuite currently never prints an infinite streams, it's
      -- better to protect us against it when it were to happen accidentally.
      s fld (Stream xs) = Just $ fld <> " = " <> show (take 50 xs)

      streams :: [String]
      streams = catMaybes
        [ s "_newBuffer"                _newBuffer
        , s "_dumpState"                _dumpState
        , s "_hOpen"                    _hOpen
        , s "_hClose"                   _hClose
        , s "_hSeek"                    _hSeek
        , s "_hGet"                     _hGet
        , s "_hPut"                     _hPut
        , s "_hPutBuffer"               _hPutBuffer
        , s "_hTruncate"                _hTruncate
        , s "_hGetSize"                 _hGetSize
        , s "_createDirectory"          _createDirectory
        , s "_createDirectoryIfMissing" _createDirectoryIfMissing
        , s "_listDirectory"            _listDirectory
        , s "_doesDirectoryExist"       _doesDirectoryExist
        , s "_doesFileExist"            _doesFileExist
        , s "_removeFile"               _removeFile
        ]

instance Semigroup Errors where
  egs1 <> egs2 = Errors
      { _newBuffer                = combine _newBuffer
      , _dumpState                = combine _dumpState
      , _hOpen                    = combine _hOpen
      , _hClose                   = combine _hClose
      , _hSeek                    = combine _hSeek
      , _hGet                     = combine _hGet
      , _hPut                     = combine _hPut
      , _hPutBuffer               = combine _hPutBuffer
      , _hTruncate                = combine _hTruncate
      , _hGetSize                 = combine _hGetSize
      , _createDirectory          = combine _createDirectory
      , _createDirectoryIfMissing = combine _createDirectoryIfMissing
      , _listDirectory            = combine _listDirectory
      , _doesDirectoryExist       = combine _doesDirectoryExist
      , _doesFileExist            = combine _doesFileExist
      , _removeFile               = combine _removeFile
      }
    where
      combine :: (Errors -> Stream a) -> Stream a
      combine f = f egs1 <> f egs2

instance Monoid Errors where
  mappend = (<>)
  mempty = simpleErrors mempty

-- | Use the given 'ErrorStream' for each field/method. No corruption of
-- 'hPut'.
simpleErrors :: ErrorStream -> Errors
simpleErrors es = Errors
    { _newBuffer                = es
    , _dumpState                = es
    , _hOpen                    = es
    , _hClose                   = es
    , _hSeek                    = es
    , _hGet                     = es
    , _hPut                     = fmap (, Nothing) es
    , _hPutBuffer               = es
    , _hTruncate                = es
    , _hGetSize                 = es
    , _createDirectory          = es
    , _createDirectoryIfMissing = es
    , _listDirectory            = es
    , _doesDirectoryExist       = es
    , _doesFileExist            = es
    , _removeFile               = es
    }

instance Arbitrary Errors where
  arbitrary = do
    let streamGen l = mkStreamGen l . QC.elements
        -- TODO which errors are possible for these operations below (that
        -- have dummy for now)?
        dummy = streamGen 2 [ FsInsufficientPermissions ]
    _newBuffer          <- dummy
    _dumpState          <- dummy
    -- TODO let this one fail:
    let _hClose = mkStream []
    _hTruncate          <- dummy
    _doesDirectoryExist <- dummy
    _doesFileExist      <- dummy
    _hOpen <- streamGen 1
      [ FsResourceDoesNotExist, FsResourceInappropriateType
      , FsResourceAlreadyInUse, FsResourceAlreadyExist
      , FsInsufficientPermissions ]
    _hSeek      <- streamGen 3 [ FsReachedEOF ]
    _hGet       <- streamGen 3 [ FsReachedEOF ]
    _hPut       <- mkStreamGen 2 $ (,) <$> return FsDeviceFull <*> arbitrary
    _hPutBuffer <- streamGen 3 [ FsDeviceFull ]
    _hGetSize   <- streamGen 2 [ FsResourceDoesNotExist ]
    _createDirectory <- streamGen 3
      [ FsInsufficientPermissions, FsResourceInappropriateType
      , FsResourceAlreadyExist ]
    _createDirectoryIfMissing <- streamGen 3
      [ FsInsufficientPermissions, FsResourceInappropriateType
      , FsResourceAlreadyExist ]
    _listDirectory <- streamGen 3
      [ FsInsufficientPermissions, FsResourceInappropriateType
      , FsResourceDoesNotExist ]
    _removeFile    <- streamGen 3
      [ FsInsufficientPermissions, FsResourceAlreadyInUse
      , FsResourceDoesNotExist, FsResourceInappropriateType ]
    return Errors {..}

  shrink err@Errors {..} = catMaybes
      [ (\s' -> err { _newBuffer = s' })                <$> dropLast _newBuffer
      , (\s' -> err { _dumpState = s' })                <$> dropLast _dumpState
      , (\s' -> err { _hOpen = s' })                    <$> dropLast _hOpen
      , (\s' -> err { _hClose = s' })                   <$> dropLast _hClose
      , (\s' -> err { _hSeek = s' })                    <$> dropLast _hSeek
      , (\s' -> err { _hGet = s' })                     <$> dropLast _hGet
      , (\s' -> err { _hPut = s' })                     <$> dropLast _hPut
      , (\s' -> err { _hPutBuffer = s' })               <$> dropLast _hPutBuffer
      , (\s' -> err { _hTruncate = s' })                <$> dropLast _hTruncate
      , (\s' -> err { _hGetSize = s' })                 <$> dropLast _hGetSize
      , (\s' -> err { _createDirectory = s' })          <$> dropLast _createDirectory
      , (\s' -> err { _createDirectoryIfMissing = s' }) <$> dropLast _createDirectoryIfMissing
      , (\s' -> err { _listDirectory = s' })            <$> dropLast _listDirectory
      , (\s' -> err { _doesDirectoryExist = s' })       <$> dropLast _doesDirectoryExist
      , (\s' -> err { _doesFileExist = s' })            <$> dropLast _doesFileExist
      , (\s' -> err { _removeFile = s' })               <$> dropLast _removeFile
      ]
    where
      dropLast :: Stream a -> Maybe (Stream a)
      dropLast (Stream []) = Nothing
      dropLast (Stream xs) = Just $ Stream $ zipWith const xs (drop 1 xs)


{-------------------------------------------------------------------------------
  Simulate Errors monad
-------------------------------------------------------------------------------}

newtype SimErrorFS m a =
  SimErrorFS { unSimErrorFS :: ReaderT (TVar m Errors) (SimFS m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadThrow
           , MonadCatch
           , MonadMask
           , MonadFork
           )

type instance FsHandle (SimErrorFS m) = FsHandle (SimFS m)
data instance Buffer   (SimErrorFS m) = MockBufferUnused

instance MonadTrans SimErrorFS where
  lift m = liftSim (lift m)

instance MonadIO m => MonadIO (SimErrorFS m) where
  liftIO = lift . liftIO

newtype TrSimErrorFS m a = TrSimErrorFS { trSimErrorFS :: m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans TrSimErrorFS where
  lift = TrSimErrorFS

instance MonadUnliftIO m => MonadUnliftIO (SimErrorFS m) where
  withRunInIO = wrappedWithRunInIO SimErrorFS unSimErrorFS

instance (MonadSTM (SimErrorFS m) , MonadSTM m) => MonadSTM (SimErrorFS m) where
  type Tr (SimErrorFS m)      = TrSimErrorFS (Tr m)
  type TVar (SimErrorFS m)    = TVar m
  type TMVar (SimErrorFS m)   = TMVar m
  type TQueue (SimErrorFS m)  = TQueue m
  type TBQueue (SimErrorFS m) = TBQueue m

  atomically        = lift . atomically . trSimErrorFS
  newTVar           = lift . newTVar
  readTVar          = lift . readTVar
  writeTVar       t = lift . writeTVar t
  retry             = lift   retry

  newTMVar          = lift . newTMVar
  newTMVarM         = lift . newTMVarM
  newEmptyTMVar     = lift   newEmptyTMVar
  newEmptyTMVarM    = lift   newEmptyTMVarM
  takeTMVar         = lift . takeTMVar
  tryTakeTMVar      = lift . tryTakeTMVar
  putTMVar        t = lift . putTMVar    t
  tryPutTMVar     t = lift . tryPutTMVar t
  swapTMVar       t = lift . swapTMVar   t
  readTMVar         = lift . readTMVar
  tryReadTMVar      = lift . tryReadTMVar
  isEmptyTMVar      = lift . isEmptyTMVar

  newTQueue         = lift   newTQueue
  readTQueue        = lift . readTQueue
  tryReadTQueue     = lift . tryReadTQueue
  writeTQueue     q = lift . writeTQueue q
  isEmptyTQueue     = lift . isEmptyTQueue

  newTBQueue        = lift . newTBQueue
  readTBQueue       = lift . readTBQueue
  tryReadTBQueue    = lift . tryReadTBQueue
  writeTBQueue    q = lift . writeTBQueue q
  isEmptyTBQueue    = lift . isEmptyTBQueue
  isFullTBQueue     = lift . isFullTBQueue


instance (Monad m, MonadState MockFS (SimFS m))
  => MonadState MockFS (SimErrorFS m) where
  state f = liftSim (state f)


mkSimErrorHasFS :: forall m. MonadSTM m
                => HasFS (SimFS m) -> HasFS (SimErrorFS m)
mkSimErrorHasFS HasFS {..} = HasFS
    { newBuffer = \_ -> return MockBufferUnused
      -- Lenses would be nice for the setters
    , dumpState =
        withErr err ["<dumpState>"] dumpState "dumpState"
        _dumpState (\e es -> es { _dumpState = e })
    , hOpen      = \p m ->
        withErr err  p (hOpen p m) "hOpen"
        _hOpen (\e es -> es { _hOpen = e })
    , hClose     = \h ->
        withErr' err h (hClose h) "hClose"
        _hClose (\e es -> es { _hClose = e })
    , hSeek      = \h m n ->
        withErr' err h (hSeek h m n) "hSeek"
        _hSeek (\e es -> es { _hSeek = e })
    , hGet       = \h n ->
        withErr' err h (hGet h n) "hGet"
        _hGet (\e es -> es { _hGet = e })
    , hPut       = hPut' err hPut
    , hPutBuffer = \h _ d ->
        withErr' err h (hPutBuffer h Sim.MockBufferUnused d) "hPutBuffer"
        _hPutBuffer (\e es -> es { _hPutBuffer = e })
    , hTruncate  = \h w ->
        withErr' err h (hTruncate h w) "hTruncate"
        _hTruncate (\e es -> es { _hTruncate = e })
    , hGetSize   =  \h ->
        withErr' err h (hGetSize h) "hGetSize"
        _hGetSize (\e es -> es { _hGetSize = e })

    , createDirectory          = \p ->
        withErr err p (createDirectory p) "createDirectory"
        _createDirectory (\e es -> es { _createDirectory = e })
    , createDirectoryIfMissing = \b p ->
        withErr err p (createDirectoryIfMissing b p) "createDirectoryIfMissing"
        _createDirectoryIfMissing (\e es -> es { _createDirectoryIfMissing = e })
    , listDirectory            = \p ->
        withErr err p (listDirectory p) "listDirectory"
        _listDirectory (\e es -> es { _listDirectory = e })
    , doesDirectoryExist       = \p ->
        withErr err p (doesDirectoryExist p) "doesDirectoryExist"
        _doesDirectoryExist (\e es -> es { _doesDirectoryExist = e })
    , doesFileExist            = \p ->
        withErr err p (doesFileExist p) "doesFileExist"
        _doesFileExist (\e es -> es { _doesFileExist = e })
    , removeFile               = \p ->
        withErr err p (removeFile p) "removeFile"
        _removeFile (\e es -> es { _removeFile = e })
    , hasFsErr = err
    }
  where
    err = liftErrSimErrorFS hasFsErr


-- | Runs a 'SimErrorFS' computation provided an 'Errors' and an initial
-- 'MockFS', producing a result and the final state of the filesystem.
runSimErrorFS :: MonadSTM m
              => SimErrorFS m a
              -> MockFS
              -> Errors
              -> m (a, MockFS)
runSimErrorFS (SimErrorFS action) mockFS errors = do
    errorsVar <- atomically $ newTVar errors
    Sim.runSimFS (runReaderT action errorsVar) mockFS

-- | Execute the next action using the given 'Errors'. After the action is
-- finished, the previous 'Errors' are restored.
withErrors :: MonadSTM m => Errors -> SimErrorFS m a -> SimErrorFS m a
withErrors tempErrors action = do
    errorsVar <- SimErrorFS ask
    originalErrors <- atomically $ do
      originalErrors <- readTVar errorsVar
      writeTVar errorsVar tempErrors
      return originalErrors
    res <- action
    atomically $ writeTVar errorsVar originalErrors
    return res


{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

-- | Lift a 'SimFS' into a 'SimErrorFS'.
liftSim :: Monad m => SimFS m a -> SimErrorFS m a
liftSim = SimErrorFS . lift

liftErrSimErrorFS :: forall e m. ErrorHandling e (SimFS m)
                  -> ErrorHandling e (SimErrorFS m)
liftErrSimErrorFS = EH.liftErrNewtype Coercion
                  . EH.liftErrReader (Proxy @(TVar m Errors))

-- | Advance to the next error in the stream of some 'ErrorStream' in the
-- 'Errors' stored in 'SimErrorFSE'. Extracts the right error stream from the
-- state with the @getter@ and stores the advanced error stream in the state
-- with the @setter@.
next :: MonadSTM m
     => (Errors -> Stream a)            -- ^ @getter@
     -> (Stream a -> Errors -> Errors)  -- ^ @setter@
     -> SimErrorFS m (Maybe a)
next getter setter = do
    errorsVar <- SimErrorFS ask
    atomically $ do
      errors <- readTVar errorsVar
      let (mb, s') = runStream (getter errors)
      writeTVar errorsVar (setter s' errors)
      return mb

-- | Execute an action or throw an error, depending on the corresponding
-- 'ErrorStream' (see 'nextError').
withErr :: (MonadSTM m, HasCallStack)
        => ErrorHandling FsError (SimErrorFS m)
        -> FsPath     -- ^ The path for the error, if thrown
        -> SimFS m a  -- ^ Action in case no error is thrown
        -> String     -- ^ Extra message for in the 'fsErrorString'
        -> (Errors -> ErrorStream)           -- ^ @getter@
        -> (ErrorStream -> Errors -> Errors) -- ^ @setter@
        -> SimErrorFS m a
withErr ErrorHandling {..} path action msg getter setter = do
    mbErr <- next getter setter
    case mbErr of
      Nothing      -> liftSim action
      Just errType -> throwError FsError
        { fsErrorType   = errType
        , fsErrorPath   = path
        , fsErrorString = "simulated error: " <> msg
        , fsErrorStack  = callStack
        , fsLimitation  = False
        }

-- | Variant of 'withErr' that works with 'Handle's.
--
-- The path of the handle is retrieved from the 'MockFS' using 'handleFsPath'.
withErr' :: (MonadSTM m, HasCallStack)
         => ErrorHandling FsError (SimErrorFS m)
         -> Handle     -- ^ The handle to get the path for the error from, if
                       -- thrown
         -> SimFS m a  -- ^ Action in case no error is thrown
         -> String     -- ^ Extra message for in the 'fsErrorString'
         -> (Errors -> ErrorStream)           -- ^ @getter@
         -> (ErrorStream -> Errors -> Errors) -- ^ @setter@
         -> SimErrorFS m a
withErr' err handle action msg getter setter = do
    mockFS <- get
    withErr err (handleFsPath mockFS handle) action msg getter setter

-- | Execute the wrapped 'hPut' or throw an error and apply possible
-- corruption to the blob to write, depending on the corresponding
-- 'ErrorStreamWithCorruption' (see 'nextError').
hPut'  :: (MonadSTM m, HasCallStack)
       => ErrorHandling FsError (SimErrorFS m)
       -> (Handle -> Builder -> SimFS m Word64)  -- ^ Wrapped 'hPut'
       -> Handle -> Builder -> SimErrorFS m Word64
hPut' ErrorHandling{..} hPutWrapped handle bld = do
    mockFS <- get
    let path = handleFsPath mockFS handle
    mbErrMbCorr <- next _hPut (\e es -> es { _hPut = e })
    case mbErrMbCorr of
      Nothing      -> liftSim (hPutWrapped handle bld)
      Just (errType, mbCorr) -> do
        whenJust mbCorr $ \corr ->
          void $ liftSim (hPutWrapped handle (corrupt bld corr))
        throwError FsError
          { fsErrorType   = errType
          , fsErrorPath   = path
          , fsErrorString = "simulated error: hPut" <> case mbCorr of
              Nothing   -> ""
              Just corr -> " with corruption: " <> show corr
          , fsErrorStack  = callStack
          , fsLimitation  = False
          }


{-------------------------------------------------------------------------------
  Demo
-------------------------------------------------------------------------------}

mockErrorDemo :: IO ()
mockErrorDemo = do
    -- let errors = mempty
    -- let errors = simpleErrors $ alwaysError FsDeviceFull
    errors <- QC.generate arbitrary
    let hasFS = mkSimErrorHasFS (Sim.simHasFS EH.exceptT)
    res <- runExceptT $ runSimErrorFS (example hasFS) Mock.example errors
    case res of
      Left  err      -> putStrLn (prettyFsError err)
      Right (bs, fs) -> putStrLn (Mock.pretty fs) >> print bs
