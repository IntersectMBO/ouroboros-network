{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | 'HasFS' instance wrapping 'SimFS' that generates errors, suitable for
-- testing error handling.
module Test.Ouroboros.Storage.FS.Sim.Error
  ( -- * Simulate Errors monad
    runSimErrorFS
  , mkSimErrorHasFS
  , withErrors
    -- * Streams
  , Stream(..)
  , mkStream
  , null
  , runStream
  , always
  , mkStreamGen
  , ErrorStream
  , ErrorStreamGetSome
  , ErrorStreamPutSome
    -- * Generating partial reads/writes
  , Partial (..)
  , hGetSomePartial
  , hPutSomePartial
    -- * Generating corruption for 'hPutSome'
  , PutCorruption(..)
  , corrupt
    -- * Error streams for 'HasFS'
  , Errors(..)
  , genErrors
  , allNull
  , simpleErrors
    -- * Testing examples
  , mockErrorDemo
  ) where

import           Prelude hiding (null)

import           Control.Monad (replicateM, void)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Except (runExceptT)

import qualified Data.ByteString as BS
import           Data.List (dropWhileEnd, intercalate)
import           Data.Maybe (catMaybes, isNothing)
import           Data.Word (Word64)

import           GHC.Stack (HasCallStack, callStack)

import           Test.QuickCheck (Arbitrary (..), Gen)
import qualified Test.QuickCheck as QC

import           Ouroboros.Consensus.Util (whenJust)

import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Example (example)
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.FS.Sim.MockFS (HandleMock, MockFS)
import qualified Ouroboros.Storage.FS.Sim.MockFS as Mock
import qualified Ouroboros.Storage.FS.Sim.STM as Sim
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Ouroboros.Storage.Util (Blob (..))

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

-- | Return 'True' if the stream is empty.
--
-- A stream consisting of only 'Nothing's (even if it is only one) is not
-- considered to be empty.
null :: Stream a -> Bool
null (Stream []) = True
null _           = False

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
  Generating partial reads/writes for hGetSome and hPutSome
-------------------------------------------------------------------------------}

-- | Given a @'Partial' p@ where @p > 0@, we do the following to make a call
-- to 'hGetSome' or 'hPutSome' partial:
--
-- * 'hGetSome': we subtract @p@ from the number of requested bytes. If that
--    would result in 0 requested bytes or less, we request 1 byte. If the
--    number of requested bytes was already 0, leave it untouched, as we can't
--    simulate a partial read in this case.
-- * 'hPutSome': we drop the last @p@ bytes from the bytestring. If that would
--   result in an empty bytestring, just take the first byte of the
--   bytestring. If the bytestring was already empty, leave it untouched, as
--   we can't simulate a partial write in this case.
newtype Partial = Partial Word64
    deriving (Show)

instance Arbitrary Partial where
  arbitrary = Partial <$> QC.choose (1, 100)
  shrink (Partial p) =
    [Partial p' | p' <- [1..p]]

hGetSomePartial :: Partial -> Word64 -> Word64
hGetSomePartial (Partial p) n
    | 0 <- n    = 0
    | p >= n    = 1
    | otherwise = n - p

hPutSomePartial :: Partial -> BS.ByteString -> BS.ByteString
hPutSomePartial (Partial p) bs
    | 0 <- len  = bs
    | p >= len  = BS.take 1 bs
    | otherwise = BS.take (fromIntegral (len - p)) bs
  where
    len = fromIntegral (BS.length bs)

-- | 'ErrorStream' for 'hGetSome': an error or a partial get.
type ErrorStreamGetSome = Stream (Either FsErrorType Partial)

{-------------------------------------------------------------------------------
  Generating corruption for hPutSome
-------------------------------------------------------------------------------}

-- | Model possible corruptions that could happen to a 'hPutSome' call.
data PutCorruption
    = SubstituteWithJunk Blob
      -- ^ The blob to write is substituted with corrupt junk
    | PartialWrite Partial
      -- ^ Only perform the write partially
    deriving (Show)

instance Arbitrary PutCorruption where
  arbitrary = QC.oneof
      [ SubstituteWithJunk <$> arbitrary
      , PartialWrite <$> arbitrary
      ]
  shrink (SubstituteWithJunk blob) =
      [SubstituteWithJunk blob' | blob' <- shrink blob]
  shrink (PartialWrite partial) =
      [PartialWrite partial' | partial' <- shrink partial]

-- | Apply the 'PutCorruption' to the 'BS.ByteString'.
corrupt :: BS.ByteString -> PutCorruption -> BS.ByteString
corrupt bs pc = case pc of
    SubstituteWithJunk blob -> getBlob blob
    PartialWrite partial    -> hPutSomePartial partial bs

-- | 'ErrorStream' for 'hPutSome': an error and possibly some corruption, or a
-- partial write.
type ErrorStreamPutSome =
  Stream (Either (FsErrorType, Maybe PutCorruption) Partial)

{-------------------------------------------------------------------------------
  Simulated errors
-------------------------------------------------------------------------------}

-- | Error streams for the methods of the 'HasFS' type class.
--
-- An 'ErrorStream' is provided for each method of the 'HasFS' type class.
-- This 'ErrorStream' will be used to generate potential errors that will be
-- thrown by the corresponding method.
--
-- For 'hPutSome', an 'ErrorStreamWithCorruption' is provided to simulate
-- corruption.
--
-- An 'Errors' is used in conjunction with 'SimErrorFS', which is a layer on
-- top of 'SimFS' that simulates methods throwing 'FsError's.
data Errors = Errors
  { _dumpState                :: ErrorStream -- TODO remove

    -- Operations on files
  , _hOpen                    :: ErrorStream
  , _hClose                   :: ErrorStream
  , _hSeek                    :: ErrorStream
  , _hGetSome                 :: ErrorStreamGetSome
  , _hPutSome                 :: ErrorStreamPutSome
  , _hTruncate                :: ErrorStream
  , _hGetSize                 :: ErrorStream

    -- Operations on directories
  , _createDirectory          :: ErrorStream
  , _createDirectoryIfMissing :: ErrorStream
  , _listDirectory            :: ErrorStream
  , _doesDirectoryExist       :: ErrorStream
  , _doesFileExist            :: ErrorStream
  , _removeFile               :: ErrorStream
  }

-- | Return 'True' if all streams are empty ('null').
allNull :: Errors -> Bool
allNull Errors {..} = null _dumpState
                   && null _hOpen
                   && null _hClose
                   && null _hSeek
                   && null _hGetSome
                   && null _hPutSome
                   && null _hTruncate
                   && null _hGetSize
                   && null _createDirectory
                   && null _createDirectoryIfMissing
                   && null _listDirectory
                   && null _doesDirectoryExist
                   && null _doesFileExist
                   && null _removeFile


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
        [ s "_dumpState"                _dumpState
        , s "_hOpen"                    _hOpen
        , s "_hClose"                   _hClose
        , s "_hSeek"                    _hSeek
        , s "_hGetSome"                 _hGetSome
        , s "_hPutSome"                 _hPutSome
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
      { _dumpState                = combine _dumpState
      , _hOpen                    = combine _hOpen
      , _hClose                   = combine _hClose
      , _hSeek                    = combine _hSeek
      , _hGetSome                 = combine _hGetSome
      , _hPutSome                 = combine _hPutSome
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
-- 'hPutSome'.
simpleErrors :: ErrorStream -> Errors
simpleErrors es = Errors
    { _dumpState                = es
    , _hOpen                    = es
    , _hClose                   = es
    , _hSeek                    = es
    , _hGetSome                 =  Left                <$> es
    , _hPutSome                 = (Left . (, Nothing)) <$> es
    , _hTruncate                = es
    , _hGetSize                 = es
    , _createDirectory          = es
    , _createDirectoryIfMissing = es
    , _listDirectory            = es
    , _doesDirectoryExist       = es
    , _doesFileExist            = es
    , _removeFile               = es
    }

-- | Generator for 'Errors' that allows some things to be disabled.
--
-- This is needed by the VolatileDB state machine tests, which try to predict
-- what should happen based on the 'Errors', which is too complex sometimes.
genErrors :: Bool  -- ^ 'True' -> generate partial writes
          -> Bool  -- ^ 'True' -> generate 'SubstituteWithJunk' corruptions
          -> Gen Errors
genErrors genPartialWrites genSubstituteWithJunk = do
    let streamGen l = mkStreamGen l . QC.elements
        -- TODO which errors are possible for these operations below (that
        -- have dummy for now)?
        dummy = streamGen 2 [ FsInsufficientPermissions ]
    _dumpState          <- dummy
    -- TODO let this one fail:
    let _hClose = mkStream []
    _hTruncate          <- dummy
    _doesDirectoryExist <- dummy
    _doesFileExist      <- dummy
    _hOpen <- streamGen 1
      [ FsResourceDoesNotExist, FsResourceInappropriateType
      , FsResourceAlreadyInUse, FsResourceAlreadyExist
      , FsInsufficientPermissions, FsTooManyOpenFiles ]
    _hSeek      <- streamGen 3 [ FsReachedEOF ]
    _hGetSome   <- mkStreamGen 20 $ QC.frequency
      [ (1, return $ Left FsReachedEOF)
      , (3, Right <$> arbitrary) ]
    _hPutSome   <- mkStreamGen 5 $ QC.frequency
      [ (1, Left . (FsDeviceFull, ) <$> QC.frequency
            [ (2, return Nothing)
            , (1, Just . PartialWrite <$> arbitrary)
            , (if genSubstituteWithJunk then 1 else 0,
               Just . SubstituteWithJunk <$> arbitrary)
            ])
      , (if genPartialWrites then 3 else 0, Right <$> arbitrary) ]
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

instance Arbitrary Errors where
  arbitrary = genErrors True True

  shrink err@Errors {..} = filter (not . allNull) $ catMaybes
      [ (\s' -> err { _dumpState = s' })                <$> dropLast _dumpState
      , (\s' -> err { _hOpen = s' })                    <$> dropLast _hOpen
      , (\s' -> err { _hClose = s' })                   <$> dropLast _hClose
      , (\s' -> err { _hSeek = s' })                    <$> dropLast _hSeek
      , (\s' -> err { _hGetSome = s' })                 <$> dropLast _hGetSome
      , (\s' -> err { _hPutSome = s' })                 <$> dropLast _hPutSome
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

-- | Introduce possibility of errors
--
-- TODO: Lenses would be nice for the setters
mkSimErrorHasFS :: forall m. MonadSTM m
                => ErrorHandling FsError m
                -> StrictTVar m MockFS
                -> StrictTVar m Errors
                -> HasFS m HandleMock
mkSimErrorHasFS err fsVar errorsVar =
    case Sim.simHasFS err fsVar of
      HasFS{..} -> HasFS{
          dumpState =
            withErr err errorsVar (mkFsPath ["<dumpState>"]) dumpState "dumpState"
              _dumpState (\e es -> es { _dumpState = e })
        , hOpen      = \p m ->
            withErr err errorsVar p (hOpen p m) "hOpen"
            _hOpen (\e es -> es { _hOpen = e })
        , hClose     = \h ->
            withErr' err errorsVar h (hClose h) "hClose"
            _hClose (\e es -> es { _hClose = e })
        , hSeek      = \h m n ->
            withErr' err errorsVar h (hSeek h m n) "hSeek"
            _hSeek (\e es -> es { _hSeek = e })
        , hGetSome   = hGetSome' err errorsVar hGetSome
        , hPutSome   = hPutSome' err errorsVar hPutSome
        , hTruncate  = \h w ->
            withErr' err errorsVar h (hTruncate h w) "hTruncate"
            _hTruncate (\e es -> es { _hTruncate = e })
        , hGetSize   =  \h ->
            withErr' err errorsVar h (hGetSize h) "hGetSize"
            _hGetSize (\e es -> es { _hGetSize = e })

        , createDirectory          = \p ->
            withErr err errorsVar p (createDirectory p) "createDirectory"
            _createDirectory (\e es -> es { _createDirectory = e })
        , createDirectoryIfMissing = \b p ->
            withErr err errorsVar p (createDirectoryIfMissing b p) "createDirectoryIfMissing"
            _createDirectoryIfMissing (\e es -> es { _createDirectoryIfMissing = e })
        , listDirectory            = \p ->
            withErr err errorsVar p (listDirectory p) "listDirectory"
            _listDirectory (\e es -> es { _listDirectory = e })
        , doesDirectoryExist       = \p ->
            withErr err errorsVar p (doesDirectoryExist p) "doesDirectoryExist"
            _doesDirectoryExist (\e es -> es { _doesDirectoryExist = e })
        , doesFileExist            = \p ->
            withErr err errorsVar p (doesFileExist p) "doesFileExist"
            _doesFileExist (\e es -> es { _doesFileExist = e })
        , removeFile               = \p ->
            withErr err errorsVar p (removeFile p) "removeFile"
            _removeFile (\e es -> es { _removeFile = e })
        , hasFsErr = err
        }

-- | Runs a computation provided an 'Errors' and an initial
-- 'MockFS', producing a result and the final state of the filesystem.
runSimErrorFS :: MonadSTM m
              => ErrorHandling FsError m
              -> MockFS
              -> Errors
              -> (StrictTVar m Errors -> HasFS m HandleMock -> m a)
              -> m (a, MockFS)
runSimErrorFS err mockFS errors action = do
    fsVar     <- atomically $ newTVar mockFS
    errorsVar <- atomically $ newTVar errors
    a         <- action errorsVar $ mkSimErrorHasFS err fsVar errorsVar
    fs'       <- atomically $ readTVar fsVar
    return (a, fs')

-- | Execute the next action using the given 'Errors'. After the action is
-- finished, the previous 'Errors' are restored.
withErrors :: MonadSTM m => StrictTVar m Errors -> Errors -> m a -> m a
withErrors errorsVar tempErrors action = do
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

-- | Advance to the next error in the stream of some 'ErrorStream' in the
-- 'Errors' stored in the 'StrictTVar'. Extracts the right error stream from
-- the state with the @getter@ and stores the advanced error stream in the
-- state with the @setter@.
next :: MonadSTM m
     => StrictTVar m Errors
     -> (Errors -> Stream a)            -- ^ @getter@
     -> (Stream a -> Errors -> Errors)  -- ^ @setter@
     -> m (Maybe a)
next errorsVar getter setter = do
    atomically $ do
      errors <- readTVar errorsVar
      let (mb, s') = runStream (getter errors)
      writeTVar errorsVar (setter s' errors)
      return mb

-- | Execute an action or throw an error, depending on the corresponding
-- 'ErrorStream' (see 'nextError').
withErr :: (MonadSTM m, HasCallStack)
        => ErrorHandling FsError m
        -> StrictTVar m Errors
        -> FsPath     -- ^ The path for the error, if thrown
        -> m a        -- ^ Action in case no error is thrown
        -> String     -- ^ Extra message for in the 'fsErrorString'
        -> (Errors -> ErrorStream)           -- ^ @getter@
        -> (ErrorStream -> Errors -> Errors) -- ^ @setter@
        -> m a
withErr ErrorHandling {..} errorsVar path action msg getter setter = do
    mbErr <- next errorsVar getter setter
    case mbErr of
      Nothing      -> action
      Just errType -> throwError FsError
        { fsErrorType   = errType
        , fsErrorPath   = path
        , fsErrorString = "simulated error: " <> msg
        , fsErrorNo     = Nothing
        , fsErrorStack  = callStack
        , fsLimitation  = False
        }

-- | Variant of 'withErr' that works with 'Handle's.
--
-- The path of the handle is retrieved from the 'MockFS' using 'handleFsPath'.
withErr' :: (MonadSTM m, HasCallStack)
         => ErrorHandling FsError m
         -> StrictTVar m Errors
         -> Handle HandleMock   -- ^ The path for the error, if thrown
         -> m a        -- ^ Action in case no error is thrown
         -> String     -- ^ Extra message for in the 'fsErrorString'
         -> (Errors -> ErrorStream)           -- ^ @getter@
         -> (ErrorStream -> Errors -> Errors) -- ^ @setter@
         -> m a
withErr' err errorsVar handle action msg getter setter =
    withErr err errorsVar (handlePath handle) action msg getter setter

-- | Execute the wrapped 'hGetSome', throw an error, or simulate a partial
-- read, depending on the corresponding 'ErrorStreamGetSome' (see
-- 'nextError').
hGetSome'  :: (MonadSTM m, HasCallStack)
           => ErrorHandling FsError m
           -> StrictTVar m Errors
           -> (Handle HandleMock -> Word64 -> m BS.ByteString)  -- ^ Wrapped 'hGetSome'
           -> Handle HandleMock -> Word64 -> m BS.ByteString
hGetSome' ErrorHandling{..} errorsVar hGetSomeWrapped handle n = do
    next errorsVar _hGetSome (\e es -> es { _hGetSome = e }) >>= \case
      Nothing             -> hGetSomeWrapped handle n
      Just (Left errType) -> throwError FsError
        { fsErrorType   = errType
        , fsErrorPath   = handlePath handle
        , fsErrorString = "simulated error: hGetSome"
        , fsErrorNo     = Nothing
        , fsErrorStack  = callStack
        , fsLimitation  = False
        }
      Just (Right partial) ->
        hGetSomeWrapped handle (hGetSomePartial partial n)

-- | Execute the wrapped 'hPutSome', throw an error and apply possible
-- corruption to the blob to write, or simulate a partial write, depending on
-- the corresponding 'ErrorStreamPutSome' (see 'nextError').
hPutSome' :: (MonadSTM m, HasCallStack)
          => ErrorHandling FsError m
          -> StrictTVar m Errors
          -> (Handle HandleMock -> BS.ByteString -> m Word64)  -- ^ Wrapped 'hPutSome'
          -> Handle HandleMock -> BS.ByteString -> m Word64
hPutSome' ErrorHandling{..} errorsVar hPutSomeWrapped handle bs = do
    next errorsVar _hPutSome (\e es -> es { _hPutSome = e }) >>= \case
      Nothing                       -> hPutSomeWrapped handle bs
      Just (Left (errType, mbCorr)) -> do
        whenJust mbCorr $ \corr ->
          void $ hPutSomeWrapped handle (corrupt bs corr)
        throwError FsError
          { fsErrorType   = errType
          , fsErrorPath   = handlePath handle
          , fsErrorString = "simulated error: hPutSome" <> case mbCorr of
              Nothing   -> ""
              Just corr -> " with corruption: " <> show corr
          , fsErrorNo     = Nothing
          , fsErrorStack  = callStack
          , fsLimitation  = False
          }
      Just (Right partial)          ->
        hPutSomeWrapped handle (hPutSomePartial partial bs)


{-------------------------------------------------------------------------------
  Demo
-------------------------------------------------------------------------------}

mockErrorDemo :: IO ()
mockErrorDemo = do
    -- let errors = mempty
    -- let errors = simpleErrors $ alwaysError FsDeviceFull
    errors <- QC.generate arbitrary
    res    <- runExceptT $ runSimErrorFS EH.exceptT Mock.example errors $ \_ ->
                             example
    case res of
      Left  err      -> putStrLn (prettyFsError err)
      Right (bs, fs) -> putStrLn (Mock.pretty fs) >> print bs
