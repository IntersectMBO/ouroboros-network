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
module Test.Util.FS.Sim.Error (
    -- * Simulate Errors monad
    mkSimErrorHasFS
  , runSimErrorFS
  , withErrors
    -- * Streams
  , ErrorStream
  , ErrorStreamGetSome
  , ErrorStreamPutSome
  , Stream (..)
  , always
  , mkStream
  , mkStreamGen
  , null
  , runStream
    -- * Generating partial reads/writes
  , Partial (..)
  , hGetSomePartial
  , hPutSomePartial
    -- * Generating corruption for 'hPutSome'
  , PutCorruption (..)
  , corrupt
    -- * Error streams for 'HasFS'
  , Errors (..)
  , allNull
  , genErrors
  , simpleErrors
  ) where

import           Prelude hiding (null)

import           Control.Monad (replicateM, void)

import qualified Data.ByteString as BS
import           Data.List (dropWhileEnd, intercalate)
import           Data.Maybe (catMaybes, isNothing)
import           Data.Word (Word64)

import           Test.QuickCheck (Arbitrary (..), Gen)
import qualified Test.QuickCheck as QC

import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike hiding (handle)

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Test.Util.Blob
import           Test.Util.FS.Sim.MockFS (HandleMock, MockFS)
import qualified Test.Util.FS.Sim.STM as Sim

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
  { dumpStateE                :: ErrorStream -- TODO remove

    -- Operations on files
  , hOpenE                    :: ErrorStream
  , hCloseE                   :: ErrorStream
  , hSeekE                    :: ErrorStream
  , hGetSomeE                 :: ErrorStreamGetSome
  , hGetSomeAtE               :: ErrorStreamGetSome
  , hPutSomeE                 :: ErrorStreamPutSome
  , hTruncateE                :: ErrorStream
  , hGetSizeE                 :: ErrorStream

    -- Operations on directories
  , createDirectoryE          :: ErrorStream
  , createDirectoryIfMissingE :: ErrorStream
  , listDirectoryE            :: ErrorStream
  , doesDirectoryExistE       :: ErrorStream
  , doesFileExistE            :: ErrorStream
  , removeFileE               :: ErrorStream
  , renameFileE               :: ErrorStream
  }

-- | Return 'True' if all streams are empty ('null').
allNull :: Errors -> Bool
allNull Errors {..} = null dumpStateE
                   && null hOpenE
                   && null hCloseE
                   && null hSeekE
                   && null hGetSomeE
                   && null hGetSomeAtE
                   && null hPutSomeE
                   && null hTruncateE
                   && null hGetSizeE
                   && null createDirectoryE
                   && null createDirectoryIfMissingE
                   && null listDirectoryE
                   && null doesDirectoryExistE
                   && null doesFileExistE
                   && null removeFileE
                   && null renameFileE


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
        [ s "dumpStateE"                dumpStateE
        , s "hOpenE"                    hOpenE
        , s "hCloseE"                   hCloseE
        , s "hSeekE"                    hSeekE
        , s "hGetSomeE"                 hGetSomeE
        , s "hGetSomeAtE"               hGetSomeAtE
        , s "hPutSomeE"                 hPutSomeE
        , s "hTruncateE"                hTruncateE
        , s "hGetSizeE"                 hGetSizeE
        , s "createDirectoryE"          createDirectoryE
        , s "createDirectoryIfMissingE" createDirectoryIfMissingE
        , s "listDirectoryE"            listDirectoryE
        , s "doesDirectoryExistE"       doesDirectoryExistE
        , s "doesFileExistE"            doesFileExistE
        , s "removeFileE"               removeFileE
        , s "renameFileE"               renameFileE
        ]

instance Semigroup Errors where
  egs1 <> egs2 = Errors
      { dumpStateE                = combine dumpStateE
      , hOpenE                    = combine hOpenE
      , hCloseE                   = combine hCloseE
      , hSeekE                    = combine hSeekE
      , hGetSomeE                 = combine hGetSomeE
      , hGetSomeAtE               = combine hGetSomeAtE
      , hPutSomeE                 = combine hPutSomeE
      , hTruncateE                = combine hTruncateE
      , hGetSizeE                 = combine hGetSizeE
      , createDirectoryE          = combine createDirectoryE
      , createDirectoryIfMissingE = combine createDirectoryIfMissingE
      , listDirectoryE            = combine listDirectoryE
      , doesDirectoryExistE       = combine doesDirectoryExistE
      , doesFileExistE            = combine doesFileExistE
      , removeFileE               = combine removeFileE
      , renameFileE               = combine renameFileE
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
    { dumpStateE                = es
    , hOpenE                    = es
    , hCloseE                   = es
    , hSeekE                    = es
    , hGetSomeE                 =  Left                <$> es
    , hGetSomeAtE               =  Left                <$> es
    , hPutSomeE                 = (Left . (, Nothing)) <$> es
    , hTruncateE                = es
    , hGetSizeE                 = es
    , createDirectoryE          = es
    , createDirectoryIfMissingE = es
    , listDirectoryE            = es
    , doesDirectoryExistE       = es
    , doesFileExistE            = es
    , removeFileE               = es
    , renameFileE               = es
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
    dumpStateE          <- dummy
    -- TODO let this one fail:
    let hCloseE = mkStream []
    hTruncateE          <- dummy
    doesDirectoryExistE <- dummy
    doesFileExistE      <- dummy
    hOpenE <- streamGen 1
      [ FsResourceDoesNotExist, FsResourceInappropriateType
      , FsResourceAlreadyInUse, FsResourceAlreadyExist
      , FsInsufficientPermissions, FsTooManyOpenFiles ]
    hSeekE      <- streamGen 3 [ FsReachedEOF ]
    hGetSomeE   <- mkStreamGen 20 $ QC.frequency
      [ (1, return $ Left FsReachedEOF)
      , (3, Right <$> arbitrary) ]
    hGetSomeAtE <- mkStreamGen 20 $ QC.frequency
      [ (1, return $ Left FsReachedEOF)
      , (3, Right <$> arbitrary) ]
    hPutSomeE   <- mkStreamGen 5 $ QC.frequency
      [ (1, Left . (FsDeviceFull, ) <$> QC.frequency
            [ (2, return Nothing)
            , (1, Just . PartialWrite <$> arbitrary)
            , (if genSubstituteWithJunk then 1 else 0,
               Just . SubstituteWithJunk <$> arbitrary)
            ])
      , (if genPartialWrites then 3 else 0, Right <$> arbitrary) ]
    hGetSizeE   <- streamGen 2 [ FsResourceDoesNotExist ]
    createDirectoryE <- streamGen 3
      [ FsInsufficientPermissions, FsResourceInappropriateType
      , FsResourceAlreadyExist ]
    createDirectoryIfMissingE <- streamGen 3
      [ FsInsufficientPermissions, FsResourceInappropriateType
      , FsResourceAlreadyExist ]
    listDirectoryE <- streamGen 3
      [ FsInsufficientPermissions, FsResourceInappropriateType
      , FsResourceDoesNotExist ]
    removeFileE    <- streamGen 3
      [ FsInsufficientPermissions, FsResourceAlreadyInUse
      , FsResourceDoesNotExist, FsResourceInappropriateType ]
    renameFileE    <- streamGen 3
      [ FsInsufficientPermissions, FsResourceAlreadyInUse
      , FsResourceDoesNotExist, FsResourceInappropriateType ]
    return Errors {..}

instance Arbitrary Errors where
  arbitrary = genErrors True True

  shrink err@Errors {..} = filter (not . allNull) $ catMaybes
      [ (\s' -> err { dumpStateE = s' })                <$> dropLast dumpStateE
      , (\s' -> err { hOpenE = s' })                    <$> dropLast hOpenE
      , (\s' -> err { hCloseE = s' })                   <$> dropLast hCloseE
      , (\s' -> err { hSeekE = s' })                    <$> dropLast hSeekE
      , (\s' -> err { hGetSomeE = s' })                 <$> dropLast hGetSomeE
      , (\s' -> err { hGetSomeAtE = s' })               <$> dropLast hGetSomeAtE
      , (\s' -> err { hPutSomeE = s' })                 <$> dropLast hPutSomeE
      , (\s' -> err { hTruncateE = s' })                <$> dropLast hTruncateE
      , (\s' -> err { hGetSizeE = s' })                 <$> dropLast hGetSizeE
      , (\s' -> err { createDirectoryE = s' })          <$> dropLast createDirectoryE
      , (\s' -> err { createDirectoryIfMissingE = s' }) <$> dropLast createDirectoryIfMissingE
      , (\s' -> err { listDirectoryE = s' })            <$> dropLast listDirectoryE
      , (\s' -> err { doesDirectoryExistE = s' })       <$> dropLast doesDirectoryExistE
      , (\s' -> err { doesFileExistE = s' })            <$> dropLast doesFileExistE
      , (\s' -> err { removeFileE = s' })               <$> dropLast removeFileE
      , (\s' -> err { renameFileE = s' })               <$> dropLast renameFileE
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
mkSimErrorHasFS :: forall m. (MonadSTM m, MonadThrow m)
                => StrictTVar m MockFS
                -> StrictTVar m Errors
                -> HasFS m HandleMock
mkSimErrorHasFS fsVar errorsVar =
    case Sim.simHasFS fsVar of
      HasFS{..} -> HasFS{
          dumpState =
            withErr errorsVar (mkFsPath ["<dumpState>"]) dumpState "dumpState"
              dumpStateE (\e es -> es { dumpStateE = e })
        , hOpen      = \p m ->
            withErr errorsVar p (hOpen p m) "hOpen"
            hOpenE (\e es -> es { hOpenE = e })
        , hClose     = \h ->
            withErr' errorsVar h (hClose h) "hClose"
            hCloseE (\e es -> es { hCloseE = e })
        , hIsOpen    = hIsOpen
        , hSeek      = \h m n ->
            withErr' errorsVar h (hSeek h m n) "hSeek"
            hSeekE (\e es -> es { hSeekE = e })
        , hGetSome   = hGetSome' errorsVar hGetSome
        , hGetSomeAt = hGetSomeAt' errorsVar hGetSomeAt
        , hPutSome   = hPutSome' errorsVar hPutSome
        , hTruncate  = \h w ->
            withErr' errorsVar h (hTruncate h w) "hTruncate"
            hTruncateE (\e es -> es { hTruncateE = e })
        , hGetSize   =  \h ->
            withErr' errorsVar h (hGetSize h) "hGetSize"
            hGetSizeE (\e es -> es { hGetSizeE = e })

        , createDirectory          = \p ->
            withErr errorsVar p (createDirectory p) "createDirectory"
            createDirectoryE (\e es -> es { createDirectoryE = e })
        , createDirectoryIfMissing = \b p ->
            withErr errorsVar p (createDirectoryIfMissing b p) "createDirectoryIfMissing"
            createDirectoryIfMissingE (\e es -> es { createDirectoryIfMissingE = e })
        , listDirectory            = \p ->
            withErr errorsVar p (listDirectory p) "listDirectory"
            listDirectoryE (\e es -> es { listDirectoryE = e })
        , doesDirectoryExist       = \p ->
            withErr errorsVar p (doesDirectoryExist p) "doesDirectoryExist"
            doesDirectoryExistE (\e es -> es { doesDirectoryExistE = e })
        , doesFileExist            = \p ->
            withErr errorsVar p (doesFileExist p) "doesFileExist"
            doesFileExistE (\e es -> es { doesFileExistE = e })
        , removeFile               = \p ->
            withErr errorsVar p (removeFile p) "removeFile"
            removeFileE (\e es -> es { removeFileE = e })
        , renameFile               = \p1 p2 ->
            withErr errorsVar p1 (renameFile p1 p2) "renameFile"
            renameFileE (\e es -> es { renameFileE = e })
        , mkFsErrorPath = fsToFsErrorPathUnmounted
        }

-- | Runs a computation provided an 'Errors' and an initial
-- 'MockFS', producing a result and the final state of the filesystem.
runSimErrorFS :: (MonadSTM m, MonadThrow m)
              => MockFS
              -> Errors
              -> (StrictTVar m Errors -> HasFS m HandleMock -> m a)
              -> m (a, MockFS)
runSimErrorFS mockFS errors action = do
    fsVar     <- uncheckedNewTVarM mockFS
    errorsVar <- uncheckedNewTVarM errors
    a         <- action errorsVar $ mkSimErrorHasFS fsVar errorsVar
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
withErr :: (MonadSTM m, MonadThrow m, HasCallStack)
        => StrictTVar m Errors
        -> FsPath     -- ^ The path for the error, if thrown
        -> m a        -- ^ Action in case no error is thrown
        -> String     -- ^ Extra message for in the 'fsErrorString'
        -> (Errors -> ErrorStream)           -- ^ @getter@
        -> (ErrorStream -> Errors -> Errors) -- ^ @setter@
        -> m a
withErr errorsVar path action msg getter setter = do
    mbErr <- next errorsVar getter setter
    case mbErr of
      Nothing      -> action
      Just errType -> throwIO FsError
        { fsErrorType   = errType
        , fsErrorPath   = fsToFsErrorPathUnmounted path
        , fsErrorString = "simulated error: " <> msg
        , fsErrorNo     = Nothing
        , fsErrorStack  = prettyCallStack
        , fsLimitation  = False
        }

-- | Variant of 'withErr' that works with 'Handle's.
--
-- The path of the handle is retrieved from the 'MockFS' using 'handleFsPath'.
withErr' :: (MonadSTM m, MonadThrow m, HasCallStack)
         => StrictTVar m Errors
         -> Handle HandleMock   -- ^ The path for the error, if thrown
         -> m a        -- ^ Action in case no error is thrown
         -> String     -- ^ Extra message for in the 'fsErrorString'
         -> (Errors -> ErrorStream)           -- ^ @getter@
         -> (ErrorStream -> Errors -> Errors) -- ^ @setter@
         -> m a
withErr' errorsVar handle action msg getter setter =
    withErr errorsVar (handlePath handle) action msg getter setter

-- | Execute the wrapped 'hGetSome', throw an error, or simulate a partial
-- read, depending on the corresponding 'ErrorStreamGetSome' (see
-- 'nextError').
hGetSome'  :: (MonadSTM m, MonadThrow m, HasCallStack)
           => StrictTVar m Errors
           -> (Handle HandleMock -> Word64 -> m BS.ByteString)  -- ^ Wrapped 'hGetSome'
           -> Handle HandleMock -> Word64 -> m BS.ByteString
hGetSome' errorsVar hGetSomeWrapped handle n =
    next errorsVar hGetSomeE (\e es -> es { hGetSomeE = e }) >>= \case
      Nothing             -> hGetSomeWrapped handle n
      Just (Left errType) -> throwIO FsError
        { fsErrorType   = errType
        , fsErrorPath   = fsToFsErrorPathUnmounted $ handlePath handle
        , fsErrorString = "simulated error: hGetSome"
        , fsErrorNo     = Nothing
        , fsErrorStack  = prettyCallStack
        , fsLimitation  = False
        }
      Just (Right partial) ->
        hGetSomeWrapped handle (hGetSomePartial partial n)

-- | In the thread safe version of 'hGetSome', we simulate exactly the same errors.
hGetSomeAt' :: (MonadSTM m, MonadThrow m, HasCallStack)
            => StrictTVar m Errors
            -> (Handle HandleMock -> Word64 -> AbsOffset -> m BS.ByteString)  -- ^ Wrapped 'hGetSomeAt'
            -> Handle HandleMock -> Word64 -> AbsOffset -> m BS.ByteString
hGetSomeAt' errorsVar hGetSomeAtWrapped handle n offset =
    next errorsVar hGetSomeAtE (\e es -> es { hGetSomeAtE = e }) >>= \case
      Nothing             -> hGetSomeAtWrapped handle n offset
      Just (Left errType) -> throwIO FsError
        { fsErrorType   = errType
        , fsErrorPath   = fsToFsErrorPathUnmounted $ handlePath handle
        , fsErrorString = "simulated error: hGetSomeAt"
        , fsErrorNo     = Nothing
        , fsErrorStack  = prettyCallStack
        , fsLimitation  = False
        }
      Just (Right partial) ->
        hGetSomeAtWrapped handle (hGetSomePartial partial n) offset

-- | Execute the wrapped 'hPutSome', throw an error and apply possible
-- corruption to the blob to write, or simulate a partial write, depending on
-- the corresponding 'ErrorStreamPutSome' (see 'nextError').
hPutSome' :: (MonadSTM m, MonadThrow m, HasCallStack)
          => StrictTVar m Errors
          -> (Handle HandleMock -> BS.ByteString -> m Word64)  -- ^ Wrapped 'hPutSome'
          -> Handle HandleMock -> BS.ByteString -> m Word64
hPutSome' errorsVar hPutSomeWrapped handle bs =
    next errorsVar hPutSomeE (\e es -> es { hPutSomeE = e }) >>= \case
      Nothing                       -> hPutSomeWrapped handle bs
      Just (Left (errType, mbCorr)) -> do
        whenJust mbCorr $ \corr ->
          void $ hPutSomeWrapped handle (corrupt bs corr)
        throwIO FsError
          { fsErrorType   = errType
          , fsErrorPath   = fsToFsErrorPathUnmounted $ handlePath handle
          , fsErrorString = "simulated error: hPutSome" <> case mbCorr of
              Nothing   -> ""
              Just corr -> " with corruption: " <> show corr
          , fsErrorNo     = Nothing
          , fsErrorStack  = prettyCallStack
          , fsLimitation  = False
          }
      Just (Right partial)          ->
        hPutSomeWrapped handle (hPutSomePartial partial bs)
