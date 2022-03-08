{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Util.CBOR (
    -- * Incremental parsing in I/O
    IDecodeIO (..)
  , deserialiseIncrementalIO
  , fromIDecode
    -- * Higher-level incremental interface
  , Decoder (..)
  , initDecoderIO
    -- * Decode as FlatTerm
  , decodeAsFlatTerm
    -- * HasFS interaction
  , ReadIncrementalErr (..)
  , readIncremental
  , withStreamIncrementalOffsets
    -- * Encoding/decoding containers
  , decodeList
  , decodeMaybe
  , decodeSeq
  , decodeWithOrigin
  , encodeList
  , encodeMaybe
  , encodeSeq
  , encodeWithOrigin
  ) where

import qualified Codec.CBOR.Decoding as CBOR.D
import qualified Codec.CBOR.Encoding as CBOR.E
import qualified Codec.CBOR.FlatTerm as CBOR.F
import qualified Codec.CBOR.Read as CBOR.R
import           Control.Exception (assert)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.ST
import qualified Control.Monad.ST.Lazy as ST.Lazy
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (toList)
import           Data.IORef
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import qualified Streaming as S
import           Streaming.Prelude (Of (..), Stream)
import qualified Streaming.Prelude as S

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

{-------------------------------------------------------------------------------
  Incremental parsing in I/O
-------------------------------------------------------------------------------}

data IDecodeIO a =
    Partial (Maybe ByteString -> IO (IDecodeIO a))
  | Done !ByteString !CBOR.R.ByteOffset a
  | Fail !ByteString !CBOR.R.ByteOffset CBOR.R.DeserialiseFailure

fromIDecode :: CBOR.R.IDecode RealWorld a -> IDecodeIO a
fromIDecode (CBOR.R.Partial k)     = Partial $ fmap fromIDecode . stToIO . k
fromIDecode (CBOR.R.Done bs off x) = Done bs off x
fromIDecode (CBOR.R.Fail bs off e) = Fail bs off e

deserialiseIncrementalIO :: (forall s. CBOR.D.Decoder s a) -> IO (IDecodeIO a)
deserialiseIncrementalIO = fmap fromIDecode
                         . stToIO
                         . CBOR.R.deserialiseIncremental

{-------------------------------------------------------------------------------
  Higher-level incremental interface
-------------------------------------------------------------------------------}

data Decoder m = Decoder {
      -- | Decode next failure
      --
      -- May throw 'CBOR.DeserialiseFailure'
      decodeNext :: forall a. (forall s. CBOR.D.Decoder s a) -> m a
    }

-- | Construct incremental decoder given a way to get chunks
--
-- Resulting decoder is not thread safe.
initDecoderIO :: IO ByteString -> IO (Decoder IO)
initDecoderIO getChunk = do
    leftover <- newIORef BS.empty
    let go :: forall a. (forall s. CBOR.D.Decoder s a) -> IO a
        go decoder = do
           i <- deserialiseIncrementalIO decoder
           case i of
             Done bs _ a -> assert (BS.null bs) $ return a
             Fail _  _ e -> throwIO e
             Partial k   -> readIORef leftover >>= (k . Just >=> goWith)

        goWith :: forall a. IDecodeIO a -> IO a
        goWith (Partial k)   = getChunk' >>= (k >=> goWith)
        goWith (Done bs _ a) = writeIORef leftover bs >> return a
        goWith (Fail _  _ e) = throwIO e

    return $ Decoder go

  where
    getChunk' :: IO (Maybe ByteString)
    getChunk' = checkEmpty <$> getChunk

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs

{-------------------------------------------------------------------------------
  Decode as FlatTerm
-------------------------------------------------------------------------------}

decodeAsFlatTerm ::
     ByteString
  -> Either CBOR.R.DeserialiseFailure CBOR.F.FlatTerm
decodeAsFlatTerm bs0 =
    ST.Lazy.runST (runExceptT (provideInput bs0))
  where
    provideInput ::
         ByteString
      -> ExceptT CBOR.R.DeserialiseFailure (ST.Lazy.ST s) CBOR.F.FlatTerm
    provideInput bs
      | BS.null bs = return []
      | otherwise      = do
          next <- lift $ ST.Lazy.strictToLazyST $ do
              -- This will always be a 'Partial' here because decodeTermToken
              -- always starts by requesting initial input. Only decoders that
              -- fail or return a value without looking at their input can give
              -- a different initial result.
              CBOR.R.Partial k <- CBOR.R.deserialiseIncremental CBOR.F.decodeTermToken
              k (Just bs)
          collectOutput next

    collectOutput ::
         CBOR.R.IDecode s CBOR.F.TermToken
      -> ExceptT CBOR.R.DeserialiseFailure (ST.Lazy.ST s) CBOR.F.FlatTerm
    collectOutput (CBOR.R.Fail _ _ err) = throwError err
    collectOutput (CBOR.R.Partial    k) = lift (ST.Lazy.strictToLazyST (k Nothing)) >>=
                                          collectOutput
    collectOutput (CBOR.R.Done bs' _ x) = do xs <- provideInput bs'
                                             return (x : xs)

{-------------------------------------------------------------------------------
  HasFS interaction
-------------------------------------------------------------------------------}

data ReadIncrementalErr =
    -- | Could not deserialise the data
    ReadFailed CBOR.R.DeserialiseFailure

    -- | Deserialisation was successful, but there was additional data
  | TrailingBytes ByteString
  deriving (Eq, Show)

-- | Read a file incrementally
--
-- NOTE: The 'MonadThrow' constraint is only needed for 'bracket'. This
-- function does not actually throw anything.
--
-- NOTE: This uses a chunk size of roughly 32k. If we use this function to read
-- small things this might not be ideal.
--
-- NOTE: This currently expects the file to contain precisely one value; see also
-- 'withStreamIncrementalOffsets'.
readIncremental :: forall m a. IOLike m
                => SomeHasFS m
                -> (forall s . CBOR.D.Decoder s a)
                -> FsPath
                -> m (Either ReadIncrementalErr a)
readIncremental = \(SomeHasFS hasFS) decoder fp -> withLiftST $ \liftST -> do
    withFile hasFS fp ReadMode $ \h ->
      go hasFS liftST h =<< liftST (CBOR.R.deserialiseIncremental decoder)
  where
    go :: HasFS m h
       -> (forall x. ST s x -> m x)
       -> Handle h
       -> CBOR.R.IDecode s a
       -> m (Either ReadIncrementalErr a)
    go hasFS@HasFS{..} liftST h (CBOR.R.Partial k) = do
        bs   <- hGetSome h (fromIntegral defaultChunkSize)
        dec' <- liftST $ k (checkEmpty bs)
        go hasFS liftST h dec'
    go _ _ _ (CBOR.R.Done leftover _ a) =
        return $ if BS.null leftover
                   then Right a
                   else Left $ TrailingBytes leftover
    go _ _ _ (CBOR.R.Fail _ _ err) =
        return $ Left $ ReadFailed err

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs

-- | Read multiple @a@s incrementally from a file in a streaming way.
--
-- Continuation-passing style to ensure proper closure of the file.
--
-- Reads the offset ('Word64') of the start of each @a@, the size ('Word64')
-- of each @a@, and each @a@ itself. When deserialising fails, it passes all
-- already deserialised @a@s, the error, and the offset after which the
-- failure occurred.
--
-- NOTE: f we introduce user-facing streaming API also, the fact that we are
-- using @streaming@ here should not dictate that we should stick with it
-- later; rather, we should revisit this code at that point.
withStreamIncrementalOffsets
  :: forall m h a r. (IOLike m, HasCallStack)
  => HasFS m h
  -> (forall s . CBOR.D.Decoder s (LBS.ByteString -> a))
  -> FsPath
  -> (Stream (Of (Word64, (Word64, a))) m (Maybe (ReadIncrementalErr, Word64)) -> m r)
  -> m r
withStreamIncrementalOffsets hasFS@HasFS{..} decoder fp = \k ->
    withLiftST $ \liftST ->
      withFile hasFS fp ReadMode $ \h -> k $ do
        fileSize <- S.lift $ hGetSize h
        if fileSize == 0 then
          -- If the file is empty, we will immediately get "end of input"
          return Nothing
        else
          S.lift (liftST (CBOR.R.deserialiseIncremental decoder)) >>=
            go liftST h 0 Nothing [] fileSize
  where
    -- TODO stream from HasFS?
    go :: (forall x. ST s x -> m x)
       -> Handle h
       -> Word64                   -- ^ Offset
       -> Maybe ByteString         -- ^ Unconsumed bytes from last time
       -> [ByteString]             -- ^ Chunks pushed for this item (rev order)
       -> Word64                   -- ^ Total file size
       -> CBOR.R.IDecode s (LBS.ByteString -> a)
       -> Stream (Of (Word64, (Word64, a))) m (Maybe (ReadIncrementalErr, Word64))
    go liftST h offset mbUnconsumed bss fileSize dec = case dec of
      CBOR.R.Partial k -> do
        -- First use the unconsumed bytes from a previous read before read
        -- some more bytes from the file.
        bs   <- case mbUnconsumed of
          Just unconsumed -> return unconsumed
          Nothing         -> S.lift $ hGetSome h (fromIntegral defaultChunkSize)
        dec' <- S.lift $ liftST $ k (checkEmpty bs)
        go liftST h offset Nothing (bs:bss) fileSize dec'

      CBOR.R.Done leftover size mkA -> do
        let nextOffset = offset + fromIntegral size
            -- We've been keeping track of the bytes pushed into the decoder
            -- for this item so far in bss. Now there's some trailing data to
            -- remove and we can get the whole bytes used for this item. We
            -- supply the bytes to the final decoded value. This is to support
            -- annotating values with their original input bytes.
            aBytes     = case bss of
                []      -> LBS.empty
                bs:bss' -> LBS.fromChunks (reverse (bs' : bss'))
                  where
                    bs' = BS.take (BS.length bs - BS.length leftover) bs
            -- The bang on the @a'@ here allows the used 'Decoder' to force
            -- its computation. For example, the decoder might decode a whole
            -- block and then (maybe through a use of 'fmap') just return its
            -- hash. If we don't force the value it returned here, we're just
            -- putting a thunk that references the whole block in the list
            -- instead of merely the hash.
            !a         = mkA aBytes
        S.yield (offset, (fromIntegral size, a))
        case checkEmpty leftover of
          Nothing
            | nextOffset == fileSize
              -- We're at the end of the file, so stop
            -> return Nothing
          -- Some more bytes, so try to read the next @a@.
          mbLeftover ->
            S.lift (liftST (CBOR.R.deserialiseIncremental decoder)) >>=
            go liftST h nextOffset mbLeftover [] fileSize

      CBOR.R.Fail _ _ err -> return $ Just (ReadFailed err, offset)

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs

{-------------------------------------------------------------------------------
  Encoding/decoding lists
-------------------------------------------------------------------------------}

encodeList :: (a -> CBOR.E.Encoding) -> [a] -> CBOR.E.Encoding
encodeList _   [] = CBOR.E.encodeListLen 0
encodeList enc xs = mconcat [
      CBOR.E.encodeListLenIndef
    , foldr (\x r -> enc x <> r) CBOR.E.encodeBreak xs
    ]

decodeList :: CBOR.D.Decoder s a -> CBOR.D.Decoder s [a]
decodeList dec = do
    mn <- CBOR.D.decodeListLenOrIndef
    case mn of
      Nothing -> CBOR.D.decodeSequenceLenIndef (flip (:)) [] reverse   dec
      Just n  -> CBOR.D.decodeSequenceLenN     (flip (:)) [] reverse n dec

encodeSeq :: (a -> CBOR.E.Encoding) -> StrictSeq a -> CBOR.E.Encoding
encodeSeq f = encodeList f . toList

decodeSeq :: CBOR.D.Decoder s a -> CBOR.D.Decoder s (StrictSeq a)
decodeSeq f = Seq.fromList <$> decodeList f

encodeMaybe :: (a -> CBOR.E.Encoding) -> Maybe a -> CBOR.E.Encoding
encodeMaybe enc = \case
    Nothing -> CBOR.E.encodeListLen 0
    Just x  -> CBOR.E.encodeListLen 1 <> enc x

decodeMaybe :: CBOR.D.Decoder s a -> CBOR.D.Decoder s (Maybe a)
decodeMaybe dec = do
    n <- CBOR.D.decodeListLen
    case n of
      0 -> return Nothing
      1 -> do !x <- dec
              return (Just x)
      _ -> fail "unknown tag"

encodeWithOrigin :: (a -> CBOR.E.Encoding) -> WithOrigin a -> CBOR.E.Encoding
encodeWithOrigin f = encodeMaybe f . withOriginToMaybe

decodeWithOrigin :: CBOR.D.Decoder s a -> CBOR.D.Decoder s (WithOrigin a)
decodeWithOrigin f = withOriginFromMaybe <$> decodeMaybe f
