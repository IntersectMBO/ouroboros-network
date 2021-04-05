{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Ouroboros.Consensus.Util.Versioned (
    VersionDecoder (..)
  , VersionError (..)
  , Versioned (..)
  , decodeVersion
  , decodeVersionWithHook
  , decodeVersioned
  , encodeVersion
  , encodeVersioned
    -- * opaque
  , VersionNumber
  ) where

import qualified Codec.CBOR.Decoding as Dec
import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Decoding (Decoder, decodeWord8)
import           Codec.Serialise.Encoding (Encoding, encodeListLen, encodeWord8)
import           Control.Exception (Exception)
import           Data.Word (Word8)

import           Cardano.Binary (enforceSize)


newtype VersionNumber = VersionNumber Word8
  deriving newtype (Eq, Ord, Num, Show)

instance Serialise VersionNumber where
  encode (VersionNumber w) = encodeWord8 w
  decode = VersionNumber <$> decodeWord8

data Versioned a = Versioned
  { versionNumber :: !VersionNumber
  , versioned     :: !a
  } deriving (Eq, Show)

data VersionError
  = IncompatibleVersion VersionNumber String
    -- ^ We cannot deserialise the version of the data with the given
    -- 'VersionNumber' because its data format is incompatible.
    --
    -- For example, the given format lacks data that was added in later
    -- version that cannot be reconstructed from scratch.
  | UnknownVersion VersionNumber
    -- ^ The given 'VersionNumber' is unknown and thus not supported.
  | MigrationFailed VersionNumber String
    -- ^ A migration from the given 'VersionNumber' failed. See 'Migrate'.
  deriving stock    (Show)
  deriving anyclass (Exception)

-- | How to decode a version of a format.
data VersionDecoder a where
  -- | This version is incompatible, fail with 'IncompatibleVersion' and the
  -- given message.
  Incompatible :: String
               -> VersionDecoder a

  -- | Decode the version using the given 'Decoder'.
  Decode       :: (forall s. Decoder s a)
               -> VersionDecoder a

  -- | Decode an other format (@from@) and migrate from that. When migration
  -- fails, the version decoder will fail with @MigrationFailed@.
  Migrate      :: VersionDecoder from
               -> (from -> Either String to)
               -> VersionDecoder to

-- | Return a 'Decoder' for the given 'VersionDecoder'.
getVersionDecoder
  :: VersionNumber
  -> VersionDecoder a
  -> forall s. Decoder s a
getVersionDecoder vn = \case
    Incompatible msg     -> fail $ show $ IncompatibleVersion vn msg
    Decode dec           -> dec
    Migrate vDec migrate -> do
      from <- getVersionDecoder vn vDec
      case migrate from of
        Left msg -> fail $ show $ MigrationFailed vn msg
        Right to -> return to

-- | Given a 'VersionNumber' and the encoding of an @a@, encode the
-- corresponding @'Versioned' a@. Use 'decodeVersion' to decode it.
encodeVersion
  :: VersionNumber
  -> Encoding
  -> Encoding
encodeVersion vn encodedA = mconcat
    [ encodeListLen 2
    , encode vn
    , encodedA
    ]

-- | Decode a /versioned/ @a@ (encoded using 'encodeVersion' or
-- 'encodeVersioned').
--
-- The corresponding 'VersionDecoder' for the deserialised 'VersionNumber' is
-- looked up in the given list. The first match is used (using the semantics
-- of 'lookup'). When no match is found, a decoder that fails with
-- 'UnknownVersion' is returned.
decodeVersion
  :: [(VersionNumber, VersionDecoder a)]
  -> forall s. Decoder s a
decodeVersion versionDecoders =
    versioned <$> decodeVersioned versionDecoders

-- | Same as 'decodeVersion', but with a hook that gets called in case the
-- encoding was not produced by a versioned encoder. This allows a transition
-- from non-versioned to versioned encodings.
--
-- Versioned encodings start with list length 2. Whenever the encoding starts
-- this way, this decoder will use the regular versioned decoder. When the
-- encoding starts differently, either with a different list length ('Just' as
-- argument) or with another token ('Nothing' as argument), the hook is called,
-- allowing the previous non-versioned decoder to try to decode the encoding.
--
-- Note that the hook should /not/ try to decode the list length /again/.
--
-- Note that this will not work if the previous encoding can start with list
-- length 2, as the new versioned decoder will be called in those cases, not the
-- hook.
decodeVersionWithHook
  :: forall a.
     (forall s. Maybe Int -> Decoder s a)
  -> [(VersionNumber, VersionDecoder a)]
  -> forall s. Decoder s a
decodeVersionWithHook hook versionDecoders = do
    tokenType <- Dec.peekTokenType

    if isListLen tokenType then do
      len <- Dec.decodeListLen
      case len of
        2 -> goVersioned
        _ -> hook (Just len)

    else
      hook Nothing

  where
    isListLen :: Dec.TokenType -> Bool
    isListLen = \case
        Dec.TypeListLen   -> True
        Dec.TypeListLen64 -> True
        _                 -> False

    goVersioned :: forall s. Decoder s a
    goVersioned = do
        vn <- decode
        case lookup vn versionDecoders of
          Nothing   -> fail $ show $ UnknownVersion vn
          Just vDec -> getVersionDecoder vn vDec

encodeVersioned
  :: (          a -> Encoding)
  -> (Versioned a -> Encoding)
encodeVersioned enc (Versioned vn a) =
    encodeVersion vn (enc a)

decodeVersioned
  :: [(VersionNumber, VersionDecoder a)]
  -> forall s. Decoder s (Versioned a)
decodeVersioned versionDecoders = do
    enforceSize "Versioned" 2
    vn <- decode
    case lookup vn versionDecoders of
      Nothing   -> fail $ show $ UnknownVersion vn
      Just vDec -> Versioned vn <$> getVersionDecoder vn vDec
