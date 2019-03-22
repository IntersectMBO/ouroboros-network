{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Ouroboros.Network.Mux.Control (
    -- * Mux Versions
      MuxVersion
    , SomeVersion (..)
    , SNat (..)
    , versionNumber

    -- * Mux Version protocol messages
    , ControlMsg (..)
    -- ** CBOR encoding \/ decoding
    , encodeControlMsg
    , decodeControlMsg

    , encodeSomeVersion
    , encodeSomeVersionList
    , decodeSomeVersion
    , decodeSomeVersionList
    ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise.Class
import           Control.Monad
import qualified Control.Monad.Fail as Fail
import           Data.Functor (($>))
import           Data.Maybe (catMaybes)
import           Data.Proxy (Proxy (..))
import           Data.List (find)
import           Data.Type.Equality
import           GHC.TypeNats


-- |
-- Type family which maps version number to version fields.  Initial there are
-- two instance, one in `Ouroboros.Network.NodeToNode` and
-- `Ouroboros.Network.NodeToClient`.  First ten `0` - `9` are reserved for
-- testing instances.
--
type family MuxVersion (n :: Nat)

-- |
-- Singletons used to pattern match version.
--
-- Adding a new version requires adding a constructor here, this will allow to
-- pattern match on @'SomeVersion'@ and get access to it's second field of type
-- @'MuxVersion' n@.
--
data SNat (n :: Nat) where
  SNat0  :: SNat 0  -- ^ test mux version 0
  SNat1  :: SNat 1  -- ^ test mux version 1

  SNat10 :: SNat 10 -- ^ Node2Node mux protocol
  SNat11 :: SNat 11 -- ^ Node2Client mux version

asProxy :: SNat n -> Proxy n
asProxy _ = Proxy

versionNumber :: KnownNat n => SNat (n :: Nat) -> Word
versionNumber = fromIntegral . natVal . asProxy

-- |
-- Existential wrapper for @'VersionField'@; it also carries singleton for @n@
-- which allows to pattern match on constructors of @'MuxVersion' n@.
--
data SomeVersion where
  SomeVersion
    :: forall (n :: Nat).
       ( KnownNat   n
       , Serialise (MuxVersion n)
       , Ord       (MuxVersion n)
       , Eq        (MuxVersion n)
       , Show      (MuxVersion n)
       )
    => SNat n       -- ^ singleton for (n :: Nat)
    -> MuxVersion n -- ^ version fields
    -> SomeVersion

instance Eq SomeVersion where
  (SomeVersion n v) == (SomeVersion n' v') =
    case sameNat (asProxy n) (asProxy n') of
      Just ref -> gcastWith ref (v == v')
      Nothing  -> False

instance Ord SomeVersion where
  compare (SomeVersion n v) (SomeVersion n' v') =
    case sameNat (asProxy n) (asProxy n') of
      Just ref -> gcastWith ref (v `compare` v')
      Nothing  -> natVal n `compare` natVal n'

instance Show SomeVersion where
  show (SomeVersion n v) = "SomeVersion " ++ show (natVal n) ++ " " ++ show v

getDecoder :: SomeVersion -> SomeDecoder s
getDecoder (SomeVersion n _) = SomeDecoder n decode

-- |
-- Existential wrapper for decoder; this is an internal type only used in this
-- module.
--
data SomeDecoder s where
  SomeDecoder
    :: forall (n :: Nat) s.
       ( KnownNat n
       , Serialise (MuxVersion n)
       , Ord (MuxVersion n)
       , Eq (MuxVersion n)
       , Show (MuxVersion n)
       )
    => SNat n
    -> CBOR.Decoder s (MuxVersion n)
    -> SomeDecoder s

lookupDecoder :: Word -> [SomeVersion] -> Maybe (SomeDecoder s)
lookupDecoder w vs = case find (\(SomeVersion n _) -> versionNumber n == w) vs of
    Just sv -> Just (getDecoder sv)
    Nothing -> Nothing

encodeSomeVersion :: SomeVersion -> CBOR.Encoding
encodeSomeVersion (SomeVersion n v) =
       CBOR.encodeListLen 2
    <> CBOR.encodeWord (fromIntegral $ natVal n)
    <> encode v

decodeSomeVersion
  :: [SomeVersion]
  -> CBOR.Decoder s SomeVersion
decodeSomeVersion knownDecoders = do
    _ <- CBOR.decodeListLen
    !vn <- CBOR.decodeWord
    case lookupDecoder vn knownDecoders of
      Nothing                      -> Fail.fail "unknown version"
      Just (SomeDecoder n decoder) -> SomeVersion n <$> decoder

encodeSomeVersionList :: [SomeVersion] -> CBOR.Encoding
encodeSomeVersionList vs =
       CBOR.encodeMapLen (fromIntegral $ length vs)
    <> foldr
        (\(SomeVersion n v) acc ->
          CBOR.encodeWord (fromIntegral $ natVal n)
            <> encode v
            <> acc)
        mempty
        vs

-- |
-- Decode a list of @'SomeVersion'@ if we are given a list of known
-- versions.  From each known version we can get a corresponding decoder.  If
-- we don't know how to decode a term we skip over it rather than fail.  We
-- return a list of all recognised versions.
--
decodeSomeVersionList
  :: forall s. 
     [SomeVersion] -- ^ list of known versions
  -> CBOR.Decoder s [SomeVersion] 
decodeSomeVersionList knownDecoders = do
    n   <- CBOR.decodeMapLen
    catMaybes <$> replicateM n decodeEntry
  where
    decodeEntry :: CBOR.Decoder s (Maybe SomeVersion)
    decodeEntry = do
      !vn <- CBOR.decodeWord
      case lookupDecoder vn knownDecoders of
        Nothing ->
          -- skip over a single cbor term
          CBOR.decodeTerm $> Nothing 
        Just (SomeDecoder n decoder) -> do
          -- decode single cbor term
          Just . SomeVersion n <$> decoder


-- |
-- Messages exchange during initial protocol negotiation.
--
data ControlMsg =

    -- |
    -- Send version which local node understands
      MsgInitReq [SomeVersion]

    -- |
    -- Receive a version which remove node accepted.  It should be one of the
    -- send versions, otherwise it is a protocol violation.
    | MsgInitRsp SomeVersion

    -- |
    -- If a local node received a version that it does not understands it sends
    -- back fail message and the negotiation will terminate.
    --
    | MsgInitFail String
    deriving (Eq, Show)

-- |
-- CBOR Encoder for @'ControlMsg'@.
--
encodeControlMsg :: ControlMsg -> CBOR.Encoding
encodeControlMsg (MsgInitReq versions) = CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> encodeSomeVersionList versions
encodeControlMsg (MsgInitRsp version)  = CBOR.encodeListLen 3 <> CBOR.encodeWord 1 <> encodeSomeVersion version
encodeControlMsg (MsgInitFail msg)     = CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> encode msg

-- |
-- CBOR Decoder for @'ControlMsg'@ requires a list of locally known versions.
--
decodeControlMsg
  :: forall s.
     [SomeVersion] -- ^ known versions
  -> CBOR.Decoder s ControlMsg
decodeControlMsg knownDecoders = do
    _ <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case key of
          0 -> MsgInitReq  <$> decodeSomeVersionList knownDecoders
          1 -> MsgInitRsp  <$> decodeSomeVersion knownDecoders
          2 -> MsgInitFail <$> decode
          a -> Fail.fail ("unknown control message type " ++ show a)
