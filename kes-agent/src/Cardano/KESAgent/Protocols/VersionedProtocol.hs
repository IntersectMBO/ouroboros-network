{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- | Versioned protocols have version identifiers associated with them, used
-- for protocol negotiation and verification upon connecting to a KES agent.
module Cardano.KESAgent.Protocols.VersionedProtocol
where

import Cardano.KESAgent.Util.Pretty
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Kind
import Data.Proxy
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import GHC.TypeLits

-- | Protocol types that have version identifiers associated with them.
class VersionedProtocol (p :: Type) where
  -- | Get the version identifier associated with protocol @p@
  versionIdentifier :: Proxy p -> VersionIdentifier

-- | Version identifiers are 'ByteString's 32 bytes long (as per
-- 'VersionIdentifierLength'). They are used to uniquely identify a protocol
-- version for the purposes of protocol negotiation (see
-- 'Cardano.KESAgent.Protocols.VersionHandshake.Protocol').
newtype VersionIdentifier
  = VersionIdentifier {unVersionIdentifier :: ByteString}
  deriving newtype (Show, Eq)

-- | Type-level version identifier length.
type VersionIdentifierLength = 32

-- | Term-level version identifier length.
versionIdentifierLength :: Num a => a
versionIdentifierLength = fromIntegral $ natVal (Proxy @VersionIdentifierLength)

-- | Create a version identifier from a raw 'ByteString'. The input will be
-- truncated or zero-padded to 'VersionIdentifierLength'.
mkVersionIdentifier :: ByteString -> VersionIdentifier
mkVersionIdentifier raw =
  VersionIdentifier $ BS.take versionIdentifierLength $ raw <> BS.replicate versionIdentifierLength 0

instance Pretty VersionIdentifier where
  pretty (VersionIdentifier bs) =
    Text.unpack . decodeUtf8 $ BS.dropWhileEnd (== 0) bs

-- | Uniquely identifies a 'Cardano.KESAgent.KES.Crypto.Crypto' instance (i.e.,
-- a set of cryptographic algorithms to use for KES and OpCert signing). We use
-- these to identify the selected crypto in 'VersionIdentifier's for protocols
-- that are parametrized over the 'Crypto' instance.
newtype CryptoName
  = CryptoName {unCryptoName :: ByteString}
  deriving newtype (Show, Eq)

-- | Create a crypto name from a raw 'ByteString'. It will be zero-padded or
-- truncated to 'VersionIdentifierLength'; note however that further truncation
-- may happen when appending a 'CryptoName' to a 'VersionIdentifier'.
mkCryptoName :: ByteString -> CryptoName
mkCryptoName raw =
  CryptoName $ BS.take versionIdentifierLength $ raw <> BS.replicate versionIdentifierLength 0

-- | 'Crypto' types that have a unique 'CryptoName' associated with them.
class NamedCrypto (p :: Type) where
  cryptoName :: Proxy p -> CryptoName
