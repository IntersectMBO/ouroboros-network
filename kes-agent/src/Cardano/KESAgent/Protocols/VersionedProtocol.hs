{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Cardano.KESAgent.Protocols.VersionedProtocol
where

import Data.Proxy
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Kind

class VersionedProtocol (p :: Type) where
  versionIdentifier :: Proxy p -> VersionIdentifier

newtype VersionIdentifier =
  VersionIdentifier { unVersionIdentifier :: ByteString }
  deriving newtype (Show, Eq)

versionIdentifierLength :: Num a => a
versionIdentifierLength = 32

mkVersionIdentifier :: ByteString -> VersionIdentifier
mkVersionIdentifier raw =
  VersionIdentifier $ BS.take versionIdentifierLength $ raw <> BS.replicate versionIdentifierLength 0

newtype CryptoName =
  CryptoName { unCryptoName :: ByteString }
  deriving newtype (Show, Eq)

mkCryptoName :: ByteString -> CryptoName
mkCryptoName raw =
  CryptoName $ BS.take versionIdentifierLength $ raw <> BS.replicate versionIdentifierLength 0

class NamedCrypto (p :: Type) where
  cryptoName :: Proxy p -> CryptoName
