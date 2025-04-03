{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Protocols.VersionedProtocol
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Cardano.KESAgent.Util.Pretty
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as Text

class VersionedProtocol (p :: Type) where
  versionIdentifier :: Proxy p -> VersionIdentifier

newtype VersionIdentifier
  = VersionIdentifier {unVersionIdentifier :: ByteString}
  deriving newtype (Show, Eq)

type VersionIdentifierLength = 32

versionIdentifierLength :: Num a => a
versionIdentifierLength = fromIntegral $ natVal (Proxy @VersionIdentifierLength)

mkVersionIdentifier :: ByteString -> VersionIdentifier
mkVersionIdentifier raw =
  VersionIdentifier $ BS.take versionIdentifierLength $ raw <> BS.replicate versionIdentifierLength 0

instance Pretty VersionIdentifier where
  pretty (VersionIdentifier bs) =
    Text.unpack . decodeUtf8 $ BS.dropWhileEnd (== 0) bs

newtype CryptoName
  = CryptoName {unCryptoName :: ByteString}
  deriving newtype (Show, Eq)

mkCryptoName :: ByteString -> CryptoName
mkCryptoName raw =
  CryptoName $ BS.take versionIdentifierLength $ raw <> BS.replicate versionIdentifierLength 0

class NamedCrypto (p :: Type) where
  cryptoName :: Proxy p -> CryptoName
