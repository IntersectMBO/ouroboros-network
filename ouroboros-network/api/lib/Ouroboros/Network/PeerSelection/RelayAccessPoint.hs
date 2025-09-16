{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Ouroboros.Network.PeerSelection.RelayAccessPoint
  ( RelayAccessPoint (..)
  , LedgerRelayAccessPoint (..)
  , LedgerRelayAccessPointV1 (..)
  , SRVPrefix
  , prefixLedgerRelayAccessPoint
  , IP.IP (..)
    -- * Socket type re-exports
  , Socket.PortNumber
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (unless)

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 qualified as BSC
import Data.IP qualified as IP
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Read (readMaybe)

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Binary qualified as Codec
import Network.DNS qualified as DNS
import Network.Socket qualified as Socket

-- | A relay can have either an IP address and a port number or
-- a domain with a port number
--
data RelayAccessPoint = RelayAccessDomain    !DNS.Domain !Socket.PortNumber
                      | RelayAccessSRVDomain !DNS.Domain
                        -- ^ SRV domain, prefixed (as defined in CIP#0155)
                      | RelayAccessAddress   !IP.IP      !Socket.PortNumber
  deriving (Eq, Ord)

instance Show RelayAccessPoint where
    show (RelayAccessDomain domain port) =
      "RelayAccessDomain " ++ show domain ++ " " ++ show port
    show (RelayAccessSRVDomain domain) =
      "RelayAccessSRVDomain " ++ show domain
    show (RelayAccessAddress ip port) =
      "RelayAccessAddress \"" ++ show ip ++ "\" " ++ show port

-- 'IP' nor 'IPv6' is strict, 'IPv4' is strict only because it's a newtype for
-- a primitive type ('Word32').
--
instance NFData RelayAccessPoint where
  rnf (RelayAccessDomain !_domain !_port) = ()
  rnf (RelayAccessSRVDomain !_domain) = ()
  rnf (RelayAccessAddress ip !_port) =
    case ip of
      IP.IPv4 ipv4 -> rnf (IP.fromIPv4w ipv4)
      IP.IPv6 ipv6 -> rnf (IP.fromIPv6w ipv6)

instance FromJSON RelayAccessPoint where
  parseJSON = withObject "RelayAccessPoint" $ \o -> do
    addr <- encodeUtf8 <$> o .: "address"
    let res = flip parseMaybe o $ const do
          port <- o .: "port"
          return (toRelayAccessPoint addr port)
    case res of
      Nothing  -> return $ RelayAccessSRVDomain (fullyQualified addr)
      Just rap -> return rap

    where
      toRelayAccessPoint :: DNS.Domain -> Int -> RelayAccessPoint
      toRelayAccessPoint address port =
          case readMaybe (BSC.unpack address) of
            Nothing   -> RelayAccessDomain (fullyQualified address) (fromIntegral port)
            Just addr -> RelayAccessAddress addr (fromIntegral port)
      fullyQualified = \case
        domain | Just (_, '.') <- BSC.unsnoc domain -> domain
               | otherwise -> domain `BSC.snoc` '.'

instance ToJSON RelayAccessPoint where
  toJSON (RelayAccessDomain addr port) =
    object
      [ "address" .= decodeUtf8 addr
      , "port" .= (fromIntegral port :: Int)
      ]
  toJSON (RelayAccessSRVDomain domain) =
    object
      [ "address" .= decodeUtf8 domain
      ]
  toJSON (RelayAccessAddress ip port) =
    object
      [ "address" .= Text.pack (show ip)
      , "port" .= (fromIntegral port :: Int)
      ]

instance ToCBOR RelayAccessPoint where
  toCBOR rap = case rap of
    RelayAccessDomain domain port ->
         Codec.encodeListLen 3
      <> Codec.encodeWord8 0
      <> serialise' port
      <> toCBOR domain
    RelayAccessAddress (IP.IPv4 ipv4) port ->
         Codec.encodeListLen 3
      <> Codec.encodeWord8 1
      <> serialise' port
      <> toCBOR (IP.fromIPv4 ipv4)
    RelayAccessAddress (IP.IPv6 ip6) port ->
         Codec.encodeListLen 3
      <> Codec.encodeWord8 2
      <> serialise' port
      <> toCBOR (IP.fromIPv6 ip6)
    RelayAccessSRVDomain domain ->
         Codec.encodeListLen 2
      <> Codec.encodeWord8 3
      <> toCBOR domain
    where
      serialise' = toCBOR . toInteger

instance FromCBOR RelayAccessPoint where
  fromCBOR = do
    listLen <- Codec.decodeListLen
    constructorTag <- Codec.decodeWord8
    unless (   listLen == 3
            || (listLen == 2 && constructorTag == 3))
      $ fail $ "Unrecognized RelayAccessPoint list length "
               <> show listLen <> "for constructor tag "
               <> show constructorTag
    case constructorTag of
      0 -> do
        port <- decodePort
        domain <- fromCBOR
        return $ RelayAccessDomain domain port
      1 -> do
        port <- decodePort
        ip <- IP.IPv4 . IP.toIPv4 <$> fromCBOR
        return $ RelayAccessAddress ip port
      2 -> do
        port <- decodePort
        ip <- IP.IPv6 . IP.toIPv6 <$> fromCBOR
        return $ RelayAccessAddress ip port
      3 -> do
        RelayAccessSRVDomain <$> fromCBOR
      _ -> fail $ "Unrecognized RelayAccessPoint tag: " <> show constructorTag
    where
      decodePort = fromIntegral @Int <$> fromCBOR


-- | A Relay as registered on the ledger.
--
-- The only difference with  `RelayAccessPoint` is that
-- `LedgerRelayAccessSRVDomain` is not prefixed, as required by CIP#0155.
--
data LedgerRelayAccessPoint =
    LedgerRelayAccessDomain    !DNS.Domain !Socket.PortNumber
  | LedgerRelayAccessSRVDomain !DNS.Domain
    -- ^ SRV domain as registered on the ledger
  | LedgerRelayAccessAddress   !IP.IP      !Socket.PortNumber
  deriving (Eq, Ord)

instance Show LedgerRelayAccessPoint where
    show (LedgerRelayAccessDomain domain port) =
      "LedgerRelayAccessDomain " ++ show domain ++ " " ++ show port
    show (LedgerRelayAccessSRVDomain domain) =
      "LedgerRelayAccessSRVDomain " ++ show domain
    show (LedgerRelayAccessAddress ip port) =
      "RelayAccessAddress \"" ++ show ip ++ "\" " ++ show port

-- 'IP' nor 'IPv6' is strict, 'IPv4' is strict only because it's a newtype for
-- a primitive type ('Word32').
--
instance NFData LedgerRelayAccessPoint where
  rnf (LedgerRelayAccessDomain !_domain !_port) = ()
  rnf (LedgerRelayAccessSRVDomain !_domain) = ()
  rnf (LedgerRelayAccessAddress ip !_port) =
    case ip of
      IP.IPv4 ipv4 -> rnf (IP.fromIPv4w ipv4)
      IP.IPv6 ipv6 -> rnf (IP.fromIPv6w ipv6)

instance FromJSON LedgerRelayAccessPoint where
  parseJSON = withObject "RelayAccessPoint" $ \o -> do
    addr <- encodeUtf8 <$> o .: "address"
    let res = flip parseMaybe o $ const do
          port <- o .: "port"
          return (toRelayAccessPoint addr port)
    case res of
      Nothing  -> return $ LedgerRelayAccessSRVDomain (fullyQualified addr)
      Just rap -> return rap

    where
      toRelayAccessPoint :: DNS.Domain -> Int -> LedgerRelayAccessPoint
      toRelayAccessPoint address port =
          case readMaybe (BSC.unpack address) of
            Nothing   -> LedgerRelayAccessDomain (fullyQualified address) (fromIntegral port)
            Just addr -> LedgerRelayAccessAddress addr (fromIntegral port)
      fullyQualified = \case
        domain | Just (_, '.') <- BSC.unsnoc domain -> domain
               | otherwise -> domain `BSC.snoc` '.'

instance ToJSON LedgerRelayAccessPoint where
  toJSON (LedgerRelayAccessDomain addr port) =
    object
      [ "address" .= decodeUtf8 addr
      , "port" .= (fromIntegral port :: Int)
      ]
  toJSON (LedgerRelayAccessSRVDomain domain) =
    object
      [ "address" .= decodeUtf8 domain
      ]
  toJSON (LedgerRelayAccessAddress ip port) =
    object
      [ "address" .= Text.pack (show ip)
      , "port" .= (fromIntegral port :: Int)
      ]

instance ToCBOR LedgerRelayAccessPoint where
  toCBOR rap = case rap of
    LedgerRelayAccessDomain domain port ->
         Codec.encodeListLen 3
      <> Codec.encodeWord8 0
      <> serialise' port
      <> toCBOR domain
    LedgerRelayAccessAddress (IP.IPv4 ipv4) port ->
         Codec.encodeListLen 3
      <> Codec.encodeWord8 1
      <> serialise' port
      <> toCBOR (IP.fromIPv4 ipv4)
    LedgerRelayAccessAddress (IP.IPv6 ip6) port ->
         Codec.encodeListLen 3
      <> Codec.encodeWord8 2
      <> serialise' port
      <> toCBOR (IP.fromIPv6 ip6)
    LedgerRelayAccessSRVDomain domain ->
         Codec.encodeListLen 2
      <> Codec.encodeWord8 3
      <> toCBOR domain
    where
      serialise' = toCBOR . toInteger

instance FromCBOR LedgerRelayAccessPoint where
  fromCBOR = do
    listLen <- Codec.decodeListLen
    constructorTag <- Codec.decodeWord8
    unless (   listLen == 3
            || (listLen == 2 && constructorTag == 3))
      $ fail $ "Unrecognized LedgerRelayAccessPoint list length "
               <> show listLen <> "for constructor tag "
               <> show constructorTag
    case constructorTag of
      0 -> do
        port <- decodePort
        domain <- fromCBOR
        return $ LedgerRelayAccessDomain domain port
      1 -> do
        port <- decodePort
        ip <- IP.IPv4 . IP.toIPv4 <$> fromCBOR
        return $ LedgerRelayAccessAddress ip port
      2 -> do
        port <- decodePort
        ip <- IP.IPv6 . IP.toIPv6 <$> fromCBOR
        return $ LedgerRelayAccessAddress ip port
      3 -> do
        LedgerRelayAccessSRVDomain <$> fromCBOR
      _ -> fail $ "Unrecognized LedgerRelayAccessPoint tag: " <> show constructorTag
    where
      decodePort = fromIntegral @Int <$> fromCBOR


-- | A new type wrapper which provides backward compatible `FromJSON` instance
-- for `LedgerRelayAccessPoint`.
--
newtype LedgerRelayAccessPointV1 = LedgerRelayAccessPointV1 { getLedgerReelayAccessPointV1 :: LedgerRelayAccessPoint }

instance FromJSON LedgerRelayAccessPointV1 where
  parseJSON = withObject "RelayAccessPoint" $ \o -> do
    addr <- encodeUtf8 <$> o .: "address"
    let res = flip parseMaybe o $ const do
          port <- o .: "port"
          return (toRelayAccessPoint addr port)
    case res of
      Nothing  -> return $ LedgerRelayAccessPointV1 $ LedgerRelayAccessSRVDomain (fullyQualified addr)
      Just rap -> return $ LedgerRelayAccessPointV1 rap

    where
      toRelayAccessPoint :: DNS.Domain -> Int -> LedgerRelayAccessPoint
      toRelayAccessPoint address port =
          case readMaybe (BSC.unpack address) of
            Nothing   -> LedgerRelayAccessDomain (fullyQualified address) (fromIntegral port)
            Just addr -> LedgerRelayAccessAddress addr (fromIntegral port)

      fullyQualified = \case
        domain | Just (_, '.') <- BSC.unsnoc domain -> domain
               | otherwise -> domain `BSC.snoc` '.'


-- | Type of a DNS SRV prefix as defined by CIP#0155
--
type SRVPrefix = DNS.Domain

prefixLedgerRelayAccessPoint
  :: SRVPrefix
  -> LedgerRelayAccessPoint
  -> RelayAccessPoint
prefixLedgerRelayAccessPoint _prefix (LedgerRelayAccessDomain domain port)
  = RelayAccessDomain domain port
prefixLedgerRelayAccessPoint  prefix (LedgerRelayAccessSRVDomain domain)
  = RelayAccessSRVDomain (prefix <> "." <> domain)
prefixLedgerRelayAccessPoint _prefix (LedgerRelayAccessAddress ip port)
  = RelayAccessAddress ip port
