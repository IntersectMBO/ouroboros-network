{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Ouroboros.Network.PeerSelection.RelayAccessPoint
  ( RelayAccessPoint (..)
  , RelayAccessPointCoded (..)
  , IP.IP (..)
    -- * Socket type re-exports
  , Socket.PortNumber
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (unless)

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 (unpack)
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
                      | RelayAccessAddress   !IP.IP      !Socket.PortNumber
  deriving (Eq, Ord)

newtype RelayAccessPointCoded = RelayAccessPointCoded { unRelayAccessPointCoded :: RelayAccessPoint }

-- | These instances are used to serialize 'LedgerPeerSnapshot'
-- consensus LocalStateQuery server which uses these instances
-- for all its query responses. It appears they provide some improved
-- debugging diagnostics over Serialize instances.
instance ToCBOR RelayAccessPointCoded where
  toCBOR (RelayAccessPointCoded rap) = case rap of
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

instance FromCBOR RelayAccessPointCoded where
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
        RelayAccessPointCoded <$> (RelayAccessDomain <$> fromCBOR <*> pure port)
      1 -> do
        port <- decodePort
        ip4 <- IP.IPv4 . IP.toIPv4 <$> fromCBOR
        return . RelayAccessPointCoded $ RelayAccessAddress ip4 port
      2 -> do
        port <- decodePort
        ip6 <- IP.IPv6 . IP.toIPv6 <$> fromCBOR
        return . RelayAccessPointCoded $ RelayAccessAddress ip6 port
      3 -> do
        RelayAccessPointCoded <$> (RelayAccessSRVDomain <$> fromCBOR)
      _ -> fail $ "Unrecognized RelayAccessPoint tag: " <> show constructorTag
    where
      decodePort = fromIntegral @Int <$> fromCBOR

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
      Nothing  -> return $ RelayAccessSRVDomain addr
      Just rap -> return rap

    where
      toRelayAccessPoint :: DNS.Domain -> Int -> RelayAccessPoint
      toRelayAccessPoint address port =
          case readMaybe (unpack address) of
            Nothing   -> RelayAccessDomain address (fromIntegral port)
            Just addr -> RelayAccessAddress addr (fromIntegral port)

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
