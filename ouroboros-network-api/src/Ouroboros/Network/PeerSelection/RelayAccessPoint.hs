{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Ouroboros.Network.PeerSelection.RelayAccessPoint
  ( DomainAccessPoint (..)
  , RelayAccessPoint (.., RelayDomainAccessPoint)
  , IP.IP (..)
    -- * Socket type re-exports
  , Socket.PortNumber
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (when)

import Data.Aeson
import Data.IP qualified as IP
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Read (readMaybe)

import Cardano.Binary
import Network.DNS qualified as DNS
import Network.Socket qualified as Socket

-- | A product of a 'DNS.Domain' and 'Socket.PortNumber'.  After resolving the
-- domain we will use the 'Socket.PortNumber' to form 'Socket.SockAddr'.
--
data DomainAccessPoint = DomainAccessPoint {
    dapDomain     :: !DNS.Domain,
    dapPortNumber :: !Socket.PortNumber
  }
  deriving (Show, Eq, Ord)

instance FromJSON DomainAccessPoint where
  parseJSON = withObject "DomainAccessPoint" $ \v ->
    DomainAccessPoint
      <$> (encodeUtf8 <$> v .: "address")
      <*> ((fromIntegral :: Int -> Socket.PortNumber) <$> v .: "port")

instance ToJSON DomainAccessPoint where
  toJSON da =
    object
      [ "address" .= decodeUtf8 (dapDomain da)
      , "port" .= (fromIntegral (dapPortNumber da) :: Int)
      ]

-- | A relay can have either an IP address and a port number or
-- a domain with a port number
--
data RelayAccessPoint = RelayAccessDomain  !DNS.Domain !Socket.PortNumber
                      | RelayAccessAddress !IP.IP      !Socket.PortNumber
  deriving (Eq, Ord)

-- | These instances are used to serialize 'LedgerPeerSnapshot'
-- consensus LocalStateQuery server which uses these instances
-- for all its query responses. It appears they provide some improved
-- debugging diagnostics over Serialize instances.
instance ToCBOR RelayAccessPoint where
  toCBOR = \case
    RelayAccessDomain domain port ->
         encodeListLen 3
      <> encodeWord8 0
      <> serialize' port
      <> toCBOR domain
    RelayAccessAddress (IP.IPv4 ipv4) port ->
         encodeListLen 3
      <> encodeWord8 1
      <> serialize' port
      <> toCBOR (IP.fromIPv4 ipv4)
    RelayAccessAddress (IP.IPv6 ip6) port ->
         encodeListLen 3
      <> encodeWord8 2
      <> serialize' port
      <> toCBOR (IP.fromIPv6 ip6)
    where
      serialize' = toCBOR . toInteger

instance FromCBOR RelayAccessPoint where
  fromCBOR = do
    listLen <- decodeListLen
    when (listLen /= 3) . fail $    "Unrecognized RelayAccessPoint list length "
                                 <> show listLen
    constructorTag <- decodeWord8
    port <- fromInteger <$> fromCBOR @Integer
    case constructorTag of
      0 -> do
        domain <- fromCBOR
        return $ RelayAccessDomain domain port
      1 -> do
        ip4 <- IP.IPv4 . IP.toIPv4 <$> fromCBOR
        return $ RelayAccessAddress ip4 port
      2 -> do
        ip6 <- IP.IPv6 . IP.toIPv6 <$> fromCBOR
        return $ RelayAccessAddress ip6 port
      _ -> fail $ "Unrecognized RelayAccessPoint tag: " <> show constructorTag

instance Show RelayAccessPoint where
    show (RelayAccessDomain domain port) =
      "RelayAccessDomain " ++ show domain ++ " " ++ show port
    show (RelayAccessAddress ip port) =
      "RelayAccessAddress \"" ++ show ip ++ "\" " ++ show port


-- | 'RelayDomainAccessPoint' a bidirectional pattern which links
-- 'RelayAccessDomain' and 'DomainAccessPoint'.
--
pattern RelayDomainAccessPoint :: DomainAccessPoint -> RelayAccessPoint
pattern RelayDomainAccessPoint dap <- (viewRelayAccessPoint -> Just dap)
  where
    RelayDomainAccessPoint DomainAccessPoint {dapDomain, dapPortNumber} =
      RelayAccessDomain dapDomain dapPortNumber

{-# COMPLETE RelayDomainAccessPoint, RelayAccessAddress #-}

viewRelayAccessPoint :: RelayAccessPoint -> Maybe DomainAccessPoint
viewRelayAccessPoint (RelayAccessDomain dapDomain dapPortNumber) =
    Just DomainAccessPoint {dapDomain, dapPortNumber}
viewRelayAccessPoint  RelayAccessAddress {} =
    Nothing


-- 'IP' nor 'IPv6' is strict, 'IPv4' is strict only because it's a newtype for
-- a primitive type ('Word32').
--
instance NFData RelayAccessPoint where
  rnf (RelayAccessDomain !_domain !_port) = ()
  rnf (RelayAccessAddress ip !_port) =
    case ip of
      IP.IPv4 ipv4 -> rnf (IP.fromIPv4w ipv4)
      IP.IPv6 ipv6 -> rnf (IP.fromIPv6w ipv6)

instance FromJSON RelayAccessPoint where
  parseJSON = withObject "RelayAccessPoint" $ \v -> do
    addr <- v .: "address"
    port <- v .: "port"
    return (toRelayAccessPoint addr port)

instance ToJSON RelayAccessPoint where
  toJSON (RelayAccessDomain addr port) =
    object
      [ "address" .= decodeUtf8 addr
      , "port" .= (fromIntegral port :: Int)
      ]
  toJSON (RelayAccessAddress ip port) =
    object
      [ "address" .= Text.pack (show ip)
      , "port" .= (fromIntegral port :: Int)
      ]

-- | Parse a address field as either an IP address or a DNS address.
-- Returns corresponding RelayAccessPoint.
--
toRelayAccessPoint :: Text -> Int -> RelayAccessPoint
toRelayAccessPoint address port =
    case readMaybe (Text.unpack address) of
      Nothing   -> RelayAccessDomain (encodeUtf8 address) (fromIntegral port)
      Just addr -> RelayAccessAddress addr (fromIntegral port)
