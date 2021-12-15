{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}

module Ouroboros.Network.PeerSelection.RelayAccessPoint
  ( DomainAccessPoint (..)
  , RelayAccessPoint (.., RelayDomainAccessPoint)
  , IP.IP (..)
    -- * Socket type re-exports
  , Socket.PortNumber
  ) where

import           Control.DeepSeq (NFData (..))

import           Data.Aeson
import qualified Data.IP as IP
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Text.Read (readMaybe)

import qualified Network.DNS as DNS
import qualified Network.Socket as Socket

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
  deriving (Show, Eq, Ord)


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
