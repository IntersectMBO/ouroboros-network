{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ouroboros.Network.PeerSelection.RelayAccessPoint
  ( DomainAccessPoint (..)
  , DomainPlainAccessPoint (..)
  , DomainSRVAccessPoint (..)
  , RelayAccessPoint (.., RelayDomainAccessPoint)
  , RelayAccessPointCoded (..)
  , IP.IP (..)
    -- * Socket type re-exports
  , Socket.PortNumber
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (unless)

import Data.Aeson
import Data.Aeson.Types
import Data.IP qualified as IP
-- import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString.Char8 (unpack)
import Text.Read (readMaybe)

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Binary qualified as Codec
import Network.DNS qualified as DNS
import Network.Socket qualified as Socket
import GHC.Generics

-- | Types of domains supported
-- NB: A deliberately limited subset of SRV is supported.
-- Concretely, a peer from only the top priority level may
-- be given a chance to connect by the peer selection governor. Other
-- priority levels will not be considered. If there are multiple records
-- of the top priority (ie. lowest numerical value), a weighted random
-- sampling is in fact performed by this implementation, and addresses
-- and port are obtained from the winner.
-- (cf. https://www.ietf.org/rfc/rfc2782.txt)
--
data DomainAccessPoint = DomainAccessPoint !DomainPlainAccessPoint
                       -- ^ An @A@ or @AAAA@ DNS record
                       | DomainSRVAccessPoint !DomainSRVAccessPoint
                       -- ^ A @SRV@ DNS record
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- | A product of a 'DNS.Domain' and 'Socket.PortNumber'.  After resolving the
-- domain we will use the 'Socket.PortNumber' to form 'Socket.SockAddr'.
--
data DomainPlainAccessPoint = DomainPlain {
    dapDomain     :: !DNS.Domain,
    dapPortNumber :: !Socket.PortNumber
  }
  deriving (Eq, Show, Ord)

-- | An SRV domain is just a DNS.Domain
--
newtype DomainSRVAccessPoint = DomainSRV {
  srvDomain :: DNS.Domain }
  deriving (Show, Eq, Ord)

instance FromJSON DomainPlainAccessPoint where
  parseJSON = withObject "DomainPlainAccessPoint" $ \v -> do
    DomainPlain
      <$> (encodeUtf8 <$> v .: "address")
      <*> ((fromIntegral :: Int -> Socket.PortNumber) <$> v .: "port")

instance ToJSON DomainPlainAccessPoint where
  toJSON da =
    object
      [ "address" .= decodeUtf8 (dapDomain da)
      , "port" .= (fromIntegral (dapPortNumber da) :: Int)
      ]

instance FromJSON DomainSRVAccessPoint where
  parseJSON = withObject "DomainSRVAccessPoint" $ \v -> do
    DomainSRV
      <$> (encodeUtf8 <$> v .: "address")

instance ToJSON DomainSRVAccessPoint where
  toJSON (DomainSRV domain) =
    object
      [ "address" .= decodeUtf8 domain
      ]

-- | A relay can have either an IP address and a port number or
-- a domain with a port number
--
data RelayAccessPoint = RelayAccessDomain  !DNS.Domain !Socket.PortNumber
                      | RelayAccessSRVDomain !DNS.Domain
                      | RelayAccessAddress !IP.IP      !Socket.PortNumber
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


-- | 'RelayDomainAccessPoint' a bidirectional pattern which links
-- 'RelayAccessDomain' and 'DomainAccessPoint'.
--
pattern RelayDomainAccessPoint :: DomainAccessPoint -> RelayAccessPoint
pattern RelayDomainAccessPoint dap <- (viewRelayAccessPoint -> Just dap)
  where
    RelayDomainAccessPoint (DomainAccessPoint (DomainPlain {dapDomain, dapPortNumber})) =
      RelayAccessDomain dapDomain dapPortNumber
    RelayDomainAccessPoint (DomainSRVAccessPoint (DomainSRV {srvDomain})) =
      RelayAccessSRVDomain srvDomain

{-# COMPLETE RelayDomainAccessPoint, RelayAccessAddress #-}

viewRelayAccessPoint :: RelayAccessPoint -> Maybe DomainAccessPoint
viewRelayAccessPoint (RelayAccessDomain dapDomain dapPortNumber) =
    Just $ DomainAccessPoint $ DomainPlain {dapDomain, dapPortNumber}
viewRelayAccessPoint (RelayAccessSRVDomain srvDomain) =
    Just $ DomainSRVAccessPoint $ DomainSRV {srvDomain}
viewRelayAccessPoint  RelayAccessAddress {} =
    Nothing


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
      Nothing -> return $ RelayAccessSRVDomain addr
      Just rap -> return rap

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

-- | Parse a address field as either an IP address or a DNS address.
-- Returns corresponding RelayAccessPoint.
--
toRelayAccessPoint :: DNS.Domain -> Int -> RelayAccessPoint
toRelayAccessPoint address port =
    case readMaybe (unpack address) of
      Nothing   -> RelayAccessDomain address (fromIntegral port)
      Just addr -> RelayAccessAddress addr (fromIntegral port)
