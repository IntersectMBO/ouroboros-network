{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Network.NodeToNode.Version
  ( NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , DiffusionMode (..)
  , ConnectionMode (..)
  , PerasSupportStatus (..)
    -- * Codecs
  , nodeToNodeVersionCodec
  , nodeToNodeCodecCBORTerm
    -- * Feature predicates
  , isValidNtnVersionDataForVersion
  , getLocalPerasSupportStatus
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Codec.CBOR.Term qualified as CBOR

import Cardano.Base.FeatureFlags
import Control.DeepSeq
import GHC.Generics
import NoThunks.Class (NoThunks)
import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.DiffusionMode
import Ouroboros.Network.Handshake.Acceptable (Accept (..), Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.Magic
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PerasSupportStatus

-- | Enumeration of node to node protocol versions.
--
data NodeToNodeVersion =
    -- commented out versions that can't cross into the current HF era
    -- NodeToNodeV_7
    -- -- ^ Changes:
    -- --
    -- -- * new 'KeepAlive' codec
    -- -- * Enable @CardanoNodeToNodeVersion5@, i.e., Alonzo
    -- | NodeToNodeV_8
    -- -- ^ Changes:
    -- --
    -- -- * Enable block diffusion pipelining in ChainSync and BlockFetch logic.
    -- | NodeToNodeV_9
    -- -- ^ Changes:
    -- --
    -- -- * Enable @CardanoNodeToNodeVersion6@, i.e., Babbage
    -- | NodeToNodeV_10
    -- -- ^ Changes:
    -- --
    -- -- * Enable full duplex connections.
    -- | NodeToNodeV_11
    -- -- ^ Changes:
    -- --
    -- -- * Adds a new extra parameter to handshake: PeerSharing
    -- --   This version is needed to support the new  Peer Sharing miniprotocol
    -- --   older versions that are negotiated will appear as not participating
    -- --   in Peer Sharing to newer versions.
    -- -- * Adds `query` to NodeToClientVersionData.
    -- | NodeToNodeV_12
    -- -- ^ No changes.
    -- --
    -- -- (In the past, this enabled Conway, but the negotiated 'NodeToNodeVersion'
    -- -- no longer en-/disables eras.)
    -- | NodeToNodeV_13
    -- ^ Changes:
    --
    -- * Removed PeerSharingPrivate constructor
    -- * Fixed Codec to disable PeerSharing with buggy versions 11 and 12.
    -- * Disable PeerSharing with InitiatorOnly nodes, since they do not run
    --   peer sharing server side and can not reply to requests.
    NodeToNodeV_14
    -- ^ Plomin HF, mandatory on mainnet as of 2025.01.29
  | NodeToNodeV_15
    -- ^ SRV support
  | NodeToNodeV_16
    -- ^ Experimental.
    --
    -- Adds Peras mini-protocols (if 'PerasFlag' is set).
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, NFData, NoThunks)

nodeToNodeVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToNodeVersion
nodeToNodeVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm NodeToNodeV_14 = CBOR.TInt 14
    encodeTerm NodeToNodeV_15 = CBOR.TInt 15
    encodeTerm NodeToNodeV_16 = CBOR.TInt 16

    decodeTerm (CBOR.TInt 14) = Right NodeToNodeV_14
    decodeTerm (CBOR.TInt 15) = Right NodeToNodeV_15
    decodeTerm (CBOR.TInt 16) = Right NodeToNodeV_16
    decodeTerm (CBOR.TInt n) = Left ( T.pack "decode NodeToNodeVersion: unknown tag: "
                                        <> T.pack (show n)
                                    , Just n
                                    )
    decodeTerm _ = Left ( T.pack "decode NodeToNodeVersion: unexpected term"
                        , Nothing)


-- | Version data for NodeToNode protocol
--
data NodeToNodeVersionData = NodeToNodeVersionData
  { networkMagic       :: !NetworkMagic
  , diffusionMode      :: !DiffusionMode
  , peerSharing        :: !PeerSharing
  , query              :: !Bool
  , perasSupportStatus :: !PerasSupportStatus
  }
  deriving (Show, Eq)

instance Acceptable NodeToNodeVersionData where
    -- | Check that both side use the same 'networkMagic'.  Choose smaller one
    -- from both 'diffusionMode's, e.g. if one is running in 'InitiatorOnlyMode'
    -- agree on it. Agree on the same 'PeerSharing' value.
    -- Also agree on whether or not Peras should be used.
    acceptableVersion local remote
      | networkMagic local == networkMagic remote
      = let acceptedDiffusionMode = diffusionMode local `min` diffusionMode remote
            acceptedPerasSupportStatus = perasSupportStatus local `min` perasSupportStatus remote
         in Accept NodeToNodeVersionData
              { networkMagic       = networkMagic local
              , diffusionMode      = acceptedDiffusionMode
              , peerSharing        = peerSharing local <> peerSharing remote
              , query              = query local || query remote
              , perasSupportStatus = acceptedPerasSupportStatus
              }
      | otherwise
      = Refuse $ T.pack $ "version data mismatch: "
                       ++ show local
                       ++ " /= " ++ show remote

instance Queryable NodeToNodeVersionData where
    queryVersion = query

-- | `perasSupportStatus` field is introduced with `NodeToNodeV_16`, and thus should be
-- set to `PerasUnsupported` (and not be serialized) for versions before that.
isValidNtnVersionDataForVersion :: NodeToNodeVersion -> NodeToNodeVersionData -> Bool
isValidNtnVersionDataForVersion version ntnData =
  version >= NodeToNodeV_16 || perasSupportStatus ntnData == PerasUnsupported

-- | Determine the local node's Peras support status based on feature flags and version.
getLocalPerasSupportStatus :: Set CardanoFeatureFlag -> NodeToNodeVersion -> PerasSupportStatus
getLocalPerasSupportStatus featureFlags v =
  if Set.member PerasFlag featureFlags && v >= NodeToNodeV_16
    then PerasSupported
    else PerasUnsupported

-- | Beware, encoding an invalid NodeToNodeVersionData (see `isValidNtnVersionDataForVersion`) for
-- a given version will fail if a future field is set to a value other than its default forwards
-- compatibility one. This way `encodeTerm` and `decodeTerm` are only inverses for valid data.
nodeToNodeCodecCBORTerm :: NodeToNodeVersion -> CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm version = CodecCBORTerm { encodeTerm = encodeTerm, decodeTerm = decodeTerm }
  where
    encodeTerm :: NodeToNodeVersionData -> CBOR.Term
    encodeTerm ntnData@NodeToNodeVersionData{ networkMagic, diffusionMode, peerSharing, query, perasSupportStatus }
      | not (isValidNtnVersionDataForVersion version ntnData) = error "perasSupportStatus should be PerasUnsupported for versions strictly before NodeToNodeV_16"
      | otherwise =
        CBOR.TList $
             [ CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)
             , CBOR.TBool (case diffusionMode of
                           InitiatorOnlyDiffusionMode         -> True
                           InitiatorAndResponderDiffusionMode -> False)
             , CBOR.TInt (case peerSharing of
                           PeerSharingDisabled -> 0
                           PeerSharingEnabled  -> 1)
             , CBOR.TBool query
             ]
          ++ [CBOR.TBool (case perasSupportStatus of
                  PerasUnsupported -> False
                  PerasSupported   -> True)
             | version >= NodeToNodeV_16
             ]

    decodeTerm :: CBOR.Term -> Either Text NodeToNodeVersionData
    decodeTerm = \case
      (CBOR.TList (CBOR.TInt networkMagic : CBOR.TBool diffusionMode : CBOR.TInt peerSharing : CBOR.TBool query : perasSupportStatusOptional)) ->
            NodeToNodeVersionData
        <$> decodeNetworkMagic networkMagic
        <*> decodeDiffusionMode diffusionMode
        <*> decodePeerSharing peerSharing
        <*> decodeQuery query
        <*> decodePerasSupportStatusOptional perasSupportStatusOptional
          where
            decodeNetworkMagic x
              | x >= 0 , x <= 0xffffffff = pure $ NetworkMagic (fromIntegral x)
              | otherwise                = die $ "networkMagic out of bound: " <> show x

            decodeDiffusionMode dm = pure $
              if dm
                then InitiatorOnlyDiffusionMode
                else InitiatorAndResponderDiffusionMode

            decodePeerSharing ps
              | ps == 0   = pure PeerSharingDisabled
              | ps == 1   = pure PeerSharingEnabled
              | otherwise = die $ "peerSharing is out of bound: " <> show ps

            decodeQuery = pure

            decodePerasSupportStatusOptional = \case
              []                   | version <  NodeToNodeV_16 -> pure PerasUnsupported
              [CBOR.TBool perasSupportStatus] | version >= NodeToNodeV_16 -> pure $
                if perasSupportStatus
                  then PerasSupported
                  else PerasUnsupported
              l -> die $ "invalid encoding for perasSupportStatus given the version " <> show version <> ": " <> show l

      other -> die $ "unexpected encoding when decoding NodeToNodeVersionData: " <> show other

    die = Left . T.pack

data ConnectionMode = UnidirectionalMode | DuplexMode
