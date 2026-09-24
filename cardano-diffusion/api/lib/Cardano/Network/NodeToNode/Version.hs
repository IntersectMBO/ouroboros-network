{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeApplications   #-}

module Cardano.Network.NodeToNode.Version
  ( NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , DiffusionMode (..)
  , ConnectionMode (..)
  , PerasSupport (..)
    -- * Codecs
  , nodeToNodeVersionCodec
  , nodeToNodeVersionDataCodec
  , NetworkMagic (..)
    -- * Feature predicates
  , isValidNtnVersionDataForVersion
  , getLocalPerasSupport
  , minPerasVersion
    -- * Internals exported for testing purposes
  , encodeNodeToNodeVersionDataHelper
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
import Ouroboros.Network.PerasSupport
import Ouroboros.Network.Util (PrettyShow (..))

-- | Enumeration of node to node protocol versions.
--
-- Historical versions:
--
-- @
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
-- -- ^ Changes:
-- -- * Removed PeerSharingPrivate constructor
-- -- * Fixed Codec to disable PeerSharing with buggy versions 11 and 12.
-- -- * Disable PeerSharing with InitiatorOnly nodes, since they do not run
-- --   peer sharing server side and can not reply to requests.
-- @
--
data NodeToNodeVersion =
    NodeToNodeV_14
    -- ^ Plomin HF, mandatory on mainnet as of 2025.01.29
  | NodeToNodeV_15
    -- ^ SRV support
  | NodeToNodeV_16
    -- ^ Support handshake on 32bit systems.
  | NodeToNodeV_17
    -- ^ Experimental.
    --
    -- Adds support for Peras mini-protocols (if 'PerasFlag' is set).
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)
  deriving anyclass (NFData, NoThunks, PrettyShow)

nodeToNodeVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToNodeVersion
nodeToNodeVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm NodeToNodeV_14 = CBOR.TInt 14
    encodeTerm NodeToNodeV_15 = CBOR.TInt 15
    encodeTerm NodeToNodeV_16 = CBOR.TInt 16
    encodeTerm NodeToNodeV_17 = CBOR.TInt 17

    decodeTerm (CBOR.TInt 14) = Right NodeToNodeV_14
    decodeTerm (CBOR.TInt 15) = Right NodeToNodeV_15
    decodeTerm (CBOR.TInt 16) = Right NodeToNodeV_16
    decodeTerm (CBOR.TInt 17) = Right NodeToNodeV_17
    decodeTerm (CBOR.TInt n) = Left ( T.pack "decode NodeToNodeVersion: unknown tag: "
                                        <> T.pack (show n)
                                    , Just n
                                    )
    decodeTerm _ = Left ( T.pack "decode NodeToNodeVersion: unexpected term"
                        , Nothing)


-- | Version data for NodeToNode protocol
--
data NodeToNodeVersionData = NodeToNodeVersionData
  { networkMagic  :: !NetworkMagic
  , diffusionMode :: !DiffusionMode
  , peerSharing   :: !PeerSharing
  , query         :: !Bool
  , perasSupport  :: !PerasSupport
  }
  deriving stock (Show, Eq, Generic)
  -- 'Eq' instance is not provided, it is not what we need in version
  -- negotiation (see 'Acceptable' instance below).
  deriving anyclass (NFData, PrettyShow)

instance Acceptable NodeToNodeVersionData where
    -- | Check that both side use the same 'networkMagic'.  Choose smaller one
    -- from both 'diffusionMode's, e.g. if one is running in 'InitiatorOnlyMode'
    -- agree on it. Agree on the same 'PeerSharing' value.
    -- Also agree on whether or not Peras should be used.
    acceptableVersion local remote
      | networkMagic local == networkMagic remote
      = let acceptedDiffusionMode = diffusionMode local `min` diffusionMode remote
            acceptedPerasSupport = perasSupport local `min` perasSupport remote
         in Accept NodeToNodeVersionData
              { networkMagic       = networkMagic local
              , diffusionMode      = acceptedDiffusionMode
              , peerSharing        = peerSharing local <> peerSharing remote
              , query              = query local || query remote
              , perasSupport       = acceptedPerasSupport
              }
      | otherwise
      = Refuse $ T.pack $ "version data mismatch: "
                       ++ show local
                       ++ " /= " ++ show remote

instance Queryable NodeToNodeVersionData where
    queryVersion = query

-- | NodeToNodeVersion which introduced Peras support
--
minPerasVersion :: NodeToNodeVersion
minPerasVersion = NodeToNodeV_17

-- | `perasSupport` field is introduced with `NodeToNodeV_17`, and thus should be
-- set to `PerasUnsupported` (and not be serialized) for versions before that.
isValidNtnVersionDataForVersion :: NodeToNodeVersion -> NodeToNodeVersionData -> Bool
isValidNtnVersionDataForVersion version ntnData =
  version >= minPerasVersion || perasSupport ntnData == PerasUnsupported


-- | Determine the local node's Peras support status based on feature flags and version.
getLocalPerasSupport :: Set CardanoFeatureFlag -> NodeToNodeVersion -> PerasSupport
getLocalPerasSupport featureFlags v =
  if Set.member PerasFlag featureFlags && v >= minPerasVersion
    then PerasSupported
    else PerasUnsupported


-- | A helper function used to encode `NodeToNodeVersionData`.
--
encodeNodeToNodeVersionDataHelper
  :: NodeToNodeVersion
  -> Integer -- ^ NetworkMagic
  -> DiffusionMode
  -> PeerSharing
  -> Bool
  -> PerasSupport
  -> CBOR.Term
encodeNodeToNodeVersionDataHelper
  version networkMagic diffusionMode peerSharing query perasSupport
  =
  CBOR.TList $
      [ -- 'CBOR.TInteger' serialises to the same bytes as 'CBOR.TInt' for any
        -- value the latter can hold, and `cborg` decodes it back as a
        -- 'CBOR.TInt' whenever it fits 'Int', so nodes which only accept
        -- 'CBOR.TInt' still decode it.  Unlike 'CBOR.TInt', it doesn't wrap on
        -- 32bit platforms.
        CBOR.TInteger networkMagic

       , CBOR.TBool (case diffusionMode of
                     InitiatorOnlyDiffusionMode         -> True
                     InitiatorAndResponderDiffusionMode -> False)
       , CBOR.TInt (case peerSharing of
                     PeerSharingDisabled -> 0
                     PeerSharingEnabled  -> 1)
       , CBOR.TBool query
       ]
    ++ [CBOR.TBool (perasSupportToBool perasSupport)
       | version >= NodeToNodeV_17
       ]


-- | Beware, encoding an invalid NodeToNodeVersionData (see `isValidNtnVersionDataForVersion`) for
-- a given version will fail if a future field is set to a value other than its default forwards
-- compatibility one. This way `encodeTerm` and `decodeTerm` are only inverses for valid data.
nodeToNodeCodecCBORTerm :: NodeToNodeVersion -> CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm version = CodecCBORTerm { encodeTerm = encodeTerm, decodeTerm = decodeTerm }
  where
    encodeTerm :: NodeToNodeVersionData -> CBOR.Term
    encodeTerm ntnData@NodeToNodeVersionData{ networkMagic, diffusionMode, peerSharing, query, perasSupport }
      | not (isValidNtnVersionDataForVersion version ntnData) = error "perasSupport should be PerasUnsupported for versions strictly before NodeToNodeV_17"
      | otherwise = encodeNodeToNodeVersionDataHelper
                      version
                      (fromIntegral $ unNetworkMagic networkMagic)
                      diffusionMode
                      peerSharing
                      query
                      perasSupport

    -- The network magic is accepted either as a 'CBOR.TInt' or a
    -- 'CBOR.TInteger': `cborg` decodes an unsigned integer as a 'CBOR.TInt'
    -- only if it fits 'Int', which on 32bit platforms excludes magics above
    -- `maxBound :: Int32`.
    decodeTerm :: CBOR.Term -> Either Text NodeToNodeVersionData
    decodeTerm = \case
        (CBOR.TList
          ( CBOR.TInt networkMagic
          : CBOR.TBool diffusionMode
          : CBOR.TInt peerSharing
          : CBOR.TBool query
          : perasSupportOptional))
          -> decode (fromIntegral networkMagic)
                    diffusionMode
                    peerSharing
                    query
                    perasSupportOptional
        (CBOR.TList
          ( CBOR.TInteger networkMagic
          : CBOR.TBool diffusionMode
          : CBOR.TInt peerSharing
          : CBOR.TBool query
          : perasSupportOptional))
          -> decode networkMagic
                    diffusionMode
                    peerSharing
                    query
                    perasSupportOptional
        other -> err $ "unexpected encoding when decoding NodeToNodeVersionData: " <> show other
      where
        decode :: Integer     -- ^ network magic
               -> Bool        -- ^ diffusion mode
               -> Int         -- ^ peer sharing
               -> Bool        -- ^ query
               -> [CBOR.Term] -- ^ tail (Peras support)
               -> Either Text NodeToNodeVersionData
        decode networkMagic diffusionMode peerSharing query perasSupportOptional =
                NodeToNodeVersionData
            <$> decodeNetworkMagic networkMagic
            <*> decodeDiffusionMode diffusionMode
            <*> decodePeerSharing peerSharing
            <*> decodeQuery query
            <*> decodePerasSupportOptional perasSupportOptional
          where
            decodeNetworkMagic :: Integer -> Either Text NetworkMagic
            decodeNetworkMagic x
              | x >= 0
              , x <= 0xffffffff
              = pure $ NetworkMagic (fromIntegral x)

              | otherwise
              = err $ "networkMagic out of bound: " <> show x

            decodeDiffusionMode dm = pure $
              if dm
                then InitiatorOnlyDiffusionMode
                else InitiatorAndResponderDiffusionMode

            decodePeerSharing ps
              | ps == 0   = pure PeerSharingDisabled
              | ps == 1   = pure PeerSharingEnabled
              | otherwise = err $ "peerSharing is out of bound: " <> show ps

            decodeQuery = pure

            decodePerasSupportOptional = \case
              []                        | version <  NodeToNodeV_17 -> pure PerasUnsupported
              [CBOR.TBool perasSupport] | version >= NodeToNodeV_17 -> pure $
                if perasSupport
                  then PerasSupported
                  else PerasUnsupported
              l -> err $ "invalid encoding for perasSupport given the version " <> show version <> ": " <> show l


    err = Left . T.pack

nodeToNodeVersionDataCodec :: VersionDataCodec NodeToNodeVersion NodeToNodeVersionData
nodeToNodeVersionDataCodec = mkVersionedCodecCBORTerm nodeToNodeCodecCBORTerm

data ConnectionMode = UnidirectionalMode | DuplexMode

