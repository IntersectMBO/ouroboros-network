{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.NodeToNode.Version
  ( NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , DiffusionMode (..)
  , ConnectionMode (..)
  , nodeToNodeVersionCodec
  , nodeToNodeCodecCBORTerm
  , isPipeliningEnabled
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)

import Codec.CBOR.Term qualified as CBOR

import Control.DeepSeq
import GHC.Generics
import Ouroboros.Network.BlockFetch.ConsensusInterface
           (WhetherReceivingTentativeBlocks (..))
import Ouroboros.Network.CodecCBORTerm
import Ouroboros.Network.Handshake.Acceptable (Accept (..), Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.Magic
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))

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
    NodeToNodeV_13
    -- ^ Changes:
    --
    -- * Removed PeerSharingPrivate constructor
    -- * Fixed Codec to disable PeerSharing with buggy versions 11 and 12.
    -- * Disable PeerSharing with InitiatorOnly nodes, since they do not run
    --   peer sharing server side and can not reply to requests.
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable, Generic, NFData)

nodeToNodeVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToNodeVersion
nodeToNodeVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm NodeToNodeV_13 = CBOR.TInt 13

    decodeTerm (CBOR.TInt 13) = Right NodeToNodeV_13
    decodeTerm (CBOR.TInt n) = Left ( T.pack "decode NodeToNodeVersion: unknown tag: "
                                        <> T.pack (show n)
                                    , Just n
                                    )
    decodeTerm _ = Left ( T.pack "decode NodeToNodeVersion: unexpected term"
                        , Nothing)



-- | The flag which indicates whether the node runs only initiator or both
-- initiator or responder node.
--
-- This data structure has two proposes:
--
-- * instruct the diffusion layer if it should listen on incoming connections;
--
-- * it is communicated via 'NodeToNodeVersionData' during handshake
--   negotiation. In non-p2p mode we always send 'InitiatorOnlyDiffusionMode',
--   in p2p mode we send exactly what the diffusion is given.  In non-p2p mode
--   every connection outbound port is ephemeral, the remote side cannot connect
--   to it, however in p2p mode the outbound port is actually the port on which
--   the node is listening (if it runs in 'InitiatorAndResponderDiffusionMode').
--
data DiffusionMode
    = InitiatorOnlyDiffusionMode
    | InitiatorAndResponderDiffusionMode
  deriving (Typeable, Eq, Ord, Show)


-- | Version data for NodeToNode protocol
--
data NodeToNodeVersionData = NodeToNodeVersionData
  { networkMagic  :: !NetworkMagic
  , diffusionMode :: !DiffusionMode
  , peerSharing   :: !PeerSharing
  , query         :: !Bool
  }
  deriving (Show, Typeable, Eq)
  -- 'Eq' instance is not provided, it is not what we need in version
  -- negotiation (see 'Acceptable' instance below).

instance Acceptable NodeToNodeVersionData where
    -- | Check that both side use the same 'networkMagic'.  Choose smaller one
    -- from both 'diffusionMode's, e.g. if one is running in 'InitiatorOnlyMode'
    -- agree on it. Agree on the same 'PeerSharing' value, if the negotiated
    -- diffusion mode is 'InitiatorAndResponder', otherwise default to
    -- 'PeerSharingDisabled'.
    acceptableVersion local remote
      | networkMagic local == networkMagic remote
      = let acceptedDiffusionMode = diffusionMode local `min` diffusionMode remote
         in Accept NodeToNodeVersionData
              { networkMagic  = networkMagic local
              , diffusionMode = acceptedDiffusionMode
              , peerSharing   = case acceptedDiffusionMode of
                                  InitiatorAndResponderDiffusionMode ->
                                    peerSharing local <> peerSharing remote
                                  InitiatorOnlyDiffusionMode         ->
                                    PeerSharingDisabled
              , query         = query local || query remote
              }
      | otherwise
      = Refuse $ T.pack $ "version data mismatch: "
                       ++ show local
                       ++ " /= " ++ show remote

instance Queryable NodeToNodeVersionData where
    queryVersion = query

nodeToNodeCodecCBORTerm :: NodeToNodeVersion -> CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm =
  \case
    NodeToNodeV_13 -> v13

  where
    v13 = CodecCBORTerm { encodeTerm = encodeTerm13, decodeTerm = decodeTerm13 }

    encodeTerm13 :: NodeToNodeVersionData -> CBOR.Term
    encodeTerm13 NodeToNodeVersionData { networkMagic, diffusionMode, peerSharing, query }
      = CBOR.TList
          [ CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)
          , CBOR.TBool (case diffusionMode of
                         InitiatorOnlyDiffusionMode         -> True
                         InitiatorAndResponderDiffusionMode -> False)
          , CBOR.TInt (case peerSharing of
                         PeerSharingDisabled -> 0
                         PeerSharingEnabled  -> 1)
          , CBOR.TBool query
          ]

    decodeTerm13 :: CBOR.Term -> Either Text NodeToNodeVersionData
    decodeTerm13 (CBOR.TList [CBOR.TInt x, CBOR.TBool diffusionMode, CBOR.TInt peerSharing, CBOR.TBool query])
      | x >= 0
      , x <= 0xffffffff
      , Just ps <- case peerSharing of
                    0 -> Just PeerSharingDisabled
                    1 -> Just PeerSharingEnabled
                    _ -> Nothing
      = Right
          NodeToNodeVersionData {
              networkMagic = NetworkMagic (fromIntegral x),
              diffusionMode = if diffusionMode
                              then InitiatorOnlyDiffusionMode
                              else InitiatorAndResponderDiffusionMode,
              peerSharing = ps,
              query = query
            }
      | x < 0 || x > 0xffffffff
      = Left $ T.pack $ "networkMagic out of bound: " <> show x
      | otherwise -- peerSharing < 0 || peerSharing > 1
      = Left $ T.pack $ "peerSharing is out of bound: " <> show peerSharing
    decodeTerm13 t
      = Left $ T.pack $ "unknown encoding: " ++ show t


data ConnectionMode = UnidirectionalMode | DuplexMode
