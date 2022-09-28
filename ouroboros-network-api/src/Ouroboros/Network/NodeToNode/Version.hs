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

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import qualified Codec.CBOR.Term as CBOR

import           Ouroboros.Network.BlockFetch.ConsensusInterface
                     (WhetherReceivingTentativeBlocks (..))
import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Handshake.Acceptable (Accept (..),
                     Acceptable (..))
import           Ouroboros.Network.Magic
import           Ouroboros.Network.PeerSelection.PeerSharing.Type
                     (PeerSharing (..))


-- | Enumeration of node to node protocol versions.
--
data NodeToNodeVersion
    = NodeToNodeV_7
    -- ^ Changes:
    --
    -- * new 'KeepAlive' codec
    -- * Enable @CardanoNodeToNodeVersion5@, i.e., Alonzo
    | NodeToNodeV_8
    -- ^ Changes:
    --
    -- * Enable block diffusion pipelining in ChainSync and BlockFetch logic.
    | NodeToNodeV_9
    -- ^ Changes:
    --
    -- * Enable @CardanoNodeToNodeVersion6@, i.e., Babbage
    | NodeToNodeV_10
    -- ^ Changes:
    --
    -- * Enable full duplex connections.
    | NodeToNodeV_11
    -- ^ Changes:
    --
    -- * Enable @CardanoNodeToNodeVersion7@, i.e., Conway
    -- * Adds a new extra parameter to handshake: PeerSharing
    --   This version is needed to support the new  Peer Sharing miniprotocol
    --   older versions that are negotiated will appear as not participating
    --   in Peer Sharing to newer versions.
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable)

nodeToNodeVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToNodeVersion
nodeToNodeVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm NodeToNodeV_7  = CBOR.TInt 7
    encodeTerm NodeToNodeV_8  = CBOR.TInt 8
    encodeTerm NodeToNodeV_9  = CBOR.TInt 9
    encodeTerm NodeToNodeV_10 = CBOR.TInt 10
    encodeTerm NodeToNodeV_11 = CBOR.TInt 11

    decodeTerm (CBOR.TInt 7) = Right NodeToNodeV_7
    decodeTerm (CBOR.TInt 8) = Right NodeToNodeV_8
    decodeTerm (CBOR.TInt 9) = Right NodeToNodeV_9
    decodeTerm (CBOR.TInt 10) = Right NodeToNodeV_10
    decodeTerm (CBOR.TInt 11) = Right NodeToNodeV_11
    decodeTerm (CBOR.TInt n) = Left ( T.pack "decode NodeToNodeVersion: unknonw tag: "
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
  }
  deriving (Show, Typeable, Eq)
  -- 'Eq' instance is not provided, it is not what we need in version
  -- negotiation (see 'Acceptable' instance below).

instance Acceptable NodeToNodeVersionData where
    -- | Check that both side use the same 'networkMagic'.  Choose smaller one
    -- from both 'diffusionMode's, e.g. if one is running in 'InitiatorOnlyMode'
    -- agree on it. Keep the remote's PeerSharing information PeerSharing
    -- information.
    acceptableVersion local remote
      | networkMagic local == networkMagic remote
      = Accept NodeToNodeVersionData
          { networkMagic  = networkMagic local
          , diffusionMode = diffusionMode local `min` diffusionMode remote
          , peerSharing   = peerSharing remote
          }
      | otherwise
      = Refuse $ T.pack $ "version data mismatch: "
                       ++ show local
                       ++ " /= " ++ show remote


nodeToNodeCodecCBORTerm :: NodeToNodeVersion -> CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm version
  | version >= NodeToNodeV_11 =
    let encodeTerm :: NodeToNodeVersionData -> CBOR.Term
        encodeTerm NodeToNodeVersionData { networkMagic, diffusionMode, peerSharing }
          = CBOR.TList $
              [ CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)
              , CBOR.TBool (case diffusionMode of
                             InitiatorOnlyDiffusionMode         -> True
                             InitiatorAndResponderDiffusionMode -> False)
              , CBOR.TInt (case peerSharing of
                             NoPeerSharing      -> 0
                             PeerSharingPrivate -> 1
                             PeerSharingPublic  -> 2)
              ]

        decodeTerm :: NodeToNodeVersion -> CBOR.Term -> Either Text NodeToNodeVersionData
        decodeTerm _ (CBOR.TList [CBOR.TInt x, CBOR.TBool diffusionMode, CBOR.TInt peerSharing])
          | x >= 0
          , x <= 0xffffffff
          , peerSharing >= 0
          , peerSharing <= 2
          = Right
              NodeToNodeVersionData {
                  networkMagic = NetworkMagic (fromIntegral x),
                  diffusionMode = if diffusionMode
                                  then InitiatorOnlyDiffusionMode
                                  else InitiatorAndResponderDiffusionMode,
                  peerSharing = case peerSharing of
                                  0 -> NoPeerSharing
                                  1 -> PeerSharingPrivate
                                  2 -> PeerSharingPublic
                                  _ -> error "decodeTerm: impossible happened!"
                }
          | x < 0 || x > 0xffffffff
          = Left $ T.pack $ "networkMagic out of bound: " <> show x
          | otherwise -- peerSharing < 0 || peerSharing > 2
          = Left $ T.pack $ "peerSharing out of bound: " <> show peerSharing
        decodeTerm _ t
          = Left $ T.pack $ "unknown encoding: " ++ show t
     in CodecCBORTerm {encodeTerm, decodeTerm = decodeTerm version }
  | otherwise =
    let encodeTerm :: NodeToNodeVersionData -> CBOR.Term
        encodeTerm NodeToNodeVersionData { networkMagic, diffusionMode }
          = CBOR.TList $
                  [ CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)
                  , CBOR.TBool (case diffusionMode of
                                 InitiatorOnlyDiffusionMode         -> True
                                 InitiatorAndResponderDiffusionMode -> False)
                  ]

        decodeTerm :: NodeToNodeVersion -> CBOR.Term -> Either Text NodeToNodeVersionData
        decodeTerm _ (CBOR.TList [CBOR.TInt x, CBOR.TBool diffusionMode])
          | x >= 0
          , x <= 0xffffffff
          = Right
              NodeToNodeVersionData {
                  networkMagic = NetworkMagic (fromIntegral x),
                  diffusionMode = if diffusionMode
                                  then InitiatorOnlyDiffusionMode
                                  else InitiatorAndResponderDiffusionMode,
                  -- By default older versions do not participate in Peer
                  -- Sharing, since they do not support the new miniprotocol
                  peerSharing = NoPeerSharing
                }
          | otherwise
          = Left $ T.pack $ "networkMagic out of bound: " <> show x
        decodeTerm _ t
          = Left $ T.pack $ "unknown encoding: " ++ show t
     in CodecCBORTerm {encodeTerm, decodeTerm = decodeTerm version }


data ConnectionMode = UnidirectionalMode | DuplexMode


-- | Check whether a version enabling diffusion pipelining has been
-- negotiated.
--
-- TODO: this ought to be defined in `ouroboros-consensus` or
-- `ouroboros-consensus-diffusion`
isPipeliningEnabled :: NodeToNodeVersion -> WhetherReceivingTentativeBlocks
isPipeliningEnabled v
  | v >= NodeToNodeV_8 = ReceivingTentativeBlocks
  | otherwise          = NotReceivingTentativeBlocks
