{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module DMQ.NodeToNode.Version
  ( NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , nodeToNodeCodecCBORTerm
  , nodeToNodeVersionCodec
  , ntnDataFlow
  ) where

import Codec.CBOR.Term qualified as CBOR
import Control.DeepSeq (NFData)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Ouroboros.Network.CodecCBORTerm (CodecCBORTerm (..))
import Ouroboros.Network.ConnectionManager.Types (DataFlow (..))
import Ouroboros.Network.DiffusionMode
import Ouroboros.Network.Handshake.Acceptable (Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.PeerSelection (PeerSharing (..))
import Ouroboros.Network.Protocol.Handshake (Accept (..))

import Ouroboros.Network.OrphanInstances ()


data NodeToNodeVersion =
  NodeToNodeV_1
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, NFData)

instance Aeson.ToJSON NodeToNodeVersion where
  toJSON NodeToNodeV_1 = Aeson.toJSON (1 :: Int)
instance Aeson.ToJSONKey NodeToNodeVersion where

nodeToNodeVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToNodeVersion
nodeToNodeVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm NodeToNodeV_1 = CBOR.TInt 1

    decodeTerm (CBOR.TInt 1) = Right NodeToNodeV_1
    decodeTerm (CBOR.TInt n) = Left ( T.pack "decode NodeToNodeVersion: unknown tag: "
                                        <> T.pack (show n)
                                    , Just n
                                    )
    decodeTerm _ = Left ( T.pack "decode NodeToNodeVersion: unexpected term"
                        , Nothing)

-- | Version data for NodeToNode protocols
--
-- This data type is inpired by the one defined in 'ouroboros-network-api',
-- however, it is redefined here to tie it to our custom `NodeToNodeVersion`
-- and to avoid divergences.
--
data NodeToNodeVersionData = NodeToNodeVersionData
  { networkMagic  :: !NetworkMagic
  , diffusionMode :: !DiffusionMode
  , peerSharing   :: !PeerSharing
  , query         :: !Bool
  }
  deriving (Show, Eq)

instance Aeson.ToJSON NodeToNodeVersionData where
  toJSON NodeToNodeVersionData {
      networkMagic,
      diffusionMode,
      peerSharing,
      query
    }
    =
    Aeson.object [ "NetworkMagic" Aeson..= unNetworkMagic networkMagic
                 , "DiffusionMode" Aeson..= diffusionMode
                 , "PeerSharing" Aeson..= peerSharing
                 , "Query" Aeson..= query
                 ]


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

nodeToNodeCodecCBORTerm :: NodeToNodeVersion
                        -> CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm =
  \case
    NodeToNodeV_1 -> v1

  where
    v1 = CodecCBORTerm { encodeTerm = encodeTerm1, decodeTerm = decodeTerm1 }

    encodeTerm1 :: NodeToNodeVersionData -> CBOR.Term
    encodeTerm1 NodeToNodeVersionData { networkMagic, diffusionMode, peerSharing, query }
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

    decodeTerm1 :: CBOR.Term -> Either Text NodeToNodeVersionData
    decodeTerm1 (CBOR.TList [CBOR.TInt x, CBOR.TBool diffusionMode, CBOR.TInt peerSharing, CBOR.TBool query])
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
    decodeTerm1 t
      = Left $ T.pack $ "unknown encoding: " ++ show t

ntnDataFlow :: NodeToNodeVersionData -> DataFlow
ntnDataFlow NodeToNodeVersionData { diffusionMode } =
  case diffusionMode of
    InitiatorAndResponderDiffusionMode -> Duplex
    InitiatorOnlyDiffusionMode         -> Unidirectional
