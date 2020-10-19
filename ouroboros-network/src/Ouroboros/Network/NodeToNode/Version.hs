{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.NodeToNode.Version
  ( NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , DiffusionMode (..)
  , AgreedOptions (..)
  , ConnectionMode (..)
  , nodeToNodeVersionCodec
  , nodeToNodeCodecCBORTerm
  , nodeToNodeDictVersion
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import qualified Codec.CBOR.Term as CBOR

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Magic
import           Ouroboros.Network.Protocol.Handshake.Version
                  (Acceptable (..), Accept (..), DictVersion (..))


-- | Enumeration of node to node protocol versions.
--
data NodeToNodeVersion
    = NodeToNodeV_1
    | NodeToNodeV_2
    -- ^ Changes:
    --
    -- * Enable block size hints for Byron headers in ChainSync
    -- * Enable Keep-Alive miniprotocol
    -- * Enable @CardanoNodeToNodeVersion2@
    | NodeToNodeV_3
    -- ^ Changes:
    --
    -- * Enable KeepAlive miniprotocol
    | NodeToNodeV_4
    -- ^ Changes:
    --
    -- * Added 'DiffusionMode' Handshake argument.  Also from this version up
    -- the node will use duplex connections.
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable)

nodeToNodeVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToNodeVersion
nodeToNodeVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm NodeToNodeV_1  = CBOR.TInt 1
    encodeTerm NodeToNodeV_2  = CBOR.TInt 2
    encodeTerm NodeToNodeV_3  = CBOR.TInt 3
    encodeTerm NodeToNodeV_4  = CBOR.TInt 4

    decodeTerm (CBOR.TInt 1) = Right NodeToNodeV_1
    decodeTerm (CBOR.TInt 2) = Right NodeToNodeV_2
    decodeTerm (CBOR.TInt 3) = Right NodeToNodeV_3
    decodeTerm (CBOR.TInt 4) = Right NodeToNodeV_4
    decodeTerm (CBOR.TInt n) = Left ( T.pack "decode NodeToNodeVersion: unknonw tag: "
                                        <> T.pack (show n)
                                    , Just n
                                    )
    decodeTerm _ = Left ( T.pack "decode NodeToNodeVersion: unexpected term"
                        , Nothing)



-- | The flag which indicates wheather the node runs only initiator or both
-- initiator or responder node.   It does not however specify weather the node
-- is using duplex connections, this is implicit see 'NodeToNodeV_4'
--
data DiffusionMode
    = InitiatorOnlyDiffusionMode
    | InitiatorAndResponderDiffusionMode
  deriving (Typeable, Eq, Show)


-- | Version data for NodeToNode protocol
--
data NodeToNodeVersionData = NodeToNodeVersionData
  { networkMagic  :: !NetworkMagic
  , diffusionMode :: !DiffusionMode
  }
  deriving (Show, Typeable)
  -- 'Eq' instance is not provided, it is not what we need in version
  -- negotiation (see 'Acceptable' instance below).

instance Acceptable NodeToNodeVersionData where
    acceptableVersion local remote
      | networkMagic local == networkMagic remote && diffusionMode remote == InitiatorOnlyDiffusionMode
      = Accept remote
      | networkMagic local == networkMagic remote
      = Accept local
      | otherwise
      = Refuse $ T.pack $ "version data mismatch: "
                       ++ show local
                       ++ " /= " ++ show remote


nodeToNodeCodecCBORTerm :: NodeToNodeVersion -> CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm version
  | version <= NodeToNodeV_3
  = let encodeTerm :: NodeToNodeVersionData -> CBOR.Term
        encodeTerm NodeToNodeVersionData { networkMagic }
          = CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)

        decodeTerm :: CBOR.Term -> Either Text NodeToNodeVersionData
        decodeTerm (CBOR.TInt x)
          | x >= 0
          , x <= 0xffffffff
          = Right
              NodeToNodeVersionData {
                  networkMagic = NetworkMagic (fromIntegral x),
                  -- the default 'NodeMode' for version @≤ NodeToNodeV_3@
                  diffusionMode = InitiatorAndResponderDiffusionMode
                }
          | otherwise
          = Left $ T.pack $ "networkMagic out of bound: " <> show x
        decodeTerm t
          = Left $ T.pack $ "unknown encoding: " ++ show t
    in CodecCBORTerm {encodeTerm, decodeTerm}

  | otherwise -- NodeToNodeV_4 and beyond
  = let encodeTerm :: NodeToNodeVersionData -> CBOR.Term
        encodeTerm NodeToNodeVersionData { networkMagic, diffusionMode }
          = CBOR.TList
              [ CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)
              , CBOR.TBool (case diffusionMode of
                             InitiatorOnlyDiffusionMode         -> True
                             InitiatorAndResponderDiffusionMode -> False)
              ]

        decodeTerm :: CBOR.Term -> Either Text NodeToNodeVersionData
        decodeTerm (CBOR.TList [CBOR.TInt x, CBOR.TBool diffusionMode])
          | x >= 0
          , x <= 0xffffffff
          = Right
              NodeToNodeVersionData {
                  networkMagic = NetworkMagic (fromIntegral x),
                  diffusionMode = if diffusionMode
                                  then InitiatorOnlyDiffusionMode
                                  else InitiatorAndResponderDiffusionMode
                }
          | otherwise
          = Left $ T.pack $ "networkMagic out of bound: " <> show x
        decodeTerm t
          = Left $ T.pack $ "unknown encoding: " ++ show t
    in CodecCBORTerm {encodeTerm, decodeTerm}


data ConnectionMode = UnidirectionalMode | DuplexMode

data AgreedOptions = AgreedOptions {
    agreedConnectionMode :: ConnectionMode,
    agreedOptions        :: NodeToNodeVersionData
  }


mkAgreedOptions :: NodeToNodeVersion -> NodeToNodeVersionData -> AgreedOptions
mkAgreedOptions version agreedOptions
  | version <= NodeToNodeV_3
  = AgreedOptions { agreedConnectionMode = UnidirectionalMode, agreedOptions }
  | otherwise
  = AgreedOptions { agreedConnectionMode = DuplexMode, agreedOptions }



nodeToNodeDictVersion :: NodeToNodeVersion
                      -> DictVersion NodeToNodeVersion AgreedOptions NodeToNodeVersionData
nodeToNodeDictVersion version =
    DictVersion (nodeToNodeCodecCBORTerm version) mkAgreedOptions
