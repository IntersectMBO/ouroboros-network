{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.NodeToNode.Version
  ( NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , DiffusionMode (..)
  , ConnectionMode (..)
  , nodeToNodeVersionCodec
  , nodeToNodeCodecCBORTerm
    -- * Feature checks
  , isPipeliningEnabled
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import qualified Codec.CBOR.Term as CBOR

import           Ouroboros.Network.BlockFetch.ClientState
                     (WhetherReceivingTentativeBlocks (..))
import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Magic
import           Ouroboros.Network.Protocol.Handshake.Version (Accept (..),
                     Acceptable (..))


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
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable)

nodeToNodeVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToNodeVersion
nodeToNodeVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm NodeToNodeV_7  = CBOR.TInt 7
    encodeTerm NodeToNodeV_8  = CBOR.TInt 8
    encodeTerm NodeToNodeV_9  = CBOR.TInt 9
    encodeTerm NodeToNodeV_10 = CBOR.TInt 10

    decodeTerm (CBOR.TInt 7) = Right NodeToNodeV_7
    decodeTerm (CBOR.TInt 8) = Right NodeToNodeV_8
    decodeTerm (CBOR.TInt 9) = Right NodeToNodeV_9
    decodeTerm (CBOR.TInt 10) = Right NodeToNodeV_10
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
  deriving (Show, Typeable, Eq)
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
nodeToNodeCodecCBORTerm _version
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

-- | Check whether a version enabling diffusion pipelining has been
-- negotiated.
isPipeliningEnabled :: NodeToNodeVersion -> WhetherReceivingTentativeBlocks
isPipeliningEnabled v
  | v >= NodeToNodeV_8 = ReceivingTentativeBlocks
  | otherwise          = NotReceivingTentativeBlocks
