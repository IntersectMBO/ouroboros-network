-- consensus uses byron types
-- type-instantiations for codecs

module Cardano.Client.API
(
-- Ouroboros.Network.Point
      WithOrigin (..)
    , blockPointHash
    , blockPointSlot
    , block
    , fromWithOrigin
-- Ouroboros.Network.Block
    , Point (..)
    , pointSlot
    , SlotNo (..)
--     , Tip (Tip, tipPoint, tipBlockNo)
    , BlockNo(unBlockNo, BlockNo)
    , blockPoint
    , decodePoint
    , encodePoint
    , genesisPoint
--    , genesisBlockNo
    , encodeTip
    , decodeTip
    , ChainHash (..)
--    , genesisSlotNo
  -- Ouroboros.Network.Mux
    , AppType (..)
    , OuroborosApplication (..)
-- Ouroboros.Network.NodeToClient
    , ErrorPolicyTrace (..)
--    , IPSubscriptionTarget (..)
    , LocalAddresses (..)
    , NodeToClientProtocols (..)
--    , NetworkIPSubscriptionTracers (..)
    , NodeToClientVersionData (..)
--    , SubscriptionParams (..)
    , WithAddr (..)
    , ncSubscriptionWorker_V1
    , networkErrorPolicies
    , newNetworkMutableState
    , nodeToClientCodecCBORTerm
-- Ouroboros.Network.Protocol.ChainSync.ClientPipelined
--    , ChainSyncClientPipelined (..)
--  , ClientPipelinedStIdle (..)
--   , ClientPipelinedStIdle  ( SendMsgFindIntersect
--                           , SendMsgRequestNext
--                           , SendMsgRequestNextPipelined
--                           , CollectResponse)
    , ClientStNext (..)
--    , chainSyncClientPeerPipelined -- cardano-explorer
--    , ClientPipelinedStIntersect (..)
-- Ouroboros.Network.Protocol.ChainSync.PipelineDecision
    , pipelineDecisionLowHighMark
    , PipelineDecision (..)
    , runPipelineDecision
    , MkPipelineDecision
-- Ouroboros.Network.Protocol.ChainSync.Codec
    , codecChainSync
-- Ouroboros.Network.Protocol.ChainSync.Type
    , ChainSync
-- Ouroboros.Network.Protocol.LocalTxSubmission.Client
    , LocalTxSubmissionClient (..)
    , LocalTxClientStIdle (SendMsgSubmitTx)
    , localTxSubmissionClientPeer
-- Ouroboros.Network.Protocol.LocalTxSubmission.Codec
    , codecLocalTxSubmission
-- Ouroboros.Network.Protocol.LocalTxSubmission.Type
    , LocalTxSubmission
-- Network.TypedProtocol.Codec
    , Codec
-- Network.TypedProtocol.Codec.Cbor
--    , DeserialiseFailure
-- Network.TypedProtocol.Driver
    , runPeer
    , runPipelinedPeer
-- Network.TypedProtocol.Pipelined
    , Nat(Zero, Succ)
-- Ouroboros.Consensus.Ledger.Byron
    , ByronBlock (ByronBlock, byronBlockRaw, byronBlockSlotNo, byronBlockHash)
    , ByronHash (..)
    , GenTx
-- Ouroboros.Consensus.Ledger.Abstract
    , BlockProtocol
-- Ouroboros.Consensus.Node.ProtocolInfo
    , pInfoConfig
    , protocolInfo
-- Ouroboros.Consensus.Node.Run.Abstract
    , RunNode
    , nodeDecodeBlock
    , nodeDecodeGenTx
    , nodeDecodeHeaderHash
    , nodeEncodeBlock
    , nodeEncodeGenTx
    , nodeEncodeHeaderHash
    , nodeNetworkMagic
-- Ouroboros.Consensus.Node.ErrorPolicy
    , consensusErrorPolicy
-- Ouroboros.Consensus.Protocol
    , NodeConfig
    , Protocol (..)

-- wallet extra imports
-- Codec.SerialiseTerm
    , CodecCBORTerm
-- Ouroboros.Network.Magic
    , NetworkMagic (..)
-- import qualified Ouroboros.Network.Block as O
-- qualified Ouroboros.Network.Point as Point
-- Ouroboros.Network.ChainFragment
    , HasHeader (..) -- blockNo

-- import Network.TypedProtocol.Channel
    , Channel
-- import Network.TypedProtocol.Driver
    , TraceSendRecv
-- import Ouroboros.Consensus.Ledger.Byron
--    , ByronBlock (..)
    , decodeByronBlock
    , decodeByronGenTx
    , decodeByronHeaderHash
    , encodeByronBlock
    , encodeByronGenTx
    , encodeByronHeaderHash
-- import Ouroboros.Network.NodeToClient
    , ConnectionId (..)
    , NetworkConnectTracers (..)
    , NodeToClientVersion (..)
    , connectTo
    , localTxSubmissionClientNull
-- import Ouroboros.Network.Protocol.ChainSync.Client
    , ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , chainSyncClientPeer
-- import Ouroboros.Network.Protocol.Handshake.Version
    , DictVersion (..)
    , simpleSingletonVersions
-- import Network.Mux.Interface
-- import Network.Mux.Types
--    , MuxError
    )

where
import Ouroboros.Network.Point
    ( WithOrigin (..)
    , blockPointHash
    , blockPointSlot
    , block
    , fromWithOrigin
    )
import Ouroboros.Network.Block
    ( Point (..)
    , SlotNo (..)
--    , Tip (Tip
--          , tipBlockNo
--          , tipPoint
--          )
    , decodePoint
    , encodePoint
    , genesisPoint
--    , genesisBlockNo
    , blockNo
    , ChainHash (..)
--    , genesisSlotNo
    , BlockNo(unBlockNo, BlockNo)
    , encodeTip
    , decodeTip
    )
import Ouroboros.Network.Mux
    ( AppType (..)
    , OuroborosApplication (..)
    )
import Ouroboros.Network.NodeToClient
    ( ErrorPolicyTrace (..)
--    , IPSubscriptionTarget (..)
    , LocalAddresses (..)
    , NodeToClientProtocols (..)
--    , NetworkIPSubscriptionTracers (..)
    , NodeToClientVersionData (..)
--    , SubscriptionParams (..)
    , WithAddr (..)
    , ncSubscriptionWorker_V1
    , networkErrorPolicies
    , newNetworkMutableState
    , nodeToClientCodecCBORTerm
    )

{-
- used by cardano explorer:
-- import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientPipelinedStIntersect (..)
    , chainSyncClientPeerPipelined
    )

import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientPipelinedStIntersect (..)
    , chainSyncClientPeerPipelined
    , recvMsgIntersectFound
    , recvMsgIntersectNotFound
    , recvMsgRollBackward
    , recvMsgRollForward
    )
-}

import Ouroboros.Network.Protocol.ChainSync.PipelineDecision
    ( pipelineDecisionLowHighMark
    , PipelineDecision (..)
    , runPipelineDecision
    , MkPipelineDecision
    )

import Ouroboros.Network.Protocol.ChainSync.Codec
    ( codecChainSync )

import Ouroboros.Network.Protocol.ChainSync.Type
    ( ChainSync )

import  Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxSubmissionClient (..)
    , LocalTxClientStIdle (..)
    , localTxSubmissionClientPeer
    )

import Ouroboros.Network.Protocol.LocalTxSubmission.Codec
    ( codecLocalTxSubmission )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)
import Network.TypedProtocol.Codec
    ( Codec )
-- import Network.TypedProtocol.Codec.Cbor ( DeserialiseFailure )
import Network.TypedProtocol.Driver
    ( runPeer
    , runPipelinedPeer
    )

import Network.TypedProtocol.Pipelined
    ( Nat(Zero, Succ) )
import Ouroboros.Consensus.Ledger.Byron
    ( ByronBlock(..)
    , ByronHash (..)
    , GenTx
    )

import Ouroboros.Consensus.Ledger.Abstract
    ( BlockProtocol )
import Ouroboros.Consensus.Node.ProtocolInfo
    ( pInfoConfig, protocolInfo )
import Ouroboros.Consensus.Node.Run.Abstract
    ( RunNode
    , nodeDecodeBlock
    , nodeDecodeGenTx
    , nodeDecodeHeaderHash
    , nodeEncodeBlock
    , nodeEncodeGenTx
    , nodeEncodeHeaderHash
    , nodeNetworkMagic
    )
import Ouroboros.Consensus.Node.ErrorPolicy
    ( consensusErrorPolicy )
import Ouroboros.Consensus.Protocol
    ( NodeConfig
    , Protocol (..)
    )

-- wallet stuff:
import Codec.SerialiseTerm
    ( CodecCBORTerm )
import Ouroboros.Network.ChainFragment
    ( HasHeader (..) )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )

import Network.TypedProtocol.Channel
    ( Channel )
import Network.TypedProtocol.Driver
    ( TraceSendRecv )

import Ouroboros.Consensus.Ledger.Byron
    ( decodeByronBlock
    , decodeByronGenTx
    , decodeByronHeaderHash
    , encodeByronBlock
    , encodeByronGenTx
    , encodeByronHeaderHash
    )

import Ouroboros.Network.Block
    ( blockPoint
    , pointSlot
    )

import Ouroboros.Network.NodeToClient
    ( ConnectionId (..)
    , NetworkConnectTracers (..)
    , NodeToClientVersion (..)
    , connectTo
    , localTxSubmissionClientNull
    )


import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    , chainSyncClientPeer
    )

import Ouroboros.Network.Protocol.Handshake.Version
    ( DictVersion (..)
    , simpleSingletonVersions
    )

--import Network.Mux.Types ( MuxError )
