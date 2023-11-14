{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Main where

import Data.Map.Strict qualified as Map

import Codec.CBOR.Read (DeserialiseFailure)
import Codec.CBOR.Term qualified as CBOR
import Control.DeepSeq
import Data.ByteString qualified as BS
import Data.ByteString.Lazy hiding (concat, replicate, span)
import Data.List.NonEmpty qualified as NonEmpty
import Network.Socket (SockAddr (..), tupleToHostAddress)
import Network.TypedProtocol.Codec
import Ouroboros.Network.Block (SlotNo)
import Ouroboros.Network.NodeToClient.Version
import Ouroboros.Network.NodeToNode.Version
import Ouroboros.Network.Protocol.BlockFetch.Codecs
import Ouroboros.Network.Protocol.BlockFetch.Type
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch
import Ouroboros.Network.Protocol.ChainSync.Codecs
import Ouroboros.Network.Protocol.ChainSync.Type
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.KeepAlive.Codec
import Ouroboros.Network.Protocol.KeepAlive.Type
import Ouroboros.Network.Protocol.KeepAlive.Type qualified as KeepAlive
import Ouroboros.Network.Protocol.LocalStateQuery.Codecs
import Ouroboros.Network.Protocol.LocalStateQuery.Test
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as LocalStateQuery
import Ouroboros.Network.Protocol.LocalTxMonitor.Codecs
import Ouroboros.Network.Protocol.LocalTxMonitor.Type
import Ouroboros.Network.Protocol.LocalTxMonitor.Type qualified as LocalTxMonitor
import Ouroboros.Network.Protocol.LocalTxSubmission.Codecs
import Ouroboros.Network.Protocol.LocalTxSubmission.Test qualified as LocalTxSubmission
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
import Ouroboros.Network.Protocol.LocalTxSubmission.Type qualified as LocalTxSubmission
import Ouroboros.Network.Protocol.PeerSharing.Codecs
import Ouroboros.Network.Protocol.PeerSharing.Type
import Ouroboros.Network.Protocol.PeerSharing.Type qualified as PeerSharing
import Ouroboros.Network.Protocol.TxSubmission2.Codecs
import Ouroboros.Network.Protocol.TxSubmission2.Test
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as TxSubmission2
import Test.Data.CDDL
import Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bgroup "CBOR Codec Benchmarks"
    [ bgroup "NodeToNode Handshake Codec Encode" $
        let printMsg :: AnyMessageAndAgency (Handshake NodeToNodeVersion CBOR.Term)
                     -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (MsgProposeVersions _) -> show tok ++ " MsgProposeVersions"
               AnyMessageAndAgency tok (MsgReplyVersions _)   -> show tok ++ " MsgReplyVersions"
               AnyMessageAndAgency tok (MsgQueryReply _)      -> show tok ++ " MsgQueryVersions"
               AnyMessageAndAgency tok (MsgAcceptVersion _ _) -> show tok ++ " MsgAcceptVersion"
               AnyMessageAndAgency tok (MsgRefuse _)          -> show tok ++ " MsgRefuse"
         in concat
            [ myBenchCodec ("NodeToNode Handshake " ++ printMsg msg)
                           (const nodeToNodeHandshakeCodec) maxBound msg
            | msg <- handshakeMessages
            ]
    , bgroup "NodeToClient Handshake Codec Encode" $
        let printMsg :: AnyMessageAndAgency (Handshake NodeToClientVersion CBOR.Term)
                     -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (MsgProposeVersions _) -> show tok ++ " MsgProposeVersions"
               AnyMessageAndAgency tok (MsgReplyVersions _)   -> show tok ++ " MsgReplyVersions"
               AnyMessageAndAgency tok (MsgQueryReply _)      -> show tok ++ " MsgQueryVersions"
               AnyMessageAndAgency tok (MsgAcceptVersion _ _) -> show tok ++ " MsgAcceptVersion"
               AnyMessageAndAgency tok (MsgRefuse _)          -> show tok ++ " MsgRefuse"
         in concat
            [ myBenchCodec ("NodeToClient Handshake " ++ printMsg msg)
                           (const nodeToClientHandshakeCodec) maxBound msg
            | msg <- handshakeMessages
            ]
    , bgroup "ChainSync Codec Encode" $
        let printMsg :: AnyMessageAndAgency (ChainSync BlockHeader HeaderPoint HeaderTip)
                     -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (MsgRollForward _ _)     -> show tok ++ " MsgRollForward"
               AnyMessageAndAgency tok (MsgRollBackward _ _)    -> show tok ++ " MsgRollBackward"
               AnyMessageAndAgency tok (MsgFindIntersect _)     -> show tok ++ " MsgFindIntersect"
               AnyMessageAndAgency tok (MsgIntersectFound _ _)  -> show tok ++ " MsgIntersectFound"
               AnyMessageAndAgency tok (MsgIntersectNotFound _) -> show tok ++ " MsgIntersectNotFound"
               AnyMessageAndAgency tok message                  -> show tok ++ " " ++ show message
         in concat
            [ myBenchCodec ("ChainSync " ++ printMsg msg)
                           (const chainSyncCodec) maxBound msg
            | msg <- chainSyncMessages
            ]
    , bgroup "BlockFetch Codec" $
        let printMsg :: AnyMessageAndAgency (BlockFetch Block BlockPoint) -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (MsgRequestRange _) -> show tok ++ " MsgRequestRange"
               AnyMessageAndAgency tok (MsgBlock _)        -> show tok ++ " MsgBlock"
               AnyMessageAndAgency tok message             -> show tok ++ " " ++ show message
         in concat
            [ myBenchCodec ("BlockFetch " ++ printMsg msg)
                           (const blockFetchCodec) maxBound msg
            | msg <- blockFetchMessages
            ]
    , bgroup "TxSumission2 Codec" $
        let printMsg :: AnyMessageAndAgency (TxSubmission2 TxId Tx) -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (MsgReplyTxIds _) -> show tok ++ " MsgReplyTxIds"
               AnyMessageAndAgency tok (MsgRequestTxs _) -> show tok ++ " MsgRequestTxs"
               AnyMessageAndAgency tok (MsgReplyTxs _)   -> show tok ++ " MsgReplyTxs"
               AnyMessageAndAgency tok message           -> show tok ++ " " ++ show message
         in concat
            [ myBenchCodec ("TxSumission2 " ++ printMsg msg)
                           (const txSubmissionCodec2) maxBound msg
            | msg <- txSubmission2Messages
            ]
    , bgroup "KeepAlive Codec" $
        let printMsg :: AnyMessageAndAgency KeepAlive -> String
            printMsg msg = case msg of
              AnyMessageAndAgency tok message -> show tok ++ " " ++ show message
         in concat
            [ myBenchCodec ("KeepAlive " ++ printMsg msg)
                           (const codecKeepAlive_v2) maxBound msg
            | msg <- keepAliveMessages
            ]
    , bgroup "LocalTxSubmission Codec" $
        let printMsg :: AnyMessageAndAgency (LocalTxSubmission LocalTxSubmission.Tx LocalTxSubmission.Reject)
                     -> String
            printMsg msg = case msg of
              AnyMessageAndAgency tok (MsgSubmitTx _) -> show tok ++ " MsgSubmitTx"
              AnyMessageAndAgency tok (MsgRejectTx _) -> show tok ++ " MsgRejectTx"
              AnyMessageAndAgency tok message         -> show tok ++ " " ++ show message
         in concat
            [ myBenchCodec ("LocalTxSubmission " ++ printMsg msg)
                           (const localTxSubmissionCodec) maxBound msg
            | msg <- localTxSubmissionMessages
            ]
    , bgroup "LocalTxMonitor Codec" $
        let printMsg :: AnyMessageAndAgency (LocalTxMonitor TxId Tx SlotNo) -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (LocalTxMonitor.MsgAcquired _)      -> show tok ++ " MsgAcquired"
               AnyMessageAndAgency tok (LocalTxMonitor.MsgReplyNextTx _)   -> show tok ++ " MsgReplyNextTx"
               AnyMessageAndAgency tok (LocalTxMonitor.MsgHasTx _)         -> show tok ++ " MsgHasTx"
               AnyMessageAndAgency tok (LocalTxMonitor.MsgReplyHasTx _)    -> show tok ++ " MsgReplyHasTx"
               AnyMessageAndAgency tok (LocalTxMonitor.MsgReplyGetSizes _) -> show tok ++ " MsgReplyGetSizes"
               AnyMessageAndAgency tok message                             -> show tok ++ " " ++ show message
         in concat
            [ myBenchCodec ("LocalTxSubmission " ++ printMsg msg)
                           (const localTxMonitorCodec) maxBound msg
            | msg <- localTxMonitorMessages
            ]
    , bgroup "LocalStateQuery Codec" $
        let printMsg :: AnyMessageAndAgency (LocalStateQuery Block BlockPoint Query)
                     -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (LocalStateQuery.MsgAcquire _) -> show tok ++ " MsgAcquire"
               AnyMessageAndAgency tok (MsgQuery _)                   -> show tok ++ " MsgQuery"
               AnyMessageAndAgency (ServerAgency (TokQuerying (Query _)))
                                   (MsgResult _ _) -> "ServerAgency TokQuerying Query MsgResult"
               AnyMessageAndAgency tok (MsgReAcquire _)               -> show tok ++ " MsgReAcquire"
               AnyMessageAndAgency tok message                        -> show tok ++ " " ++ show message
         in concat
              [ myBenchCodec ("LocalStateQuery " ++ printMsg msg)
                             (const localStateQueryCodec) maxBound msg
              | msg <- getAnyMessageAndAgencyWithResult <$> localStateQueryMessages
              ]
    , bgroup "PeerSharing Codec" $
        let printMsg :: AnyMessageAndAgency (PeerSharing SockAddr) -> String
            printMsg msg = case msg of
              AnyMessageAndAgency tok (MsgSharePeers _) -> show tok ++ " MsgSharePeers"
              AnyMessageAndAgency tok message           -> show tok ++ " " ++ show message
         in concat
              [ myBenchCodec ("PeerSharing " ++ printMsg msg ++ " " ++ show ntnVersion)
                             peerSharingCodec ntnVersion msg
              | msg <- peerSharingMessages
              , ntnVersion <- [minBound .. maxBound]
              ]
    ]
  ]

largeBS :: BS.ByteString
largeBS = BS.replicate 99999 maxBound

handshakeMessages :: ( Enum vNumber
                    , Bounded vNumber
                    , Ord vNumber
                     )
                  => [AnyMessageAndAgency (Handshake vNumber CBOR.Term)]
handshakeMessages =
  [ AnyMessageAndAgency (ClientAgency TokPropose)
                        (MsgProposeVersions
                          (Map.fromList [ (ntnVersion , CBOR.TBytes largeBS)
                                        | ntnVersion <- [minBound .. maxBound]
                                        ]))
  , AnyMessageAndAgency (ServerAgency TokConfirm)
                        (MsgReplyVersions
                          (Map.fromList [ (ntnVersion , CBOR.TBytes largeBS)
                                        | ntnVersion <- [minBound .. maxBound]
                                        ]))
  , AnyMessageAndAgency (ServerAgency TokConfirm)
                        (MsgQueryReply
                          (Map.fromList [ (ntnVersion , CBOR.TBytes largeBS)
                                        | ntnVersion <- [minBound .. maxBound]
                                        ]))
  , AnyMessageAndAgency (ServerAgency TokConfirm)
                        (MsgAcceptVersion maxBound (CBOR.TBytes largeBS))
  , AnyMessageAndAgency (ServerAgency TokConfirm)
                        (MsgRefuse (VersionMismatch [minBound..maxBound]
                                                    (replicate 99 maxBound)))
  ]

chainSyncMessages :: [AnyMessageAndAgency (ChainSync BlockHeader HeaderPoint HeaderTip)]
chainSyncMessages =
  [ AnyMessageAndAgency (ClientAgency ChainSync.TokIdle) MsgRequestNext
  , AnyMessageAndAgency (ServerAgency (TokNext TokCanAwait)) MsgAwaitReply
  , AnyMessageAndAgency (ServerAgency (TokNext TokCanAwait))
                        (MsgRollForward (BlockHeader largeCBORBS)
                                        (HeaderTip largeCBORBS))
  , AnyMessageAndAgency (ServerAgency (TokNext TokMustReply))
                        (MsgRollForward (BlockHeader largeCBORBS)
                                        (HeaderTip largeCBORBS))
  , AnyMessageAndAgency (ServerAgency (TokNext TokCanAwait))
                        (MsgRollBackward (HeaderPoint largeCBORBS)
                                         (HeaderTip largeCBORBS))
  , AnyMessageAndAgency (ServerAgency (TokNext TokMustReply))
                        (MsgRollBackward (HeaderPoint largeCBORBS)
                                         (HeaderTip largeCBORBS))
  , AnyMessageAndAgency (ClientAgency ChainSync.TokIdle)
                        (MsgFindIntersect [HeaderPoint largeCBORBS])
  , AnyMessageAndAgency (ServerAgency TokIntersect)
                        (MsgIntersectFound (HeaderPoint largeCBORBS)
                                           (HeaderTip largeCBORBS))
  , AnyMessageAndAgency (ServerAgency TokIntersect)
                        (MsgIntersectNotFound (HeaderTip largeCBORBS))
  , AnyMessageAndAgency (ClientAgency ChainSync.TokIdle) ChainSync.MsgDone
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)

blockFetchMessages :: [AnyMessageAndAgency (BlockFetch Block BlockPoint)]
blockFetchMessages =
  [ AnyMessageAndAgency (ClientAgency BlockFetch.TokIdle)
                        (MsgRequestRange (ChainRange (BlockPoint largeCBORBS)
                                                     (BlockPoint largeCBORBS)))
  , AnyMessageAndAgency (ServerAgency BlockFetch.TokBusy) MsgStartBatch
  , AnyMessageAndAgency (ServerAgency BlockFetch.TokBusy) MsgNoBlocks
  , AnyMessageAndAgency (ServerAgency TokStreaming) (MsgBlock (Block largeCBORBS))
  , AnyMessageAndAgency (ServerAgency TokStreaming) MsgBatchDone
  , AnyMessageAndAgency (ClientAgency BlockFetch.TokIdle) MsgClientDone
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)

txSubmission2Messages :: [AnyMessageAndAgency (TxSubmission2 TxId Tx)]
txSubmission2Messages =
  [ AnyMessageAndAgency (ClientAgency TokInit) MsgInit
  , AnyMessageAndAgency (ServerAgency TxSubmission2.TokIdle)
                        (MsgRequestTxIds TokBlocking maxBound maxBound)
  , AnyMessageAndAgency (ServerAgency TxSubmission2.TokIdle)
                        (MsgRequestTxIds TokNonBlocking maxBound maxBound)
  , AnyMessageAndAgency (ClientAgency (TokTxIds TokBlocking))
                        (MsgReplyTxIds (BlockingReply nonEmpty))
  , AnyMessageAndAgency (ClientAgency (TokTxIds TokNonBlocking))
                        (MsgReplyTxIds (NonBlockingReply list))
  , AnyMessageAndAgency (ServerAgency TxSubmission2.TokIdle)
                        (MsgRequestTxs (replicate 9 largeTxId))
  , AnyMessageAndAgency (ClientAgency TokTxs)
                        (MsgReplyTxs (replicate 9 largeTx))
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)
    largeTxId = TxId largeCBORBS
    largeTx = Tx largeTxId
    txSize = fromIntegral $ BS.length largeBS
    list = replicate 99 (largeTxId, txSize)
    nonEmpty = NonEmpty.fromList list

keepAliveMessages :: [AnyMessageAndAgency KeepAlive]
keepAliveMessages =
  [ AnyMessageAndAgency (ClientAgency TokClient)
                        (MsgKeepAlive (Cookie maxBound))
  , AnyMessageAndAgency (ServerAgency TokServer)
                        (MsgKeepAliveResponse (Cookie maxBound))
  , AnyMessageAndAgency (ClientAgency TokClient) KeepAlive.MsgDone
  ]

localTxSubmissionMessages :: [AnyMessageAndAgency (LocalTxSubmission LocalTxSubmission.Tx LocalTxSubmission.Reject)]
localTxSubmissionMessages =
  [ AnyMessageAndAgency (ClientAgency LocalTxSubmission.TokIdle)
                        (MsgSubmitTx (LocalTxSubmission.Tx largeCBORBS))
  , AnyMessageAndAgency (ServerAgency LocalTxSubmission.TokBusy)
                        MsgAcceptTx
  , AnyMessageAndAgency (ServerAgency LocalTxSubmission.TokBusy)
                        (MsgRejectTx (LocalTxSubmission.Reject maxBound))
  , AnyMessageAndAgency (ClientAgency LocalTxSubmission.TokIdle)
                        LocalTxSubmission.MsgDone
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)

localTxMonitorMessages :: [AnyMessageAndAgency (LocalTxMonitor TxId Tx SlotNo)]
localTxMonitorMessages =
  [ AnyMessageAndAgency (ClientAgency LocalTxMonitor.TokIdle)
                        LocalTxMonitor.MsgAcquire
  , AnyMessageAndAgency (ServerAgency LocalTxMonitor.TokAcquiring)
                        (LocalTxMonitor.MsgAcquired maxBound)
  , AnyMessageAndAgency (ClientAgency LocalTxMonitor.TokAcquired) MsgAwaitAcquire
  , AnyMessageAndAgency (ClientAgency LocalTxMonitor.TokAcquired) MsgNextTx
  , AnyMessageAndAgency (ServerAgency (LocalTxMonitor.TokBusy TokNextTx))
                        (MsgReplyNextTx (Just largeTx))
  , AnyMessageAndAgency (ClientAgency LocalTxMonitor.TokAcquired)
                        (MsgHasTx largeTxId)
  , AnyMessageAndAgency (ServerAgency (LocalTxMonitor.TokBusy TokHasTx))
                        (MsgReplyHasTx maxBound)
  , AnyMessageAndAgency (ClientAgency LocalTxMonitor.TokAcquired) MsgGetSizes
  , AnyMessageAndAgency (ServerAgency (LocalTxMonitor.TokBusy TokGetSizes))
                        (MsgReplyGetSizes (MempoolSizeAndCapacity maxBound
                                                                  maxBound
                                                                  maxBound))
  , AnyMessageAndAgency (ClientAgency LocalTxMonitor.TokAcquired)
                        LocalTxMonitor.MsgRelease
  , AnyMessageAndAgency (ClientAgency LocalTxMonitor.TokIdle)
                        LocalTxMonitor.MsgDone
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)
    largeTxId = TxId largeCBORBS
    largeTx = Tx largeTxId

localStateQueryMessages :: [AnyMessageAndAgencyWithResult Block BlockPoint Query Result]
localStateQueryMessages =
  [ AnyMessageAndAgencyWithResult
      (AnyMessageAndAgency (ClientAgency LocalStateQuery.TokIdle)
                           (LocalStateQuery.MsgAcquire
                              (SpecificPoint (BlockPoint largeCBORBS))))
  , AnyMessageAndAgencyWithResult
      (AnyMessageAndAgency (ServerAgency LocalStateQuery.TokAcquiring)
                           LocalStateQuery.MsgAcquired)
  , AnyMessageAndAgencyWithResult
      (AnyMessageAndAgency (ServerAgency LocalStateQuery.TokAcquiring)
                           (LocalStateQuery.MsgFailure LocalStateQuery.AcquireFailurePointNotOnChain))
  , AnyMessageAndAgencyWithResult
      (AnyMessageAndAgency (ServerAgency LocalStateQuery.TokAcquiring)
                           (LocalStateQuery.MsgFailure LocalStateQuery.AcquireFailurePointTooOld))
  , AnyMessageAndAgencyWithResult
      (AnyMessageAndAgency (ClientAgency LocalStateQuery.TokAcquired)
                           (MsgQuery (Query largeCBORBS)))
  , AnyMessageAndAgencyWithResult
      (AnyMessageAndAgency (ServerAgency (TokQuerying (Query largeCBORBS)))
                           (MsgResult (Query largeCBORBS) (Result (Any CBOR.TNull))))
  , AnyMessageAndAgencyWithResult
      (AnyMessageAndAgency (ClientAgency LocalStateQuery.TokAcquired)
                           LocalStateQuery.MsgRelease)
  , AnyMessageAndAgencyWithResult
      (AnyMessageAndAgency (ClientAgency LocalStateQuery.TokAcquired)
                           (MsgReAcquire (SpecificPoint (BlockPoint largeCBORBS))))
  , AnyMessageAndAgencyWithResult
      (AnyMessageAndAgency (ClientAgency LocalStateQuery.TokIdle)
                           LocalStateQuery.MsgDone)
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)

peerSharingMessages :: [AnyMessageAndAgency (PeerSharing SockAddr)]
peerSharingMessages =
  [ AnyMessageAndAgency (ClientAgency PeerSharing.TokIdle)
                        (MsgShareRequest (PeerSharingAmount maxBound))
  , AnyMessageAndAgency (ClientAgency PeerSharing.TokIdle) PeerSharing.MsgDone
  , AnyMessageAndAgency (ServerAgency PeerSharing.TokBusy)
                        (MsgSharePeers [ SockAddrInet maxBound addr
                                       | x <- [0..maxBound]
                                       , let addr = tupleToHostAddress (192, 168, 1, x)
                                       ])
  ]

myBenchCodec :: ( forall (st :: ps) (st' :: ps). NFData (Message ps st st')
                , forall (st :: ps) pr. NFData (PeerHasAgency pr st)
                )
             => String
             -> (NodeToNodeVersion -> Codec ps DeserialiseFailure IO ByteString)
             -> NodeToNodeVersion
             -> AnyMessageAndAgency ps
             -> [Benchmark]
myBenchCodec title getCodec ntnVersion msg =
  [ myBenchEncode (title ++ " " ++ "Encode") getCodec ntnVersion msg
  , myBenchDecode (title ++ " " ++ "Decode") getCodec ntnVersion msg
  ]

-- TODO: We don't force the agency along with the message because this leads
-- to a weird error that could be a tasty-bench bug.
-- See: https://github.com/Bodigrim/tasty-bench/issues/52
--
-- Only LocalStateQuery agency tokens carry the query information so it
-- shouldn't be too problematic.
myBenchEncode :: ( forall (st :: ps) (st' :: ps). NFData (Message ps st st')
                 , forall (st :: ps) pr. NFData (PeerHasAgency pr st)
                 )
              => String
              -> (NodeToNodeVersion -> Codec ps DeserialiseFailure IO ByteString)
              -> NodeToNodeVersion
              -> AnyMessageAndAgency ps
              -> Benchmark
myBenchEncode title getCodec ntnVersion (AnyMessageAndAgency a m) =
  let Codec { encode } = getCodec ntnVersion
   in env (pure (a, m)) $ \ ~(agency, message) ->
        bench title $ nf (encode agency) message

-- TODO: We don't force the agency along with the message because this leads
-- to a weird error that could be a tasty-bench bug.
-- See: https://github.com/Bodigrim/tasty-bench/issues/52
--
-- Only LocalStateQuery agency tokens carry the query information so it
-- shouldn't be too problematic.
myBenchDecode :: ( forall (st :: ps) (st' :: ps). NFData (Message ps st st')
                 , forall (st :: ps) pr. NFData (PeerHasAgency pr st)
                 )
              => String
              -> (NodeToNodeVersion -> Codec ps DeserialiseFailure IO ByteString)
              -> NodeToNodeVersion
              -> AnyMessageAndAgency ps
              -> Benchmark
myBenchDecode title getCodec ntnVersion (AnyMessageAndAgency a m) =
  let Codec { encode
            , decode
            } = getCodec ntnVersion
   in env (pure (a, encode a m)) $ \ ~(agency, encodedMessage) ->
        bench title $ nfIO (do
                              decoder <- decode agency
                              res <- runDecoder [encodedMessage] decoder
                              return $ case res of
                                Left err -> Just err
                                Right {} -> Nothing
                           )
