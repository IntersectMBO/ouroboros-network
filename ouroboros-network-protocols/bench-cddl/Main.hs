{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Main where

import Control.DeepSeq

import Codec.CBOR.Read (DeserialiseFailure)
import Codec.CBOR.Term qualified as CBOR
import Data.ByteString qualified as BS
import Data.ByteString.Lazy hiding (concat, replicate, span)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map

import Network.Socket (SockAddr (..), tupleToHostAddress)
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Stateful.Codec qualified as Stateful

import Ouroboros.Network.Block (SlotNo)
import Ouroboros.Network.NodeToClient.Version
import Ouroboros.Network.NodeToNode.Version
import Ouroboros.Network.Protocol.BlockFetch.Codec.CDDL
import Ouroboros.Network.Protocol.BlockFetch.Type
import Ouroboros.Network.Protocol.ChainSync.Codec.CDDL
import Ouroboros.Network.Protocol.ChainSync.Type
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.KeepAlive.Codec
import Ouroboros.Network.Protocol.KeepAlive.Type
import Ouroboros.Network.Protocol.KeepAlive.Type qualified as KeepAlive
import Ouroboros.Network.Protocol.LocalStateQuery.Codec.CDDL
import Ouroboros.Network.Protocol.LocalStateQuery.Test hiding (Query)
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as LocalStateQuery
import Ouroboros.Network.Protocol.LocalTxMonitor.Codec.CDDL
import Ouroboros.Network.Protocol.LocalTxMonitor.Type
import Ouroboros.Network.Protocol.LocalTxMonitor.Type qualified as LocalTxMonitor
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec.CDDL
import Ouroboros.Network.Protocol.LocalTxSubmission.Test qualified as LocalTxSubmission
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
import Ouroboros.Network.Protocol.LocalTxSubmission.Type qualified as LocalTxSubmission
import Ouroboros.Network.Protocol.PeerSharing.Codec.CDDL
import Ouroboros.Network.Protocol.PeerSharing.Type
import Ouroboros.Network.Protocol.PeerSharing.Type qualified as PeerSharing
import Ouroboros.Network.Protocol.TxSubmission2.Codec.CDDL
import Ouroboros.Network.Protocol.TxSubmission2.Test
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Test.Data.CDDL
import Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bgroup "CBOR Codec Benchmarks"
    [ bgroup "NodeToNode Handshake Codec Encode" $
        let printMsg :: AnyMessage (Handshake NodeToNodeVersion CBOR.Term)
                     -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (MsgProposeVersions _) -> show tok ++ " MsgProposeVersions"
               AnyMessageAndAgency tok (MsgReplyVersions _)   -> show tok ++ " MsgReplyVersions"
               AnyMessageAndAgency tok (MsgQueryReply _)      -> show tok ++ " MsgQueryVersions"
               AnyMessageAndAgency tok (MsgAcceptVersion _ _) -> show tok ++ " MsgAcceptVersion"
               AnyMessageAndAgency tok (MsgRefuse _)          -> show tok ++ " MsgRefuse"
         in concat
            [ benchmarkCodec ("NodeToNode Handshake " ++ printMsg msg)
                             (const nodeToNodeHandshakeCodec) maxBound msg
            | msg <- handshakeMessages
            ]
    , bgroup "NodeToClient Handshake Codec Encode" $
        let printMsg :: AnyMessage (Handshake NodeToClientVersion CBOR.Term)
                     -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (MsgProposeVersions _) -> show tok ++ " MsgProposeVersions"
               AnyMessageAndAgency tok (MsgReplyVersions _)   -> show tok ++ " MsgReplyVersions"
               AnyMessageAndAgency tok (MsgQueryReply _)      -> show tok ++ " MsgQueryVersions"
               AnyMessageAndAgency tok (MsgAcceptVersion _ _) -> show tok ++ " MsgAcceptVersion"
               AnyMessageAndAgency tok (MsgRefuse _)          -> show tok ++ " MsgRefuse"
         in concat
            [ benchmarkCodec ("NodeToClient Handshake " ++ printMsg msg)
                             (const nodeToClientHandshakeCodec) maxBound msg
            | msg <- handshakeMessages
            ]
    , bgroup "ChainSync Codec Encode" $
        let printMsg :: AnyMessage (ChainSync BlockHeader HeaderPoint HeaderTip)
                     -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (MsgRollForward _ _)     -> show tok ++ " MsgRollForward"
               AnyMessageAndAgency tok (MsgRollBackward _ _)    -> show tok ++ " MsgRollBackward"
               AnyMessageAndAgency tok (MsgFindIntersect _)     -> show tok ++ " MsgFindIntersect"
               AnyMessageAndAgency tok (MsgIntersectFound _ _)  -> show tok ++ " MsgIntersectFound"
               AnyMessageAndAgency tok (MsgIntersectNotFound _) -> show tok ++ " MsgIntersectNotFound"
               AnyMessageAndAgency tok message                  -> show tok ++ " " ++ show message
         in concat
            [ benchmarkCodec ("ChainSync " ++ printMsg msg)
                             (const chainSyncCodec) maxBound msg
            | msg <- chainSyncMessages
            ]
    , bgroup "BlockFetch Codec" $
        let printMsg :: AnyMessage (BlockFetch Block BlockPoint) -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (MsgRequestRange _) -> show tok ++ " MsgRequestRange"
               AnyMessageAndAgency tok (MsgBlock _)        -> show tok ++ " MsgBlock"
               AnyMessageAndAgency tok message             -> show tok ++ " " ++ show message
         in concat
            [ benchmarkCodec ("BlockFetch " ++ printMsg msg)
                             (const blockFetchCodec) maxBound msg
            | msg <- blockFetchMessages
            ]
    , bgroup "TxSumission2 Codec" $
        let printMsg :: AnyMessage (TxSubmission2 TxId Tx) -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (MsgReplyTxIds _) -> show tok ++ " MsgReplyTxIds"
               AnyMessageAndAgency tok (MsgRequestTxs _) -> show tok ++ " MsgRequestTxs"
               AnyMessageAndAgency tok (MsgReplyTxs _)   -> show tok ++ " MsgReplyTxs"
               AnyMessageAndAgency tok message           -> show tok ++ " " ++ show message
         in concat
            [ benchmarkCodec ("TxSumission2 " ++ printMsg msg)
                             (const txSubmissionCodec2) maxBound msg
            | msg <- txSubmission2Messages
            ]
    , bgroup "KeepAlive Codec" $
        let printMsg :: AnyMessage KeepAlive -> String
            printMsg msg = case msg of
              AnyMessageAndAgency tok message -> show tok ++ " " ++ show message
         in concat
            [ benchmarkCodec ("KeepAlive " ++ printMsg msg)
                             (const codecKeepAlive_v2) maxBound msg
            | msg <- keepAliveMessages
            ]
    , bgroup "LocalTxSubmission Codec" $
        let printMsg :: AnyMessage (LocalTxSubmission LocalTxSubmission.Tx LocalTxSubmission.Reject)
                     -> String
            printMsg msg = case msg of
              AnyMessageAndAgency tok (MsgSubmitTx _) -> show tok ++ " MsgSubmitTx"
              AnyMessageAndAgency tok (MsgRejectTx _) -> show tok ++ " MsgRejectTx"
              AnyMessageAndAgency tok message         -> show tok ++ " " ++ show message
         in concat
            [ benchmarkCodec ("LocalTxSubmission " ++ printMsg msg)
                             (const localTxSubmissionCodec) maxBound msg
            | msg <- localTxSubmissionMessages
            ]
    , bgroup "LocalTxMonitor Codec" $
        let printMsg :: AnyMessage (LocalTxMonitor TxId Tx SlotNo) -> String
            printMsg msg = case msg of
               AnyMessageAndAgency tok (LocalTxMonitor.MsgAcquired _)      -> show tok ++ " MsgAcquired"
               AnyMessageAndAgency tok (LocalTxMonitor.MsgReplyNextTx _)   -> show tok ++ " MsgReplyNextTx"
               AnyMessageAndAgency tok (LocalTxMonitor.MsgHasTx _)         -> show tok ++ " MsgHasTx"
               AnyMessageAndAgency tok (LocalTxMonitor.MsgReplyHasTx _)    -> show tok ++ " MsgReplyHasTx"
               AnyMessageAndAgency tok (LocalTxMonitor.MsgReplyGetSizes _) -> show tok ++ " MsgReplyGetSizes"
               AnyMessageAndAgency tok message                             -> show tok ++ " " ++ show message
         in concat
            [ benchmarkCodec ("LocalTxSubmission " ++ printMsg msg)
                             (const localTxMonitorCodec) maxBound msg
            | msg <- localTxMonitorMessages
            ]
    , bgroup "LocalStateQuery Codec" $
        concat
          [ benchmarkCodecSt ("LocalStateQuery " ++ show msg)
                             (const localStateQueryCodec) maxBound msg
          | msg <- getAnyMessageWithResult <$> localStateQueryMessages
          ]
    , bgroup "PeerSharing Codec" $
        let printMsg :: AnyMessage (PeerSharing SockAddr) -> String
            printMsg msg = case msg of
              AnyMessageAndAgency tok (MsgSharePeers _) -> show tok ++ " MsgSharePeers"
              AnyMessageAndAgency tok message           -> show tok ++ " " ++ show message
         in concat
              [ benchmarkCodec ("PeerSharing " ++ printMsg msg ++ " " ++ show ntnVersion)
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
                  => [AnyMessage (Handshake vNumber CBOR.Term)]
handshakeMessages =
  [ AnyMessage (MsgProposeVersions
                   (Map.fromList [ (ntnVersion , CBOR.TBytes largeBS)
                                 | ntnVersion <- [minBound .. maxBound]
                                 ]))
  , AnyMessage (MsgReplyVersions
                   (Map.fromList [ (ntnVersion , CBOR.TBytes largeBS)
                                 | ntnVersion <- [minBound .. maxBound]
                                 ]))
  , AnyMessage (MsgQueryReply
                   (Map.fromList [ (ntnVersion , CBOR.TBytes largeBS)
                                 | ntnVersion <- [minBound .. maxBound]
                                 ]))
  , AnyMessage (MsgAcceptVersion maxBound (CBOR.TBytes largeBS))
  , AnyMessage (MsgRefuse (VersionMismatch [minBound..maxBound]
                          (replicate 99 maxBound)))
  ]

chainSyncMessages :: [AnyMessage (ChainSync BlockHeader HeaderPoint HeaderTip)]
chainSyncMessages =
  [ AnyMessage MsgRequestNext
  , AnyMessage MsgAwaitReply
  , AnyMessage (MsgRollForward (BlockHeader largeCBORBS)
                               (HeaderTip largeCBORBS)
                  :: Message (ChainSync BlockHeader HeaderPoint HeaderTip)
                             (ChainSync.StNext ChainSync.StCanAwait)
                              ChainSync.StIdle
               )
  , AnyMessage (MsgRollForward (BlockHeader largeCBORBS)
                               (HeaderTip largeCBORBS)
                  :: Message (ChainSync BlockHeader HeaderPoint HeaderTip)
                             (ChainSync.StNext ChainSync.StMustReply)
                              ChainSync.StIdle
               )
  , AnyMessage (MsgRollBackward (HeaderPoint largeCBORBS)
                                (HeaderTip largeCBORBS)
                  :: Message (ChainSync BlockHeader HeaderPoint HeaderTip)
                             (ChainSync.StNext ChainSync.StCanAwait)
                              ChainSync.StIdle
               )
  , AnyMessage (MsgRollBackward (HeaderPoint largeCBORBS)
                                (HeaderTip largeCBORBS)
                  :: Message (ChainSync BlockHeader HeaderPoint HeaderTip)
                             (ChainSync.StNext ChainSync.StMustReply)
                              ChainSync.StIdle
               )
  , AnyMessage (MsgFindIntersect [HeaderPoint largeCBORBS])
  , AnyMessage (MsgIntersectFound (HeaderPoint largeCBORBS)
                                  (HeaderTip largeCBORBS))
  , AnyMessage (MsgIntersectNotFound (HeaderTip largeCBORBS))
  , AnyMessage ChainSync.MsgDone
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)

blockFetchMessages :: [AnyMessage (BlockFetch Block BlockPoint)]
blockFetchMessages =
  [ AnyMessage (MsgRequestRange (ChainRange (BlockPoint largeCBORBS)
                                            (BlockPoint largeCBORBS)))
  , AnyMessage MsgStartBatch
  , AnyMessage MsgNoBlocks
  , AnyMessage (MsgBlock (Block largeCBORBS))
  , AnyMessage MsgBatchDone
  , AnyMessage MsgClientDone
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)

txSubmission2Messages :: [AnyMessage (TxSubmission2 TxId Tx)]
txSubmission2Messages =
  [ AnyMessage MsgInit
  , AnyMessage (MsgRequestTxIds SingBlocking maxBound maxBound)
  , AnyMessage (MsgRequestTxIds SingNonBlocking maxBound maxBound)
  , AnyMessage (MsgReplyTxIds (BlockingReply nonEmpty))
  , AnyMessage (MsgReplyTxIds (NonBlockingReply list))
  , AnyMessage (MsgRequestTxs (replicate 9 largeTxId))
  , AnyMessage (MsgReplyTxs (replicate 9 largeTx))
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)
    largeTxId = TxId largeCBORBS
    largeTx = Tx largeTxId
    txSize = fromIntegral $ BS.length largeBS
    list = replicate 99 (largeTxId, txSize)
    nonEmpty = NonEmpty.fromList list

keepAliveMessages :: [AnyMessage KeepAlive]
keepAliveMessages =
  [ AnyMessage (MsgKeepAlive (Cookie maxBound))
  , AnyMessage (MsgKeepAliveResponse (Cookie maxBound))
  , AnyMessage KeepAlive.MsgDone
  ]

localTxSubmissionMessages :: [AnyMessage (LocalTxSubmission LocalTxSubmission.Tx LocalTxSubmission.Reject)]
localTxSubmissionMessages =
  [ AnyMessage (MsgSubmitTx (LocalTxSubmission.Tx largeCBORBS))
  , AnyMessage MsgAcceptTx
  , AnyMessage (MsgRejectTx (LocalTxSubmission.Reject maxBound))
  , AnyMessage LocalTxSubmission.MsgDone
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)

localTxMonitorMessages :: [AnyMessage (LocalTxMonitor TxId Tx SlotNo)]
localTxMonitorMessages =
  [ AnyMessage LocalTxMonitor.MsgAcquire
  , AnyMessage (LocalTxMonitor.MsgAcquired maxBound)
  , AnyMessage MsgAwaitAcquire
  , AnyMessage MsgNextTx
  , AnyMessage (MsgReplyNextTx (Just largeTx))
  , AnyMessage (MsgHasTx largeTxId)
  , AnyMessage (MsgReplyHasTx maxBound)
  , AnyMessage MsgGetSizes
  , AnyMessage (MsgReplyGetSizes (MempoolSizeAndCapacity maxBound maxBound maxBound))
  , AnyMessage LocalTxMonitor.MsgRelease
  , AnyMessage LocalTxMonitor.MsgDone
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)
    largeTxId = TxId largeCBORBS
    largeTx = Tx largeTxId

localStateQueryMessages :: [AnyMessageWithResult Block BlockPoint Query Result]
localStateQueryMessages =
  [ AnyMessageWithResult
      (Stateful.AnyMessage
        StateIdle
        (LocalStateQuery.MsgAcquire
        (SpecificPoint (BlockPoint largeCBORBS))))
  , AnyMessageWithResult
      (Stateful.AnyMessage
        StateAcquiring
        LocalStateQuery.MsgAcquired)
  , AnyMessageWithResult
      (Stateful.AnyMessage
         StateAcquiring
        (LocalStateQuery.MsgFailure LocalStateQuery.AcquireFailurePointNotOnChain))
  , AnyMessageWithResult
      (Stateful.AnyMessage
        StateAcquiring
        (LocalStateQuery.MsgFailure LocalStateQuery.AcquireFailurePointTooOld))
  , AnyMessageWithResult
      (Stateful.AnyMessage
        StateAcquired
        (MsgQuery (Query largeCBORBS)))
  , AnyMessageWithResult
      (Stateful.AnyMessage
        (StateQuerying (Query largeCBORBS))
        (MsgResult (Result (Any CBOR.TNull))))
  , AnyMessageWithResult
      (Stateful.AnyMessage
        StateAcquired
        LocalStateQuery.MsgRelease)
  , AnyMessageWithResult
      (Stateful.AnyMessage
        StateAcquired
        (MsgReAcquire (SpecificPoint (BlockPoint largeCBORBS))))
  , AnyMessageWithResult
      (Stateful.AnyMessage
        StateIdle
        LocalStateQuery.MsgDone)
  ]
  where
    largeCBORBS = Any (CBOR.TBytes largeBS)

peerSharingMessages :: [AnyMessage (PeerSharing SockAddr)]
peerSharingMessages =
  [ AnyMessage (MsgShareRequest (PeerSharingAmount maxBound))
  , AnyMessage PeerSharing.MsgDone
  , AnyMessage (MsgSharePeers [ SockAddrInet maxBound addr
                              | x <- [0..maxBound]
                              , let addr = tupleToHostAddress (192, 168, 1, x)
                              ])
  ]

benchmarkCodec :: ( forall (st :: ps) (st' :: ps). NFData (Message ps st st')
                  )
               => String
               -> (NodeToNodeVersion -> Codec ps DeserialiseFailure IO ByteString)
               -> NodeToNodeVersion
               -> AnyMessage ps
               -> [Benchmark]
benchmarkCodec title getCodec ntnVersion msg =
  [ benchmarkEncode (title ++ " " ++ "Encode") getCodec ntnVersion msg
  , benchmarkDecode (title ++ " " ++ "Decode") getCodec ntnVersion msg
  ]

benchmarkCodecSt :: (forall (st :: ps) (st' :: ps). NFData (Message ps st st'))
                 => String
                 -> (NodeToNodeVersion -> Stateful.Codec ps DeserialiseFailure f IO ByteString)
                 -> NodeToNodeVersion
                 -> Stateful.AnyMessage ps f
                 -> [Benchmark]
benchmarkCodecSt title getCodec ntnVersion msg =
  [ benchmarkEncodeSt (title ++ " " ++ "Encode") getCodec ntnVersion msg
  , benchmarkDecodeSt (title ++ " " ++ "Decode") getCodec ntnVersion msg
  ]

-- TODO: We don't force the agency along with the message because this leads
-- to a weird error that could be a tasty-bench bug.
-- See: https://github.com/Bodigrim/tasty-bench/issues/52
--
-- Only LocalStateQuery agency tokens carry the query information so it
-- shouldn't be too problematic.
benchmarkEncode :: ( forall (st :: ps) (st' :: ps). NFData (Message ps st st')
                   )
                => String
                -> (NodeToNodeVersion -> Codec ps DeserialiseFailure IO ByteString)
                -> NodeToNodeVersion
                -> AnyMessage ps
                -> Benchmark
benchmarkEncode title getCodec ntnVersion (AnyMessage msg) =
  let Codec { encode } = getCodec ntnVersion
   in env (pure msg) $ \message ->
        bench title $ nf encode message

-- TODO: We don't force the agency along with the message because this leads
-- to a weird error that could be a tasty-bench bug.
-- See: https://github.com/Bodigrim/tasty-bench/issues/52
--
-- Only LocalStateQuery agency tokens carry the query information so it
-- shouldn't be too problematic.
benchmarkDecode :: forall ps.
                   (forall (st :: ps) (st' :: ps). NFData (Message ps st st'))
                => String
                -> (NodeToNodeVersion -> Codec ps DeserialiseFailure IO ByteString)
                -> NodeToNodeVersion
                -> AnyMessage ps
                -> Benchmark
benchmarkDecode title getCodec ntnVersion (AnyMessageAndAgency !stok msg) =
  let Codec { encode
            , decode
            } = getCodec ntnVersion
   in env (pure (encode msg)) $ \encodedMessage ->
        bench title $ nfIO (do
                              decoder <- decode stok
                              res <- runDecoder [encodedMessage] decoder
                              return $ case res of
                                Left err -> Just err
                                Right !_ -> Nothing
                           )

-- TODO: We don't force the agency along with the message because this leads
-- to a weird error that could be a tasty-bench bug.
-- See: https://github.com/Bodigrim/tasty-bench/issues/52
--
-- Only LocalStateQuery agency tokens carry the query information so it
-- shouldn't be too problematic.
benchmarkEncodeSt :: (forall (st :: ps) (st' :: ps). NFData (Message ps st st'))
                  => String
                  -> (NodeToNodeVersion -> Stateful.Codec ps DeserialiseFailure f IO ByteString)
                  -> NodeToNodeVersion
                  -> Stateful.AnyMessage ps f
                  -> Benchmark
benchmarkEncodeSt title getCodec ntnVersion (Stateful.AnyMessage f msg) =
  let Stateful.Codec { Stateful.encode } = getCodec ntnVersion
   in env (pure msg) $ \message ->
        bench title $ nf (encode f) message

-- TODO: We don't force the agency along with the message because this leads
-- to a weird error that could be a tasty-bench bug.
-- See: https://github.com/Bodigrim/tasty-bench/issues/52
--
-- Only LocalStateQuery agency tokens carry the query information so it
-- shouldn't be too problematic.
benchmarkDecodeSt :: forall ps f.
                     (forall (st :: ps) (st' :: ps). NFData (Message ps st st'))
                  => String
                  -> (NodeToNodeVersion -> Stateful.Codec ps DeserialiseFailure f IO ByteString)
                  -> NodeToNodeVersion
                  -> Stateful.AnyMessage ps f
                  -> Benchmark
benchmarkDecodeSt title getCodec ntnVersion (Stateful.AnyMessageAndAgency !stok !f msg) =
  let Stateful.Codec { Stateful.encode
                     , Stateful.decode } = getCodec ntnVersion
   in env (pure (encode f msg)) $ \encodedMessage ->
        bench title $ nfIO (do
                              decoder <- decode stok f
                              res <- runDecoder [encodedMessage] decoder
                              return $ case res of
                                Left err -> Just err
                                Right !_ -> Nothing
                           )
