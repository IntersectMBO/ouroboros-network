{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit properties of `txSubmissionOutbound`, driven directly by the
-- requests of the inbound side, or run against a scripted inbound side.
--
module Test.Ouroboros.Network.TxSubmission.Outbound (tests) where

import Control.Concurrent.Class.MonadSTM
import Control.Concurrent.Class.MonadSTM.Strict qualified as StrictSTM
import Control.Monad (forever)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI (MonadDelay, threadDelay)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (nullTracer)
import Data.Foldable qualified as Foldable
import Data.Function (on)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word16)

import Network.TypedProtocol.Core (N (..))

import Ouroboros.Network.Channel (createConnectedChannels)
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Driver (runPeer, runPipelinedPeer)
import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool
import Ouroboros.Network.TxSubmission.Outbound

import Test.Ouroboros.Network.TxSubmission.Types

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Outbound"
    [ testProperty "blocking request with a zero window is a protocol error"
                   prop_blockingRequest_zeroWindow
    , testProperty "blocking requests respect the window"
                   prop_blockingRequest_positiveWindow
    , testProperty "non-blocking requests respect the window after acknowledgements"
                   prop_nonBlockingRequests_respectWindow
    , testProperty "re-announced tx is served"
                   prop_reAnnouncedTxServed
    , testProperty "duplicate request is rejected"
                   prop_duplicateRequestRejected
    , testProperty "acknowledging the older entry keeps the newer one requested"
                   prop_ackOlderEntry
    ]


-- | Send a blocking `MsgRequestTxIds` for `reqNo` txids to a fresh
-- `txSubmissionOutbound` whose mempool holds `n` txs.  Returns the protocol
-- error, or the txids of the reply (`Nothing` for `MsgDone`).
--
blockingRequest :: NumTxIdsToAck
                -> NumTxIdsToReq
                -> Word16
                -> Either TxSubmissionProtocolError (Maybe [TxId])
blockingRequest maxUnacked reqNo n = runSimOrThrow $ do
    mempool <- newMempool
                 [ Tx { getTxId      = txid,
                        getTxSize    = 100,
                        getTxAdvSize = 100,
                        getTxValid   = True,
                        getTxParent  = Nothing }
                 | txid <- [1 .. fromIntegral n] ]
    ClientStIdle { recvMsgRequestTxIds } <-
      runTxSubmissionClient $
        txSubmissionOutbound nullTracer maxUnacked (getMempoolReader mempool)
                             () (pure Continue)
    reply <- try (recvMsgRequestTxIds SingBlocking 0 reqNo)
    pure $ case reply of
      Left err -> Left err
      Right (SendMsgReplyTxIds (BlockingReply txids) _) ->
        Right (Just (fst <$> NonEmpty.toList txids))
      Right (SendMsgDone ()) -> Right Nothing


-- | Any positive blocking request exceeds a zero window.
--
prop_blockingRequest_zeroWindow :: Positive (Small Word16)
                                -> Positive (Small Word16)
                                -> Property
prop_blockingRequest_zeroWindow (Positive (Small reqNo))
                                (Positive (Small n)) =
    case blockingRequest 0 (NumTxIdsToReq reqNo) n of
      Left ProtocolErrorRequestedTooManyTxids {} -> property True
      result -> counterexample (show result) False


-- | A blocking request within the window returns the first txs in the
-- mempool. A request above the window raises a protocol error.
--
prop_blockingRequest_positiveWindow :: Positive (Small Word16)
                                    -> Positive (Small Word16)
                                    -> Positive (Small Word16)
                                    -> Property
prop_blockingRequest_positiveWindow (Positive (Small window))
                                    (Positive (Small reqNo))
                                    (Positive (Small n)) =
    classify (reqNo > window) "request exceeds window" $
    case blockingRequest (NumTxIdsToAck window) (NumTxIdsToReq reqNo) n of
      Left ProtocolErrorRequestedTooManyTxids {}
        | reqNo > window  -> property True
      Right (Just txids)
        | reqNo <= window -> txids === [1 .. fromIntegral (min reqNo n)]
      result -> counterexample (show result) False


-- | A `MsgRequestTxIds`.
--
data Request = Blocking    NumTxIdsToAck NumTxIdsToReq
             | NonBlocking NumTxIdsToAck NumTxIdsToReq
  deriving Show

-- | Send the requests in turn to a fresh `txSubmissionOutbound` whose mempool
-- holds `n` txs.  Returns the txids of each reply, up to the first protocol
-- error or `MsgDone`.
--
runRequests :: NumTxIdsToAck
            -> Int
            -> [Request]
            -> ([[TxId]], Maybe TxSubmissionProtocolError)
runRequests maxUnacked n requests = runSimOrThrow $ do
    mempool <- newMempool
                 [ Tx { getTxId      = txid,
                        getTxSize    = 100,
                        getTxAdvSize = 100,
                        getTxValid   = True,
                        getTxParent  = Nothing }
                 | txid <- [1 .. n] ]
    client <-
      runTxSubmissionClient $
        txSubmissionOutbound nullTracer maxUnacked (getMempoolReader mempool)
                             () (pure Continue)
    go client requests []
  where
    go _ [] replies = pure (reverse replies, Nothing)
    go ClientStIdle { recvMsgRequestTxIds } (request : rest) replies = do
      reply <- try $ case request of
        Blocking ackNo reqNo -> do
          r <- recvMsgRequestTxIds SingBlocking ackNo reqNo
          pure $ case r of
            SendMsgReplyTxIds (BlockingReply txids) k ->
              Just (fst <$> NonEmpty.toList txids, k)
            SendMsgDone () -> Nothing
        NonBlocking ackNo reqNo -> do
          SendMsgReplyTxIds (NonBlockingReply txids) k <-
            recvMsgRequestTxIds SingNonBlocking ackNo reqNo
          pure (Just (fst <$> txids, k))
      case reply of
        Left err                -> pure (reverse replies, Just err)
        Right Nothing           -> pure (reverse replies, Nothing)
        Right (Just (txids, k)) -> go k rest (txids : replies)


-- | Acknowledgements free slots for new txids in the same request or a
-- later request. Asking for one more txid with a full window is an error.
--
prop_nonBlockingRequests_respectWindow :: Positive (Small Word16)
                                       -> Positive (Small Word16)
                                       -> Property
prop_nonBlockingRequests_respectWindow (Positive (Small w0)) (Positive (Small k0)) =
    let w = max 2 (fromIntegral w0)
        -- Keep an unacknowledged txid so non-blocking requests are valid.
        k = min (fromIntegral k0) (w - 1)
        requests =
          [ Blocking    0                (fromIntegral w)
          , NonBlocking (fromIntegral k) (fromIntegral k)
          , NonBlocking 1                0
          , NonBlocking 0                1
          , NonBlocking 0                1
          ]
        expected =
          [ [1 .. w]
          , [w + 1 .. w + k]
          , []
          , [w + k + 1]
          ]
    in case runRequests (fromIntegral w) (w + k + 1) requests of
         (replies, Just ProtocolErrorRequestedTooManyTxids {}) -> replies === expected
         result -> counterexample (show result) False


--
-- Outbound against a scripted inbound
--

-- | Scripted behaviours of the inbound side.
--
data InboundScript =
    RequestAfterReAnnounce
    -- ^ request txids only after each of them was announced for the second
    -- time, then request them again (requires 'reAddAnnounced' on the
    -- outbound side and a window which can hold both announcements of every
    -- tx)
  | RequestTwice
    -- ^ request the txids of the first reply, then request the first of them
    -- again, without it being announced again, which is a protocol violation
  | RequestAcrossReAnnounce
    -- ^ request the first announced txid, then again once it was announced
    -- for the second time, acknowledge only its older entry, and request it
    -- for the third time, which is a protocol violation (requires
    -- 'reAddAnnounced' on the outbound side)
  deriving Show


-- | A scripted inbound side.  It requests up to @reqNo@ txids at a time and
-- acknowledges only what its script says.  It passes the bodies received in
-- reply to each of its requests for txs to the first callback, and calls the
-- second one when its script is done, then stays idle; there's no timeout in
-- `StIdle`.
--
scriptedInbound
  :: forall m.
     MonadDelay m
  => NumTxIdsToReq
  -- ^ maximum number of txids to request at a time
  -> Int
  -- ^ number of distinct txs in the outbound's mempool
  -> ([Tx TxId] -> m ())
  -- ^ record a reply to a request for txs
  -> m ()
  -- ^ the script is done
  -> InboundScript
  -> TxSubmissionServerPipelined TxId (Tx TxId) m ()
scriptedInbound reqNo txsNo received finished script =
    TxSubmissionServerPipelined $ pure $
      -- the window holds two announcements of every tx
      SendMsgRequestTxIdsBlocking 0 (reqNo `min` fromIntegral (2 * txsNo)) (pure ()) $ \txids ->
        let announced = announce Map.empty (NonEmpty.toList txids) in
        case script of
          RequestAfterReAnnounce ->
            collectAnnouncements announced

          RequestTwice ->
              requestTxs (fst <$> announced)
              -- request the first txid again, without it being announced
              -- again
            $ requestTxs (Map.take 1 (fst <$> announced))
            $ done

          RequestAcrossReAnnounce ->
            -- the first announced txid, its entry is the oldest one in the
            -- window
            let (txid, size) = NonEmpty.head txids
                request = Map.singleton txid size in

              -- `reAddAnnounced` already moved the tx on the outbound side, so
              -- nothing is sent back because the tx changed its idx
              requestTxs request
              -- wait for the tx to be reannounced
            $ reAnnounce txid announced
              -- served from the newer entry
            $ requestTxs request
              -- acknowledge only the older entry
            $ pure . SendMsgRequestTxIdsPipelined 1 0
            $ pure . CollectPipelined Nothing $ \case
                CollectTxIds {} ->
                  -- only the newer entry is left, and it was already
                  -- requested: a protocol error, `done` is never reached.
                  requestTxs request done
                CollectTxs {} ->
                  error "scriptedInbound: unexpected CollectTxs"
  where
    -- count announcements of each txid
    announce :: Map TxId (SizeInBytes, Int)
             -> [(TxId, SizeInBytes)]
             -> Map TxId (SizeInBytes, Int)
    announce =
      List.foldl' (\m (txid, size) ->
                    Map.insertWith (\_ (size', n) -> (size', n + 1))
                                   txid (size, 1) m)

    -- request more txids until all txs are announced twice, then request all
    -- of them, twice
    collectAnnouncements :: Map TxId (SizeInBytes, Int)
                         -> m (ServerStIdle Z TxId (Tx TxId) m ())
    collectAnnouncements announced
      | missing == 0
      = requestTxs (fst <$> announced) $
          requestTxs (fst <$> announced) done
      | otherwise
      = requestMoreTxIds announced collectAnnouncements
      where
        missing = 2 * txsNo - sum (snd <$> announced)

    -- request more txids until @txid@ is announced for the second time
    reAnnounce :: TxId
               -> Map TxId (SizeInBytes, Int)
               -> m (ServerStIdle Z TxId (Tx TxId) m ())
               -> m (ServerStIdle Z TxId (Tx TxId) m ())
    reAnnounce txid announced k
      | Just (_, a) <- Map.lookup txid announced
      , a >= 2
      = k
      | otherwise
      = requestMoreTxIds announced $ \announced' -> reAnnounce txid announced' k

    -- request more txids, without acknowledging any, as many as fit in the
    -- window which holds two announcements of every tx
    requestMoreTxIds
      :: Map TxId (SizeInBytes, Int)
      -> (Map TxId (SizeInBytes, Int) -> m (ServerStIdle Z TxId (Tx TxId) m ()))
      -> m (ServerStIdle Z TxId (Tx TxId) m ())
    requestMoreTxIds announced k =
      pure $ SendMsgRequestTxIdsPipelined 0 (reqNo `min` fromIntegral missing) $ pure $
        CollectPipelined Nothing $ \case
          CollectTxIds _ [] ->
            error "scriptedInbound: empty reply"
          CollectTxIds _ txids ->
            k (announce announced txids)
          CollectTxs {} ->
            error "scriptedInbound: unexpected CollectTxs"
      where
        missing = 2 * txsNo - sum (snd <$> announced)

    -- request txs, record the reply and continue
    requestTxs
      :: forall n.
         Map TxId SizeInBytes
      -> m (ServerStIdle n TxId (Tx TxId) m ())
      -> m (ServerStIdle n TxId (Tx TxId) m ())
    requestTxs txids k =
      pure $ SendMsgRequestTxsPipelined txids $ pure $
        CollectPipelined Nothing $ \case
          CollectTxs _ txs -> received txs
                           >> k
          CollectTxIds {}  -> error "scriptedInbound: unexpected CollectTxIds"

    done :: m (ServerStIdle Z TxId (Tx TxId) m ())
    done = finished >> forever (threadDelay 3600)


-- | Re-add every announced tx to the mempool once, right after it was
-- announced: it is moved to the end of the mempool under a fresh index, as if
-- it left the mempool and was added again.  The next 'MsgReplyTxIds'
-- announces it again, so the inbound's unacknowledged window holds the same
-- txid twice, the older entry referring to an index which is no longer in
-- the mempool.
--
reAddAnnounced
  :: forall m a.
     MonadSTM m
  => Mempool m TxId (Tx TxId)
  -> TxSubmissionClient TxId (Tx TxId) m a
  -> TxSubmissionClient TxId (Tx TxId) m a
reAddAnnounced (Mempool.Mempool mempoolVar) (TxSubmissionClient mIdle) =
    TxSubmissionClient (wrapIdle Set.empty <$> mIdle)
  where
    reAdd :: [TxId] -> m ()
    reAdd txids =
      atomically $ StrictSTM.modifyTVar mempoolVar $
        \ms@Mempool.MempoolSeq { Mempool.mempoolSeq, Mempool.nextIdx } ->
          let (moved, kept) =
                Seq.partition ((`elem` txids) . getTxId . Mempool.getTx) mempoolSeq
              moved' = Seq.fromList
                         [ Mempool.WithIndex idx (Mempool.getTx wtx)
                         | (idx, wtx) <- zip [nextIdx ..] (Foldable.toList moved)
                         ]
          in ms { Mempool.mempoolSeq = kept <> moved'
                , Mempool.nextIdx    = nextIdx + fromIntegral (Seq.length moved)
                }

    -- `reAdded`: txids which were already re-added; each tx is re-added
    -- only once, so it's announced at most twice.
    wrapIdle :: Set TxId
             -> ClientStIdle TxId (Tx TxId) m a
             -> ClientStIdle TxId (Tx TxId) m a
    wrapIdle reAdded ClientStIdle { recvMsgRequestTxIds, recvMsgRequestTxs } = ClientStIdle
      { recvMsgRequestTxIds = \blocking ack req -> do
          reply <- recvMsgRequestTxIds blocking ack req
          case reply of
            SendMsgReplyTxIds txids k -> do
              let new = [ txid
                        | (txid, _) <- Foldable.toList txids
                        , txid `Set.notMember` reAdded
                        ]
              reAdd new
              pure (SendMsgReplyTxIds txids (wrapIdle (reAdded <> Set.fromList new) k))
            SendMsgDone a -> pure (SendMsgDone a)
      , recvMsgRequestTxs = \txids -> do
          SendMsgReplyTxs txs k <- recvMsgRequestTxs txids
          pure (SendMsgReplyTxs txs (wrapIdle reAdded k))
      }


-- | Run `txSubmissionOutbound` against 'scriptedInbound'.  Returns the
-- protocol error thrown by the outbound (`Nothing` if the script is done),
-- and the txids of the bodies received by the inbound until then, for each of
-- its requests for txs.
--
runOutboundWithScriptedInbound
  :: Bool
  -- ^ wrap the outbound with 'reAddAnnounced'
  -> InboundScript
  -> NumTxIdsToReq
  -> [Tx TxId]
  -- ^ txs in the outbound's mempool; their txids must be distinct
  -> (Maybe TxSubmissionProtocolError, [[TxId]])
runOutboundWithScriptedInbound reAdd script reqNo txs = runSimOrThrow $ do
    mempool <- newMempool txs
    -- replies in reverse order
    receivedVar <- newTVarIO []
    finishedVar <- newEmptyTMVarIO
    (outboundChannel, inboundChannel) <- createConnectedChannels
    let outbound = txSubmissionOutbound nullTracer maxUnacked
                     (getMempoolReader mempool) () (pure Continue)
        client | reAdd     = reAddAnnounced mempool outbound
               | otherwise = outbound
        server = scriptedInbound reqNo (length txs)
                   (\txs' -> atomically $ modifyTVar receivedVar (map getTxId txs' :))
                   (atomically $ putTMVar finishedVar ())
                   script
    withAsync (runPeer nullTracer txSubmissionCodec2 outboundChannel
                       (txSubmissionClientPeer client)) $ \outboundAsync ->
      withAsync (runPipelinedPeer nullTracer txSubmissionCodec2 inboundChannel
                                  (txSubmissionServerPeerPipelined server)) $ \inboundAsync ->
        atomically $ do
          result <-
                (Nothing <$ takeTMVar finishedVar)
            `orElse`
                (waitCatchSTM outboundAsync >>= \case
                  Left e | Just err <- fromException e -> pure (Just err)
                  Left e                               -> throwSTM e
                  Right _                              -> retry)
            `orElse`
                (waitCatchSTM inboundAsync >>= \case
                  Left e  -> throwSTM e
                  Right _ -> retry)
          received <- reverse <$> readTVar receivedVar
          pure (result, received)
  where
    -- the window holds two announcements of every tx
    maxUnacked = 2 * fromIntegral (length txs)


-- | Txs with distinct txids, and the maximum number of txids requested at
-- a time.
--
genScriptedInput :: NonEmptyList (Tx TxId)
                 -> Positive (Small Word16)
                 -> ([Tx TxId], NumTxIdsToReq)
genScriptedInput (NonEmpty txs) (Positive (Small reqNo)) =
    (List.nubBy ((==) `on` getTxId) txs, NumTxIdsToReq reqNo)


-- | Txs which left the outbound's mempool and were added again under a fresh
-- index are announced again, so the inbound's unacknowledged window holds
-- each txid twice, the older entry referring to an index which is no longer
-- in the mempool.  Each txid can be requested twice, once for each entry, in
-- the order of the entries: the first request is served from the older
-- entry, so nothing is sent, the second one from the newer entry.
--
prop_reAnnouncedTxServed :: NonEmptyList (Tx TxId)
                         -> Positive (Small Word16)
                         -> Property
prop_reAnnouncedTxServed txs0 reqNo0 =
    let (txs, reqNo) = genScriptedInput txs0 reqNo0 in
    case runOutboundWithScriptedInbound True RequestAfterReAnnounce reqNo txs of
      (Nothing, replies) -> (List.sort <$> replies) === [[], List.sort (getTxId <$> txs)]
      result             -> counterexample (show result) False


-- | Requesting the same txid twice, without it being announced again, is
-- a protocol error.
--
prop_duplicateRequestRejected :: NonEmptyList (Tx TxId)
                              -> Positive (Small Word16)
                              -> Property
prop_duplicateRequestRejected txs0 reqNo0 =
    let (txs, reqNo) = genScriptedInput txs0 reqNo0
        -- txids announced in reply to the first, blocking, request
        announced = getTxId <$> take (fromIntegral (getNumTxIdsToReq reqNo)) txs
    in
    case runOutboundWithScriptedInbound False RequestTwice reqNo txs of
      -- the first request is served, the second one, for the smallest
      -- announced txid, is rejected
      (Just (ProtocolErrorDuplicateTxIds txid), [reply]) ->
             List.sort reply === List.sort announced
        .&&. show txid === show (minimum announced)
      result -> counterexample (show result) False


-- | A txid requested before and after it was announced again, has two
-- requested entries.  Acknowledging the older one leaves the newer one, which
-- was requested, so requesting the txid again is a protocol error.
--
prop_ackOlderEntry :: NonEmptyList (Tx TxId)
                   -> Positive (Small Word16)
                   -> Property
prop_ackOlderEntry txs0 reqNo0 =
    case genScriptedInput txs0 reqNo0 of
      ([], _) -> error "prop_ackOlderEntry: no txs"
      (txs@(tx : _), reqNo) ->
        -- the first announced txid
        let txid = getTxId tx in
        case runOutboundWithScriptedInbound True RequestAcrossReAnnounce reqNo txs of
          (Just (ProtocolErrorDuplicateTxIds txid'), replies) ->
                 show txid' === show txid
            .&&. replies === [[], [txid]]
          result -> counterexample (show result) False
