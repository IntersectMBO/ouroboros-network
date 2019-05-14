{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.TxSubmission.Examples where

import           Data.List (find)
import           Data.Word (Word16)

import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Server


-- |
-- An example @'TxSubmissionClient'@ which sends trasactions from a fixed pool
-- of transactions.  It returns a list of @tx@ sent back to the client.
--
-- It is only ment to be used in test.  The client will error if a server will
-- ask for a transaction which is not in the pool or if a server will ask for
-- the same trasaction twice.
--
txSubmissionClientFixed
  :: forall hash tx m.
     ( Applicative m
     , Eq hash
     )
  => [tx]
  -> (tx -> hash)
  -> TxSubmissionClient hash tx m [tx]
txSubmissionClientFixed txs0 txHash = TxSubmissionClient (pure $ handlers [] txs0)
    where
      handlers :: [tx] -> [tx] -> TxSubmissionHandlers hash tx m [tx]
      handlers !sent txs =
        TxSubmissionHandlers {
          getHashes = \n -> 
            let (resp, txs') = splitAt (fromIntegral n) txs
            in pure (map txHash resp, handlers sent txs'),
          getTx    = \hash -> case find (\tx -> txHash tx == hash) txs0 of
                        Nothing -> error "no such transaction"
                        Just tx -> pure (tx, handlers (tx:sent) txs),
          done     = reverse sent
        }

-- |
-- Auxilary data type which allows to collect requests and responses of
-- tx-submission protocol.
--
data ReqOrResp hash tx
  = ReqHashes Word16
  | RespHashes [hash]
  | ReqTx hash
  | RespTx tx
  deriving (Eq, Show)

-- |
-- A non pipelined tx-submission server.  It send @'MsgGetHashes'@ awaits for
-- the response and then requests each transaction awaiting for it before
-- sending the next @'MsgGetTx'@ request.
--
txSubmissionServer
  :: forall hash tx m.
     Applicative m
  => [Word16] -- ^ each element corresponds to a single @'MsgGetHashes'@ request
  -> TxSubmissionServerPipelined hash tx m [ReqOrResp hash tx]
txSubmissionServer ns0 = TxSubmissionServerPipelined (sender [] ns0)
    where
      sender
        :: [ReqOrResp hash tx]
        -> [Word16]
        -> TxSubmissionSender hash tx Z (Collection hash tx) m [ReqOrResp hash tx]
      sender txs []     = SendMsgDone (reverse txs)
      sender txs (n:ns) = SendMsgGetHashes n
                            $ \hs -> pure (getHashes (RespHashes hs:ReqHashes n:txs) ns hs)

      getHashes
        :: [ReqOrResp hash tx]
        -> [Word16]
        -> [hash]
        -> TxSubmissionSender hash tx Z (Collection hash tx) m [ReqOrResp hash tx]
      getHashes txs ns []     = sender txs ns
      getHashes txs ns (h:hs) = SendMsgGetTx h
                                  (pure $ CollectPipelined Nothing
                                            $ \c -> case c of
                                              Left _   -> pure $ getHashes txs      ns hs
                                              Right tx -> pure $ getHashes (RespTx tx:ReqTx h:txs) ns hs )

-- |
-- A piplined tx-submission server that sends @'MsgGetTx'@ eagerly but always tries to
-- collect any replies as soon as they are available.  This keeps pipelining to
-- bare minimum, and gives maximum choice to the environment (drivers).
--
-- It returns the interleaving of requests and received trasactions.
--
txSubmissionServerPipelinedMin
  :: forall hash tx m.
     Applicative m
  => [Word16]
  -> TxSubmissionServerPipelined hash tx m [ReqOrResp hash tx]
txSubmissionServerPipelinedMin ns0 = TxSubmissionServerPipelined (sender [] ns0)
    where
      sender
        :: [ReqOrResp hash tx]
        -> [Word16]
        -> TxSubmissionSender hash tx Z (Collection hash tx) m [ReqOrResp hash tx]
      sender txs []     = SendMsgDone (reverse txs)
      sender txs (n:ns) = SendMsgGetHashes n
                            $ \hs -> pure (getHashes (RespHashes hs:ReqHashes n:txs) ns hs Zero)

      getHashes
        :: [ReqOrResp hash tx]
        -> [Word16]
        -> [hash]
        -> Nat n
        -> TxSubmissionSender hash tx n (Collection hash tx) m [ReqOrResp hash tx]
      getHashes txs ns []         (Succ o) = CollectPipelined
                                                Nothing
                                                (\c -> case c of
                                                  Left _   -> error "txSubmissionServerPipelinedMin"
                                                  Right tx -> pure $ getHashes (RespTx tx:txs) ns [] o)

      getHashes txs ns hs@(h:hs') (Succ o) = CollectPipelined
                                               (Just $ requestMoreTx txs ns h hs' (Succ o))
                                               (\c -> case c of
                                                 Left _   -> error "txSubmissionServerPiplinedMin"
                                                 Right tx -> pure $ getHashes (RespTx tx:txs) ns hs o)
      
      getHashes txs ns (h:hs)     Zero     = requestMoreTx txs ns h hs Zero

      getHashes txs ns []         Zero     = sender txs ns

      
      requestMoreTx 
        :: [ReqOrResp hash tx]
        -> [Word16]
        -> hash
        -> [hash]
        -> Nat n
        -> TxSubmissionSender hash tx n (Collection hash tx) m [ReqOrResp hash tx]
      requestMoreTx txs ns h hs o = SendMsgGetTx h (pure $ getHashes (ReqTx h:txs) ns hs (Succ o))


-- |
-- An example tx-submission server which sends @'MsgGetHashes'@ awaits for the
-- response, and then piplines requests for each received hash.  The responses
-- are collected before next @'MsgGetHashes'@.
--
txSubmissionServerPipelinedMax
  :: forall hash tx m.
     Applicative m
  => [Word16] -- ^ each element corresponds to a single @'MsgGetHashes'@ request
  -> TxSubmissionServerPipelined hash tx m [ReqOrResp hash tx]
txSubmissionServerPipelinedMax ns0 = TxSubmissionServerPipelined (sender [] ns0)
    where
      sender
        :: [ReqOrResp hash tx]
        -> [Word16]
        -> TxSubmissionSender hash tx Z (Collection hash tx) m [ReqOrResp hash tx]
      sender txs []     = SendMsgDone (reverse txs)
      sender txs (n:ns) = SendMsgGetHashes n 
                            $ \hs -> pure (getHashes (RespHashes hs:ReqHashes n:txs) ns hs Zero)

      getHashes
        :: [ReqOrResp hash tx]
        -> [Word16]
        -> [hash]
        -> Nat n
        -> TxSubmissionSender hash tx n (Collection hash tx) m [ReqOrResp hash tx]
      getHashes txs ns []     Zero     = sender txs ns
      getHashes txs ns (h:hs) o        = SendMsgGetTx h (pure $ getHashes (ReqTx h:txs) ns hs (Succ o))
      getHashes txs ns []     (Succ o) = CollectPipelined Nothing
                                        (\c -> case c of
                                          Left  _  -> pure $ getHashes txs             ns [] o
                                          Right tx -> pure $ getHashes (RespTx tx:txs) ns [] o)

-- |
-- Like @'txSubmissionServerPipelinedMin'@ but it also pipelines @'MsgGetHashes'@.
-- We pipeline the request for more hashes whenever we already get half of the
-- transactions.
--
-- It returns the interleaving of requests and received trasactions.
--
txSubmissionServerPipelinedAllMin
  :: forall hash tx m.
     ( Applicative m
     , Eq hash
     )
  => [Word16]
  -> TxSubmissionServerPipelined hash tx m [ReqOrResp hash tx]
txSubmissionServerPipelinedAllMin ns0 = TxSubmissionServerPipelined (sender [] ns0)
    where
      middle :: [as] -> Maybe as
      middle as = let l = length as
                      i = l `div` 2
                  in if i < l then Just (as !! i) else Nothing

      sender
        :: [ReqOrResp hash tx]
        -> [Word16]
        -> TxSubmissionSender hash tx Z (Collection hash tx) m [ReqOrResp hash tx]
      sender txs []     = SendMsgDone (reverse txs)
      sender txs (n:ns) = SendMsgGetHashes n
                            $ \hs -> pure (getHashes (RespHashes hs:ReqHashes n:txs) ns hs (middle hs) Zero)

      senderPipelined
        :: [ReqOrResp hash tx]
        -> [Word16]
        -> [hash]
        -> Nat n
        -> TxSubmissionSender hash tx n (Collection hash tx) m [ReqOrResp hash tx]
      senderPipelined txs (n:ns) hs o = SendMsgGetHashesPipelined n
                                                         (pure $ getHashes (ReqHashes n:txs) ns hs Nothing (Succ o)) 

      senderPipelined txs []     hs@(_:_) (o@Succ{}) = getHashes txs [] hs Nothing o

      senderPipelined txs []     hs@(_:_) Zero       = getHashes txs [] hs Nothing Zero

      senderPipelined txs []     []       Zero       = SendMsgDone (reverse txs)

      senderPipelined _   []     []       Succ{}     = error "ups, impossible is possible!"


      -- Eagerly collect responses; send @'MsgGetTx'@, after we reached the
      -- marked hash, we pipeline @'MsgGetHashes'@ using @'senderPipelined'@
      getHashes
        :: [ReqOrResp hash tx]
        -> [Word16]
        -> [hash]
        -> Maybe hash -- ^ hash at which we do @'senderPipelined'@
        -> Nat n
        -> TxSubmissionSender hash tx n (Collection hash tx) m [ReqOrResp hash tx]
      getHashes txs ns (h:hs) hash o        | hash == Just h
                                            = senderPipelined txs ns (h:hs) o

      getHashes txs ns []     hash (Succ o) = CollectPipelined
                                                Nothing
                                                (\c -> case c of
                                                  Left hs  -> pure $ getHashes (RespHashes hs:txs) ns hs (middle hs) o
                                                  Right tx -> pure $ getHashes (RespTx tx:txs) ns [] hash o)

      getHashes txs ns (h:hs) hash (Succ o) = CollectPipelined
                                                (Just $ requestMoreTx txs ns h hs hash (Succ o))
                                                (\c -> case c of
                                                  Left hs'  ->
                                                    let hsNext = hs ++ hs'
                                                    in pure $ getHashes (RespHashes hs:txs) ns hsNext (middle hsNext) o
                                                  Right tx -> pure $ getHashes (RespTx tx:txs) ns hs hash o)
      
      getHashes txs ns (h:hs) hash Zero     = requestMoreTx txs ns h hs hash Zero

      getHashes txs ns []     _    Zero     = sender txs ns

      
      requestMoreTx 
        :: [ReqOrResp hash tx]
        -> [Word16]
        -> hash
        -> [hash]
        -> Maybe hash
        -> Nat n
        -> TxSubmissionSender hash tx n (Collection hash tx) m [ReqOrResp hash tx]
      requestMoreTx txs ns h hs hash o = SendMsgGetTx h (pure $ getHashes (ReqTx h:txs) ns hs hash (Succ o))
