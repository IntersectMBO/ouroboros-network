{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Mock.TxSubmission (
      command'
    , parseMockTx
    , handleTxSubmission
    , handleUSSASubmission
    , spawnMempoolListener
    , spawnUSSAListener
    ) where

import           Codec.Serialise (decode, hPutSerialise)
import qualified Control.Concurrent.Async as Async
import           Control.Monad.Except
import           Control.Monad.Class.MonadSTM
import           Control.Tracer
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Options.Applicative
import           System.IO (IOMode (..))

import           Ouroboros.Consensus.Crypto.Hash (ShortHash)
import qualified Ouroboros.Consensus.Crypto.Hash as H
import           Ouroboros.Consensus.Demo.Run
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.NodeId (NodeId (..))
import           Ouroboros.Consensus.Node   (NodeKernel (..))
import           Ouroboros.Consensus.Update
import           Ouroboros.Consensus.Util.CBOR (Decoder (..), initDecoderIO)
import           Ouroboros.Consensus.Util.Condense

import           NamedPipe
import           Topology

{-------------------------------------------------------------------------------
  Parsers for the mock UTxO model
-------------------------------------------------------------------------------}

parseMockTx :: Parser Mock.Tx
parseMockTx = mkTx
    <$> many parseMockTxIn
    <*> many parseMockTxOut
  where
    mkTx :: [Mock.TxIn] -> [Mock.TxOut] -> Mock.Tx
    mkTx ins = Mock.Tx (Set.fromList ins)

parseMockTxIn :: Parser Mock.TxIn
parseMockTxIn = (,)
    <$> strOption (mconcat [
            long "txin"
          , help "Hash of the input transaction. Single hex char."
          ])
    <*> option auto (mconcat [
            long "txix"
          , help "Index of the output in the specified transaction"
          ])

parseMockTxOut :: Parser Mock.TxOut
parseMockTxOut = (,)
    <$> strOption (mconcat [
            long "address"
          , help "Address to transfer to"
          ])
    <*> option auto (mconcat [
            long "amount"
          , help "Amount to transfer"
          ])


{-------------------------------------------------------------------------------
  optparse-applicative auxiliary
-------------------------------------------------------------------------------}

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper) $ mconcat [
        progDesc descr
      ]

{-------------------------------------------------------------------------------
  Main logic
-------------------------------------------------------------------------------}

withValidatedNode :: TopologyInfo -> (TopologyInfo -> IO a) -> IO a
withValidatedNode tinfo act = do
    topoE <- readTopologyFile (topologyFile tinfo)
    case topoE of
         Left e  -> error e
         Right t ->
             case M.lookup (node tinfo) (toNetworkMap t) of
                  Nothing -> error "Target node not found."
                  Just _  -> act tinfo

{-------------------------------------------------------------------------------
  USSA (Update System Stimulus Args) smuggling
-------------------------------------------------------------------------------}
-- | Very roughly validate US stimulus args.
basicValidateUSSA :: USSArgs -> Maybe String

basicValidateUSSA (SubmitVote _upid _forAgainst) = Nothing

basicValidateUSSA (ProposeSoftware mprop) =
  case mprop of
    MProposalBody Nothing Nothing (Just _) hashes ->
      if M.null hashes
      then Just "software proposal mentions no payloads"
      else Nothing
    MProposalBody _ _ (Just _) _ -> Just "software proposal has protocol elements"
    MProposalBody _ _ Nothing  _ -> Just "software proposal missing software version"

basicValidateUSSA (ProposeProtocol mprop) =
  case mprop of
    MProposalBody (Just _) (Just _) Nothing _ -> Nothing
    MProposalBody (Just _) (Just _) _ _ -> Just "protocol proposal has software elements"
    MProposalBody Nothing  (Just _) _ _ -> Just "protocol proposal missing protocol version"
    MProposalBody _        Nothing  _ _ -> Just "protocol proposal missing protocol parameters update"

-- | Submission side.
handleUSSASubmission :: TopologyInfo -> USSArgs -> IO ()
handleUSSASubmission tinfo ussa =
  withValidatedNode tinfo $ \_ -> do
    case basicValidateUSSA ussa of
      Nothing -> withUSSAPipe (node tinfo) WriteMode False $ \h -> do
        hPutSerialise h ussa
      Just err ->
        error $ "Update system stimulus malformed: " <> err <> ".  Stimulus: " <> show ussa

-- | Node side.
readIncomingUSSArgs
               :: RunDemo blk
               => Tracer IO String
               -> NodeKernel IO peer blk
               -> Decoder IO
               -> IO ()
readIncomingUSSArgs tracer kernel Decoder{..} = forever $ do
    ussa :: USSArgs <- decodeNext decode
    let mErr = basicValidateUSSA ussa
    case mErr of
      Just err ->
        traceWith tracer $ "Update system stimulus malformed: " <> err <> ".  Stimulus: " <> show ussa
      Nothing  -> do
        traceWith tracer $ "Locally submitted update system stimulus not obviously malformed, processing: " <> show ussa
        atomically $ writeTQueue (getUSSAQueue kernel) ussa

{-------------------------------------------------------------------------------
  Tx smuggling
-------------------------------------------------------------------------------}
handleTxSubmission :: TopologyInfo -> Mock.Tx -> IO ()
handleTxSubmission tinfo tx =
  withValidatedNode tinfo $ \_ ->
  submitTx (node tinfo) tx

submitTx :: NodeId -> Mock.Tx -> IO ()
submitTx n tx = do
    withTxPipe n WriteMode False $ \h -> hPutSerialise h tx
    putStrLn $ "The Id for this transaction is: " <> condense (H.hash @ShortHash tx)

-- | Auxiliary to 'spawnMempoolListener'
readIncomingTx :: RunDemo blk
               => Tracer IO String
               -> NodeKernel IO peer blk
               -> Decoder IO
               -> IO ()
readIncomingTx tracer kernel Decoder{..} = forever $ do
    newTx :: Mock.Tx <- decodeNext decode
    rejected <- addTxs (getMempool kernel) [demoMockTx (getNodeConfig kernel) newTx]
    traceWith tracer $
      (if null rejected then "Accepted" else "Rejected") <>
      " transaction: " <> show newTx

{-------------------------------------------------------------------------------
  Listeners
-------------------------------------------------------------------------------}
-- | Listen for objects coming down a named pipe and process them
spawnListener :: RunDemo blk
              => (NodeId -> String)
              -> (Tracer IO String -> NodeKernel IO peer blk -> Decoder IO -> IO ())
              -> Tracer IO String
              -> NodeId
              -> NodeKernel IO peer blk
              -> IO (Async.Async ())
spawnListener pipeSchema process tracer myNodeId kernel = do
    Async.async $ do
        -- Apparently I have to pass 'ReadWriteMode' here, otherwise the
        -- node will die prematurely with a (DeserialiseFailure 0 "end of input")
        -- error.
        withNamedPipe pipeSchema myNodeId ReadWriteMode True $ \h -> do
            let getChunk = BS.hGetSome h 1024
            decoder <- initDecoderIO getChunk
            process tracer kernel decoder

spawnMempoolListener
              :: RunDemo blk
              => Tracer IO String
              -> NodeId
              -> NodeKernel IO peer blk
              -> IO (Async.Async ())
spawnMempoolListener = spawnListener namedTxPipeFor readIncomingTx

spawnUSSAListener
              :: RunDemo blk
              => Tracer IO String
              -> NodeId
              -> NodeKernel IO peer blk
              -> IO (Async.Async ())
spawnUSSAListener = spawnListener namedUSSAPipeFor readIncomingUSSArgs
