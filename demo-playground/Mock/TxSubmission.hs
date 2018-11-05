{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mock.TxSubmission (
      command'
    , parseMockTx
    , handleTxSubmission
    , spawnMempoolListener
    ) where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Void
import           Options.Applicative
import           System.IO (IOMode (..))

import           Ouroboros.Consensus.Crypto.Hash (MD5)
import qualified Ouroboros.Consensus.Crypto.Hash as H
import           Ouroboros.Consensus.Ledger.Mempool (Mempool (..), consistent,
                     mempoolInsert)
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Util (Condense (..))
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.MonadClass hiding (threadDelay)
import           Ouroboros.Network.Node (NodeId (..))
import           Ouroboros.Network.Pipe
import           Ouroboros.Network.Serialise

import           NamedPipe
import           Payload
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


handleTxSubmission :: TopologyInfo -> Mock.Tx -> IO ()
handleTxSubmission tinfo tx = do
    topoE <- readTopologyFile (topologyFile tinfo)
    case topoE of
         Left e  -> error e
         Right t ->
             case M.lookup (node tinfo) (toNetworkMap t) of
                  Nothing -> error "Target node not found."
                  Just n -> case role n of
                                 CoreNode -> submitTx (node tinfo) tx
                                 _ -> error "The target node is not a core one."


submitTx :: NodeId -> Mock.Tx -> IO ()
submitTx n tx = do
    withTxPipe n WriteMode False $ \hdl -> do
        let x = error "submitTx: this handle wasn't supposed to be used"
        runProtocolWithPipe x hdl proto
    putStrLn $ "The Id for this transaction is: " <> condense (H.hash @MD5 tx)
  where
      proto :: Protocol Mock.Tx Void ()
      proto = sendMsg tx

readIncomingTx :: Mock.HasUtxo (Payload pt)
               => MVar IO (Mempool Mock.Tx)
               -> TVar IO (ChainProducerState (Payload pt))
               -> Protocol Void Mock.Tx ()
readIncomingTx poolVar chainVar = do
    newTx <- recvMsg
    liftIO $ do
        chain <- chainState <$> atomically (readTVar chainVar)
        modifyMVar_ poolVar $ \mempool -> do
            isConsistent <- runExceptT $ consistent (Mock.utxo chain) mempool newTx
            case isConsistent of
                Left err -> do
                    liftIO $ print err
                    return mempool
                Right ()  -> return $ mempoolInsert newTx mempool
        threadDelay 1000
    -- Loop over
    readIncomingTx poolVar chainVar

instance Serialise Void where
    encode = error "You cannot encode Void."
    decode = error "You cannot decode Void."

spawnMempoolListener :: Mock.HasUtxo (Payload pt)
                     => NodeId
                     -> MVar IO (Mempool Mock.Tx)
                     -> TVar IO (ChainProducerState (Payload pt))
                     -> IO (Async.Async ())
spawnMempoolListener myNodeId poolVar chainVar = do
    Async.async $ do
        -- Apparently I have to pass 'ReadWriteMode' here, otherwise the
        -- node will die prematurely with a (DeserialiseFailure 0 "end of input")
        -- error.
        withTxPipe myNodeId ReadWriteMode True $ \hdl -> do
            let x = error "spawnMempoolListener: this handle shouldn't have been used"
            runProtocolWithPipe hdl x (readIncomingTx poolVar chainVar)
