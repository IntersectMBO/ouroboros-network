{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Mock.TxSubmission (
      command'
    , parseMockTx
    , handleTxSubmission
    , spawnMempoolListener
    ) where

import           Codec.Serialise (hPutSerialise)
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Except
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Options.Applicative
import           System.IO (IOMode (..))

import           Ouroboros.Consensus.Crypto.Hash (ShortHash)
import qualified Ouroboros.Consensus.Crypto.Hash as H
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node (NodeId (..), NodeKernel (getChainDB))
import           Ouroboros.Consensus.Util.CBOR (Decoder (..), initDecoderIO)
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Storage.ChainDB (getCurrentLedger)

import           Mock.Mempool (Mempool (..), consistent, mempoolInsert)
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


handleTxSubmission :: TopologyInfo -> Mock.Tx -> IO ()
handleTxSubmission tinfo tx = do
    topoE <- readTopologyFile (topologyFile tinfo)
    case topoE of
         Left e  -> error e
         Right t ->
             case M.lookup (node tinfo) (toNetworkMap t) of
                  Nothing -> error "Target node not found."
                  Just _  -> submitTx (node tinfo) tx

submitTx :: NodeId -> Mock.Tx -> IO ()
submitTx n tx = do
    withTxPipe n WriteMode False $ \h -> hPutSerialise h tx
    putStrLn $ "The Id for this transaction is: " <> condense (H.hash @ShortHash tx)

readIncomingTx :: TVar IO (Mempool Mock.Tx)
               -> NodeKernel IO NodeId (Mock.SimpleBlock p c) (Mock.SimpleHeader p c)
               -> Decoder IO
               -> IO ()
readIncomingTx poolVar kernel Decoder{..} = forever $ do
    newTx <- decodeNext
    liftIO $ atomically $ do
        l <- getCurrentLedger $ getChainDB kernel
        mempool <- readTVar poolVar
        isConsistent <- runExceptT $ consistent (Mock.slsUtxo . ledgerState $ l) mempool newTx
        case isConsistent of
            Left _err -> return ()
            Right ()  -> writeTVar poolVar (mempoolInsert newTx mempool)
    liftIO $ threadDelay 1000

spawnMempoolListener :: NodeId
                     -> TVar IO (Mempool Mock.Tx)
                     -> NodeKernel IO NodeId (Mock.SimpleBlock p c) (Mock.SimpleHeader p c)
                     -> IO (Async.Async ())
spawnMempoolListener myNodeId poolVar kernel = do
    Async.async $ do
        -- Apparently I have to pass 'ReadWriteMode' here, otherwise the
        -- node will die prematurely with a (DeserialiseFailure 0 "end of input")
        -- error.
        withTxPipe myNodeId ReadWriteMode True $ \h -> do
            let getChunk = BS.hGetSome h 1024
            readIncomingTx poolVar kernel =<< initDecoderIO getChunk
