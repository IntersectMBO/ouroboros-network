{-# LANGUAGE FlexibleContexts #-}
module Mock.TxSubmission (
      command'
    , parseMockTx
    , handleTxSubmission
    ) where

import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Builder as B
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Options.Applicative
import           System.IO (hFlush)

import qualified Ouroboros.Consensus.UTxO.Mock as Mock
import           Ouroboros.Network.Node (NodeId (..))
import           Ouroboros.Network.Serialise

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
                  Just n -> case role n of
                                 CoreNode -> submitTx (node tinfo) tx
                                 _ -> error "The target node is not a core one."


submitTx :: NodeId -> Mock.Tx -> IO ()
submitTx n tx =
    withTxPipe n False $ \hdl -> do
        B.hPutBuilder hdl (CBOR.toBuilder (encode tx))
        hFlush hdl

{-
-- | Adds a 'Mempool' to a consumer.
addMempool :: Mock.HasUtxo block
           => MVar (Mempool Mock.Tx)
           -> TVar (Chain block)
           -> ConsumerHandlers block IO
           -> ConsumerHandlers block IO
addMempool poolVar chainVar c = ConsumerHandlers {
          getChainPoints = getChainPoints c

        , addBlock = \b -> do
            chain <- atomically $ readTVar chainVar
            modifyMVar_ poolVar $ \mempool -> do
              let updateMempool tx pool =
                    let isConsistent = runExceptT $ consistent (Mock.utxo chain) mempool tx
                    in case runIdentity isConsistent of
                         Left _err -> pool
                         Right ()  -> mempoolInsert tx mempool
              return $ foldr updateMempool mempool (Mock.confirmed b)
            addBlock c b

        , rollbackTo = \p -> do
            rollbackTo c p
        }
-}
