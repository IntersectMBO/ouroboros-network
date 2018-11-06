{-# LANGUAGE FlexibleContexts #-}
module Mock.TxSubmission (
      command'
    , parseMockTx
    -- * Decorating consumers
    , addMempool
    ) where

import           Control.Concurrent (MVar, modifyMVar_)
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Data.Set as Set
import           Data.String
import           Options.Applicative

import           Ouroboros.Consensus.Infra.Crypto.Hash
import           Ouroboros.Consensus.UTxO.Mempool (Mempool, consistent,
                     mempoolInsert)
import qualified Ouroboros.Consensus.UTxO.Mock as Mock
import           Ouroboros.Network.Chain (Chain)
import           Ouroboros.Network.ProtocolInterfaces (ConsumerHandlers (..))

import           Debug.Trace

{-------------------------------------------------------------------------------
  Parsers for the mock UTxO model
-------------------------------------------------------------------------------}

parseMockTx :: Parser Mock.Tx
parseMockTx = mkTx
    <$> many parseMockTxIn
    <*> many parseMockTxOut
  where
    mkTx :: [Mock.TxIn] -> [Mock.TxOut] -> Mock.Tx
    mkTx ins = traceShow ins . Mock.Tx (Set.fromList ins)

-- A simple mock 'Hash' where the user is requested to input only the last
-- hexadecimal digit.
readSimpleMockHash :: ReadM (Hash Mock.Tx)
readSimpleMockHash = fromString . mappend (replicate 31 '0') <$> str

parseMockTxIn :: Parser Mock.TxIn
parseMockTxIn = (,)
    <$> option readSimpleMockHash (mconcat [
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


handleTxSubmission :: Mock.Tx -> IO ()
handleTxSubmission tx = traceShow "here" (print tx)

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

>>>>>>> 659f679... Add the 'addMempool' decorator
