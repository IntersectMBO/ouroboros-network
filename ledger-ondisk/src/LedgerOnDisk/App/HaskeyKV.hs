-- |

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LedgerOnDisk.App.HaskeyKV(main) where

import Options.Applicative

import qualified Database.Haskey.Store.InMemory as Haskey
import qualified LedgerOnDisk.Haskey.KVHandle as Haskey
import Example.Arbitrary
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Example.Types
import Control.Monad.State.Strict
import LedgerOnDisk.KVHandle.Class
import Example.Rules
import qualified Data.Sequence as Seq
import Data.Foldable
import LedgerOnDisk.Mapping.PTMap
import Control.Lens
import qualified System.Log.FastLogger as FastLogger
import Control.Tracer
import Control.Exception

data Params = Params
  { tickets :: !Int
  , minLag :: !Int
  } deriving Show

-- type LedgerStateHM = LedgerState HashMap
type LedgerStateReadSet = ReadSet LedgerState (Haskey.KVHandle LedgerState)

paramsParserInfo :: ParserInfo Params
paramsParserInfo = info (helper <*> paramsParser) mempty
  where
    paramsParser = let
      go tickets minLag = Params{..}
      parseTickets = option auto (long "tickets" <> help "The maximum number of simultaneous queries" <> value 5)
      parseMinLag = option auto (long "min-lag" <> help "The minimum number of blocks lag between prepare and submit" <> value 2)
      in go <$> parseTickets <*> parseMinLag

generateBlocks :: GenState -> TBQueue Block -> IO ()
generateBlocks gs chan = flip evalStateT gs $ forever $ do
  b <- randomBlock
  liftIO . atomically $ writeTBQueue chan b

prepareBlocks :: Haskey.KVHandle LedgerState -> TBQueue Block -> Chan (Block, LedgerStateReadSet) -> IO ()
prepareBlocks h in_q out_chan = forever $ do
  b <- atomically $ readTBQueue in_q
  rs <- prepare h (ledgerStateBlockKeys b)
  writeChan out_chan (b, rs)

constantDelay :: (String -> IO ()) -> Int -> Chan a -> Chan a -> IO ()
constantDelay trace lag in_chan out_chan = flip evalStateT init_state . forever $ do
  next <- liftIO $ readChan in_chan
  mb_to_send Seq.:<| rest <- get
  for_ mb_to_send $ \to_send -> liftIO $ do
    trace $ "constantDelay: sending"
    writeChan out_chan to_send
  put $ rest Seq.:|> Just next
  where
    init_state = Seq.fromList [Nothing | _ <- [0.. lag -1]]


submitBlocks :: (String -> IO ()) ->LedgerState NullMap -> Haskey.KVHandle LedgerState -> Chan (Block, LedgerStateReadSet) -> IO ()
submitBlocks trace init_state h chan = flip evalStateT init_state .  forever $ do
  (b, rs) <- liftIO $ readChan chan
  liftIO $ trace $ "submitBlocks" <> show b
  s0 <- get
  (m, s) <- liftIO $ submit h rs $ \odm -> let
    ls0 = s0 & onDiskMappingsLens .~ odm
    Just ls1 = applyBlock b ls0
    diff_map = ls1 ^. onDiskMappingsLens . to (mapMappings Haskey.proxyConstraint diffMapFromPTMap)
    m = ls1 ^. onDiskMappingsLens . to (mapMappings Haskey.proxyConstraint mapFromPTMap)
    in ((m, ls1 & onDiskMappingsLens .~ nullMap), diff_map)
  liftIO $ trace $ "submitBlocksReulst: " <> show m
  put s

main :: IO ()
main = do
  params@Params{..} <- execParser paramsParserInfo
  memoryBackend <- Haskey.inMemoryBackend Haskey.defMemoryStoreConfig
  gs <- smallGenState
  let init_ledger_state = LedgerState
        { pparams = PParams (Coin 100)
        , utxos = UTxOState
          { utxo = NullMap
          , utxoagg = NullMap
          }
        }
  blocksChannel <- newTBQueueIO 100

  preparedChannel <- newChan
  delayedChannel <- newChan

  time_cache <- FastLogger.newTimeCache FastLogger.simpleTimeFormat
  r :: Either SomeException () <- try $ FastLogger.withTimedFastLogger time_cache (FastLogger.LogStderr 1024) $ \tfl -> do
    let
      tracer = Tracer $ \trace_haskeykv -> tfl $ \t -> FastLogger.toLogStr t <> ":" <> FastLogger.toLogStr (show trace_haskeykv) <> "\n"
      log_str s = tfl $ \t -> "#" <> FastLogger.toLogStr t <> ":" <> FastLogger.toLogStr s <> "\n"

    log_str $ ("Starting..." :: String)
    log_str $ "Params: " <> show params
    Haskey.withKVHandle tracer tickets memoryBackend "/" $ \h -> do
      withAsync (generateBlocks gs blocksChannel) $ \a -> do
        withAsync (prepareBlocks h blocksChannel preparedChannel) $ \b -> do
          withAsync (constantDelay log_str minLag preparedChannel delayedChannel) $ \c -> do
            submitBlocks log_str init_ledger_state h delayedChannel
            wait c
          wait b
        wait a
  print r
