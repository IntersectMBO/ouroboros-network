{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Database analyse tool.
module Main (main) where

import           Data.Foldable (asum)
import           Options.Applicative

import           Control.Tracer (contramap, debugTracer, nullTracer)

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import qualified Ouroboros.Consensus.Node as Node
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Util.IOLike (atomically)
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (fromChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
import qualified Ouroboros.Consensus.Storage.VolatileDB.Types as VolDB

import           Analysis
import           Block.Byron (ByronBlockArgs)
import           Block.Cardano (Args (..), CardanoBlockArgs)
import           Block.Shelley (ShelleyBlockArgs)
import           HasAnalysis

main :: IO ()
main = do
    cmdLine <- getCmdLine
    case blockType cmdLine of
      ByronBlock   args -> analyse cmdLine args
      ShelleyBlock args -> analyse cmdLine args
      CardanoBlock args -> analyse cmdLine args

data CmdLine = CmdLine {
    dbDir      :: FilePath
  , verbose    :: Bool
  , onlyImmDB  :: Bool
  , validation :: Maybe ValidateBlocks
  , blockType  :: BlockType
  , analysis   :: AnalysisName
  }

data ValidateBlocks = ValidateAllBlocks | MinimumBlockValidation

data BlockType =
    ByronBlock   ByronBlockArgs
  | ShelleyBlock ShelleyBlockArgs
  | CardanoBlock CardanoBlockArgs

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseCmdLine :: Parser CmdLine
parseCmdLine = CmdLine
    <$> strOption (mconcat [
            long "db"
          , help "Path to the Chain DB"
          , metavar "PATH"
          ])
    <*> switch (mconcat [
            long "verbose"
          , help "Enable verbose logging"
          ])
    <*> switch (mconcat [
            long "onlyImmDB"
          , help "Validate only the Immutable DB (e.g. do not do ledger validation)"
          ])
    <*> parseValidationPolicy
    <*> blockTypeParser
    <*> parseAnalysis

parseValidationPolicy :: Parser (Maybe ValidateBlocks)
parseValidationPolicy = parseMaybe $ asum [
      flag' ValidateAllBlocks $ mconcat [
          long "validate-all-blocks"
        , help "Validate all blocks of the Volatile and Immutable DB"
        ]
    , flag' MinimumBlockValidation $ mconcat [
          long "minimum-block-validation"
        , help "Validate a minimum part of the Volatile and Immutable DB"
        ]
    ]

parseAnalysis :: Parser AnalysisName
parseAnalysis = asum [
      flag' ShowSlotBlockNo $ mconcat [
          long "show-slot-block-no"
        , help "Show slot and block number of all blocks"
        ]
    , flag' CountTxOutputs $ mconcat [
          long "count-tx-outputs"
        , help "Show number of transaction outputs per block"
        ]
    , flag' ShowBlockHeaderSize $ mconcat [
          long "show-block-header-size"
        , help "Show the header sizes of all blocks"
        ]
    , flag' ShowBlockTxsSize $ mconcat [
          long "show-block-txs-size"
        , help "Show the total transaction sizes per block"
        ]
    , flag' ShowEBBs $ mconcat [
          long "show-ebbs"
        , help "Show all EBBs and their predecessors"
        ]
    , pure OnlyValidation
    ]

blockTypeParser :: Parser BlockType
blockTypeParser = subparser $ mconcat
  [ command "byron"
      (info (parseByronType   <**> helper) (progDesc "Analyse a Byron-only DB"))
  , command "shelley"
      (info (parseShelleyType <**> helper) (progDesc "Analyse a Shelley-only DB"))
  , command "cardano"
      (info (parseCardanoType <**> helper) (progDesc "Analyse a Cardano DB"))
  ]

parseByronType :: Parser BlockType
parseByronType = ByronBlock <$> argsParser Proxy

parseShelleyType :: Parser BlockType
parseShelleyType = ShelleyBlock <$> argsParser Proxy

parseCardanoType :: Parser BlockType
parseCardanoType = CardanoBlock <$> argsParser Proxy

parseMaybe ::  Parser a -> Parser (Maybe a)
parseMaybe parser = asum [Just <$> parser, pure Nothing]

getCmdLine :: IO CmdLine
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Simple framework used to analyse a Chain DB"
        ])

{-------------------------------------------------------------------------------
  Analyse
-------------------------------------------------------------------------------}

analyse
  :: (Node.RunNode blk, Show (Header blk), HasAnalysis blk)
  => CmdLine
  -> Args blk
  -> IO ()
analyse CmdLine {..} args =
    withRegistry $ \registry -> do
      ProtocolInfo { pInfoInitLedger = initLedger, pInfoConfig = cfg } <-
        mkProtocolInfo args
      let chunkInfo  = Node.nodeImmDbChunkInfo cfg
          args' = Node.mkChainDbArgs tracer registry InFuture.dontCheck
                    dbDir cfg initLedger chunkInfo
          chainDbArgs = args' {
              ChainDB.cdbImmValidation = immValidationPolicy
            , ChainDB.cdbVolValidation = volValidationPolicy
            }
          (immDbArgs, _, _, _) = fromChainDbArgs chainDbArgs
      if onlyImmDB then
        ImmDB.withImmDB immDbArgs $ \immDB -> do
          runAnalysis analysis cfg (Left immDB) registry
          immDbTipPoint <- ImmDB.getPointAtTip immDB
          putStrLn $ "ImmDB tip: " ++ show immDbTipPoint
      else
        ChainDB.withDB chainDbArgs $ \chainDB -> do
          runAnalysis analysis cfg (Right chainDB) registry
          chainDbTipPoint <- atomically $ ChainDB.getTipPoint chainDB
          putStrLn $ "ChainDB tip: " ++ show chainDbTipPoint
  where
    tracer
      | verbose   = contramap show debugTracer
      | otherwise = nullTracer

    immValidationPolicy = case (analysis, validation) of
      (_, Just ValidateAllBlocks)      -> ImmDB.ValidateAllChunks
      (_, Just MinimumBlockValidation) -> ImmDB.ValidateMostRecentChunk
      (OnlyValidation, _ )             -> ImmDB.ValidateAllChunks
      _                                -> ImmDB.ValidateMostRecentChunk

    volValidationPolicy = case (analysis, validation) of
      (_, Just ValidateAllBlocks)      -> VolDB.ValidateAll
      (_, Just MinimumBlockValidation) -> VolDB.NoValidation
      (OnlyValidation, _ )             -> VolDB.ValidateAll
      _                                -> VolDB.NoValidation
