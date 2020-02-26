{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import           Control.Monad.Except
import           Data.Coerce
import           Data.Foldable (asum)
import           Data.IORef
import           Data.List (foldl', intercalate)
import           Data.Proxy (Proxy (..))
import           Data.Word
import           Options.Applicative

import           Cardano.Binary (unAnnotated)
import qualified Cardano.Chain.Block as Chain
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochSlots (..))
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as Chain
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..),
                     genesisPoint)

import           Ouroboros.Consensus.BlockchainTime.Mock (fixedBlockchainTime)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB.API (BlockComponent (..),
                     StreamFrom (..), StreamTo (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB hiding
                     (withImmDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
                     (withImmDB)
import           Ouroboros.Consensus.Storage.Common (EpochNo (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmDB

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock, ByronHash)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import           Ouroboros.Consensus.Byron.Node

main :: IO ()
main = do
    CmdLine{..}   <- getCmdLine
    genesisConfig <- openGenesis clConfig clIsMainNet
    let epochSlots = Genesis.configEpochSlots genesisConfig
        chunkInfo  = simpleChunkInfo (coerce epochSlots)
        cfg        = mkByronTopLevelConfig genesisConfig
    withRegistry $ \registry ->
      withImmDB clImmDB cfg chunkInfo registry $ \immDB -> do
        runAnalysis clAnalysis immDB chunkInfo registry
        putStrLn "Done"

{-------------------------------------------------------------------------------
  Run the requested analysis
-------------------------------------------------------------------------------}

data AnalysisName =
    ShowSlotBlockNo
  | CountTxOutputs

type Analysis = ImmDB IO ByronBlock
             -> ChunkInfo
             -> ResourceRegistry IO
             -> IO ()

runAnalysis :: AnalysisName -> Analysis
runAnalysis ShowSlotBlockNo = showSlotBlockNo
runAnalysis CountTxOutputs  = countTxOutputs

{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: Analysis
showSlotBlockNo immDB _epochInfo rr =
    processAll immDB rr go
  where
    go :: ByronBlock -> IO ()
    go blk = putStrLn $ intercalate "\t" [
        show (blockNo   blk)
      , show (blockSlot blk)
      ]

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs :: Analysis
countTxOutputs immDB chunkInfo rr = do
    cumulative <- newIORef 0
    processAll immDB rr (go cumulative)
  where
    go :: IORef Int -> ByronBlock -> IO ()
    go cumulative blk = case blk of
      Byron.ByronBlock (Chain.ABOBBlock regularBlk) _ _ ->
        go' cumulative (blockSlot blk) regularBlk
      _otherwise ->
        return () -- Skip EBBs

    go' :: IORef Int -> SlotNo -> Chain.ABlock a -> IO ()
    go' cumulative slotNo Chain.ABlock{..} = do
        countCum  <- atomicModifyIORef cumulative $ \c ->
                       let c' = c + count in (c', c')
        let epochSlot = relativeSlotNo chunkInfo slotNo
        putStrLn $ intercalate "\t" [
            show slotNo
          , show epochSlot
          , show count
          , show countCum
          ]
      where
        Chain.AHeader{..} = blockHeader
        Chain.ABody{..}   = blockBody

        count :: Int
        count = countTxPayload bodyTxPayload

    countTxPayload :: Chain.ATxPayload a -> Int
    countTxPayload = sum'
                   . map (countTx . unAnnotated . Chain.aTaTx)
                   . Chain.aUnTxPayload

    countTx :: Chain.Tx -> Int
    countTx = length . Chain.txOutputs

    sum' :: [Int] -> Int
    sum' = foldl' (+) 0

-- | Convert 'SlotNo' to relative 'EpochSlot'
--
-- NOTE: Unlike 'epochInfoBlockRelative', which puts the EBB at relative slot 0,
-- this puts the first real block at relative slot 0.
relativeSlotNo :: ChunkInfo -> SlotNo -> (EpochNo, Word64)
relativeSlotNo chunkInfo (SlotNo absSlot) =
    let epoch        = epochInfoEpoch chunkInfo (SlotNo absSlot)
        SlotNo first = epochInfoFirst chunkInfo epoch
    in (epoch, absSlot - first)

{-------------------------------------------------------------------------------
  Auxiliary: processing all blocks in the imm DB
-------------------------------------------------------------------------------}

processAll :: ImmDB IO ByronBlock
           -> ResourceRegistry IO
           -> (ByronBlock -> IO ())
           -> IO ()
processAll immDB rr callback = do
    tipPoint <- getPointAtTip immDB
    Right itr <- stream immDB rr GetBlock
      (StreamFromExclusive genesisPoint)
      (StreamToInclusive tipPoint)
    go itr
  where
    go :: Iterator ByronHash IO (IO ByronBlock) -> IO ()
    go itr = do
        itrResult <- ImmDB.iteratorNext itr
        case itrResult of
          IteratorExhausted   -> return ()
          IteratorResult mblk -> mblk >>= \blk -> callback blk >> go itr

{-------------------------------------------------------------------------------
  Command line args
-------------------------------------------------------------------------------}

data CmdLine = CmdLine {
      clConfig    :: FilePath
    , clIsMainNet :: Bool
    , clImmDB     :: FilePath
    , clAnalysis  :: AnalysisName
    }

parseCmdLine :: Parser CmdLine
parseCmdLine = CmdLine
    <$> (strOption $ mconcat [
            long "config"
          , help "Path to config file"
          , metavar "PATH"
          ])
    <*> (flag True False $ mconcat [
            long "testnet"
          , help "The DB contains blocks from testnet rather than mainnet"
          ])
    <*> (strOption $ mconcat [
            long "db"
          , help "Path to the chain DB (parent of \"immutable\" directory)"
          , metavar "PATH"
          ])
    <*> parseAnalysis

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
    ]

getCmdLine :: IO CmdLine
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Simple framework for running analysis over the immutable DB"
        ])

{-------------------------------------------------------------------------------
  Genesis config
-------------------------------------------------------------------------------}

openGenesis :: FilePath -> Bool -> IO Genesis.Config
openGenesis configFile onMainNet = do
    Right genesisHash <- runExceptT $
      snd <$> Genesis.readGenesisData configFile
    Right genesisConfig <- runExceptT $
      Genesis.mkConfigFromFile
        (if onMainNet -- transactions on testnet include magic number
          then Crypto.RequiresNoMagic
          else Crypto.RequiresMagic)
        configFile
        (Genesis.unGenesisHash genesisHash)
    return genesisConfig

{-------------------------------------------------------------------------------
  Top-level configuration
-------------------------------------------------------------------------------}

mkByronTopLevelConfig :: Genesis.Config -> TopLevelConfig ByronBlock
mkByronTopLevelConfig genesisConfig = pInfoConfig $
    protocolInfoByron
      genesisConfig
      Nothing
      (Update.ProtocolVersion 1 0 0)
      (Update.SoftwareVersion (Update.ApplicationName "db-analyse") 2)
      Nothing

{-------------------------------------------------------------------------------
  Interface with the ImmDB
-------------------------------------------------------------------------------}

withImmDB :: FilePath
          -> TopLevelConfig ByronBlock
          -> ChunkInfo
          -> ResourceRegistry IO
          -> (ImmDB IO ByronBlock -> IO a)
          -> IO a
withImmDB fp cfg chunkInfo registry = ImmDB.withImmDB args
  where
    args :: ImmDbArgs IO ByronBlock
    args = (defaultArgs fp) {
          immDecodeHash     = nodeDecodeHeaderHash    (Proxy @ByronBlock)
        , immDecodeBlock    = nodeDecodeBlock         cfg
        , immDecodeHeader   = nodeDecodeHeader        cfg SerialisedToDisk
        , immEncodeHash     = nodeEncodeHeaderHash    (Proxy @ByronBlock)
        , immEncodeBlock    = nodeEncodeBlockWithInfo cfg
        , immChunkInfo      = chunkInfo
        , immHashInfo       = nodeHashInfo            (Proxy @ByronBlock)
        , immValidation     = ValidateMostRecentEpoch
        , immIsEBB          = nodeIsEBB
        , immCheckIntegrity = nodeCheckIntegrity      cfg
        , immAddHdrEnv      = nodeAddHeaderEnvelope   (Proxy @ByronBlock)
        , immRegistry       = registry
          -- We don't want to truncate blocks from the future
        , immBlockchainTime = fixedBlockchainTime maxBound
        }
