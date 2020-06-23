{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main (main) where

import           Control.Monad.Except
import qualified Data.ByteString as BS
import           Data.Either (fromRight)
import           Data.Foldable (asum)
import           Data.IORef
import           Data.List (intercalate)
import           Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import           Options.Applicative
import           System.FilePath ((</>))

import           Cardano.Slotting.Slot

import           Cardano.Crypto.ProtocolMagic

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash,
                     genesisPoint)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node.DbMarker
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation (SizeInBytes)
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.ChainDB.API (BlockComponent (..),
                     StreamFrom (..), StreamTo (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB hiding
                     (withImmDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
                     (withImmDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmDB

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..))
import           Ouroboros.Consensus.Shelley.Protocol.Crypto
                     (TPraosStandardCrypto)

import           Analysis (HasAnalysis)
import qualified Analysis

main :: IO ()
main = do
    cmdLine@CmdLine{..} <- getCmdLine
    case clBlockType of
      Just Byron   -> analyse cmdLine (Proxy @ByronBlock)
      Just Shelley -> analyse cmdLine (Proxy @(ShelleyBlock TPraosStandardCrypto))
      Just Cardano -> analyse cmdLine (Proxy @(CardanoBlock TPraosStandardCrypto))
      Nothing -> do
        -- check the dbmarker of the db if the block type is not specified.
        protocolMagicId <- readDBMarker clImmDB
        case unProtocolMagicId protocolMagicId of
          764824073  -> analyse cmdLine (Proxy @ByronBlock)
          1097911063 -> analyse cmdLine (Proxy @ByronBlock)
          42         -> analyse cmdLine (Proxy @(ShelleyBlock TPraosStandardCrypto))
          _          -> error $ "unsupported protocolMagicId: " ++ show protocolMagicId

readDBMarker :: FilePath -> IO ProtocolMagicId
readDBMarker dbPath = do
    bs <- BS.readFile markerPath
    protocolMagicId <- runExceptT $ dbMarkerParse markerPath bs
    return $ fromRight
      (error "failed to parse protocolMagicId from db Marker file")
      protocolMagicId
  where
    markerPath = dbPath </> Text.unpack dbMarkerFile


analyse :: forall blk. (RunNode blk, HasAnalysis blk)
        => CmdLine -> Proxy blk -> IO ()
analyse CmdLine{..} _ = do
    cfg :: TopLevelConfig blk <- Analysis.mkTopLevelConfig clConfig clIsMainNet
    withRegistry $ \registry ->
      withImmDB clImmDB cfg (nodeImmDbChunkInfo cfg) registry $ \immDB -> do
        runAnalysis clAnalysis immDB registry
        putStrLn "Done"

{-------------------------------------------------------------------------------
  Run the requested analysis
-------------------------------------------------------------------------------}

data AnalysisName =
    ShowSlotBlockNo
  | CountTxOutputs
  | ShowBlockHeaderSize
  | ShowBlockTxsSize
  deriving Show

type Analysis blk = ImmDB IO blk
                 -> ResourceRegistry IO
                 -> IO ()

runAnalysis :: (HasAnalysis blk, RunNode blk)
            => AnalysisName -> Analysis blk
runAnalysis ShowSlotBlockNo     = showSlotBlockNo
runAnalysis CountTxOutputs      = countTxOutputs
runAnalysis ShowBlockHeaderSize = showBlockHeaderSize
runAnalysis ShowBlockTxsSize    = showBlockTxsSize

{-------------------------------------------------------------------------------
  Analysis: show block and slot number for all blocks
-------------------------------------------------------------------------------}

showSlotBlockNo :: forall blk. (HasHeader blk, ImmDbSerialiseConstraints blk)
                => Analysis blk
showSlotBlockNo immDB rr =
    processAll immDB rr go
  where
    go :: blk -> IO ()
    go blk = putStrLn $ intercalate "\t" [
        show (blockNo   blk)
      , show (blockSlot blk)
      ]

{-------------------------------------------------------------------------------
  Analysis: show total number of tx outputs per block
-------------------------------------------------------------------------------}

countTxOutputs
  :: forall blk. (HasHeader blk, HasAnalysis blk, ImmDbSerialiseConstraints blk)
  => Analysis blk
countTxOutputs immDB rr = do
    cumulative <- newIORef 0
    processAll immDB rr (go cumulative)
  where
    go :: IORef Int -> blk -> IO ()
    go cumulative blk = do
        countCum  <- atomicModifyIORef cumulative $ \c ->
                       let c' = c + count in (c', c')
        putStrLn $ intercalate "\t" [
            show slotNo
          , show count
          , show countCum
          ]
      where
        count = Analysis.countTxOutputs blk
        slotNo = blockSlot blk

{-------------------------------------------------------------------------------
  Analysis: show the block header size in bytes for all blocks
-------------------------------------------------------------------------------}

showBlockHeaderSize
  :: forall blk. (HasAnalysis blk, HasHeader blk, ImmDbSerialiseConstraints blk)
  => Analysis blk
showBlockHeaderSize immDB rr = do
    maxBlockHeaderSizeRef <- newIORef 0
    processAll immDB rr (go maxBlockHeaderSizeRef)
    maxBlockHeaderSize <- readIORef maxBlockHeaderSizeRef
    putStrLn ("Maximum encountered block header size = " <> show maxBlockHeaderSize)
  where
    go :: IORef SizeInBytes -> blk -> IO ()
    go maxBlockHeaderSizeRef blk = do
        void $ modifyIORef' maxBlockHeaderSizeRef (max blockHdrSz)
        putStrLn $ intercalate "\t" [
            show slotNo
          , "Block header size = " <> show blockHdrSz
          ]
      where
        slotNo = blockSlot blk
        blockHdrSz = Analysis.blockHeaderSize blk
{-------------------------------------------------------------------------------
  Analysis: show the total transaction sizes in bytes per block
-------------------------------------------------------------------------------}

showBlockTxsSize
  :: forall blk. (HasHeader blk, HasAnalysis blk, ImmDbSerialiseConstraints blk)
  => Analysis blk
showBlockTxsSize immDB rr = processAll immDB rr process
  where
    process :: blk -> IO ()
    process blk = putStrLn $ intercalate "\t" [
          show slotNo
        , "Num txs in block = " <> show numBlockTxs
        , "Total size of txs in block = " <> show blockTxsSize
        ]
      where
        txSizes :: [SizeInBytes]
        txSizes = Analysis.blockTxSizes blk

        numBlockTxs :: Int
        numBlockTxs = length txSizes

        blockTxsSize :: SizeInBytes
        blockTxsSize = sum txSizes

        slotNo = blockSlot blk

{-------------------------------------------------------------------------------
  Auxiliary: processing all blocks in the imm DB
-------------------------------------------------------------------------------}

processAll :: forall blk. (HasHeader blk, ImmDbSerialiseConstraints blk)
           => ImmDB IO blk
           -> ResourceRegistry IO
           -> (blk -> IO ())
           -> IO ()
processAll immDB rr callback = do
    tipPoint <- getPointAtTip immDB
    case pointToWithOriginRealPoint tipPoint of
      Origin -> return ()
      At tip -> do
        Right itr <- stream immDB rr GetBlock
          (StreamFromExclusive genesisPoint)
          (StreamToInclusive tip)
        go itr
  where
    go :: Iterator (HeaderHash blk) IO (IO blk) -> IO ()
    go itr = do
        itrResult <- ImmDB.iteratorNext itr
        case itrResult of
          IteratorExhausted   -> return ()
          IteratorResult mblk -> mblk >>= \blk -> callback blk >> go itr

{-------------------------------------------------------------------------------
  Command line args
-------------------------------------------------------------------------------}

data CmdLine = CmdLine {
      clConfig    :: [FilePath]
    , clIsMainNet :: Bool
    , clImmDB     :: FilePath
    , clBlockType :: Maybe BlockType
    , clAnalysis  :: AnalysisName
    } deriving Show

data BlockType = Byron | Shelley | Cardano
  deriving Show

parseCmdLine :: Parser CmdLine
parseCmdLine = CmdLine
    <$> many (strOption (mconcat [
            long "config"
          , help "Path to config file or files. Multiple occurences of the\
                 \option can be used"
          , metavar "PATH"
          ]))
    <*> flag True False (mconcat [
            long "testnet"
          , help "The DB contains blocks from testnet rather than mainnet"
          ])
    <*> strOption (mconcat [
            long "db"
          , help "Path to the chain DB (parent of \"immutable\" directory)"
          , metavar "PATH"
          ])
    <*> parseBlockType
    <*> parseAnalysis

parseBlockType :: Parser (Maybe BlockType)
parseBlockType = asum [
      flag' (Just Byron) $ mconcat [
          long "byron"
        , help "A Byron network"
        ]
    , flag' (Just Shelley) $ mconcat [
          long "shelley"
        , help "A Shelley network"
        ]
    , flag' (Just Cardano) $ mconcat [
          long "cardano"
        , help "A Byron-to-Shelley network"
        ]
    , pure Nothing
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
    ]

getCmdLine :: IO CmdLine
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Simple framework for running analysis over the immutable DB"
        ])

{-------------------------------------------------------------------------------
  Interface with the ImmDB
-------------------------------------------------------------------------------}

withImmDB :: forall blk a.
             RunNode blk
          => FilePath
          -> TopLevelConfig blk
          -> ChunkInfo
          -> ResourceRegistry IO
          -> (ImmDB IO blk -> IO a)
          -> IO a
withImmDB fp cfg chunkInfo registry = ImmDB.withImmDB args
  where
    args :: ImmDbArgs IO blk
    args = (defaultArgs fp) {
          immGetBinaryBlockInfo = nodeGetBinaryBlockInfo
        , immCodecConfig        = getCodecConfig $ configBlock cfg
        , immChunkInfo          = chunkInfo
        , immValidation         = ValidateMostRecentChunk
        , immCheckIntegrity     = nodeCheckIntegrity cfg
        , immRegistry           = registry
        }
