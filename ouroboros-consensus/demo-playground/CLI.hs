module CLI (
    CLI(..)
  , TopologyInfo(..)
  , Command(..)
  , Protocol(..)
  , fromProtocol
  , parseCLI
  -- * Handy re-exports
  , execParser
  , info
  , (<**>)
  , helper
  , fullDesc
  , progDesc
  ) where

import           Data.Foldable (asum)
import           Data.Semigroup ((<>))
import           Options.Applicative

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node (NodeId (..))
import           Ouroboros.Consensus.Util

import           Mock.TxSubmission (command', parseMockTx)
import           Topology (TopologyInfo (..))

-- Needed to read genesis config
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import           Cardano.Prelude (identity, panic, runExceptT)
import           Formatting (build, sformat)


data CLI = CLI {
    systemStart  :: SystemStart
  , slotDuration :: SlotLength
  , command      :: Command
  }

data Command =
    SimpleNode  TopologyInfo Protocol
  | TxSubmitter TopologyInfo Mock.Tx

data Protocol =
    BFT
  | Praos
  | MockPBFT
  | RealPBFT FilePath

fromProtocol :: Protocol -> IO (Some DemoProtocol)
fromProtocol BFT =
    return $ Some $ DemoBFT defaultSecurityParam
fromProtocol Praos =
    return $ Some $ DemoPraos defaultDemoPraosParams
fromProtocol MockPBFT =
    return $ Some $ DemoMockPBFT (defaultDemoPBftParams genesisConfig)
  where
    -- TODO: This is nasty
    genesisConfig = error "genesis config not needed when using mock ledger"
fromProtocol (RealPBFT fp) = do
    genesisConfig <- readMainetCfg
    return $ Some $ DemoMockPBFT (defaultDemoPBftParams genesisConfig)

parseCLI :: Parser CLI
parseCLI = CLI
    <$> parseSystemStart
    <*> parseSlotDuration
    <*> parseCommand

parseSystemStart :: Parser SystemStart
parseSystemStart = option (SystemStart <$> auto) $ mconcat [
      long "system-start"
    , help "The start time of the system (e.g. \"2018-12-10 15:58:06\""
    ]

parseSlotDuration :: Parser SlotLength
parseSlotDuration = option (mkSlotLength <$> auto) $ mconcat [
      long "slot-duration"
    , value (mkSlotLength 5)
    , help "The slot duration (seconds)"
    ]
  where
    mkSlotLength :: Integer -> SlotLength
    mkSlotLength = slotLengthFromMillisec . (* 1000)

parseProtocol :: Parser Protocol
parseProtocol = asum [
      flag' BFT $ mconcat [
          long "bft"
        , help "Use the BFT consensus algorithm"
        ]
    , flag' Praos $ mconcat [
          long "praos"
        , help "Use the Praos consensus algorithm"
        ]
    , flag' MockPBFT $ mconcat [
          long "mock-pbft"
        , help "Use the Permissive BFT consensus algorithm using a mock ledger"
        ]
    , (flag' RealPBFT $ mconcat [
          long "real-pbft"
        , help "Use the Permissive BFT consensus algorithm using the real ledger"
        ]) <*> (argument str $ mconcat [
            help "Path to JSON file with the genesis configuration"
          , metavar "FILE"
          ])
    ]

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
    command' "node" "Run a node." $
      SimpleNode <$> parseTopologyInfo <*> parseProtocol
  , command' "submit" "Submit a transaction." $
      TxSubmitter <$> parseTopologyInfo <*> parseMockTx
  ]

parseNodeId :: Parser NodeId
parseNodeId =
    option (fmap CoreId auto) (
            long "node-id"
         <> short 'n'
         <> metavar "NODE-ID"
         <> help "The ID for this node"
    )

parseTopologyFile :: Parser FilePath
parseTopologyFile =
    strOption (
            long "topology"
         <> short 't'
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
    )

parseTopologyInfo :: Parser TopologyInfo
parseTopologyInfo = TopologyInfo <$> parseNodeId <*> parseTopologyFile

{-------------------------------------------------------------------------------
  Read genesis

  Copied from Test.Cardano.Chain.Config (not exported)
  TODO: Export it (or use it from someplace else)
-------------------------------------------------------------------------------}

readMainetCfg :: IO Genesis.Config
readMainetCfg =
  either
      (panic . sformat build)
      identity
    <$> runExceptT
          (Genesis.mkConfigFromFile RequiresNoMagic "mainnet-genesis.json" Nothing)
