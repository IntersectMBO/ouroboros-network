module CLI (
    CLI(..)
  , TopologyInfo(..)
  , Command(..)
  , parseCLI
  -- * Handy re-exports
  , execParser
  , info
  , (<**>)
  , helper
  , fullDesc
  , progDesc
  ) where

import           Data.Semigroup ((<>))
import           Data.Time
import           Options.Applicative

import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Network.Node (NodeId (..))

import           Mock.TxSubmission (command', parseMockTx)
import           Topology (TopologyInfo (..))

data CLI = CLI {
    systemStart  :: UTCTime
  , slotDuration :: Double
  , command      :: Command
  }

data Command =
    SimpleNode  TopologyInfo
  | TxSubmitter TopologyInfo Mock.Tx

parseCLI :: Parser CLI
parseCLI = CLI <$> parseSystemStart <*> parseSlotDuration <*> parseCommand

parseSystemStart :: Parser UTCTime
parseSystemStart = option auto (long "system-start" <>
                                help "The start time of the system (e.g. \"2018-12-10 15:58:06\""
                               )

parseSlotDuration :: Parser Double
parseSlotDuration = option auto (long "slot-duration" <>
                                 value 5.0 <>
                                 help "The slot duration (seconds)"
                                )

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
    command' "node" "Run a node." $
      SimpleNode <$> parseTopologyInfo
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
