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
import           Options.Applicative

import qualified Ouroboros.Consensus.UTxO.Mock as Mock
import           Ouroboros.Network.Node (NodeId (..))

import           Mock.TxSubmission (command', parseMockTx)
import           Payload (PayloadType (..), allPayloadTypes, readPayloadType)
import           Topology (TopologyInfo (..))

data CLI = CLI
  { command      :: Command
  }

data Command =
    SimpleNode  TopologyInfo
  | TxSubmitter TopologyInfo Mock.Tx

parseCLI :: Parser CLI
parseCLI = CLI <$> parseCommand

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

_parsePayloadType :: Parser PayloadType
_parsePayloadType =
    option (readPayloadType <$> str) (
               long "payload-type"
            <> short 'p'
            <> metavar allPayloadTypes
            <> value DummyPayloadType
            <> help "The content of the payload in the messages"
    )
