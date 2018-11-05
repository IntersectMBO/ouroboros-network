module CLI (
    CLI(..)
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

data CLI = CLI
  { myNodeId     :: NodeId
  , topologyFile :: FilePath
  , command      :: Command
  }

data Command =
    SimpleNode PayloadType
  | TxSubmitter Mock.Tx

parseCLI :: Parser CLI
parseCLI = CLI
      <$> option (fmap CoreId auto)
          ( long "node-id"
         <> short 'n'
         <> metavar "NODE-ID"
         <> help "The ID for this node" )
      <*> strOption
         ( long "topology"
         <> short 't'
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
         )
      <*> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
    command' "node" "Run a node." $
      fmap SimpleNode parsePayloadType
  , command' "submit" "Submit a transaction." $
      fmap TxSubmitter parseMockTx
  ]

parsePayloadType :: Parser PayloadType
parsePayloadType =
    option (readPayloadType <$> str) (
               long "payload-type"
            <> short 'p'
            <> metavar allPayloadTypes
            <> value DummyPayloadType
            <> help "The content of the payload in the messages"
    )
