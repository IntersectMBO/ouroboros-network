module CLI (
    CLI(..)
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
  , payloadType  :: PayloadType
  , txs          :: Maybe Mock.Tx
  -- ^ A 'Tx' to submit, if any.
  }

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
      <*> (option (readPayloadType <$> str) (
               long "payload-type"
            <> short 'p'
            <> metavar allPayloadTypes
            <> value DummyPayloadType
            <> help "The content of the payload in the messages"
            ))
      <*> optional (subparser (
          command' "submit" "Submit a transaction" parseMockTx
      ))
