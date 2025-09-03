module DMQ.Configuration.CLIOptions (parseCLIOptions) where

import Data.Monoid (Last (..))
import Options.Applicative

import DMQ.Configuration

parseCLIOptions :: Parser PartialConfig
parseCLIOptions =
  mkConfiguration
    <$> optional (
          option auto
          (  long "host-addr"
          <> metavar "IPv4"
          <> help "IPv4 that the node will bind to"
          )
        )
    <*> optional (
          option auto
           (  long "host-ipv6-addr"
           <> metavar "IPv6"
           <> help "IPv6 that the node will bind to"
           )
        )
    <*> optional (
          option auto
          (  long "port"
          <> short 'p'
          <> metavar "Port Number"
          <> help "Port Number that the node will bind to"
          )
        )
    <*> optional (
          strOption
          (  long "configuration-file"
          <> short 'c'
          <> metavar "FILENAME"
          <> help "Configuration file for DMQ Node"
          )
        )
    <*> optional (
          strOption
          (  long "topology-file"
          <> short 't'
          <> metavar "FILENAME"
          <> help "Topology file for DMQ Node"
          )
        )
    <*> optional (
          strOption
          (   long "cardano-node-socket"
          <> metavar "Cardano node socket path"
          <> help "Used for local connections to Cardano node"
          )
        )
  where
    mkConfiguration ipv4 ipv6 portNumber configFile topologyFile cardanoNodeSocket =
      mempty { dmqcIPv4 = Last (Just <$> ipv4),
               dmqcIPv6 = Last (Just <$> ipv6),
               dmqcPortNumber = Last portNumber,
               dmqcConfigFile = Last configFile,
               dmqcTopologyFile = Last topologyFile,
               dmqcCardanoNodeSocket = Last cardanoNodeSocket
             }
