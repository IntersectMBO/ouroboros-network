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
          (  long "local-socket"
          <> metavar "FILENAME"
          <> help "Unix socket for node-to-client communication"
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
  where
    mkConfiguration ipv4 ipv6 portNumber localAddress configFile topologyFile =
      mempty { dmqcIPv4         = Last (Just <$> ipv4),
               dmqcIPv6         = Last (Just <$> ipv6),
               dmqcLocalAddress = Last (LocalAddress <$> localAddress),
               dmqcPortNumber   = Last portNumber,
               dmqcConfigFile   = Last configFile,
               dmqcTopologyFile = Last topologyFile
             }


