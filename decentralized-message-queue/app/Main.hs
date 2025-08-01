{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (void)
import Control.Tracer (Tracer (..), traceWith)

import Data.Act
import Data.Aeson (ToJSON)
import Data.Void (Void)
import Debug.Trace (traceShowM)
import Options.Applicative
import System.Random (newStdGen, split)

import DMQ.Configuration
import DMQ.Configuration.CLIOptions (parseCLIOptions)
import DMQ.Configuration.Topology (readTopologyFileOrError)
import DMQ.Diffusion.Applications (diffusionApplications)
import DMQ.Diffusion.Arguments
import DMQ.Diffusion.NodeKernel (withNodeKernel)
import DMQ.NodeToNode (dmqCodecs, dmqLimitsAndTimeouts, ntnApps)
import DMQ.Tracer

import DMQ.Diffusion.PeerSelection (policy)
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress,
           encodeRemoteAddress)

main :: IO ()
main = void . runDMQ =<< execParser opts
  where
    opts = info (parseCLIOptions <**> helper)
                ( fullDesc
                <> progDesc "Run the POC DMQ node"
                )

runDMQ :: PartialConfig -> IO Void
runDMQ commandLineConfig = do
    -- get the configuration file path
    let configFilePath = unI
                       $ dmqcConfigFile commandLineConfig
                   `act` dmqcConfigFile defaultConfiguration

    -- read & parse configuration file
    config' <- readConfigurationFileOrError configFilePath
    -- combine default configuration, configuration file and command line
    -- options
    let dmqConfig@Configuration {
          dmqcPrettyLog    = I prettyLog,
          dmqcTopologyFile = I topologyFile
        } = config' <> commandLineConfig
            `act`
            defaultConfiguration

    let tracer :: ToJSON ev => Tracer IO (String, ev)
        tracer = dmqTracer prettyLog

    traceWith tracer ("Configuration", dmqConfig)
    nt <- readTopologyFileOrError topologyFile
    traceWith tracer ("NetworkTopology", nt)

    stdGen <- newStdGen
    let (psRng, policyRng) = split stdGen

    withNodeKernel psRng $ \nodeKernel -> do
      dmqDiffusionConfiguration <- mkDiffusionConfiguration dmqConfig nt

      let dmqNtNApps =
            ntnApps nodeKernel
                    (dmqCodecs
                              -- TODO: `maxBound :: Cardano.Network.NodeToNode.NodeToNodeVersion`
                              -- is unsafe here!
                               (encodeRemoteAddress maxBound)
                               (decodeRemoteAddress maxBound))
                    dmqLimitsAndTimeouts
                    defaultSigDecisionPolicy
          dmqDiffusionArguments =
            diffusionArguments debugTracer
                               debugTracer
          dmqDiffusionApplications =
            diffusionApplications nodeKernel
                                  dmqConfig
                                  dmqDiffusionConfiguration
                                  dmqLimitsAndTimeouts
                                  dmqNtNApps
                                  (policy policyRng)

      Diffusion.run dmqDiffusionArguments
                    (dmqDiffusionTracers dmqConfig tracer)
                    dmqDiffusionConfiguration
                    dmqDiffusionApplications

debugTracer :: (Show a, Applicative m) => Tracer m a
debugTracer = Tracer traceShowM
