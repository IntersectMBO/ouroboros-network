{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (void)
import Control.Tracer (Tracer (..))

import Data.Act
import Data.Void (Void)
import Debug.Trace (traceShowM)
import Options.Applicative
import System.Random (newStdGen, split)

import DMQ.Configuration
import DMQ.Configuration.CLIOptions (parseCLIOptions)
import DMQ.Configuration.Topology (readTopologyFileOrError)
import DMQ.Diffusion.Applications (diffusionApplications)
import DMQ.Diffusion.Arguments (diffusionArguments)
import DMQ.Diffusion.NodeKernel (withNodeKernel)
import DMQ.NodeToNode (dmqCodecs, dmqLimitsAndTimeouts, ntnApps)

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
          dmqcTopologyFile = I topologyFile
        } = config' <> commandLineConfig
            `act`
            defaultConfiguration

    print dmqConfig
    nt <- readTopologyFileOrError topologyFile

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
                    debugTracers
                    dmqDiffusionConfiguration
                    dmqDiffusionApplications

debugTracer :: (Show a, Applicative m) => Tracer m a
debugTracer = Tracer traceShowM

debugTracers :: ( Applicative m
                , Ord ntnAddr
                , Show extraCounters
                , Show extraDebugState
                , Show extraPeers
                , Show extraState
                , Show ntcAddr
                , Show ntcVersion
                , Show ntcVersionData
                , Show ntnAddr
                , Show ntnVersion
                , Show ntnVersionData
                )
            => Diffusion.Tracers ntnAddr ntnVersion ntnVersionData
                                 ntcAddr ntcVersion ntcVersionData
                                 extraState extraDebugState
                                 NoExtraFlags extraPeers extraCounters m
debugTracers =
  Diffusion.Tracers {
    Diffusion.dtBearerTracer                               = debugTracer
  , Diffusion.dtChannelTracer                              = debugTracer
  , Diffusion.dtMuxTracer                                  = debugTracer
  , Diffusion.dtHandshakeTracer                            = debugTracer
  , Diffusion.dtLocalBearerTracer                          = debugTracer
  , Diffusion.dtLocalChannelTracer                         = debugTracer
  , Diffusion.dtLocalMuxTracer                             = debugTracer
  , Diffusion.dtLocalHandshakeTracer                       = debugTracer
  , Diffusion.dtDiffusionTracer                            = debugTracer
  , Diffusion.dtTraceLocalRootPeersTracer                  = debugTracer
  , Diffusion.dtTracePublicRootPeersTracer                 = debugTracer
  , Diffusion.dtTraceLedgerPeersTracer                     = debugTracer
  , Diffusion.dtTracePeerSelectionTracer                   = debugTracer
  , Diffusion.dtTraceChurnCounters                         = debugTracer
  , Diffusion.dtDebugPeerSelectionInitiatorTracer          = debugTracer
  , Diffusion.dtDebugPeerSelectionInitiatorResponderTracer = debugTracer
  , Diffusion.dtTracePeerSelectionCounters                 = debugTracer
  , Diffusion.dtPeerSelectionActionsTracer                 = debugTracer
  , Diffusion.dtConnectionManagerTracer                    = debugTracer
  , Diffusion.dtConnectionManagerTransitionTracer          = debugTracer
  , Diffusion.dtServerTracer                               = debugTracer
  , Diffusion.dtInboundGovernorTracer                      = debugTracer
  , Diffusion.dtInboundGovernorTransitionTracer            = debugTracer
  , Diffusion.dtLocalConnectionManagerTracer               = debugTracer
  , Diffusion.dtLocalServerTracer                          = debugTracer
  , Diffusion.dtLocalInboundGovernorTracer                 = debugTracer
  , Diffusion.dtDnsTracer                                  = debugTracer
  }
