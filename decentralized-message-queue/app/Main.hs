{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (IOException)
import Control.Monad (void)
import Control.Tracer (Tracer (..))
import Data.Void (Void)
import Debug.Trace (traceShowM)
import Options.Applicative
import System.Random (newStdGen, split)

import DMQ.Configuration (mkDiffusionConfiguration,
           readConfigurationFileOrError)
import DMQ.Configuration.CLIOptions (CLIOptions (..), parseCLIOptions)
import DMQ.Configuration.Topology (readTopologyFileOrError)
import DMQ.Diffusion.Applications (diffusionApplications)
import DMQ.Diffusion.Arguments (diffusionArguments)
import DMQ.Diffusion.NodeKernel (newNodeKernel)
import DMQ.NodeToNode (dmqCodecs, dmqLimitsAndTimeouts, mapNtNDMQtoOuroboros,
           ntnApps)

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

runDMQ :: CLIOptions -> IO Void
runDMQ cliopts@CLIOptions {
         configFile
       , topologyFile
       } = do

  dmqConfig <- readConfigurationFileOrError configFile
  nt <- readTopologyFileOrError @() @() topologyFile

  stdGen <- newStdGen
  let (psRng, policyRng) = split stdGen

  nodeKernel <- newNodeKernel psRng

  dmqDiffusionConfiguration <- mkDiffusionConfiguration cliopts nt dmqConfig

  let dmqNtNApps =
        ntnApps nodeKernel
                (dmqCodecs (encodeRemoteAddress (mapNtNDMQtoOuroboros maxBound))
                           (decodeRemoteAddress (mapNtNDMQtoOuroboros maxBound)))
                dmqLimitsAndTimeouts
      dmqDiffusionArguments =
        diffusionArguments @_ @IOException
                           debugTracer
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
                , Show extraFlags
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
                                 extraFlags extraPeers extraCounters m
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
