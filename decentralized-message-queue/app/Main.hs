{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (void)
import Control.Tracer (Tracer (..), nullTracer, traceWith)

import Data.Act
import Data.Aeson (ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Void (Void)
import Options.Applicative
import System.Random (newStdGen, split)

import DMQ.Configuration
import DMQ.Configuration.CLIOptions (parseCLIOptions)
import DMQ.Configuration.Topology (readTopologyFileOrError)
import DMQ.Diffusion.Applications (diffusionApplications)
import DMQ.Diffusion.Arguments
import DMQ.Diffusion.NodeKernel (mempool, withNodeKernel)
import DMQ.NodeToClient qualified as NtC
import DMQ.NodeToNode (dmqCodecs, dmqLimitsAndTimeouts, ntnApps)
import DMQ.Protocol.LocalMsgSubmission.Codec
import DMQ.Protocol.SigSubmission.Codec
import DMQ.Protocol.SigSubmission.Type (Sig (..))
import DMQ.Tracer

import DMQ.Diffusion.PeerSelection (policy)
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress,
           encodeRemoteAddress)
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool

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
          dmqcPrettyLog            = I prettyLog,
          dmqcTopologyFile         = I topologyFile,
          dmqcHandshakeTracer      = I handshakeTracer,
          dmqcLocalHandshakeTracer = I localHandshakeTracer
        } = config' <> commandLineConfig
            `act`
            defaultConfiguration

    let tracer :: ToJSON ev => Tracer IO (WithEventType ev)
        tracer = dmqTracer prettyLog

    traceWith tracer (WithEventType "Configuration" dmqConfig)
    nt <- readTopologyFileOrError topologyFile
    traceWith tracer (WithEventType "NetworkTopology" nt)

    stdGen <- newStdGen
    let (psRng, policyRng) = split stdGen

    withNodeKernel psRng $ \nodeKernel -> do
      dmqDiffusionConfiguration <- mkDiffusionConfiguration dmqConfig nt

      let dmqNtNApps =
            ntnApps tracer
                    dmqConfig
                    nodeKernel
                    (dmqCodecs
                              -- TODO: `maxBound :: Cardano.Network.NodeToNode.NodeToNodeVersion`
                              -- is unsafe here!
                               (encodeRemoteAddress maxBound)
                               (decodeRemoteAddress maxBound))
                    dmqLimitsAndTimeouts
                    defaultSigDecisionPolicy
          dmqNtCApps =
            let sigSize _ = 0 -- TODO
                maxMsgs = 1000 -- TODO: make this dynamic?
                mempoolReader = Mempool.getReader sigId sigSize (mempool nodeKernel)
                mempoolWriter = Mempool.getWriter sigId (const True) (mempool nodeKernel)
             in NtC.ntcApps mempoolReader mempoolWriter maxMsgs
                            (NtC.dmqCodecs encodeSig decodeSig encodeReject decodeReject)
          dmqDiffusionArguments =
            diffusionArguments (if handshakeTracer
                                  then WithEventType "Handshake" >$< tracer
                                  else nullTracer)
                               (if localHandshakeTracer
                                  then WithEventType "Handshake" >$< tracer
                                  else nullTracer)
          dmqDiffusionApplications =
            diffusionApplications nodeKernel
                                  dmqConfig
                                  dmqDiffusionConfiguration
                                  dmqLimitsAndTimeouts
                                  dmqNtNApps
                                  dmqNtCApps
                                  (policy policyRng)

      Diffusion.run dmqDiffusionArguments
                    (dmqDiffusionTracers dmqConfig tracer)
                    dmqDiffusionConfiguration
                    dmqDiffusionApplications
