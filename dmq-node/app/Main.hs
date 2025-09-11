{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

module Main where

import Control.Monad (void)
import Control.Tracer (Tracer (..), nullTracer, traceWith)

import Data.Act
import Data.Aeson (ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Void (Void)
import Options.Applicative
import System.Random (newStdGen, split)

import Cardano.KESAgent.Protocols.StandardCrypto (StandardCrypto)
import Cardano.Ledger.Keys (VKey (..))
import Cardano.Ledger.Hashes (hashKey)

import DMQ.Configuration
import DMQ.Configuration.CLIOptions (parseCLIOptions)
import DMQ.Configuration.Topology (readTopologyFileOrError)
import DMQ.Diffusion.Applications (diffusionApplications)
import DMQ.Diffusion.Arguments
import DMQ.Diffusion.NodeKernel
import DMQ.NodeToClient qualified as NtC
import DMQ.NodeToNode (dmqCodecs, dmqLimitsAndTimeouts, ntnApps)
import DMQ.Protocol.LocalMsgSubmission.Codec
import DMQ.Protocol.SigSubmission.Type (Sig (..))
import DMQ.Tracer

import DMQ.Diffusion.PeerSelection (policy)
import DMQ.NodeToClient.LocalStateQueryClient
import DMQ.Protocol.SigSubmission.Validate
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress,
           encodeRemoteAddress)
import Ouroboros.Network.SizeInBytes
import Ouroboros.Network.Snocket
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
          dmqcLocalHandshakeTracer = I localHandshakeTracer,
          dmqcCardanoNodeSocket    = I snocketPath
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
        diffusionTracers = dmqDiffusionTracers dmqConfig tracer

    Diffusion.withIOManager \iocp -> do
      let localSnocket'      = localSnocket iocp
          mkStakePoolMonitor = connectToCardanoNode tracer localSnocket' snocketPath

      withNodeKernel @StandardCrypto psRng mkStakePoolMonitor \nodeKernel -> do
        dmqDiffusionConfiguration <- mkDiffusionConfiguration dmqConfig nt

        let sigSize :: Sig StandardCrypto -> SizeInBytes
            sigSize _ = 0 -- TODO
            mempoolReader = Mempool.getReader sigId sigSize (mempool nodeKernel)
            dmqNtNApps =
              let ntnMempoolWriter = Mempool.writerAdapter $
                    Mempool.getWriter sigId
                                      (poolValidationCtx $ stakePools nodeKernel)
                                      (validateSig FailDefault (hashKey . VKey))
                                      SigDuplicate
                                      (mempool nodeKernel)
               in ntnApps tracer
                          dmqConfig
                          mempoolReader
                          ntnMempoolWriter
                          sigSize
                          nodeKernel
                          (dmqCodecs
                                    -- TODO: `maxBound :: Cardano.Network.NodeToNode.NodeToNodeVersion`
                                    -- is unsafe here!
                                     (encodeRemoteAddress maxBound)
                                     (decodeRemoteAddress maxBound))
                          dmqLimitsAndTimeouts
                          defaultSigDecisionPolicy
            dmqNtCApps =
              let maxMsgs = 1000 -- TODO: make this negotiated in the handshake?
                  ntcMempoolWriter =
                    Mempool.getWriter sigId
                                      (poolValidationCtx $ stakePools nodeKernel)
                                      (validateSig FailSoft (hashKey . VKey))
                                      SigDuplicate
                                      (mempool nodeKernel)
               in NtC.ntcApps mempoolReader ntcMempoolWriter maxMsgs
                              (NtC.dmqCodecs encodeReject decodeReject)
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
                      diffusionTracers
                      dmqDiffusionConfiguration
                      dmqDiffusionApplications
