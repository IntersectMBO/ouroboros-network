{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Control.Monad (void, when)
import Control.Tracer (Tracer (..), nullTracer, traceWith)

import Data.Act
import Data.Aeson (ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Data.Void (Void)
import Options.Applicative
import System.Exit (exitSuccess)
import System.Random (newStdGen, split)

import Cardano.Git.Rev (gitRev)
import Cardano.KESAgent.Protocols.StandardCrypto (StandardCrypto)

import DMQ.Configuration
import DMQ.Configuration.CLIOptions (parseCLIOptions)
import DMQ.Configuration.Topology (readTopologyFileOrError)
import DMQ.Diffusion.Applications (diffusionApplications)
import DMQ.Diffusion.Arguments
import DMQ.Diffusion.NodeKernel (mempool, withNodeKernel)
import DMQ.Handlers.TopLevel (toplevelExceptionHandler)
import DMQ.NodeToClient qualified as NtC
import DMQ.NodeToNode (NodeToNodeVersion, dmqCodecs, dmqLimitsAndTimeouts,
           ntnApps)
import DMQ.Protocol.LocalMsgSubmission.Codec
import DMQ.Protocol.SigSubmission.Type (Sig (..))
import DMQ.Tracer

import DMQ.Diffusion.PeerSelection (policy)
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress,
           encodeRemoteAddress)
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool

import Paths_dmq_node qualified as Meta

main :: IO ()
main = toplevelExceptionHandler $ void . runDMQ =<< execParser opts
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
          dmqcVersion              = I version
        } = config' <> commandLineConfig
            `act`
            defaultConfiguration
    let tracer :: ToJSON ev => Tracer IO (WithEventType ev)
        tracer = dmqTracer prettyLog

    when version $ do
      let gitrev = $(gitRev)
          cleanGitRev = if | Text.take 6 (Text.drop 7 gitrev) == "-dirty"
                           -- short dirty revision
                           -> Just $ Text.take (6 + 7) gitrev
                           | Text.all (== '0') gitrev
                           -- no git revision available
                           -> Nothing
                           | otherwise
                           -> Just gitrev
      Text.putStr $ Text.unlines $
        [ "dmq-node version: " <> Text.pack (showVersion Meta.version) ]
        ++
        [ "git revision: " <> rev
        | rev <- maybeToList cleanGitRev
        ]
      exitSuccess

    traceWith tracer (WithEventType "Configuration" dmqConfig)
    nt <- readTopologyFileOrError topologyFile
    traceWith tracer (WithEventType "NetworkTopology" nt)

    stdGen <- newStdGen
    let (psRng, policyRng) = split stdGen

    withNodeKernel @StandardCrypto tracer dmqConfig psRng $ \nodeKernel -> do
      dmqDiffusionConfiguration <- mkDiffusionConfiguration dmqConfig nt

      let dmqNtNApps =
            ntnApps tracer
                    dmqConfig
                    nodeKernel
                    (dmqCodecs
                              -- TODO: `maxBound :: Cardano.Network.NodeToNode.NodeToNodeVersion`
                              -- is unsafe here!
                                (encodeRemoteAddress (maxBound :: NodeToNodeVersion))
                                (decodeRemoteAddress (maxBound :: NodeToNodeVersion)))
                    dmqLimitsAndTimeouts
                    defaultSigDecisionPolicy
          dmqNtCApps =
            let sigSize _ = 0 -- TODO
                maxMsgs = 1000 -- TODO: make this dynamic?
                mempoolReader = Mempool.getReader sigId sigSize (mempool nodeKernel)
                mempoolWriter = Mempool.getWriter sigId (pure ())
                                                        (\_ _ -> Right () :: Either Void ())
                                                        (\_ -> True)
                                                        (mempool nodeKernel)
             in NtC.ntcApps tracer dmqConfig
                            mempoolReader mempoolWriter maxMsgs
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
                    (dmqDiffusionTracers dmqConfig tracer)
                    dmqDiffusionConfiguration
                    dmqDiffusionApplications
