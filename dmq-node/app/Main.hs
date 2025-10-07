{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE PackageImports      #-}

module Main where

import Control.Monad (void, when)
import "contra-tracer" Control.Tracer (traceWith)

import Data.Act
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
import DMQ.Tracer (
    mkCardanoTracer
  , WithEventType (WithEventType), EventType (DMQ)
  )
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
          dmqcTopologyFile         = I topologyFile,
          dmqcShelleyGenesisFile   = I genesisFile,
          dmqcVersion              = I version
        } = config' <> commandLineConfig
            `act`
            defaultConfiguration

    (tracer, dmqDiffusionTracers) <- mkCardanoTracer configFilePath

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
          "dmq-node version: " <> Text.pack (showVersion Meta.version)
        : [ "git revision: " <> rev
          | rev <- maybeToList cleanGitRev
          ]
      exitSuccess

    traceWith tracer (WithEventType (DMQ "Configuration") dmqConfig)
    res <- KES.evolutionConfigFromGenesisFile genesisFile
    evolutionConfig <- case res of
      Left err -> traceWith tracer (WithEventType (DMQ "ShelleyGenesisFile") err)
               >> throwIO (userError $ err)
      Right ev -> return ev

    nt <- readTopologyFileOrError topologyFile
    traceWith tracer (WithEventType (DMQ "NetworkTopology") nt)

    stdGen <- newStdGen
    let (psRng, policyRng) = split stdGen

    withNodeKernel @StandardCrypto
                   tracer
                   dmqConfig
                   psRng $ \nodeKernel -> do
      dmqDiffusionConfiguration <- mkDiffusionConfiguration dmqConfig nt

      let dmqNtNApps =
            ntnApps tracer
                    dmqConfig
                    nodeKernel
                    (dmqCodecs
                       (encodeRemoteAddress (maxBound @NodeToNodeVersion))
                       (decodeRemoteAddress (maxBound @NodeToNodeVersion)))
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
            diffusionArguments (WithEventType (DMQ "Handshake") >$< tracer)
                               (WithEventType (DMQ "LocalHandshake") >$< tracer)
          dmqDiffusionApplications =
            diffusionApplications nodeKernel
                                  dmqConfig
                                  dmqDiffusionConfiguration
                                  dmqLimitsAndTimeouts
                                  dmqNtNApps
                                  dmqNtCApps
                                  (policy policyRng)

      Diffusion.run dmqDiffusionArguments
                    dmqDiffusionTracers
                    dmqDiffusionConfiguration
                    dmqDiffusionApplications
