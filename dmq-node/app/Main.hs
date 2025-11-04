{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

module Main where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (throwIO)
import Control.Monad (void, when)
import Control.Tracer (Tracer (..), nullTracer, traceWith)

import Data.Act
import Data.Aeson (ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Data.Void (Void)
import Options.Applicative
import System.Exit (exitSuccess)
import System.Random (newStdGen, split)

import Cardano.Git.Rev (gitRev)
import Cardano.KESAgent.KES.Evolution qualified as KES
import Cardano.KESAgent.Protocols.StandardCrypto (StandardCrypto)
import Cardano.Ledger.Keys (VKey (..))
import Cardano.Ledger.Hashes (hashKey)

import DMQ.Configuration
import DMQ.Configuration.CLIOptions (parseCLIOptions)
import DMQ.Configuration.Topology (readTopologyFileOrError)
import DMQ.Diffusion.Applications (diffusionApplications)
import DMQ.Diffusion.Arguments
import DMQ.Diffusion.NodeKernel
import DMQ.Handlers.TopLevel (toplevelExceptionHandler)
import DMQ.NodeToClient qualified as NtC
import DMQ.NodeToNode (NodeToNodeVersion, dmqCodecs, dmqLimitsAndTimeouts,
           ntnApps)
import DMQ.Protocol.LocalMsgSubmission.Codec
import DMQ.Protocol.SigSubmission.Type (Sig (..))
import DMQ.Tracer

import DMQ.Diffusion.PeerSelection (policy)
import DMQ.NodeToClient.LocalStateQueryClient
import DMQ.Protocol.SigSubmission.Validate
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress,
           encodeRemoteAddress)
import Ouroboros.Network.SizeInBytes
import Ouroboros.Network.Snocket
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
          dmqcShelleyGenesisFile   = I genesisFile,
          dmqcHandshakeTracer      = I handshakeTracer,
          dmqcLocalHandshakeTracer = I localHandshakeTracer,
          dmqcCardanoNodeSocket    = I snocketPath,
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

    res <- KES.evolutionConfigFromGenesisFile genesisFile
    evolutionConfig <- case res of
      Left err -> traceWith tracer (WithEventType "ShelleyGenesisFile" err)
               >> throwIO (userError err)
      Right ev -> return ev

    traceWith tracer (WithEventType "Configuration" dmqConfig)
    nt <- readTopologyFileOrError topologyFile
    traceWith tracer (WithEventType "NetworkTopology" nt)

    stdGen <- newStdGen
    let (psRng, policyRng) = split stdGen

    Diffusion.withIOManager \iocp -> do
      let localSnocket'      = localSnocket iocp
          mkStakePoolMonitor = connectToCardanoNode tracer localSnocket' snocketPath

      withNodeKernel @StandardCrypto
                     tracer
                     dmqConfig
                     evolutionConfig
                     psRng
                     mkStakePoolMonitor $ \nodeKernel -> do
        dmqDiffusionConfiguration <-
          mkDiffusionConfiguration dmqConfig nt nodeKernel.stakePools.ledgerBigPeersVar

        let sigSize :: Sig StandardCrypto -> SizeInBytes
            sigSize _ = 0 -- TODO
            mempoolReader = Mempool.getReader sigId sigSize (mempool nodeKernel)
            dmqNtNApps =
              let ntnMempoolWriter = Mempool.writerAdapter $
                    Mempool.getWriter sigId
                                      (poolValidationCtx $ stakePools nodeKernel)
                                      (validateSig evolutionConfig (hashKey . VKey))
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
                                   (encodeRemoteAddress (maxBound @NodeToNodeVersion))
                                   (decodeRemoteAddress (maxBound @NodeToNodeVersion)))
                          dmqLimitsAndTimeouts
                          defaultSigDecisionPolicy
            dmqNtCApps =
              let maxMsgs = 1000 -- TODO: make this negotiated in the handshake?
                  ntcMempoolWriter =
                    Mempool.getWriter sigId
                                      (poolValidationCtx $ stakePools nodeKernel)
                                      (validateSig (hashKey . VKey))
                                      SigDuplicate
                                      (mempool nodeKernel)
               in NtC.ntcApps tracer dmqConfig
                              mempoolReader ntcMempoolWriter maxMsgs
                              (NtC.dmqCodecs encodeReject decodeReject)
            dmqDiffusionArguments =
              diffusionArguments (if handshakeTracer
                                    then WithEventType "Handshake" >$< tracer
                                    else nullTracer)
                                 (if localHandshakeTracer
                                    then WithEventType "Handshake" >$< tracer
                                    else nullTracer)
                                 $ maybe [] out <$> tryReadTMVar nodeKernel.stakePools.ledgerPeersVar
              where
                out :: LedgerPeerSnapshot AllLedgerPeers
                    -> [(PoolStake, NonEmpty LedgerRelayAccessPoint)]
                out (LedgerAllPeerSnapshotV23 _pt _magic relays) = relays

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
