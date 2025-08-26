{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import Control.Monad (void)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTimer
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
import DMQ.Diffusion.NodeKernel
import DMQ.NodeToClient qualified as NtC
import DMQ.NodeToNode (dmqCodecs, dmqLimitsAndTimeouts, ntnApps)
import DMQ.Protocol.SigSubmission.Codec
import DMQ.Protocol.SigSubmission.Type (Sig (..))
import DMQ.Tracer

import DMQ.Diffusion.PeerSelection (policy)
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress,
           encodeRemoteAddress)
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool

-- TODO tidy up
import Ouroboros.Network.Socket
import Ouroboros.Network.Snocket
import Cardano.Network.NodeToClient
import Control.Monad.Class.MonadThrow
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import Ouroboros.Network.Protocol.LocalStateQuery.Client
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Network.Mux qualified as Mx
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Network.NodeToClient
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Data.Map qualified as Map
import Data.Proxy
import Ouroboros.Network.Block
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Magic
import Cardano.Crypto.ProtocolMagic
import Cardano.Chain.Genesis
import Cardano.Chain.Slotting
import Ouroboros.Consensus.Shelley.Ledger.Query
import Control.Concurrent.Class.MonadSTM.Strict

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
        diffusionTracers = dmqDiffusionTracers dmqConfig tracer

    Diffusion.withIOManager \iocp -> do
      let localSnocket' = localSnocket iocp
          snocketPath   = "cardano-node.socket" -- TODO path

      withNodeKernel psRng $ \nodeKernel -> do
        dmqDiffusionConfiguration <- mkDiffusionConfiguration dmqConfig nt

        let stakePoolMonitor =
              do
                connectTo
                 localSnocket'
                 debuggingNetworkConnectTracers --nullNetworkConnectTracers
                 (combineVersions
                   [ simpleSingletonVersions
                       version
                       NodeToClientVersionData {
                           networkMagic =
                             NetworkMagic
                               . unProtocolMagicId
                               $ mainnetProtocolMagicId
                         , query = False
                       }
                       \_version ->
                         (Mx.OuroborosApplication [
                            Mx.MiniProtocol {
                             miniProtocolNum    = Mx.MiniProtocolNum 7,
                             miniProtocolStart  = Mx.StartEagerly,
                             miniProtocolLimits = Mx.MiniProtocolLimits {
                               maximumIngressQueue = 0xffffffff
                             },
                             miniProtocolRun =
                               Mx.InitiatorProtocolOnly .
                                 Mx.mkMiniProtocolCbFromPeerSt . const $
                                   (nullTracer, cStateQueryCodec, StateIdle, localStateQueryClientPeer $ client (poolIds nodeKernel))
                            }
                            ])
                   | version <- [minBound..maxBound]
                   , let supportedVersionMap = supportedNodeToClientVersions (Proxy :: Proxy (CardanoBlock StandardCrypto))
                         blk = supportedVersionMap Map.! version
                         Codecs {cStateQueryCodec} =
                           clientCodecs (pClientInfoCodecConfig . protocolClientInfoCardano $ EpochSlots 21600) blk version
                   ])
                 snocketPath

        withAsync stakePoolMonitor \aid -> do
          link aid
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
                    maxMsgs = 1000 -- TODO: make this negotiated in the handshake?
                    mempoolReader = Mempool.getReader sigId sigSize (mempool nodeKernel)
                    mempoolWriter = Mempool.getWriter sigId (const $ pure True) (mempool nodeKernel)
                 in NtC.ntcApps () mempoolReader mempoolWriter maxMsgs
                                (NtC.dmqCodecs encodeSig decodeSig)
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


-- TODO use this to get stake pool data into the kernel
-- expand
client
  :: forall block query point crypto m. (MonadDelay m, MonadSTM m, MonadThrow m)
  => (block ~ CardanoBlock crypto, query ~ Query block, point ~ Point block)
  => PoolIds m
  -> LocalStateQueryClient (CardanoBlock crypto) (Point block) (Query block) m Void
client PoolIds { getPoolIds } = go
  where
    go = LocalStateQueryClient .
           pure $ SendMsgAcquire ImmutableTip acquiring

    acquiring :: ClientStAcquiring block point query m Void
    acquiring = ClientStAcquiring {
       recvMsgAcquired =
         pure . SendMsgQuery (BlockQuery . QueryIfCurrentShelley $ GetStakePools) $ ClientStQuerying \cardanoQueryResult-> do
           case cardanoQueryResult of
             Left mismatcherainfo -> throwIO . userError $ "mismatch era info"
             Right res -> do
               atomically $ writeTVar getPoolIds res
               pure $ SendMsgRelease do
                 threadDelay 86400
                 runLocalStateQueryClient go
      , recvMsgFailure = error "recvMsgFailure not implemented"
      }
