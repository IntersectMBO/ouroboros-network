{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.ImmDBServer.Diffusion (run) where

import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Contravariant ((>$<))
import           Data.Void (Void)
import           Network.Socket (SockAddr (..))

import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux
import qualified Ouroboros.Network.NodeToNode as N2N
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Socket (configureSocket)

import           Cardano.Tools.ImmDBServer.MiniProtocols (immDBServer)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Node.InitStorage
                     (NodeInitStorage (nodeCheckIntegrity, nodeImmutableDbChunkInfo))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Storage.FS.API (SomeHasFS (..))
import           Ouroboros.Consensus.Storage.FS.API.Types
                     (MountPoint (MountPoint))
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

serve ::
     SockAddr
  -> N2N.Versions N2N.NodeToNodeVersion N2N.NodeToNodeVersionData
       (OuroborosApplication 'ResponderMode SockAddr BL.ByteString IO Void ())
  -> IO Void
serve sockAddr application = withIOManager \iocp -> do
    let sn     = Snocket.socketSnocket iocp
        family = Snocket.addrFamily sn sockAddr
    bracket (Snocket.open sn family) (Snocket.close sn) \socket -> do
      networkMutableState <- N2N.newNetworkMutableState
      configureSocket socket (Just sockAddr)
      Snocket.bind sn socket sockAddr
      Snocket.listen sn socket
      N2N.withServer
        sn
        N2N.nullNetworkServerTracers {
          N2N.nstHandshakeTracer   = show >$< stdoutTracer
        , N2N.nstErrorPolicyTracer = show >$< stdoutTracer
        }
        networkMutableState
        acceptedConnectionsLimit
        socket
        application
        nullErrorPolicies
  where
    acceptedConnectionsLimit = N2N.AcceptedConnectionsLimit {..}
      where
        acceptedConnectionsHardLimit = maxBound
        acceptedConnectionsSoftLimit = maxBound
        acceptedConnectionsDelay     = 0

run ::
     forall blk.
     ( GetPrevHash blk
     , ShowProxy blk
     , SupportedNetworkProtocolVersion blk
     , SerialiseNodeToNodeConstraints blk
     , ImmutableDB.ImmutableDbSerialiseConstraints blk
     , NodeInitStorage blk
     , ConfigSupportsNode blk
     )
  => FilePath
  -> SockAddr
  -> TopLevelConfig blk
  -> IO Void
run immDBDir sockAddr cfg =
    withRegistry \registry ->
    ImmutableDB.withDB
      (ImmutableDB.openDB (immDBArgs registry) runWithTempRegistry)
      \immDB ->
        serve sockAddr $ immDBServer codecCfg immDB networkMagic
  where
    immDBArgs immRegistry = ImmutableDB.ImmutableDbArgs {..}
      where
        ImmutableDB.ImmutableDbArgs {
            immCacheConfig
          , immHasFS
          , immValidationPolicy
          , immTracer
          } = ImmutableDB.defaultArgs $
            SomeHasFS $ ioHasFS $ MountPoint immDBDir

        immCheckIntegrity   = nodeCheckIntegrity storageCfg
        immChunkInfo        = nodeImmutableDbChunkInfo storageCfg
        immCodecConfig      = codecCfg

    codecCfg     = configCodec cfg
    storageCfg   = configStorage cfg
    networkMagic = getNetworkMagic . configBlock $ cfg
