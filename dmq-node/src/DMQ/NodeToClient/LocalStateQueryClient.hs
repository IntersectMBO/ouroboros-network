{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeOperators            #-}

module DMQ.NodeToClient.LocalStateQueryClient
  ( cardanoClient
  , connectToCardanoNode
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Trans.Except
import Control.Tracer (Tracer (..), nullTracer)
import Data.Functor.Contravariant ((>$<))
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.Void

import Cardano.Chain.Genesis
import Cardano.Chain.Slotting
import Cardano.Crypto.ProtocolMagic
import Cardano.Network.NodeToClient
import Cardano.Slotting.EpochInfo.API
import Cardano.Slotting.Time
import DMQ.Diffusion.NodeKernel
import DMQ.Tracer
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.Node
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import Ouroboros.Consensus.HardFork.History.EpochInfo
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Network.NodeToClient
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Shelley.Ledger.Query
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
import Ouroboros.Network.Magic
import Ouroboros.Network.Mux qualified as Mx
import Ouroboros.Network.Protocol.LocalStateQuery.Client
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import Ouroboros.Network.Socket

-- TODO generalize to handle ledger eras other than Conway
-- | connects the dmq node to cardano node via local state query
-- and updates the node kernel with stake pool data necessary to perform message
-- validation
cardanoClient
  :: forall block query point crypto m. (MonadDelay m, MonadSTM m, MonadThrow m, MonadTime m)
  => (block ~ CardanoBlock crypto, query ~ Query block, point ~ Point block)
  => Tracer m String -- TODO: replace string with a proper type
  -> StakePools m
  -> StrictTVar m (Maybe UTCTime) -- ^ from node kernel
  -> LocalStateQueryClient (CardanoBlock crypto) (Point block) (Query block) m Void
cardanoClient _tracer StakePools { stakePoolsVar } nextEpochVar = LocalStateQueryClient (idle Nothing)
  where
    idle mSystemStart = pure $ SendMsgAcquire ImmutableTip acquire
      where
        acquire :: ClientStAcquiring block point query m Void
        acquire = ClientStAcquiring {
          recvMsgAcquired =
            let epochQry systemStart = pure $
                    SendMsgQuery (BlockQuery . QueryIfCurrentConway $ GetEpochNo)
                  $ wrappingMismatch (handleEpoch systemStart)
            in case mSystemStart of
                 Just systemStart -> epochQry systemStart
                 Nothing -> pure $
                   SendMsgQuery GetSystemStart $ ClientStQuerying epochQry

         , recvMsgFailure = \failure ->
             throwIO . userError $ "recvMsgFailure: " <> show failure
         }

    wrappingMismatch k = ClientStQuerying $
      either (const . throwIO . userError $ "mismatch era info") k

    getInterpreter systemStart epoch = ClientStQuerying \interpreter -> do
      let ei  = interpreterToEpochInfo interpreter
          res =
            runExcept do
              lastSlot <- snd <$> epochInfoRange ei epoch
              lastSlotTime <- epochInfoSlotToRelativeTime ei lastSlot
              lastSlotLength <- epochInfoSlotLength ei lastSlot
              pure $ addRelativeTime (getSlotLength lastSlotLength) lastSlotTime

      case res of
        Left _err -> pure $ SendMsgRelease do
          threadDelay 86400 -- TODO fuzz this?
          idle $ Just systemStart
        Right relativeTime -> do
          now <- getCurrentTime
          let nextEpoch   = fromRelativeTime systemStart relativeTime
              toNextEpoch = diffUTCTime nextEpoch now
          if toNextEpoch < 5 then
            pure $ SendMsgRelease do
              threadDelay $ realToFrac toNextEpoch
              idle $ Just systemStart
          else pure $
            SendMsgQuery (BlockQuery . QueryIfCurrentConway $ GetStakeSnapshots Nothing)
            $ wrappingMismatch (handleStakeSnapshots systemStart nextEpoch toNextEpoch)

    handleEpoch systemStart epoch = pure
      . SendMsgQuery (BlockQuery . QueryHardFork $ GetInterpreter)
      $ getInterpreter systemStart epoch

    handleStakeSnapshots systemStart nextEpoch toNextEpoch StakeSnapshots { ssStakeSnapshots } = do
      atomically do
        writeTVar stakePoolsVar ssStakeSnapshots
        writeTVar nextEpochVar $ Just nextEpoch
      pure $ SendMsgRelease do
        threadDelay $ min (realToFrac toNextEpoch) 86400 -- TODO fuzz this?
        idle $ Just systemStart

connectToCardanoNode :: Tracer IO (WithEventType String)
                     -> LocalSnocket
                     -> FilePath
                     -> NodeKernel crypto ntnAddr IO
                     -> IO (Either SomeException Void)
connectToCardanoNode tracer localSnocket' snocketPath nodeKernel =
  connectTo
   localSnocket'
   debuggingNetworkConnectTracers --nullNetworkConnectTracers
   (combineVersions
     [ simpleSingletonVersions
         version
         NodeToClientVersionData {
             networkMagic =
                   NetworkMagic -- 2 {- preview net -}
                 . unProtocolMagicId
                 $ mainnetProtocolMagicId
           , query = False
         }
         \_version ->
           Mx.OuroborosApplication
             [ Mx.MiniProtocol
                 { miniProtocolNum = Mx.MiniProtocolNum 7
                 , miniProtocolStart = Mx.StartEagerly
                 , miniProtocolLimits =
                     Mx.MiniProtocolLimits
                       { maximumIngressQueue = 0xffffffff
                       }
                 , miniProtocolRun =
                     Mx.InitiatorProtocolOnly
                       . Mx.mkMiniProtocolCbFromPeerSt
                       . const
                       $ ( nullTracer
                         , cStateQueryCodec
                         , StateIdle
                         , localStateQueryClientPeer
                           $ cardanoClient (WithEventType "LocalStateQuery" >$< tracer)
                                    (stakePools nodeKernel)
                                    (nextEpochVar nodeKernel)
                         )
                 }
             ]
     | version <- [minBound..maxBound]
     , let supportedVersionMap = supportedNodeToClientVersions (Proxy :: Proxy (CardanoBlock StandardCrypto))
           blk = supportedVersionMap Map.! version
           Codecs {cStateQueryCodec} =
             clientCodecs (pClientInfoCodecConfig . protocolClientInfoCardano $ EpochSlots 21600) blk version
     ])
   snocketPath
