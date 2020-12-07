{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Scratch.ThreadNet.MockNetwork (
  -- * Mock network layer
  HandshakeCmd,
  initiate,
  respond,
  -- * Channels
  MpChannel,
  newMpChannelsPair,
  ) where

import           Control.Tracer (Tracer (..), traceWith)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor (void)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)

import           Control.Monad.Class.MonadSTM (STM, atomically)
import qualified Control.Monad.Class.MonadSTM as MonadSTM
import           Control.Monad.Class.MonadThrow (generalBracket)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import qualified Control.Monad.Class.MonadTimer as MonadTimer

import qualified Ouroboros.Consensus.Node.NetworkProtocolVersion as Real
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Util.Condense (Condense, condense)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     forkLinkedThread)
import           Ouroboros.Network.Channel (Channel (..))
import           Ouroboros.Network.Channel (createConnectedBufferedChannelsSTM)
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Mux as Mux
import qualified Ouroboros.Network.Protocol.Handshake.Version as Version
import           Ouroboros.Network.Protocol.Handshake.Unversioned

import           Scratch.ThreadNet.Types

-- | Handles the given 'HandshakeCmd'
--
-- It merely spawns the local client threads.
initiate :: forall nid m blk.
     (Condense nid, IOLike m)
  => STM m Mux.ControlMessage
  -> Tracer m (ThreadNetEvent blk)
  -> (nid -> CoreNodeId)
  -> nid
  -> ResourceRegistry m
  -> Diffusion.DiffusionApplications
       PeerId PeerId
       UnversionedProtocolData UnversionedProtocolData
       m
  -> HandshakeCmd m
  -> m ()
initiate
  controlMessageSTM
  tracerHarness
  getCoreNodeId
  nid
  registry
  apps
  (them, nodeToNodeVersion, clientChans)
  = do
    traceWith tracerHarness $ Handshake Client them

    let lbl =
            condense nid <> " " <>
            "chaperone for clients for " <> condense them
    void $ forkLinkedThread registry lbl $ do
      let connectionId = Diffusion.ConnectionId
            { Diffusion.localAddress  = PeerId (getCoreNodeId nid) k
            , Diffusion.remoteAddress = them
            }
            where
              PeerId _coreNodeId k = them
      runHalfOfDirectedEdge
        tracerHarness
        connectionId
        (clientMpActions
           apps
           nodeToNodeVersion
           connectionId
           clientChans
           controlMessageSTM)

      -- check if it stopped because it was told to stop
      atomically controlMessageSTM >>= \case
        Mux.Continue  -> error "the test infra did not tell it to stop"
        Mux.Quiesce   -> error "the test infra does not use Quiesce"
        Mux.Terminate -> pure ()

-- | Handles the given 'HandshakeCmd'
--
-- It merely spawns the local server threads.
respond :: forall nid m blk.
     (Condense nid, IOLike m)
  => Tracer m (ThreadNetEvent blk)
  -> (nid -> CoreNodeId)
  -> nid
     -- ^ me
  -> ResourceRegistry m
  -> Diffusion.DiffusionApplications
       PeerId PeerId
       UnversionedProtocolData UnversionedProtocolData
       m
  -> HandshakeCmd m
  -> m ()
respond
  tracerHarness
  getCoreNodeId
  nid
  registry
  apps
  (them, nodeToNodeVersion, serverChans)
  = do
    traceWith tracerHarness $ Handshake Server them

    let lbl :: String
        lbl =
            condense nid <> " " <>
            "chaperone for servers for " <> condense them
    void $ forkLinkedThread registry lbl $ do
      -- any server termination will terminate all of these servers
      let connectionId = Diffusion.ConnectionId
            { Diffusion.localAddress  = PeerId (getCoreNodeId nid) k
            , Diffusion.remoteAddress = them
            }
            where
              PeerId _coreNodeId k = them
      _again <- runHalfOfDirectedEdge
        tracerHarness
        connectionId
        (serverMpActions apps nodeToNodeVersion connectionId serverChans)

      pure ()   -- _again is only relevant for the client side

-- | The output is only relevant for 'Client', since only clients ever
-- (re-)initiate.
runHalfOfDirectedEdge :: forall m blk.
     IOLike m
  => Tracer m (ThreadNetEvent blk)
  -> Diffusion.ConnectionId PeerId
  -> (ClientServer, MpTuple (MpAction m))
  -> m ()
runHalfOfDirectedEdge
  tracerHarness
  connectionId
  (clientServer, mpActions)
  = do
    withAsyncsAny $
      mpTupleNonEmpty $
      execute <$> mpMP <*> mpActions

    -- not via finally, because any exceptions thrown above are fatal
    traceWith tracerHarness $ ChaperoneDown clientServer them
  where
    Diffusion.ConnectionId
      { Diffusion.localAddress  = PeerId coreNodeId _k
      , Diffusion.remoteAddress = them
      } = connectionId

    -- how to execute a mini protocol thread
    execute :: MP -> MpAction m -> (String, m ())
    execute mp mpAction =
        (,) lbl $
        fmap (\(UNDERSCORE, ()) -> ()) $
        generalBracket

          -- trace beginnings
          (traceWith tracerHarness $ MpEvent mp clientServer them Begin)

          (\() ec -> do
            -- trace ends, including unhandled exceptions
            traceWith tracerHarness $
              MpEvent mp clientServer them $ End ec

            -- Any exception not handled in the mini protocol threads tears
            -- down the entire test; nothing more to do.
          )

          (\() ->
            -- explicitly ignore return value and any unconsumed message
            --
            -- TODO assert no trailing message?
            (\(_a, _mbTrailingMsg) -> UNDERSCORE) <$> mpAction
          )

      where
        lbl = unwords [
            condense coreNodeId
          , show mp
          , show clientServer
          , "for " <> condense them
          ]

{-------------------------------------------------------------------------------
  Abbreviations
-------------------------------------------------------------------------------}

-- | The test infrastructure sends this to a node to cause it to create spawn
-- mini protocol threads for the specified peer using the given channels and
-- version.
type HandshakeCmd m =
  (PeerId, Real.NodeToNodeVersion, MpTuple (MpChannel m))

-- | A fully saturated mini protocol thread
type MpAction m = m ((), Maybe Lazy.ByteString)

{-------------------------------------------------------------------------------
  Thread management
-------------------------------------------------------------------------------}

-- | Spawn multiple async actions as labeled threads and then block until one
-- terminates. Behave like the first to terminate.
--
-- 'NE.NonEmpty' prevents the divergent @'IOLike.waitAny' []@ or similar.
withAsyncsAny :: forall m a. IOLike m => NE.NonEmpty (String, m a) -> m a
withAsyncsAny =
    go [] . NE.toList
  where
    go acc = \case
      []          -> snd <$> IOLike.waitAny acc
      (lbl, m):ms ->
          IOLike.withAsync (do IOLike.labelThisThread lbl; m) $ \h ->
            go (h : acc) ms

clientMpActions :: forall m.
     IOLike m
  => Diffusion.DiffusionApplications
       PeerId PeerId
       UnversionedProtocolData UnversionedProtocolData
       m
  -> Real.NodeToNodeVersion
  -> Diffusion.ConnectionId PeerId
  -> MpTuple (MpChannel m)
  -> STM m Mux.ControlMessage
  -> (ClientServer, MpTuple (MpAction m))
clientMpActions apps nodeToNodeVersion connectionId chans controlCS =
    (,) Client $
    extractMpActions
      (Diffusion.daInitiatorApplication apps)
      (\(Mux.InitiatorProtocolOnly x) -> x)
      nodeToNodeVersion
      connectionId
      chans
      controls
  where
    controls = MpTuple {
        mpBF = trivialControl
      , mpCS = controlCS
      , mpKA = pure $ error "KeepAlive client doesn't use control messages"
      , mpTS = trivialControl
      }

    trivialControl :: STM m Mux.ControlMessage
    trivialControl = pure Mux.Continue

serverMpActions :: forall m.
     IOLike m
  => Diffusion.DiffusionApplications
       PeerId PeerId
       UnversionedProtocolData UnversionedProtocolData
       m
  -> Real.NodeToNodeVersion
  -> Diffusion.ConnectionId PeerId
  -> MpTuple (MpChannel m)
  -> (ClientServer, MpTuple (MpAction m))
serverMpActions apps nodeToNodeVersion connectionId chans =
    (,) Server $
    extractMpActions
      (Diffusion.daResponderApplication apps)
      (\(Mux.ResponderProtocolOnly x) -> x)
      nodeToNodeVersion
      connectionId
      chans
      (pure $ error "servers don't use control vars")

-- | Common logic for 'clientMpActions' and 'serverMpActions'
extractMpActions :: forall vdata mode id m a b.
     IOLike m
  => Version.Versions
       Real.NodeToNodeVersion
       vdata
       (Mux.OuroborosApplication
          mode
          id
          Lazy.ByteString
          m
          a
          b)
  -> (    Mux.RunMiniProtocol mode Lazy.ByteString m a b
       -> Mux.MuxPeer Lazy.ByteString m ()
     )
  -> Real.NodeToNodeVersion
  -> Diffusion.ConnectionId id
  -> MpTuple (MpChannel m)
  -> MpTuple (Mux.ControlMessageSTM m)
  -> MpTuple (MpAction m)
extractMpActions versions prj nodeToNodeVersion connectionId chans controls =
    Mux.runMuxPeer <$> (each <$> mpProtocolNum <*> controls) <*> chans
  where
    each ::
         Mux.MiniProtocolNum
      -> Mux.ControlMessageSTM m
      -> Mux.MuxPeer Lazy.ByteString m ()
    each pnum controlMessageSTM =
        case Map.lookup nodeToNodeVersion (Version.getVersions versions) of
            Nothing -> error "bad NodeToNode version"
            Just v  ->
                case filter ((== pnum) . Mux.miniProtocolNum) mps of
                    [x] -> prj $ Mux.miniProtocolRun x
                    _   -> error "bad MiniprotocolNum"
              where
                Version.Version
                  { Version.versionApplication
                  , Version.versionData
                  } = v

                -- omitting noisy signature
                Mux.OuroborosApplication app =
                    Version.runApplication versionApplication versionData

                mps :: [Mux.MiniProtocol mode Lazy.ByteString m a b]
                mps = app connectionId controlMessageSTM

{-------------------------------------------------------------------------------
  Channels
-------------------------------------------------------------------------------}

-- | A channel for a single mini-protocol connection
--
-- NOTE A 'Channel' is one mini protocol thread's interface to the connection
-- with another node's complementary mini protocol thread, which has its own
-- corresponding (ie connected) 'Channel'.
--
-- These channels are all single-reader and single-writer, so we do not need to
-- consider races.
type MpChannel m = Channel m Lazy.ByteString

-- | A pair of connected channels for each mini protocol.
newMpChannelsPair :: forall m.
     MonadTimer m
  => (String -> m ())
     -- ^ Tick /just/ /before/ /every/ CS or BF send
  -> (String -> m ())
     -- ^ Tick /just/ /after/ /every/ CS or BF receive
  -> MpTuple (Lazy.ByteString -> Bool)
  -> MpTuple (ClientServerPair Delays)
     -- ^ Delays applied to the sending of messages that satisfy the given
     -- predicate
  -> m (ClientServerPair (MpTuple (MpChannel m)))
newMpChannelsPair tickSend tickRecv mpDelayIf mpDelays =
    (fmap @m . fmap @ClientServerPair) aT $
    fmap transpose $
    sequence $
      newDelayedChannelPair <$>
      mpDelayIf <*>
      mpDelays
  where
    transpose :: forall t a.
      Functor t => t (ClientServerPair a) -> ClientServerPair (t a)
    transpose x = ClientServerPair (prjClient <$> x) (prjServer <$> x)

    aT :: MpTuple (MpChannel m) -> MpTuple (MpChannel m)
    aT chans =
      (mpConst id)
        { mpBF = addTicks (tickSend "BF") (tickRecv "BF")
        , mpCS = addTicks (tickSend "CS") (tickRecv "CS")
        }
      <*> chans

-- TODO ChainSync only disconnects gracefully; however the other protocols do
-- not necessarily, so we need to decrement the send once we /somehow/ know a
-- message can never be received
addTicks :: Monad m => m () -> m () -> Channel m a -> Channel m a
addTicks tickSend tickRecv Channel{send, recv} = Channel
  { send = \x -> do tickSend; send x
  , recv = do
      x <- recv
      x <$ tickRecv
  }

-- | Used in 'newMpChannelsPair'
newDelayedChannelPair :: forall m a.
     MonadTimer m
  => (a -> Bool)
  -> ClientServerPair Delays
  -> m (ClientServerPair (Channel m a))
newDelayedChannelPair delayIf delays = do
    chans <- atomically $
      uncurry ClientServerPair <$> createConnectedBufferedChannelsSTM bufferSize
    sequence $ delayChan delayIf <$> delays <*> chans
  where
    -- How many un'recv'ed 'send' calls there can be before 'send' blocks.
    bufferSize = 3

-- | The underlying message cannot be properly received until the paired
-- transaction completes. The transaction is monotonic wrt the wall-clock.
--
-- NOTE Our channels are all single-reader and single-writer, so we do not need
-- to consider if two writers write a message which one will arrive first.
type DelayedMessage m a = (a, STM m ())

-- | Used in 'newDelayedChannelPair'
--
-- NOTE If @sendTime2 + delay2 < sendTime1 + delay1@, then the second message
-- will arrive " one STM transaction " after the first. This corresponds to the
-- packets arriving out of order in a TCP stream: notably, the receiver
-- application doesn't notice (its lower level routines hide that fact).
delayChan :: forall m a.
     MonadTimer m
  => (a -> Bool)
  -> Delays
  -> Channel (STM m) (DelayedMessage m a)
  -> m (Channel m a)
delayChan delayIf (Delays delays0) chan = do
    popDelay <- do
      var <- atomically $ MonadSTM.newTVar delays0
      pure $
        atomically $ MonadSTM.stateTVar var $
          \st@(hd NE.:| tl) -> (hd, fromMaybe st $ NE.nonEmpty tl)
    let _ = popDelay :: m IOLike.DiffTime

    pure Channel {
        send = \x -> do
          wait <- if not (delayIf x) then pure (pure ()) else do
            delay <- popDelay
            (void . MonadTimer.awaitTimeout) <$> MonadTimer.newTimeout delay
          atomically $ send (x, wait)
      , recv = atomically $ do
          mbMsg <- recv
          mapM (\(x, wait) -> (\() -> x) <$> wait) mbMsg
      }
  where
    Channel {send, recv} = chan
