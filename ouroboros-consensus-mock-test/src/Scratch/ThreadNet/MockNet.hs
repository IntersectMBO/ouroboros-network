{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Scratch.ThreadNet.MockNet (
  -- * Mock net
  Arguments (..),
  ForgeBlockFunction,
  NetArguments (..),
  NodeArguments (..),
  SomeNodeArguments (..),
  doNotWrapForgeBlock,
  executeNet,
  forkNet,
  simulateNet,
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Control.Monad (foldM, forever)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Foldable (forM_)
import           Data.Functor (void)
import           Data.Functor.Contravariant (contramap)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Word (Word64)

import           Control.Tracer (Tracer (..), traceWith)

import           Control.Monad.Class.MonadSTM (MonadSTM, atomically)
import           Control.Monad.Class.MonadSTM.Strict (newEmptyTMVar, putTMVar)
import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Monad.IOSim (IOSim, setCurrentTime)

import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Consensus.Block.Abstract (BlockNo, BlockProtocol, blockNo)
import qualified Ouroboros.Consensus.Block.Forging as Forging
import qualified Ouroboros.Consensus.BlockchainTime as BlockchainTime
import           Ouroboros.Consensus.Config (SecurityParam, TopLevelConfig)
import           Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory)
import           Ouroboros.Consensus.Ledger.Basics (LedgerState, TickedLedgerState)
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import qualified Ouroboros.Consensus.Node as Real
import qualified Ouroboros.Consensus.Node.Tracers as Real
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState, IsLeader)
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Ticked (Ticked)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     forkLinkedThread, withRegistry)

import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (simHasFS)
import qualified Test.Util.HardFork.Future as HFF

import qualified Scratch.ThreadNet.MockNetwork as MockNetwork
import           Scratch.ThreadNet.MockNode
import           Scratch.ThreadNet.Throttler (ThrottlerArgs (..))
import qualified Scratch.ThreadNet.Throttler as Throttler
import           Scratch.ThreadNet.Types
import           Scratch.ThreadNet.Usher (Usher, UsherNetConfig, newUsher)
import qualified Scratch.ThreadNet.Usher as Usher

data NodeArguments m blk = NodeArguments {
    protocolInfo    :: Real.ProtocolInfo m blk
  , vertexTracers   :: VertexTracers m blk
  , wrapForgeBlock  ::
         Tracer m (Usher.UsherEvent (TickedLedgerState blk) (LedgerState blk))
      -> UsherNetConfig
      -> Usher m
      -> ForgeBlockFunction m blk -> ForgeBlockFunction m blk
  }

-- | A default for 'wrapForgeBlock' for ledgers that do not support parameter
-- updates (eg mock ledger).
doNotWrapForgeBlock ::
     Tracer m (Usher.UsherEvent (TickedLedgerState blk) (LedgerState blk))
  -> UsherNetConfig
  -> Usher m
  -> ForgeBlockFunction m blk
  -> ForgeBlockFunction m blk
doNotWrapForgeBlock _tracer _usher _netCfg = id

data SomeNodeArguments m =
  forall blk.
  (Real.RunNode blk, HasHardForkHistory blk, LedgerSupportsProtocol blk) =>
  SomeNodeArguments (NodeArguments m blk)

data NetArguments m = NetArguments {
    commonK              :: SecurityParam
    -- ^ The security parameter of all net participants
  , firstProperBlockNo   :: BlockNo
    -- ^ The first valid block to be forged in this simulation must have this
    -- number.
  , future               :: HFF.Future
    -- ^ The specified era transitions
  , mbEndOfDays          :: Maybe IOLike.DiffTime
    -- ^ How long to simulate
    --
    -- TODO requires OracularClock as-is; can we just specify DiffTime instead
    -- or is slots the most useful delimiter?
  , plan                 :: NetPlan
    -- ^ What events to simulate
  , systemStart          :: BlockchainTime.SystemStart
    -- ^ The wall clock time at which the simulation begins
  , tracerThrottlerEvent :: Tracer m (ThrottlerEvent m)
  , tracerPlannedEvent   :: Tracer m PlannedEvent
  , usherNetConfig       :: UsherNetConfig
  }

data Arguments m =
  Arguments (NetArguments m) (SecurityParam -> HFF.Future -> CoreNodeId -> SomeNodeArguments m)

forkNet :: forall m.
     ( IOLike m
     , MonadTime m
     , MonadTimer m
     )
  => ResourceRegistry m
  -> Arguments m
  -> m (Map CoreNodeId (InputVars m), ThrottlerArgs m)
forkNet registry0 arguments = do
    throttlerArgs <- Throttler.newArgs firstProperBlockNo
    usher         <- newUsher

    void $ forkLinkedThread registry0 "flusher" $
      forever $ Throttler.flushOnce tracerThrottlerEvent throttlerArgs

    inputVars <- forkNet_ registry0 arguments throttlerArgs usher
    pure (inputVars, throttlerArgs)
  where
    Arguments netArguments _mkNodeArguments = arguments
    NetArguments {
        firstProperBlockNo
      , tracerThrottlerEvent
      } = netArguments

relativize :: Num k => [(k, v)] -> [(k, v)]
relativize =
    go 0
  where
    go acc = \case
      []         -> []
      (k, v):kvs -> (k - acc, v) : go k kvs

forkNet_ :: forall m.
     ( IOLike m
     , MonadTime m
     , MonadTimer m
     )
  => ResourceRegistry m
  -> Arguments m
  -> ThrottlerArgs m
  -> Usher m
  -> m (Map CoreNodeId (InputVars m))
forkNet_ registry0 arguments throttlerArgs usher = do
    inputVars <-
      foldM snoc1 Map.empty $
      relativize $
      Map.toAscList $ unNetPlan plan
    pure inputVars
  where
    Arguments netArguments mkNodeArguments = arguments
    ThrottlerArgs
      { taStateVar
      , taRecvVar
      , taSendVar
      } = throttlerArgs
    NetArguments {
        commonK
      , future
      , plan
      , tracerThrottlerEvent
      , tracerPlannedEvent
      , usherNetConfig
      } = netArguments

    snoc1 ::
         Map CoreNodeId (InputVars m)
      -> (IOLike.DiffTime, NonEmpty PlannedEvent)
      -> m (Map CoreNodeId (InputVars m))
    snoc1 acc (t, events) = do
      IOLike.threadDelay t
      foldM snoc2 acc events

    -- omitting noisy type
    snoc2 acc ev = do
        traceWith tracerPlannedEvent ev
        enact acc ev

    enact ::
         Map CoreNodeId (InputVars m)
      -> PlannedEvent
      -> m (Map CoreNodeId (InputVars m))
    enact acc (PlannedNodeUp nodeId nodeUpData) =
        case mkNodeArguments commonK future coreNodeId of
          SomeNodeArguments nodeArguments -> do
              myVertexCommandVar <- atomically $ newEmptyTMVar
              let myInputVars = myVertexCommandVar

              -- fork the vertex
              void $ forkLinkedThread registry0 label $ do
                vertexArguments <-
                  mkVertexArguments nodeId nodeArguments nodeUpData
                runVertex
                  ( extendVertexTracersForThrottler
                      nodeArguments
                      nodeId
                  )
                  vertexArguments
                  myInputVars
              pure $ Map.insert coreNodeId myInputVars acc
      where
        label = "node " <> condense nodeId

        NodeId coreNodeId _k = nodeId

    enact acc (PlannedConnect connId delaysPair nodeToNodeVersion) = do
        case (,) <$> Map.lookup coreNodeId acc <*> Map.lookup them acc of
          Nothing
            -> error "bad plan"
          Just (myInputVars, theirInputVars)
            -> do
              let myVertexCommandVar    = myInputVars
                  theirVertexCommandVar = theirInputVars

              -- NB no registry
              chansPair <- MockNetwork.newMpChannelsPair
                (tickTVar "send" taSendVar) (tickTVar "recv" taRecvVar)
                ( asTypeOf (mpConst $ \_msg -> True) $   -- TODO
                  (mpConst $ \_msg -> False){ mpCS = isRollForward }
                )
                delaysPair
              let ClientServerPair clientChans serverChans = chansPair

              atomically $
                putTMVar myVertexCommandVar $
                  InitiateTo (PeerId them k, nodeToNodeVersion, clientChans)

              atomically $
                putTMVar theirVertexCommandVar $
                  RespondTo (PeerId coreNodeId k, nodeToNodeVersion, serverChans)

        pure acc
      where
        ConnectionId (ClientServerPair coreNodeId them) k = connId

    enact acc (PlannedDisconnect (ConnectionId pair k)) = do
        forM_ (Map.lookup coreNodeId acc) $ \myInputVars -> do
          let myVertexCommandVar = myInputVars
          atomically $
            putTMVar myVertexCommandVar $
              DisconnectFrom (PeerId them k)

        pure acc
      where
        ClientServerPair coreNodeId them = pair

    mkVertexArguments :: forall blk.
         NodeId
      -> NodeArguments m blk
      -> NodeUpData
      -> m (VertexArguments m blk)
    mkVertexArguments nodeId nodeArguments nodeUpData = do
        -- TODO the DB FSs should only be created once per CoreNodeId and
        -- reused for boots thereafter
        nodeDBs <- sequence $ NodeDBs {
            nodeDBsImmutable = newFS
          , nodeDBsVolatile  = newFS
          , nodeDBsLedger    = newFS
          }
        pure VertexArguments {
            coreNodeId
          , epochSizeImmDb =
              -- TODO is this correspondence necessary?
              HFF.futureFirstEpochSize future
          , nodeDBs
          , nodeUpData
          , protocolInfo   =
              (over_pInfoBlockForging . fmap @m . map . over_updateForgeState)
                (wrapUpdateForgeState nodeId) $
              (over_pInfoBlockForging . fmap @m . map . over_forgeBlock)
                (wrapForgeBlock usherTracer usherNetConfig usher) $
                protocolInfo
          }
      where
        NodeId coreNodeId _k = nodeId

        newFS :: m (SomeHasFS m)
        newFS = do
            fs <- IOLike.uncheckedNewTVarM Mock.empty
            pure $ SomeHasFS $ simHasFS fs
        NodeArguments {
            protocolInfo
          , wrapForgeBlock
          } = nodeArguments

        usherTracer = Tracer $ \case
            Usher.UsherDone{}           -> pure ()
            Usher.UsherForgedInFinalEra ->
                Throttler.truncateToLastFlushState
                  tracerThrottlerEvent
                  throttlerArgs
            Usher.UsherInjectedTxs{}    -> pure ()
            Usher.UsherForged{}         -> pure ()
          

    -- Wrap 'enterForge' around the underlying 'updateForgeState'
    wrapUpdateForgeState ::
         NodeId
      -> UpdateForgeStateFunction m blk
      -> UpdateForgeStateFunction m blk
    wrapUpdateForgeState nodeId underlying cfg sl tchdepst = do
        threadId <- IOLike.myThreadId
        (updateInfo, mbNewState) <- atomically $ do
          Throttler.modifyingTVar taStateVar $
            Throttler.enterForge (nodeId, threadId) sl
        forM_ mbNewState $ \new -> do
          traceWith tracerThrottlerEvent $ ForgeTransition new
        case updateInfo of
          DoSuppress    -> pure Forging.ForgeStateUpdateSuppressed
          DoNotSuppress -> underlying cfg sl tchdepst

    -- Invoke 'exitForge' for the forging "Ouroboros.Consensus.NodeKernel"
    -- events
    extendVertexTracersForThrottler :: forall blk.
         LedgerSupportsProtocol blk
      => NodeArguments m blk
      -> NodeId
      -> VertexTracers m blk
    extendVertexTracersForThrottler nodeArguments nodeId =
        mk preForgeTracer <> vertexTracers <> mk postForgeTracer
      where
        NodeArguments
          { vertexTracers
          } = nodeArguments

        -- create an otherwise empty set of tracers
        mk :: (Real.TraceForgeEvent blk -> m ()) -> VertexTracers m blk
        mk tr = nullVertexTracers {
            tracersConsensus = Real.nullTracers {
                Real.forgeTracer           =
                  ignoreTraceLabelCreds $ Tracer tr
              }
          }

        -- after the outer tracer
        postForgeTracer :: Real.TraceForgeEvent blk -> m ()
        postForgeTracer = \case
            Real.TraceForgedInvalidBlock _slot _blk err
              ->
                error $
                "This tracer should be dead-code; an outer tracer should" <>
                " have already failed fatally for this event! " <> unlines [show _slot, show err]
            _ -> pure ()

        -- before the outer tracer
        preForgeTracer :: Real.TraceForgeEvent blk -> m ()
        preForgeTracer ev = case ev of
            -- Information recorded at key moments during the forging, but
            -- unnecessary for the governor.
            Real.TraceStartLeadershipCheck{} -> pure ()
            Real.TraceBlockContext{}         -> pure ()
            Real.TraceLedgerState{}          -> pure ()
            Real.TraceLedgerView{}           -> pure ()
            Real.TraceNodeIsLeader{}         -> pure ()
            Real.TraceForgedBlock{}          -> pure ()
                -- FYI The block is forged but has not yet been added to the
                -- ChainDB; that is now imminent.

            -- One of these happens if the node was unable to even forge a
            -- block (ie just before each call to @exitEarly@), but this
            -- happens before the call to 'enterForge'.
            Real.TraceSlotIsImmutable{}       -> pure ()
            Real.TraceBlockFromFuture{}       -> pure ()
            Real.TraceNoLedgerState{}         -> pure ()
            Real.TraceNoLedgerView{}          -> pure ()

            -- One of these happens if the node was unable to even forge a
            -- block (ie just before each call to @exitEarly@).
            Real.TraceForgeStateUpdateError{} -> exitedEarly
            Real.TraceNodeCannotForge{}       -> exitedEarly
            Real.TraceNodeNotLeader{}         -> exitedEarly

            -- One of these three happens /after/ the forged block has been
            -- added to the node's ChainDB.
            Real.TraceDidntAdoptBlock{}         -> exit ForgeFail
                -- TODO throw error here. PBFT is the only reason this would
                -- happen, and that's TraceNodeCannotForge above.
            Real.TraceForgedInvalidBlock{}      -> do
                -- this is dead-code; see 'postForgeTracer'
                pure ()
            Real.TraceAdoptedBlock _sl blk _txs -> do
                exit $ ForgeSuccess (blockNo blk)
          where
            sl = slotTraceForgeEvent ev

            exitedEarly = exit ForgeExitEarly

            -- invoke 'exitForge' correctly
            exit :: ForgeExitCase -> m ()
            exit ec = do
                threadId <- IOLike.myThreadId
                ((), mbNew) <- atomically $ do
                  Throttler.modifyingTVar taStateVar $ \st ->
                    pure
                      ( ()
                      , Throttler.exitForge (nodeId, threadId) commonK sl ec st
                      )
                forM_ mbNew $ \new -> do
                  traceWith tracerThrottlerEvent $ ForgeTransition new

executeNet :: forall m.
     ( IOLike m
     , MonadTime m
     , MonadTimer m
     )
  => Arguments m -> m ()
executeNet arguments = withRegistry $ \registry -> do
    (_, throttlerArgs) <- forkNet registry arguments

    forM_ mbEndOfDays $ \endOfDays -> do
      IOLike.threadDelay endOfDays   -- TODO this is absolute, not relative
      Throttler.truncateToLastFlushState tracerThrottlerEvent throttlerArgs
    Throttler.blockUntilShutdownState throttlerArgs
  where
    Arguments netArguments _mkNodeArguments = arguments
    NetArguments {
        mbEndOfDays
      , tracerThrottlerEvent
      } = netArguments

simulateNet ::
     IOLike (IOSim s)
  => Arguments (IOSim s)
  -> IOSim s ()
simulateNet arguments = do
    setCurrentTime t
    executeNet arguments
  where
    Arguments netArguments _ = arguments
    NetArguments {
        systemStart
      } = netArguments

    BlockchainTime.SystemStart t = systemStart

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

-- | Is this serialization of
-- 'Ouroboros.Network.Protocol.ChainSync.Type.MsgRollForward'?
isRollForward :: Lazy.ByteString -> Bool
isRollForward msg =
    case deserialiseFromBytes dec msg of
      Right (_, b) -> b
      Left _e      -> False
  where
    -- see 'Ouroboros.Network.Protocol.ChainSync.Codec.codecChainSync'
    dec :: forall s. CBOR.Decoder s Bool
    dec = do
        n   <- CBOR.decodeListLen
        tag <- CBOR.decodeWord
        pure $ (n, tag) == (3, 2)

over_pInfoBlockForging ::
     (m [Forging.BlockForging m blk] -> m [Forging.BlockForging m blk])
  -> Real.ProtocolInfo m blk -> Real.ProtocolInfo m blk
over_pInfoBlockForging f pinfo = pinfo
  { Real.pInfoBlockForging = f $ Real.pInfoBlockForging pinfo
  }

type UpdateForgeStateFunction m blk =
     TopLevelConfig blk
  -> SlotNo
  -> Ticked (ChainDepState (BlockProtocol blk))
  -> m (Forging.ForgeStateUpdateInfo blk)

over_updateForgeState ::
     (UpdateForgeStateFunction m blk -> UpdateForgeStateFunction m blk)
  -> Forging.BlockForging m blk -> Forging.BlockForging m blk
over_updateForgeState f blockForging = blockForging
  { Forging.updateForgeState = f $ Forging.updateForgeState blockForging
  }

type ForgeBlockFunction m blk =
  Usher.ForgeBlockType
    m
    blk
    (TopLevelConfig blk)
    BlockNo
    (IsLeader (BlockProtocol blk))

over_forgeBlock ::
     (ForgeBlockFunction m blk -> ForgeBlockFunction m blk)
  -> Forging.BlockForging m blk -> Forging.BlockForging m blk
over_forgeBlock f blockForging = blockForging
  { Forging.forgeBlock = f $ Forging.forgeBlock blockForging
  }

tickTVar :: MonadSTM m => String -> IOLike.StrictTVar m Word64 -> String -> m ()
tickTVar _s1 var _s2 = atomically $ IOLike.modifyTVar var (+1)
-- tickTVar s1 var s2 = atomically $ IOLike.modifyTVar var (\x -> trace (s2 <> " " <> s1 <> " " <> show (x+1)) x + 1)

slotTraceForgeEvent :: Real.TraceForgeEvent blk -> SlotNo
slotTraceForgeEvent = \case
    Real.TraceStartLeadershipCheck sl -> sl
    Real.TraceSlotIsImmutable sl _p _bno -> sl
    Real.TraceBlockFromFuture sl _sl -> sl
    Real.TraceBlockContext sl _bno _p -> sl
    Real.TraceNoLedgerState sl _p -> sl
    Real.TraceLedgerState sl _p -> sl
    Real.TraceNoLedgerView sl _err -> sl
    Real.TraceLedgerView sl -> sl
    Real.TraceForgeStateUpdateError sl _err -> sl
    Real.TraceNodeCannotForge sl _evi -> sl
    Real.TraceNodeNotLeader sl -> sl
    Real.TraceNodeIsLeader sl -> sl
    Real.TraceForgedBlock sl _p _blk _sz -> sl
    Real.TraceDidntAdoptBlock sl _blk -> sl
    Real.TraceForgedInvalidBlock sl _blk _err -> sl
    Real.TraceAdoptedBlock sl _blk _txs -> sl

ignoreTraceLabelCreds :: Tracer m x -> Tracer m (Real.TraceLabelCreds x)
ignoreTraceLabelCreds = contramap $ \(Real.TraceLabelCreds _ ev) -> ev
