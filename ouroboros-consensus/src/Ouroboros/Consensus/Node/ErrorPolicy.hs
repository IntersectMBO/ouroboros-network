{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy) where

import           Data.Proxy (Proxy)
import           Data.Time.Clock (DiffTime)

import           Control.Monad.Class.MonadAsync (ExceptionInLinkedThread (..))

import           Ouroboros.Network.ErrorPolicy

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbError (..),
                     ChainDbFailure)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError)
import           Ouroboros.Consensus.Storage.ImmutableDB.Types
                     (ImmutableDBError)
import           Ouroboros.Consensus.Storage.VolatileDB.Types (VolatileDBError)

import           Ouroboros.Consensus.BlockchainTime.WallClock
                     (SystemClockMovedBackException)
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (BlockFetchServerException)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ChainSyncClientException (..))
import           Ouroboros.Consensus.Node.DbMarker (DbMarkerError)
import           Ouroboros.Consensus.Node.Run (RunNode (nodeExceptionIsFatal))
import           Ouroboros.Consensus.Util.ResourceRegistry
                     (RegistryClosedException, ResourceRegistryThreadException,
                     TempRegistryException)

consensusErrorPolicy :: RunNode blk => Proxy blk -> ErrorPolicies
consensusErrorPolicy pb = ErrorPolicies {
      -- Exception raised during connect
      --
      -- This is entirely a network-side concern.
      epConErrorPolicies = []

      -- Exception raised during interaction with the peer
      --
      -- The list below should contain an entry for every type declared as an
      -- instance of 'Exception' within ouroboros-consensus.
      --
      -- If a particular exception is not handled by any policy, a default
      -- kicks in, which currently means logging the exception and disconnecting
      -- from the peer (in both directions), but allowing an immediate
      -- reconnect. This is fine for exceptions that only affect that peer.
      -- It is however essential that we handle exceptions here that /must/
      -- shut down the node (mainly storage layer errors).
      --
      -- TODO: Talk to devops about what they should do when the node does
      -- terminate with a storage layer exception (restart with full recovery).
    , epAppErrorPolicies = [
          -- Any exceptions in the storage layer should terminate the node
          --
          -- NOTE: We do not catch IOExceptions here; they /ought/ to be caught
          -- by the FS layer (and turn into FsError). If we do want to catch
          -- them, we'd somehow have to distinguish between IO exceptions
          -- arising from disk I/O (shutdownNode) and those arising from
          -- network failures (SuspendConsumer).
          ErrorPolicy $ \(_ :: DbMarkerError)    -> Just shutdownNode
        , ErrorPolicy $ \(_ :: ChainDbFailure)   -> Just shutdownNode
          -- The three exceptions below will always be wrapped in a
          -- 'ChainDbFailure', but we include them in the policy just in case.
        , ErrorPolicy $ \(_ :: VolatileDBError)  -> Just shutdownNode
        , ErrorPolicy $ \(_ :: FsError)          -> Just shutdownNode
        , ErrorPolicy $ \(_ :: ImmutableDBError) -> Just shutdownNode

          -- When the system clock moved back, we have to restart the node,
          -- because the ImmutableDB validation might have to truncate some
          -- blocks from the future. Note that a full validation is not
          -- required, as the default validation (most recent epoch) will keep
          -- on truncating epochs until a block that is not from the future is
          -- found.
        , ErrorPolicy $ \(_ :: SystemClockMovedBackException) -> Just shutdownNode

          -- Some chain DB errors are indicative of a bug in our code, others
          -- indicate an invalid request from the peer. If the DB is closed
          -- entirely, it will only be reopened after a node restart.
        , ErrorPolicy $ \(e :: ChainDbError) ->
            case e of
              ClosedDBError{}        -> Just shutdownNode
              ClosedReaderError{}    -> Just ourBug
              InvalidIteratorRange{} -> Just theyBuggyOrEvil

          -- We have some resource registries that are used per-connection,
          -- and so if we have ResourceRegistry related exception, we close
          -- the connection but leave the rest of the node running.
        , ErrorPolicy $ \(_ :: RegistryClosedException)         -> Just ourBug
        , ErrorPolicy $ \(_ :: ResourceRegistryThreadException) -> Just ourBug
        , ErrorPolicy $ \(_ :: TempRegistryException)           -> Just ourBug

          -- An exception in the block fetch server meant the client asked
          -- for some blocks we used to have but got GCed. This means the
          -- peer is on a chain that forks off more than @k@ blocks away.
        , ErrorPolicy $ \(_ :: BlockFetchServerException) -> Just distantPeer

          -- Some chain sync client exceptions indicate malicious behaviour,
          -- others merely mean that we should disconnect from this client
          -- because we have diverged too much.
        , ErrorPolicy $ \(e :: ChainSyncClientException) ->
            case e of
              HeaderExceedsClockSkew{} -> Just misconfiguredPeer
              ForkTooDeep{}            -> Just distantPeer
              HeaderError{}            -> Just theyBuggyOrEvil
              InvalidRollForward{}     -> Just distantPeer
              InvalidRollBack{}        -> Just theyBuggyOrEvil
              InvalidIntersection{}    -> Just theyBuggyOrEvil
              NoMoreIntersection{}     -> Just distantPeer
              DoesntFit{}              -> Just theyBuggyOrEvil
              InvalidBlock{}           -> Just theyBuggyOrEvil

          -- Dispatch on nested exception
        , ErrorPolicy $ \(ExceptionInLinkedThread _ e) ->
            evalErrorPolicies e (epAppErrorPolicies (consensusErrorPolicy pb))

          -- Blocks can have their own specific exceptions, which can be fatal
          -- too
        , ErrorPolicy $ \e -> case nodeExceptionIsFatal pb e of
            Just _reason -> Just shutdownNode
            Nothing      -> Nothing
        ]
    }
  where
    -- Shutdown the node. If we have a storage layer failure, the node /must/
    -- be restarted (triggering recovery).
    shutdownNode :: SuspendDecision DiffTime
    shutdownNode = Throw

    -- Peer sent us wrong data, but it's more than likely due to a
    -- misconfiguration. We try to reconnect in a few minutes
    misconfiguredPeer :: SuspendDecision DiffTime
    misconfiguredPeer = SuspendPeer defaultDelay defaultDelay

    -- Peer is either on a distant chain (one that forks more than k blocks ago)
    -- or else is just too far behind; the chain sync client doesn't really have
    -- any way of distinguishing between these two cases. If they are merely
    -- far behind, we might want to reconnect to them later.
    distantPeer :: SuspendDecision DiffTime
    distantPeer = SuspendConsumer defaultDelay

    -- The peer sent us some data that they could have known was invalid.
    -- This can only be due to a bug or malice.
    theyBuggyOrEvil :: SuspendDecision DiffTime
    theyBuggyOrEvil = SuspendPeer defaultDelay defaultDelay

    -- Something went wrong due to a bug in our code. We disconnect from the
    -- peer, but allow to try again later in the hope the bug was transient.
    -- We do not close the connection in the other direction; if the bug was
    -- indeed local, it might not affect communication in the other direction.
    ourBug :: SuspendDecision DiffTime
    ourBug = SuspendConsumer defaultDelay

    -- Default delay
    --
    -- We might want to tweak the delays for the various different kinds of
    -- problems, but we'd need to establish a policy on how to set them.
    defaultDelay :: DiffTime
    defaultDelay = 200 -- seconds
