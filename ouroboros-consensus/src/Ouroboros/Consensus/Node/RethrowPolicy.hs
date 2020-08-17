{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.RethrowPolicy (consensusRethrowPolicy) where

import           Control.Exception (toException)
import           Control.Monad.Class.MonadAsync (ExceptionInLinkedThread (..))

import           Ouroboros.Network.ConnectionManager.RethrowPolicy

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbError (..),
                     ChainDbFailure)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError)
import           Ouroboros.Consensus.Storage.ImmutableDB.Types
                     (ImmutableDBError)
import           Ouroboros.Consensus.Storage.VolatileDB.Types (VolatileDBError)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (BlockFetchServerException)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ChainSyncClientException (..))
import           Ouroboros.Consensus.Node.DbLock
import           Ouroboros.Consensus.Node.DbMarker (DbMarkerError)
import           Ouroboros.Consensus.Util.ResourceRegistry
                     (RegistryClosedException, ResourceRegistryThreadException,
                     TempRegistryException)

consensusRethrowPolicy :: RethrowPolicy
consensusRethrowPolicy =
       mkRethrowPolicy (\_ctx (_ :: DbMarkerError) -> shutdownNode)
      -- Any exceptions in the storage layer should terminate the node
      --
      -- NOTE: We do not catch IOExceptions here; they /ought/ to be caught
      -- by the FS layer (and turn into FsError). If we do want to catch
      -- them, we'd somehow have to distinguish between IO exceptions
      -- arising from disk I/O (shutdownNode) and those arising from
      -- network failures (SuspendConsumer).
   <> mkRethrowPolicy (\_ctx (_ :: DbMarkerError)  -> shutdownNode)
   <> mkRethrowPolicy (\_ctx (_ :: DbLocked)       -> shutdownNode)
   <> mkRethrowPolicy (\_ctx (_ :: ChainDbFailure) -> shutdownNode)
      -- The three exceptions below will always be wrapped in a
      -- 'ChainDbFailure', but we include them in the policy just in case.
   <> mkRethrowPolicy (\_ctx (_ :: VolatileDBError)  -> shutdownNode)
   <> mkRethrowPolicy (\_ctx (_ :: FsError)          -> shutdownNode)
   <> mkRethrowPolicy (\_ctx (_ :: ImmutableDBError) -> shutdownNode)

      -- When the system clock moved back, we have to restart the node,
      -- because the ImmutableDB validation might have to truncate some
      -- blocks from the future. Note that a full validation is not
      -- required, as the default validation (most recent epoch) will keep
      -- on truncating epochs until a block that is not from the future is
      -- found.
    <> mkRethrowPolicy (\_ctx (_ :: SystemClockMovedBackException) -> shutdownNode)

       -- Some chain DB errors are indicative of a bug in our code, others
       -- indicate an invalid request from the peer. If the DB is closed
       -- entirely, it will only be reopened after a node restart.
    <> mkRethrowPolicy (\_ctx (e :: ChainDbError) ->
            case e of
              ClosedDBError{}        -> shutdownNode
              ClosedReaderError{}    -> ourBug
              InvalidIteratorRange{} -> theyBuggyOrEvil)

       -- We have some resource registries that are used per-connection,
       -- and so if we have ResourceRegistry related exception, we close
       -- the connection but leave the rest of the node running.
    <> mkRethrowPolicy (\_ctx (_ :: RegistryClosedException)         -> ourBug)
    <> mkRethrowPolicy (\_ctx (_ :: ResourceRegistryThreadException) -> ourBug)
    <> mkRethrowPolicy (\_ctx (_ :: TempRegistryException)           -> ourBug)

       -- An exception in the block fetch server meant the client asked
       -- for some blocks we used to have but got GCed. This means the
       -- peer is on a chain that forks off more than @k@ blocks away.
    <> mkRethrowPolicy (\_ctx (_ :: BlockFetchServerException) -> distantPeer)

       -- Some chain sync client exceptions indicate malicious behaviour,
       -- others merely mean that we should disconnect from this client
       -- because we have diverged too much.
    <> mkRethrowPolicy (\_ctx e ->
          case e of
            ForkTooDeep{}         -> distantPeer
            HeaderError{}         -> theyBuggyOrEvil
            InvalidRollForward{}  -> distantPeer
            InvalidRollBack{}     -> theyBuggyOrEvil
            InvalidIntersection{} -> theyBuggyOrEvil
            NoMoreIntersection{}  -> distantPeer
            DoesntFit{}           -> theyBuggyOrEvil
            -- A block so far in the future that it exceeds the max clock
            -- skew is also considered to be invalid
            -- ('InFutureExceedsClockSkew' constructor).
            InvalidBlock{}        -> theyBuggyOrEvil)

       -- Dispatch on nested exception
    <> mkRethrowPolicy (\ctx (ExceptionInLinkedThread _ e) ->
          runRethrowPolicy consensusRethrowPolicy ctx (toException e))
  where
    -- Shutdown the node. If we have a storage layer failure, the node /must/
    -- be restarted (triggering recovery).
    shutdownNode :: ErrorCommand
    shutdownNode = ShutdownNode

    -- Peer is either on a distant chain (one that forks more than k blocks ago)
    -- or else is just too far behind; the chain sync client doesn't really have
    -- any way of distinguishing between these two cases. If they are merely
    -- far behind, we might want to reconnect to them later.
    distantPeer :: ErrorCommand
    distantPeer = ShutdownPeer

    -- The peer sent us some data that they could have known was invalid.
    -- This can only be due to a bug or malice.
    theyBuggyOrEvil :: ErrorCommand
    theyBuggyOrEvil = ShutdownPeer

    -- Something went wrong due to a bug in our code. We disconnect from the
    -- peer, but allow to try again later in the hope the bug was transient.
    ourBug :: ErrorCommand
    ourBug = ShutdownPeer
