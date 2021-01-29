{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.RethrowPolicy (consensusRethrowPolicy) where

import           Data.Proxy (Proxy)
import           Data.Typeable (Typeable)

import           Control.Monad.Class.MonadAsync (ExceptionInLinkedThread (..))

import           Ouroboros.Consensus.Block (StandardHash)

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDbError (..),
                     ChainDbFailure)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError)
import           Ouroboros.Consensus.Storage.ImmutableDB.API (ImmutableDBError)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmutableDB
import           Ouroboros.Consensus.Storage.VolatileDB.API (VolatileDBError)
import qualified Ouroboros.Consensus.Storage.VolatileDB.API as VolatileDB

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (BlockFetchServerException)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ChainSyncClientException)
import           Ouroboros.Consensus.Node.DbLock
import           Ouroboros.Consensus.Node.DbMarker (DbMarkerError)
import           Ouroboros.Consensus.Util.ResourceRegistry
                     (RegistryClosedException, ResourceRegistryThreadException,
                     TempRegistryException)

import           Ouroboros.Network.RethrowPolicy

-- Exception raised during interaction with the peer
--
-- The list below should contain an entry for every type declared as an
-- instance of 'Exception' within ouroboros-consensus.
--
-- If a particular exception is not handled by any policy, a default
-- kicks in, which currently means logging the exception and disconnecting
-- from the peer (in both directions), but allowing a reconnect within a saall
-- delay (10-20s). This is fine for exceptions that only affect that peer.  It
-- is however essential that we handle exceptions here that /must/ shut down the
-- node (mainly storage layer errors).
--
-- TODO: Talk to devops about what they should do when the node does
-- terminate with a storage layer exception (restart with full recovery).
consensusRethrowPolicy ::
     forall blk. (Typeable blk, StandardHash blk)
  => Proxy blk
  -> RethrowPolicy
consensusRethrowPolicy pb =
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
   <> mkRethrowPolicy (\_ctx (_ :: ChainDbFailure blk) -> shutdownNode)
   <> mkRethrowPolicy (\_ctx (e :: VolatileDBError blk)  ->
          case e of
            VolatileDB.ApiMisuse{}         -> ourBug
            VolatileDB.UnexpectedFailure{} -> shutdownNode)
   <> mkRethrowPolicy (\_ctx (e :: ImmutableDBError blk) ->
          case e of
            ImmutableDB.ApiMisuse{}         -> ourBug
            ImmutableDB.UnexpectedFailure{} -> shutdownNode)
   <> mkRethrowPolicy (\_ctx (_ :: FsError) -> shutdownNode)

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
    <> mkRethrowPolicy (\_ctx (e :: ChainDbError blk) ->
            case e of
              ClosedDBError{}        -> shutdownNode
              ClosedFollowerError{}  -> ourBug
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
       <> mkRethrowPolicy (\_ctx (_ :: ChainSyncClientException) -> theyBuggyOrEvil)

       -- Dispatch on nested exception
    <> mkRethrowPolicy (\ctx (ExceptionInLinkedThread _ e) ->
          runRethrowPolicy (consensusRethrowPolicy pb) ctx e)
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
