{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Which addresses belong to a big-ledger pool, for the residual tier's
-- credit buckets: a thread that follows the ledger's pool list, resolves
-- every relay and keeps 'PoolAllowances' in step with it.
module Ouroboros.Network.Diffusion.PoolRelays
  ( PoolRelaysArgs (..)
  , TracePoolRelays (..)
  , poolRelaysThread
  , sockAddrWithoutPort
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeAsyncException (..))
import Control.Monad (unless)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Data.Bits (shiftR)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Network.DNS qualified as DNS
import Network.Socket qualified as Socket
import System.Random (StdGen)

import Ouroboros.Network.Block (SlotNo)
import Ouroboros.Network.Diffusion.PoolAllowances
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
           (DNSPeersKind (..), PeerActionsDNS (..))
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore (DNSSemaphore)
import Ouroboros.Network.PeerSelection.RootPeersDNS.LedgerPeers (resolveRelays)

data PoolRelaysArgs extraAPI ntnAddr resolver m = PoolRelaysArgs {
    prTracer       :: Tracer m TracePoolRelays,
    prConsensus    :: LedgerPeersConsensusInterface extraAPI m,
    prUseLedger    :: STM m UseLedgerPeers,
    prSnapshot     :: STM m (Maybe (LedgerPeerSnapshot BigLedgerPeers)),
    prSRVPrefix    :: SRVPrefix,
    prDNS          :: PeerActionsDNS ntnAddr resolver m,
    prSemaphore    :: DNSSemaphore m,
    prRng          :: StdGen,
    -- ^ for the resolver's interface: membership lookups draw no
    -- randomness, so the one generator serves every poll as it is
    prAddressKey   :: ntnAddr -> ntnAddr,
    -- ^ the address as the index keys it: without its port, since a relay
    -- behind a NAT may connect from an ephemeral one
    prPollInterval :: DiffTime
    -- ^ how often the ledger list is read and its relays resolved
  }

data TracePoolRelays =
    PoolRelaysPolled Int Int Int
    -- ^ a poll through: pools in the list, buckets kept from the poll
    -- before, addresses indexed
  | PoolRelaysDisabled
    -- ^ ledger peers are off, so there are no buckets
  | PoolRelaysResolveFailed String
    -- ^ the previous index stays until the next tick
  deriving (Eq, Show)

-- | The pool list and the stake maps the selection caches between reads.
data Followed = Followed {
    fPeerMap    :: !(Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)),
    fBigPeerMap :: !(Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)),
    fCachedSlot :: !(Maybe SlotNo)
  }

-- | Follow the big-ledger pools: on every poll take the same selection the
-- ledger peers thread samples from, the ledger when young enough and the
-- snapshot when too old, resolve every relay and install the list. A pool
-- keeps its bucket from poll to poll by its relay list, whatever its stake
-- or position; a pool new to the list starts full, and one that left loses
-- its bucket. With ledger peers disabled there are no buckets at all.
poolRelaysThread :: forall extraAPI ntnAddr resolver m.
                    ( MonadAsync m
                    , MonadCatch m
                    , MonadDelay m
                    , Ord ntnAddr
                    )
                 => PoolRelaysArgs extraAPI ntnAddr resolver m
                 -> PoolAllowances m ntnAddr
                 -> m Void
poolRelaysThread PoolRelaysArgs { .. } allowances =
    go (Followed Map.empty Map.empty Nothing) False
  where
    -- @cleared@ is whether the buckets were dropped for disabled ledger
    -- peers already. Both are forced on entry: left lazy, a failed poll's
    -- next state would hold its maps until a poll succeeds.
    go :: Followed -> Bool -> m Void
    go !followed !cleared = do
      useLedger <- atomically prUseLedger
      (followed', cleared') <-
        case useLedger of
             DontUseLedgerPeers -> do
               unless cleared $ do
                 now <- getMonotonicTime
                 _ <- atomically $ rebuild allowances now [] Map.empty
                 traceWith prTracer PoolRelaysDisabled
               return (followed, True)

             UseLedgerPeers useLedgerAfter -> do
               (ledgerWithOrigin, ledgerPeers, peerSnapshot) <- atomically $
                 (,,) <$> lpGetLatestSlot prConsensus
                      <*> getLedgerPeers prSRVPrefix prConsensus useLedgerAfter
                      <*> prSnapshot
               let (peerMap, bigPeerMap, cachedSlot) =
                     stakeMapWithSlotOverSource StakeMapOverSource {
                         ledgerWithOrigin,
                         ledgerPeers,
                         peerSnapshot,
                         cachedSlot = fCachedSlot followed,
                         peerMap    = fPeerMap followed,
                         bigPeerMap = fBigPeerMap followed,
                         useLedgerAfter,
                         srvPrefix  = prSRVPrefix
                       }
                   followedNow = Followed peerMap bigPeerMap cachedSlot
               resolveAndApply bigPeerMap
               return (followedNow, False)

      threadDelay prPollInterval
      go followed' cleared'

    -- resolve every relay of the list and install it: a pool known by its
    -- relays keeps its bucket, a newcomer starts full, and the index
    -- follows; when resolution fails what was installed stands
    resolveAndApply :: Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint) -> m ()
    resolveAndApply bigPeerMap = do
      let pools  = Map.elems bigPeerMap
          relays = concatMap (NonEmpty.toList . snd) pools
      r <- try $ resolveInBatches relays
      case r of
           Left e | Just (SomeAsyncException _) <- fromException e -> throwIO e
                  | otherwise -> traceWith prTracer (PoolRelaysResolveFailed (show e))
           Right resolved -> do
             let index = Map.map (Set.toAscList . Set.fromList) $ Map.fromListWith (++)
                           [ (prAddressKey addr, [position])
                           | (position, (_, rs)) <- zip [0 ..] pools
                           , rp   <- NonEmpty.toList rs
                           , addr <- addressesOf resolved rp ]
             now  <- getMonotonicTime
             kept <- atomically $ rebuild allowances now (map snd pools) index
             traceWith prTracer (PoolRelaysPolled (length pools) kept (Map.size index))

    -- the list in slices of 'resolveBatch': each call spawns a thread per
    -- name, and behind the thread's own semaphore two are ever in flight, so
    -- nothing the governor waits for queues behind a thousand names; the
    -- lookups are for membership, so an SRV name gives every target and
    -- none draws on the generator
    resolveInBatches :: [RelayAccessPoint] -> m (Map DNS.Domain (Set ntnAddr))
    resolveInBatches = batches Map.empty
      where
        batches acc []     = return acc
        batches acc relays = do
          let (batch, rest) = splitAt resolveBatch relays
          resolved <- resolveRelays prSemaphore DNS.defaultResolvConf (paDnsActions prDNS)
                                    DNSPoolRelay batch prRng
          batches (Map.unionWith Set.union acc resolved) rest

    resolveBatch :: Int
    resolveBatch = 16

    -- a literal address as given, a name as the resolver returned it
    addressesOf :: Map DNS.Domain (Set ntnAddr) -> RelayAccessPoint -> [ntnAddr]
    addressesOf _        (RelayAccessAddress ip port) = [paToPeerAddr prDNS ip port]
    addressesOf resolved (RelayAccessDomain d _)      = maybe [] Set.toList (Map.lookup d resolved)
    addressesOf resolved (RelayAccessSRVDomain d)     = maybe [] Set.toList (Map.lookup d resolved)

-- | A socket address as the index keys it: its port removed, since a relay
-- behind a NAT may connect from an ephemeral port, and an IPv4 address a
-- dual-stack socket presents as IPv4-mapped IPv6 (::ffff:a.b.c.d) as the
-- IPv4 address, the form an A record resolves to.
sockAddrWithoutPort :: Socket.SockAddr -> Socket.SockAddr
sockAddrWithoutPort (Socket.SockAddrInet  _ h)      = Socket.SockAddrInet  0 h
sockAddrWithoutPort (Socket.SockAddrInet6 _ _ (0, 0, 0xffff, h) _) =
  Socket.SockAddrInet 0 (Socket.tupleToHostAddress
                           ( fromIntegral (h `shiftR` 24), fromIntegral (h `shiftR` 16)
                           , fromIntegral (h `shiftR` 8),  fromIntegral h ))
sockAddrWithoutPort (Socket.SockAddrInet6 _ f h s)  = Socket.SockAddrInet6 0 f h s
sockAddrWithoutPort addr                            = addr
