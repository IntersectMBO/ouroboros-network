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
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Network.DNS qualified as DNS
import Network.Socket qualified as Socket
import System.Random (StdGen, splitGen)

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
    prAddressKey   :: ntnAddr -> ntnAddr,
    -- ^ the address as the index keys it: without its port, since a relay
    -- behind a NAT may connect from an ephemeral one
    prPollInterval :: DiffTime
    -- ^ how often the ledger list is read and its relays resolved
  }

data TracePoolRelays =
    PoolRelaysRebuilt Int Int
    -- ^ the number of pools changed: pools, addresses indexed
  | PoolRelaysResolved Int
    -- ^ as many pools as before, resolved again: addresses indexed
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
-- snapshot when too old, and resolve every relay. Only a change in the number
-- of pools rebuilds the buckets, and they start empty; any other change, of
-- stake, order or relays, swaps the index and leaves every bucket as it was,
-- so re-registering relays or shifting stake never refills one. With ledger
-- peers disabled there are no buckets at all.
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
    go prRng (Followed Map.empty Map.empty Nothing) Nothing
  where
    -- @built@ is how many buckets there are, @Just 0@ once cleared for
    -- disabled ledger peers, @Nothing@ before the first build
    go :: StdGen -> Followed -> Maybe Int -> m Void
    go rng followed built = do
      useLedger <- atomically prUseLedger
      (followed', built', rng') <-
        case useLedger of
             DontUseLedgerPeers -> do
               case built of
                    Just 0 -> return ()
                    _ -> do
                      now <- getMonotonicTime
                      atomically $ rebuild allowances now 0 Map.empty
                      traceWith prTracer PoolRelaysDisabled
               return (followed, Just 0, rng)

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
                   count       = Map.size bigPeerMap
                   changed     = Just count /= built
               (ok, rng') <- resolveAndApply rng changed bigPeerMap
               return (followedNow, if changed && ok then Just count else built, rng')

      threadDelay prPollInterval
      go rng' followed' built'

    -- resolve every relay of the list; with @full@ the buckets are rebuilt,
    -- otherwise only the index is swapped. False when resolution failed.
    resolveAndApply :: StdGen -> Bool
                    -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
                    -> m (Bool, StdGen)
    resolveAndApply rng full bigPeerMap = do
      let (rng', rng'') = splitGen rng
          pools  = Map.elems bigPeerMap
          relays = concatMap (NonEmpty.toList . snd) pools
      r <- try $ resolveInBatches rng' relays
      case r of
           Left e | Just (SomeAsyncException _) <- fromException e -> throwIO e
                  | otherwise -> do
                      traceWith prTracer (PoolRelaysResolveFailed (show e))
                      return (False, rng'')
           Right resolved -> do
             let index = Map.map (Set.toAscList . Set.fromList) $ Map.fromListWith (++)
                           [ (prAddressKey addr, [position])
                           | (position, (_, rs)) <- zip [0 ..] pools
                           , rp   <- NonEmpty.toList rs
                           , addr <- addressesOf resolved rp ]
             now <- getMonotonicTime
             atomically $
               if full then rebuild allowances now (length pools) index
                       else reindex allowances index
             traceWith prTracer $
               if full then PoolRelaysRebuilt (length pools) (Map.size index)
                       else PoolRelaysResolved (Map.size index)
             return (True, rng'')

    -- the list in slices of 'resolveBatch': each call spawns a thread per
    -- name, and behind the thread's own semaphore two are ever in flight, so
    -- nothing the governor waits for queues behind a thousand names; the
    -- lookups are for membership, so an SRV name gives every target
    resolveInBatches :: StdGen -> [RelayAccessPoint] -> m (Map DNS.Domain (Set ntnAddr))
    resolveInBatches = batches Map.empty
      where
        batches acc _   []     = return acc
        batches acc rng relays = do
          let (batch, rest)  = splitAt resolveBatch relays
              (rng', rng'')  = splitGen rng
          resolved <- resolveRelays prSemaphore DNS.defaultResolvConf (paDnsActions prDNS)
                                    DNSPoolRelay batch rng'
          batches (Map.unionWith Set.union acc resolved) rng'' rest

    resolveBatch :: Int
    resolveBatch = 16

    -- a literal address as given, a name as the resolver returned it
    addressesOf :: Map DNS.Domain (Set ntnAddr) -> RelayAccessPoint -> [ntnAddr]
    addressesOf _        (RelayAccessAddress ip port) = [paToPeerAddr prDNS ip port]
    addressesOf resolved (RelayAccessDomain d _)      = maybe [] Set.toList (Map.lookup d resolved)
    addressesOf resolved (RelayAccessSRVDomain d)     = maybe [] Set.toList (Map.lookup d resolved)

-- | A socket address with its port removed: the index key, since a relay
-- behind a NAT may connect from an ephemeral port.
sockAddrWithoutPort :: Socket.SockAddr -> Socket.SockAddr
sockAddrWithoutPort (Socket.SockAddrInet  _ h)      = Socket.SockAddrInet  0 h
sockAddrWithoutPort (Socket.SockAddrInet6 _ f h s)  = Socket.SockAddrInet6 0 f h s
sockAddrWithoutPort addr                            = addr
