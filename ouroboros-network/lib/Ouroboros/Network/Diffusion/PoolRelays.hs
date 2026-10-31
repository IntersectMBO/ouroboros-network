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
import Control.Monad (when)
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

import Ouroboros.Network.Block (SlotNo)
import Ouroboros.Network.Diffusion.PoolAllowances
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions (DNSActions (..),
           DNSPeersKind (..), DNSorIOError (..), PeerActionsDNS (..),
           Resource (..))
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore (DNSSemaphore,
           withDNSSemaphore)

data PoolRelaysArgs extraAPI ntnAddr resolver m = PoolRelaysArgs {
    prTracer       :: Tracer m TracePoolRelays,
    prConsensus    :: LedgerPeersConsensusInterface extraAPI m,
    prUseLedger    :: STM m UseLedgerPeers,
    prSnapshot     :: STM m (Maybe (LedgerPeerSnapshot BigLedgerPeers)),
    prSRVPrefix    :: SRVPrefix,
    prDNS          :: PeerActionsDNS ntnAddr resolver m,
    prSemaphore    :: DNSSemaphore m,
    prAddressKey   :: ntnAddr -> ntnAddr,
    -- ^ the address as the index keys it: without its port, since a relay
    -- behind a NAT may connect from an ephemeral one
    prPollInterval :: DiffTime
    -- ^ how often the ledger list is read and its relays resolved
  }

data TracePoolRelays =
    PoolRelaysRebuilt Int Int
    -- ^ the number of pools changed: pools, addresses indexed
  | PoolRelaysResolved Int Int
    -- ^ as many pools as before, resolved again: addresses indexed, and
    -- names kept as last known because their lookup failed for the moment
  | PoolRelaysDisabled
    -- ^ ledger peers are off, so there are no buckets
  | PoolRelaysResolveFailed String
    -- ^ the resolver failed; what was published so far stands
  deriving (Eq, Show)

-- | The pool list and the stake maps the selection caches between reads.
data Followed = Followed {
    fPeerMap    :: !(Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)),
    fBigPeerMap :: !(Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)),
    fCachedSlot :: !(Maybe SlotNo)
  }

-- | What the thread last learned of each name: the targets of each SRV
-- name, and by record type the addresses of each name, kept across polls
-- so that a lookup failing for the moment costs nothing, and pruned to the
-- names the list has.
data Known = Known {
    knTargets :: !(Map DNS.Domain [(DNS.Domain, Socket.PortNumber)]),
    knIPs     :: !(Map (DNS.Domain, DNS.TYPE) (Set IP))
  }

-- | Follow the big-ledger pools: on every poll take the same selection the
-- ledger peers thread samples from, the ledger when young enough and the
-- snapshot when too old, and resolve every relay in two stages, each SRV
-- name to its targets and then every name, the relays' own and the
-- targets, by record type. Each lookup publishes as it completes, so no
-- name waits for another. An answer replaces what was known of a name, a
-- name that does not exist is forgotten, and any other failure keeps what
-- was known. Only a change in the number of pools rebuilds the buckets,
-- and they start empty; any other change, of stake, order or relays,
-- swaps the index and leaves every bucket as it was, so re-registering
-- relays or shifting stake never refills one. With ledger peers disabled
-- there are no buckets at all.
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
    go (Followed Map.empty Map.empty Nothing) Nothing (Known Map.empty Map.empty)
  where
    -- @built@ is how many buckets there are, @Just 0@ once cleared for
    -- disabled ledger peers, @Nothing@ before the first build. All three
    -- are forced on entry: left lazy, a failed poll's next state would hold
    -- its maps until a poll succeeds.
    go :: Followed -> Maybe Int -> Known -> m Void
    go !followed !built !known = do
      useLedger <- atomically prUseLedger
      (followed', built', known') <-
        case useLedger of
             DontUseLedgerPeers -> do
               case built of
                    Just 0 -> return ()
                    _ -> do
                      now <- getMonotonicTime
                      atomically $ rebuild allowances now 0 Map.empty
                      traceWith prTracer PoolRelaysDisabled
               return (followed, Just 0, known)

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
                   pools       = Map.elems bigPeerMap
                   count       = length pools
                   changed     = Just count /= built
               -- what is known serves at once; new buckets start empty
               when changed $ do
                 now <- getMonotonicTime
                 atomically $ rebuild allowances now count (indexOf pools known)
               r <- try $ resolvePools pools known
               knownNow <- case r of
                 Left e | Just (SomeAsyncException _) <- fromException e -> throwIO e
                        | otherwise -> do
                            traceWith prTracer (PoolRelaysResolveFailed (show e))
                            return (prune pools known)
                 Right (knownNow, kept) -> do
                   let indexed = Map.size (indexOf pools knownNow)
                   traceWith prTracer $
                     if changed then PoolRelaysRebuilt count indexed
                                else PoolRelaysResolved indexed kept
                   return knownNow
               return (followedNow, if changed then Just count else built, knownNow)

      threadDelay prPollInterval
      go followed' built' known'

    -- resolve the list's names in two stages, publishing after every
    -- lookup; what is known afterwards, pruned to the list, and how many
    -- names were kept as last known
    resolvePools :: [(PoolStake, NonEmpty RelayAccessPoint)] -> Known -> m (Known, Int)
    resolvePools pools known0 = do
      knownVar <- newTVarIO known0
      keptVar  <- newTVarIO (0 :: Int)
      withResolver $ \resolver ->
        lookups [ do r <- dnsLookupSRV acts DNSPoolRelay d conf resolver
                     atomically $ do
                       kn <- readTVar knownVar
                       let (targets, kept) = learn d r (knTargets kn)
                       writeTVar knownVar kn { knTargets = targets }
                       when kept $ modifyTVar keptVar (+ 1)
                | d <- Set.toList (srvNamesOf pools) ]
      names <- namesOf pools . knTargets <$> readTVarIO knownVar
      withResolver $ \resolver ->
        lookups [ do rs <- dnsLookupIPs acts DNSPoolRelay d conf resolver
                     atomically $ do
                       kn <- readTVar knownVar
                       let step (m, n) (ty, r) =
                             let (m', held) = learn (d, ty) (Set.fromList . map fst <$> r) m
                             in (m', if held then n + 1 else n)
                           (ips, kept) = foldl step (knIPs kn, 0) rs
                           kn' = kn { knIPs = ips }
                       writeTVar knownVar kn'
                       modifyTVar keptVar (+ kept)
                       reindex allowances (indexOf pools kn')
                | d <- Set.toList names ]
      known <- prune pools <$> readTVarIO knownVar
      -- a list that changed without a name to look up still swaps the index
      atomically $ reindex allowances (indexOf pools known)
      kept <- readTVarIO keptVar
      return (known, kept)

    acts = paDnsActions prDNS
    conf = DNS.defaultResolvConf

    -- a lookup's outcome applied to what is known under a key: an answer
    -- replaces, a name that does not exist is forgotten, and any other
    -- failure keeps what was known, which is the True
    learn :: Ord k => k -> Either DNS.DNSError v -> Map k v -> (Map k v, Bool)
    learn k (Right v) m = (Map.insert k v m, False)
    learn k (Left e)  m
      | definitive e = (Map.delete k m, False)
      | otherwise    = (m, Map.member k m)

    definitive :: DNS.DNSError -> Bool
    definitive DNS.NameError     = True
    definitive DNS.IllegalDomain = True
    definitive _                 = False

    -- the resolver for a stage; its failure is the stage's
    withResolver :: (resolver -> m a) -> m a
    withResolver k = do
      rr <- dnsResolverResource acts conf
      (er, _) <- withResource rr
      case er of
           Left (DNSError e) -> throwIO e
           Left (IOError e)  -> throwIO e
           Right resolver    -> k resolver

    -- every lookup in its own thread, as many in flight as the thread's
    -- semaphore allows; one that throws ends them all
    lookups :: [m ()] -> m ()
    lookups = run []
      where
        run as []       = mapM_ wait (reverse as)
        run as (x : xs) = withAsync (withDNSSemaphore prSemaphore x) (\a -> run (a : as) xs)

    srvNamesOf :: [(PoolStake, NonEmpty RelayAccessPoint)] -> Set DNS.Domain
    srvNamesOf pools = Set.fromList [ d | (_, rs) <- pools, RelayAccessSRVDomain d <- NonEmpty.toList rs ]

    -- the names to look up: the relays' own and the targets of its SRV names
    namesOf :: [(PoolStake, NonEmpty RelayAccessPoint)]
            -> Map DNS.Domain [(DNS.Domain, Socket.PortNumber)] -> Set DNS.Domain
    namesOf pools targets = Set.fromList $
         [ d | (_, rs) <- pools, RelayAccessDomain d _ <- NonEmpty.toList rs ]
      ++ [ t | d <- Set.toList (srvNamesOf pools), (t, _) <- Map.findWithDefault [] d targets ]

    -- forget the names the list no longer has
    prune :: [(PoolStake, NonEmpty RelayAccessPoint)] -> Known -> Known
    prune pools Known { knTargets, knIPs } =
      let targets = Map.restrictKeys knTargets (srvNamesOf pools)
          names   = namesOf pools targets
      in Known targets (Map.filterWithKey (\(d, _) _ -> Set.member d names) knIPs)

    -- the index from what is known: each address, without its port, to the
    -- positions of the pools it belongs to
    indexOf :: [(PoolStake, NonEmpty RelayAccessPoint)] -> Known -> Map ntnAddr [Int]
    indexOf pools Known { knTargets, knIPs } =
      Map.map (Set.toAscList . Set.fromList) $ Map.fromListWith (++)
        [ (prAddressKey addr, [position])
        | (position, (_, rs)) <- zip [0 ..] pools
        , rp   <- NonEmpty.toList rs
        , addr <- addressesOf rp ]
      where
        addressesOf (RelayAccessAddress ip port) = [paToPeerAddr prDNS ip port]
        addressesOf (RelayAccessDomain d port)   = named d port
        addressesOf (RelayAccessSRVDomain d)     =
          concat [ named t port | (t, port) <- Map.findWithDefault [] d knTargets ]
        named d port = [ paToPeerAddr prDNS ip port
                       | ty <- [DNS.A, DNS.AAAA]
                       , ip <- Set.toList (Map.findWithDefault Set.empty (d, ty) knIPs) ]

-- | A socket address with its port removed: the index key, since a relay
-- behind a NAT may connect from an ephemeral port.
sockAddrWithoutPort :: Socket.SockAddr -> Socket.SockAddr
sockAddrWithoutPort (Socket.SockAddrInet  _ h)      = Socket.SockAddrInet  0 h
sockAddrWithoutPort (Socket.SockAddrInet6 _ f h s)  = Socket.SockAddrInet6 0 f h s
sockAddrWithoutPort addr                            = addr
