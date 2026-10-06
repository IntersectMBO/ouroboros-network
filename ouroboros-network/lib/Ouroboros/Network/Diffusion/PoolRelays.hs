{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
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
  , poolRelaysLookups
  , sockAddrWithoutPort
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeAsyncException (..))
import Control.Monad (unless, when)
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

data PoolRelaysArgs extraAPI ntnAddr resolver m = PoolRelaysArgs {
    prTracer       :: Tracer m TracePoolRelays,
    prConsensus    :: LedgerPeersConsensusInterface extraAPI m,
    prUseLedger    :: STM m UseLedgerPeers,
    prSnapshot     :: STM m (Maybe (LedgerPeerSnapshot BigLedgerPeers)),
    prSRVPrefix    :: SRVPrefix,
    prDNS          :: PeerActionsDNS ntnAddr resolver m,
    prAddressKey   :: ntnAddr -> ntnAddr,
    -- ^ the address as the index keys it: without its port, since a relay
    -- behind a NAT may connect from an ephemeral one
    prPollInterval :: DiffTime
    -- ^ how often the ledger list is read and its relays resolved
  }

-- | Lookups in flight at once: a thousand names every half hour, at a
-- trickle, on the thread's own workers, so the lookups the governor waits
-- for never queue behind them.
poolRelaysLookups :: Int
poolRelaysLookups = 2

data TracePoolRelays =
    PoolRelaysPolled Int Int Int Int
    -- ^ a poll through: pools in the list, buckets kept from the poll
    -- before, addresses indexed, and names kept as last known because
    -- their lookup failed for the moment
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

-- | What each name reaches: the positions and ports of the relays named so,
-- and of the services whose targets name it.
type Refs = Map DNS.Domain [(Int, Socket.PortNumber)]

-- | The index with its reasons: for each address the positions it reaches,
-- and by how many routes each, a route being a relay's name and port with
-- one of the name's records, or a literal address. The index is the
-- positions; the counts let an answer for one name change only what that
-- name reaches.
type Contributions ntnAddr = Map ntnAddr (Map Int Int)

-- | Follow the big-ledger pools: on every poll take the same selection the
-- ledger peers thread samples from, the ledger when young enough and the
-- snapshot when too old, install it, and resolve every relay in two stages,
-- each SRV name to its targets and then every name, the relays' own and the
-- targets, by record type. The second stage publishes after every answer,
-- so no name waits for another. An answer replaces what was known of a
-- name, a name that does not exist is forgotten, and any other failure
-- keeps what was known. A pool keeps its bucket from poll to poll by its
-- relay list, whatever its stake or position; a pool new to the list
-- starts empty, and one that left loses its bucket. With ledger peers
-- disabled there are no buckets at all.
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
    go (Followed Map.empty Map.empty Nothing) False (Known Map.empty Map.empty)
  where
    -- @cleared@ is whether the buckets were dropped for disabled ledger
    -- peers already. All three are forced on entry: left lazy, a failed
    -- poll's next state would hold its maps until a poll succeeds.
    go :: Followed -> Bool -> Known -> m Void
    go !followed !cleared !known = do
      useLedger <- atomically prUseLedger
      (followed', cleared', known') <-
        case useLedger of
             DontUseLedgerPeers -> do
               unless cleared $ do
                 now <- getMonotonicTime
                 _ <- atomically $ rebuild allowances now [] Map.empty
                 traceWith prTracer PoolRelaysDisabled
               return (followed, True, known)

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
               -- what is known serves at once; a pool new to the list
               -- starts empty
               now  <- getMonotonicTime
               kept <- atomically $ rebuild allowances now (map snd pools) (indexOf pools known)
               r <- try $ resolvePools pools known
               knownNow <- case r of
                 Left e | Just (SomeAsyncException _) <- fromException e -> throwIO e
                        | otherwise -> do
                            traceWith prTracer (PoolRelaysResolveFailed (show e))
                            return (prune pools known)
                 Right (knownNow, indexed, held) -> do
                   traceWith prTracer (PoolRelaysPolled (length pools) kept indexed held)
                   return knownNow
               return (followedNow, False, knownNow)

      threadDelay prPollInterval
      go followed' cleared' known'

    -- resolve the list's names in two stages, publishing after every
    -- answer of the second; what is known afterwards, pruned to the list,
    -- how many addresses the index has, and how many names were kept as
    -- last known
    resolvePools :: [(PoolStake, NonEmpty RelayAccessPoint)] -> Known -> m (Known, Int, Int)
    resolvePools pools known0 = do
      knownVar <- newTVarIO known0
      heldVar  <- newTVarIO (0 :: Int)
      withResolver $ \resolver ->
        lookups [ do r <- dnsLookupSRV acts DNSPoolRelay d conf resolver
                     atomically $ do
                       kn <- readTVar knownVar
                       let (targets, held) = learn d r (knTargets kn)
                       writeTVar knownVar kn { knTargets = targets }
                       when held $ modifyTVar heldVar (+ 1)
                | d <- Set.toList (srvNamesOf pools) ]
      -- the targets are settled, so what each name reaches is fixed for
      -- the second stage; the index is kept as contributions, published
      -- as it stands and then patched where an answer changes it
      kn1 <- readTVarIO knownVar
      let refs = refsOf pools (knTargets kn1)
      contribVar <- newTVarIO (contributionsOf pools refs (knIPs kn1))
      atomically $ readTVar contribVar >>= reindex allowances . indexFrom
      withResolver $ \resolver ->
        lookups [ do rs <- dnsLookupIPs acts DNSPoolRelay d conf resolver
                     atomically $ do
                       kn <- readTVar knownVar
                       let step (m, n) (ty, r) =
                             let (m', kept) = learn (d, ty) (Set.fromList . map fst <$> r) m
                             in (m', if kept then n + 1 else n)
                           (ips, held) = foldl step (knIPs kn, 0) rs
                           before = routesOf refs d (knIPs kn)
                           after  = routesOf refs d ips
                       writeTVar knownVar kn { knIPs = ips }
                       modifyTVar heldVar (+ held)
                       contrib <- addRoutes after . dropRoutes before <$> readTVar contribVar
                       writeTVar contribVar contrib
                       patchIndex allowances
                         [ (a, maybe [] Map.keys (Map.lookup a contrib))
                         | a <- Set.toList (Set.fromList (map fst (before ++ after))) ]
                | d <- Map.keys refs ]
      known   <- prune pools <$> readTVarIO knownVar
      indexed <- Map.size <$> readTVarIO contribVar
      held    <- readTVarIO heldVar
      return (known, indexed, held)

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

    -- the lookups in turn on 'poolRelaysLookups' workers; one that throws
    -- ends them all
    lookups :: [m ()] -> m ()
    lookups jobs = do
      queue <- newTVarIO jobs
      let worker = do
            next <- atomically $ stateTVar queue $ \case
                      []     -> (Nothing, [])
                      j : js -> (Just j, js)
            case next of
                 Nothing -> return ()
                 Just j  -> j >> worker
      replicateConcurrently_ poolRelaysLookups worker

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

    -- what each name reaches, given the SRV names' targets
    refsOf :: [(PoolStake, NonEmpty RelayAccessPoint)]
           -> Map DNS.Domain [(DNS.Domain, Socket.PortNumber)] -> Refs
    refsOf pools targets =
      Map.fromListWith (++)
        [ (d, [(position, port)])
        | (position, (_, rs)) <- zip [0 ..] pools
        , rp <- NonEmpty.toList rs
        , (d, port) <- case rp of
                            RelayAccessAddress {}    -> []
                            RelayAccessDomain d port -> [(d, port)]
                            RelayAccessSRVDomain s   -> Map.findWithDefault [] s targets ]

    -- the routes through a name: an address, without its port, and the
    -- position it reaches, one per record of the name and port referring
    -- to it
    routesOf :: Refs -> DNS.Domain -> Map (DNS.Domain, DNS.TYPE) (Set IP) -> [(ntnAddr, Int)]
    routesOf refs d ips =
      [ (prAddressKey (paToPeerAddr prDNS ip port), position)
      | (position, port) <- Map.findWithDefault [] d refs
      , ty <- [DNS.A, DNS.AAAA]
      , ip <- Set.toList (Map.findWithDefault Set.empty (d, ty) ips) ]

    -- the whole index as contributions: the literal relays, and every
    -- name's routes from what is known of it
    contributionsOf :: [(PoolStake, NonEmpty RelayAccessPoint)] -> Refs
                    -> Map (DNS.Domain, DNS.TYPE) (Set IP) -> Contributions ntnAddr
    contributionsOf pools refs ips =
      addRoutes (literals ++ concat [ routesOf refs d ips | d <- Map.keys refs ]) Map.empty
      where
        literals = [ (prAddressKey (paToPeerAddr prDNS ip port), position)
                   | (position, (_, rs)) <- zip [0 ..] pools
                   , RelayAccessAddress ip port <- NonEmpty.toList rs ]

    addRoutes, dropRoutes :: [(ntnAddr, Int)] -> Contributions ntnAddr -> Contributions ntnAddr
    addRoutes rs m =
      foldr (\(a, p) -> Map.insertWith (Map.unionWith (+)) a (Map.singleton p 1)) m rs
    dropRoutes rs m =
      foldr (\(a, p) -> Map.update (nonEmpty . Map.update decrement p) a) m rs
      where
        decrement n = if n <= 1 then Nothing else Just (n - 1)
        nonEmpty ps = if Map.null ps then Nothing else Just ps

    indexFrom :: Contributions ntnAddr -> Map ntnAddr [Int]
    indexFrom = Map.map Map.keys

    -- the index from what is known: each address, without its port, to the
    -- positions of the pools it belongs to
    indexOf :: [(PoolStake, NonEmpty RelayAccessPoint)] -> Known -> Map ntnAddr [Int]
    indexOf pools Known { knTargets, knIPs } =
      indexFrom (contributionsOf pools (refsOf pools knTargets) knIPs)

-- | A socket address with its port removed: the index key, since a relay
-- behind a NAT may connect from an ephemeral port.
sockAddrWithoutPort :: Socket.SockAddr -> Socket.SockAddr
sockAddrWithoutPort (Socket.SockAddrInet  _ h)      = Socket.SockAddrInet  0 h
sockAddrWithoutPort (Socket.SockAddrInet6 _ f h s)  = Socket.SockAddrInet6 0 f h s
sockAddrWithoutPort addr                            = addr
