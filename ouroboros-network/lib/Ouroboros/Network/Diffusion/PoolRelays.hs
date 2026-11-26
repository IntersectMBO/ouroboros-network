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
  , poolRelaysMaxRelays
  , poolRelaysMaxAddresses
  , poolRelaysMaxEntries
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
import Data.Bits (shiftR)
import Data.List as List (foldl')
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
    prTracer        :: Tracer m TracePoolRelays,
    prConsensus     :: LedgerPeersConsensusInterface extraAPI m,
    prUseLedger     :: STM m UseLedgerPeers,
    prSnapshot      :: STM m (Maybe (LedgerPeerSnapshot BigLedgerPeers)),
    prSRVPrefix     :: SRVPrefix,
    prDNS           :: PeerActionsDNS ntnAddr resolver m,
    prAddressKey    :: ntnAddr -> ntnAddr,
    -- ^ the address as the index keys it: without its port, since a relay
    -- behind a NAT may connect from an ephemeral one
    prMaxRelays     :: Int,
    -- ^ a pool's relays that enter the index: the first so many of its
    -- list, an SRV name standing for its targets; 'poolRelaysMaxRelays'
    prMaxAddresses  :: Int,
    -- ^ addresses kept per name and record type, the smallest so many of
    -- an answer; 'poolRelaysMaxAddresses'
    prMaxEntries    :: Int,
    -- ^ addresses the index holds at most, the largest pools' first;
    -- 'poolRelaysMaxEntries'
    prPollInterval  :: DiffTime,
    -- ^ how often the ledger list is read and its relays resolved
    prRetryInterval :: DiffTime
    -- ^ how soon after a poll that failed, or that indexed nothing of a
    -- list with pools, as at boot before DNS answers
  }

-- | Lookups in flight at once: a thousand names every hour, at a
-- trickle, on the thread's own workers, so the lookups the governor waits
-- for never queue behind them.
poolRelaysLookups :: Int
poolRelaysLookups = 2

-- | What one pool can put into the index, and what the index holds in all.
-- A DNS answer over TCP carries thousands of records, a pool's relay list
-- hundreds of entries, and the thread keeps what it learns across polls,
-- so without bounds one registration could fill a node's memory. On
-- mainnet (2026-11) no pool lists more than 16 relays, no name answers
-- more than 8 addresses of a type, and the big-ledger pools resolve to
-- under a thousand addresses in all.
poolRelaysMaxRelays, poolRelaysMaxAddresses, poolRelaysMaxEntries :: Int
poolRelaysMaxRelays    = 16
poolRelaysMaxAddresses = 8
poolRelaysMaxEntries   = 8192

data TracePoolRelays =
    PoolRelaysPolled Int Int Int Int Int Int
    -- ^ a poll through: pools in the list, buckets kept from the poll
    -- before, addresses indexed, names kept as last known because their
    -- lookup failed for the moment, relays, targets and addresses left out
    -- by the bounds per pool and per name, and addresses beyond the budget,
    -- left out of the index
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
-- so that a lookup failing for the moment costs nothing, cut to the bounds
-- as learnt, and pruned to the names the list has.
data Known = Known {
    knTargets :: !(Map DNS.Domain [(DNS.Domain, Socket.PortNumber)]),
    knIPs     :: !(Map (DNS.Domain, DNS.TYPE) (Set IP))
  }

-- | What each name reaches: the positions and ports of the relays named so,
-- and of the services whose targets name it.
type Refs = Map DNS.Domain [(Int, Socket.PortNumber)]

-- | A relay as the index takes it: a literal address, or a name, with its
-- port.
type Entry = Either (IP, Socket.PortNumber) (DNS.Domain, Socket.PortNumber)

-- | A pool's relays that enter the index: the first @most@ of its list in
-- registration order, an SRV name standing for its targets in priority
-- order, each taking a slot; and how many were left out.
relaysOf :: Int -> Map DNS.Domain [(DNS.Domain, Socket.PortNumber)]
         -> NonEmpty RelayAccessPoint -> ([Entry], Int)
relaysOf most targets rs = (take most entries, max 0 (length entries - most))
  where
    entries = concatMap expand (NonEmpty.toList rs)
    expand (RelayAccessAddress ip port) = [Left (ip, port)]
    expand (RelayAccessDomain d port)   = [Right (d, port)]
    expand (RelayAccessSRVDomain s)     = map Right (Map.findWithDefault [] s targets)

-- | The index with its reasons: for each address the positions it reaches,
-- and by how many routes each, a route being a relay's name and port with
-- one of the name's records, or a literal address; the counts let an
-- answer for one name change only what that name reaches. The index is
-- the positions of the first 'cBudget' addresses by rank, an address
-- ranked by the lowest position it reaches and then by itself, so the
-- largest pools' addresses are held and the index is the same whatever
-- order the answers came in.
data Contributions ntnAddr = Contributions {
    cRoutes :: !(Map ntnAddr (Map Int Int)),
    cRank   :: !(Set (Int, ntnAddr)),
    cBudget :: !Int
  }

emptyContributions :: Int -> Contributions ntnAddr
emptyContributions = Contributions Map.empty Set.empty

addRoute :: Ord ntnAddr => (ntnAddr, Int) -> Contributions ntnAddr -> Contributions ntnAddr
addRoute (a, p) c@Contributions { cRoutes = routes, cRank = rank } =
  case Map.lookup a routes of
       Just ps ->
         let q = fst (Map.findMin ps)
         in c { cRoutes = Map.insert a (Map.insertWith (+) p 1 ps) routes,
                cRank   = if p < q then Set.insert (p, a) (Set.delete (q, a) rank) else rank }
       Nothing ->
         c { cRoutes = Map.insert a (Map.singleton p 1) routes, cRank = Set.insert (p, a) rank }

dropRoute :: Ord ntnAddr => (ntnAddr, Int) -> Contributions ntnAddr -> Contributions ntnAddr
dropRoute (a, p) c@Contributions { cRoutes = routes, cRank = rank } =
  case Map.lookup a routes of
       Nothing -> c
       Just ps ->
         let q   = fst (Map.findMin ps)
             ps' = Map.update (\n -> if n <= 1 then Nothing else Just (n - 1)) p ps
         in if Map.null ps'
               then c { cRoutes = Map.delete a routes, cRank = Set.delete (q, a) rank }
               else let q' = fst (Map.findMin ps')
                    in c { cRoutes = Map.insert a ps' routes,
                           cRank   = if q' == q then rank
                                                else Set.insert (q', a) (Set.delete (q, a) rank) }

addRoutes, dropRoutes :: Ord ntnAddr => [(ntnAddr, Int)] -> Contributions ntnAddr -> Contributions ntnAddr
addRoutes rs c  = List.foldl' (flip addRoute) c rs
dropRoutes rs c = List.foldl' (flip dropRoute) c rs

-- | The addresses the index holds: the first 'cBudget' by rank.
published :: Ord ntnAddr => Contributions ntnAddr -> Set ntnAddr
published Contributions { cRank, cBudget } = Set.fromList (map snd (Set.toList (Set.take cBudget cRank)))

indexFrom :: Ord ntnAddr => Contributions ntnAddr -> Map ntnAddr [Int]
indexFrom c = Map.map Map.keys (Map.restrictKeys (cRoutes c) (published c))

-- | An address's entry: its positions when the index holds it, else none.
entryOf :: Ord ntnAddr => Contributions ntnAddr -> ntnAddr -> [Int]
entryOf Contributions { cRoutes, cRank, cBudget } a =
  case Map.lookup a cRoutes of
       Just ps | Just i <- Set.lookupIndex (fst (Map.findMin ps), a) cRank, i < cBudget -> Map.keys ps
       _ -> []

-- | The addresses held by one of two states and not the other: those that
-- crossed the budget's edge, none while the budget does not bind.
crossed :: Ord ntnAddr => Contributions ntnAddr -> Contributions ntnAddr -> Set ntnAddr
crossed before after
  | Set.size (cRank before) <= cBudget before && Set.size (cRank after) <= cBudget after = Set.empty
  | otherwise = (pb `Set.difference` pa) `Set.union` (pa `Set.difference` pb)
  where
    pb = published before
    pa = published after

-- | Follow the big-ledger pools: on every poll take the same selection the
-- ledger peers thread samples from, the ledger when young enough and the
-- snapshot when too old, install it, and resolve every relay in two stages,
-- each SRV name to its targets and then every name, the relays' own and the
-- targets, by record type. The second stage publishes after every answer,
-- so no name waits for another. An answer replaces what was known of a
-- name, a name that does not exist is forgotten, and any other failure
-- keeps what was known. The index is bounded: a pool's first
-- 'prMaxRelays' relays, an SRV name standing for its targets, the
-- 'prMaxAddresses' smallest addresses of a name per record type, and
-- 'prMaxEntries' addresses in all, the largest pools' first. A pool keeps
-- its bucket from poll to poll by its
-- relay list, whatever its stake or position; a pool new to the list
-- starts empty, and one that left loses its bucket. With ledger peers
-- disabled there are no buckets at all. A poll that failed, or that indexed
-- nothing of a list with pools, is followed by the next after the retry
-- interval rather than the poll interval, and a change of the ledger peers
-- setting ends the wait between polls.
poolRelaysThread :: forall extraAPI ntnAddr resolver m.
                    ( MonadAsync m
                    , MonadCatch m
                    , MonadTimer m
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
      (followed', cleared', known', soon) <-
        case useLedger of
             DontUseLedgerPeers -> do
               unless cleared $ do
                 _ <- atomically $ rebuild allowances [] Map.empty
                 traceWith prTracer PoolRelaysDisabled
               return (followed, True, known, False)

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
               kept <- atomically $ rebuild allowances (map snd pools) (indexOf pools known)
               knownVar <- newTVarIO known
               r <- try $ resolvePools pools knownVar
               (knownNow, soon) <- case r of
                 Left e | Just (SomeAsyncException _) <- fromException e -> throwIO e
                        | otherwise -> do
                            traceWith prTracer (PoolRelaysResolveFailed (show e))
                            -- what the poll learned before the failure is
                            -- kept, as the index already shows it
                            kn <- readTVarIO knownVar
                            return (prune pools kn, True)
                 Right (knownNow, indexed, held, asked, got, cut, unplaced) -> do
                   traceWith prTracer (PoolRelaysPolled (length pools) kept indexed held cut unplaced)
                   -- lookups made and none answered: DNS not answering, as
                   -- at boot, whatever literal addresses the list indexed.
                   -- A name kept as last known among others that answered
                   -- is the ordinary case. An empty list, before the ledger
                   -- or a snapshot names a pool, looks nothing up and is
                   -- polled again soon as well
                   return (knownNow, null pools || (asked > 0 && got == 0))
               return (followedNow, False, knownNow, soon)

      -- the wait to the next poll, ended early by a change of the setting:
      -- the timer and the setting read in one transaction; the timer goes
      -- with the wait, however it ends
      bracket (registerDelayCancellable (if soon then prRetryInterval else prPollInterval))
              snd $ \(readTimer, _) ->
        atomically $ (readTimer >>= check . (/= TimeoutPending))
            `orElse` (prUseLedger >>= check . (/= useLedger))
      go followed' cleared' known'

    -- resolve the list's names in two stages, each answer learnt into
    -- @knownVar@ as it comes, publishing after every
    -- answer of the second; what is known afterwards, pruned to the list,
    -- how many addresses the index has, how many names were kept as last
    -- known, how many lookups were made and how many of them answered,
    -- what the bounds per pool and per name left out, and what lies beyond
    -- the budget
    resolvePools :: [(PoolStake, NonEmpty RelayAccessPoint)] -> StrictTVar m Known
                 -> m (Known, Int, Int, Int, Int, Int, Int)
    resolvePools pools knownVar = do
      heldVar     <- newTVarIO (0 :: Int)
      askedVar    <- newTVarIO (0 :: Int)
      answeredVar <- newTVarIO (0 :: Int)
      cutVar      <- newTVarIO (0 :: Int)
      let count r = do modifyTVar askedVar (+ 1)
                       when (answered r) $ modifyTVar answeredVar (+ 1)
      withResolver $ \resolver ->
        lookups [ do r <- dnsLookupSRV acts DNSPoolRelay d conf resolver
                     atomically $ do
                       count r
                       kn <- readTVar knownVar
                       -- the targets cut to what a pool can hold
                       let (targets, held) = learn d (take prMaxRelays <$> r) (knTargets kn)
                       writeTVar knownVar kn { knTargets = targets }
                       when held $ modifyTVar heldVar (+ 1)
                       modifyTVar cutVar (+ either (const 0) (\ts -> max 0 (length ts - prMaxRelays)) r)
                | d <- Set.toList (srvNamesOf pools) ]
      -- the targets are settled, so what each name reaches is fixed for
      -- the second stage; the index is kept as contributions, published
      -- as it stands and then patched where an answer changes it
      kn1 <- readTVarIO knownVar
      let (entries, cutRelays) = entriesOf pools (knTargets kn1)
          refs                 = refsOf entries
          contrib0             = contributionsOf entries (knIPs kn1)
      contribVar <- newTVarIO contrib0
      atomically $ do
        modifyTVar cutVar (+ cutRelays)
        reindex allowances (indexFrom contrib0)
      -- the names in the order of the pools they reach, the largest first,
      -- so that under a binding budget theirs are the lookups made first
      withResolver $ \resolver ->
        lookups [ do rs <- dnsLookupIPs acts DNSPoolRelay d conf resolver
                     atomically $ do
                       mapM_ (count . snd) rs
                       kn <- readTVar knownVar
                       -- an answer cut to the smallest addresses a name
                       -- may hold per type
                       let step (m, n, o) (ty, r) =
                             let full       = Set.fromList . map fst <$> r
                                 (m', kept) = learn (d, ty) (Set.take prMaxAddresses <$> full) m
                                 cut        = either (const 0) (\s -> max 0 (Set.size s - prMaxAddresses)) full
                             in (m', if kept then n + 1 else n, o + cut)
                           (ips, held, over) = List.foldl' step (knIPs kn, 0, 0) rs
                           before = routesOf refs d (knIPs kn)
                           after  = routesOf refs d ips
                       writeTVar knownVar kn { knIPs = ips }
                       modifyTVar heldVar (+ held)
                       modifyTVar cutVar (+ over)
                       contrib <- readTVar contribVar
                       let contrib' = addRoutes after (dropRoutes before contrib)
                       writeTVar contribVar contrib'
                       -- the entries of what the answer touched, and of
                       -- what crossed the budget's edge either way
                       patchIndex allowances
                         [ (a, entryOf contrib' a)
                         | a <- Set.toList (Set.fromList (map fst (before ++ after))
                                            `Set.union` crossed contrib contrib') ]
                | d <- byPosition refs ]
      known   <- prune pools <$> readTVarIO knownVar
      contrib <- readTVarIO contribVar
      held    <- readTVarIO heldVar
      asked   <- readTVarIO askedVar
      got     <- readTVarIO answeredVar
      cut     <- readTVarIO cutVar
      let indexed  = min (cBudget contrib) (Map.size (cRoutes contrib))
          unplaced = Map.size (cRoutes contrib) - indexed
      return (known, indexed, held, asked, got, cut, unplaced)

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

    -- whether DNS answered a lookup: records, or a definitive error
    answered :: Either DNS.DNSError v -> Bool
    answered (Right _) = True
    answered (Left e)  = definitive e

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

    -- the SRV names to look up: among a pool's first 'prMaxRelays' relays,
    -- the only ones whose targets could enter the index
    srvNamesOf :: [(PoolStake, NonEmpty RelayAccessPoint)] -> Set DNS.Domain
    srvNamesOf pools = Set.fromList [ d | (_, rs) <- pools
                                        , RelayAccessSRVDomain d <- take prMaxRelays (NonEmpty.toList rs) ]

    -- every pool's entries with its position, the bounds per pool applied,
    -- and how many relays and targets they left out
    entriesOf :: [(PoolStake, NonEmpty RelayAccessPoint)]
              -> Map DNS.Domain [(DNS.Domain, Socket.PortNumber)] -> ([(Int, Entry)], Int)
    entriesOf pools targets =
      ( [ (position, e) | (position, (es, _)) <- perPool, e <- es ]
      , sum [ n | (_, (_, n)) <- perPool ] )
      where
        perPool = [ (position, relaysOf prMaxRelays targets rs)
                  | (position, (_, rs)) <- zip [0 ..] pools ]

    -- the names to look up: the relays' own and the targets of its SRV
    -- names, within the bounds
    namesOf :: [(PoolStake, NonEmpty RelayAccessPoint)]
            -> Map DNS.Domain [(DNS.Domain, Socket.PortNumber)] -> Set DNS.Domain
    namesOf pools targets =
      Set.fromList [ d | (_, Right (d, _)) <- fst (entriesOf pools targets) ]

    -- forget the names the list no longer has
    prune :: [(PoolStake, NonEmpty RelayAccessPoint)] -> Known -> Known
    prune pools Known { knTargets, knIPs } =
      let targets = Map.restrictKeys knTargets (srvNamesOf pools)
          names   = namesOf pools targets
      in Known targets (Map.filterWithKey (\(d, _) _ -> Set.member d names) knIPs)

    -- what each name reaches, from the entries
    refsOf :: [(Int, Entry)] -> Refs
    refsOf entries =
      Map.fromListWith (++) [ (d, [(position, port)]) | (position, Right (d, port)) <- entries ]

    -- the names by the lowest position they reach
    byPosition :: Refs -> [DNS.Domain]
    byPosition refs =
      map snd (Set.toAscList (Set.fromList [ (minimum (map fst ps), d) | (d, ps) <- Map.toList refs ]))

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
    contributionsOf :: [(Int, Entry)] -> Map (DNS.Domain, DNS.TYPE) (Set IP) -> Contributions ntnAddr
    contributionsOf entries ips =
      addRoutes (literals ++ concat [ routesOf refs d ips | d <- byPosition refs ])
                (emptyContributions prMaxEntries)
      where
        refs     = refsOf entries
        literals = [ (prAddressKey (paToPeerAddr prDNS ip port), position)
                   | (position, Left (ip, port)) <- entries ]

    -- the index from what is known: each address, without its port, to the
    -- positions of the pools it belongs to
    indexOf :: [(PoolStake, NonEmpty RelayAccessPoint)] -> Known -> Map ntnAddr [Int]
    indexOf pools Known { knTargets, knIPs } =
      indexFrom (contributionsOf (fst (entriesOf pools knTargets)) knIPs)

-- | A socket address as the index keys it: its port removed, since a relay
-- behind a NAT may connect from an ephemeral port, an IPv4 address a
-- dual-stack socket presents as IPv4-mapped IPv6 (::ffff:a.b.c.d) as the
-- IPv4 address, the form an A record resolves to, and an IPv6 address
-- without the flow label and scope an accepting socket may carry, which
-- a resolved address never has.
sockAddrWithoutPort :: Socket.SockAddr -> Socket.SockAddr
sockAddrWithoutPort (Socket.SockAddrInet  _ h)      = Socket.SockAddrInet  0 h
sockAddrWithoutPort (Socket.SockAddrInet6 _ _ (0, 0, 0xffff, h) _) =
  Socket.SockAddrInet 0 (Socket.tupleToHostAddress
                           ( fromIntegral (h `shiftR` 24), fromIntegral (h `shiftR` 16)
                           , fromIntegral (h `shiftR` 8),  fromIntegral h ))
sockAddrWithoutPort (Socket.SockAddrInet6 _ _ h _)  = Socket.SockAddrInet6 0 0 h 0
sockAddrWithoutPort addr                            = addr
