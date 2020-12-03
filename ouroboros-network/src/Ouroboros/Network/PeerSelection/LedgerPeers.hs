{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.LedgerPeers (
    DomainAddress (..),
    LedgerPeersConsensusInterface (..),
    RelayAddress (..),
    PoolStake,
    runLedgerPeers,
    TraceLedgerPeers (..),
    pickPeers,
    ackPoolStake,
    addPeerMetric,
    initPeerMetric,
    PeerMetric,
    simpleResolve,
    pickWorstPeer,
    upstreamyness,

    Socket.PortNumber
    ) where

import           Control.Monad (foldM)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, traceWith)
import qualified Data.IP as IP
import           Data.List (foldl', intercalate, nub)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isNothing, fromJust, catMaybes)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Ratio
import           Data.Word
import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
import           Network.Socket (SockAddr)
import           System.Random

import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.PeerSelection.RootPeersDNS (DomainAddress (..))
import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.Dns

import           Text.Printf

data LedgerPeersConsensusInterface m a = LedgerPeersConsensusInterface {
      lpGetPeers :: (SlotNo -> STM m (SlotNo, [(PoolStake, NonEmpty RelayAddress)]))
    }

data TraceLedgerPeers =
      PickedPeer !RelayAddress !AckPoolStake ! PoolStake
    | PickedPeers !Int ![RelayAddress]
    | FetchingNewLedgerState !SlotNo !Int
    | LedgerPeersXXX !String

type PeerMetric m a = StrictTVar m (Map a (Map SlotNo DiffTime))

initPeerMetric :: MonadSTM m => m (PeerMetric m a)
initPeerMetric = newTVarIO Map.empty

addPeerMetric :: forall m a.
       ( MonadSTM m
       , Ord a
       )
    => PeerMetric m a
    -> ConnectionId a
    -> SlotNo
    -> DiffTime
    -> STM m ()
addPeerMetric db conId slotNo dTime = do
    readTVar db >>= Map.alterF fn (remoteAddress conId) >>= writeTVar db
  where
    fn :: Maybe (Map SlotNo DiffTime) -> STM m (Maybe (Map SlotNo DiffTime))
    fn Nothing = return $ Just $ Map.singleton slotNo dTime
    fn (Just pm) = return $ Just $ Map.insert slotNo dTime pm


instance Show TraceLedgerPeers where
    show (PickedPeer addr ackStake stake) =
        printf "PickedPeer %s ack stake %s ( %.04f) relative stake %s ( %.04f )"
            (show addr)
            (show ackStake) (fromRational ackStake :: Double)
            (show stake) (fromRational stake :: Double)
    show (PickedPeers n peers) =
        printf "PickedPeers %d %s" n (show peers)
    show (FetchingNewLedgerState tip cnt) =
        printf "Fetching new ledgerstate at slot %s , %d registered pools"
            (show tip)
            cnt
    show (LedgerPeersXXX msg) = msg


data RelayAddress = RelayAddressDomain DomainAddress
                  | RelayAddressAddr IP.IP Socket.PortNumber
                  deriving (Show, Eq, Ord)

-- | The relative stake of the stakepool. A value in the [0, 1] range.
type PoolStake = Rational

-- | The relative stake of the stakepool and all preceeding pools. A value in the range [0, 1].
type AckPoolStake = Rational

-- | Convert a list of pools with stake to a Map keyed on the ackumulated stake.
-- 
ackPoolStake :: [(PoolStake, NonEmpty RelayAddress)]
             -> Map Rational (PoolStake, NonEmpty RelayAddress)
ackPoolStake pl =
    let pl' = reRelativeStake pl
        ackList = snd $ foldl' (\(as, ps) (s, rs) -> (as + s, (as + s, (s, rs)):ps)) (0, []) pl' in
    Map.fromList ackList

-- | Not all stake pools have valid/usable relay information. This means that we need to
-- recalculate the relative stake for each pool.
reRelativeStake :: [(PoolStake, NonEmpty RelayAddress)]
                -> [(PoolStake, NonEmpty RelayAddress)]
reRelativeStake pl =
    let total = sum $ map fst pl in
    map (\(s, rls) -> (s / total, rls)) pl

-- try to pick n random peers
pickPeers :: forall m. Monad m
          => StdGen
          -> Tracer m TraceLedgerPeers
          -> Map Rational (PoolStake, NonEmpty RelayAddress)
          -> Word16
          -> m (StdGen, [RelayAddress])
pickPeers inRng _ pools _ | Map.null pools = return (inRng, [])
pickPeers inRng tracer pools cnt = go inRng cnt []
  where
    go :: StdGen -> Word16 -> [RelayAddress] -> m (StdGen, [RelayAddress])
    go rng 0 picked = return (rng, picked)
    go rng n picked =
        let (r :: Word64, rng') = random rng
            d = maxBound :: Word64
            x = fromIntegral r % fromIntegral d
            !pick_m = Map.lookupGE x pools in
        case pick_m of
             Nothing -> go rng' (n - 1) picked -- XXX We failed pick a peer. Shouldn't this be an error?
             Just (ackStake, (stake, relays)) -> do
                 let (ix, rng'') = randomR (0, NonEmpty.length relays - 1) rng'
                     relay = relays NonEmpty.!! ix
                 traceWith tracer $ PickedPeer relay ackStake stake
                 go rng'' (n - 1) (relay : picked)


pickWorstPeerSTM :: forall m.
       MonadSTM m
    => PeerMetric m SockAddr
    -> SlotNo
    -> Int
    -> STM m [(SockAddr, Int)]
pickWorstPeerSTM pmVar minSlotNo peersNo = do
    pm <- readTVar pmVar
    -- First remove all samples older than minSlotNo
    let pm' = Map.map minFn pm
        -- pruned = pickWorstPeer pm' peersNo
        pruned = upstreamyness pm' peersNo
        prunedAddrs = map fst pruned
        pm'' = Map.filterWithKey (\p _ -> List.all (p /=) prunedAddrs) pm'
    writeTVar pmVar pm''
    return pruned
  where
    minFn :: Map SlotNo DiffTime -> Map SlotNo DiffTime
    minFn pms = Map.filterWithKey (\k _ -> k >= minSlotNo) pms

upstreamyness
    :: forall a no ts.
       ( Ord a
       , Ord no
       , Ord ts
       )
    => Map a (Map no ts)
    -> Int
    -> [(a, Int)]
upstreamyness pm peersNo =
    let (pmZero, pmNotZero) = Map.partition Map.null pm
        zeroAs = zip (Map.keys pmZero) (repeat 0) in
    if Map.size pmNotZero <= peersNo
       then zeroAs
       else
           let slotMap = Map.foldlWithKey' foldByPeer Map.empty pmNotZero
               cntMap = Map.foldl' countMin (Map.map (const 0) pmNotZero) slotMap
               sortedPeers = List.sortBy (\(_, a) (_,b) -> compare a b) $ Map.toList cntMap
               poorPeers = take (Map.size pmNotZero - peersNo) sortedPeers in
           zeroAs ++ poorPeers
  where
    countMin :: Map a Int
             -> [(ts, a)]
             -> Map a Int
    countMin mm samples =
        let minPeer = snd $ List.minimumBy (\(a,_) (b,_) -> compare a b) samples in
        Map.alter fn minPeer mm
      where
        fn :: Maybe Int -> Maybe Int
        fn Nothing = Just 1
        fn (Just cnt) = Just $ cnt + 1

    foldByPeer :: Map no [(ts, a)]
               ->  a
               -> Map no ts
               -> Map no [(ts, a)]
    foldByPeer bestSlotM peer peerMetric = Map.foldlWithKey' foldBySlot bestSlotM peerMetric
      where
        foldBySlot :: Map no [(ts, a)]
                   -> no
                   -> ts
                   -> Map no [(ts, a)]
        foldBySlot bestSlotM' slotNo dTime =
            case Map.lookup slotNo bestSlotM' of
                 Nothing -> Map.insert slotNo [(dTime, peer)] bestSlotM'
                 Just ov -> Map.insert slotNo ((dTime, peer) : ov) bestSlotM'





pickWorstPeer
    :: Map SockAddr (Map SlotNo DiffTime)
    -> Int
    -> [SockAddr]
pickWorstPeer pm peersNo =
    let (pmZero, pmNotZero) = Map.partition Map.null pm in
    if Map.size pmNotZero <= peersNo
       then Map.keys pmZero
       else
           let peers = Map.keys pmNotZero
               slotMap = Map.foldlWithKey' foldByPeer Map.empty pmNotZero
               peerUtility = map (\p -> Map.foldlWithKey' withOutPeer (p, 0) slotMap)  peers
               peerUtilitySorted = List.sortBy (\a b -> compare b a) $ peerUtility
               peersToKill = take (Map.size pmNotZero - peersNo) $ map fst peerUtilitySorted in
           (peersToKill ++ Map.keys pmZero)

  where
    withOutPeer :: (SockAddr, DiffTime)
                -> SlotNo
                -> [(DiffTime, SockAddr)]
                -> (SockAddr, DiffTime)
    withOutPeer (peer,res) _ samples =
        let samples' = filter (\(_, p) -> peer /= p) samples in
        if null samples'
           then
               let mt = fst $ head samples in
               (peer, mt + res)
           else
               let mt = fst $ List.minimumBy (\(a,_) (b,_) -> compare a b) samples' in
               (peer, mt + res)


    foldByPeer :: Map SlotNo [(DiffTime, SockAddr)]
               -> SockAddr
               -> Map SlotNo DiffTime
               -> Map SlotNo [(DiffTime, SockAddr)]
    foldByPeer bestSlotM peer peerMetric = Map.foldlWithKey' foldBySlot bestSlotM peerMetric
      where
        foldBySlot :: Map SlotNo [(DiffTime, SockAddr)]
                   -> SlotNo
                   -> DiffTime
                   -> Map SlotNo [(DiffTime, SockAddr)]
        foldBySlot bestSlotM' slotNo dTime =
            case Map.lookup slotNo bestSlotM' of
                 Nothing -> Map.insert slotNo [(dTime, peer)] bestSlotM'
                 Just ov -> Map.insert slotNo ((dTime, peer) : ov) bestSlotM'



runLedgerPeers :: forall m.
                      ( MonadAsync m
                      , MonadDelay m
                      , MonadTime m
                      )
               => StdGen
               -> Tracer m TraceLedgerPeers
               -> LedgerPeersConsensusInterface m SockAddr
               -> PeerMetric m SockAddr
               -> (IPSubscriptionTarget -> m ())
               -> (DnsSubscriptionTarget -> m ())
               -> (RelayAddress -> m (Maybe SockAddr))
               -> m ()
runLedgerPeers inRng tracer LedgerPeersConsensusInterface{..} peerMetric runIP runDns doResolve =
    go inRng (2*desirvedActivePeers + 4) Nothing Map.empty Map.empty
  where
    desirvedActivePeers :: Int
    desirvedActivePeers = 12

    go :: StdGen -> Int -> Maybe Time -> Map Rational (PoolStake, NonEmpty RelayAddress)
       -> Map SockAddr (Async m ()) -> m ()
    go rng pickNo oldTs_m peerMap activePeers = do
        let peerListLifeTime = if Map.null peerMap then 30
                                                   else 200 -- XXX moar
            useLedgerAfter = 4492800 + 2*21600 -- XXX two epochs after shelley hard fork
        !now <- getMonotonicTime
        (!peerMap', ts_m) <-
            if isNothing oldTs_m || diffTime now (fromJust oldTs_m) > peerListLifeTime
               then do
                   (tip, plx) <- atomically $ lpGetPeers useLedgerAfter
                   let pl = ackPoolStake plx
                   !now' <- getMonotonicTime
                   traceWith tracer $ FetchingNewLedgerState tip $ Map.size pl
                   traceWith tracer $ LedgerPeersXXX $ concatMap dumpPoolInfo $ take 32 $ filter (\(_, l) -> NonEmpty.length l == 1) plx
                   return (pl, Just now')
               else return (peerMap, oldTs_m)

        (rng', !pickedPeers) <- pickPeers rng tracer peerMap' $ fromIntegral pickNo
        traceWith tracer $ PickedPeers pickNo pickedPeers
        {-let (ipTarget, dnsTargets) = foldl' peersToSubTarget
                                                 (IPSubscriptionTarget [] 0 , []) $ nub pickedPeers
        ipAid <- async $ runIP ipTarget

        dnsAids <- sequence [async $ runDns peer | peer <- dnsTargets]-}

        newAddrs <- catMaybes <$> (mapM doResolve pickedPeers)
        activePeers' <- foldM startSub activePeers newAddrs

        -- XXX let it run for 3 minutes, then repeat with a new set of random peers.
        ttlAid <- async (threadDelay $ 1 * 3600)

        wait ttlAid
        currentSlot <- getCurrentSlotNo

        worstPeersWithCnt <- atomically $ pickWorstPeerSTM peerMetric (currentSlot - 1 * 3600) $ fromIntegral desirvedActivePeers
        let worstPeers = map fst worstPeersWithCnt
        traceWith tracer $ LedgerPeersXXX $ printf "worst peers %s" (show worstPeersWithCnt)

        cpm <- atomically $ readTVar peerMetric
        traceWith tracer $ LedgerPeersXXX $ dumpPeers cpm

        let inactivePeers = Map.keys $ Map.filterWithKey (\k _ -> Map.notMember k cpm) activePeers'
            killPeers = nub $ worstPeers ++ inactivePeers
        traceWith tracer $ LedgerPeersXXX $ printf "going to kill %s" (show killPeers)


       -- traceWith tracer $ LedgerPeersXXX $ printf "peerUtility length %d" (length peerUtility)
       -- traceWith tracer $ LedgerPeersXXX $ show peerUtility
        --traceWith tracer $ LedgerPeersXXX $ dumpMinPeers pm

        activePeers'' <- foldM killPeer activePeers' killPeers

        let pickNo' = max 4 (desirvedActivePeers - Map.size activePeers'' + 4)

        -- _ <- waitAnyCancel (ipAid : dnsAids)
        go rng' pickNo' ts_m peerMap' activePeers''

    killPeer activePeers addr =
        case Map.lookup addr activePeers of
             Nothing  -> return activePeers -- XXX error ?
             Just aid -> do
                 cancel aid
                 return $ Map.delete addr activePeers

    startSub activePeers addr =
        case Map.lookup addr activePeers of
             Nothing -> do
               aid <- async $ runIP $ IPSubscriptionTarget [addr] 1
               return $ Map.insert addr aid activePeers
             Just _  -> return activePeers

    peersToSubTarget :: (IPSubscriptionTarget, [DnsSubscriptionTarget])
          -> RelayAddress
          -> (IPSubscriptionTarget, [DnsSubscriptionTarget])
    peersToSubTarget (ipSub, dnsSubs) (RelayAddressAddr addr port) =
        ( ipSub { ispIps = IP.toSockAddr (addr, port) : ispIps ipSub
               , ispValency = 1 + ispValency ipSub
               }
        , dnsSubs
        )
    peersToSubTarget (ipSub, dnsSubs) (RelayAddressDomain domain) =
        let dnsTarget = DnsSubscriptionTarget {
                              dstDomain  = daDomain domain
                            , dstPort    = daPortNumber domain
                            , dstValency = 1
                            } in
        ( ipSub
        , dnsTarget : dnsSubs
        )

    getCurrentSlotNo :: m SlotNo
    getCurrentSlotNo = do
        now <- getCurrentTime
        let firstShelleySlot = 4492800
            shelleyStart = read "2020-07-29 21:44:51 UTC" :: UTCTime
            x = realToFrac $ diffUTCTime now shelleyStart :: Double

        return $ SlotNo $ firstShelleySlot + (round x)

    dumpMinPeers :: Map SlotNo (DiffTime, SockAddr) -> String
    dumpMinPeers m =
        let l = Map.toList m in
        concatMap (\(k, (dTime, peer)) -> printf "XXY%d,%s,%s"
            (unSlotNo k) (show dTime) (show peer)) l

    dumpPeers :: Map SockAddr (Map SlotNo DiffTime) -> String
    dumpPeers m =
        let l = Map.toList m
            slots = nub $ concatMap (Map.keys . snd) l
            header = printf "XXZ:peer,%s" (intercalate "," $ map show slots) in
        header ++ concatMap (dumpSlotMap slots) l

    dumpSlot :: Map SlotNo DiffTime -> SlotNo -> String
    dumpSlot m slot =
        case Map.lookup slot m of
             Nothing -> ""
             Just t -> printf "%f" (realToFrac t :: Double)

    dumpSlotMap :: [SlotNo] -> (SockAddr, Map SlotNo DiffTime) -> String
    dumpSlotMap slots (peer, m) =
        let slotStr = intercalate "," $ map (dumpSlot m) slots in
        printf "XXZ:%s,%s" (show peer) slotStr

    dumpPoolInfo :: (PoolStake, NonEmpty RelayAddress) -> String
    dumpPoolInfo (stake, relays) = printf "XXQ: %02.2f,%s\n"
        (100 * fromRational stake :: Double)
        (show relays)



simpleResolve :: RelayAddress -> IO (Maybe SockAddr)
simpleResolve (RelayAddressDomain domain) = do
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
    res_e <- DNS.withResolver rs $ \resolver -> DNS.lookupA resolver (daDomain domain)
    case res_e of
         Left  _     -> return Nothing
         Right []    -> return Nothing
         Right (a:_) -> return $ Just $ Socket.SockAddrInet  (daPortNumber domain) (IP.toHostAddress a)
simpleResolve (RelayAddressAddr ip port) = return $ Just $ IP.toSockAddr (ip, port)
