{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Main (main) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM_, unless)
import Control.Monad.Class.MonadAsync (wait, withAsync)
import Control.Monad.Class.MonadTime.SI (Time (..))
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import Control.Monad.IOSim (runSimOrThrow)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Sequence.Strict qualified as Q
import Data.Word (Word64)
import Ouroboros.Network.Tx (getRawTxId)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry
import Ouroboros.Network.TxSubmission.Inbound.V2.State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
import Test.Ouroboros.Network.TxSubmission.Types (Tx (..), TxId)

type S = SharedTxState Int TxId
type P = PeerTxLocalState (Tx TxId)

policy :: TxDecisionPolicy
policy = either (error . show) id (mkTxDecisionPolicy defaultTxSubmissionConfigV2)

base :: S
base = emptySharedTxState
  { sharedRevision = 17, sharedGeneration = 5, sharedClaimable = 3 }

entry :: TxEntry Int
entry = TxEntry { txLease = TxClaimable (Time 0), txAttempt = 0,
  txInSubmission = False, txDoom = Undoomed, currentMaxInflightMultiplicity = 2 }

active :: [(TxId, TxEntry Int)] -> S
active = foldl add base
  where
    add st (tid, e) = let (_, TxKey k, st') = internTxId tid st
                     in st' { sharedTxTable = IM.insert k e (sharedTxTable st') }

retained :: S
retained = base { sharedRetainedTxs = retainedFromList [(0, retainDeadline (Time 0))],
  sharedTxIdToKey = M.singleton (getRawTxId (1 :: TxId)) (TxKey 0),
  sharedKeyToTxId = IM.singleton 0 1, sharedNextTxKey = 1 }

ad :: Bool -> S -> S
ad known st = let (_, _, st') = handleReceivedTxIds (const known) (Time 10) policy 1 [(1,64)]
                     emptyPeerTxLocalState {peerRequestedTxIds = 1}
                     emptyPeerTxInFlight st
              in st'

bodyFixture :: (S, P, PeerTxInFlight)
bodyFixture = (st, ps, pif)
  where
    st = active [(1, entry { txLease = TxLeased 0 (Time 20), txAttempt = 1 })]
    ps = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = Q.singleton (TxKey 0)
      , peerAvailableTxIds = IM.singleton 0 64
      , peerRequestedTxs = IS.singleton 0
      , peerRequestedTxBatches = Q.singleton (RequestedTxBatch (IS.singleton 0) 64)
      , peerRequestedTxsSize = 64 }
    pif = emptyPeerTxInFlight
      { pifAdvertised = IS.singleton 0, pifAttempting = IS.singleton 0, pifLeased = IS.singleton 0 }

body :: Bool -> S
body delivered = let (st, ps, pif) = bodyFixture
                     (_, _, _, _, st') = handleReceivedTxs (const False) (Time 10) policy 0
                         [(1, Tx 1 64 64 True Nothing) | delivered] ps pif st
                 in st'

submitting :: S
submitting = active [(1, entry { txLease = TxLeased 0 (Time 20), txInSubmission = True })]

submission :: Bool -> S
submission accepted = let (_, _, st') = handleSubmittedTxs (Time 10) policy 0
                              [(TxKey 0,1) | accepted] [TxKey 0 | not accepted]
                              emptyPeerTxLocalState emptyPeerTxInFlight submitting
                       in st'

stuck :: Bool -> (S, S)
stuck blocked = (st, st')
  where
    stuckEntry = entry { txLease = TxLeased 0 (Time 0.25), txAttempt = 2 }
    blocker = entry { txLease = TxLeased 1 (Time 20), txAttempt = 1 }
    st = active (if blocked then [(1,blocker),(2,stuckEntry)] else [(2,stuckEntry)])
    key = if blocked then 1 else 0
    ps = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = Q.fromList (if blocked then [TxKey 0,TxKey 1] else [TxKey 0])
      , peerDownloadedTxs = IM.singleton key (Tx 2 64 64 True Nothing) }
    pif = emptyPeerTxInFlight {pifAdvertised = IS.fromList (if blocked then [0,1] else [0]),
                             pifAttempting = IS.singleton key, pifLeased = IS.singleton key}
    (_, _, _, st') = nextPeerAction (Time 10) policy defaultTxSubmissionConfigV2 0 ps pif st

data Row = Row String S S (Word64, Word64, Word64) Bool

rows :: [Row]
rows =
  [ Row "idle" base base (0,0,0) False
  , Row "freshAd" base (ad False base) (1,0,0) True
  , Row "duplicateAd" one (ad False one) (0,0,0) False
  , Row "sparedAd" doomed (ad False doomed) (1,0,0) True
  , Row "retainedAd" retained (ad False retained) (0,0,0) False
  , Row "leaseClaim" one claimed (1,0,0) True
  , Row "beginSubmission" leased (markSubmittingTxs [TxKey 0] leased) (1,0,0) True
  , Row "bodyBuffered" (first3 bodyFixture) (body True) (0,0,0) False
  , Row "mempoolHit" one (ad True one) (1,1,0) True
  , Row "freshMempoolHit" base (ad True base) (1,0,0) True
  , Row "omitLive" (first3 bodyFixture) (body False) (1,1,1) True
  , Row "capBump" (fst (stuck True)) (snd (stuck True)) (1,1,1) True
  , Row "accept" submitting (submission True) (1,1,0) True
  , Row "rejectLive" submitting (submission False) (1,1,1) True
  , Row "scrubLive" submitting (scrubFromPeerInFlight 0 (Time 10)
      emptyPeerTxInFlight {pifSubmitting = IS.singleton 0} submitting) (1,1,1) True
  , Row "scrubGone" base (scrubFromPeerInFlight 0 (Time 10)
      emptyPeerTxInFlight {pifSubmitting = IS.singleton 99} base) (0,0,0) False
  , Row "expire" retained (fst (expireSharedStateUpTo 10 (Time 10) retained)) (1,0,0) True
  , Row "doom" one swept (1,0,0) True
  , Row "review" doomed (fst (reviewDoomed (Time 10) IS.empty (IS.singleton 0) doomed)) (1,0,0) True
  , Row "emptySubmission" base emptyOutcome (1,1,0) False
  , Row "rejectGone" base rejectedGone (1,1,0) False
  , Row "capBump+beginSubmission" (fst (stuck False)) (snd (stuck False)) (2,1,1) True
  , Row "revisionWrap" (wrap leased) (markSubmittingTxs [TxKey 0] (wrap leased)) (1,0,0) True
  , Row "allCountersWrap" (wrap submitting) (scrubFromPeerInFlight 0 (Time 10)
      emptyPeerTxInFlight {pifSubmitting = IS.singleton 0} (wrap submitting)) (1,1,1) True
  ]
  where
    one = active [(1,entry)]
    doomed = active [(1,entry {txDoom = Doomed})]
    leased = first3 bodyFixture
    (_, _, _, claimed) = nextPeerAction (Time 10) policy defaultTxSubmissionConfigV2 0
      emptyPeerTxLocalState {peerUnacknowledgedTxIds = Q.singleton (TxKey 0), peerAvailableTxIds = IM.singleton 0 64}
      emptyPeerTxInFlight {pifAdvertised = IS.singleton 0} one
    (swept, _, _, _) = sweepSharedStateUpTo 10 1 (Time 10) sweepStart one
    (_, _, emptyOutcome) = handleSubmittedTxs (Time 10) policy 0 [] []
                             emptyPeerTxLocalState emptyPeerTxInFlight base
    (_, _, rejectedGone) = handleSubmittedTxs (Time 10) policy 0 [] [TxKey 99]
                             emptyPeerTxLocalState emptyPeerTxInFlight base
    wrap st = st { sharedRevision = maxBound, sharedGeneration = maxBound, sharedClaimable = maxBound }

first3 (a,_,_) = a
counters s = (sharedRevision s, sharedGeneration s, sharedClaimable s)
deltas a b = (sharedRevision b - sharedRevision a, sharedGeneration b - sharedGeneration a,
              sharedClaimable b - sharedClaimable a)
payloadSame a b = a {sharedRevision = sharedRevision b, sharedGeneration = sharedGeneration b,
                    sharedClaimable = sharedClaimable b} == b

main :: IO ()
main = do
  forM_ rows $ \(Row name before after expected changed) -> do
    unless (deltas before after == expected) $ error (name ++ ": wrong deltas " ++ show (deltas before after))
    unless (payloadSame before after /= changed) $ error (name ++ ": wrong payload-change classification")
    let (r,g,c,gm,cm) = runSimOrThrow $ do
          stateVar <- newSharedTxStateVar before
          mirrors <- CounterMirrors <$> newTVarIO (sharedGeneration before) <*> newTVarIO (sharedClaimable before)
          atomically $ writeSharedStateIfChanged mirrors stateVar (sharedRevision before) after
          stored <- readTVarIO stateVar
          gen <- readTVarIO (generationMirror mirrors)
          cla <- readTVarIO (claimableMirror mirrors)
          pure (sharedRevision stored, sharedGeneration stored, sharedClaimable stored, gen, cla)
        (dr,_,_) = expected
    unless (r == sharedRevision before + (if dr == 0 then 0 else 1) && g == gm && c == cm) $
      error (name ++ ": publication did not normalise revision or sync mirrors")
    putStrLn $ name ++ ": delta=" ++ show expected ++ "; committed=" ++ show (r,g,c)
  -- A fast peer waits on old key 0, currently submitted by another peer.
  -- A third party advertises txid 1, allocating fresh key 1 and retaining
  -- it on the mempool hit. The waiting peer's key and action do not change.
  let blocked = active [(2, entry { txLease = TxLeased 1 (Time 0), txInSubmission = True })]
      afterFreshHit = ad True blocked
      waiterPeer :: P
      waiterPeer = emptyPeerTxLocalState
        { peerUnacknowledgedTxIds = Q.singleton (TxKey 0)
        , peerAvailableTxIds = IM.singleton 0 64 }
      waiterFlight = emptyPeerTxInFlight { pifAdvertised = IS.singleton 0 }
      waiterAction st = let (action, _, _, _) = nextPeerAction (Time 10) policy
                                defaultTxSubmissionConfigV2 2 waiterPeer waiterFlight st
                        in action
      isWakePark (PeerDoNothing Nothing WakeCounter) = True
      isWakePark _                                   = False
  unless (isWakePark (waiterAction blocked) && isWakePark (waiterAction afterFreshHit)) $
    error "expected the observer to have the same counter-only park before and after"
  unless (IM.lookup 0 (sharedTxTable blocked) == IM.lookup 0 (sharedTxTable afterFreshHit)) $
    error "the observer's key unexpectedly changed"
  -- Use the actual STM waiter, then publish that fresh mempool hit.
  let woke = runSimOrThrow $ do
        stateVar <- newSharedTxStateVar blocked
        mirrors <- CounterMirrors <$> newTVarIO 5 <*> newTVarIO 3
        withAsync (timeout 1 (atomically (awaitMirrorChange (Just (generationMirror mirrors)) 5 Nothing))) $ \parked -> do
          threadDelay 0.001
          atomically $ writeSharedStateIfChanged mirrors stateVar 17 afterFreshHit
          wait parked
  unless (woke == Nothing) $ error "a fresh mempool hit woke an unrelated peer"
  putStrLn "Fresh mempool hit left an actual WakeCounter waiter asleep; its referenced entry and next action were unchanged."
  let (coldSeen, selfSeen, selfMoved, gapWoke) = runSimOrThrow $ do
        let before = active [(1, entry)]
        stateVar <- newSharedTxStateVar before
        mirrors <- CounterMirrors <$> newTVarIO 0 <*> newTVarIO 0
        coldResult <- atomically $ resolvePeerAction mirrors before (emptyPeerTxLocalState :: P)
                                    (PeerDoNothing Nothing WakeCounter)
        ownResult <- atomically $ do
          writeSharedStateIfChanged mirrors stateVar 17 (ad True before)
          stored <- readTVar stateVar
          resolvePeerAction mirrors stored (emptyPeerTxLocalState :: P) (PeerDoNothing Nothing WakeCounter)
        case (coldResult, ownResult) of
          (ResolvedDoNothing _ _ coldSeen, ResolvedDoNothing _ _ seen) -> do
            current <- readTVarIO (generationMirror mirrors)
            atomically $ do
              stored <- readTVar stateVar
              writeSharedStateIfChanged mirrors stateVar (sharedRevision stored)
                stored { sharedRevision = sharedRevision stored + 1, sharedGeneration = sharedGeneration stored + 1 }
            -- The update has already landed when the park is entered.
            woke <- timeout 1 (atomically (awaitMirrorChange (Just (generationMirror mirrors)) seen Nothing))
            pure (coldSeen, seen, current /= seen, woke)
          _ -> error "expected two resolved parks"
  unless (coldSeen == 0 && selfSeen == 6 && not selfMoved && gapWoke == Just ()) $
    error "cold/own-write/before-park capture check failed"
  putStrLn "Cold and post-own-write baselines are the mirror's; an update before entering park is not missed."
  putStrLn $ "All " ++ show (length rows) ++ " transition/publication checks passed."
