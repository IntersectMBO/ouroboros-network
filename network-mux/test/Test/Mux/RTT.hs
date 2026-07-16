{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Mux.RTT (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Control.Monad.IOSim

import System.Random

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Network.Mux.RTT
import Network.Mux.Types


tests :: TestTree
tests = testGroup "RTT"
    [ testProperty "matched echo yields expected RTT sample"
        prop_rtt_matchedEcho
    , testProperty "unknown echo cookies drop the sample"
        prop_rtt_unknownEchoNoSample
    , testProperty "last-peer-cookie reflects freshest send"
        prop_rtt_lastPeerCookie
    , testProperty "match-on-match prunes older outstanding cookies"
        prop_rtt_pruneOnMatch
    , testProperty "sub-interval sends reuse the same cookie"
        prop_rtt_mintReuse
    , testProperty "burst continuation reports inter-SDU gap"
        prop_rtt_burstContinuation
    , testProperty "burst tracker ages out after 'rttBurstMaxAge'"
        prop_rtt_burstMaxAgeEviction
    ]


-- | Blank SDU header carrying a specific (send, echo) cookie pair.
mkHdr :: Cookie -> Cookie -> SDUHeader
mkHdr s e = SDUHeader {
      mhSendCookie = s
    , mhEchoCookie = e
    , mhNum        = MiniProtocolNum 42
    , mhDir        = InitiatorDir
    , mhLength     = 0
    }


isNoSignal :: IngressEcho -> Bool
isNoSignal EchoNoSignal = True
isNoSignal _            = False


-- | Draw a fresh cookie from egress at a known @sendTime@, wait
-- @rttMs@ milliseconds, then feed an ingress SDU echoing that cookie.
-- 'peerRTT' at 0.5 should return within a coarse tolerance of
-- @rttMs / 1000@.
prop_rtt_matchedEcho :: Property
prop_rtt_matchedEcho =
    forAll (choose (1, 500 :: Int)) $ \rttMs ->
      let sendTime = Time 0.100
          recvTime = Time (0.100 + fromIntegral rttMs / 1000)
          expected = fromIntegral rttMs / 1000 :: Double
      in runSimOrThrow $ do
           st           <- newRTTState (mkStdGen 42)
           (cookie, _)  <- atomically $ newSendCookie st sendTime
           _            <- processIngress st (mkHdr noCookie cookie) recvTime
           result       <- atomically $ readPeerRTTQuantile (peerRTT st) 0.5
           return $ case result of
             Nothing ->
               counterexample "no RTT sample recorded" False
             Just d  ->
               let got = realToFrac d :: Double
               in counterexample
                    ("expected " ++ show expected ++ ", got " ++ show got) $
                    abs (got - expected) <= 1e-6


-- | An ingress SDU whose 'mhEchoCookie' doesn't match anything we
-- sent produces no RTT sample. (Combined with 'noCookie' sentinel:
-- that case must also drop the sample.)
prop_rtt_unknownEchoNoSample :: Property
prop_rtt_unknownEchoNoSample = once $ runSimOrThrow $ do
    st        <- newRTTState (mkStdGen 7)
    _         <- processIngress st (mkHdr (Cookie 100) noCookie)     (Time 0.001)
    _         <- processIngress st (mkHdr (Cookie 200) (Cookie 999)) (Time 0.002)
    result    <- atomically $ readPeerRTTQuantile (peerRTT st) 0.5
    return $ counterexample
      ("expected Nothing, got " ++ show result) $
      result == Nothing


-- | Ingress writes 'mhSendCookie' into the last-peer-cookie TVar. On
-- the next 'newSendCookie', that value is what we echo back.
prop_rtt_lastPeerCookie :: Property
prop_rtt_lastPeerCookie = once $ runSimOrThrow $ do
    st       <- newRTTState (mkStdGen 3)
    _        <- processIngress st (mkHdr (Cookie 100) noCookie) (Time 0.001)
    _        <- processIngress st (mkHdr (Cookie 200) noCookie) (Time 0.002)
    (_, echo) <- atomically $ newSendCookie st (Time 0.003)
    return $ counterexample
      ("expected Cookie 200, got " ++ show echo) $
      echo == Cookie 200


-- | Once we accept an echo for cookie C₂ (sent after C₁), C₁ must
-- have been pruned from outstanding — a subsequent echo of C₁
-- (violating the peer's monotone-echo invariant) finds nothing and
-- yields no additional sample.
prop_rtt_pruneOnMatch :: Property
prop_rtt_pruneOnMatch = once $ runSimOrThrow $ do
    st            <- newRTTState (mkStdGen 11)
    (c1, _)       <- atomically $ newSendCookie st (Time 0.010)
    (c2, _)       <- atomically $ newSendCookie st (Time 0.020)
    -- Accept echo of c2 first (matches, prunes c1 too).
    EchoMatched _ <- processIngress st (mkHdr noCookie c2) (Time 0.050)
    -- Now a stale echo of c1 must NOT produce a sample; c1 is not in
    -- outstanding, and it's not the tracked burst cookie either
    -- (c2 is).
    m             <- processIngress st (mkHdr noCookie c1) (Time 0.060)
    return $ counterexample
      ("expected EchoNoSignal for stale c1 echo") $
      isNoSignal m


-- | Two successive 'newSendCookie' calls within 'defaultMintInterval'
-- return the same cookie, and only one entry lands in outstanding
-- (verified by matching that cookie once and seeing the second
-- echo turn into a burst-SDU sample rather than a fresh match).
prop_rtt_mintReuse :: Property
prop_rtt_mintReuse = once $ runSimOrThrow $ do
    st        <- newRTTState (mkStdGen 5)
    -- Two sends 0.1 ms apart — well inside the 1 ms mint interval.
    (c1, _)   <- atomically $ newSendCookie st (Time 0.001)
    (c2, _)   <- atomically $ newSendCookie st (Time 0.0011)
    let reused = c1 == c2
    -- First echo of the shared cookie: fresh RTT match.
    r1        <- processIngress st (mkHdr noCookie c1) (Time 0.010)
    -- Second echo: the cookie has been deleted from outstanding
    -- (prune-on-match), but the burst tracker is fresh, so we get an
    -- EchoBurstSDU rather than a second EchoMatched.
    r2        <- processIngress st (mkHdr noCookie c2) (Time 0.011)
    return $ counterexample
      ("c1=" ++ show c1 ++ " c2=" ++ show c2 ++
       " r1 matched: " ++ show (case r1 of EchoMatched _ -> True; _ -> False) ++
       " r2 burst: "   ++ show (case r2 of EchoBurstSDU _ -> True; _ -> False)) $
      reused
        .&&. (case r1 of EchoMatched _ -> True; _ -> False)
        .&&. (case r2 of EchoBurstSDU _ -> True; _ -> False)


-- | A response burst — first SDU produces a fresh RTT match, the
-- second and third SDUs that echo the same cookie produce
-- 'EchoBurstSDU' samples whose gap equals the interval between
-- successive ingress arrivals.
prop_rtt_burstContinuation :: Property
prop_rtt_burstContinuation = once $ runSimOrThrow $ do
    st          <- newRTTState (mkStdGen 13)
    (c, _)      <- atomically $ newSendCookie st (Time 0.010)
    r1          <- processIngress st (mkHdr noCookie c) (Time 0.100)
    r2          <- processIngress st (mkHdr noCookie c) (Time 0.101)
    r3          <- processIngress st (mkHdr noCookie c) (Time 0.103)
    return $ conjoin
      [ counterexample ("r1 not EchoMatched: " ++ show r1) $
          case r1 of EchoMatched _  -> True; _ -> False
      , counterexample ("r2 not EchoBurstSDU with gap ≈ 1 ms") $
          case r2 of
            EchoBurstSDU gap ->
              abs (realToFrac gap - (1e-3 :: Double)) <= 1e-9
            _ -> False
      , counterexample ("r3 not EchoBurstSDU with gap ≈ 2 ms") $
          case r3 of
            EchoBurstSDU gap ->
              abs (realToFrac gap - (2e-3 :: Double)) <= 1e-9
            _ -> False
      ]


-- | After 'defaultBurstMaxAge' seconds elapse from the first echo,
-- further echoes of the same cookie are dropped even though the
-- burst tracker would have kept accepting them.
prop_rtt_burstMaxAgeEviction :: Property
prop_rtt_burstMaxAgeEviction = once $ runSimOrThrow $ do
    st          <- newRTTState (mkStdGen 17)
    (c, _)      <- atomically $ newSendCookie st (Time 0)
    r1          <- processIngress st (mkHdr noCookie c) (Time 0.100)
    -- Well past the 1 s default 'defaultBurstMaxAge' since the first
    -- match; the burst tracker must be evicted, not renewed.
    rLate       <- processIngress st (mkHdr noCookie c) (Time 2.000)
    return $ conjoin
      [ counterexample ("r1 not EchoMatched: " ++ show r1) $
          case r1 of EchoMatched _ -> True; _ -> False
      , counterexample ("late echo should be EchoNoSignal") $
          isNoSignal rLate
      ]
