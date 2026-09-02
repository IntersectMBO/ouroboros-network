{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

-- | Per-flow CUBIC congestion control (RFC 9438) plus a full RTO, at one-round
-- (one-RTT) granularity.
--
-- Each RFC clause is one named function that can be diffed against the text:
-- 'enterCA' is §4.2 Figure 2, 'cubicTarget' is §4.4/§4.5, 'fastConvergence' is §4.7,
-- 'multiplicativeDecrease' is §4.6 Figure 5, and 'onCongestionEvent' composes the last
-- three in the RFC's own order (§4.7 runs "before the window reduction described in
-- Section 4.6").  Names and units follow §4.1: all windows in segments, all times in
-- seconds, and beta_cubic is the RFC's 0.7 -- the RETAINED fraction -- so every formula
-- reads as printed.
--
-- This is the single-flow core.  The diffusion DES drives the same primitives per round
-- with a loss decision derived from egress contention rather than a sampled rate.
module SmallWorld.TCP
  ( -- * RFC 9438 §4.1.2 state
    Epoch(..)
  , Phase(..)
  , RttEst(..)
  , FlowState(..)
  , TcpParams(..)
  , mkTcpParams
  , initFlow
  , warmFlow
    -- * The clauses
  -- , enterCA
  -- , wCubic
  -- , nextWEst
  -- , nextCACwnd
  -- , cubicTarget
  -- , fastConvergence
  -- , multiplicativeDecrease
  , CongestionEvent(..)
  , onCongestionEvent
  , rtoCollapse
  , idleReset
  , onDelivery
    -- * Round drivers
  , growOnAck
  , stepRound
  , flowTime
  , flowTimeFrom
  ) where

import System.Random (RandomGen, randomR)

-- | RFC 9438 §4.1.2: the congestion-avoidance epoch.  First free are defined "at the
-- beginning of the current congestion avoidance stage", so they are built together by
-- 'enterCA' and never assigned piecewise -- a partially-updated epoch (a new W_max with a
-- stale K, say) has no representation.  The elapsed time is updated on by `growOnAck`.
data Epoch = Epoch
  { epWMax      :: !Double   -- ^ W_max
  , epCwndEpoch :: !Double   -- ^ cwnd_epoch
  , epK         :: !Double   -- ^ K, seconds
  , epElapsed   :: !Double   -- ^ t - t_epoch, seconds
  , epWEst      :: !Double   -- ^ W_est (§4.3), the Reno-friendly window estimate
  } deriving (Eq, Show)

-- | Slow start carries no epoch: W_max is undefined until the first congestion event
-- (§4.10), and an RTO returns here precisely in order to forget it (§4.8).
data Phase =
    SlowStart
   -- ^ slow start phase
 | CA !Epoch
   -- ^ congestion avoidance phase with internal state
 deriving (Eq, Show)

-- | RFC 6298 §2's retransmission-timer state.  'NoSample' is §2.1 -- before any RTT
-- measurement has been made -- kept as its own constructor so §2.2's initialisation
-- cannot be confused with §2.3's update.
data RttEst = NoSample
              -- ^ §2.1: no measurement yet
            | RttEst !Double !Double
              -- ^ SRTT and RTTVAR, seconds
  deriving (Eq, Show)

-- | Static per-flow parameters (constant over the transfer).
data TcpParams = TcpParams
  { tpFileBytes  :: !Int
  , tpRttS       :: !Double
  , tpLossP      :: !Double   -- ^ per-packet loss this transfer (the DES overrides per round)
  , tpMss        :: !Int
  , tpBdpCapSegs :: !Int      -- ^ window cap = BDP/MSS (bandwidth ceiling)
  , tpCwnd0      :: !Double
  , tpSsthresh0  :: !Double
  , tpCubicC     :: !Double   -- ^ C, segments/s^3 (§4.1.1)
  , tpBetaCubic  :: !Double   -- ^ beta_cubic, the RETAINED fraction (§4.6: SHOULD be 0.7)
  , tpRtoMinS    :: !Double
  , tpRtoMaxS    :: !Double
  , tpClockG     :: !Double   -- ^ clock granularity G (§2.2/§2.3's @max(G, K*RTTVAR)@)
  , tpInitRtoS   :: !Double   -- ^ RTO before any RTT measurement (§2.1: SHOULD be 1 s)
  , tpFastConv   :: !Bool     -- ^ §4.7
  , tpEnableRto  :: !Bool
  } deriving (Show)

-- | Smart constructor: derive the BDP window cap and initial ssthresh from a capacity
-- (bits/s) and RTT (ssthresh at link saturation).
mkTcpParams :: Int -> Double -> Double -> Double -> TcpParams
mkTcpParams fileBytes rttS lossP capBps = TcpParams
  { tpFileBytes  = fileBytes
  , tpRttS       = rttS
  , tpLossP      = lossP
  , tpMss        = mss
  , tpBdpCapSegs = bdpCap
  , tpCwnd0      = 10
  , tpSsthresh0  = fromIntegral bdpCap
  , tpCubicC     = 0.4
  , tpBetaCubic  = 0.7
  , tpRtoMinS    = 0.2
  , tpRtoMaxS    = 120
  , tpClockG     = 0.001
  , tpInitRtoS   = 1.0
  , tpFastConv   = True
  , tpEnableRto  = True
  }
  where
    mss    = 1460
    bdpCap = max 1 (floor (capBps * rttS / 8 / fromIntegral mss))

data FlowState = FlowState
  { fsCwnd      :: !Double   -- ^ cwnd, segments
  , fsSsthresh  :: !Double   -- ^ ssthresh, segments
  , fsCwndPrior :: !Double   -- ^ cwnd_prior (§4.1.2): cwnd when ssthresh was last set
  , fsPhase     :: !Phase
  , fsRttEst    :: !RttEst   -- ^ RFC 6298 §2: SRTT/RTTVAR, or NoSample before the first
                             --   measurement.  Only rounds that deliver non-retransmitted
                             --   data update it (§3, Karn's algorithm).
  , fsBytesSent :: !Int
  , fsT         :: !Double
  , fsConsecTO  :: !Int
  } deriving (Show)

initFlow :: TcpParams -> FlowState
initFlow tp = FlowState
  { fsCwnd = tpCwnd0 tp, fsSsthresh = tpSsthresh0 tp, fsCwndPrior = 0
  , fsPhase = SlowStart, fsRttEst = NoSample
  , fsBytesSent = 0, fsT = 0, fsConsecTO = 0 }

-- | A warm flow at (near-)steady cwnd: already ramped to the BDP cap and in congestion
-- avoidance, so a WARM transfer skips the cold slow-start ramp.  Its epoch is §4.10's
-- loss-free-exit shape: W_max = cwnd_epoch = cwnd, K = 0.
warmFlow :: TcpParams -> FlowState
warmFlow tp = (initFlow tp) { fsCwnd = cap, fsSsthresh = cap, fsCwndPrior = cap
                            , fsPhase = enterCA (tpCubicC tp) cap cap }
  where cap = fromIntegral (tpBdpCapSegs tp)

-- | §4.2 Figure 2: @K = cbrt((W_max - cwnd_epoch)/C)@.  The only way to build an 'Epoch',
-- so @W_cubic(0) == cwnd_epoch@ holds by construction and the curve can never start below
-- the installed window.  The first branch is §4.8/§4.10's rule for an epoch beginning at
-- or above W_max -- "K is set to 0, and W_max is set to the congestion window size at the
-- beginning of the current congestion avoidance stage" (Linux: @last_max_cwnd <= cwnd =>
-- bic_K = 0, bic_origin_point = cwnd@).
enterCA :: Double
        -- ^ @C@
        -> Double
        -- ^ @W_max@
        -> Double
        -- ^ @cwnd_epoch@
        -> Phase
enterCA c wMax cwndEpoch
  | wMax <= cwndEpoch
  = CA Epoch { epWMax = cwndEpoch
             , epCwndEpoch = cwndEpoch
             , epK = 0
             , epElapsed = 0
             , epWEst = cwndEpoch   -- §4.3, and §4.8/§4.10 for the post-timeout stage
             }
  | otherwise
  = CA Epoch { epWMax = wMax
             , epCwndEpoch = cwndEpoch
             , epK = ((wMax - cwndEpoch) / c) ** (1/3) -- Figure 2
             , epElapsed = 0
             , epWEst = cwndEpoch   -- §4.3: "W_est is set equal to cwnd_epoch at the
                                    -- start of the congestion avoidance stage"
             }

-- | The per-round rendering of §4.4/§4.5.
--
-- The RFC's per-ACK rule is @cwnd += (target - cwnd)/cwnd@ with @target = W_cubic(t+RTT)@.
-- That is a first-order lag of time constant RTT, so the cwnd it *achieves* at time t is
-- W_cubic(t) to within O(RTT^2·W''), not the target -- a model that assigns once per round
-- assigns W_cubic(t) and must NOT also apply the lookahead, or it runs a full RTT of curve
-- fast.  §4.2's two bounds still apply, ensuring "CUBIC's congestion window increase rate
-- is non-decreasing and is less than the increase rate of slow start": @max cwnd@ (Linux:
-- @ca->cnt = 100 * cwnd@ when the target is at or below cwnd) and @min (1.5 * cwnd)@
-- (Linux: @ca->cnt = max(ca->cnt, 2U)@).
cubicTarget :: Double
            -- ^ @C@
            -> Epoch
            -> Double
            -- ^ @cwnd@
            -> Double
cubicTarget c ep cwnd =
    -- Section §4.2, `target` formula
    if | w < cwnd      -> cwnd
       | w > cwndLimit -> cwndLimit
       | otherwise     -> w
  where
    w          = wCubic c ep
    cwndLimit  = 1.5 * cwnd

-- | §4.2 Figure 1: @W_cubic(t) = C*(t - K)^3 + W_max@.  Kept separate from 'cubicTarget'
-- because §4.3's region test is on this raw value, not on the clamped target.
wCubic :: Double
       -- ^ @C@
       -> Epoch
       -> Double
wCubic c ep = c * (epElapsed ep - epK ep) ** 3 + epWMax ep

-- | §4.3 Figure 4: @W_est = W_est + alpha_cubic * segments_acked / cwnd@, applied once per
-- round with the round's acknowledged-segment count.
--
-- @alpha_cubic = 3*(1 - beta_cubic)/(1 + beta_cubic)@ (= 0.529 at 0.7) is the additive
-- factor that "achieves approximately the same average window size as Reno".  §4.3 then
-- escalates it: "Once W_est has grown to reach the cwnd at the time of most recently
-- setting ssthresh -- that is, W_est >= cwnd_prior -- the sender SHOULD set alpha_cubic
-- to 1 to ensure that it can achieve the same congestion window increment rate as Reno".
nextWEst :: Double
         -- ^ @β_cubic@
         -> Double
         -- ^ @cwnd_prior@
         -> Double
         -- ^ @cwnd@
         -> Double
         -- ^ @segments_acked@ this round
         -> Double
         -- ^ @W_est@
         -> Double
nextWEst betaCubic cwndPrior cwnd segmentsAcked wEst =
    wEst + alphaCubic * segmentsAcked / cwnd
  where
    -- α_cubic
    alphaCubic | wEst >= cwndPrior = 1
               | otherwise         = 3 * (1 - betaCubic) / (1 + betaCubic)

-- | The congestion-avoidance window for the next round: §4.3's region choice.
--
-- "CUBIC checks whether W_cubic(t) is less than W_est.  If so, CUBIC is in the
-- Reno-friendly region and cwnd SHOULD be set to W_est at each reception of a new ACK";
-- otherwise §4.2's @target@ applies.  Two details of that sentence matter: the test is on
-- the RAW W_cubic(t), not on the clamped target, and the W_est arm *assigns* rather than
-- taking a maximum, so §4.2's bounds do not gate it.  (They coincide in practice -- W_est
-- starts at cwnd_epoch, both only grow, and in this region W_est is the larger -- but the
-- branch is what the RFC specifies.)  The region is orthogonal to concave/convex: §4.3
-- applies "where cwnd could be greater than or less than W_max".
nextCACwnd :: Double
           -- ^ @C@
           -> Epoch
           -> Double
           -- ^ @cwnd@
           -> Double
nextCACwnd c ep cwnd
  -- Reno-friendly region
  | wCubic c ep < epWEst ep
  = epWEst ep

  -- CUBIC region
  | otherwise
  = cubicTarget c ep cwnd


-- | One loss-free round: RFC 5681 slow-start doubling up to ssthresh, then §4.4/§4.5.
-- Takes the round's RTT and owns the epoch clock, so @t - t_epoch@ has exactly one writer
-- per round (§4.2: t must exclude any period in which cwnd was not updated).
growOnAck :: TcpParams
          -> Double
          -- ^ @RTT@
          -> Double
          -- ^ @segments_acked@ this round: what the round actually put on the wire, which
          -- is below @cwnd@ for a share-throttled round or the file tail
          -> FlowState
          -> FlowState
growOnAck tp rtt segmentsAcked fs = case fsPhase fs of
  SlowStart
    | doubled < fsSsthresh fs ->
      fs { fsCwnd = doubled }
    -- §4.10: cwnd is no longer at or below ssthresh, so leave slow start.  A loss-free
    -- exit leaves W_max undefined; the RFC's remedy is "CUBIC sets cwnd_prior = cwnd and
    -- switches to congestion avoidance ... K is set to 0, and W_max is set to the
    -- congestion window size at the beginning of the current congestion avoidance stage".
    -- `max` because after an idleReset ssthresh can sit below cwnd, and slow start must
    -- never reduce the window.
    | otherwise ->
      fs { fsCwnd      = w
         , fsCwndPrior = w
         , fsPhase     = enterCA (tpCubicC tp) w w
         }

  -- congestion avoidance
  CA ep ->
    -- §4.3 updates W_est on the new ACK and only then compares it with W_cubic(t), so the
    -- epoch clock and W_est both advance before the region choice.
    let ep' = ep { epElapsed = epElapsed ep + rtt
                 , epWEst    = nextWEst (tpBetaCubic tp) (fsCwndPrior fs)
                                        (fsCwnd fs) segmentsAcked (epWEst ep)
                 }
    in fs { fsCwnd  = nextCACwnd (tpCubicC tp) ep' (fsCwnd fs)
          , fsPhase = CA ep'
          }
  where
    doubled = fsCwnd fs * 2
    w       = fsSsthresh fs `max` fsCwnd fs

-- | W_max as of now: undefined (0) in slow start, so fast convergence cannot fire on a
-- flow that has not been cut in this stage -- which is also how §4.8's "forget W_max on a
-- timeout" falls out, since 'rtoCollapse' returns to slow start.
currentWMax :: FlowState -> Double
currentWMax fs = case fsPhase fs of
  CA ep     -> epWMax ep
  SlowStart -> 0

-- | §4.7 Fast Convergence.  Runs BEFORE the reduction, on the pre-reduction cwnd, and
-- compares against W_max -- the value a previous firing may already have reduced (§4.1.2:
-- "if fast convergence is enabled, W_max may be further reduced") -- never against
-- cwnd_prior.  RFC 8312's separate W_last_max, which compared against the un-reduced
-- pre-cut cwnd and so fired more often, does not exist in RFC 9438.
fastConvergence :: Bool -> Double -> Double -> Double -> Double
fastConvergence enabled betaCubic cwnd wMax
  | enabled && cwnd < wMax = cwnd * (1 + betaCubic) / 2
  | otherwise              = cwnd

data CongestionEvent = Loss | ECE
  deriving Show

-- | §4.6 Figure 5, returning all three outputs at once so none can be dropped.
--
-- @flightSize@ is the RFC's flight_size.  §4.6 permits cwnd in its place, but only for
-- implementations that "use other measures to prevent cwnd from growing when the volume of
-- bytes in flight is smaller than cwnd" -- a caller substituting cwnd takes on that
-- obligation.  Figure 5 applies the cwnd floor to the *unfloored* ssthresh and floors
-- ssthresh on the following line, hence the order here.
multiplicativeDecrease
  :: Double
  -> CongestionEvent
  -> Double
  -- ^ @cwnd@
  -> Double
  -> (Double, Double, Double)
  -- ^ @(ssthresh, cwnd_prior, cwnd)@
multiplicativeDecrease betaCubic ev cwnd flightSize =
  ( max ssthresh 2                            -- ssthresh = max(ssthresh, 2)
  , cwnd                                      -- cwnd_prior = cwnd
  , max ssthresh floorSegs )                  -- cwnd = max(ssthresh, 2) / max(ssthresh, 1)
  where
    ssthresh  = flightSize * betaCubic
    floorSegs = case ev of Loss -> 2; ECE -> 1

-- | A congestion event (§4.6): fast convergence, then the multiplicative decrease, then a
-- fresh epoch -- in the RFC's order.  fsBytesSent and fsT are untouched: a loss costs a
-- window reduction (and, at the caller's discretion, a recovery RTT), never delivered data.
onCongestionEvent :: TcpParams -> CongestionEvent -> Double -> FlowState -> FlowState
onCongestionEvent tp ev flightSize fs = fs
  { fsSsthresh  = ssthresh
  , fsCwndPrior = cwndPrior
  , fsCwnd      = cwnd
  , fsPhase     = enterCA (tpCubicC tp) wMax cwnd }
  where
    wMax = fastConvergence
            (tpFastConv tp)
            (tpBetaCubic tp)
            (fsCwnd fs)
            (currentWMax fs)

    (ssthresh, cwndPrior, cwnd) =
      multiplicativeDecrease
        (tpBetaCubic tp)
        ev
        (fsCwnd fs)
        flightSize


-- | RFC 6298 §2.2 on the first RTT measurement, §2.3 on every subsequent one.
--
-- @
--   (2.2)  SRTT <- R                 (2.3)  RTTVAR <- (1-beta)*RTTVAR + beta*|SRTT - R'|
--          RTTVAR <- R/2                    SRTT   <- (1-alpha)*SRTT + alpha*R'
-- @
--
-- with alpha = 1/8 and beta = 1/4.  §2.3 makes the *order* a MUST -- RTTVAR is updated
-- using the value of SRTT from before SRTT itself is updated -- which holds here because
-- @rttVar'@ refers to the pattern-bound @srtt@, not to @srtt'@.
--
-- Karn's algorithm (§3) forbids sampling a retransmitted segment, so this is called only
-- from a round that delivered original transmissions: 'stepRound' applies it in 'advance',
-- which runs on clean and fast-retransmit rounds but not on a timeout.
sampleRtt :: Double
          -- ^ the round's measured RTT
          -> RttEst
          -> RttEst
sampleRtt r  NoSample             = RttEst r (r / 2)
sampleRtt r' (RttEst srtt rttVar) = RttEst srtt' rttVar'
  where
    rttVar' = (1 - 0.25)  * rttVar + 0.25  * abs (srtt - r')
    srtt'   = (1 - 0.125) * srtt   + 0.125 * r'

-- | RFC 6298 @RTO <- SRTT + max(G, K*RTTVAR)@ with K = 4 (§2.2/§2.3), or §2.1's initial
-- value while no measurement has been taken.  §2.4's floor and §2.5's ceiling are applied
-- by the caller, around §5.5's backoff, because §5.5 doubles an already-floored RTO.
baseRto :: TcpParams -> RttEst -> Double
baseRto tp NoSample              = tpInitRtoS tp
baseRto tp (RttEst srtt rttVar)  = srtt + max (tpClockG tp) (4 * rttVar)

-- | Bookkeeping shared by every round that delivered non-retransmitted data.
--
-- Such a round yields a Karn-legal RTT sample (RFC 6298 §3), and because a new measurement
-- recomputes the RTO it also collapses any exponential backoff -- the note after §5.7:
-- "once a new RTT measurement is obtained (which can only happen when new data has been
-- sent and acknowledged) ... which may result in 'collapsing' RTO back down after it has
-- been subject to exponential back off".
--
-- Both drivers must apply this on exactly the rounds that put original transmissions on
-- the wire and got them acknowledged: a clean round, a fast-retransmit round, and the
-- pre-loss prefix of a timeout round.  Keeping it in one function is what stops them
-- drifting apart on which rounds count.
onDelivery :: Double
           -- ^ the round's measured RTT
           -> FlowState
           -> FlowState
onDelivery rtt fs = fs { fsRttEst = sampleRtt rtt (fsRttEst fs)
                       , fsConsecTO = 0
                       }

-- | §4.8 Timeout: "CUBIC follows Reno to reduce cwnd but sets ssthresh using beta_cubic
-- (same as in Section 4.6)".  cwnd goes to 1 and the flow returns to slow start, which
-- drops the epoch and with it W_max -- §4.8's requirement that the next congestion
-- avoidance stage begin with K = 0 and W_max = cwnd_epoch (Linux: bictcp_reset on
-- TCP_CA_Loss).  fsBytesSent/fsT are untouched; the caller charges the backoff stall.
rtoCollapse :: TcpParams -> Double -> FlowState -> FlowState
rtoCollapse tp flightSize fs =
  fs { fsSsthresh  = max 2 (flightSize * tpBetaCubic tp)
     , fsCwndPrior = fsCwnd fs
     , fsCwnd      = 1
     , fsPhase     = SlowStart }

-- | Linux @tcp_slow_start_after_idle@ (RFC 5681 §4.1): a connection idle for longer than
-- one RTO restarts its window at the initial cwnd, back in slow start, keeping ssthresh --
-- so a *reused* connection does not send from a stale warm window.  Stacks that disable it
-- (@tcp_slow_start_after_idle=0@, @keepsWarm=True@) retain the warm cwnd.  @idleGapS@ is
-- the inactivity since the connection last sent; gaps within one RTO (e.g. the ~1-RTT
-- body->closure request inside a single EB) are not idle and stay warm regardless.
--
-- In single-EB diffusion every connection is used once, so this never fires; it is the
-- seam a multi-round driver uses when a connection is reused after seconds of inactivity.
idleReset :: Bool -> Double -> TcpParams -> FlowState -> FlowState
idleReset keepsWarm idleGapS tp fs
  | keepsWarm                                      = fs
  | idleGapS <= max (tpRtoMinS tp) (baseRto tp (fsRttEst fs)) = fs
  | otherwise = fs { fsCwnd = tpCwnd0 tp, fsPhase = SlowStart, fsConsecTO = 0 }

-- | Sample losses in a window of @n@ packets at per-packet rate @p@: returns
-- (loss count, 1-based first-loss position or 0, rng').
sampleLosses :: RandomGen g
             => Int
             -- ^ window size in packets
             -> Double
             -- ^ per packet loss probability
             -> g
             -> (Int, Int, g)
sampleLosses n p = go 1 0 0
  where
    go !i !k !firstPos !g
      | i > n
      = (k, firstPos, g)

      | otherwise
      = let (u, g') = randomR (0, 1) g in
        if u < p
        then go (i+1) (k+1) (if firstPos == 0 then i else firstPos) g'
        else go (i+1) k firstPos g'

-- | One round (one RTT, or one RTO stall on timeout).  @rtt@ and @lossP@ are passed in so
-- a driver can vary them per round (jitter, egress-derived loss).
stepRound :: RandomGen g
          => TcpParams
          -> Double
          -- ^ RTT of this round
          -> Double
          -- ^ loss probability, thus from the interval [0, 1]
          -> FlowState
          -> g
          -> (FlowState, Bool, g)
stepRound tp rtt lossP fs g =
  let mss, effCwnd, remPkts, attempt, k, firstLoss :: Int

      -- maximum segment size
      mss = tpMss tp

      -- effective cwnd
      effCwnd = max 1 (min (floor (fsCwnd fs)) (tpBdpCapSegs tp))

      -- number of packets needed to finish the transmission
      remPkts = (tpFileBytes tp - fsBytesSent fs + mss - 1) `div` mss

      -- number of packets attempted to handle in this round
      attempt = min effCwnd remPkts

      flight :: Double
      flight  = fromIntegral effCwnd          -- see note at the call sites below

      -- k is the number of lost packets
      (k, firstLoss, g') = sampleLosses attempt lossP g
      lossRound = k > 0
      -- RFC 5681 §3.2: a fast retransmit needs three dup-ACKs, so the first loss must
      -- leave >= 3 packets behind it; anything else (or >= 2 losses, no SACK) times out.
      fastRtx | lossRound && tpEnableRto tp = k == 1 && attempt - firstLoss >= 3
              | otherwise                   = lossRound
  in
  if not lossRound
    then
    -- clean round resets the RTO backoff
    let fs1 = advance attempt rtt fs in
    ( if fsBytesSent fs1 >= tpFileBytes tp
        then
            -- RFC 9438
            -- §5.8 "CUBIC does not increase its congestion window if
            --       a flow is application limited"
            -- §4.2 "t MUST NOT include periods during which cwnd has not
            --       been updated due to application-limited behaviour"
            --
            -- But above all, `flowTimeFrom` will terminate and will not
            -- evaluate `cwnd` any more thus no need to recompute it.
             fs1
        else growOnAck tp rtt (fromIntegral attempt) fs1
    , False
    , g'
    )
    else if fastRtx
      then
        -- data delivered => fresh RTT samples clear the backoff (RFC 6298 §2.2/2.3 + Karn)
        let fs1 = advance attempt rtt fs
        in if fsBytesSent fs1 >= tpFileBytes tp
             -- final-window fast retransmit: mid-transfer the fill RTT is absorbed by
             -- the next round's continued transmission, but at the file tail nothing
             -- rides behind the hole, so the in-order prefix closes only when the
             -- retransmit lands ~1 RTT on -- charge it.
             then ( fs1 { fsT = fsT fs1 + rtt }
                  , True
                  , g'
                  )
             -- flight_size is the BDP-capped window rather than `attempt`: §4.6 warns
             -- that cutting from a short flight "would decrease cwnd to a much lower
             -- value than necessary", and `attempt` dips below the window only at the
             -- file tail.  The BDP cap is the "other measure" §4.6 asks of a
             -- cwnd-based implementation.
             else ( onCongestionEvent tp Loss flight fs1
                  , True
                  , g'
                  )
      else -- RTO
        let delivered = max 0 (firstLoss - 1)
            backoff   = (2 :: Int) ^ min (fsConsecTO fs) (30 :: Int)
            rto       = min (tpRtoMaxS tp) (max (tpRtoMinS tp) (baseRto tp (fsRttEst fs)) * fromIntegral backoff)
            -- RFC 6298 §3: the packets ahead of the hole are original transmissions, so
            -- they are Karn-legal and §3 requires a measurement where one is possible;
            -- with nothing acked there is no sample and the backoff compounds (§5.5).
            fs0 | delivered > 0 = onDelivery rtt fs
                | otherwise     = fs { fsConsecTO = fsConsecTO fs + 1 }
            fs1 = fs0 { fsBytesSent = min (tpFileBytes tp) (fsBytesSent fs + delivered * mss)
                      , fsT = fsT fs + rto
                      }
        in ( rtoCollapse tp flight fs1
           , True
           , g'
           )
  where
    -- a delivering round: bytes and wall clock only.  The epoch clock belongs to
    -- growOnAck, and fsConsecTO to the caller (cleared on any delivering round, since a
    -- successful delivery draws fresh RTT samples; only back-to-back timeouts keep it).
    -- `onDelivery` takes the round's RTT sample and clears the RTO backoff; a timeout
    -- round does not call `advance`, and handles its delivered prefix separately.
    advance pkts rtt' f =
      (onDelivery rtt' f)
        { fsBytesSent = min (tpFileBytes tp) (fsBytesSent f + pkts * tpMss tp)
        , fsT = fsT f + rtt'
        }

-- | Isolated single-flow completion: (download time s, effective loss events).
flowTime :: RandomGen g => TcpParams -> g -> (Double, Int)
flowTime tp = flowTimeFrom tp (initFlow tp)

-- | As 'flowTime' but from an arbitrary initial 'FlowState' -- lets a caller start a
-- transfer WARM (cwnd already ramped) instead of cold slow-start.
flowTimeFrom :: RandomGen g => TcpParams -> FlowState -> g -> (Double, Int)
flowTimeFrom tp fs0 g0
  | tpFileBytes tp <= 0 = (0, 0)
  | otherwise           = loop fs0 { fsBytesSent = 0, fsT = 0 } 0 g0 (0 :: Int)
  where
    loop !fs !nLoss !g !rounds
      | fsBytesSent fs >= tpFileBytes tp = (fsT fs, nLoss)
      | rounds > 10000000                = (1/0, nLoss)
      | otherwise =
          let (fs', lossRound, g') = stepRound tp (tpRttS tp) (tpLossP tp) fs g
          in loop fs' (if lossRound then nLoss + 1 else nLoss) g' (rounds + 1)
