module DeltaQ.TCP where

import Data.Time.Clock
import Numeric.Natural
import Data.Bifunctor

import DeltaQ.LinkRestriction
import DeltaQ.SimpleGS


data BearerCharacteristics
  = Bearer { linkDeltaQ  :: SimpleGS
           , restriction :: LinkRestriction
           }
    deriving (Show)

-- | make a BearerCharacteristics, testing for consistency. ∆Q|S of
--   the SimpleGS can not be smaller than the e2ePDUServiceTime of the
--   LinkRestriction.
mkBearer :: SimpleGS
         -> LinkRestriction
         -> BearerCharacteristics
mkBearer Bottom                lr = Bearer Bottom lr
mkBearer dq@(SimpleDeltaQ g s) lr
  = inv'g `seq` inv's'1 `seq` inv's'mtu `seq`  Bearer dq lr
  where
    inv'g
      | g < e2ePDUServiceTime lr 0
      = error "mkBearer: ∆Q|G invariant check failure."
      | otherwise = ()
    inv's'1
      | g + s 1 < e2ePDUServiceTime lr 1
      = error "mkBearer: ∆Q|G,S(1) invariant check failure."
      | otherwise = ()
    inv's'mtu
      | g + s (e2eMaxSDU lr) < e2ePDUServiceTime lr (e2eMaxSDU lr)
      = error "mkBearer: ∆Q|G,S(mtu) invariant check failure."
      | otherwise = ()
      
instance Semigroup BearerCharacteristics where
 a <> b
   = Bearer (linkDeltaQ a <> linkDeltaQ b) (restriction a <> restriction b)

instance Monoid BearerCharacteristics where
  mempty = Bearer mempty mempty

-- | Model the arrival pattern of data for a basic RPC pattern over a,
--   pre-established, TCP connection.
--
--   The pattern is a request from A to B, where B is sending a
--   (relatively large) response.
--
--   The aim is to give an over-optimistic arrival (i.e. represents a
--   definite lower bound on the time to complete given starting from
--   initial window state), In particular:
--
--     * every data packet is ACKed;
--
--     * the relevant protocol engines (at A and B) respond
--       instantaneously;
--
--     * no packet loss, and just self-induced delay due to this
--       packet flow (i.e assuming restricted resource is otherwise
--       idle)
--
--     * no queuing effects from competition for any underlying
--       resource in either direction (∆Q|V of perfection).
--
tcpRPCLoadPattern :: BearerCharacteristics
                  -- ^ for A -> B
                  -> BearerCharacteristics
                  -- ^ for B -> A
                  -> Natural
                  -- ^ The PDH overhead that needs to be accommodated
                  --   in each packet. The SDU size (MSS) is the MTU
                  --   size reduced by this value. This defines the
                  --   segment size (see below). Assumed symmetrical.
                  -> Natural
                  -- ^ The IW (initial window, in terms of "segments") -
                  --   see https://tools.ietf.org/html/rfc6928.
                  --
                  --   /Note/: the RFC states that the IW should be:
                  --      > min (10*MSS, max (2*MSS, 14600))
                  --   this is enforced.
                  -> Maybe Natural
                  -- ^ Maximum window size (in octets). Default is unbounded.
                  -> Natural
                  -- ^ Initial request size (in octets). Note that in this
                  --   simplified model the load of the requests and
                  --   TCP acks is not modeled.
                  -> Natural
                  -- ^ The Response size (in octets).
                  -> [(DiffTime, Natural)]
                  -- ^ The arrival pattern (at A) of SDU data from B.
tcpRPCLoadPattern a2b b2a ohead iw' mws irs rs
  = arrivals
  where
    -- the actual data arrivals at A, system being primed with the
    -- intital window.
    arrivals :: [(DiffTime, Natural)]
    arrivals
      = arrival'pattern (initial'window ++ concatMap duplicate'ack ack'arrival'pattern)

    -- the generator of the arrival pattern - clocked by the acks with
    -- their translocation quality attenutated by the bearer.
    --
    -- TBD why is there a free 'b' here?
    arrival'pattern :: [(DiffTime, b)] -> [(DiffTime, Natural)]
    arrival'pattern
      = b2a'bearer . tcp'data'flow rs

    -- the ack pattern - clocked by the arrival the acks from data
    -- packets (again attenutated), acks have no SDU content.
    ack'arrival'pattern :: [(DiffTime, Natural)]
    ack'arrival'pattern
      = a2b'bearer . map (second (const 0)) $ arrivals

    -- the intial window, happens at request arrival time, of initial
    -- burst size.
    initial'window :: [(DiffTime, x)]
    initial'window
      = replicate (fromIntegral $ iw `div` mss)
                  (request'arrival'time, error "initial window")

    -- request arrival time: transit time for the request from A to B,
    -- request deemed to have occured at time 0.
    request'arrival'time :: DiffTime
    request'arrival'time
      = fst . head $  a2b'bearer [(0, irs)]

    -- perform the logical duplication of a ack (i.e the window opening)
    duplicate'ack :: a -> [a]
    duplicate'ack = replicate 2

    --  MSS for the both  directions (note assumption of MTU symmetry)
    mss = e2eMaxSDU (restriction b2a) - ohead

    -- Initial window size
    iw = iw' * mss `min` (2 * mss `max` 14600)

    -- the size of an acknowledgment is a PDH size
    _ack'size = ohead

    -- the a -> b bearer with quality attenutation, transforms from SDU to PDU.
    a2b'bearer :: [(DiffTime, Natural)] -> [(DiffTime, Natural)]
    a2b'bearer = bearerTransitDelay a2b . map (second (+ ohead))

    -- the b -> a bearer with quality attenutation, transforms from SDU to PDU
    b2a'bearer :: [(DiffTime, Natural)] -> [(DiffTime, Natural)]
    b2a'bearer = bearerTransitDelay b2a . map (second (+ ohead))

    -- choose the TCP data flow model
    tcp'data'flow :: Natural -> [(a, b)] -> [(a, Natural)]
    tcp'data'flow = maybe tcp'data'flow'unb (error "TBW - bounded case") mws

    -- TCP data flow (no max window size version)
    tcp'data'flow'unb :: Natural -> [(a, b)] -> [(a, Natural)]
    tcp'data'flow'unb to'send ((ack'time, _):acks)
      | to'send > 2 * mss
      = (ack'time, mss) : (ack'time, mss) : tcp'data'flow'unb (to'send - 2 * mss) acks
      | to'send > mss && to'send <= 2 * mss
      = (ack'time, mss) : (ack'time, to'send - mss) : tcp'data'flow'unb 0 acks
      | to'send > 0 && to'send <= mss
      = (ack'time, to'send) : tcp'data'flow'unb 0 acks
      | to'send <= 0
      = []
    tcp'data'flow'unb _       _ = error "tcp'data'flow'unb partial"


-- | Given a bearer and sequence of offered load, calculate what the
--   arrival pattern would be at the remote end. The modeling
--   assumption is that the restriction is at the ingress, if we were
--   including ∆Q|V in the model other locations of the restriction
--   could be captured.
--
--   FIXME assumes the bearer has a non-bottom delta Q.
--   Will crash if it is bottom.
bearerTransitDelay :: BearerCharacteristics
                   -- ^ Characteristics of path A -> B
                   -> [(DiffTime, Natural)]
                   -- ^ Loading pattern at "A" - leading edge
                   -> [(DiffTime, Natural)]
                   -- ^ Loading pattern at "B" - trailing edge
bearerTransitDelay b as
  = evolve 0 as
  where
    -- The first argument is the time delta after which the link becomes idle
    -- and ready to transmit again (according to the PDU service time from the
    -- link restriction).
    evolve :: DiffTime -> [(DiffTime, Natural)] -> [(DiffTime, Natural)]
    evolve _ [] = []
    evolve next'idle (e@(t,n):as')
      -- The time offset at which to send (t) is later than the time offset at
      -- which the link becomes ready, so we can simply add the transit time to
      -- this list entry. step'idle i set for the recursive case so that
      -- if the next loading pattern time offset is less than that, the delay
      -- will show up in the output.
      | t >= next'idle
        = (t + transit'time e, n) : evolve (step'idle e) as'
      -- The link is the bottleneck here. The data will leave at next'idle.
      | otherwise
        = evolve next'idle ((next'idle, n) : as')

    -- Use the simple G/S ∆Q to determine transit time.
    transit'time :: (DiffTime, Natural) -> DiffTime
    transit'time (_, n)
      = dqG (linkDeltaQ b) + dqS (linkDeltaQ b) n

    -- Use the link restriction to determine idle time: 
    step'idle :: (DiffTime, Natural) -> DiffTime
    step'idle (t, n)
      = t + e2ePDUServiceTime (restriction b) n

    -- Partial functions which assume that the bearer characteristics are not
    -- Bottom.
    -- FIXME this needs to be rethought.
    dqG :: SimpleGS -> DiffTime
    dqG (SimpleDeltaQ g _) = g
    dqG _                  = error "bearerTransitDelay given _|_ for delta q"

    dqS :: SimpleGS -> Natural -> DiffTime
    dqS (SimpleDeltaQ _ s) = s
    dqS _                  = error "bearerTransitDelay given _|_ for delta q"
