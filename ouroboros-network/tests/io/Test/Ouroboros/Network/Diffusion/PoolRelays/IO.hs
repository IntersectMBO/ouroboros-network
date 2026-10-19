{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | What the pool-relays thread holds, in IO: what it retains is visible
-- only to the garbage collector.
module Test.Ouroboros.Network.Diffusion.PoolRelays.IO (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (evaluate)
import Control.Monad.Class.MonadAsync
import Control.Tracer (nullTracer)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Ratio ((%))
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)

import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Diffusion.PoolAllowances
import Ouroboros.Network.Diffusion.PoolRelays
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS (DNSActions (..),
           DNSorIOError (..), PeerActionsDNS (..), constantResource,
           newPoolRelaysDNSSemaphore)
import Ouroboros.Network.Point (WithOrigin (..))

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Diffusion.PoolRelays"
    [ testProperty "a failed poll's list is released by the next" prop_failed_poll_released
    ]

-- | A run with the resolver down: how many pools the ledger lists, the poll
-- whose list is probed, and how many polls to let pass after it.
data Down = Down {
    dPools :: Int,
    dProbe :: Int,
    dAfter :: Int
  }
  deriving Show

instance Arbitrary Down where
  arbitrary = Down <$> choose (1, 40) <*> choose (1, 8) <*> choose (2, 6)
  shrink Down { dPools, dProbe, dAfter } =
       [ Down p dProbe dAfter | p <- shrink dPools, p >= 1 ]
    ++ [ Down dPools k dAfter | k <- shrink dProbe, k >= 1 ]
    ++ [ Down dPools dProbe a | a <- shrink dAfter, a >= 2 ]

-- | With the resolver down and the ledger changing every poll, nothing of a
-- poll's list is held once the next poll is through: a name from the probed
-- poll is dead to the garbage collector two polls on. The loop's state
-- forced is what makes it so; left lazy, the next state is a conditional
-- holding the poll's maps, and they pile up for as long as the resolver is
-- down.
prop_failed_poll_released :: Down -> Property
prop_failed_poll_released Down { dPools, dProbe, dAfter } = ioProperty $ do
    polls      <- newTVarIO (0 :: Int)
    probeVar   <- newTVarIO Nothing
    semaphore  <- newPoolRelaysDNSSemaphore
    allowances <- newPoolAllowances (Allowance 1000) (const (return (Fresh 0))) (Fresh 0)
    w <- arm probeVar
    let -- every poll a new list, naming the probe at the probed poll
        ledger = do
          n <- (+ 1) <$> readTVar polls
          writeTVar polls n
          probe_m <- if n == dProbe
                        then readTVar probeVar <* writeTVar probeVar Nothing
                        else return Nothing
          let name i = case probe_m of
                            Just d | i == 1 -> d
                            _ -> BSC.pack ("pool" ++ show n ++ "-" ++ show i)
          return [ (PoolStake (1 % fromIntegral dPools), LedgerRelayAccessDomain (name i) 3001 :| [])
                 | i <- [1 .. dPools] ]
        interface = LedgerPeersConsensusInterface {
            lpGetLatestSlot  = At . SlotNo . fromIntegral <$> readTVar polls,
            lpGetLedgerPeers = ledger,
            lpExtraAPI       = ()
          }
        noResolver = return (constantResource (Left (IOError (userError "resolver down"))))
        down :: DNSActions () () IO
        down = DNSActions {
            dnsResolverResource      = const noResolver,
            dnsAsyncResolverResource = const noResolver,
            dnsLookupWithTTL         = \_ _ _ _ _ -> error "no resolver, no lookup",
            dnsLookupSRV             = \_ _ _ _ -> error "no resolver, no lookup",
            dnsLookupIPs             = \_ _ _ _ -> error "no resolver, no lookup"
          }
        args :: PoolRelaysArgs () () () IO
        args = PoolRelaysArgs {
            prTracer       = nullTracer,
            prConsensus    = interface,
            prUseLedger    = pure (UseLedgerPeers Always),
            prSnapshot     = pure Nothing,
            prSRVPrefix    = "_cardano._tcp",
            prDNS          = PeerActionsDNS { paToPeerAddr = \_ _ -> (), paDnsActions = down },
            prSemaphore    = semaphore,
            prAddressKey   = id,
            prPollInterval = 0.001
          }
    withAsync (poolRelaysThread args allowances) $ \_ -> do
      atomically $ readTVar polls >>= check . (>= dProbe + dAfter)
      performMajorGC
      alive <- deRefWeak w
      return (counterexample "the probed poll's name is still held" (isNothing alive))
  where
    -- the probe is made and offered here, so that no reference to it
    -- survives but the weak pointer's and the thread's
    arm :: StrictTVar IO (Maybe ByteString) -> IO (Weak ByteString)
    arm var = do
      d <- evaluate (BSC.pack ("probe" ++ show dProbe))
      atomically (writeTVar var (Just d))
      mkWeakPtr d Nothing
