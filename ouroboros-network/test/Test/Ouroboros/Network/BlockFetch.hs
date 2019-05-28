{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Test.Ouroboros.Network.BlockFetch (tests) where

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.ChainGenerators (TestChainFork(..))

import           GHC.Generics (Generic)
import           Data.Aeson
import           Data.List
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Text (pack)
import           Data.Typeable (Typeable)
import           Data.Dynamic (fromDynamic)
import           Control.Monad.IOSim
import           Control.Tracer (Tracer(Tracer), contramap)
import           Control.Exception (throw)

import           Cardano.BM.Data.LogItem (LogObject)
import           Cardano.BM.Data.Tracer (ToLogObject (..), ToObject (..))
import           Cardano.BM.Setup (setupTrace)
import           Cardano.BM.Trace (appendName)

--TODO: could re-export some of the trace types from more convenient places:
import           Ouroboros.Network.Block
import           Network.TypedProtocol.Driver (TraceSendRecv)
import qualified Ouroboros.Network.Chain            as Chain
import qualified Ouroboros.Network.ChainFragment    as ChainFragment
import qualified Ouroboros.Network.AnchoredFragment as AnchoredFragment
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Testing.ConcreteBlock
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.ClientState
import           Ouroboros.Network.BlockFetch.Examples
import           Test.Ouroboros.Network.BlockFetch.Orphans ()


--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "BlockFetch"
  [ testProperty "static chains without overlap"
                 prop_blockFetchStaticNoOverlap

  , testProperty "static chains without overlap IO"
                 $ withMaxSuccess 2 blockFetchStaticNoOverlapIO

  , testProperty "static chains with overlap"
                 prop_blockFetchStaticWithOverlap
  ]


--
-- Properties
--

-- | In this test we have two candidates chains that are static throughout the
-- run. The two chains share some common prefix (genesis in the degenerate
-- case). The test runs the block fetch logic to download all of both chain
-- candidates.
--
-- In this variant we set up the common prefix of the two candidates as the
-- \"current\" chain. This means the block fetch only has to download the
-- suffixes of the two candidates. This also means that the two suffixes are
-- guaranteed not to have any overlap in their blocks. We rely on this
-- guarantee in this special case to check stronger properties.
--
-- This runs the block fetch and then checks that the trace of the events in
-- that run satisfy the trace properties:
--
-- * 'tracePropertyBlocksRequestedAndRecievedPerPeer'
-- * 'tracePropertyClientStateSanity'
--
prop_blockFetchStaticNoOverlap :: TestChainFork -> Property
prop_blockFetchStaticNoOverlap (TestChainFork common fork1 fork2) =
    let trace = selectTraceDynamic $
                runSimTrace $
                  blockFetchExample1
                    (contramap TraceFetchDecision       dynamicTracer)
                    (contramap TraceFetchClientState    dynamicTracer)
                    (contramap TraceFetchClientSendRecv dynamicTracer)
                    common' forks

     in counterexample ("\nTrace:\n" ++ unlines (map show trace)) $

        -- For fetch reqs added and received, we observe exactly the sequence
        -- of blocks we expect, which is the whole fork suffix.
        tracePropertyBlocksRequestedAndRecievedPerPeer fork1'' fork2'' trace

        -- state sanity check
   .&&. property (tracePropertyClientStateSanity trace)

  where
    -- TODO: consider making a specific generator for anchored fragment forks
    common' = chainToAnchoredFragment common
    fork1'  = chainToAnchoredFragment fork1
    fork2'  = chainToAnchoredFragment fork2
    forks   = [fork1', fork2']
    -- And just the extensions
    Just (_, _, fork1'', fork2'') = AnchoredFragment.intersect fork1' fork2'

blockFetchStaticNoOverlapIO :: TestChainFork -> Property
blockFetchStaticNoOverlapIO (TestChainFork common fork1 fork2) =
    ioProperty $ do
        let configFile = "cfg/test-logging.yaml"
        trace :: Tracer IO (LogObject Example1TraceEvent)
            <- setupTrace (Left configFile) $ pack "block-fetch"
        -- add names to the three tracers
        tracerDecision       <- toLogObject <$> appendName (pack "decision")         trace
        tracerClientState    <- toLogObject <$> appendName (pack "client-state")     trace
        tracerClientSendRecv <- toLogObject <$> appendName (pack "client-send-recv") trace
        blockFetchExample1
            (contramap TraceFetchDecision       tracerDecision)
            (contramap TraceFetchClientState    tracerClientState)
            (contramap TraceFetchClientSendRecv tracerClientSendRecv)
            common' forks
        putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~"
        return True

        -- For fetch reqs added and received, we observe exactly the sequence
        -- of blocks we expect, which is the whole fork suffix.
        -- tracePropertyBlocksRequestedAndRecievedPerPeer fork1'' fork2'' trace

        -- state sanity check

  where
    -- TODO: consider making a specific generator for anchored fragment forks
    common' = chainToAnchoredFragment common
    fork1'  = chainToAnchoredFragment fork1
    fork2'  = chainToAnchoredFragment fork2
    forks   = [fork1', fork2']

instance ToObject [TraceLabelPeer Int (FetchDecision [Point BlockHeader])]
instance ToObject (TraceLabelPeer Int (TraceFetchClientState BlockHeader))
instance ToObject (TraceLabelPeer Int (TraceSendRecv (BlockFetch BlockHeader Block)))

-- | In this test we have two candidates chains that are static throughout the
-- run. The two chains share some common prefix (genesis in the degenerate
-- case). The test runs the block fetch logic to download all of both chain
-- candidates.
--
-- In this variant we set up the \"current\" chain as empty (genesis). This
-- means the block has to download the whole of both candidates. This also
-- means that we typically expect there to be overlap in the blocks in the two
-- chains. We rely on there typically being overlap to check properties of that
-- overlap.
--
-- This runs the block fetch and then checks that the trace of the events in
-- that run satisfy the trace properties:
--
-- * 'tracePropertyBlocksRequestedAndRecievedAllPeers'
-- * 'tracePropertyNoDuplicateBlocksBetweenPeers'
-- * 'tracePropertyClientStateSanity'
--
prop_blockFetchStaticWithOverlap :: TestChainFork -> Property
prop_blockFetchStaticWithOverlap (TestChainFork _common fork1 fork2) =
    let trace = selectTraceDynamic $
                runSimTrace $
                  blockFetchExample1
                    (contramap TraceFetchDecision       dynamicTracer)
                    (contramap TraceFetchClientState    dynamicTracer)
                    (contramap TraceFetchClientSendRecv dynamicTracer)
                    (AnchoredFragment.Empty Chain.genesisPoint) forks

     in counterexample ("\nTrace:\n" ++ unlines (map show trace)) $

        -- For fetch reqs added and received, between the two peers we observe
        -- the set of blocks we expect, which is the union of the two chains.
        tracePropertyBlocksRequestedAndRecievedAllPeers fork1' fork2' trace

        -- For fetch reqs added, the set of blocks added for the two peers
        -- should not intersect
   .&&. tracePropertyNoDuplicateBlocksBetweenPeers fork1' fork2' trace

        -- state sanity check
   .&&. property (tracePropertyClientStateSanity trace)

  where
    -- TODO: consider making a specific generator for anchored fragment forks
    fork1'  = chainToAnchoredFragment fork1
    fork2'  = chainToAnchoredFragment fork2
    forks   = [fork1', fork2']

chainToAnchoredFragment :: Chain.Chain Block -> AnchoredFragment Block
chainToAnchoredFragment =
    AnchoredFragment.fromNewestFirst Chain.genesisPoint
  . Chain.chainToList

-- TODO: move elsewhere and generalise
chainPoints :: AnchoredFragment Block -> [Point BlockHeader]
chainPoints = map (castPoint . blockPoint)
            . AnchoredFragment.toOldestFirst

data Example1TraceEvent =
     TraceFetchDecision       [TraceLabelPeer Int
                                (FetchDecision [Point BlockHeader])]
   | TraceFetchClientState    (TraceLabelPeer Int
                                (TraceFetchClientState BlockHeader))
   | TraceFetchClientSendRecv (TraceLabelPeer Int
                                (TraceSendRecv (BlockFetch BlockHeader Block)))

instance Show Example1TraceEvent where
  show (TraceFetchDecision       x) = "TraceFetchDecision " ++ show x
  show (TraceFetchClientState    x) = show x
  show (TraceFetchClientSendRecv x) = show (fmap (\_ -> "msg") x)


deriving instance Generic Example1TraceEvent
deriving instance ToJSON Example1TraceEvent
instance ToObject Example1TraceEvent

-- | Check the execution trace for a particular property: we observe all the
-- blocks in the 'FetchRequest's added by the decision logic and the blocks
-- received by the fetch clients; check that the ordered sequence of blocks
-- requested and completed by both fetch clients is exactly the sequence
-- expected. The expected sequence is exactly the chain suffixes in order.
--
-- This property is stronger than 'tracePropertyBlocksRequestedAndRecievedAllPeers'
-- since it works with sequences rather than sets and for each chain
-- individually rather than both chains together. It only holds for the
-- situation where the suffixes of the chains that need to be fetched are
-- disjoint, sharing no common prefix.
--
-- It turns out that no duplicates part is not trivial. Earlier versions of the
-- block fetch logic did not satisfy this in all cases.
--
tracePropertyBlocksRequestedAndRecievedPerPeer
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> [Example1TraceEvent]
  -> Property
tracePropertyBlocksRequestedAndRecievedPerPeer fork1 fork2 es =
      requestedFetchPoints === requiredFetchPoints
 .&&. receivedFetchPoints  === requiredFetchPoints
  where
    requiredFetchPoints =
      Map.filter (not . Prelude.null) $
      Map.fromList $
        [ (1, chainPoints fork1)
        , (2, chainPoints fork2)
        ]

    requestedFetchPoints :: Map Int [Point BlockHeader]
    requestedFetchPoints =
      Map.fromListWith (flip (++))
        [ (peer, map blockPoint (ChainFragment.toOldestFirst fragment))
        | TraceFetchClientState
            (TraceLabelPeer peer
              (AddedFetchRequest
                (FetchRequest fragments) _ _ _)) <- es
        , fragment <- fragments
        ]

    receivedFetchPoints :: Map Int [Point BlockHeader]
    receivedFetchPoints =
      Map.fromListWith (flip (++))
        [ (peer, [pt])
        | TraceFetchClientState
            (TraceLabelPeer peer (CompletedBlockFetch pt _ _ _)) <- es
        ]


-- | Check the execution trace for a particular property: we observe all the
-- blocks in the 'FetchRequest's added by the decision logic and the blocks
-- received by the fetch clients; check that the set of all blocks requested
-- across the two peers is the set of blocks we expect, and similarly for the
-- set of all blocks received. The expected set of blocks is the union of the
-- blocks on the two candidate chains.
--
-- This property is weaker than 'tracePropertyBlocksRequestedAndRecievedPerPeer'
-- since it does not involve order or frequency, but it holds for the general
-- case of multiple chains with common prefixes.
--
tracePropertyBlocksRequestedAndRecievedAllPeers
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> [Example1TraceEvent]
  -> Property
tracePropertyBlocksRequestedAndRecievedAllPeers fork1 fork2 es =
      requestedFetchPoints === requiredFetchPoints
 .&&. receivedFetchPoints  === requiredFetchPoints
  where
    requiredFetchPoints =
      Set.fromList (chainPoints fork1 ++ chainPoints fork2)

    requestedFetchPoints :: Set (Point BlockHeader)
    requestedFetchPoints =
      Set.fromList
        [ blockPoint block
        | TraceFetchClientState
            (TraceLabelPeer _
              (AddedFetchRequest
                (FetchRequest fragments) _ _ _)) <- es
        , fragment <- fragments
        , block    <- ChainFragment.toOldestFirst fragment
        ]

    receivedFetchPoints :: Set (Point BlockHeader)
    receivedFetchPoints =
      Set.fromList
        [ pt
        | TraceFetchClientState
            (TraceLabelPeer _ (CompletedBlockFetch pt _ _ _)) <- es
        ]


-- | Check the execution trace for a particular property: we observe all the
-- blocks in the 'FetchRequest's added by the decision logic; check that the
-- set blocks requested for one peer and for the other do not intersect.
--
-- This is a non-trivial property because in the general case, the chain
-- suffixes do intersect by sharing a common prefix. This property therefore
-- demonstrates that in the 'FetchModeBulkSync' the decision logic is properly
-- allocating fetch requests to different peers to avoid asking for duplicate
-- blocks from different peers.
--
tracePropertyNoDuplicateBlocksBetweenPeers
  :: AnchoredFragment Block
  -> AnchoredFragment Block
  -> [Example1TraceEvent]
  -> Property
tracePropertyNoDuplicateBlocksBetweenPeers fork1 fork2 es =
    classify (hasDupes requiredFetchPoints)
             "Overlapping chains between peers" $

    Set.null $

    Map.findWithDefault Set.empty 1 requestedFetchPoints
      `Set.intersection`
    Map.findWithDefault Set.empty 2 requestedFetchPoints

  where
    hasDupes = not . null . filter ((>1) . length)  . group . sort

    requiredFetchPoints =
      nub (chainPoints fork1 ++ chainPoints fork2)

    requestedFetchPoints :: Map Int (Set (Point BlockHeader))
    requestedFetchPoints =
      Map.fromListWith Set.union
        [ (peer, points fragment)
        | TraceFetchClientState
            (TraceLabelPeer peer
              (AddedFetchRequest
                (FetchRequest fragments) _ _ _)) <- es
        , fragment <- fragments
        , let points = Set.fromList . map blockPoint
                     . ChainFragment.toOldestFirst
        ]


-- | This is just a sanity check on the in-flight statistics maintained between
-- the decision logic thread and the block fetch client threads.
--
tracePropertyClientStateSanity :: [Example1TraceEvent] -> Bool
tracePropertyClientStateSanity es =
    and [ saneStateValues inflight status
        | TraceFetchClientState
            (TraceLabelPeer _
              (AddedFetchRequest _ inflight _ status)) <- es
        ]
  where
    saneStateValues PeerFetchInFlight {..} status =
        -- Here we know the fixed dummy block size so we know exactly what
        -- the bytes in flight should be.
           Set.size peerFetchBlocksInFlight * 2000
        == fromIntegral peerFetchBytesInFlight

     && case status of
          PeerFetchStatusReady _ -> Set.size peerFetchBlocksInFlight <  3
          PeerFetchStatusBusy    -> Set.size peerFetchBlocksInFlight == 3
          _                      -> False -- not used in this test

     && if peerFetchReqsInFlight == 0
           then Set.null peerFetchBlocksInFlight
           else True


-- TODO: the idea of this property was that we check that we're not making too
-- many decisions and waking up unnecessarily. But it's not trivial since
-- towards the end of a download we can have multiple occurences of decisions
-- where it's just the trailing data that's in-flight. Need to think about
-- whether there's any concise and robust way of expressing this.
--
-- tracePropertyDecisions _fork1 _fork2 _es = True


--
-- Trace utils
--

dynamicTracer :: Typeable a => Tracer (SimM s) a
dynamicTracer = Tracer traceM

-- | Select all the traced values matching the expected type. This relies on
-- the sim's dynamic trace facility.
--
-- For convenience, this throws exceptions for abnormal sim termination.
--
selectTraceDynamic :: Typeable b => Trace a -> [b]
selectTraceDynamic = go
  where
    go (Trace _ _ (EventLog dyn) trace) = case fromDynamic dyn of
                                            Just x  -> x : go trace
                                            Nothing ->     go trace
    go (Trace _ _ _              trace) = go trace
    go (TraceMainException _ e _)       = throw (FailureException e)
    go (TraceDeadlock      _   _)       = throw FailureDeadlock
    go (TraceMainReturn    _ _ _)       = []

