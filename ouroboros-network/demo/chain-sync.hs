{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import           Data.Functor (void)
import           Data.List
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM (STM, atomically, check)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad (when)
import           Control.Tracer

import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Random
import           System.Random.SplitMix

import qualified Codec.Serialise as CBOR

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.Magic
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.NodeToNode
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.Point (WithOrigin (..))
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Testing.ConcreteBlock

import           Ouroboros.Network.Codec
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version

import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Codec as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync

import qualified Ouroboros.Network.Protocol.BlockFetch.Codec as BlockFetch
import qualified Ouroboros.Network.Protocol.BlockFetch.Server as BlockFetch
import qualified Ouroboros.Network.Protocol.BlockFetch.Type as BlockFetch

import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.Client


main :: IO ()
main = do
    args <- getArgs
    let restArgs = drop 2 args
    case args of
      "chainsync":"client":_  ->
        case restArgs of
          []        -> clientChainSync [defaultLocalSocketAddrPath]
          sockAddrs -> clientChainSync sockAddrs
      "chainsync":"server":_          -> do
        let sockAddr = case restArgs of
              addr:_ -> addr
              []     -> defaultLocalSocketAddrPath
        rmIfExists sockAddr
        void $ serverChainSync sockAddr

      "blockfetch":"client":_ ->
        case restArgs of
          []        -> clientBlockFetch [defaultLocalSocketAddrPath]
          sockAddrs -> clientBlockFetch sockAddrs
      "blockfetch":"server":_ -> do
        let sockAddr = case restArgs of
              addr:_ -> addr
              []     -> defaultLocalSocketAddrPath
        rmIfExists sockAddr
        void $ serverBlockFetch sockAddr

      _          -> usage

usage :: IO ()
usage = do
    hPutStrLn stderr "usage: demo-chain-sync [chainsync|blockfetch] {client|server} [addr]"
    exitFailure

defaultLocalSocketAddrPath :: FilePath
#if defined(mingw32_HOST_OS)
defaultLocalSocketAddrPath =  "\\\\.\\pipe\\demo-chain-sync"
#else
defaultLocalSocketAddrPath =  "./demo-chain-sync.sock"
#endif

defaultLocalSocketAddr :: LocalAddress
defaultLocalSocketAddr = localAddressFromPath defaultLocalSocketAddrPath

rmIfExists :: FilePath -> IO ()
rmIfExists path = do
  b <- doesFileExist path
  when b (removeFile path)


--
-- Chain sync demo
--

data DemoProtocol2 = ChainSync2
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocol2 where
  fromProtocolEnum ChainSync2  = MiniProtocolNum 2

instance MiniProtocolLimits DemoProtocol2 where
  maximumIngressQueue _ = maxBound


clientChainSync :: [FilePath] -> IO ()
clientChainSync sockPaths = withIOManager $ \iocp ->
    forConcurrently_ sockPaths $ \sockPath ->
      connectToNode
        (localSnocket iocp sockPath)
        cborTermVersionDataCodec
        nullNetworkConnectTracers
        (simpleSingletonVersions
           (0::Int)
           (NodeToNodeVersionData $ NetworkMagic 0)
           (DictVersion nodeToNodeCodecCBORTerm)
           (\_peerid -> app))
        Nothing
        (localAddressFromPath sockPath)

  where
    app :: OuroborosApplication InitiatorApp DemoProtocol2 LBS.ByteString IO () Void
    app = simpleInitiatorApplication protocols

    protocols :: DemoProtocol2 -> MuxPeer DeserialiseFailure
                                          IO LBS.ByteString ()
    protocols ChainSync2 =
      MuxPeer
        (contramap show stdoutTracer)
         codecChainSync
        (ChainSync.chainSyncClientPeer chainSyncClient)


serverChainSync :: FilePath -> IO Void
serverChainSync sockAddr = withIOManager $ \iocp -> do
    networkState <- newNetworkMutableState
    _ <- async $ cleanNetworkMutableState networkState
    withServerNode
      (localSnocket iocp defaultLocalSocketAddrPath)
      nullNetworkServerTracers
      networkState
      (localAddressFromPath sockAddr)
      cborTermVersionDataCodec
      (\(DictVersion _) -> acceptableVersion)
      (simpleSingletonVersions
        (0::Int)
        (NodeToNodeVersionData $ NetworkMagic 0)
        (DictVersion nodeToNodeCodecCBORTerm)
        (\_peerid -> SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    prng = mkSMGen 0

    app :: OuroborosApplication ResponderApp DemoProtocol2 LBS.ByteString IO Void ()
    app = simpleResponderApplication protocols

    protocols :: DemoProtocol2 -> MuxPeer DeserialiseFailure
                                          IO LBS.ByteString ()
    protocols ChainSync2 =
      MuxPeer
        (contramap show stdoutTracer)
         codecChainSync
        (ChainSync.chainSyncServerPeer (chainSyncServer prng))


codecChainSync :: ( CBOR.Serialise (HeaderHash block)
                  , CBOR.Serialise block
                  , CBOR.Serialise tip
                  )
               => Codec (ChainSync.ChainSync block tip)
                        CBOR.DeserialiseFailure
                        IO LBS.ByteString
codecChainSync =
    ChainSync.codecChainSync
      CBOR.encode CBOR.decode
      CBOR.encode CBOR.decode
      CBOR.encode CBOR.decode


--
-- Block fetch demo
--

data DemoProtocol3 = BlockFetch3 | ChainSync3
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocol3 where
  fromProtocolEnum ChainSync3  = MiniProtocolNum 2
  fromProtocolEnum BlockFetch3 = MiniProtocolNum 3

instance MiniProtocolLimits DemoProtocol3 where
  maximumIngressQueue _ = maxBound


clientBlockFetch :: [FilePath] -> IO ()
clientBlockFetch sockAddrs = withIOManager $ \iocp -> do
    registry   <- newFetchClientRegistry
    blockHeap  <- mkTestFetchedBlockHeap []

    candidateChainsVar <- newTVarIO Map.empty
    currentChainVar    <- newTVarIO genesisChainFragment

    let app :: LocalConnectionId
            -> OuroborosApplication InitiatorApp DemoProtocol3 LBS.ByteString IO () Void
        app peerid = OuroborosInitiatorApplication (protocols peerid)

        protocols :: LocalConnectionId
                  -> DemoProtocol3
                  -> Channel IO LBS.ByteString
                  -> IO ()
        protocols peerid ChainSync3 channel =
          bracket register unregister $ \chainVar ->
          runPeer
            nullTracer -- (contramap (show . TraceLabelPeer peerid) stdoutTracer)
            codecChainSync
            channel
            (ChainSync.chainSyncClientPeer
              (chainSyncClient' syncTracer currentChainVar chainVar))
          where
            register     = atomically $ do
                             chainvar <- newTVar genesisChainFragment
                             modifyTVar' candidateChainsVar
                                         (Map.insert peerid chainvar)
                             return chainvar
            unregister _ = atomically $
                             modifyTVar' candidateChainsVar
                                         (Map.delete peerid)

        protocols peerid BlockFetch3 channel =
          bracketFetchClient registry peerid $ \clientCtx ->
            runPipelinedPeer
              nullTracer -- (contramap (show . TraceLabelPeer peerid) stdoutTracer)
              codecBlockFetch
              channel
              (blockFetchClient clientCtx)

        blockFetchPolicy :: BlockFetchConsensusInterface
                             LocalConnectionId BlockHeader Block IO
        blockFetchPolicy =
            BlockFetchConsensusInterface {
              readCandidateChains    = readTVar candidateChainsVar
                                       >>= traverse readTVar,
              readCurrentChain       = readTVar currentChainVar,
              readFetchMode          = return FetchModeBulkSync,
              readFetchedBlocks      = (\h p -> castPoint p `Set.member` h) <$>
                                         getTestFetchedBlocks blockHeap,
              readFetchedMaxSlotNo   = foldl' max NoMaxSlotNo .
                                       map (maxSlotNoFromWithOrigin . pointSlot) .
                                       Set.elems <$>
                                       getTestFetchedBlocks blockHeap,
              addFetchedBlock        = \p b -> addTestFetchedBlock blockHeap
                                         (castPoint p) (blockHeader b),

              plausibleCandidateChain,
              compareCandidateChains,

              blockFetchSize         = \_ -> 1000,
              blockMatchesHeader     = \_ _ -> True
            }
          where
            plausibleCandidateChain cur candidate =
                AF.headBlockNo candidate > AF.headBlockNo cur

        compareCandidateChains c1 c2 =
          AF.headBlockNo c1 `compare` AF.headBlockNo c2

        chainSelection fingerprint = do
          (fingerprint', currentChain') <- atomically $ do
            candidates <- readTVar candidateChainsVar
                      >>= traverse readTVar
            let fingerprint' = Map.map AF.headPoint candidates
            check (fingerprint /= fingerprint')
--            currentChain <- readTVar currentChainVar
            fetched      <- getTestFetchedBlocks blockHeap
            let currentChain' =
                  -- So this does chain selection by taking the longest
                  -- downloaded candidate chain
                 --FIXME: there's something wrong here, we always get chains
                 -- of length 1.
                  maximumBy compareCandidateChains $
                  [ candidateChainFetched
                  | candidateChain <- Map.elems candidates
                  , let candidateChainFetched =
                          AF.mkAnchoredFragment
                            (AF.anchor candidateChain) $
                          CF.takeWhileOldest
                            (\b -> blockPoint b `Set.member` fetched)
                            (AF.unanchorFragment candidateChain)
                  ]
            writeTVar currentChainVar currentChain'
            return (fingerprint', currentChain')
          traceWith chainTracer (AF.lastPoint currentChain',
                                 AF.headPoint currentChain')
          chainSelection fingerprint'

    peerAsyncs <- sequence
                    [ async $
                        connectToNode
                          (localSnocket iocp defaultLocalSocketAddrPath)
                          cborTermVersionDataCodec
                          nullNetworkConnectTracers
                          (simpleSingletonVersions (0::Int) (NodeToNodeVersionData $ NetworkMagic 0) (DictVersion nodeToNodeCodecCBORTerm) app)
                          Nothing
                          (localAddressFromPath sockAddr)
                    | sockAddr <- sockAddrs ]

    fetchAsync <- async $
                    blockFetchLogic
                      (contramap show stdoutTracer) -- decisionTracer
                      (contramap show stdoutTracer) -- state tracer
                      blockFetchPolicy
                      registry
                      (BlockFetchConfiguration 1 1)
                 >> return ()

    chainAsync <- async (chainSelection Map.empty)

    _ <- waitAnyCancel (fetchAsync : chainAsync : peerAsyncs)
    return ()
  where
--    chainSyncMsgTracer  = contramap (show) stdoutTracer
--    blockFetchMsgTracer = contramap (show) stdoutTracer

--    decisionTracer      = contramap show stdoutTracer
--    clientStateTracer   = contramap show stdoutTracer

    syncTracer :: Tracer IO (Point BlockHeader, Point BlockHeader)
    syncTracer  = contramap (\x -> "sync client: " ++ show x) stdoutTracer

    chainTracer :: Tracer IO (Point BlockHeader, Point BlockHeader)
    chainTracer = contramap (\x -> "cur chain  : " ++ show x) stdoutTracer


serverBlockFetch :: FilePath -> IO Void
serverBlockFetch sockAddr = withIOManager $ \iocp -> do
    networkState <- newNetworkMutableState
    _ <- async $ cleanNetworkMutableState networkState
    withServerNode
      (localSnocket iocp defaultLocalSocketAddrPath)
      nullNetworkServerTracers
      networkState
      (localAddressFromPath sockAddr)
      cborTermVersionDataCodec
      (\(DictVersion _) -> acceptableVersion)
      (simpleSingletonVersions
        (0::Int)
        (NodeToNodeVersionData $ NetworkMagic 0)
        (DictVersion nodeToNodeCodecCBORTerm)
        (\_peerid -> SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    prng = mkSMGen 0

    app :: OuroborosApplication ResponderApp DemoProtocol3 LBS.ByteString IO Void ()
    app = simpleResponderApplication protocols

    protocols :: DemoProtocol3 -> MuxPeer DeserialiseFailure
                                          IO LBS.ByteString ()
    protocols ChainSync3 =
      MuxPeer
        (contramap show stdoutTracer)
         codecChainSync
        (ChainSync.chainSyncServerPeer (chainSyncServer prng))

    protocols BlockFetch3 =
      MuxPeer
        (contramap show stdoutTracer)
         codecBlockFetch
        (BlockFetch.blockFetchServerPeer (blockFetchServer prng))

codecBlockFetch :: Codec (BlockFetch.BlockFetch Block)
                         CBOR.DeserialiseFailure
                         IO LBS.ByteString
codecBlockFetch =
    BlockFetch.codecBlockFetch
      CBOR.encode CBOR.decode
      CBOR.encode CBOR.decode


--
-- Chain sync and block fetch protocol handlers
--

chainSyncClient :: ChainSync.ChainSyncClient
                     BlockHeader (Point BlockHeader) IO ()
chainSyncClient =
    ChainSync.ChainSyncClient $ do
      curvar   <- newTVarIO genesisChainFragment
      chainvar <- newTVarIO genesisChainFragment
      let ChainSync.ChainSyncClient k =
            chainSyncClient' nullTracer curvar chainvar
      k

chainSyncClient' :: Tracer IO (Point BlockHeader, Point BlockHeader)
                 -> TVar (AF.AnchoredFragment BlockHeader)
                 -> TVar (AF.AnchoredFragment BlockHeader)
                 -> ChainSync.ChainSyncClient
                      BlockHeader (Point BlockHeader) IO ()
chainSyncClient' syncTracer _currentChainVar candidateChainVar =
    ChainSync.ChainSyncClient (return requestNext)
  where
    requestNext :: ChainSync.ClientStIdle
                     BlockHeader (Point BlockHeader) IO ()
    requestNext =
      ChainSync.SendMsgRequestNext
        handleNext
        (return handleNext) -- wait case, could trace

    handleNext :: ChainSync.ClientStNext
                    BlockHeader (Point BlockHeader) IO ()
    handleNext =
      ChainSync.ClientStNext {
        recvMsgRollForward  = \header _pHead ->
          ChainSync.ChainSyncClient $ do
            addBlock header
            --FIXME: the notTooFarAhead bit is not working
            -- it seems the current chain is always of length 1.
--            notTooFarAhead
            return requestNext

      , recvMsgRollBackward = \pIntersect _pHead ->
          ChainSync.ChainSyncClient $ do
            rollback pIntersect
            return requestNext
      }

    addBlock :: BlockHeader -> IO ()
    addBlock b = do
        chain <- atomically $ do
          chain <- readTVar candidateChainVar
          let !chain' = shiftAnchoredFragment 50 b chain
          writeTVar candidateChainVar chain'
          return chain'
        traceWith syncTracer (AF.lastPoint chain, AF.headPoint chain)

    rollback :: Point BlockHeader -> IO ()
    rollback p = atomically $ do
        chain <- readTVar candidateChainVar
        -- we do not handle rollback failure in this demo
        let (Just !chain') = AF.rollback p chain
        writeTVar candidateChainVar chain'
{-
    notTooFarAhead = atomically $ do
      currentChain   <- readTVar currentChainVar
      candidateChain <- readTVar candidateChainVar
      check $ case (AF.headBlockNo currentChain,
                    AF.headBlockNo candidateChain) of
                (Just bn, Just bn') -> bn' < bn + 5
                _                   -> True
-}
chainSyncServer :: RandomGen g
                => g
                -> ChainSync.ChainSyncServer
                     BlockHeader (Point BlockHeader) IO ()
chainSyncServer seed =
    let blocks = chainGenerator seed in
    ChainSync.ChainSyncServer (return (idleState blocks))
  where
    idleState :: [Block]
              -> ChainSync.ServerStIdle
                   BlockHeader (Point BlockHeader) IO ()
    idleState blocks =
      ChainSync.ServerStIdle {
        recvMsgRequestNext   = do threadDelay 500000
                                  return (Left (nextState blocks)),
        recvMsgFindIntersect = \_ -> return (intersectState blocks),
        recvMsgDoneClient    = return ()
      }

    nextState :: [Block]
              -> ChainSync.ServerStNext
                   BlockHeader (Point BlockHeader) IO ()
    nextState [] = error "chainSyncServer: impossible"
    nextState (block:blocks) =
      ChainSync.SendMsgRollForward
        (blockHeader block)
         -- pretend chain head is next one:
        (blockPoint (blockHeader (head blocks)))
        (ChainSync.ChainSyncServer (return (idleState blocks)))

    intersectState :: [Block]
                   -> ChainSync.ServerStIntersect
                        BlockHeader (Point BlockHeader) IO ()
    intersectState blocks =
      ChainSync.SendMsgIntersectNotFound
         -- pretend chain head is next one:
        (blockPoint (blockHeader (head blocks)))
        (ChainSync.ChainSyncServer (return (idleState blocks)))


blockFetchServer :: RandomGen g
                 => g
                 -> BlockFetch.BlockFetchServer Block IO ()
blockFetchServer seed =
    let blocks = chainGenerator seed in
    idleState blocks
  where
    idleState blocks =
      BlockFetch.BlockFetchServer
        (\range -> return (senderState range blocks))
        ()

    senderState range blocks =
      case selectBlockRange range blocks of
        Nothing ->
          BlockFetch.SendMsgNoBlocks (return (idleState blocks))

        Just (batch, blocks') ->
          BlockFetch.SendMsgStartBatch $ do
            threadDelay 1000000
            return (sendingState batch blocks')

    sendingState []        blocks =
      BlockFetch.SendMsgBatchDone (return (idleState blocks))
    sendingState (b:batch) blocks =
      BlockFetch.SendMsgBlock b $ do
        threadDelay 1000000
        return (sendingState batch blocks)

selectBlockRange :: BlockFetch.ChainRange Block
                 -> [Block]
                 -> Maybe ([Block], [Block])
selectBlockRange (BlockFetch.ChainRange lower upper) blocks0 = do
    (_,  blocks1)     <- splitBeforePoint lower blocks0
    (bs, b:remaining) <- splitBeforePoint upper blocks1
    return (bs ++ [b], remaining)

splitBeforePoint :: Point Block -> [Block] -> Maybe ([Block], [Block])
splitBeforePoint pt = go []
  where
    go acc (b:bs) =
      case compare (At (blockSlot b)) (pointSlot pt) of
        LT -> go (b:acc) bs
        EQ | pt == castPoint (blockPoint b)
           -> Just (reverse acc, b:bs)
        _  -> Nothing
    go _ [] = Nothing


--
-- Block generator
--

prop_chainGenerator :: RandomGen g => g -> Bool
prop_chainGenerator =
    Chain.valid
  . Chain.fromOldestFirst
  . take 1000
  . chainGenerator

chainGenerator :: RandomGen g => g -> [Block]
chainGenerator g =
    genBlockChain g Nothing

genBlockChain :: RandomGen g => g -> Maybe BlockHeader -> [Block]
genBlockChain !g prevHeader =
    block : genBlockChain g'' (Just (blockHeader block))
  where
    block     = genBlock g' prevHeader
    (g', g'') = split g

genBlock :: RandomGen g => g -> Maybe BlockHeader -> Block
genBlock g prevHeader =
    Block { blockBody, blockHeader }
  where
    blockBody   = genBlockBody g'
    blockHeader = genBlockHeader g'' prevHeader blockBody
    (g', g'')   = split g

genBlockHeader :: RandomGen g
               => g -> Maybe BlockHeader -> BlockBody -> BlockHeader
genBlockHeader g prevHeader body =
    header
  where
    header = BlockHeader {
      headerHash     = hashHeader header,
      headerPrevHash = maybe GenesisHash (BlockHash . headerHash) prevHeader,
      headerSlot     = maybe 1 (addSlotGap slotGap  . headerSlot) prevHeader,
      headerBlockNo  = maybe 1 (succ             . headerBlockNo) prevHeader,
      headerBodyHash = hashBody body
    }
    (slotGap, _) = randomR (1,3) g

    addSlotGap :: Int -> SlotNo -> SlotNo
    addSlotGap m (SlotNo n) = SlotNo (n + fromIntegral m)

genBlockBody :: RandomGen g => g -> BlockBody
genBlockBody g =
    BlockBody . take len . drop offset $ bodyData
  where
    (offset, g') = randomR (0, bodyDataCycle-1)    g
    (len   , _ ) = randomR (1, bodyDataCycle*10-1) g'

bodyData :: String
bodyData = concat
         . replicate 11
         $ doloremIpsum

bodyDataCycle :: Int
bodyDataCycle = length doloremIpsum

doloremIpsum :: String
doloremIpsum = concat 
  [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam hendrerit"
  , "nisi sed sollicitudin pellentesque. Nunc posuere purus rhoncus pulvinar"
  , "aliquam. Ut aliquet tristique nisl vitae volutpat. Nulla aliquet porttitor"
  , "venenatis. Donec a dui et dui fringilla consectetur id nec massa. Aliquam"
  , "erat volutpat. Sed ut dui ut lacus dictum fermentum vel tincidunt neque."
  , "Sed sed lacinia lectus. Duis sit amet sodales felis. Duis nunc eros,"
  , "mattis at dui ac, convallis semper risus. In adipiscing ultrices tellus,"
  , "in suscipit massa vehicula eu."
  ]

--
-- Mock downloaded block heap
--

-- | This provides an interface to a collection of dowloaded blocks. This is
-- enough to implement the 'addFetchedBlock' and 'readFetchedBlocks' methods
-- in the 'BlockFetchConsensusInterface' and related interfaces.
--
-- The interface is enough to use in examples and tests.
--
data TestFetchedBlockHeap m block = TestFetchedBlockHeap {
       getTestFetchedBlocks :: STM (Set (Point block)),
       addTestFetchedBlock  :: Point block -> block -> m ()
     }

-- | Make a 'TestFetchedBlockHeap' using a simple in-memory 'Map', stored in an
-- 'STM' 'TVar'.
--
-- This is suitable for examples and tests.
--
mkTestFetchedBlockHeap :: [Point BlockHeader]
                       -> IO (TestFetchedBlockHeap IO BlockHeader)
mkTestFetchedBlockHeap points = do
    v <- atomically (newTVar (Set.fromList points))
    return TestFetchedBlockHeap {
      getTestFetchedBlocks = readTVar v,
      addTestFetchedBlock  = \p _b -> atomically (modifyTVar' v (Set.insert p))
    }

--
-- Utils
--

genesisChainFragment :: AF.AnchoredFragment BlockHeader
genesisChainFragment = AF.Empty AF.AnchorGenesis

shiftAnchoredFragment :: HasHeader block
                      => Int
                      -> block
                      -> AF.AnchoredFragment block
                      -> AF.AnchoredFragment block
shiftAnchoredFragment n b af =
  case AF.unanchorFragment af of
    cf@(b0 CF.:< cf') | CF.length cf >= n
      -> AF.mkAnchoredFragment (AF.anchorFromBlock b0) (CF.addBlock b cf')
    _ -> AF.addBlock b af
