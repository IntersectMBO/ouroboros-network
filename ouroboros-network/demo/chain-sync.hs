{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main where

import           Data.List
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Void (Void)

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, atomically, check)
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Control.Monad (when)
import Control.Exception
import Control.Tracer

import System.IO
import System.Directory
import System.Environment
import System.Exit
import System.Random
import System.Random.SplitMix

import           Codec.Serialise (DeserialiseFailure)
import qualified Codec.Serialise as CBOR
import           Codec.SerialiseTerm

import           Network.Mux

import qualified Network.Socket as Socket

import Ouroboros.Network.Block
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.ChainFragment as CF
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Testing.ConcreteBlock
import Ouroboros.Network.Socket
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToNode

import Network.TypedProtocol.Channel
import Network.TypedProtocol.Driver
import Network.TypedProtocol.Pipelined

import Network.TypedProtocol.PingPong.Client as PingPong
import Network.TypedProtocol.PingPong.Codec.Cbor
import Network.TypedProtocol.PingPong.Server as PingPong

import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.Handshake.Version

import Ouroboros.Network.Protocol.ChainSync.Codec  as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Client as ChainSync

import Ouroboros.Network.Protocol.BlockFetch.Type   as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Codec  as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Server as BlockFetch

import Ouroboros.Network.BlockFetch
import Ouroboros.Network.BlockFetch.Client


main :: IO ()
main = do
    args <- getArgs
    case args of
      "pingpong":"client":[]           -> clientPingPong False
      "pingpong":"client-pipelined":[] -> clientPingPong True
      "pingpong":"server":[] -> do
        rmIfExists defaultLocalSocketAddrPath
        serverPingPong

      "pingpong2":"client":[] -> clientPingPong2
      "pingpong2":"server":[] -> do
        rmIfExists defaultLocalSocketAddrPath
        serverPingPong2

      "chainsync":"client":sockAddrs   -> clientChainSync sockAddrs
      "chainsync":"server":sockAddr:[] -> do
        rmIfExists sockAddr
        serverChainSync sockAddr

      "blockfetch":"client":sockAddrs   -> clientBlockFetch sockAddrs
      "blockfetch":"server":sockAddr:[] -> do
        rmIfExists sockAddr
        serverBlockFetch sockAddr

      _          -> usage

usage :: IO ()
usage = do
    hPutStrLn stderr "usage: demo-chain-sync [pingpong|pingpong2|chainsync|blockfetch] {client|server} [addr]"
    exitFailure

mkLocalSocketAddrInfo :: FilePath -> Socket.AddrInfo
mkLocalSocketAddrInfo socketPath =
    Socket.AddrInfo
      []
      Socket.AF_UNIX
      Socket.Stream
      Socket.defaultProtocol
      (Socket.SockAddrUnix socketPath)
      Nothing

defaultLocalSocketAddrPath :: FilePath
defaultLocalSocketAddrPath =  "./demo-chain-sync.sock"

defaultLocalSocketAddrInfo :: Socket.AddrInfo
defaultLocalSocketAddrInfo =
    mkLocalSocketAddrInfo defaultLocalSocketAddrPath

rmIfExists :: FilePath -> IO ()
rmIfExists path = do
  b <- doesFileExist path
  when b (removeFile path)

--
-- Ping pong demo
--

data DemoProtocol0 = PingPong0
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocol0 where
  fromProtocolEnum PingPong0 = 2
  toProtocolEnum 2 = Just PingPong0
  toProtocolEnum _ = Nothing

instance MiniProtocolLimits DemoProtocol0 where
  maximumMessageSize _ = maxBound
  maximumIngressQueue _ = maxBound


clientPingPong :: Bool -> IO ()
clientPingPong pipelined =
    connectToNode
      (\(DictVersion codec) -> encodeTerm codec)
      (\(DictVersion codec) -> decodeTerm codec)
      (,)
      (simpleSingletonVersions (0::Int) (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) app)
      Nothing
      defaultLocalSocketAddrInfo
  where
    app :: OuroborosApplication InitiatorApp (Socket.SockAddr, Socket.SockAddr) DemoProtocol0 IO LBS.ByteString () Void
    app = simpleInitiatorApplication protocols

    protocols :: DemoProtocol0 -> MuxPeer (Socket.SockAddr, Socket.SockAddr) DeserialiseFailure IO LBS.ByteString ()
    protocols PingPong0 | pipelined =
      MuxPeerPipelined
        (contramap show stdoutTracer)
        codecPingPong
        (pingPongClientPeerPipelined (pingPongClientPipelinedMax 5))

    protocols PingPong0 =
      MuxPeer
        (contramap show stdoutTracer)
        codecPingPong
        (pingPongClientPeer (pingPongClientCount 5))


pingPongClientCount :: Applicative m => Int -> PingPongClient m ()
pingPongClientCount 0 = PingPong.SendMsgDone ()
pingPongClientCount n = SendMsgPing (pure (pingPongClientCount (n-1)))

serverPingPong :: IO ()
serverPingPong = do
    tbl <- newConnectionTable
    withSimpleServerNode
      tbl
      defaultLocalSocketAddrInfo
      (\(DictVersion codec)-> encodeTerm codec)
      (\(DictVersion codec)-> decodeTerm codec)
      (,)
      (\(DictVersion _) -> acceptEq)
      (simpleSingletonVersions (0::Int) (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) app) $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    app :: OuroborosApplication ResponderApp (Socket.SockAddr, Socket.SockAddr) DemoProtocol0 IO LBS.ByteString Void ()
    app = simpleResponderApplication protocols

    protocols :: DemoProtocol0 -> MuxPeer (Socket.SockAddr, Socket.SockAddr) DeserialiseFailure IO LBS.ByteString ()
    protocols PingPong0 =
      MuxPeer
        (contramap show stdoutTracer)
        codecPingPong
        (pingPongServerPeer pingPongServerStandard)

pingPongServerStandard
  :: Applicative m
  => PingPongServer m ()
pingPongServerStandard =
    PingPongServer {
      recvMsgPing = pure pingPongServerStandard,
      recvMsgDone = ()
    }


--
-- Ping pong demo2
--

data DemoProtocol1 = PingPong1 | PingPong1'
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocol1 where
  fromProtocolEnum PingPong1  = 2
  fromProtocolEnum PingPong1' = 3
  toProtocolEnum 2 = Just PingPong1
  toProtocolEnum 3 = Just PingPong1'
  toProtocolEnum _ = Nothing

instance MiniProtocolLimits DemoProtocol1 where
  maximumMessageSize _ = maxBound
  maximumIngressQueue _ = maxBound


clientPingPong2 :: IO ()
clientPingPong2 =
    connectToNode
      (\(DictVersion codec) -> encodeTerm codec)
      (\(DictVersion codec) -> decodeTerm codec)
      (,)
      (simpleSingletonVersions (0::Int) (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) app)
      Nothing
      defaultLocalSocketAddrInfo
  where
    app :: OuroborosApplication InitiatorApp (Socket.SockAddr, Socket.SockAddr) DemoProtocol1 IO LBS.ByteString () Void
    app = simpleInitiatorApplication protocols

    protocols :: DemoProtocol1 -> MuxPeer
                                    (Socket.SockAddr, Socket.SockAddr)
                                    DeserialiseFailure IO LBS.ByteString ()
    protocols PingPong1 =
      MuxPeer
        (contramap (show . (,) (1 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongClientPeer (pingPongClientCount 5))

    protocols PingPong1' =
      MuxPeer
        (contramap (show . (,) (2 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongClientPeer (pingPongClientCount 5))

pingPongClientPipelinedMax
  :: forall m. Monad m
  => Int
  -> PingPongClientPipelined m ()
pingPongClientPipelinedMax c =
    PingPongClientPipelined (go [] Zero 0)
  where
    go :: [Either Int Int] -> Nat o -> Int
       -> PingPongSender o Int m ()
    go acc o        n | n < c
                      = SendMsgPingPipelined
                          (return n)
                          (go (Left n : acc) (Succ o) (succ n))
    go _    Zero     _ = SendMsgDonePipelined ()
    go acc (Succ o) n = CollectPipelined
                          Nothing
                          (\n' -> go (Right n' : acc) o n)

serverPingPong2 :: IO ()
serverPingPong2 = do
    tbl <- newConnectionTable
    withSimpleServerNode
      tbl
      defaultLocalSocketAddrInfo
      (\(DictVersion codec)-> encodeTerm codec)
      (\(DictVersion codec)-> decodeTerm codec)
      (,)
      (\(DictVersion _) -> acceptEq)
      (simpleSingletonVersions (0::Int) (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) app) $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    app :: OuroborosApplication ResponderApp (Socket.SockAddr, Socket.SockAddr) DemoProtocol1 IO LBS.ByteString Void ()
    app = simpleResponderApplication protocols

    protocols :: DemoProtocol1 -> MuxPeer (Socket.SockAddr, Socket.SockAddr)
                                           DeserialiseFailure IO LBS.ByteString ()
    protocols PingPong1 =
      MuxPeer
        (contramap (show . (,) (1 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongServerPeer pingPongServerStandard)

    protocols PingPong1' =
      MuxPeer
        (contramap (show . (,) (2 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongServerPeer pingPongServerStandard)


--
-- Chain sync demo
--

data DemoProtocol2 = ChainSync2
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocol2 where
  fromProtocolEnum ChainSync2  = 2

  toProtocolEnum 2 = Just ChainSync2
  toProtocolEnum _ = Nothing

instance MiniProtocolLimits DemoProtocol2 where
  maximumMessageSize _ = maxBound
  maximumIngressQueue _ = maxBound


clientChainSync :: [FilePath] -> IO ()
clientChainSync sockAddrs =
    forConcurrently_ sockAddrs $ \sockAddr ->
      connectToNode
        (\(DictVersion codec) -> encodeTerm codec)
        (\(DictVersion codec) -> decodeTerm codec)
        (,)
        (simpleSingletonVersions (0::Int) (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) app)
        Nothing
        (mkLocalSocketAddrInfo sockAddr)
  where
    app :: OuroborosApplication InitiatorApp (Socket.SockAddr, Socket.SockAddr) DemoProtocol2 IO LBS.ByteString () Void
    app = simpleInitiatorApplication protocols

    protocols :: DemoProtocol2 -> MuxPeer (Socket.SockAddr, Socket.SockAddr)
                                          DeserialiseFailure IO LBS.ByteString ()
    protocols ChainSync2 =
      MuxPeer
        (contramap show stdoutTracer)
        (codecChainSync CBOR.encode CBOR.decode CBOR.encode CBOR.decode)
        (chainSyncClientPeer chainSyncClient)


serverChainSync :: FilePath -> IO ()
serverChainSync sockAddr = do
    tbl <- newConnectionTable
    withSimpleServerNode
      tbl
      (mkLocalSocketAddrInfo sockAddr)
      (\(DictVersion codec)-> encodeTerm codec)
      (\(DictVersion codec)-> decodeTerm codec)
      (,)
      (\(DictVersion _) -> acceptEq)
      (simpleSingletonVersions (0::Int) (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) app) $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    prng = mkSMGen 0

    app :: OuroborosApplication ResponderApp (Socket.SockAddr, Socket.SockAddr) DemoProtocol2 IO LBS.ByteString Void ()
    app = simpleResponderApplication protocols

    protocols :: DemoProtocol2 -> MuxPeer (Socket.SockAddr, Socket.SockAddr)
                                          DeserialiseFailure IO LBS.ByteString ()
    protocols ChainSync2 =
      MuxPeer
        (contramap show stdoutTracer)
        (codecChainSync CBOR.encode CBOR.decode CBOR.encode CBOR.decode)
        (chainSyncServerPeer (chainSyncServer prng))



--
-- Block fetch demo
--

data DemoProtocol3 = BlockFetch3 | ChainSync3
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocol3 where
  fromProtocolEnum ChainSync3  = 2
  fromProtocolEnum BlockFetch3 = 3

  toProtocolEnum 2 = Just ChainSync3
  toProtocolEnum 3 = Just BlockFetch3
  toProtocolEnum _ = Nothing

instance MiniProtocolLimits DemoProtocol3 where
  maximumMessageSize _ = maxBound
  maximumIngressQueue _ = maxBound


clientBlockFetch :: [FilePath] -> IO ()
clientBlockFetch sockAddrs = do
    registry   <- newFetchClientRegistry
    blockHeap  <- mkTestFetchedBlockHeap []

    candidateChainsVar <- newTVarIO Map.empty
    currentChainVar    <- newTVarIO genesisChainFragment

    let app :: OuroborosApplication InitiatorApp (Socket.SockAddr, Socket.SockAddr) DemoProtocol3 IO LBS.ByteString () Void
        app = OuroborosInitiatorApplication protocols

        protocols :: (Socket.SockAddr, Socket.SockAddr)
                  -> DemoProtocol3
                  -> Channel IO LBS.ByteString
                  -> IO ()
        protocols peerid ChainSync3 channel =
          bracket register unregister $ \chainVar ->
          runPeer
            nullTracer -- (contramap (show . TraceLabelPeer peerid) stdoutTracer)
            (codecChainSync CBOR.encode CBOR.decode CBOR.encode CBOR.decode)
            peerid
            channel
            (chainSyncClientPeer
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
              (codecBlockFetch CBOR.encode CBOR.encode CBOR.decode CBOR.decode)
              peerid
              channel
              (blockFetchClient clientCtx)

        blockFetchPolicy :: BlockFetchConsensusInterface (Socket.SockAddr, Socket.SockAddr)
                              BlockHeader Block IO
        blockFetchPolicy =
            BlockFetchConsensusInterface {
              readCandidateChains    = readTVar candidateChainsVar
                                       >>= traverse readTVar,
              readCurrentChain       = readTVar currentChainVar,
              readFetchMode          = return FetchModeBulkSync,
              readFetchedBlocks      = (\h p -> castPoint p `Set.member` h) <$>
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
                            (AF.anchorPoint candidateChain) $
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
                          (\(DictVersion codec) -> encodeTerm codec)
                          (\(DictVersion codec) -> decodeTerm codec)
                          (,)
                          (simpleSingletonVersions (0::Int) (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) app)
                          Nothing
                          (mkLocalSocketAddrInfo sockAddr)
                    | sockAddr <- sockAddrs ]

    fetchAsync <- async $
                    blockFetchLogic
                      (contramap show stdoutTracer) -- decisionTracer
                      (contramap show stdoutTracer) -- state tracer
                      blockFetchPolicy
                      registry
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


serverBlockFetch :: FilePath -> IO ()
serverBlockFetch sockAddr = do
    tbl <- newConnectionTable
    withSimpleServerNode
      tbl
      (mkLocalSocketAddrInfo sockAddr)
      (\(DictVersion codec)-> encodeTerm codec)
      (\(DictVersion codec)-> decodeTerm codec)
      (,)
      (\(DictVersion _) -> acceptEq)
      (simpleSingletonVersions (0::Int) (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) app) $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    prng = mkSMGen 0

    app :: OuroborosApplication ResponderApp (Socket.SockAddr, Socket.SockAddr) DemoProtocol3 IO LBS.ByteString Void ()
    app = simpleResponderApplication protocols

    protocols :: DemoProtocol3 -> MuxPeer (Socket.SockAddr, Socket.SockAddr)
                                          DeserialiseFailure IO LBS.ByteString ()
    protocols ChainSync3 =
      MuxPeer
        (contramap show stdoutTracer)
        (codecChainSync CBOR.encode CBOR.decode CBOR.encode CBOR.decode)
        (chainSyncServerPeer (chainSyncServer prng))

    protocols BlockFetch3 =
      MuxPeer
        (contramap show stdoutTracer)
        (codecBlockFetch CBOR.encode CBOR.encode CBOR.decode CBOR.decode)
        (blockFetchServerPeer (blockFetchServer prng))


--
-- Chain sync and block fetch protocol handlers
--

chainSyncClient :: ChainSyncClient BlockHeader (Point BlockHeader) IO ()
chainSyncClient =
    ChainSyncClient $ do
      curvar   <- newTVarIO genesisChainFragment
      chainvar <- newTVarIO genesisChainFragment
      let ChainSyncClient k = chainSyncClient' nullTracer curvar chainvar
      k

chainSyncClient' :: Tracer IO (Point BlockHeader, Point BlockHeader)
                 -> TVar (AF.AnchoredFragment BlockHeader)
                 -> TVar (AF.AnchoredFragment BlockHeader)
                 -> ChainSyncClient BlockHeader (Point BlockHeader) IO ()
chainSyncClient' syncTracer _currentChainVar candidateChainVar =
    ChainSyncClient (return requestNext)
  where
    requestNext :: ClientStIdle BlockHeader (Point BlockHeader) IO ()
    requestNext =
      SendMsgRequestNext
        handleNext
        (return handleNext) -- wait case, could trace

    handleNext :: ClientStNext BlockHeader (Point BlockHeader) IO ()
    handleNext =
      ClientStNext {
        recvMsgRollForward  = \header _pHead ->
          ChainSyncClient $ do
            addBlock header
            --FIXME: the notTooFarAhead bit is not working
            -- it seems the current chain is always of length 1.
--            notTooFarAhead
            return requestNext

      , recvMsgRollBackward = \pIntersect _pHead ->
          ChainSyncClient $ do
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
                -> ChainSyncServer BlockHeader (Point BlockHeader) IO ()
chainSyncServer seed =
    let blocks = chainGenerator seed in
    ChainSyncServer (return (idleState blocks))
  where
    idleState :: [Block]
              -> ServerStIdle BlockHeader (Point BlockHeader) IO ()
    idleState blocks =
      ServerStIdle {
        recvMsgRequestNext   = do threadDelay 500000
                                  return (Left (nextState blocks)),
        recvMsgFindIntersect = \_ -> return (intersectState blocks),
        recvMsgDoneClient    = return ()
      }

    nextState :: [Block]
              -> ServerStNext BlockHeader (Point BlockHeader) IO ()
    nextState [] = error "chainSyncServer: impossible"
    nextState (block:blocks) =
      SendMsgRollForward
        (blockHeader block)
         -- pretend chain head is next one:
        (blockPoint (blockHeader (head blocks)))
        (ChainSyncServer (return (idleState blocks)))

    intersectState :: [Block]
                   -> ServerStIntersect BlockHeader (Point BlockHeader) IO ()
    intersectState blocks =
      SendMsgIntersectUnchanged
         -- pretend chain head is next one:
        (blockPoint (blockHeader (head blocks)))
        (ChainSyncServer (return (idleState blocks)))


blockFetchServer :: RandomGen g
                 => g
                 -> BlockFetchServer Block IO ()
blockFetchServer seed =
    let blocks = chainGenerator seed in
    idleState blocks
  where
    idleState blocks =
      BlockFetchServer
        (\range -> return (senderState range blocks))
        ()

    senderState range blocks =
      case selectBlockRange range blocks of
        Nothing ->
          SendMsgNoBlocks (return (idleState blocks))

        Just (batch, blocks') ->
          SendMsgStartBatch $ do
            threadDelay 1000000
            return (sendingState batch blocks')

    sendingState []        blocks =
      SendMsgBatchDone (return (idleState blocks))
    sendingState (b:batch) blocks =
      SendMsgBlock b $ do
        threadDelay 1000000
        return (sendingState batch blocks)

selectBlockRange :: ChainRange Block
                 -> [Block]
                 -> Maybe ([Block], [Block])
selectBlockRange (ChainRange lower upper) blocks0 = do
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
      headerSigner   = expectedBFTSigner (headerSlot header),
      headerBodyHash = hashBody body
    }
    (slotGap, _) = randomR (1,3) g

    expectedBFTSigner :: SlotNo -> BlockSigner
    expectedBFTSigner (SlotNo n) = BlockSigner (n `mod` 7)

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
doloremIpsum =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam hendrerit\
  \ nisi sed sollicitudin pellentesque. Nunc posuere purus rhoncus pulvinar\
  \ aliquam. Ut aliquet tristique nisl vitae volutpat. Nulla aliquet porttitor\
  \ venenatis. Donec a dui et dui fringilla consectetur id nec massa. Aliquam\
  \ erat volutpat. Sed ut dui ut lacus dictum fermentum vel tincidunt neque.\
  \ Sed sed lacinia lectus. Duis sit amet sodales felis. Duis nunc eros,\
  \ mattis at dui ac, convallis semper risus. In adipiscing ultrices tellus,\
  \ in suscipit massa vehicula eu."

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
       getTestFetchedBlocks  :: STM (Set (Point block)),
       addTestFetchedBlock   :: Point block -> block -> m ()
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
genesisChainFragment = AF.Empty (Point Origin)

shiftAnchoredFragment :: HasHeader block
                      => Int
                      -> block
                      -> AF.AnchoredFragment block
                      -> AF.AnchoredFragment block
shiftAnchoredFragment n b af =
  case AF.unanchorFragment af of
    cf@(b0 CF.:< cf') | CF.length cf >= n
      -> AF.mkAnchoredFragment (blockPoint b0) (CF.addBlock b cf')
    _ -> AF.addBlock b af
