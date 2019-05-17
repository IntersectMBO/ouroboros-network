{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main where

import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Maybe

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Control.Exception
import Control.Tracer

import System.IO
import System.Environment
import System.Exit
import System.Random
import System.Random.SplitMix

import qualified Codec.Serialise as CBOR

import qualified Network.Socket as Socket

--import Ouroboros.Network.NodeToNode
import Ouroboros.Network.Block
import qualified Ouroboros.Network.Chain as Chain
import qualified Ouroboros.Network.ChainFragment as CF
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Testing.ConcreteBlock
import Ouroboros.Network.Socket
import Ouroboros.Network.Mux.Interface
import Ouroboros.Network.Codec (DeserialiseFailure)

import Network.TypedProtocol.Channel
import Network.TypedProtocol.Driver
import Network.TypedProtocol.Codec (AnyMessage(..))
import Network.TypedProtocol.PingPong.Type   as PingPong
import Network.TypedProtocol.PingPong.Client as PingPong
import Network.TypedProtocol.PingPong.Server as PingPong
import Ouroboros.Network.Protocol.PingPong.Codec

--import Ouroboros.Network.Protocol.ChainSync.Type   as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Codec  as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Client as ChainSync

import Ouroboros.Network.Protocol.BlockFetch.Type   as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Codec  as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Server as BlockFetch
--import Ouroboros.Network.Protocol.BlockFetch.Client as BlockFetch

import Ouroboros.Network.BlockFetch
import Ouroboros.Network.BlockFetch.Client


main :: IO ()
main = do
    args <- getArgs
    case args of
      "pingpong":"client":[] -> clientPingPong
      "pingpong":"server":[] -> serverPingPong

      "chainsync":"client":sockAddrs   -> clientChainSync sockAddrs
      "chainsync":"server":sockAddr:[] -> serverChainSync sockAddr

      "blockfetch":"client":sockAddrs   -> clientBlockFetch sockAddrs
      "blockfetch":"server":sockAddr:[] -> serverBlockFetch sockAddr

      _          -> usage

usage :: IO ()
usage = do
    hPutStrLn stderr "usage: demo-chain-sync {client|server} [pingpong]"
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

defaultLocalSocketAddrInfo :: Socket.AddrInfo
defaultLocalSocketAddrInfo = 
    mkLocalSocketAddrInfo "./demo-chain-sync.sock"


--
-- Ping pong demo
--

data DemoProtocol1 = PingPong
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocol1 where
  fromProtocolEnum PingPong = 2
  toProtocolEnum 2 = Just PingPong
  toProtocolEnum _ = Nothing

pingPingMsgTracer :: Tracer IO (TraceSendRecv PingPong)
pingPingMsgTracer =
    contramap (showTraceSendRecv show) stdoutTracer

showTraceSendRecv :: (AnyMessage ps -> String) 
                  -> TraceSendRecv ps -> String
showTraceSendRecv showMsg (TraceSendMsg msg) = "send: " ++ showMsg msg
showTraceSendRecv showMsg (TraceRecvMsg msg) = "recv: " ++ showMsg msg

clientPingPong :: IO ()
clientPingPong =
    withConnection
      (clientApplication muxApplication)
      defaultLocalSocketAddrInfo $ \runConn ->
        runConn
  where
    muxApplication :: MuxApplication ClientApp DemoProtocol1 IO
    muxApplication =
      simpleMuxClientApplication protocols

    protocols :: DemoProtocol1 -> MuxPeer DeserialiseFailure IO ()
    protocols PingPong =
      MuxPeer
        pingPingMsgTracer
        codecPingPong
        (pingPongClientPeer (pingPongClientCount 5))

pingPongClientCount :: Applicative m => Int -> PingPongClient m ()
pingPongClientCount 0 = PingPong.SendMsgDone ()
pingPongClientCount n = SendMsgPing (pure (pingPongClientCount (n-1)))

serverPingPong :: IO ()
serverPingPong =
    withServerNode
      defaultLocalSocketAddrInfo
      muxApplication $ \_node serverAsync ->
        wait serverAsync   -- block until async exception
  where
    muxApplication :: MuxApplication ServerApp DemoProtocol1 IO
    muxApplication =
      simpleMuxServerApplication protocols

    protocols :: DemoProtocol1 -> MuxPeer DeserialiseFailure IO ()
    protocols PingPong =
      MuxPeer
        pingPingMsgTracer
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
-- Chain sync demo
--

data DemoProtocol2 = ChainSync2
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocol2 where
  fromProtocolEnum ChainSync2  = 2

  toProtocolEnum 2 = Just ChainSync2
  toProtocolEnum _ = Nothing


clientChainSync :: [FilePath] -> IO ()
clientChainSync sockAddrs =
    forConcurrently_ sockAddrs $ \sockAddr ->
      withConnection
        (clientApplication muxApplication)
        (mkLocalSocketAddrInfo sockAddr) $ \runConn ->
          runConn
  where
    muxApplication :: MuxApplication ClientApp DemoProtocol2 IO
    muxApplication =
      simpleMuxClientApplication protocols

    protocols :: DemoProtocol2 -> MuxPeer DeserialiseFailure IO ()
    protocols ChainSync2 =
      MuxPeer
        (contramap (showTraceSendRecv show) stdoutTracer)
        (codecChainSync CBOR.encode CBOR.encode CBOR.decode CBOR.decode)
        (chainSyncClientPeer chainSyncClient)


serverChainSync :: FilePath -> IO ()
serverChainSync sockAddr = do
    prng <- newSMGen

    let muxApplication :: MuxApplication ServerApp DemoProtocol2 IO
        muxApplication =
          simpleMuxServerApplication protocols

        protocols :: DemoProtocol2 -> MuxPeer DeserialiseFailure IO ()
        protocols ChainSync2 =
          MuxPeer
            (contramap (showTraceSendRecv show) stdoutTracer)
            (codecChainSync CBOR.encode CBOR.encode CBOR.decode CBOR.decode)
            (chainSyncServerPeer (chainSyncServer prng))

    withServerNode
      (mkLocalSocketAddrInfo sockAddr)
      muxApplication $ \_node serverAsync ->
        wait serverAsync   -- block until async exception


--
-- Block fetch demo
--

data DemoProtocol3 = ChainSync3 | BlockFetch3
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocol3 where
  fromProtocolEnum ChainSync3  = 2
  fromProtocolEnum BlockFetch3 = 3

  toProtocolEnum 2 = Just ChainSync3
  toProtocolEnum 3 = Just BlockFetch3
  toProtocolEnum _ = Nothing

showMessageBlockFetch :: (StandardHash header, Show body)
                      => AnyMessage (BlockFetch header body) -> String
showMessageBlockFetch (AnyMessage msg) = show msg


clientBlockFetch :: [FilePath] -> IO ()
clientBlockFetch sockAddrs = do
    registry   <- newFetchClientRegistry
    blockHeap  <- mkTestFetchedBlockHeap []

    candidateChainsVar <- newTVarIO Map.empty
    currentChainVar    <- newTVarIO genesisChainFragment

    let muxApplication :: FilePath
                       -> MuxApplication ClientApp DemoProtocol3 IO
        muxApplication peerid =
          MuxClientApplication (protocols peerid)

        protocols :: FilePath
                  -> DemoProtocol3
                  -> Channel IO LBS.ByteString
                  -> IO ()
        protocols peerid ChainSync3 channel =
          bracket register unregister $ \chainVar ->
          runPeer
            (contramap (showTraceSendRecv show) stdoutTracer)
            (codecChainSync CBOR.encode CBOR.encode CBOR.decode CBOR.decode)
            channel
            (chainSyncClientPeer (chainSyncClient' chainVar))
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
          bracketFetchClient registry peerid $ \stateVars ->
            runPipelinedPeer 10
              (contramap (showTraceSendRecv showMessageBlockFetch) stdoutTracer)
              (codecBlockFetch CBOR.encode CBOR.encode CBOR.decode CBOR.decode)
              channel
              (blockFetchClient
                 nullTracer --TODO
                 fetchClientPolicy
                 stateVars)

        fetchClientPolicy = FetchClientPolicy {
          blockFetchSize     = \_   -> 2000,
          blockMatchesHeader = \_ _ -> True,
          addFetchedBlock    = addTestFetchedBlock blockHeap
        }

        blockFetchPolicy :: BlockFetchConsensusInterface FilePath
                              BlockHeader Block IO
        blockFetchPolicy =
            BlockFetchConsensusInterface {
              readCandidateChains    = readTVar candidateChainsVar
                                       >>= traverse readTVar,
              readCurrentChain       = readTVar currentChainVar,
              readFetchMode          = return FetchModeBulkSync,
              readFetchedBlocks      = flip Set.member <$>
                                         getTestFetchedBlocks blockHeap,
              addFetchedBlock        = addTestFetchedBlock blockHeap,

              plausibleCandidateChain,
              compareCandidateChains,

              blockFetchSize         = \_ -> 2000,
              blockMatchesHeader     = \_ _ -> True
            }
          where
            plausibleCandidateChain cur candidate =
                AF.headBlockNo candidate > AF.headBlockNo cur

            compareCandidateChains c1 c2 =
              AF.headBlockNo c1 `compare` AF.headBlockNo c2


    peerAsyncs <- sequence
                    [ async $
                        withConnection
                          (clientApplication (muxApplication sockAddr))
                          (mkLocalSocketAddrInfo sockAddr) id
                    | sockAddr <- sockAddrs ]

    fetchAsync <- async $
                    blockFetchLogic
                      nullTracer -- decisionTracer
                      blockFetchPolicy
                      registry
                 >> return ()

    _ <- waitAnyCancel (fetchAsync : peerAsyncs)
    return ()


serverBlockFetch :: FilePath -> IO ()
serverBlockFetch sockAddr = do
    prng <- newSMGen

    let muxApplication :: MuxApplication ServerApp DemoProtocol3 IO
        muxApplication =
          simpleMuxServerApplication protocols

        protocols :: DemoProtocol3 -> MuxPeer DeserialiseFailure IO ()
        protocols ChainSync3 =
          MuxPeer
            (contramap (showTraceSendRecv show) stdoutTracer)
            (codecChainSync CBOR.encode CBOR.encode CBOR.decode CBOR.decode)
            (chainSyncServerPeer (chainSyncServer prng))

        protocols BlockFetch3 =
          MuxPeer
            (contramap (showTraceSendRecv showMessageBlockFetch) stdoutTracer)
            (codecBlockFetch CBOR.encode CBOR.encode CBOR.decode CBOR.decode)
            (blockFetchServerPeer (blockFetchServer prng))

    withServerNode
      (mkLocalSocketAddrInfo sockAddr)
      muxApplication $ \_node serverAsync ->
        wait serverAsync   -- block until async exception


--
-- Chain sync and block fetch protocol handlers
--

chainSyncClient :: ChainSyncClient BlockHeader (Point BlockHeader) IO ()
chainSyncClient =
    ChainSyncClient $ do
      chainvar <- newTVarIO genesisChainFragment
      let ChainSyncClient k = chainSyncClient' chainvar
      k

chainSyncClient' :: TVar (AF.AnchoredFragment BlockHeader)
                 -> ChainSyncClient BlockHeader (Point BlockHeader) IO ()
chainSyncClient' chainvar =
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
            return requestNext

      , recvMsgRollBackward = \pIntersect _pHead ->
          ChainSyncClient $ do
            rollback pIntersect
            return requestNext
      }

    addBlock :: BlockHeader -> IO ()
    addBlock b = atomically $ do
        chain <- readTVar chainvar
        let !chain' = shiftAnchoredFragment 100 b chain
        writeTVar chainvar chain'

    rollback :: Point BlockHeader -> IO ()
    rollback p = atomically $ do
        chain <- readTVar chainvar
        -- we do not handle rollback failure in this demo
        let (Just !chain') = AF.rollback p chain
        writeTVar chainvar chain'

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
        recvMsgRequestNext   = do threadDelay 1000000
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
                 -> BlockFetchServer BlockHeader Block IO ()
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
          SendMsgStartBatch (return (sendingState batch blocks'))

    sendingState []        blocks =
      SendMsgBatchDone (return (idleState blocks))
    sendingState (b:batch) blocks =
      SendMsgBlock b (return (sendingState batch blocks))

selectBlockRange :: ChainRange BlockHeader
                 -> [Block]
                 -> Maybe ([Block], [Block])
selectBlockRange (ChainRange lower upper) blocks0 = do
    (_,  blocks1)     <- splitBeforePoint lower blocks0
    (bs, b:remaining) <- splitBeforePoint upper blocks1
    return (bs ++ [b], remaining)

splitBeforePoint :: Point BlockHeader -> [Block] -> Maybe ([Block], [Block])
splitBeforePoint pt = go []
  where
    go acc (b:bs) =
      case compare (blockSlot b) (pointSlot pt) of
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
    BlockBody . BSC.take len . BSC.drop offset $ bodyData
  where
    (offset, g') = randomR (0, bodyDataCycle-1)    g
    (len   , _ ) = randomR (1, bodyDataCycle*10-1) g'

bodyData :: ByteString
bodyData = BSC.concat
         . replicate 11
         . BSC.pack
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
mkTestFetchedBlockHeap :: [Point Block]
                       -> IO (TestFetchedBlockHeap IO Block)
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
genesisChainFragment = AF.Empty (Point 0 GenesisHash)

shiftAnchoredFragment :: HasHeader block
                      => Int
                     -> block
                     -> AF.AnchoredFragment block
                     -> AF.AnchoredFragment block
shiftAnchoredFragment n b c0 =
    (\c -> AF.mkAnchoredFragment (fromMaybe p0 (CF.lastPoint c)) c)
  . CF.takeNewest n
  . AF.unanchorFragment
  . AF.addBlock b
  $ c0
  where
    p0 = AF.lastPoint c0

