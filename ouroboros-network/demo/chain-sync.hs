{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns        #-}

module Main where

import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (maximumBy)
import Data.List.Infinite (Infinite ((:<)))
import Data.List.Infinite qualified as Inf
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)

import Control.Applicative (many)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception
import Control.Monad (when)
import Control.Monad.Class.MonadTime.SI (Time (..))
import Control.Tracer

import System.Directory
import System.Random

import Options.Applicative qualified as Opts

import Codec.Serialise qualified as CBOR

import Network.TypedProtocol.Codec

import Network.Mux qualified as Mx

import Ouroboros.Network.AnchoredFragment qualified as AF
import Ouroboros.Network.Block
import Ouroboros.Network.ControlMessage (continueForever)
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Mock.ConcreteBlock
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToClient (LocalConnectionId)
import Ouroboros.Network.NodeToNode
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Snocket
import Ouroboros.Network.Socket

import Ouroboros.Network.Driver
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Unversioned
import Ouroboros.Network.Protocol.Handshake.Version

import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Server qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync

import Ouroboros.Network.Protocol.BlockFetch.Codec qualified as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Server qualified as BlockFetch
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch

import Ouroboros.Network.BlockFetch
import Ouroboros.Network.BlockFetch.Client
import Ouroboros.Network.BlockFetch.ClientRegistry (FetchClientRegistry (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (ChainSelStarvation (..))
import Ouroboros.Network.DeltaQ (defaultGSV)


data Options = Options {
      oBlockFetch   :: Bool,
      -- ^ run `block-fetch`, if false only run `chain-sync`
      oServer       :: Bool,
      -- ^ if `True`, run a client
      oSeed         :: Maybe Int,
      -- ^ seed used by the server
      oMaxSlotNo    :: Maybe SlotNo,
      -- ^ max slot used by the `chain-sync` client.  After
      -- that slot the client will terminate.
      oSocketPaths' :: [String],
      -- ^ socket path
      oSlotLength   :: Int
    }
    deriving Show

oSocketPath :: Options -> FilePath
oSocketPath Options { oSocketPaths' } =
    case oSocketPaths' of
      []  -> defaultLocalSocketAddrPath
      a:_ -> a

oSocketPaths :: Options -> [FilePath]
oSocketPaths Options { oSocketPaths' } =
    case oSocketPaths' of
      [] -> [defaultLocalSocketAddrPath]
      _  -> oSocketPaths'

optionsParser :: Opts.Parser Options
optionsParser = Opts.subparser
              (  Opts.command "block-fetch" (Opts.info (go True  Opts.<**> Opts.helper) Opts.fullDesc)
              <> Opts.command "chain-sync"  (Opts.info (go False Opts.<**> Opts.helper) Opts.fullDesc))
  where
    go :: Bool -> Opts.Parser Options
    go blockFetch =
          Options blockFetch
      <$> Opts.switch
            ( Opts.long "server"
            <> Opts.help "run server side, by default client side is run" )
      <*> Opts.option (Just <$> Opts.auto)
            ( Opts.long "seed"
            <> Opts.help "seed for the server, if not given a random seed will be used"
            <> Opts.showDefault
            <> Opts.value Nothing
            <> Opts.metavar "SEED")
      <*> Opts.option (Just . fromIntegral @Int <$> Opts.auto)
            ( Opts.long "max-slot-no"
            <> Opts.help "slot number after which the block-fetch client should terminate"
            <> Opts.value Nothing
            <> Opts.metavar "SLOTNO" )

      -- parse `--socket-path` option multiple times and concatenate the
      -- results
      <*> (concat <$> many (Opts.option ((:[]) <$> Opts.str)
            ( Opts.long "socket-path"
            <> Opts.help ("a socket path, can be passed multiple times (default: '" ++ defaultLocalSocketAddrPath ++ "')")
            <> Opts.metavar "PATH"
            )))

      <*> Opts.option Opts.auto
            ( Opts.long "slot-length"
              <> Opts.help "slot length in ms"
              <> Opts.showDefault
              <> Opts.value 500
              <> Opts.metavar "TIME" )


oChainSyncClient :: Options -> Bool
oChainSyncClient opts  = not (oBlockFetch opts) && not (oServer opts)

oChainSyncServer :: Options -> Bool
oChainSyncServer opts  = not (oBlockFetch opts) &&      oServer opts

oBlockFetchClient :: Options -> Bool
oBlockFetchClient opts =      oBlockFetch opts  && not (oServer opts)

oBlockFetchServer :: Options -> Bool
oBlockFetchServer opts =      oBlockFetch opts  &&      oServer opts


main :: IO ()
main = do
    opts <- Opts.execParser parserInfo
    print opts
    case opts of
      Options {}
        -- chain-sync client
        | oChainSyncClient opts ->
          -- TODO: implement `oMaxSlotNo`
          clientChainSync (oSocketPaths opts) (oMaxSlotNo opts)

        -- chain-sync server
        | oChainSyncServer opts -> do
          traverse_ rmIfExists (oSocketPaths opts)
          void $ serverChainSync (oSocketPath opts) (oSlotLength opts * 1000) (oSeed opts)

        -- block-fetch client
        | oBlockFetchClient opts ->
          clientBlockFetch (oSocketPaths opts) (oMaxSlotNo opts)

        -- block-fetch server
        | oBlockFetchServer opts -> do
          rmIfExists (oSocketPath opts)
          void $ serverBlockFetch (oSocketPath opts) (oSlotLength opts * 1000) (oSeed opts)

        | otherwise -> error "impossible"
  where
    parserInfo = Opts.info (optionsParser Opts.<**> Opts.helper)
                           (Opts.fullDesc <> Opts.progDesc "Run chain-sync / block-fetch demo")


defaultLocalSocketAddrPath :: FilePath
defaultLocalSocketAddrPath =  "./demo-chain-sync.sock"

defaultLocalSocketAddr :: LocalAddress
defaultLocalSocketAddr = localAddressFromPath defaultLocalSocketAddrPath

rmIfExists :: FilePath -> IO ()
rmIfExists path = do
  b <- doesFileExist path
  when b (removeFile path)

-- TODO: provide sensible limits
-- https://github.com/intersectmbo/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = maxBound
    }


--
-- Chain sync demo
--

demoProtocol2
  :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b -- ^ chainSync
  -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
demoProtocol2 chainSync =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = chainSync
      }
    ]


clientChainSync :: [FilePath]
                -> Maybe SlotNo
                -> IO ()
clientChainSync sockPaths maxSlotNo = withIOManager $ \iocp ->
    forConcurrently_ (zip [0..] sockPaths) $ \(index, sockPath) -> do
      threadDelay (50000 * index)
      void $ connectToNode
        (localSnocket iocp)
        makeLocalBearer
        ConnectToArgs {
          ctaHandshakeCodec      = unversionedHandshakeCodec,
          ctaHandshakeTimeLimits = noTimeLimitsHandshake,
          ctaVersionDataCodec    = unversionedProtocolDataCodec,
          ctaConnectTracers      = nullNetworkConnectTracers,
          ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
        }
        mempty
        (simpleSingletonVersions
           UnversionedProtocol
           UnversionedProtocolData
           app)
        Nothing
        (localAddressFromPath sockPath)

  where
    app :: OuroborosApplicationWithMinimalCtx Mx.InitiatorMode addr LBS.ByteString IO () Void
    app = demoProtocol2 $
          InitiatorProtocolOnly $
          mkMiniProtocolCbFromPeer $ \_ctx ->
            ( contramap show stdoutTracer
            , codecChainSync
            , ChainSync.chainSyncClientPeer (chainSyncClient (continueForever Proxy) maxSlotNo)
            )


serverChainSync :: FilePath
                -> Int -- ^ slot length in μs
                -> Maybe Int -- ^ seed
                -> IO Void
serverChainSync sockAddr slotLength seed = withIOManager $ \iocp -> do
    prng <- case seed of
      Nothing -> initStdGen
      Just a  -> return (mkStdGen a)
    networkState <- newNetworkMutableState
    _ <- async $ cleanNetworkMutableState networkState
    withServerNode
      (localSnocket iocp)
      makeLocalBearer
      mempty
      nullNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      (localAddressFromPath sockAddr)
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      unversionedProtocolDataCodec
      (HandshakeCallbacks acceptableVersion queryVersion)
      (simpleSingletonVersions
        UnversionedProtocol
        UnversionedProtocolData
        (SomeResponderApplication (app prng)))
      nullErrorPolicies
      $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    app :: StdGen
        -> OuroborosApplicationWithMinimalCtx Mx.ResponderMode addr LBS.ByteString IO Void ()
    app prng = demoProtocol2 $
          ResponderProtocolOnly $
          mkMiniProtocolCbFromPeer $ \_ctx ->
            ( contramap show stdoutTracer
            , codecChainSync
            , ChainSync.chainSyncServerPeer (chainSyncServer prng slotLength)
            )


codecChainSync :: ( CBOR.Serialise block
                  , CBOR.Serialise point
                  , CBOR.Serialise tip
                  )
               => Codec (ChainSync.ChainSync block point tip)
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

demoProtocol3
  :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b -- ^ chainSync
  -> RunMiniProtocolWithMinimalCtx appType addr bytes m a b -- ^ blockFetch
  -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
demoProtocol3 chainSync blockFetch =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = chainSync
      }
    , MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 3,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = blockFetch
      }
    ]

-- We run `chain-sync` and `block-fetch`, without `keep-alive`, so we need to
-- register peers in `dqRegistry`.
bracketDqRegistry :: FetchClientRegistry (ConnectionId LocalAddress) header block IO
                  -> ConnectionId LocalAddress
                  -> IO a -> IO a
bracketDqRegistry FetchClientRegistry { fcrDqRegistry = dqRegistry } peer k =
    bracket (atomically (modifyTVar dqRegistry (Map.insert peer defaultGSV)))
            (\_ -> atomically (modifyTVar dqRegistry (Map.delete peer)))
            (\_ -> k)

clientBlockFetch :: [FilePath]
                 -> Maybe SlotNo
                 -> IO ()
clientBlockFetch sockAddrs maxSlotNo = withIOManager $ \iocp -> do
    registry   <- newFetchClientRegistry
    blockHeap  <- mkTestFetchedBlockHeap []

    candidateChainsVar <- newTVarIO Map.empty
    currentChainVar    <- newTVarIO genesisAnchoredFragment

    let continueUntilMaxSlot :: ControlMessageSTM IO
        continueUntilMaxSlot =
          case maxSlotNo of
            Nothing -> return Continue
            Just maxSlot -> do
              point <- getLastFetchedPoint blockHeap
              return $ case point of
                Just BlockPoint { atSlot = slot }
                  | slot <= maxSlot -> Continue
                  | otherwise       -> Terminate
                _                   -> Continue


        app :: OuroborosApplicationWithMinimalCtx
                 Mx.InitiatorMode LocalAddress LBS.ByteString IO () Void
        app = demoProtocol3 chainSync blockFetch

        chainSync :: RunMiniProtocolWithMinimalCtx
                       Mx.InitiatorMode LocalAddress LBS.ByteString IO () Void
        chainSync =
          InitiatorProtocolOnly $
            MiniProtocolCb $ \MinimalInitiatorContext { micConnectionId = connId } channel ->
              let register = atomically $ do
                             chainvar <- newTVar genesisAnchoredFragment
                             modifyTVar candidateChainsVar
                                        (Map.insert connId chainvar)
                             return chainvar
                  unregister _ = atomically $
                                 modifyTVar candidateChainsVar
                                            (Map.delete connId)
              in bracketSyncWithFetchClient registry connId $
                 bracket register unregister $ \chainVar ->
                 runPeer
                   nullTracer -- (contramap (show . TraceLabelPeer ("chain-sync", getFilePath $ remoteAddress connId)) stdoutTracer)
                   codecChainSync
                   channel
                   (ChainSync.chainSyncClientPeer
                     (chainSyncClient' continueUntilMaxSlot  maxSlotNo syncTracer currentChainVar chainVar))

        blockFetch :: RunMiniProtocolWithMinimalCtx
                        Mx.InitiatorMode LocalAddress LBS.ByteString IO () Void
        blockFetch =
          InitiatorProtocolOnly $
            MiniProtocolCb $ \MinimalInitiatorContext { micConnectionId = connId } channel ->
              bracketDqRegistry registry connId $
              bracketFetchClient registry (maxBound :: NodeToNodeVersion) connId $ \clientCtx -> do
                threadDelay 1000000
                runPipelinedPeer
                  nullTracer -- (contramap (show . TraceLabelPeer ("block-fetch", getFilePath $ remoteAddress connId)) stdoutTracer)
                  codecBlockFetch
                  channel
                  (blockFetchClient (maxBound :: NodeToNodeVersion) continueUntilMaxSlot
                                    nullTracer clientCtx)

        blockFetchPolicy :: BlockFetchConsensusInterface
                             LocalConnectionId BlockHeader Block IO
        blockFetchPolicy =
            BlockFetchConsensusInterface {
              readCandidateChains    = readTVar candidateChainsVar
                                       >>= traverse readTVar,
              readCurrentChain       = readTVar currentChainVar,
              readFetchMode          = return $ PraosFetchMode FetchModeBulkSync,
              readFetchedBlocks      = (\h p -> castPoint p `Set.member` h) <$>
                                         getTestFetchedBlocks blockHeap,
              readFetchedMaxSlotNo   = maybe NoMaxSlotNo (maxSlotNoFromWithOrigin . pointSlot) <$>
                                         getLastFetchedPoint blockHeap,
              mkAddFetchedBlock      = do
                  pure $ \p b ->
                    addTestFetchedBlock blockHeap (castPoint p) (blockHeader b),

              plausibleCandidateChain,
              compareCandidateChains,

              blockFetchSize         = \_ -> 1000,
              blockMatchesHeader     = \_ _ -> True,

              headerForgeUTCTime,
              readChainSelStarvation = pure (ChainSelStarvationEndedAt (Time 0)),

              demoteChainSyncJumpingDynamo = \_ -> pure ()
            }
          where
            plausibleCandidateChain cur candidate =
                AF.headBlockNo candidate > AF.headBlockNo cur

            headerForgeUTCTime (FromConsensus hdr) =
                pure $
                convertSlotToTimeForTestsAssumingNoHardFork (headerSlot hdr)

        compareCandidateChains c1 c2 =
          AF.headBlockNo c1 `compare` AF.headBlockNo c2

        chainSelection fingerprint = do
          (fingerprint', currentChain') <- atomically $ do
            candidates <- readTVar candidateChainsVar
                      >>= traverse readTVar
            let fingerprint' = Map.map AF.headPoint candidates
            check (fingerprint /= fingerprint')
            -- currentChain <- readTVar currentChainVar
            fetched      <- getTestFetchedBlocks blockHeap
            let currentChain' =
                 -- So this does chain selection by taking the longest
                 -- downloaded candidate chain
                 -- FIXME: there's something wrong here, we always get chains of
                 -- length 1.
                  maximumBy compareCandidateChains $
                  [ candidateChainFetched
                  | candidateChain <- Map.elems candidates
                  , let candidateChainFetched =
                          AF.takeWhileOldest
                            (\b -> blockPoint b `Set.member` fetched)
                            candidateChain
                  ]
            writeTVar currentChainVar currentChain'
            return (fingerprint', currentChain')
          traceWith chainTracer (AF.lastPoint currentChain',
                                 AF.headPoint currentChain')
          chainSelection fingerprint'

    peerAsyncs <- sequence
                    [ async . void $
                        connectToNode
                          (localSnocket iocp)
                          makeLocalBearer
                          ConnectToArgs {
                            ctaHandshakeCodec      = unversionedHandshakeCodec,
                            ctaHandshakeTimeLimits = noTimeLimitsHandshake,
                            ctaVersionDataCodec    = unversionedProtocolDataCodec,
                            ctaConnectTracers      = nullNetworkConnectTracers,
                            ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
                          }
                          mempty
                          (simpleSingletonVersions
                            UnversionedProtocol
                            UnversionedProtocolData
                            app)
                          Nothing
                          (localAddressFromPath sockAddr)
                    | sockAddr <- sockAddrs ]

    fetchAsync <- async $
                    blockFetchLogic
                      (contramap show stdoutTracer) -- decisionTracer
                      (contramap show stdoutTracer) -- state tracer
                      blockFetchPolicy
                      registry
                      (BlockFetchConfiguration {
                        bfcMaxConcurrencyBulkSync = 1,
                        bfcMaxConcurrencyDeadline = 2,
                        bfcMaxRequestsInflight    = 10,
                        bfcDecisionLoopIntervalGenesis = 0.04,
                        bfcDecisionLoopIntervalPraos = 0.01,
                        bfcSalt                   = 0,
                        bfcGenesisBFConfig        = GenesisBlockFetchConfiguration
                          { gbfcGracePeriod = 10 -- seconds
                          }
                        })
                 >> return ()

    chainAsync <- async (chainSelection Map.empty)

    _ <- waitAnyCancel (fetchAsync : chainAsync : peerAsyncs)
    return ()
  where
    -- chainSyncMsgTracer  = contramap (show) stdoutTracer
    -- blockFetchMsgTracer = contramap (show) stdoutTracer

    -- decisionTracer      = contramap show stdoutTracer
    -- clientStateTracer   = contramap show stdoutTracer

    syncTracer :: Tracer IO (Point BlockHeader, Point BlockHeader)
    syncTracer  = contramap (\x -> "sync client: " ++ show x) stdoutTracer

    chainTracer :: Tracer IO (Point BlockHeader, Point BlockHeader)
    chainTracer = contramap (\x -> "cur chain  : " ++ show x) stdoutTracer


serverBlockFetch :: FilePath
                 -> Int -- ^ slot length in μs
                 -> Maybe Int -- ^ seed
                 -> IO Void
serverBlockFetch sockAddr slotLength seed = withIOManager $ \iocp -> do
    prng <- case seed of
      Nothing -> initStdGen
      Just a  -> return (mkStdGen a)
    networkState <- newNetworkMutableState
    _ <- async $ cleanNetworkMutableState networkState
    withServerNode
      (localSnocket iocp)
      makeLocalBearer
      mempty
      nullNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      (localAddressFromPath sockAddr)
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      unversionedProtocolDataCodec
      (HandshakeCallbacks acceptableVersion queryVersion)
      (simpleSingletonVersions
        UnversionedProtocol
        UnversionedProtocolData
        (SomeResponderApplication (app prng)))
      nullErrorPolicies
      $ \_ serverAsync ->
        wait serverAsync   -- block until async exception
  where
    app :: StdGen
        -> OuroborosApplicationWithMinimalCtx
             Mx.ResponderMode LocalAddress LBS.ByteString IO Void ()
    app prng = demoProtocol3 (chainSync prng) (blockFetch prng)

    chainSync :: StdGen
              -> RunMiniProtocolWithMinimalCtx
                   Mx.ResponderMode LocalAddress LBS.ByteString IO Void ()
    chainSync prng =
      ResponderProtocolOnly $
      mkMiniProtocolCbFromPeer $ \_ctx ->
        ( contramap show stdoutTracer
        , codecChainSync
        , ChainSync.chainSyncServerPeer (chainSyncServer prng slotLength)
        )

    blockFetch :: StdGen
               -> RunMiniProtocolWithMinimalCtx
                    Mx.ResponderMode LocalAddress LBS.ByteString IO Void ()
    blockFetch prng =
      ResponderProtocolOnly $
      mkMiniProtocolCbFromPeer $ \_ctx ->
        ( contramap show stdoutTracer
        , codecBlockFetch
        , BlockFetch.blockFetchServerPeer (blockFetchServer prng)
        )

codecBlockFetch :: Codec (BlockFetch.BlockFetch Block (Point Block))
                         CBOR.DeserialiseFailure
                         IO LBS.ByteString
codecBlockFetch =
    BlockFetch.codecBlockFetch
      CBOR.encode CBOR.decode
      CBOR.encode CBOR.decode


--
-- Chain sync and block fetch protocol handlers
--

chainSyncClient :: ControlMessageSTM IO
                -> Maybe SlotNo
                -> ChainSync.ChainSyncClient
                     BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
chainSyncClient controlMessageSTM maxSlotNo =
    ChainSync.ChainSyncClient $ do
      curvar   <- newTVarIO genesisAnchoredFragment
      chainvar <- newTVarIO genesisAnchoredFragment
      case chainSyncClient' controlMessageSTM maxSlotNo  nullTracer curvar chainvar of
        ChainSync.ChainSyncClient k -> k

chainSyncClient' :: ControlMessageSTM IO
                 -> Maybe SlotNo
                 -> Tracer IO (Point BlockHeader, Point BlockHeader)
                 -> StrictTVar IO (AF.AnchoredFragment BlockHeader)
                 -> StrictTVar IO (AF.AnchoredFragment BlockHeader)
                 -> ChainSync.ChainSyncClient
                      BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
chainSyncClient' controlMessageSTM _maxSlotNo syncTracer _currentChainVar candidateChainVar =
    ChainSync.ChainSyncClient (return requestNext)
  where
    requestNext :: ChainSync.ClientStIdle
                     BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    requestNext =
      ChainSync.SendMsgRequestNext
        (pure ())   -- on MsgAwaitReply; could trace
        handleNext

    terminate :: ChainSync.ClientStIdle
                     BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    terminate = ChainSync.SendMsgDone ()

    handleNext :: ChainSync.ClientStNext
                    BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    handleNext =
      ChainSync.ClientStNext {
        ChainSync.recvMsgRollForward  = \header _pHead ->
          ChainSync.ChainSyncClient $ do
            addBlock header
            cm <- atomically controlMessageSTM
            return $ case cm of
              Terminate -> terminate
              _         -> requestNext

      , ChainSync.recvMsgRollBackward = \pIntersect _pHead ->
          ChainSync.ChainSyncClient $ do
            rollback pIntersect
            cm <- atomically controlMessageSTM
            return $ case cm of
              Terminate -> terminate
              _         -> requestNext
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

chainSyncServer :: StdGen
                -> Int -- ^ slot length in μs
                -> ChainSync.ChainSyncServer
                     BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
chainSyncServer prng slotLength =
    let blocks = chainGenerator prng in
    ChainSync.ChainSyncServer (return (idleState blocks))
  where
    idleState :: Infinite Block
              -> ChainSync.ServerStIdle
                   BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    idleState blocks =
      ChainSync.ServerStIdle {
        ChainSync.recvMsgRequestNext   = do when (slotLength > 0) (threadDelay slotLength)
                                            return (Left (nextState blocks)),
        ChainSync.recvMsgFindIntersect = \_ -> return (intersectState blocks),
        ChainSync.recvMsgDoneClient    = return ()
      }

    nextState :: Infinite Block
              -> ChainSync.ServerStNext
                   BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    nextState (block :< blocks) =
      ChainSync.SendMsgRollForward
        (blockHeader block)
         -- pretend chain head is next one:
        (blockPoint (blockHeader (Inf.head blocks)))
        (ChainSync.ChainSyncServer (return (idleState blocks)))

    intersectState :: Infinite Block
                   -> ChainSync.ServerStIntersect
                        BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    intersectState blocks =
      ChainSync.SendMsgIntersectNotFound
         -- pretend chain head is next one:
        (blockPoint (blockHeader (Inf.head blocks)))
        (ChainSync.ChainSyncServer (return (idleState blocks)))


blockFetchServer :: StdGen
                 -> BlockFetch.BlockFetchServer Block (Point Block) IO ()
blockFetchServer prng =
    let blocks = chainGenerator prng in
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

selectBlockRange :: BlockFetch.ChainRange (Point Block)
                 -> Infinite Block
                 -> Maybe ([Block], Infinite Block)
selectBlockRange (BlockFetch.ChainRange lower upper) blocks0 = do
    (_,  blocks1)     <- splitBeforePoint lower blocks0
    (bs, b :< remaining) <- splitBeforePoint upper blocks1
    return (bs ++ [b], remaining)

splitBeforePoint :: Point Block -> Infinite Block -> Maybe ([Block], Infinite Block)
splitBeforePoint pt = go []
  where
    go acc (b :< bs) =
      case compare (At (blockSlot b)) (pointSlot pt) of
        LT -> go (b:acc) bs
        EQ | pt == castPoint (blockPoint b)
           -> Just (reverse acc, b :< bs)
        _  -> Nothing


--
-- Block generator
--

prop_chainGenerator :: RandomGen g => g -> Bool
prop_chainGenerator =
    Chain.valid
  . Chain.fromOldestFirst
  . Inf.take 1000
  . chainGenerator

chainGenerator :: RandomGen g => g -> Infinite Block
chainGenerator g =
    genBlockChain g Nothing

genBlockChain :: RandomGen g => g -> Maybe BlockHeader -> Infinite Block
genBlockChain !g prevHeader =
    block :< genBlockChain g'' (Just (blockHeader block))
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
    BlockBody . BSC.take len . BSC.drop offset . BSC.pack $ bodyData
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
       getTestFetchedBlocks :: STM m (Set (Point block)),
       getLastFetchedPoint  :: GetLastFetchedPoint m block,
       addTestFetchedBlock  :: Point block -> block -> m ()
     }

type GetLastFetchedPoint m block = STM m (Maybe (Point block))

-- | Make a 'TestFetchedBlockHeap' using a simple in-memory 'Map', stored in an
-- 'StrictTVar'.
--
-- This is suitable for examples and tests.
--
mkTestFetchedBlockHeap :: [Point BlockHeader]
                       -> IO (TestFetchedBlockHeap IO BlockHeader)
mkTestFetchedBlockHeap points = do
    v <- newTVarIO (Set.fromList points)
    h <- newTVarIO Nothing
    return TestFetchedBlockHeap {
      getTestFetchedBlocks = readTVar v,
      getLastFetchedPoint  = readTVar h,
      addTestFetchedBlock  = \p _b -> atomically $ modifyTVar v (Set.insert p)
                                                >> modifyTVar h (max (Just p))
    }

--
-- Utils
--

genesisAnchoredFragment :: AF.AnchoredFragment BlockHeader
genesisAnchoredFragment = AF.Empty AF.AnchorGenesis

shiftAnchoredFragment :: HasHeader block
                      => Int
                      -> block
                      -> AF.AnchoredFragment block
                      -> AF.AnchoredFragment block
shiftAnchoredFragment n b af =
  AF.anchorNewest (fromIntegral (AF.length af - n)) af AF.:> b
