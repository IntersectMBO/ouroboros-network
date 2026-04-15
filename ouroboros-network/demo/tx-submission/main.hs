{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Codec.Serialise (Serialise (..))
import Codec.Serialise qualified as CBOR
import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception
import Control.Monad
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..))
import Control.Tracer qualified as Tracer
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS.Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity (Identity (..))
import Data.Hashable qualified as Hashable
import Data.List.NonEmpty qualified as NonEmpty
import Data.IP (IP)
import Data.Word (Word8, Word32)
import Data.Vector qualified as Vec
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Options.Applicative
import Statistics.Quantile qualified as Stat
import System.IO (stderr, hPutStrLn)
import System.Random.SplitMix qualified as SM

import Network.Mux qualified as Mx
import Network.Mux.Bearer qualified as Mx
import Network.Socket (PortNumber)
import Network.Socket qualified as Socket
import Network.TypedProtocol.Codec (AnyMessage (..), AnnotatedCodec)

import Ouroboros.Network.Protocol.Codec.Utils
import Ouroboros.Network.Protocol.TxSubmission2.Client qualified as Tx.Outbound
import Ouroboros.Network.Protocol.TxSubmission2.Codec qualified as Tx
import Ouroboros.Network.Protocol.TxSubmission2.Server qualified as Tx.Inbound
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as Tx

import Ouroboros.Network.Driver.Limits qualified as Driver
import Ouroboros.Network.SizeInBytes
import Ouroboros.Network.Socket ()
import Ouroboros.Network.TxSubmission.Inbound.V1 qualified as V1
import Ouroboros.Network.TxSubmission.Inbound.V2 (TxSubmissionLogicVersion (..))
import Ouroboros.Network.TxSubmission.Inbound.V2 qualified as V2
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool
import Ouroboros.Network.Util.ShowProxy
-- import Ouroboros.Network.Util

import Demo.TxSubmission.Outbound

import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Gen qualified as QC
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Random (newQCGen, QCGen (..))
import GHC.IO.Exception (IOException(..), IOErrorType (..))

main :: IO ()
main =
  execParser (info (optionParser <**> helper) fullDesc)
    >>= \case
      InboundOptions addr maxNumTxIdsToRequest maxUnacknowledgedTxIds version txDelay -> do
        let txDecisionPolicy = V2.defaultTxDecisionPolicy
              { V2.maxNumTxIdsToRequest,
                V2.maxUnacknowledgedTxIds
              }
        runTxInbound addr txDecisionPolicy version (microsecondsAsIntToDiffTime txDelay)
      OutboundOptions addr bindAddr maxUnacknowledgedTxIds version filePath num -> do
        let txDecisionPolicy = V2.defaultTxDecisionPolicy
              { V2.maxUnacknowledgedTxIds
              }
        lock <- newMVar ()
        let stderrTracer = Tracer $ \msg -> withMVar lock $ \_ -> hPutStrLn stderr msg
            addrs :: [Addr]
            addrs = fmap (\a -> bindAddr { port = port bindAddr + fromIntegral a })
                         [0..(num - 1)]
        runConcurrently
          $ foldMap
            (\bindAddr' -> Concurrently $ runTxOutbound stderrTracer addr bindAddr' txDecisionPolicy version filePath)
            addrs
      GenerateTxs num filePath ->
        runTxsGenerator num filePath
      AnalyseTxs filePath ->
        runTxsAnalyser filePath

data Options =
    InboundOptions
      Addr -- ^ address of the inbound side
      Tx.NumTxIdsToReq -- ^ maximum number txids to request
      Tx.NumTxIdsToReq -- ^ unacked txids
      TxSubmissionLogicVersion
      Int -- ^ tx validation time in microseconds

  | OutboundOptions
      Addr -- ^ address of the inbound side
      Addr -- ^ ip address of the outbound side, for `n > 0` port number will be incremented
      Tx.NumTxIdsToReq -- ^ unacked txids
      TxSubmissionLogicVersion
      FilePath -- ^ file path of tx cache
      Int -- ^ number of outbound clients to fork

  | GenerateTxs
      Int -- ^ number of txs
      FilePath -- ^ file path

  | AnalyseTxs
      FilePath
  deriving Show

data Addr = Addr { addr :: IP, port :: PortNumber }
  deriving Show


optionParser :: Parser Options
optionParser =
        hsubparser
          ( command "inbound"
          $ info inboundParser
          $    fullDesc
            <> progDesc "run tx-submission inbound server"
          )
    <|> hsubparser
          (command "outbound"
          $ info outboundParser
          $    fullDesc
            <> progDesc "run tx-submission outbound client")
    <|> hsubparser
         (command "generate"
         $ info generateParser
         $    fullDesc
           <> progDesc "generate tx")
    <|> hsubparser
         (command "analyse-txs"
         $ info analyseParser
         $    fullDesc
           <> progDesc "analyze tx file")
  where
    defaultPort :: PortNumber
    defaultPort = 4000

    defaultAddr :: IP
    defaultAddr = read "127.0.0.1"

    inboundParser,
      outboundParser,
      generateParser,
      analyseParser :: Parser Options

    inboundParser =
      (\addr port maxTxIdsToReq maxUnacked version txDelay ->
        InboundOptions Addr { addr, port } maxTxIdsToReq maxUnacked version txDelay
      ) <$> option auto
              (  long "addr"
              <> metavar "ADDR"
              <> help "accept address"
              <> value defaultAddr
              <> showDefault
              )
        <*> option auto
              (  long "port"
              <> metavar "PORT"
              <> help "accept port number"
              <> value defaultPort
              <> showDefault
              )
        <*> option (Tx.NumTxIdsToReq <$> auto)
            (    long "txids-to-request"
              <> help "maximum number of txids to request"
              <> value (V2.maxNumTxIdsToRequest V2.defaultTxDecisionPolicy)
              <> showDefaultWith (show . Tx.getNumTxIdsToReq)
            )
        <*> option (Tx.NumTxIdsToReq <$> auto)
            (    long "unacked-txids"
              <> help "size of unacknowledged txid buffer"
              <> value (V2.maxUnacknowledgedTxIds V2.defaultTxDecisionPolicy)
              <> showDefaultWith (show . Tx.getNumTxIdsToReq)
            )
        <*> flag TxSubmissionLogicV2
                 TxSubmissionLogicV1
            (    long "v1"
              <> help "use tx-submission-v1 (default is v2)"
            )
        <*> option auto
            (    long "tx-delay"
              <> help "tx validation delay in microseconds"
              <> value 10
              <> showDefault
            )

    outboundParser =
      (\addr port addr' port' maxUnacked version filePath num ->
        OutboundOptions Addr { addr, port } Addr { addr = addr', port = port' } maxUnacked version filePath num
      ) <$> option auto
              (  long "addr"
              <> metavar "ADDR"
              <> help "inbound address to connect to"
              <> value defaultAddr
              <> showDefault
              )
        <*> option auto
              (  long "port"
              <> metavar "PORT"
              <> help "inbound port number to connect to"
              <> value defaultPort
              <> showDefault
              )
        <*> option auto
            (  long "bind-addr"
            <> metavar "ADDR"
            <> help "outbound address to bind to"
            <> value defaultAddr
            <> showDefault
            )
        <*> option auto
              (  long "bind-port"
              <> metavar "PORT"
              <> help "outbound port number to bind to"
              <> value defaultPort
              <> showDefault
              )
        <*> option (Tx.NumTxIdsToReq <$> auto)
            (    long "unacked-txids"
              <> help "size of unacknowledged txid buffer"
              <> value (V2.maxUnacknowledgedTxIds V2.defaultTxDecisionPolicy)
              <> showDefaultWith (show . Tx.getNumTxIdsToReq)
            )
        <*> flag TxSubmissionLogicV2
                 TxSubmissionLogicV1
            (    long "v1"
              <> help "use tx-submission-v1 (default is v2)"
            )
        <*> strOption
             (    long "input"
               <> help "file with txs"
             )
        <*> option auto
             (   long "num"
              <> short 'n'
              <> help "Number of outbound clients to fork"
              <> value 1
              <> showDefault
             )

    generateParser =
      GenerateTxs
      <$> option auto
            (  long "number"
            <> short 'n'
            <> help "number of txs to generate"
            <> value defaultNumTxs
            <> showDefault
            )
      <*> strOption
           (  long "output"
           <> help "output file in cbor format"
           )
      where
        defaultNumTxs = 500

    analyseParser =
      AnalyseTxs
      <$> strOption
           (  long "input"
           <> help "input file in cbor format"
           )


newtype TxId = TxId Int
  deriving stock    (Eq, Ord, Show)
  deriving anyclass ShowProxy
  deriving newtype  (Serialise, NoThunks)

data Tx = Tx { txid      :: TxId,
               txPayload :: ByteString,
               txSize    :: SizeInBytes
             }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass ShowProxy
  deriving anyclass NoThunks

encodeTx :: Tx -> CBOR.Encoding
encodeTx Tx { txid, txPayload } =
     CBOR.encodeListLen 2
  <> CBOR.encode txid
  <> CBOR.encode txPayload

decodeTx :: CBOR.Decoder s (LBS.ByteString -> Tx)
decodeTx = do
  _ <- CBOR.decodeListLen
  txid <- CBOR.decode
  txPayload <- CBOR.decode
  pure $ \bs ->
    Tx { txid,
         txPayload,
         txSize = fromIntegral (LBS.length bs)
       }

-- | A `Tx`'s smart constructor which computes `txid` from the payload and
-- `txSize` by encoding the `Tx`.
--
mkTx :: ByteString -> Tx
mkTx txPayload =
  let tx =
        Tx { txid = TxId (Hashable.hash txPayload),
             txPayload,
             txSize = fromIntegral . BS.length . CBOR.toStrictByteString . encodeTx $ tx
           }
  in tx

getTxSize :: Tx -> SizeInBytes
getTxSize = txSize

getTxId :: Tx -> TxId
getTxId = txid


data DuplicateTxError = DuplicateTxError
  deriving stock    Show
  deriving anyclass Exception

type TxSubmission = Tx.TxSubmission2 TxId Tx

codec :: AnnotatedCodec
          TxSubmission
          CBOR.DeserialiseFailure
          IO
          LBS.ByteString
codec = Tx.anncodecTxSubmission2' (\bs tx -> tx { txSize = fromIntegral (LBS.length bs) })
                                  CBOR.encode CBOR.decode
                                  encodeTx    decodeTx

byteLimits
  :: Driver.ProtocolSizeLimits TxSubmission LBS.ByteString
byteLimits =
  Tx.byteLimitsTxSubmission2 (fromIntegral . LBS.length)

prettyWithBearer :: (a -> String) -> Mx.WithBearer Socket.SockAddr a -> String
prettyWithBearer pretty (Mx.WithBearer addr a) =
  unwords
    [ show addr
    , pretty a
    ]

txSubmissionTracer :: MVar () -> Tracer IO (Mx.WithBearer Socket.SockAddr (Driver.TraceSendRecv TxSubmission))
txSubmissionTracer lock =
    Tracer $ \msg ->
      withMVar lock $ \_ -> putStrLn (prettyWithBearer prettyMsg msg)
  where
    prettyMsg :: Driver.TraceSendRecv TxSubmission -> String
    prettyMsg (Driver.TraceSendMsg (AnyMessage msg)) =
      "send " ++ pretty msg
    prettyMsg (Driver.TraceRecvMsg (AnyMessage msg)) =
      "recv " ++ pretty msg

    -- Show `txid`s, conceal the `txPayload`.
    pretty :: forall (from :: TxSubmission)
                     (to :: TxSubmission).
              Tx.Message TxSubmission from to
           -> String
    pretty Tx.MsgInit = "MsgInit"
    pretty (Tx.MsgRequestTxIds _ ack req) =
      unwords
      [ "MsgRequestTxIds"
      , show ack
      , show req
      ]
    pretty (Tx.MsgReplyTxIds (Tx.BlockingReply txids)) =
      unwords
      $ "MsgReplyTxIds.bl"
      : (show <$> NonEmpty.toList txids)
    pretty (Tx.MsgReplyTxIds (Tx.NonBlockingReply txids)) =
      unwords
      $ "MsgReplyTxIds.nb"
      : (show <$> txids)
    pretty (Tx.MsgRequestTxs txids) =
      unwords
      $ "MsgRequestTxs"
      : (show <$> txids)
    pretty (Tx.MsgReplyTxs txs) =
      unwords
      $ "MsgReplyTxs"
      : (show . getTxId <$> txs)
    pretty Tx.MsgDone = "MsgDone"


inboundTracer :: MVar () -> Tracer IO (Mx.WithBearer Socket.SockAddr (V2.TraceTxSubmissionInbound TxId Tx))
inboundTracer lock =
    Tracer $ \msg ->
      withMVar lock $ \_ -> putStrLn (prettyWithBearer prettyMsg msg)
  where
    prettyMsg :: V2.TraceTxSubmissionInbound TxId Tx -> String
    prettyMsg (V2.TraceTxSubmissionCollected txids) =
      unwords ["TraceTxSubmissionCollected ", show txids]
    prettyMsg (V2.TraceTxSubmissionProcessed cnt) =
      unwords ["TraceTxSubmissionProcessed ", show cnt]
    prettyMsg (V2.TraceTxInboundCanRequestMoreTxs a) =
      unwords ["TraceTxInboundCanRequestMoreTxs ", show a]
    prettyMsg (V2.TraceTxInboundCannotRequestMoreTxs a) =
      unwords ["TraceTxInboundCannotRequestMoreTxs ", show a]
    prettyMsg (V2.TraceTxInboundAddedToMempool txids time) =
      unwords ["TraceTxInboundAddedToMempool", show txids, show time]
    prettyMsg (V2.TraceTxInboundRejectedFromMempool txids time) =
      unwords ["TraceTxInboundRejectedFromMempool", show txids, show time]
    prettyMsg (V2.TraceTxInboundError err) =
      unwords ["TraceTxInboundError", show err]
    prettyMsg  V2.TraceTxInboundTerminated =
      "TraceTxInboundTerminated"
    prettyMsg (V2.TraceTxInboundRequestTxs txids) =
      unwords ["TraceTxInboundDecision", show txids]


printTracer :: Show a => MVar () -> Tracer IO (Mx.WithBearer Socket.SockAddr a)
printTracer lock = Tracer $ \(Mx.WithBearer addr a) ->
  withMVar lock $ \_ -> putStrLn (show addr ++ " " ++ show a)


runTxInbound :: Addr
             -> V2.TxDecisionPolicy
             -> TxSubmissionLogicVersion
             -> DiffTime
             -> IO ()
runTxInbound Addr { addr, port } txDecisionPolicy version txDelay = do
    traceLock <- newMVar ()
    let hints = Socket.defaultHints
                  { Socket.addrFlags = [Socket.AI_ADDRCONFIG]
                  , Socket.addrFamily = Socket.AF_INET
                  , Socket.addrSocketType = Socket.Stream
                  }
    sockAddr:_ <- Socket.getAddrInfo (Just hints) (Just $ show addr) (Just $ show port)
    bracket
      (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
      Socket.close
      $ \sock -> do
        Socket.setSocketOption sock Socket.ReuseAddr 1
        Socket.bind sock (Socket.addrAddress sockAddr)
        Socket.listen sock 10

        -- V2 only
        txSharedStateVar <- V2.newSharedTxStateVar V2.emptySharedTxState
        txCountersVar <- V2.newTxSubmissionCountersVar mempty
        case version of
          TxSubmissionLogicV1 -> return ()
          TxSubmissionLogicV2 -> do
            thrd <- async $ V2.txCountersThreadV2
                            Tracer.nullTracer
                            txCountersVar
            link thrd

        mempool <- Mempool.new getTxId []

        forever $ do
          (sock', addr') <- Socket.accept sock
          forkIO $ flip finally (Socket.close sock')
                 $ handleJust (\e -> case fromException e of
                                    Just SomeAsyncException {} -> Nothing
                                    Nothing                    -> Just e
                              )
                              (\e -> throwIO e
                              )
                 $ Mx.withReadBufferIO $ \buffer -> do
            bearer <- Mx.getBearer Mx.makeSocketBearer 1.0 sock' buffer
            let dir = Mx.ResponderDirectionOnly
            mux <- Mx.new Mx.nullTracers
                          { Mx.tracer = Mx.WithBearer addr' . runIdentity >$< printTracer traceLock
                          -- , Mx.bearerTracer = Mx.WithBearer addr' . runIdentity >$< printTracer traceLock
                          }
                   (protocols dir)
            withAsync (Mx.run mux bearer) $ \_ ->
                  either throwIO return
              =<< atomically
              =<< Mx.runMiniProtocol
                    mux
                    txMiniProtocolNum
                    dir
                    Mx.StartOnDemand
                    (\chann -> do
                      let reader =
                            Mempool.getReader
                              getTxId
                              getTxSize
                              mempool
                          writer =
                            Mempool.getWriterWithCtx
                              (\txs -> fromLazyTVar
                                   <$> registerDelay
                                         (fromIntegral (length txs) * txDelay)
                              )
                              DuplicateTxError
                              getTxId
                              (\v txs -> do
                                -- NOTE: we are blocking access to the
                                -- mempool for all other threads that want
                                -- to add txs to it while we validate txs.
                                readTVar v >>= check
                                pure (Right <$> txs)
                              )
                              mempty
                              mempool
                      case version of
                        TxSubmissionLogicV1 -> do
                          Driver.runPipelinedAnnotatedPeerWithLimits
                            (Mx.WithBearer addr' >$< txSubmissionTracer traceLock)
                            codec
                            byteLimits
                            Tx.timeLimitsTxSubmission2
                            chann
                            ( Tx.Inbound.txSubmissionServerPeerPipelined
                            $ V1.txSubmissionInbound
                                Tracer.nullTracer
                                V1.NoTxSubmissionInitDelay
                                (fromIntegral $ V2.maxUnacknowledgedTxIds txDecisionPolicy)
                                reader
                                writer
                                ()
                            )

                        TxSubmissionLogicV2 ->
                          V2.withPeer
                            txDecisionPolicy
                            reader
                            txSharedStateVar
                            txCountersVar
                            addr'
                            $ \peerTxApi ->
                              Driver.runPipelinedAnnotatedPeerWithLimits
                                (Mx.WithBearer addr' >$< txSubmissionTracer traceLock)
                                codec
                                byteLimits
                                Tx.timeLimitsTxSubmission2
                                chann
                                ( Tx.Inbound.txSubmissionServerPeerPipelined
                                $ V2.txSubmissionInboundV2
                                  (Mx.WithBearer addr' >$< inboundTracer traceLock)
                                  V2.NoTxSubmissionInitDelay
                                  reader
                                  writer
                                  getTxSize
                                  peerTxApi
                                )
                    )


runTxOutbound :: Tracer IO String
              -> Addr -- ^ address to connect to
              -> Addr -- ^ address to bind to
              -> V2.TxDecisionPolicy
              -> TxSubmissionLogicVersion
              -> FilePath
              -> IO ()
runTxOutbound stderrTracer inboundAddr outboundAddr
                           txDecisionPolicy _version filePath = do
    traceLock <- newMVar ()
    mempool <- readTxs filePath
           >>= Mempool.new getTxId
    let hints = Socket.defaultHints
                  { Socket.addrFlags = [Socket.AI_ADDRCONFIG]
                  , Socket.addrFamily = Socket.AF_INET
                  , Socket.addrSocketType = Socket.Stream
                  }
    sockAddr:_ <- Socket.getAddrInfo (Just hints)
                                     (Just . show . addr $ inboundAddr)
                                     (Just . show . port $ inboundAddr)
    bindAddr:_ <- Socket.getAddrInfo (Just hints)
                                     (Just . show . addr $ outboundAddr)
                                     (Just . show . port $ outboundAddr)
    bracket
      (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
      Socket.close
      $ \sock -> do
        Socket.bind sock (Socket.addrAddress bindAddr)
        Socket.connect sock (Socket.addrAddress sockAddr)
          `catch` \case
            e | case ioe_type e of
                  NoSuchThing -> True
                  _ -> False
              -> do
                Tracer.traceWith stderrTracer $
                  unwords
                  [ "start the inbound side first:"
                  , "`cabal run exe:demo-tx-inbound -- inbound"
                  , "--addr"
                  , show (addr inboundAddr)
                  ,"--port"
                  , show (port inboundAddr) ++ "`"
                  ]
                throwIO e
              | otherwise -> do
                Tracer.traceWith stderrTracer (show e)
                throwIO e
        addr' <- Socket.getSocketName sock
        Mx.withReadBufferIO $ \buffer -> do
          bearer <- Mx.getBearer Mx.makeSocketBearer 1.0 sock buffer
          let dir = Mx.InitiatorDirectionOnly
          mux <- Mx.new Mx.nullTracers
                        { Mx.tracer = Mx.WithBearer addr' . runIdentity >$< printTracer traceLock
                        -- , Mx.bearerTracer = Mx.WithBearer addr' . runIdentity >$< printTracer traceLock
                        }
                 (protocols dir)
          withAsync (Mx.run mux bearer) $ \_ -> do
            let reader =
                  Mempool.getReader
                    getTxId
                    getTxSize
                    mempool
            either throwIO return =<< atomically =<< Mx.runMiniProtocol
              mux
              txMiniProtocolNum
              dir
              Mx.StartEagerly
              (\chann ->
                Driver.runAnnotatedPeerWithLimits
                  (Mx.WithBearer addr' >$< txSubmissionTracer traceLock)
                  codec
                  byteLimits
                  Tx.timeLimitsTxSubmission2
                  chann
                  ( Tx.Outbound.txSubmissionClientPeer
                  $ txSubmissionOutbound
                      Tracer.nullTracer
                      (Tx.NumTxIdsToAck . Tx.getNumTxIdsToReq $ V2.maxUnacknowledgedTxIds txDecisionPolicy)
                      reader
                  )
              )


-- | Tx-Submission mini-protocol number
txMiniProtocolNum :: Mx.MiniProtocolNum
txMiniProtocolNum = Mx.MiniProtocolNum 4


protocols :: Mx.MiniProtocolDirection mode -> [Mx.MiniProtocolInfo mode]
protocols miniProtocolDir =
  [ Mx.MiniProtocolInfo {
      Mx.miniProtocolNum        = txMiniProtocolNum,
      Mx.miniProtocolDir,
      Mx.miniProtocolLimits     = Mx.MiniProtocolLimits maxBound,
      Mx.miniProtocolCapability = Nothing
    }
  ]


genTxSizeRealistic :: Gen SizeInBytes
genTxSizeRealistic =
    -- Distribution targets (approx):
    -- min 55, p10 268, p25 402, median 879, p75 1400, p90 4500, p95 9600, max 16384, mean ~1600.
    -- Distribution taken from all TXs on mainnet during 2024 - 2025.
    QC.frequency
      [ (10, biasedRange 55   268)
      , (15, biasedRange 269  402)
      , (25, biasedRange 403  879)
      , (25, biasedRange 880  1400)
      , (15, biasedRange 1401 4500)
      , (5,  biasedRange 4501 9600)
      , (5,  biasedRange 9601 16384)
      ]
  where
    -- NOTE: This is intentionally not `choose lo hi` (uniform). Squaring `u`
    -- biases toward smaller values to match the target quantiles above.
    biasedRange :: SizeInBytes
                -> SizeInBytes
                -> Gen SizeInBytes
    biasedRange (SizeInBytes lo) (SizeInBytes hi) = do
      u <- QC.choose @Double (0.0, 1.0)
      let range :: Double
          range = fromIntegral (hi - lo)
          offset :: Word32
          offset = floor (u * u * range)
      pure (SizeInBytes (lo + offset))


-- | Generate a tx with payload of a given size.
-- 
-- NOTE: this slightly skews the realistic size distribution, since the
-- generated `txSize` will be larger than the payload size by CBOR overhead.
--
genTx :: SizeInBytes
      -- ^ payload size
      -> Gen Tx
genTx size = MkGen $ \(QCGen g0) _size ->
    if size <= 0
      then mkTx BS.empty
      else mkTx $ fst (BS.unfoldrN (fromIntegral size) gen g0)
  where
    gen :: SM.SMGen -> Maybe (Word8, SM.SMGen)
    gen !g = case SM.nextWord64 g of
      ~(w64, g') -> Just (fromIntegral w64, g')


-- | Generate [Tx] and write them to a file.
--
runTxsGenerator :: Int -> FilePath -> IO ()
runTxsGenerator num filePath = do
    gen <- newQCGen
    let txs :: [Tx]
        txs = unGen genTxs gen num
    BS.Builder.writeFile filePath (CBOR.toBuilder (encodeTxs txs))
  where
    genTxs :: Gen [Tx]
    genTxs = QC.vectorOf num (genTxSizeRealistic >>= genTx)

    encodeTxs :: [Tx] -> CBOR.Encoding
    encodeTxs txs =
         CBOR.encodeListLen (fromIntegral $ length txs)
      <> foldMap encodeTx txs



readTxs :: FilePath -> IO [Tx]
readTxs filePath = do
    bs <- LBS.readFile filePath
    case CBOR.deserialiseFromBytes decodeTxs' bs of
      Left e         -> throwIO e
      Right (_, txs) -> return $ runByteSpan bs <$> txs
  where
    runByteSpan :: LBS.ByteString
                -> WithByteSpan (LBS.ByteString -> Tx)
                -> Tx
    runByteSpan bs (WithByteSpan (f, start, end)) =
      f (bytesBetweenOffsets start end bs)

    decodeTxs' :: CBOR.Decoder s [WithByteSpan (LBS.ByteString -> Tx)]
    decodeTxs' = do
      l <- CBOR.decodeListLen
      replicateM l decodeTx'

    decodeTx' :: CBOR.Decoder s (WithByteSpan (LBS.ByteString -> Tx))
    decodeTx' = decodeWithByteSpan decodeTx


runTxsAnalyser :: FilePath -> IO ()
runTxsAnalyser filePath = do
  txs <- readTxs filePath
  let sizes = getSizeInBytes . getTxSize <$> txs

      minsize, maxsize :: Word32
      minsize = minimum sizes
      maxsize = maximum sizes

      avg :: Float
      avg = fromIntegral (sum sizes) / fromIntegral (length sizes)

      med :: Double
      med = Stat.median Stat.medianUnbiased (Vec.fromList $ fromIntegral <$> sizes)

  putStrLn ("length:  " ++ show (length sizes))
  putStrLn ("minimum: " ++ show minsize ++ "B")
  putStrLn ("maximum: " ++ show maxsize ++ "B")
  putStrLn ("average: " ++ show avg ++ "B")
  putStrLn ("median:  " ++ show med ++ "B")

