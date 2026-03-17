{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}

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
import Data.ByteString.Builder qualified as BS.Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity (Identity (..))
import Data.Hashable qualified as Hashable
import Data.IP (IP)
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Options.Applicative
import System.Random qualified as Random

import Network.Mux qualified as Mx
import Network.Mux.Bearer qualified as Mx
import Network.Socket (PortNumber)
import Network.Socket qualified as Socket
import Network.TypedProtocol.Codec (AnyMessage (..), Codec)

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
import Ouroboros.Network.Util

import Demo.TxSubmission.Outbound

import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Gen qualified as QC
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Random (newQCGen)

main :: IO ()
main =
  execParser (info (optionParser <**> helper) fullDesc)
    >>= \case
      InboundOptions addr version txDelay ->
        runTxInbound addr version (microsecondsAsIntToDiffTime txDelay)
      OutboundOptions addr version filePath ->
        runTxOutbound addr version filePath
      GenerateTxs num filePath ->
        runTxsGenerator num filePath
      AnalyseTxs filePath ->
        runTxsAnalyser filePath

data Options =
    InboundOptions
      Addr
      TxSubmissionLogicVersion
      Int -- ^ tx validation time in microseconds

  | OutboundOptions
      Addr
      TxSubmissionLogicVersion
      FilePath -- ^ file path of tx cache

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
      (\addr port version txDelay ->
        InboundOptions Addr { addr, port } version txDelay
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
      (\addr port version filePath ->
        OutboundOptions Addr { addr, port } version filePath
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
        <*> flag TxSubmissionLogicV2
                 TxSubmissionLogicV1
            (    long "v1"
              <> help "use tx-submission-v1 (default is v2)"
            )
        <*> strOption
             (    long "input"
               <> help "file with txs"
             )

    generateParser =
      GenerateTxs
      <$> option auto
            (  long "number"
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
               txPayload :: ByteString
             }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass ShowProxy
  deriving anyclass NoThunks

instance CBOR.Serialise Tx where
  encode Tx { txid, txPayload } =
       CBOR.encodeListLen 2
    <> CBOR.encode txid
    <> CBOR.encode txPayload
  decode = do
    _ <- CBOR.decodeListLen
    txid <- CBOR.decode
    txPayload <- CBOR.decode
    pure Tx { txid, txPayload }

-- | A `Tx`'s smart constructor which computes `txid` from the payload.
--
mkTx :: ByteString -> Tx
mkTx txPayload = Tx { txid = TxId (Hashable.hash txPayload),
                      txPayload }

-- TODO: avoid re-serialisation
getTxSize :: Tx -> SizeInBytes
getTxSize = fromIntegral . LBS.length . CBOR.serialise


data DuplicateTxError = DuplicateTxError
  deriving stock    Show
  deriving anyclass Exception

type TxSubmission = Tx.TxSubmission2 TxId Tx

codec :: Codec TxSubmission
               CBOR.DeserialiseFailure
               IO
               LBS.ByteString
codec = Tx.codecTxSubmission2 CBOR.encode CBOR.decode
                              CBOR.encode CBOR.decode

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
      : (show . txid <$> txs)
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
    prettyMsg (V2.TraceTxInboundDecision decision) =
      unwords ["TraceTxInboundDecision", prettyShow decision]


printTracer :: Show a => MVar () -> Tracer IO (Mx.WithBearer Socket.SockAddr a)
printTracer lock = Tracer $ \(Mx.WithBearer addr a) ->
  withMVar lock $ \_ -> putStrLn (show addr ++ " " ++ show a)


runTxInbound :: Addr
             -> TxSubmissionLogicVersion
             -> DiffTime
             -> IO ()
runTxInbound Addr { addr, port } version txDelay = do
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
        txChann <- V2.newTxChannelsVar
        txMempoolSem <- V2.newTxMempoolSem
        txSharedState <- Random.newStdGen >>= V2.newSharedTxStateVar
        case version of
          TxSubmissionLogicV1 -> return ()
          TxSubmissionLogicV2 -> do
            thrd <- async $ V2.decisionLogicThreads
                       Tracer.nullTracer
                       Tracer.nullTracer
                       txDecisionPolicy
                       txChann
                       txSharedState
            link thrd


        mempool <- Mempool.new txid []

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
                              txid
                              getTxSize
                              mempool
                          writer =
                            Mempool.getWriterWithCtx
                              (\txs -> fromLazyTVar
                                   <$> registerDelay
                                         (fromIntegral (length txs) * txDelay)
                              )
                              DuplicateTxError
                              txid
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
                          Driver.runPipelinedPeerWithLimits
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
                            Tracer.nullTracer -- (Mx.WithBearer addr' >$< printTracer traceLock)
                            txChann
                            txMempoolSem
                            txDecisionPolicy
                            txSharedState
                            reader
                            writer
                            getTxSize
                            addr'
                            $ \peerTxApi ->
                              Driver.runPipelinedPeerWithLimits
                                (Mx.WithBearer addr' >$< txSubmissionTracer traceLock)
                                codec
                                byteLimits
                                Tx.timeLimitsTxSubmission2
                                chann
                                ( Tx.Inbound.txSubmissionServerPeerPipelined
                                $ V2.txSubmissionInboundV2
                                  (Mx.WithBearer addr' >$< inboundTracer traceLock)
                                  V2.NoTxSubmissionInitDelay
                                  writer
                                  peerTxApi
                                )
                    )

-- TODO: make it configurable
txDecisionPolicy  :: V2.TxDecisionPolicy
txDecisionPolicy = V2.defaultTxDecisionPolicy


runTxOutbound :: Addr
              -> TxSubmissionLogicVersion
              -> FilePath
              -> IO ()
runTxOutbound Addr { addr, port } _version filePath = do
    traceLock <- newMVar ()
    mempool <- readTxs filePath
           >>= Mempool.new txid
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
        Socket.connect sock (Socket.addrAddress sockAddr)
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
                    txid
                    getTxSize
                    mempool
            either throwIO return =<< atomically =<< Mx.runMiniProtocol
              mux
              txMiniProtocolNum
              dir
              Mx.StartEagerly
              (\chann ->
                Driver.runPeerWithLimits
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


-- | Generate [Tx] and write them to a file.
--
runTxsGenerator :: Int -> FilePath -> IO ()
runTxsGenerator num filePath = do
    gen <- newQCGen
    let txs :: [Tx]
        txs = unGen genTxs gen num
    BS.Builder.writeFile filePath (CBOR.toBuilder (encode txs))
  where
    genTxs :: Gen [Tx]
    genTxs = QC.vectorOf num $ mkTx <$> arbitrary


readTxs :: FilePath -> IO [Tx]
readTxs filePath = do
  bs <- LBS.readFile filePath
  case CBOR.deserialiseFromBytes decode bs of
    Left e         -> throwIO e
    Right (_, txs) -> return txs


runTxsAnalyser :: FilePath -> IO ()
runTxsAnalyser filePath = do
  txs <- readTxs filePath
  let sizes = getSizeInBytes . getTxSize <$> txs
      minsize = minimum sizes
      maxsize = maximum sizes
      avg :: Float
      avg = fromIntegral (sum sizes) / fromIntegral (length sizes)
  putStrLn ("minimum: " ++ show minsize ++ "B")
  putStrLn ("maximum: " ++ show maxsize ++ "B")
  putStrLn ("average: " ++ show avg ++ "B")

