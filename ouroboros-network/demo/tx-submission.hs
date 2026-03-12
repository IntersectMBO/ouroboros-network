{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.IP (IP)

import Network.Mux qualified as Mx
import Network.Mux.Bearer qualified as Mx
import Network.Socket (PortNumber)
import Network.Socket qualified as Socket

import Options.Applicative
import Ouroboros.Network.TxSubmission.Inbound.V2.Types

main :: IO ()
main =
  execParser (info (optionParser <**> helper) fullDesc)
    >>= \case
      InboundOptions addr version ->
        runTxInbound addr version
      OutboundOptions addr version filePath ->
        runTxOutbound addr version filePath

data Options =
    InboundOptions Addr TxSubmissionLogicVersion
                   
  | OutboundOptions Addr TxSubmissionLogicVersion FilePath
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
  where
    defaultPort :: PortNumber
    defaultPort = 4000

    defaultAddr :: IP
    defaultAddr = read "127.0.0.1"

    inboundParser, outboundParser :: Parser Options
    inboundParser =
      (\addr port version ->
        InboundOptions Addr { addr, port } version
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
             (    long "txs"
               <> help "file with txs"
             )


runTxInbound :: Addr
             -> TxSubmissionLogicVersion
             -> IO ()
runTxInbound Addr { addr, port } _version = do
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
        forever $
          bracket
            (Socket.accept sock)
            (Socket.close . fst)
            $ \(sock', _) -> Mx.withReadBufferIO $ \buffer -> do
              bearer <- Mx.getBearer Mx.makeSocketBearer 1.0 sock' buffer
              let dir = Mx.ResponderDirectionOnly
              mux <- Mx.new Mx.nullTracers
                     (protocols dir)
              withAsync (Mx.run mux bearer) $ \_ -> do
                stm <- Mx.runMiniProtocol
                  mux
                  (Mx.MiniProtocolNum 1)
                  dir
                  Mx.StartOnDemand
                  $ \chann -> undefined -- TODO
                atomically stm >>= \case
                  Left e  -> throwIO e
                  Right _ -> return ()



runTxOutbound :: Addr
              -> TxSubmissionLogicVersion
              -> FilePath
              -> IO ()
runTxOutbound Addr { addr, port } _version _file = do
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
        Mx.withReadBufferIO $ \buffer -> do
          bearer <- Mx.getBearer Mx.makeSocketBearer 1.0 sock buffer
          let dir = Mx.InitiatorDirectionOnly
          mux <- Mx.new Mx.nullTracers
                 (protocols dir)
          withAsync (Mx.run mux bearer) $ \_ -> do
            stm <- Mx.runMiniProtocol
              mux
              (Mx.MiniProtocolNum 1)
              dir
              Mx.StartEagerly
              $ \chann -> undefined -- TODO
            atomically stm >>= \case
              Left e  -> throwIO e
              Right _ -> return ()


protocols :: Mx.MiniProtocolDirection mode -> [Mx.MiniProtocolInfo mode]
protocols miniProtocolDir =
  [ Mx.MiniProtocolInfo {
      Mx.miniProtocolNum        = Mx.MiniProtocolNum 1,
      Mx.miniProtocolDir,
      Mx.miniProtocolLimits     = Mx.MiniProtocolLimits maxBound,
      Mx.miniProtocolCapability = Nothing
    }
  ]
