{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent.Async (race)
import Control.Exception (bracket)
import Control.Tracer (nullTracer)
import Data.Functor.Contravariant (Op (..))

import Pos.Util.Trace (Trace (..), traceWith)

import Network.TypedProtocol.Driver (runPeer)
import qualified Network.Socket as Socket

import Ouroboros.Network.Channel (socketAsChannel)
import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Byron.Proxy.ChainSync.Client as Client
import qualified Ouroboros.Byron.Proxy.ChainSync.Types as ChainSync

import qualified Ouroboros.Byron.Proxy.DB as DB (blockEpochAndRelativeSlot, ebbEpoch)

-- | Echos rolls (forward or backward) using a trace.
chainSyncClient
  :: forall m x .
     ( Monad m )
  => Trace m (Either ChainSync.Point ChainSync.Block, ChainSync.Point)
  -> ChainSync.ChainSyncClient ChainSync.Block ChainSync.Point m x
chainSyncClient trace = Client.chainSyncClient fold
  where
  fold :: Client.Fold m x
  fold = Client.Fold $ pure $ Client.Continue forward backward
  forward :: ChainSync.Block -> ChainSync.Point -> Client.Fold m x
  forward blk point = Client.Fold $ do
    traceWith trace (Right blk, point)
    Client.runFold fold
  backward :: ChainSync.Point -> ChainSync.Point -> Client.Fold m x
  backward point1 point2 = Client.Fold $ do
    traceWith trace (Left point1, point2)
    Client.runFold fold

-- | Connects to a server at 127.0.0.1:7777 and runs the chain sync client.
runChainSyncClient :: IO x
runChainSyncClient = bracket mkSocket Socket.close $ \socket -> do
  _ <- Socket.connect socket (Socket.SockAddrInet 7777 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  let channel = socketAsChannel socket
      peer = ChainSync.chainSyncClientPeer (chainSyncClient clientEcho)
  runPeer nullTracer ChainSync.codec channel peer
  where
  mkSocket = do
    socket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.bind socket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (0, 0, 0, 0)))
    pure socket

clientEcho :: Trace IO (Either ChainSync.Point ChainSync.Block, ChainSync.Point)
clientEcho = Trace $ Op $ \(roll, _tip) ->
  let msg = case roll of
        Left  back    -> mconcat
          [ "Roll back to "
          , show back
          ]
        Right forward -> mconcat
          [ "Roll forward to "
          , case ChainSync.getBlock forward of
              Left ebb  -> show $ DB.ebbEpoch ebb
              Right blk -> show $ DB.blockEpochAndRelativeSlot blk
          ]
  in  putStrLn msg

main :: IO ()
main = do
  let userInterrupt = getChar
      mainThread = runChainSyncClient
  outcome <- race userInterrupt mainThread
  case outcome of
    Left _ -> pure ()
    Right impossible -> impossible
