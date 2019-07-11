module Shelley.Server
  ( Options (..)
  , runServer
  ) where

import Codec.SerialiseTerm (encodeTerm, decodeTerm)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait)
import Control.Exception (throwIO)
import Control.Monad.Trans.Class (lift)
import qualified Network.Socket as Network

import qualified Cardano.Chain.Slotting as Cardano (EpochSlots)

import Ouroboros.Byron.Proxy.DB (DB)
import Ouroboros.Byron.Proxy.Network.Protocol (responderVersions)
import Ouroboros.Byron.Proxy.ChainSync.Server (PollT, chainSyncServer)
import Ouroboros.Network.Protocol.Handshake.Type (Accept (..))
import Ouroboros.Network.Socket (AnyMuxResponderApp (..), newConnectionTable, withServerNode)

import Orphans ()

data Options = Options
  { hostName    :: !Network.HostName
  , serviceName :: !Network.ServiceName
  }

-- | Run a Shelley server. It's just chain sync on cardano-ledger blocks.
runServer
  :: Options
  -> Cardano.EpochSlots
  -> DB IO
  -> IO ()
runServer serverOptions epochSlots db = do
  addrInfos <- Network.getAddrInfo (Just addrInfoHints) (Just host) (Just port)
  tbl <- newConnectionTable
  case addrInfos of
    [] -> error "no getAddrInfo"
    (addrInfo : _) -> withServerNode
      tbl
      addrInfo
      encodeTerm
      decodeTerm
      -- TODO: this should be some proper type rather than a tuple
      (,)
      (\_ _ _ -> Accept)
      (fmap AnyMuxResponderApp (responderVersions epochSlots app))
      (\_ -> wait)

  where

  app = chainSyncServer epochSlots err poll db

  host = hostName serverOptions
  port = serviceName serverOptions
  addrInfoHints = Network.defaultHints
  err =  throwIO

  -- TODO configure this
  -- microsecond polling time of the DB. Needed until there is a proper
  -- storage layer.
  usPoll = 1000000

  -- Definition of how to poll in IO.
  poll :: PollT IO
  poll p m = do
    s <- m
    mbT <- p s
    case mbT of
      Nothing -> lift (threadDelay usPoll) >> poll p m
      Just t  -> pure t
