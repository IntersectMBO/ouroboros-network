{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocol.Stream.Client
    ( Client
    , newClient
    , streamClient
    ) where

import Control.Monad.STM
import Control.Concurrent.STM.TBQueue
import Control.Monad (unless)
import Numeric.Natural (Natural)

import Protocol.Core
import Protocol.Stream.Type

data Client a = Client
    { clientWindow :: Natural
    , clientQueue  :: TBQueue a
    }

newClient :: Natural -> IO (Client a)
newClient n = Client n <$> atomically (newTBQueue n)

streamClient
    :: forall rng a.
       Client a
    -> rng
    -> Peer StreamProtocol (StreamMessage rng a)
        ('Yielding 'StIdle) ('Finished 'StDone)
        IO ()
streamClient Client{..} r = over (MsgRequest r clientWindow) go 
  where
    go :: Peer StreamProtocol (StreamMessage rng a)
            ('Awaiting 'StBusy) ('Finished 'StDone)
            IO ()
    go = await $ \resp ->
        case resp of
            -- wait until the queue is consumed, send @'MsgUpdateWindow'@,
            -- recurse
            MsgRenewWindow  -> hole $ do
                atomically $ do
                    empty <- isEmptyTBQueue clientQueue
                    unless empty retry
                pure $ over MsgUpdateWindow go
            -- write data to the queue and recurse
            MsgData a -> hole $ do
                atomically $ writeTBQueue clientQueue a
                return go
            -- we are done
            MsgStreamEnd -> done ()
