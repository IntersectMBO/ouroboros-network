{-# LANGUAGE ScopedTypeVariables #-}
module NamedPipe (
    runConsumer
  , runProducer
  -- * Sending & receiving txs
  , withTxPipe
  ) where

import           Control.Exception (SomeException, bracket, catch)
import           Control.Monad (when)
import           Data.Semigroup ((<>))
import           GHC.Stack
import           System.Directory (removeFile)
import           System.IO
import           System.Posix.Files (createNamedPipe, otherReadMode,
                     otherWriteMode, ownerModes, ownerReadMode, unionFileModes)

import           Ouroboros.Network.Block (HasHeader)
import           Ouroboros.Network.ConsumersAndProducers
import           Ouroboros.Network.Node (NodeId (..))
import qualified Ouroboros.Network.Pipe as P
import           Ouroboros.Network.Serialise (Serialise)

-- | Creates two pipes, one for reading, one for writing.
withPipes :: HasCallStack
          => (NodeId, NodeId)
          -> ((Handle, Handle) -> IO a)
          -> IO a
withPipes (fromNode, toNode) action = do
    let (src,tgt) = (dashify fromNode, dashify toNode)
    let readName  = "ouroboros-" <> tgt <> "-to-" <> src
    let writeName = "ouroboros-" <> src <> "-to-" <> tgt
    bracket (do createNamedPipe readName  (unionFileModes ownerModes otherReadMode)
                    `catch` (\(_ :: SomeException) -> pure ())
                createNamedPipe writeName (unionFileModes ownerModes otherReadMode)
                    `catch` (\(_ :: SomeException) -> pure ())
                (,) <$> openFile readName   ReadWriteMode
                    <*> openFile writeName  ReadWriteMode
            ) (\(r,w) -> do
                hClose r
                hClose w
                -- Destroy the pipes
                removeFile readName
                  `catch` (\(_ :: SomeException) -> pure ())
                removeFile writeName
                  `catch` (\(_ :: SomeException) -> pure ())
                )
            action

-- | Given a 'NodeId', it dashifies it.
dashify :: NodeId -> String
dashify (CoreId n)  = "core-node-"  <> show n
dashify (RelayId n) = "relay-node-" <> show n

-- | Given a 'NodeId', it yields a predictable name which can be used to
-- read transactions out of band.
namedTxPipeFor :: NodeId -> String
namedTxPipeFor n = "ouroboros-" <> dashify n <> "-tx-pipe"

-- | Creates a unidirectional pipe for Tx transmission.
withTxPipe :: HasCallStack
          => NodeId
          -> Bool
          -- ^ Whether or not to destroy the pipe at teardown.
          -> (Handle -> IO a)
          -> IO a
withTxPipe node destroyAfterUse action = do
    let pipeName = namedTxPipeFor node
    bracket (do createNamedPipe pipeName (unionFileModes ownerReadMode otherWriteMode)
                    `catch` (\(_ :: SomeException) -> pure ())
                openFile pipeName ReadWriteMode
            ) (\p -> do
                hClose p
                when destroyAfterUse $
                  -- Destroy the pipe
                  removeFile pipeName
                    `catch` (\(_ :: SomeException) -> pure ())
                )
            action

-- | Runs a producer protocol over a named pipe (technically speaking two
-- pipes, one for reads, one for writes).
runProducer :: (HasHeader block, Serialise block)
            => NodeId
            -> NodeId
            -> ProducerHandlers block IO r
            -> IO ()
runProducer myId targetId producer =
    withPipes (myId, targetId) $ \(hndRead, hndWrite) ->
      P.runProducer hndRead hndWrite producer

-- | Runs a consumer protocol over a named pipe (technically speaking two
-- pipes, one for reads, one for writes).
runConsumer :: (HasHeader block, Serialise block)
            => NodeId
            -> NodeId
            -> ConsumerHandlers block IO
            -> IO ()
runConsumer myNodeId targetId consumer =
    withPipes (myNodeId, targetId) $ \(hndRead, hndWrite) ->
      P.runConsumer hndRead hndWrite consumer
