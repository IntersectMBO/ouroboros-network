{-# LANGUAGE ScopedTypeVariables #-}
module NamedPipe (
    runConsumer
  , runProducer
  ) where

import           Control.Exception (SomeException, bracket, catch)
import           Data.Semigroup ((<>))
import           System.Directory (removeFile)
import           System.IO
import           System.Posix.Files (createNamedPipe, otherReadMode, ownerModes,
                     unionFileModes)

import           Ouroboros.Network.Block (HasHeader)
import           Ouroboros.Network.ConsumersAndProducers
import           Ouroboros.Network.Node (NodeId (..))
import qualified Ouroboros.Network.Pipe as P
import           Ouroboros.Network.Serialise (Serialise)

-- | Creates two pipes, one for reading, one for writing.
withPipe :: (NodeId, NodeId)
         -> ((Handle, Handle) -> IO a)
         -> IO a
withPipe (fromNode, toNode) action = do
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
  where
    dashify :: NodeId -> String
    dashify (CoreId n)  = "core-node-"  <> show n
    dashify (RelayId n) = "relay-node-" <> show n

-- | Runs a producer protocol over a named pipe (technically speaking two
-- pipes, one for reads, one for writes).
runProducer :: (HasHeader block, Serialise block)
            => NodeId
            -> NodeId
            -> ProducerHandlers block IO r
            -> IO ()
runProducer myId targetId producer =
    withPipe (myId, targetId) $ \(hndRead, hndWrite) ->
      P.runProducer hndRead hndWrite producer

-- | Runs a consumer protocol over a named pipe (technically speaking two
-- pipes, one for reads, one for writes).
runConsumer :: (HasHeader block, Serialise block)
            => NodeId
            -> NodeId
            -> ConsumerHandlers block IO
            -> IO ()
runConsumer myNodeId targetId consumer =
    withPipe (myNodeId, targetId) $ \(hndRead, hndWrite) ->
      P.runConsumer hndRead hndWrite consumer
