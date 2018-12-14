{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module NamedPipe (
    runPeerUsingNamedPipeCbor
  -- * Sending & receiving txs
  , withTxPipe
  ) where

import           Control.Exception (SomeException, bracket, catch)
import           Control.Monad (when)
import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import           Data.ByteString (ByteString)
import           Data.Semigroup ((<>))
import           Data.Text (Text, unpack)
import           GHC.Stack
import           System.Directory (removeFile)
import           System.IO
import           System.Posix.Files (createNamedPipe, otherReadMode,
                     otherWriteMode, ownerModes, unionFileModes)

import           Protocol.Codec (Codec)
import           Protocol.Core (Peer)
import           Protocol.Driver (Result (..), useCodecWithDuplex)

import           Ouroboros.Network.Node (NodeId (..))
import qualified Ouroboros.Network.Pipe as P

-- | Creates two pipes, one for reading, one for writing.
withPipe :: HasCallStack
          => (NodeId, NodeId)
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
          -> IOMode
          -> Bool
          -- ^ Whether or not to destroy the pipe at teardown.
          -> (Handle -> IO a)
          -> IO a
withTxPipe node ioMode destroyAfterUse action = do
    let pipeName = namedTxPipeFor node
    bracket (do createNamedPipe pipeName (unionFileModes ownerModes otherWriteMode)
                    `catch` (\(_ :: SomeException) -> pure ())
                openFile pipeName ioMode
            ) (\p -> do
                hClose p
                when destroyAfterUse $
                  -- Destroy the pipe
                  removeFile pipeName
                    `catch` (\(_ :: SomeException) -> pure ())
                )
            action

-- | Runs a peer over a named pipe using a cbor-encoded transition (technically
-- speaking two pipes, one for reads, one for writes).
runPeerUsingNamedPipeCbor
  :: NodeId
  -> NodeId
  -> Codec IO Text CBOR.Encoding ByteString tr begin
  -> Peer proto tr (status begin) end IO a
  -> IO a
runPeerUsingNamedPipeCbor myId targetId codec peer =
    withPipe (myId, targetId) $ \(hndRead, hndWrite) ->
      let channel = P.pipeDuplex hndRead hndWrite
       in throwOnUnexpected =<< useCodecWithDuplex channel codec peer
  where
  throwOnUnexpected (Normal t) = pure t
  throwOnUnexpected (Unexpected txt) = error (unpack txt)
