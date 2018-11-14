{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Ouroboros.Network.Protocol
  ( MsgConsumer(..)
  , MsgProducer(..)
  , ConsumerHandlers
  , ProducerHandlers
  , consumerSideProtocol1
  , producerSideProtocol1
  , MsgConsumerBlock(..)
  , MsgProducerBlock(..)
  , withConsumerBlockLayer
  , producerBlockLayer
  , Window (..)
  , MsgConsumerBlocks(..)
  , MsgStreamBlocks(..)
  , withConsumerBlockStreamLayer
  , producerBlockStreamLayer
  , loggingSend
  , loggingRecv
  ) where

import           Control.Monad
import           Control.Exception (assert)
import           Data.Word (Word32)
import qualified Streaming.Prelude as S

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (ChainUpdate (..), Point (..), blockPoint)
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.ProtocolInterfaces (BlockConsumerHandlers (..),
                     BlockProducerHandlers (..), ConsumerHandlers (..),
                     ProducerHandlers (..), Promise (..))
import           Ouroboros.Network.Serialise

{-# ANN module "HLint: ignore Use readTVarIO" #-}

--
-- IPC based protocol
--

-- | In this protocol the consumer always initiates things and the producer
-- replies. This is the type of messages that the consumer sends.
data MsgConsumer block
  = MsgRequestNext
  -- ^ Request next block from the producer
  | MsgSetHead [Point block]
  -- ^
  -- Send set of points, it is up to the producer to find the intersection
  -- point on its chain and send it back to the consumer.
    deriving (Eq, Show)

-- | This is the type of messages that the producer sends.
data MsgProducer block
  = MsgRollForward  block
  -- ^ Ask the consumer to roll forward to a given block
  | MsgRollBackward (Point block)
  -- ^
  -- Ask the consumer to roll back to a given Point on its chain
  | MsgAwaitReply
  -- ^
  -- Inform the consumer to await for next instructions; This means that the
  -- producer is synced with the consumer end and its awaiting for its chain to
  -- be changed.
  | MsgIntersectImproved (Point block) (Point block)
  -- ^
  -- Sends to consumer found intersection, but only if this is an improvement
  -- over previously established intersection point.  The consumer
  -- will decide weather to send more points.  They should all be newer than the
  -- received intersection.  The first point is the improved point, the second
  -- is the current tip.
  | MsgIntersectUnchanged
  -- ^
  -- After receiving intersection points from the consumer it maybe happen that
  -- none of the points is on the producer chain; in this case
  -- @'MsgIntersectUnchanged'@ is send back.
    deriving (Eq, Show)

-- |
-- A simple version of a consumer which sends set of points, accepts any respond
-- and steps into the second phase of the protocol in which it sends @'MsgRequestNext'@ and expects one of:
--   - @'MsgAwaitReplay'@
--   - @'MsgRollForward'@
--   - @'MsgRollBackward'@
-- @'ConsumerHandlers'@ is a record which contains all the callbacks needed to
-- run the consumer side of the protocol.
consumerSideProtocol1
  :: forall block m.
     Monad m
  => ConsumerHandlers block m
  -> (MsgConsumer block -> m ()) -- ^ send
  -> (m (MsgProducer block))     -- ^ recv
  -> m ()
consumerSideProtocol1 ConsumerHandlers{..} send recv = do
  -- The consumer opens by sending a list of points on their chain.
  -- This typically includes the head block and recent points
  points <- getChainPoints
  unless (null points) $ do
    send (MsgSetHead points)
    _msg <- recv
    return ()
  requestNext
 where
  requestNext :: m ()
  requestNext = do
    send MsgRequestNext
    reply <- recv
    case reply of
      MsgAwaitReply -> do reply' <- recv
                          handleChainUpdate reply'
      _             -> handleChainUpdate reply
    requestNext

  handleChainUpdate :: MsgProducer block -> m ()
  handleChainUpdate (MsgRollForward  b) = addBlock b
  handleChainUpdate (MsgRollBackward p) = rollbackTo p
  handleChainUpdate  MsgAwaitReply           = fail $ "protocol error: MsgAwaitReply"
  handleChainUpdate (MsgIntersectImproved{}) = fail $ "protocol error: MsgIntersectImproved"
  handleChainUpdate  MsgIntersectUnchanged   = fail $ "protocol error: MsgIntersectUnchanged"


-- | Producer side of the chain following protocol.
-- It awaits and serves requests from the consumer side: next or set head.
-- These are backed by the 'ProducerHandlers' parameter.
-- The other two parameter are for sending and receiving messages to/from the
-- consumer. This producer never terminates.
producerSideProtocol1
  :: forall block m r.
     Monad m
  => ProducerHandlers block m r
  -> (MsgProducer block -> m ()) -- ^ send
  -> (m (MsgConsumer block))     -- ^ recv
  -> m ()
producerSideProtocol1 ProducerHandlers{..} send recv =
  newReader >>= awaitOngoing
 where
  awaitOngoing r = forever $ do
    msg <- recv
    case msg of
      MsgRequestNext    -> handleNext r
      MsgSetHead points -> handleSetHead r points

  handleNext r = do
    mupdate <- tryReadChainUpdate r
    update  <- case mupdate of
      Just update -> return update

      -- Reader is at the head, have to wait for producer state changes.
      Nothing -> do
        send MsgAwaitReply
        readChainUpdate r
    send (updateMsg update)

  handleSetHead r points = do
    -- TODO: guard number of points, points sorted
    -- Find the first point that is on our chain
    changed <- improveReadPoint r points
    case changed of
      Just (pt, tip) -> send (MsgIntersectImproved pt tip)
      Nothing        -> send MsgIntersectUnchanged

  updateMsg (AddBlock b) = MsgRollForward b
  updateMsg (RollBack p) = MsgRollBackward p

data MsgConsumerBlock block
  = MsgRequestBlock (Point block)
  -- ^ Ask for a block
  deriving (Eq, Show)

data MsgProducerBlock blockBody
  = MsgBlock blockBody
  -- ^ Respond with a block body
  | MsgNoBlock
  -- ^ Producer has no such block
  | MsgAwaitBlock
  -- ^ Producer requested a block, and is awaiting
  deriving (Eq, Show)

withConsumerBlockLayer
  :: forall m blockHeader blockBody.
     ( MonadFork m
     , MonadSTM  m
     , HasHeader blockHeader
     )
  => BlockConsumerHandlers blockHeader blockBody m
  -> (MsgConsumerBlock blockHeader -> m ()) -- ^ request a block
  -> m (MsgProducerBlock blockBody)         -- ^ receive a block
  -> ((blockHeader -> m ()) -> m ())
      -- ^ conversation which requests a block body
      -- and runs the continuation
  -> (Maybe blockBody -> m ())
      -- ^ continuation; @'Nothing'@ signifies that
      -- the producer responded with @'MsgNoBlock'@
  -> m ()
withConsumerBlockLayer BlockConsumerHandlers{..} send recv conv k = conv forkRequestBlockConv
 where
  forkRequestBlockConv :: blockHeader -> m ()
  forkRequestBlockConv h = fork $ do
    send $ MsgRequestBlock (blockPoint h)
    recvBlock

  recvBlock :: m ()
  recvBlock = do
    msg <- recv
    case msg of
      MsgBlock bb   -> k (Just bb)
      MsgNoBlock    -> k Nothing
      MsgAwaitBlock -> recvBlock

producerBlockLayer
  :: forall m blockHeader blockBody.
     ( MonadFork m
     , MonadSTM  m
     , StandardHash blockHeader
     )
  => BlockProducerHandlers blockHeader blockBody m
  -> (MsgProducerBlock blockBody -> m ()) -- ^ send a block
  -> m (MsgConsumerBlock blockHeader)     -- ^ receive a request
  -> m ()
producerBlockLayer BlockProducerHandlers {..} send recv = fork $ forever $ do
  MsgRequestBlock p <- recv
  mbb <- getBlock p
  case mbb of
    Just (Fullfilled bb) -> send (MsgBlock bb)
    Just Awaiting -> do
      send MsgAwaitBlock
      mbb' <- awaitBlock p
      case mbb' of
        Nothing -> send MsgNoBlock
        Just bb -> send (MsgBlock bb)
    Nothing -> send MsgNoBlock

-- | Request a range of blocks which the producer will stream
--
data MsgConsumerBlocks blockHeader
  = MsgRequestBlocks (Point blockHeader) (Point blockHeader) Window
  -- ^ Ask for a range of blocks with a given window size
  | MsgUpdateWindow Window
  -- ^ update streaming window

-- | When requesting a range of blocks the producer will stream block bodies.
--
data MsgStreamBlocks blockBody
  = MsgStreamBlock blockBody
  -- ^ Response with a single block body
  | MsgStreamEnd
  -- ^ if the consumer requested a range of blocks, the producer will respond
  -- with a sequence of @'MsgBlock'@ finalised with @'MsgStreamEnd'@
  deriving (Eq, Show)

-- | Type which is used to pack elements of a `TBQueue` which gives information
-- when a stream of elements ends.
--
data StreamElement a
  = StreamElement a
  | EndOfStream
  deriving (Eq, Ord, Show)

-- | Window size for the block layer streaming protocol.
--
newtype Window = Window { runWindow :: Word32 }
  deriving (Eq, Show, Ord, Enum, Num)

deriving instance Real Window
deriving instance Integral Window

-- | Threshold for the block layer protocol.  It should be striclly smaller
-- than the window size.
--
newtype Threshold = Threshold { runThreshold :: Word32 }
  deriving (Eq, Show, Ord, Enum, Num)

deriving instance Real Threshold
deriving instance Integral Threshold

-- | Consumer part of the block streaming protocol.
--
-- When the consumer will receive `Threshold` messages it will send
-- `MsgUpdateWindow` with the difference between window and the threshold.
-- This means that the receivers queue might have at most `window + (window
-- threshold)` writes.  After `window` writes its queue will block (`TBQueue`).
--
--  In the following digrams, lines from client to server represent
--  `MsgRequestBlock` (the initial line), or `MsgUpdateWindow`.  The lines from
--  server to client represent `MsgStreamBlock`.
--
--  `x` represent the point when the client will block (not sending next
--  `MsgUpdateQueue`) if the consumer was not reading from its queue.  This is
--  after receiving `window + (window - threshold)` messages.  This is the
--  maximum number of allocations the client must be prepared for.
--
--  window    = 3
--  threshold = 2
--
--  server -----------------------
--            / \ \ \  /\    /\
--           /   \ \ \/  \  /  \
--          /     \ \/\   \/    \
--  client ---------------x-------
--
--  window    = 4
--  threshold = 2
--  
--  server -----------------------
--            / \ \ \ \/\ \  /\ \
--           /   \ \ \/\ \ \/  \ \
--          /     \ \/\ \ \/\   \ \
--  client ---------------x-------
--
withConsumerBlockStreamLayer
  :: forall m blockHeader blockBody.
     ( MonadSTM m
     , MonadTBQueue m
     , HasHeader blockHeader
     )
  => BlockConsumerHandlers blockHeader blockBody m
  -> (MsgConsumerBlocks blockHeader -> m ()) -- ^ request blocks
  -> m (MsgStreamBlocks blockBody)           -- ^ receive a block
  -> ((Window -> Threshold -> Point blockHeader -> Point blockHeader -> m ()) -> m ())
    -- ^
    -- conversation which requests a stream of
    -- blocks and runs the continuation.  Size
    -- is the size of queue and it should be non
    -- zero.
  -> (TBQueue m (StreamElement blockBody) -> m ())
    -- ^
    -- continuation, which is run in a new thread
  -> m ()
withConsumerBlockStreamLayer BlockConsumerHandlers{..} send recv conv k
  = conv forkRequestBlockStreamConv
 where
  forkRequestBlockStreamConv :: Window -> Threshold -> Point blockHeader -> Point blockHeader -> m ()
  forkRequestBlockStreamConv window threshold from to
    = assert
        (window /= Window 0 && runWindow window > runThreshold threshold)
      $ fork $ do
        send $ MsgRequestBlocks from to window
        queue <- atomically $ newTBQueue (fromIntegral window)
        fork (k queue)
        recvBlock window threshold 0 queue

  recvBlock :: Window -> Threshold -> Word32 -> TBQueue m (StreamElement blockBody) -> m ()
  recvBlock window threshold !writes queue = do
    msg <- recv
    case msg of
      MsgStreamBlock bb -> do
        -- at most `window - threshold` `writeTBQueue` calls will
        -- await writting to the queue, which size is `window`.
        atomically $ writeTBQueue queue (StreamElement bb)
        if writes >= fromIntegral threshold
          -- threshold reached, update the window
          then do
            send $ MsgUpdateWindow (window - fromIntegral threshold)
            recvBlock window threshold 0 queue
          else do
            recvBlock window threshold (writes + 1) queue
      MsgStreamEnd ->
        atomically $ writeTBQueue queue EndOfStream


-- | Producer of the block streams. It awaits for initial `MsgRequestBlocks`
-- message which commands how many blocks the producer can stream before it
-- needs to wait for `MsgUpdateWindow`.
--
producerBlockStreamLayer
  :: forall m blockHeader blockBody.
     ( MonadFork m
     , MonadSTM  m
     )
  => BlockProducerHandlers blockHeader blockBody m
  -> (MsgStreamBlocks blockBody -> m ()) -- ^ send a block
  -> m (MsgConsumerBlocks blockHeader)   -- ^ receive a request
  -> m ()
producerBlockStreamLayer BlockProducerHandlers {..} send recv = fork $ forever $ do
  msg <- recv
  case msg of
    MsgUpdateWindow{} -> error "producerBlockStreamLayer: protocol error"
    MsgRequestBlocks from to window -> do
      windowVar <- atomically $ newTVar window
      S.mapM_ (fn windowVar) (streamBlocks from to)
 where
  fn windowVar bb = do
    window <- atomically (readTVar windowVar)
    if window <= 0
      then do
        msg' <- recv
        case msg' of
          MsgUpdateWindow window' -> do
            send (MsgStreamBlock bb)
            atomically $ writeTVar windowVar (pred window')
          MsgRequestBlocks{}    -> error "producerBlockStreamLayer: block streaming: protocol error"
      else do
        send (MsgStreamBlock bb)
        atomically $ modifyTVar' windowVar pred

-- | A wrapper for send that logs the messages
--
loggingSend :: (Show msg, MonadSay m, Show id) => id -> (msg -> m a) -> msg -> m a
loggingSend ident send msg = do
  say $ (show ident) ++ ":send: " ++ show msg
  send msg

-- | A wrapper for recv that logs the messages
--
loggingRecv :: (Show msg, MonadSay m, Show id) => id -> m msg -> m msg
loggingRecv ident recv = do
  msg <- recv
  say $ (show ident) ++ ":recv: " ++ show msg
  return msg

--
-- Serialisation
--

encodeMessage :: Word -> Word -> Encoding -> Encoding
encodeMessage conversationId messageTag messageBody =
    encodeListLen 3
 <> encodeWord conversationId
 <> encodeWord messageTag
 <> messageBody

instance HasHeader block => Serialise (MsgConsumer block) where

  encode MsgRequestNext  = encodeMessage 1 0 $ encodeNull
  encode (MsgSetHead ps) = encodeMessage 1 1 $ encode ps

  decode = do
    decodeListLenOf 3
    decodeWordOf 1
    tag <- decodeWord
    case tag of
      0 -> MsgRequestNext <$ decodeNull
      1 -> MsgSetHead <$> decode
      _ -> fail "MsgConsumer unexpected tag"

instance (Serialise block, HasHeader block) => Serialise (MsgProducer block) where

  encode (MsgRollForward  b)        = encodeMessage 2 0 $ encode b
  encode (MsgRollBackward p)        = encodeMessage 2 1 $ encode p
  encode  MsgAwaitReply             = encodeMessage 2 2 $ encodeNull
  encode (MsgIntersectImproved p t) = encodeMessage 2 3 $ encode (p, t)
  encode  MsgIntersectUnchanged     = encodeMessage 2 4 $ encodeNull

  decode = do
    decodeListLenOf 3
    decodeWordOf 2
    tag <- decodeWord
    case tag of
      0 -> MsgRollForward        <$> decode
      1 -> MsgRollBackward       <$> decode
      2 -> MsgAwaitReply         <$  decodeNull
      3 -> uncurry MsgIntersectImproved <$> decode
      4 -> MsgIntersectUnchanged <$  decodeNull
      _ -> fail "MsgProducer unexpected tag"
