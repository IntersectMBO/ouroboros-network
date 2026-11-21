{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Opt-in element progress and functional observations around the existing
-- annotated driver. Protocol codecs supply complete-element boundaries;
-- consumers supply one immutable receive scope per reply.
-- Rationale (repository-root path): @docs/progress-driver.md@.
module Ouroboros.Network.Driver.Progress
  ( WithByteSpan (..)
  , ProgressLimits (..)
  , InputObservation (..)
  , ProgressEvent (..)
  , MessageReceipt (..)
  , ReceiveScope (..)
  , ProgressEmitter
  , ProgressFailure (..)
  , ProgressState
  , progressTrailingBytes
  , progressDriverWithLimits
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Class.MonadFork (MonadFork, yield)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Sequence (Seq, ViewL (..), (|>))
import Data.Sequence qualified as Seq
import Data.Word (Word32, Word64)

import Network.Mux.Timeout (TimeoutFn)
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver (Driver (..))

import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.Protocol.Codec.Utils (WithByteSpan (..))
import Ouroboros.Network.Util.ShowProxy

-- | Receive-context bounds checked before element callbacks. Message bounds
-- also apply to replies without element progress. Zero elements disables it.
-- Rationale: @docs/progress-driver.md#bounds-and-input-ownership@.
data ProgressLimits = ProgressLimits
  { progressMessageBytes :: !Word32 -- ^ Maximum consumed message bytes.
  , progressElementBytes :: !Word32 -- ^ Maximum encoded bytes per element.
  , progressElements     :: !Word32 -- ^ Maximum number of element callbacks.
  } deriving (Eq, Show)

-- | An input span supplied to this response, including retained trailing input.
-- Its time is observed after the original channel read, before local processing.
data InputObservation = InputObservation
  { inputBytes      :: !Word32 -- ^ Nonzero number of bytes in this span.
  , inputObservedAt :: !Time    -- ^ Original monotonic read-return observation.
  } deriving (Eq, Show)

-- | One complete element whose limits and input boundary have been checked.
data ProgressEvent element = ProgressEvent
  { progressIndex      :: !Word32 -- ^ Zero-based position in this response.
  , progressObservedAt :: !Time    -- ^ Read observation of its final byte.
  , progressElement    :: !(WithByteSpan element)
    -- ^ Candidate and original encoded-byte span relative to the message start.
  } deriving (Eq, Show)

-- | Terminal wire boundary of the original message, independent of when its
-- decoder, tracer or consumer subsequently finishes processing it.
data MessageReceipt = MessageReceipt
  { receiptBytes      :: !Word32 -- ^ Consumed message bytes, excluding trailing input.
  , receiptObservedAt :: !Time    -- ^ Read observation of the final consumed byte.
  } deriving (Eq, Show)

-- | Immutable callbacks and bounds captured before decoding one response.
-- Callbacks run synchronously under masking, must be bounded, and must not wait
-- for storage. Accepted elements remain consumer-owned after any later failure.
-- Rationale: @docs/progress-driver.md#receive-lifetime@.
data ReceiveScope m element = ReceiveScope
  { receiveLimits        :: !ProgressLimits
    -- ^ Request-specific encoded-byte and element bounds.
  , receiveStarted       :: m ()
    -- ^ Begin this pinned response before reporting retained or newly read input.
  , receiveInputStarting :: m ()
    -- ^ Protect publication after nonempty input is available, before taking
    -- its timestamp. Never invoked while the channel read is blocked.
  , receiveInput         :: InputObservation -> m ()
    -- ^ Publish a nonempty input span before decoding it; never re-stamp trailing input.
  , receiveAwaitInput    :: m ()
    -- ^ Decoding exhausted available input. Settle local validation gates before waiting.
  , receiveProgress      :: ProgressEvent element -> m ()
    -- ^ Validate and hand off one complete candidate without waiting for durability.
  , receiveComplete      :: MessageReceipt -> Bool -> m ()
    -- ^ Accept the terminal boundary and report nonempty retained input for the
    -- next reply before returning to typed settlement. No element ingestion.
  , receiveAbort         :: SomeException -> m ()
    -- ^ Clean up once on unsuccessful receive, preserving previously accepted elements.
  }

-- | Codec-supplied complete-element event. Offsets must reflect actual parsing,
-- increase monotonically and refer only to already supplied input.
type ProgressEmitter m element = WithByteSpan element -> m ()

-- | Invalid codec boundaries or exceeded opt-in resource bounds. Ordinary
-- decoder and fatal wire-limit exceptions retain their existing exception types.
data ProgressFailure
  = InvalidProgressRange !Int64 !Int64
  | ProgressBeyondInput !Word64 !Word64
  | ProgressMessageLimit !Word64 !Word64
  | ProgressElementLimit !Word64 !Word64
  | ProgressCountLimit !Word32
  | ProgressInputLimit !Word64 !Word64
  | InvalidProgressTrailing
  | ProgressScopeClosed
  deriving (Eq, Show)

instance Exception ProgressFailure

-- | Opaque trailing bytes and the original observations covering them. Pass it
-- unchanged between receives on this driver. Use 'progressTrailingBytes' when
-- returning unused bytes to the ordinary mux runner.
data ProgressState = ProgressState !(Maybe BL.ByteString) !(Seq InputObservation)

-- | Unconsumed bytes for the ordinary channel/mux continuation boundary.
progressTrailingBytes :: ProgressState -> Maybe BL.ByteString
progressTrailingBytes (ProgressState trailing _) = trailing

-- | Span prefixes are discarded as checked progress advances. At most one span
-- per nonempty input byte is retained, plus the most recent endpoint time.
data InputState = InputState
  { suppliedBytes :: !Word64 -- ^ Widened total bytes supplied to this decoder.
  , cursorOffset  :: !Word64 -- ^ Processed prefix whose span metadata was discarded.
  , cursorTime    :: !(Maybe Time) -- ^ Observation at that prefix's last byte.
  , inputSpans    :: !(Seq InputObservation) -- ^ Observations for the remaining suffix.
  , elementCount  :: !Word32 -- ^ Checked callbacks; incremented only below the count cap.
  , inputPhase    :: !InputPhase -- ^ Receive ownership and terminal handoff phase.
  }

-- | A decoding scope, a decoded reply awaiting terminal acceptance, or permanent
-- closure. Escaped emitters cannot publish into decoded or closed scopes.
data InputPhase = Receiving | Decoded !MessageReceipt !ProgressState | Closed

-- | Compose 'annotatedDriverWithLimits' with checked progress and observations.
-- Physical reads must fit the positive input-chunk bound. The ordinary protocol
-- size limits must count encoded bytes. Scope acquisition runs masked to protect
-- the handoff into the driver's abort handler. Before it returns, the scope
-- provider must undo any partial acquisition on failure. Once it returns, the
-- driver owns abort cleanup. A provider that only reads an existing scope has
-- no additional partial acquisition to undo.
-- The codec builder receives one scope-bound emitter per response; it must
-- actually decode elements incrementally. Send encoding must not invoke it.
-- Rationale: @docs/progress-driver.md#adapter-and-codec@ and
-- @docs/progress-driver.md#receive-lifetime@.
progressDriverWithLimits
  :: forall ps (pr :: PeerRole) failure element m.
     ( MonadMask m, MonadSTM m, MonadThrow (STM m), MonadMonotonicTime m, MonadEvaluate m, MonadFork m
     , ShowProxy ps
     , forall (st :: ps) tok. tok ~ StateToken st => Show tok
     , NFData failure, Show failure
     )
  => Tracer m (TraceSendRecv ps)
  -> TimeoutFn m
  -> (ProgressEmitter m element -> AnnotatedCodec ps failure m BL.ByteString)
  -> ProtocolSizeLimits ps BL.ByteString
  -> ProtocolTimeLimits ps
  -> Word32
  -- ^ Maximum bytes in one physical input chunk, matching the ingress budget.
  -> (forall (st :: ps). StateToken st -> m (ReceiveScope m element))
  -- ^ Acquire the scope for the message this receiver is about to decode.
  -- A pipelined client installs its next FIFO request's scope before
  -- @ReceiverAwait@. Capture it once: callbacks must retain that request's
  -- original identity/descriptors while updating shared acquisition state.
  -- This adapter does not choose the request or look up a replacement scope.
  -- Rationale: @docs/progress-driver.md#receive-context-and-fifo-replies@.
  -> (AnyMessage ps -> Time -> m ())
  -- ^ Send-completion observation after ordinary channel send returns.
  -> Channel m BL.ByteString
  -> Driver ps pr ProgressState m
progressDriverWithLimits tracer timeoutFn buildCodec sizes times chunkLimit getScope sent channel = Driver
    { initialDState = ProgressState Nothing Seq.empty
    , sendMessage = \agency msg -> do
        let output = channel { send = \bytes -> mask $ \restore -> do
              restore (send channel bytes)
              observedAt <- getMonotonicTime
              sent (AnyMessage msg) observedAt }
            codec = buildCodec (\_ -> throwIO ProgressScopeClosed)
        sendMessage (annotatedDriverWithLimits tracer timeoutFn codec sizes times output :: Driver ps pr (Maybe BL.ByteString) m) agency msg
    , recvMessage = receive
    }
  where
    receive :: forall (st :: ps). (StateTokenI st, ActiveState st)
            => TheyHaveAgencyProof pr st -> ProgressState -> m (SomeMessage st, ProgressState)
    receive agency (ProgressState trailing spans) = mask $ \restore -> do
        let supplied = maybe 0 byteLength trailing
        -- Allocate private bookkeeping before taking scope ownership. Once
        -- getScope returns, only pure setup precedes the installed handler.
        stateVar <- newTVarIO (InputState supplied 0 Nothing spans 0 Receiving)
        scope <- getScope (stateToken @st)
        let configured = receiveLimits scope
            -- Take the minimum while widened; the configured Word32 bound
            -- proves the subsequent narrowing safe even for a native Word max.
            messageLimit = min (widen (progressMessageBytes configured))
              (fromIntegral (sizeLimitForState sizes (stateToken @st)))
            limits = configured { progressMessageBytes = fromIntegral messageLimit }
        let abort err = do
              owned <- atomically $ do
                state <- readTVar stateVar
                case inputPhase state of
                  Closed -> pure False
                  _      -> writeTVar stateVar closedState >> pure True
              when owned (receiveAbort scope err)
              throwIO (err :: SomeException)
            emit element@(WithByteSpan (_, rawStart, rawEnd)) = mask_ $ do
              (index, observedAt) <- atomically $ do
                state <- readTVar stateVar
                ensureReceiving state
                when (rawStart < 0 || rawEnd <= rawStart) $
                  throwSTM (InvalidProgressRange rawStart rawEnd)
                let start = fromIntegral rawStart
                    end = fromIntegral rawEnd
                when (start < cursorOffset state) $
                  throwSTM (InvalidProgressRange rawStart rawEnd)
                when (end > messageLimit) $
                  throwSTM (ProgressMessageLimit end messageLimit)
                when (end - start > widen (progressElementBytes limits)) $
                  throwSTM (ProgressElementLimit (end - start) (widen (progressElementBytes limits)))
                when (elementCount state >= progressElements limits) $
                  throwSTM (ProgressCountLimit (progressElements limits))
                (at, next) <- either throwSTM pure (advance end state)
                writeTVar stateVar next { elementCount = elementCount state + 1 }
                pure (elementCount state, at)
              receiveProgress scope (ProgressEvent index observedAt element)
            input = channel { recv = mask $ \restoreRead -> do
              receiveAwaitInput scope
              let readNonempty = do
                    bytes <- restoreRead (recv channel)
                    case bytes of
                      Just chunk | BL.null chunk -> do
                        -- A channel returning empty input without blocking must
                        -- not starve the fatal timeout or async cancellation.
                        restoreRead yield
                        readNonempty
                      Just chunk -> do
                        let len = byteLength chunk
                        when (len > widen chunkLimit) (throwIO (ProgressInputLimit len (widen chunkLimit)))
                        receiveInputStarting scope
                        observedAt <- getMonotonicTime
                        atomically $ do
                          state <- readTVar stateVar
                          ensureReceiving state
                          -- Both limits are Word32, and suppliedBytes has
                          -- already passed this widened sum bound. No Word64
                          -- addition here can approach overflow.
                          let total = suppliedBytes state + len
                              bound = min maximumCounter (messageLimit + widen chunkLimit)
                          when (total > bound) (throwSTM (ProgressInputLimit total bound))
                          writeTVar stateVar state
                            { suppliedBytes = total
                            , inputSpans = inputSpans state |> InputObservation (fromIntegral len) observedAt }
                        receiveInput scope (InputObservation (fromIntegral len) observedAt)
                        pure bytes
                      Nothing -> pure Nothing
              readNonempty }
            codec0 = buildCodec emit
            codec = codec0 { decode = \tok -> decode codec0 tok >>= finishStep stateVar limits }
            delegate :: Driver ps pr (Maybe BL.ByteString) m
            delegate = annotatedDriverWithLimits tracer timeoutFn codec sizes times input
        (do
          when (chunkLimit == 0 || supplied > widen chunkLimit || supplied > maximumCounter
                || sum (fmap (widen . inputBytes) spans) /= supplied) (throwIO InvalidProgressTrailing)
          receiveStarted scope
          mapM_ (receiveInput scope) spans
          (message, _) <- restore (recvMessage delegate agency trailing)
          (receipt, next) <- atomically $ do
            state <- readTVar stateVar
            case inputPhase state of
              Decoded receipt next -> pure (receipt, next)
              _                    -> throwSTM ProgressScopeClosed
          receiveComplete scope receipt (maybe False (not . BL.null) (progressTrailingBytes next))
          atomically $ writeTVar stateVar closedState
          pure (message, next)) `catch` abort
    -- Decorate decoder outcomes; the existing driver retains its input,
    -- annotation, timeout, tracing and final protocol-size-check loops.
    finishStep :: forall (st :: ps).
                  StrictTVar m InputState -> ProgressLimits
               -> DecodeStep BL.ByteString failure m (Annotator BL.ByteString st)
               -> m (DecodeStep BL.ByteString failure m (Annotator BL.ByteString st))
    finishStep stateVar limits step = case step of
      DecodePartial more -> pure $ DecodePartial $ \bytes ->
        more bytes >>= finishStep stateVar limits
      DecodeFail failure -> pure (DecodeFail failure)
      DecodeDone (Annotator annotate) rest -> do
        consumed <- atomically $ do
          state <- readTVar stateVar
          ensureReceiving state
          let remaining = maybe 0 byteLength rest
          when (remaining > suppliedBytes state) (throwSTM InvalidProgressTrailing)
          let consumed = suppliedBytes state - remaining
          when (consumed > widen (progressMessageBytes limits)) $
            throwSTM (ProgressMessageLimit consumed (widen (progressMessageBytes limits)))
          (at, next) <- either throwSTM pure (advance consumed state)
          let receipt = MessageReceipt (fromIntegral consumed) at
              trailingState = ProgressState rest (inputSpans next)
          writeTVar stateVar next { inputPhase = Decoded receipt trailingState }
          pure consumed
        pure $ DecodeDone (Annotator (annotate . BL.take (fromIntegral consumed))) rest

byteLength :: BL.ByteString -> Word64
byteLength = fromIntegral . BL.length

widen :: Word32 -> Word64
widen = fromIntegral

-- Counters must also fit the existing driver's Word and lazy bytestring Int64.
maximumCounter :: Word64
maximumCounter = min (fromIntegral (maxBound :: Word)) (fromIntegral (maxBound :: Int64))

closedState :: InputState
closedState = InputState 0 0 Nothing Seq.empty 0 Closed

ensureReceiving :: (MonadSTM m, MonadThrow (STM m)) => InputState -> STM m ()
ensureReceiving state = case inputPhase state of
  Receiving -> pure ()
  _         -> throwSTM ProgressScopeClosed

-- Resolve the final byte and discard its processed metadata prefix. Retaining
-- the endpoint permits a terminal coincident with the final element boundary.
advance :: Word64 -> InputState -> Either ProgressFailure (Time, InputState)
advance end state
  | end == cursorOffset state, Just at <- cursorTime state = Right (at, state)
  | end <= cursorOffset state || end > suppliedBytes state =
      Left (ProgressBeyondInput end (suppliedBytes state))
  | otherwise = do
      (at, rest) <- walk (end - cursorOffset state) (inputSpans state)
      pure (at, state { cursorOffset = end, cursorTime = Just at, inputSpans = rest })
  where
    walk n spans = case Seq.viewl spans of
      EmptyL -> Left InvalidProgressTrailing
      InputObservation len32 at :< rest ->
        let len = widen len32 in
        if n < len
          then Right (at, InputObservation (fromIntegral (len - n)) at Seq.<| rest)
          else if n == len then Right (at, rest) else walk (n - len) rest
