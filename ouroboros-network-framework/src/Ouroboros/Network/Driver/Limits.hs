{-# LANGUAGE BangPatterns          #-}
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
{-# LANGUAGE UndecidableInstances  #-}

-- | Drivers for running 'Peer's.
--
module Ouroboros.Network.Driver.Limits
  ( -- * Limits
    ProtocolSizeLimits (..)
  , ProtocolTimeLimits (..)
  , ProtocolLimitFailure (..)
    -- * Normal peers
  , runPeerWithLimits
  , TraceSendRecv (..)
    -- * Driver utilities
  , driverWithLimits
  ) where

import           Data.Bifunctor

import           Control.Applicative (Alternative (..))
import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Exception (SomeAsyncException (..))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Tracer (Tracer (..), traceWith)

import           Network.Mux.Timeout
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Peer

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Simple
import           Ouroboros.Network.Protocol.Limits
import           Ouroboros.Network.Util.ShowProxy




data DState bytes =
    DState { dsTrailing      :: !(Maybe bytes)
           , dsRecvStartTime :: !(Maybe Time)
             -- ^ time when we started receiving last message.
           }


-- | An existential handle to an 'async' thread.
--
data SomeAsync m where
    SomeAsync :: forall m a. !(Async m a) -> SomeAsync m


driverWithLimits :: forall ps (pr :: PeerRole) failure bytes m.
                    ( MonadAsync m
                    , MonadMask  m
                    , MonadThrow (STM m)
                    , MonadTimer m
                    , Exception failure
                    , ShowProxy ps
                    , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
                    )
                 => Tracer m (TraceSendRecv ps)
                 -> TimeoutFn m
                 -> Codec ps failure m bytes
                 -> ProtocolSizeLimits ps bytes
                 -> ProtocolTimeLimits ps
                 -> Channel m bytes
                 -> m ( Driver ps pr bytes failure (DState bytes) m
                      , StrictTVar m (Maybe (SomeAsync m))
                      )
driverWithLimits tracer timeoutFn
                 Codec{encode, decode}
                 ProtocolSizeLimits{sizeLimitForState, dataSize}
                 ProtocolTimeLimits{timeLimitForState}
                 channel@Channel{send} = do
    v <- newTVarIO Nothing
    return ( Driver { sendMessage
                    , recvMessage
                    , tryRecvMessage
                    , recvMessageSTM = recvMessageSTM v
                    , startDState = DState Nothing Nothing
                    }
            , v
            )
  where
    sendMessage :: forall (st :: ps) (st' :: ps).
                   StateTokenI st
                => ActiveState st
                => ReflRelativeAgency (StateAgency st)
                                        WeHaveAgency
                                       (Relative pr (StateAgency st))
                -> Message ps st st'
                -> m ()
    sendMessage _ msg = do
      send (encode msg)
      traceWith tracer (TraceSendMsg (AnyMessage msg))


    recvMessage :: forall (st :: ps).
                   StateTokenI st
                => ActiveState st
                => ReflRelativeAgency (StateAgency st)
                                        TheyHaveAgency
                                       (Relative pr (StateAgency st))
                -> DriverState ps pr st bytes failure (DState bytes) m
                -> m (SomeMessage st, DState bytes)
    recvMessage _ state = do
      let tok = stateToken @st
          sizeLimit = sizeLimitForState tok
      t <- getMonotonicTime
      result <- case state of
        DecoderState decoder dstate ->
          case timeLimitFn t tok dstate of
            Nothing        ->  return Nothing
            Just timeLimit ->
              timeoutFn timeLimit $
                runDecoderWithLimit sizeLimit dataSize
                                    channel dstate decoder
        DriverState dstate ->
          case timeLimitFn t tok dstate of
            Nothing        -> return Nothing
            Just timeLimit ->
              timeoutFn timeLimit $
                runDecoderWithLimit sizeLimit dataSize
                                    channel dstate     =<< decode stateToken

        DriverStateSTM recvMsgSTM dstate ->
          case timeLimitFn t tok dstate of
            Nothing        -> return Nothing
            Just timeLimit ->
              timeoutFn timeLimit $
                Right <$> atomically recvMsgSTM

      case result of
        Just (Right (msg, dstate)) -> return (msg, dstate { dsRecvStartTime = Nothing })
        Just (Left (Just failure)) -> throwIO (DecoderFailure tok failure)
        Just (Left Nothing)        -> throwIO (ExceededSizeLimit tok)
        Nothing                    -> throwIO (ExceededTimeLimit tok)


    tryRecvMessage :: forall (st :: ps).
                      StateTokenI st
                   => ActiveState st
                   => ReflRelativeAgency (StateAgency st)
                                           TheyHaveAgency
                                          (Relative pr (StateAgency st))
                   -> DriverState ps pr st bytes failure (DState bytes) m
                   -> m (Either (DriverState ps pr st bytes failure (DState bytes) m)
                                (SomeMessage st, DState bytes))
    tryRecvMessage _ state = do
      let tok = stateToken @st
          sizeLimit = sizeLimitForState tok
      t <- getMonotonicTime
      result <-
        case state of
          DecoderState decoder dstate ->
            case timeLimitFn t tok dstate of
              Nothing        -> return Nothing
              Just timeLimit ->
                timeoutFn timeLimit $
                    swizzle (uncurry DecoderState)
                <$> tryRunDecoderWithLimit sizeLimit dataSize
                                           channel dstate decoder
          DriverState dstate ->
            case timeLimitFn t tok dstate of
              Nothing        -> return Nothing
              Just timeLimit ->
                timeoutFn timeLimit $
                    swizzle (uncurry DecoderState)
                <$>
                    (tryRunDecoderWithLimit sizeLimit dataSize
                                            channel dstate     =<< decode stateToken)

          DriverStateSTM recvMsgSTM dstate ->
            case timeLimitFn t tok dstate of
              Nothing        -> return Nothing
              Just timeLimit ->
                timeoutFn timeLimit $
                  atomically $
                    (Right . Right <$> recvMsgSTM)
                    `orElse`
                    pure (Right $ Left $ DriverStateSTM recvMsgSTM dstate)

      case result of
        Just (Right (Right x@(SomeMessage msg, _))) -> do
          traceWith tracer (TraceRecvMsg (AnyMessage msg))
          return (Right x)
        Just (Right (Left state'))  -> do
          let state'' = updateDState
                          (\dstate -> dstate { dsRecvStartTime =
                                               dsRecvStartTime dstate <|> Just t })
                          state'
          return $ Left state''
        Just (Left (Just failure)) -> throwIO (DecoderFailure tok failure)
        Just (Left Nothing)        -> throwIO (ExceededSizeLimit tok)
        Nothing                    -> throwIO (ExceededTimeLimit tok)


    -- TODO: we trace received message
    recvMessageSTM :: forall (st :: ps).
                      StateTokenI st
                   => ActiveState st
                   => StrictTVar m (Maybe (SomeAsync m))
                   -> ReflRelativeAgency (StateAgency st)
                                           TheyHaveAgency
                                          (Relative pr (StateAgency st))
                   -> DriverState ps pr st bytes failure (DState bytes) m
                   -> m (STM m (SomeMessage st, DState bytes))
    recvMessageSTM v _ (DecoderState decoder dstate) = mask_ $ do
      let tok = stateToken @st
          sizeLimit = sizeLimitForState tok

      -- stm timeout
      t <- getMonotonicTime
      timeoutSTM <- case timeLimitFn t tok dstate of
          Nothing -> return retry
          Just timeLimit -> do
            var <- registerDelay timeLimit
            return (LazySTM.readTVar var >>= check)

      hndl <- asyncWithUnmask $ \unmask ->
                unmask (runDecoderWithLimit sizeLimit dataSize channel
                                            dstate decoder)
                `finally`
                atomically (writeTVar v Nothing)
      atomically (writeTVar v (Just $! SomeAsync hndl))
      return (do r <-         (Just    <$> waitSTM hndl)
                     `orElse` (Nothing <$  timeoutSTM)
                 case r of
                   Just (Right result)        -> return result
                   Just (Left (Just failure)) -> throwSTM (DecoderFailure tok failure)
                   Just (Left  Nothing)       -> throwSTM (ExceededSizeLimit tok)
                   Nothing                    -> throwSTM (ExceededTimeLimit tok)
             )
    recvMessageSTM v _ (DriverState dstate) = mask_ $ do
      let tok = stateToken @st
          sizeLimit = sizeLimitForState tok

      -- stm timeout
      t <- getMonotonicTime
      timeoutSTM <- case timeLimitFn t tok dstate of
          Nothing -> return retry
          Just timeLimit -> do
            var <- registerDelay timeLimit
            return (LazySTM.readTVar var >>= check)

      hndl <- asyncWithUnmask $ \unmask ->
        unmask (runDecoderWithLimit sizeLimit dataSize channel
                                    dstate                     =<< decode stateToken)
        `finally`
        atomically (writeTVar v Nothing)
      atomically (writeTVar v (Just $! SomeAsync hndl))

      return (do r <-          (Just <$> waitSTM hndl)
                      `orElse` (Nothing <$ timeoutSTM)
                 writeTVar v Nothing
                 case r of
                   Just (Right result)        -> return result
                   Just (Left (Just failure)) -> throwSTM (DecoderFailure tok failure)
                   Just (Left  Nothing)       -> throwSTM (ExceededSizeLimit tok)
                   Nothing                    -> throwSTM (ExceededTimeLimit tok)
             )
    recvMessageSTM _ _ (DriverStateSTM stmRecvMessage _) =
      return stmRecvMessage


    timeLimitFn :: forall (st :: ps). ActiveState st
                => Time
                -> StateToken st
                -> DState bytes
                -> Maybe DiffTime
    timeLimitFn t tok dstate =
      case (timeLimitForState tok, dstate) of

        (Nothing, _)
          -> Just (-1)

        (Just timeLimit, DState _ (Just t'))
          -> let timeLimit' = timeLimit - t `diffTime` t'
                 -- if we are over time budget return Nothing.
             in if timeLimit' >= 0 then Just timeLimit' else Nothing

        (Just timeLimit, DState _ Nothing)
          -> Just timeLimit

    swizzle :: (x -> y)
            -> Either (Maybe failure)
                      (Either x (a, DState bytes))
            -> Either (Maybe failure)
                      (Either y (a, DState bytes))
    swizzle f = fmap (first f)


    updateDState :: (dstate -> dstate)
                 -> DriverState ps pr st bytes failure dstate m
                 -> DriverState ps pr st bytes failure dstate m
    updateDState f (DecoderState step  dstate) = DecoderState  step (f dstate)
    updateDState f (DriverState        dstate) = DriverState        (f dstate)
    updateDState f (DriverStateSTM stm dstate) = DriverStateSTM stm (f dstate)


runDecoderWithLimit
    :: forall m bytes failure a. Monad m
    => Word
    -- ^ message size limit
    -> (bytes -> Word)
    -- ^ byte size
    -> Channel m bytes
    -> DState bytes
    -> DecodeStep bytes failure m a
    -> m (Either (Maybe failure) (a, DState bytes))
runDecoderWithLimit limit size Channel{recv} =
    go 0
  where
    -- Our strategy here is as follows...
    --
    -- We of course want to enforce the maximum data limit, but we also want to
    -- detect and report when we exceed the limit rather than having it be
    -- misclassified as a generic decode error. For example if we simply limited
    -- the decoder input to the maximum size then the failure would be reported
    -- as an unexpected end of input, rather than that the size limit was
    -- exceeded.
    --
    -- So our strategy is to allow the last chunk of input to exceed the limit.
    -- This leaves just one special case: if the decoder finishes with that
    -- final chunk, we must check if it consumed too much of the final chunk.
    --
    go :: Word         -- ^ size of consumed input so far
       -> DState bytes -- ^ any trailing data
       -> DecodeStep bytes failure m a
       -> m (Either (Maybe failure) (a, DState bytes))

    go !sz dstate (DecodeDone x trailing)
      | let sz' = sz - maybe 0 size trailing
      , sz' > limit = return (Left Nothing)
      | otherwise   = return (Right (x, dstate { dsTrailing      = trailing
                                               , dsRecvStartTime = Nothing
                                               }))

    go !_ _  (DecodeFail failure) = return (Left (Just failure))

    go !sz dstate@DState { dsTrailing } (DecodePartial k)
      | sz > limit = return (Left Nothing)
      | otherwise  = case dsTrailing of
                       Nothing -> do mbs <- recv
                                     let !sz' = sz + maybe 0 size mbs
                                         dstate' = dstate { dsTrailing = Nothing
                                                          }
                                     go sz' dstate' =<< k mbs
                       Just bs -> do let sz' = sz + size bs
                                         dstate' = dstate { dsTrailing = Nothing
                                                          }
                                     go sz' dstate' =<< k (Just bs)

tryRunDecoderWithLimit
  :: forall m bytes failure a. Monad m
  => Word
  -- ^ message size limit
  -> (bytes -> Word)
  -- ^ byte size
  -> Channel m bytes
  -> DState bytes
  -> DecodeStep bytes failure m a
  -> m (Either (Maybe failure)
               (Either ( DecodeStep bytes failure m a
                       , DState bytes
                       )
                       (a, DState bytes)))
tryRunDecoderWithLimit limit size Channel{tryRecv} =
    go 0
  where
    go :: Word         -- ^ size of consumed input so far
       -> DState bytes -- ^ any trailing data
       -> DecodeStep bytes failure m a
       -> m (Either (Maybe failure)
                    (Either ( DecodeStep bytes failure m a
                            , DState bytes
                            )
                            (a, DState bytes)))

    go !sz dstate (DecodeDone x trailing)
      | let sz' = sz - maybe 0 size trailing
      , sz' > limit = return (Left Nothing)
      | otherwise   = return (Right $ Right (x, dstate { dsTrailing      = trailing
                                                       , dsRecvStartTime = Nothing
                                                       }))

    go !_ _  (DecodeFail failure) = return (Left (Just failure))

    go !sz dstate d@(DecodePartial k)
      | sz > limit = return (Left Nothing)
      | otherwise  = case dsTrailing dstate of
                       Nothing -> do r <- tryRecv
                                     case r of
                                       Nothing -> return (Right (Left (d, dstate)))
                                       Just mbs ->
                                         let !sz' = sz + maybe 0 size mbs
                                         in k mbs
                                            >>= go sz' dstate { dsTrailing = Nothing }
                       Just bs -> do let sz' = sz + size bs
                                     k (Just bs)
                                       >>= go sz' dstate { dsTrailing = Nothing }

runPeerWithLimits
  :: forall ps (st :: ps) pr (pl :: IsPipelined) failure bytes m a .
     ( Alternative (STM m)
     , MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadThrow (STM m)
     , MonadTimer m
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , ShowProxy ps
     , Exception failure
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure m bytes
  -> ProtocolSizeLimits ps bytes
  -> ProtocolTimeLimits ps
  -> Channel m bytes
  -> Peer ps pr pl Empty st m (STM m) a
  -> m (a, Maybe bytes)
runPeerWithLimits tracer codec slimits tlimits channel peer =
    withTimeoutSerial $ \timeoutFn -> do
      (driver, v) <- driverWithLimits tracer timeoutFn codec slimits tlimits channel
      (f <$> runPeerWithDriver driver peer)
        `catch` handleAsyncException v
  where
    f (a, DState { dsTrailing }) = (a, dsTrailing)

    handleAsyncException :: StrictTVar m (Maybe (SomeAsync m))
                         -> SomeAsyncException
                         -> m (a, Maybe bytes)
    handleAsyncException v e = do
      (mbHndl :: Maybe (SomeAsync m))
        <- (atomically :: forall x. STM m x -> m x)
           (readTVar v :: STM m (Maybe (SomeAsync m)))
      case mbHndl of
        Nothing               -> throwIO e
        Just (SomeAsync hndl) -> cancelWith hndl e
                              >> throwIO e
