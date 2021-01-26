{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Examples where

import           Ouroboros.Network.Protocol.LocalStateQuery.Client
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..))


--
-- Example client
--

-- | An example 'LocalStateQueryClient', which, for each point in the given
-- list, acquires the state for that point, and if that succeeds, returns the
-- result for the corresponding query. When the state could not be acquired,
-- the 'AcquireFailure' is returned instead of the query results.
--
localStateQueryClient
  :: forall block point query result m.
     Applicative m
  => [(Maybe point, query result)]
  -> LocalStateQueryClient block point query m
                           [(Maybe point, Either AcquireFailure result)]
localStateQueryClient = LocalStateQueryClient . pure . goIdle []
  where
    goIdle
      :: [(Maybe point, Either AcquireFailure result)]  -- ^ Accumulator
      -> [(Maybe point, query result)]                  -- ^ Remainder
      -> ClientStIdle block point query m
                      [(Maybe point, Either AcquireFailure result)]
    goIdle acc []              = SendMsgDone $ reverse acc
    goIdle acc ((pt, q):ptqs') = SendMsgAcquire pt $ goAcquiring acc pt q ptqs'

    goAcquiring
      :: [(Maybe point, Either AcquireFailure result)]  -- ^ Accumulator
      -> Maybe point
      -> query result
      -> [(Maybe point, query result)]                  -- ^ Remainder
      -> ClientStAcquiring block point query m
                           [(Maybe point, Either AcquireFailure result)]
    goAcquiring acc pt q ptqss' = ClientStAcquiring {
        recvMsgAcquired = goQuery q $ \r -> goAcquired ((pt, Right r):acc) ptqss'
      , recvMsgFailure  = \failure -> pure $ goIdle ((pt, Left failure):acc) ptqss'
      }

    goAcquired
      :: [(Maybe point, Either AcquireFailure result)]
      -> [(Maybe point, query result)]   -- ^ Remainder
      -> ClientStAcquired block point query m
                          [(Maybe point, Either AcquireFailure result)]
    goAcquired acc [] = SendMsgRelease $ pure $ SendMsgDone $ reverse acc
    goAcquired acc ((pt, qs):ptqss') = SendMsgReAcquire pt $
      goAcquiring acc pt qs ptqss'

    goQuery
      :: forall a.
         query result
      -> (result -> ClientStAcquired block point query m a)
         -- ^ Continuation
      -> ClientStAcquired block point query m a
    goQuery q k = SendMsgQuery q $ ClientStQuerying $ \r -> pure $ k r

--
-- Example server
--

-- | An example 'LocalStateQueryServer'. The first function is called to
-- acquire a @state@, after which the second will be used to query the state.
--
localStateQueryServer
  :: forall block point query m state. Applicative m
  => (Maybe point -> Either AcquireFailure state)
  -> (forall result. state -> query result -> result)
  -> LocalStateQueryServer block point query m ()
localStateQueryServer acquire answer =
    LocalStateQueryServer $ pure goIdle
  where
    goIdle :: ServerStIdle block point query m ()
    goIdle = ServerStIdle {
        recvMsgAcquire = goAcquiring
      , recvMsgDone    = pure ()
      }

    goAcquiring :: Maybe point -> m (ServerStAcquiring block point query m ())
    goAcquiring pt = pure $ case acquire pt of
      Left failure -> SendMsgFailure failure goIdle
      Right state  -> SendMsgAcquired $ goAcquired state

    goAcquired :: state -> ServerStAcquired block point query m ()
    goAcquired state = ServerStAcquired {
        recvMsgQuery     = \query ->
          pure $ SendMsgResult (answer state query) $ goAcquired state
      , recvMsgReAcquire = goAcquiring
      , recvMsgRelease   = pure goIdle
      }
