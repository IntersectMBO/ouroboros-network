{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Examples where

import           Ouroboros.Network.Block (Point)

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
  :: forall block query result m. Applicative m
  => [(Point block, query result)]
  -> LocalStateQueryClient block query m
                           [(Point block, Either AcquireFailure result)]
localStateQueryClient = LocalStateQueryClient . pure . goIdle []
  where
    goIdle
      :: [(Point block, Either AcquireFailure result)]  -- ^ Accumulator
      -> [(Point block, query result)]  -- ^ Remainder
      -> ClientStIdle block query m
                      [(Point block, Either AcquireFailure result)]
    goIdle acc []              = SendMsgDone $ reverse acc
    goIdle acc ((pt, q):ptqs') = SendMsgAcquire pt $ goAcquiring acc pt q ptqs'

    goAcquiring
      :: [(Point block, Either AcquireFailure result)]  -- ^ Accumulator
      -> Point block
      -> query result
      -> [(Point block, query result)]  -- ^ Remainder
      -> ClientStAcquiring block query m
                           [(Point block, Either AcquireFailure result)]
    goAcquiring acc pt q ptqss' = ClientStAcquiring {
        recvMsgAcquired = goQuery q $ \r -> goAcquired ((pt, Right r):acc) ptqss'
      , recvMsgFailure  = \failure -> pure $ goIdle ((pt, Left failure):acc) ptqss'
      }

    goAcquired
      :: [(Point block, Either AcquireFailure result)]
      -> [(Point block, query result)]   -- ^ Remainder
      -> ClientStAcquired block query m
                          [(Point block, Either AcquireFailure result)]
    goAcquired acc [] = SendMsgRelease $ SendMsgDone $ reverse acc
    goAcquired acc ((pt, qs):ptqss') = SendMsgReAcquire pt $
      goAcquiring acc pt qs ptqss'

    goQuery
      :: forall a.
         query result
      -> (result -> ClientStAcquired block query m a)
         -- ^ Continuation
      -> ClientStAcquired block query m a
    goQuery q k = SendMsgQuery q $ ClientStQuerying $ \r -> pure $ k r

--
-- Example server
--

-- | An example 'LocalStateQueryServer'. The first function is called to
-- acquire a @state@, after which the second will be used to query the state.
--
localStateQueryServer
  :: forall block query m state. Applicative m
  => (Point block -> Either AcquireFailure state)
  -> (forall result. state -> query result -> result)
  -> LocalStateQueryServer block query m ()
localStateQueryServer acquire answer =
    LocalStateQueryServer $ pure goIdle
  where
    goIdle :: ServerStIdle block query m ()
    goIdle = ServerStIdle {
        recvMsgAcquire = goAcquiring
      , recvMsgDone    = pure ()
      }

    goAcquiring :: Point block -> m (ServerStAcquiring block query m ())
    goAcquiring pt = pure $ case acquire pt of
      Left failure -> SendMsgFailure failure goIdle
      Right state  -> SendMsgAcquired $ goAcquired state

    goAcquired :: state -> ServerStAcquired block query m ()
    goAcquired state = ServerStAcquired {
        recvMsgQuery     = \query ->
          pure $ SendMsgResult (answer state query) $ goAcquired state
      , recvMsgReAcquire = goAcquiring
      , recvMsgRelease   = pure goIdle
      }
