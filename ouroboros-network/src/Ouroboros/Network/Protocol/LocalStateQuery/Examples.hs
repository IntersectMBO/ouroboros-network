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
-- results for all the corresponding queries. When the state could not be
-- acquired, the 'AcquireFailure' is returned instead of the query results.
--
localStateQueryClient
  :: forall block query result m. Applicative m
  => [(Point block, [query])]
  -> LocalStateQueryClient block query result m
                           [(Point block, Either AcquireFailure [result])]
localStateQueryClient = LocalStateQueryClient . goIdle []
  where
    goIdle
      :: [(Point block, Either AcquireFailure [result])]  -- ^ Accumulator
      -> [(Point block, [query])]  -- ^ Remainder
      -> ClientStIdle block query result m
                      [(Point block, Either AcquireFailure [result])]
    goIdle acc []                = SendMsgDone $ reverse acc
    goIdle acc ((pt, qs):ptqss') = SendMsgAcquire pt $
      goAcquiring acc pt qs ptqss'

    goAcquiring
      :: [(Point block, Either AcquireFailure [result])]  -- ^ Accumulator
      -> Point block
      -> [query]
      -> [(Point block, [query])]  -- ^ Remainder
      -> ClientStAcquiring block query result m
                           [(Point block, Either AcquireFailure [result])]
    goAcquiring acc pt qs ptqss' = ClientStAcquiring {
        recvMsgAcquired = goQuery [] qs $ \rs -> goAcquired ((pt, Right rs):acc) ptqss'
      , recvMsgFailure  = \failure -> pure $ goIdle ((pt, Left failure):acc) ptqss'
      }

    goAcquired
      :: [(Point block, Either AcquireFailure [result])]
      -> [(Point block, [query])]   -- ^ Remainder
      -> ClientStAcquired block query result m
                          [(Point block, Either AcquireFailure [result])]
    goAcquired acc [] = SendMsgRelease $ SendMsgDone $ reverse acc
    goAcquired acc ((pt, qs):ptqss') = SendMsgReAcquire pt $
      goAcquiring acc pt qs ptqss'

    goQuery
      :: forall a.
        [result]  -- ^ Result accumulator
      -> [query]
      -> ([result] -> ClientStAcquired block query result m a)
         -- ^ Continuation
      -> ClientStAcquired block query result m a
    goQuery acc []      k = k (reverse acc)
    goQuery acc (q:qs') k = SendMsgQuery q $ ClientStQuerying $ \r ->
      pure $ goQuery (r:acc) qs' k

--
-- Example server
--

-- | An example 'LocalStateQueryServer'. The first function is called to
-- acquire a @state@, after which the second will be used to query the state.
--
localStateQueryServer
  :: forall block query result m state. Applicative m
  => (Point block -> Either AcquireFailure state)
  -> (state -> query -> result)
  -> LocalStateQueryServer block query result m ()
localStateQueryServer acquire answer =
    LocalStateQueryServer $ pure goIdle
  where
    goIdle :: ServerStIdle block query result m ()
    goIdle = ServerStIdle {
        recvMsgAcquire = goAcquiring
      , recvMsgDone    = pure ()
      }

    goAcquiring :: Point block -> m (ServerStAcquiring block query result m ())
    goAcquiring pt = pure $ case acquire pt of
      Left failure -> SendMsgFailure failure goIdle
      Right state  -> SendMsgAcquired $ goAcquired state

    goAcquired :: state -> ServerStAcquired block query result m ()
    goAcquired state = ServerStAcquired {
        recvMsgQuery     = \query ->
          pure $ SendMsgResult (answer state query) $ goAcquired state
      , recvMsgReAcquire = goAcquiring
      , recvMsgRelease   = pure goIdle
      }
