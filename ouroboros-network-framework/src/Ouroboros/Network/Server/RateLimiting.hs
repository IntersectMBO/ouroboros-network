{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rage limiting of accepted connections
--
module Ouroboros.Network.Server.RateLimiting
  ( AcceptedConnectionsLimit (..)
  , runConnectionRateLimits
    -- * Tracing
  , AcceptConnectionsPolicyTrace (..)
  ) where

import           Control.Monad (when)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Tracer (Tracer, traceWith)

import           Data.Typeable (Typeable)
import           Data.Word
import           Text.Printf

import           Data.Aeson.Types


-- | Policy which governs how to limit the number of accepted connections.
--
data AcceptedConnectionsLimit = AcceptedConnectionsLimit {

    -- | Hard limit of accepted connections.
    --
    acceptedConnectionsHardLimit :: !Word32,

    -- | Soft limit of accepted connections.  If we are above this threshold,
    -- we will start rate limiting.
    --
    acceptedConnectionsSoftLimit :: !Word32,

    -- | Max delay for limiting accepted connections.  We use linear
    -- regression starting from 0 at the soft limit up to
    -- `acceptedConnectionDelay` at the hard limit.
    --
    acceptedConnectionsDelay     :: !DiffTime
  }
  deriving (Eq, Ord, Show)

instance ToJSON AcceptedConnectionsLimit where
  toJSON AcceptedConnectionsLimit
          { acceptedConnectionsHardLimit
          , acceptedConnectionsSoftLimit
          , acceptedConnectionsDelay
          } =
    object [ "AcceptedConnectionsLimit" .=
      object [ "hardLimit" .=
                  toJSON acceptedConnectionsHardLimit
             , "softLimit" .=
                  toJSON acceptedConnectionsSoftLimit
             , "delay" .=
                  toJSON acceptedConnectionsDelay
             ]
           ]

instance FromJSON AcceptedConnectionsLimit where
  parseJSON = withObject "AcceptedConnectionsLimit" $ \v ->
    AcceptedConnectionsLimit
      <$> v .: "hardLimit"
      <*> v .: "softLimit"
      <*> v .: "delay"

-- | Rate limiting instruction.
--
data RateLimitDelay =
      -- | no rate limiting
      --
      NoRateLimiting

      -- | We are above the soft limit, we delay accepting the next connection>
    | SoftDelay DiffTime

      -- | We are above the hard limit, wait until the number of connections
      -- drops below the given threshold (currently this is the hard limit,
      -- which means we keep `acceptedConnectionsHardLimit` number of
      -- connections, later we c could be configured to something between
      -- `acceptedConnesiontSoftLimit` and `acceptedConnectionsHardLimit`).
      --
    | HardLimit Word32


-- | Interpretation of the 'AcceptedConnectionsLimit' policy.
--
getRateLimitDecision :: Int
                     -- ^ number of served concurrent connections
                     -> AcceptedConnectionsLimit
                     -- ^ limits
                     -> RateLimitDelay
getRateLimitDecision numberOfConnections
                     AcceptedConnectionsLimit { acceptedConnectionsHardLimit
                                              , acceptedConnectionsSoftLimit
                                              , acceptedConnectionsDelay
                                              }
    -- below the soft limit we accept connections without any delay
    | numberOfConnections  < softLimit = NoRateLimiting

    -- above the hard limit will will wait until the number of connections drops
    -- below the soft limit
    | numberOfConnections >= hardLimit = HardLimit acceptedConnectionsHardLimit

    -- in between we scale the delay using linear regression.
    | otherwise =
        SoftDelay $
            fromIntegral (numberOfConnections - softLimit)
          * acceptedConnectionsDelay
          / fromIntegral ((hardLimit - softLimit) `max` 1)
  where
    hardLimit, softLimit :: Int
    hardLimit = fromIntegral acceptedConnectionsHardLimit
    softLimit = fromIntegral acceptedConnectionsSoftLimit


-- | Get the number of current connections, make decision based on
-- 'AcceptedConnectionsLimit' and execute it.
--
runConnectionRateLimits
    :: ( MonadSTM   m
       , MonadDelay m
       )
    => Tracer m AcceptConnectionsPolicyTrace
    -> STM m Int
    -> AcceptedConnectionsLimit
    -> m ()
runConnectionRateLimits tracer
                        numberOfConnectionsSTM
                        acceptedConnectionsLimit@AcceptedConnectionsLimit
                          { acceptedConnectionsDelay } = do
    numberOfConnections <- atomically numberOfConnectionsSTM
    case getRateLimitDecision numberOfConnections acceptedConnectionsLimit of

      NoRateLimiting  -> pure ()

      SoftDelay delay -> do
        traceWith tracer (ServerTraceAcceptConnectionRateLimiting delay numberOfConnections)
        threadDelay delay

      -- wait until the current number of connection drops below the limit, and
      -- wait at least 'acceptedConnectionsDelay'.  This is to avoid accepting
      -- the last connection to frequently if it fails almost immediately .
      HardLimit limit -> do
        traceWith tracer (ServerTraceAcceptConnectionHardLimit limit)
        start <- getMonotonicTime
        atomically $ do
          numberOfConnections' <- numberOfConnectionsSTM
          check (numberOfConnections' < fromIntegral limit)
        end <- getMonotonicTime
        let remainingDelay = acceptedConnectionsDelay - end `diffTime` start
        when (remainingDelay > 0)
          $ threadDelay remainingDelay
        numberOfConnections' <- atomically numberOfConnectionsSTM
        traceWith tracer $ ServerTraceAcceptConnectionResume numberOfConnections'


--
-- trace
--


-- | Trace for the 'AcceptConnectionsLimit' policy.
--
data AcceptConnectionsPolicyTrace
      = ServerTraceAcceptConnectionRateLimiting DiffTime Int
      | ServerTraceAcceptConnectionHardLimit Word32
      | ServerTraceAcceptConnectionResume Int
  deriving (Eq, Ord, Typeable)

instance Show AcceptConnectionsPolicyTrace where
    show (ServerTraceAcceptConnectionRateLimiting delay numberOfConnections) =
      printf
        "rate limiting accepting connections, delaying next accept for %s, currently serving %s connections"
        (show delay) (show numberOfConnections)
    show (ServerTraceAcceptConnectionHardLimit limit) =
      printf
        "hard rate limit reached, waiting until the number of connections drops below %s"
        (show limit)
    show (ServerTraceAcceptConnectionResume numberOfConnections) =
      printf "hard rate limit over, accepting connections again, currently serving %d connections"
        numberOfConnections
