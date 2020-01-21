{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Network.Connections.Types
  ( Provenance (..)
  , Initiated (..)
  , Decision (..)
  , Connections (..)
  , Resource (..)

  , CannotReject

  , Client
  , Server
  , runClientWith
  , runServerWith
  ) where

data Provenance = Remote | Local
  deriving (Show)

data Initiated (provenance :: Provenance) where
  Incoming :: Initiated Remote
  Outgoing :: Initiated Local

deriving instance Show (Initiated provenance)

data Decision (provenance :: Provenance) reject accept where
  Rejected :: reject provenance -> Decision provenance reject accept
  Accepted :: accept provenance -> Decision provenance reject accept
  deriving (Show)

-- | Useful type with kind `Provenance -> Type` to express that rejection is
-- not possible.
data CannotReject (provenance :: Provenance) where

data Resource provenance m r where
  -- | An existing resource, with a close action.
  -- Corresponds to remotely-initiated, incoming connections.
  Existing ::   r -> m ()        -> Resource Remote m r
  -- | A new resource, with create and close actions (a bracket).
  -- Corresponds to locally-initiated, outgoing connections.
  New      :: m r -> (r -> m ()) -> Resource Local m r

-- | Connections identified by `id`, carried by `socket`, supported by `m`.
--
-- For `Existing` resources, if the decision is `Rejected`, or if the call
-- throws an exception (`m` is IO-capable), then the caller is responsible for
-- closing the resource. Otherwise, the `Connections` term must ensure it is
-- closed appropriately.
--
-- For `New` resources, the caller does not create the resource, so the
-- `Connections` term is responsible for creating and closing it appropriately.
--
-- Note on exceptions: in case `m` can throw exceptions, there's some
-- ambiguity about when it's a good idea to do so, because it's not clear
-- whether an exception should be caught and used to give a `Rejected` value,
-- or simply ignore/re-thrown.
--
-- If the resource acquisition of a `New` connection throws an exception, that
-- should be re-thrown. This is consistent with bracketing of the acquire and
-- release fields of the `New` constructor, which also re-throws the exception.
--
-- TODO may need a way to check / be notified when there is an incoming
-- connection at a given identifier.
-- Could just put that on the concurrent connections: an `STM (Map id handle)`.
-- When you get the handle back, the underlying connection and its handler
-- thread may of course have died, but that's always a possibility even if
-- you `include`. 
--
data Connections id socket request reject accept m = Connections
  { include :: forall provenance .
               id
            -> Resource provenance m socket
            -> request provenance
            -> m (Decision provenance reject accept)
  }

-- Client and Server types take any suitable Connections include callback,
-- specialized to Outgoing or Incoming, and use it to get a decision, for any
-- accept or reject type.

type Client addr sock m request = forall accept reject .
     (addr -> m sock -> (sock -> m ()) -> request Local -> m (Decision Local accept reject))
  -> m (Decision Local accept reject)

type Server addr sock m request = forall accept reject .
     (addr -> sock -> m () -> request Remote -> m (Decision Remote accept reject))
  -> m (Decision Remote accept reject)

runClientWith
  :: Connections addr sock request reject accept m
  -> Client addr sock m request
  -> m (Decision Local reject accept)
runClientWith connections client = client $ \addr sopen sclose req ->
  include connections addr (New sopen sclose) req

runServerWith
  :: Connections addr sock request reject accept m
  -> Server addr sock m request
  -> m (Decision Remote reject accept)
runServerWith connections server = server $ \addr s sclose req ->
  include connections addr (Existing s sclose) req
