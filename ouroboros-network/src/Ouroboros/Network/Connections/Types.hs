{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Connections.Types
  ( Provenance (..)
  , Decision (..)
  , Connections (..)
  , Resource (..)

  , Client
  , Server
  , runClientWith
  , runServerWith
  ) where

data Provenance = Incoming | Outgoing
  deriving (Show)

data Decision (provenance :: Provenance) reject accept where
  Rejected :: reject provenance -> Decision provenance reject accept
  Accepted :: accept provenance -> Decision provenance reject accept
  deriving (Show)

data Resource provenance m r where
  -- | An existing resource, with a close action.
  -- Corresponds to remotely-initiated, incoming connections.
  Existing ::   r -> m ()        -> Resource Incoming m r
  -- | A new resource, with create and close actions (a bracket).
  -- Corresponds to locally-initiated, outgoing connections.
  New      :: m r -> (r -> m ()) -> Resource Outgoing m r

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
data Connections id socket reject accept m = Connections
  { include :: forall provenance . id -> Resource provenance m socket -> m (Decision provenance reject accept) }

-- Client and Server types take any suitable Connections include callback,
-- specialized to Outgoing or Incoming, and use it to get a decision, for any
-- accept or reject type.

type Client addr sock m = forall accept reject .
     (addr -> m sock -> (sock -> m ()) -> m (Decision Outgoing accept reject))
  -> m (Decision Outgoing accept reject)

type Server addr sock m = forall accept reject .
     (addr -> sock -> m () -> m (Decision Incoming accept reject))
  -> m (Decision Incoming accept reject)

runClientWith
  :: Connections addr sock reject accept m
  -> Client addr sock m
  -> m (Decision Outgoing reject accept)
runClientWith connections client = client $ \addr sopen sclose ->
  include connections addr (New sopen sclose)

runServerWith
  :: Connections addr sock reject accept m
  -> Server addr sock m
  -> m (Decision Incoming reject accept)
runServerWith connections server = server $ \addr s sclose ->
  include connections addr (Existing s sclose)
