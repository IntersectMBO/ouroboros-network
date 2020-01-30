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
  , UnitRequest (..)
  , LocalOnlyRequest (..)
  , RemoteOnlyRequest (..)

  , contramapRequest
  , mapResult
  , mapDecision

  , Client
  , Server
  , runClientWith
  , runServerWith
  ) where

-- | Used as a data kind to distinguish between incoming (remotely-initiated)
-- and outgoing (locally-initiated) connections.
data Provenance = Remote | Local
  deriving (Show)

-- | Singleton type for `Provenance`, with names that hopefully read well.
data Initiated (provenance :: Provenance) where
  Incoming :: Initiated Remote
  Outgoing :: Initiated Local

deriving instance Show (Initiated provenance)

-- | Generic abstract type for the decision taken by a `Connections` term:
-- a connection is either rejected or accepted, with a value of a
-- `Provenance`-indexed type. Particular `Connections` will specialize
-- these to domain-specific types.
data Decision (provenance :: Provenance) reject accept where
  Rejected :: reject provenance -> Decision provenance reject accept
  Accepted :: accept provenance -> Decision provenance reject accept
  deriving (Show)

mapDecision
  :: (forall provenance' . reject1 provenance' -> reject2 provenance')
  -> (forall provenance' . accept1 provenance' -> accept2 provenance')
  -> Decision provenance reject1 accept1
  -> Decision provenance reject2 accept2
mapDecision frej facc decision = case decision of
  Rejected rej -> Rejected (frej rej)
  Accepted acc -> Accepted (facc acc)

-- | Useful type with kind `Provenance -> Type` to express that rejection is
-- not possible.
data CannotReject (provenance :: Provenance)

-- | Useful for concurrent connections manager which have only one request
-- type that carries no information.
data UnitRequest (p :: Provenance) where
  UnitRequest :: UnitRequest p

data LocalOnlyRequest (p :: Provenance) where
  LocalOnlyRequest :: LocalOnlyRequest Local

data RemoteOnlyRequest (p :: Provenance) where
  RemoteOnlyRequest :: RemoteOnlyRequest Remote

-- | Description of a resource: something that can be acquired in the bracket
-- pattern, or something that already exists but can be closed.
--
-- This is indexed by `Provenance` because incoming and outgoing resources
-- have different contracts: incoming ones already exist but can be closed;
-- outgoing ones _may_ be created but may also already exist.
data Resource provenance m r where
  -- | An existing resource, with a close action.
  -- Corresponds to remotely-initiated, incoming connections.
  Existing ::   r -> m ()        -> Resource Remote m r
  -- | A new resource, with create and close actions (a bracket).
  -- Corresponds to locally-initiated, outgoing connections.
  New      :: m r -> (r -> m ()) -> Resource Local m r

-- | Connections identified by `id`, carried by `resource`, supported by `m`.
-- `resource` might be a file handle / socket, STM queue, etc. anything that
-- can transport data in `m`.
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
newtype Connections id resource request reject accept m = Connections
  { include :: forall provenance .
               id
            -> Resource provenance m resource
            -> request provenance
            -> m (Decision provenance reject accept)
  }

-- | Connections is a contravariant functor in request, but sadly it's not the
-- final type parameter so we can't give a Contravariant instance. Also, the
-- request type is not of kind `Type` so it wouldn't work anyway.
contramapRequest
  :: (forall provenance . req1 provenance -> req2 provenance)
  -> Connections id resource req2 reject accept m
  -> Connections id resource req1 reject accept m
contramapRequest f connections = connections
  { include = \ident res req -> include connections ident res (f req) }

mapResult
  :: ( Functor m )
  => (forall provenance . reject1 provenance -> reject2 provenance)
  -> (forall provenance . accept1 provenance -> accept2 provenance)
  -> Connections id resource req reject1 accept1 m
  -> Connections id resource req reject2 accept2 m
mapResult frej facc connections = connections
  { include = \ident res req -> fmap (mapDecision frej facc) (include connections ident res req) }

-- Client and Server types take any suitable Connections include callback,
-- specialized to Outgoing or Incoming, and use it to get a decision, for any
-- accept or reject type.

-- | A `Client` takes a continuation that is derived from a `Connections`
-- (see `runClientWith`) and uses it to get a decision parameteric in accept
-- and reject.
type Client addr sock m request = forall accept reject .
     (addr -> m sock -> (sock -> m ()) -> request Local -> m (Decision Local accept reject))
  -> m (Decision Local accept reject)

-- | A `Server` takes a continuation that is derived from a `Connections`
-- (see `runServerWith`) and uses it to get a decision parameteric in accept
-- and reject.
type Server addr sock m request = forall accept reject .
     (addr -> sock -> m () -> request Remote -> m (Decision Remote accept reject))
  -> m (Decision Remote accept reject)

-- | Run a `Client` against a given `Connections`.
runClientWith
  :: Connections addr sock request reject accept m
  -> Client addr sock m request
  -> m (Decision Local reject accept)
runClientWith connections client = client $ \addr sopen sclose req ->
  include connections addr (New sopen sclose) req

-- | Run a `Server` against a given `Connections`.
--
-- This may not look like a server, because it gives back a decision and then
-- stops. For a more conventional looping server, see examples in the
-- `Ouroboros.Network.Connections.Socket.Server` module. It uses `runServerWith`
-- in a loop.
runServerWith
  :: Connections addr sock request reject accept m
  -> Server addr sock m request
  -> m (Decision Remote reject accept)
runServerWith connections server = server $ \addr s sclose req ->
  include connections addr (Existing s sclose) req
