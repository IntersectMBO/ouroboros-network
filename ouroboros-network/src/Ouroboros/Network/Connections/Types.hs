{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Ouroboros.Network.Connections.Types
  ( Provenance (..)
  , Initiated (..)
  , Resource (..)
  , AcquiredResource (..)
  , Decision (..)
  , Connections (..)

  , Outcome (..)
  , includeResource

  , CannotReject
  , UnitRequest (..)
  , LocalOnlyRequest (..)
  , RemoteOnlyRequest (..)

  , contramapRequest
  , mapResult
  , mapDecision
  ) where

import Data.Void (Void, absurd)

-- | Used as a data kind to distinguish between incoming (remotely-initiated)
-- and outgoing (locally-initiated) connections.
data Provenance = Remote | Local
  deriving (Show)

-- | Singleton type for `Provenance`, with names that hopefully read well.
data Initiated (provenance :: Provenance) where
  Incoming :: Initiated Remote
  Outgoing :: Initiated Local

deriving instance Show (Initiated provenance)

-- | The decision taken by a `Connections` term given an acquired resource.
-- It either accepts the connection, taking ownership of the resource, or it
-- rejects the connection, giving ownership back to the caller.
-- Domain-specific reject and accept types are used.
data Decision (provenance :: Provenance) reject accept resource m where
  -- | Resource acquired but rejected for domain-specific reasons.
  -- The resource is returned, indicating that the caller is now responsible for
  -- it. This is the same for locally- and remotely-initiated cases.
  Rejected    :: reject provenance
              -> AcquiredResource resource m
              -> Decision provenance reject accept resource m
  -- | Resource acquired and accepted. The connections term now owns the
  -- resource and is responsible for closing it.
  Accepted    :: accept provenance
              -> Decision provenance reject accept resource m

mapDecision
  :: (forall provenance' . reject1 provenance' -> reject2 provenance')
  -> (forall provenance' . accept1 provenance' -> accept2 provenance')
  -> Decision provenance reject1 accept1 resource m
  -> Decision provenance reject2 accept2 resource m
mapDecision frej facc decision = case decision of
  Rejected rej ares -> Rejected (frej rej) ares
  Accepted acc      -> Accepted (facc acc)

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

data AcquiredResource resource m = AcquiredResource
  { resourceHandle :: resource
  , closeResource  :: m ()
  }

-- | Description of a resource: something that can be acquired in the bracket
-- pattern, or something that already exists but can be closed.
--
-- This is indexed by `Provenance` because incoming and outgoing resources
-- have different contracts.
--
-- Incoming resources are by definition created in response to an external
-- actor (a remote machine, for instance), and so this program has no control
-- over when they are created. At the point of inclusion into a Connections
-- term, the resource already exists.
--
-- Outgoing resources are by definition created by, at the discretion of, this
-- program, but there _may_ already exist a resource which is suitable for
-- carrying the desired connection (according to the identifier/address given).
-- So the locally-initiated case does not give a resource, but a way to
-- create a resource (with possibly error) which may or may not actually be
-- called.
data Resource provenance err resource m where
  -- | An existing resource, with a close action.
  -- Corresponds to remotely-initiated, incoming connections.
  -- There can be no acquire error here, so we specialize it to Void.
  Existing :: AcquiredResource resource m
           -> Resource Remote Void resource m
  -- | A new resource, with create and close actions.
  -- Corresponds to locally-initiated, outgoing connections.
  -- Unlike the 'Existing' case, this one can give an error. For that case the
  -- error can also come up, but it comes up _prior_ to even dealing with a
  -- 'Connections' term.
  New      :: m (Either err (AcquiredResource resource m))
           -> Resource Local err resource m

data Outcome (provenance :: Provenance) err reject accept resource m where
  -- | Could not acquire the resource.
  -- From a wholistic point of view, this can indeed happen even for
  -- remotely-initiated connections, as there must be some system which is
  -- listening for them and which can fail to acquire a new one. This case does
  -- however appear to be asymmetric with the locally-initiated case, in which
  -- the "not-acquired" error comes out of the decision of the Connections term.
  NotAcquired :: err
              -> Outcome provenance err reject accept resource m
  Acquired    :: Decision provenance reject accept resource m
              -> Outcome provenance err reject accept resource m

-- | Connections identified by `id`, carried by `resource`, supported by `m`.
-- `resource` might be a file handle / socket, STM queue, etc. anything that
-- can transport data in `m`.
--
-- TODO may need a way to check / be notified when there is an incoming
-- connection at a given identifier.
-- Could just put that on the concurrent connections: an `STM (Map id handle)`.
-- When you get the handle back, the underlying connection and its handler
-- thread may of course have died, but that's always a possibility even if
-- you `include`. 
newtype Connections id resource request reject accept m = Connections
  { -- Since provenance ~ Remote implies err ~ Void (by the Resource GADT),
    -- a Connections term may never give a Left for a remotely-initiated
    -- resource, i.e. the Outcome shall always be Acquired (see
    -- includeResource)
    include :: forall err provenance .
               id
            -> Resource provenance err resource m
            -> request provenance
            -> m (Either err (Decision provenance reject accept resource m))
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
  { include = \ident res req -> (fmap . fmap)
      (mapDecision frej facc)
      (include connections ident res req)
  }

includeResource
  :: ( Functor m )
  => Connections id resource request reject accept m
  -> id
  -> Resource provenance err resource m
  -> request provenance
  -> m (Outcome provenance err reject accept resource m)
includeResource connections ident resource request =
  fmap mkOutcome (include connections ident resource request)
  where
  mkOutcome errOrDecision = case (resource, errOrDecision) of
    (New _,      Right decision) -> Acquired    decision
    (Existing _, Right decision) -> Acquired    decision
    (New _,      Left err)       -> NotAcquired err
    (Existing _, Left  void)     -> absurd void
