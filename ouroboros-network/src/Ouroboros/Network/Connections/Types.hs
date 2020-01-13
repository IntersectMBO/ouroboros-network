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
data Connections id socket reject accept m = Connections
  { include :: forall provenance . id -> Resource provenance m socket -> m (Decision provenance reject accept) }

type Client addr sock reject conn m t = (addr -> m sock -> (sock -> m ()) -> m (Decision Outgoing reject conn)) -> m t

type Server addr sock reject conn m t = (addr -> sock   -> m ()           -> m (Decision Incoming reject conn)) -> m t

runClientWith
  :: Connections addr sock reject accept m
  -> Client addr sock reject accept m t
  -> m t
runClientWith connections client = client $ \addr sopen sclose ->
  include connections addr (New sopen sclose)

runServerWith
  :: Connections addr sock reject accept m
  -> Server addr sock reject accept m t
  -> m t
runServerWith connections server = server $ \addr s sclose ->
  include connections addr (Existing s sclose)
