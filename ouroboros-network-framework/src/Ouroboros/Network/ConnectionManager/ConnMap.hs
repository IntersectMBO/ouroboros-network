{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- Undecidable instances are need for 'Show' instance of 'ConnectionState'.
{-# LANGUAGE QuantifiedConstraints #-}

module Ouroboros.Network.ConnectionManager.ConnMap
  ( ConnMap (..)
  , LocalAddr (..)
  , toList
  , toMap
  , empty
  , insert
  , insertUnknownLocalAddr
  , updateLocalAddr
  , delete
  , deleteUnknownLocalAddr
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Ouroboros.Network.ConnectionId

data LocalAddr peerAddr =
    -- | A reserved slot for an outbound connection which is being created.  The
    -- outbound connection must be in the `ReservedOutbound` state.
    UnknownLocalAddr
    -- | All connections which are not in the `ReservedOutbound` state use
    -- `LocalAddr`, since for them the local address is known.
  | LocalAddr peerAddr
  deriving (Eq, Ord)


-- | The outer map keys are remote addresses, the internal ones are local
-- addresses.
--
newtype ConnMap peerAddr a
  = ConnMap {
      getConnMap ::
        Map peerAddr
          (Map (LocalAddr peerAddr) a)
    }
  deriving (Functor, Foldable)

instance  Traversable (ConnMap peerAddr) where
  traverse f (ConnMap m) = ConnMap <$> traverse (traverse f) m


toList :: ConnMap m a -> [a]
toList = Foldable.toList


-- | Create a map of all connections with a known `ConnectionId`.
--
toMap :: forall peerAddr a.
         Ord peerAddr
      => ConnMap peerAddr a
      -> Map (ConnectionId peerAddr) a
toMap =
    -- We can use `fromAscList` because of the `Ord` instance of `ConnectionId`.
    -- /NOTE:/ if `fromAscList` is used on input which doesn't satisfy the
    -- precondition, then `Map.lookup` might fail when it shouldn't.
    Map.fromAscList
  . Map.foldrWithKey
      (\remoteAddress st conns ->
        Map.foldrWithKey
          (\localAddr conn conns' ->
            case localAddr of
              UnknownLocalAddr -> conns'
              LocalAddr localAddress ->
                (ConnectionId { remoteAddress, localAddress }, conn) : conns'
          )
          conns
          st
      )
      []
  . getConnMap


empty :: ConnMap peerAddr a
empty = ConnMap Map.empty


insert :: Ord peerAddr
       => ConnectionId peerAddr
       -> a
       -> ConnMap peerAddr a
       -> ConnMap peerAddr a
insert ConnectionId { remoteAddress, localAddress } a =
    ConnMap
  . Map.alter
      (\case
        Nothing -> Just $! Map.singleton (LocalAddr localAddress) a
        Just st -> Just $! Map.insert (LocalAddr localAddress) a st)
    remoteAddress
  . getConnMap


insertUnknownLocalAddr
  :: Ord peerAddr
  => peerAddr
  -> a
  -> ConnMap peerAddr a
  -> ConnMap peerAddr a
insertUnknownLocalAddr remoteAddress a =
    ConnMap
  . Map.alter
      (\case
        Nothing -> Just $! Map.singleton UnknownLocalAddr a
        Just st -> Just $! Map.insert UnknownLocalAddr a st
      )
    remoteAddress
  . getConnMap


deleteUnknownLocalAddr
  :: Ord peerAddr
  => peerAddr
  -> ConnMap peerAddr a
  -> ConnMap peerAddr a
deleteUnknownLocalAddr remoteAddress =
    ConnMap
  . Map.alter
      (\case
         Nothing -> Nothing
         Just st -> Just $ Map.delete UnknownLocalAddr st
      )
      remoteAddress
  . getConnMap




-- | Promote `UnknownLocalAddr` to `LocalAddr`.
--
updateLocalAddr
  :: Ord peerAddr
  => ConnectionId peerAddr
  -> ConnMap peerAddr a
  -> ConnMap peerAddr a
updateLocalAddr ConnectionId { remoteAddress, localAddress } (ConnMap m) =
    ConnMap
  $ Map.alter
      (\case
          Nothing -> error "updateOutbound: invariant violation"
          Just m' -> Just $ Map.insert
                              (LocalAddr localAddress)
                              (m' Map.! UnknownLocalAddr)
                              m'
      )
      remoteAddress
      m


delete :: Ord peerAddr
       => ConnectionId peerAddr
       -> ConnMap peerAddr a
       -> ConnMap peerAddr a
delete ConnectionId { remoteAddress, localAddress } =
    ConnMap
  . Map.alter
      (\case
          Nothing -> Nothing
          Just st ->
            let st' = Map.delete (LocalAddr localAddress) st
            in if Map.null st'
                 then Nothing
                 else Just st'
      )
      remoteAddress
  . getConnMap


lookup :: Ord peerAddr
       => ConnectionId peerAddr
       -> ConnMap peerAddr a
       -> Maybe a
lookup ConnectionId { remoteAddress, localAddress } (ConnMap st) =
   case remoteAddress `Map.lookup` st of
     Nothing  -> Nothing
     Just st' -> LocalAddr localAddress `Map.lookup` st'
