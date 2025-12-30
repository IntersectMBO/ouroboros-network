{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.ConnectionManager.ConnMap
  ( ConnMap (..)
  , LocalAddr (..)
  , toList
  , toMap
  , unknownSet
  , empty
  , insert
  , insertUnknownLocalAddr
  , delete
    -- , deleteUnknownLocalAddr
  , deleteAtRemoteAddr
  , lookup
  , lookupByRemoteAddr
  , updateLocalAddr
  , traverseMaybe
  ) where

import Prelude hiding (lookup)

import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import System.Random (RandomGen, uniformR)

import Ouroboros.Network.ConnectionId

data LocalAddr peerAddr =
    -- | A reserved slot for an outbound connection which is being created.  The
    -- outbound connection must be in the `ReservedOutbound` state.
    UnknownLocalAddr
    -- | All connections which are not in the `ReservedOutbound` state use
    -- `LocalAddr`, since for them the local address is known.
  | LocalAddr peerAddr
  deriving (Show, Eq, Ord)


-- | The outer map keys are remote addresses, the internal ones are local
-- addresses.
--
newtype ConnMap peerAddr a
  = ConnMap {
      getConnMap ::
        Map peerAddr
          (Map (LocalAddr peerAddr) a)
    }
  deriving (Show, Functor, Foldable)

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
    -- /NOTE:/ if `fromAscList` is used on input which doesn't satisfy its
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


unknownSet :: Ord peerAddr => ConnMap peerAddr a -> Set peerAddr
unknownSet = Map.keysSet
           . Map.filter (UnknownLocalAddr `Map.member`)
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


deleteAtRemoteAddr
  :: (Ord peerAddr, Eq a)
  => peerAddr
  -- ^ remoteAddr
  -> a
  -- ^ element to remove
  -> ConnMap peerAddr a
  -> ConnMap peerAddr a
deleteAtRemoteAddr remoteAddress a =
    ConnMap
  . Map.alter
      (\case
          Nothing -> Nothing
          Just st ->
            let st' = Map.filter (/=a) st in
            if Map.null st'
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


-- | Find a random entry for a given remote address.
--
-- NOTE: the outbound governor will only ask for a connection to a peer if it
-- doesn't have one (and one isn't being created).  This property, simplifies
-- `lookupOutbound`: we can pick (randomly) one of the connections to the
-- remote peer.  When the outbound governor is asking, likely all of the
-- connections are in an inbound state.  The outbound governor will has a grace
-- period after demoting a peer, so a race (when a is being demoted and
-- promoted at the same time) is unlikely.
--
lookupByRemoteAddr
    :: forall rnd peerAddr a.
       ( Ord peerAddr
       , RandomGen rnd
       )
    => rnd
    -- ^ a fresh `rnd` (it must come from a `split`)
    -> peerAddr
    -- ^ remote address
    -> ConnMap peerAddr a
    -> (Maybe a)
lookupByRemoteAddr rnd remoteAddress (ConnMap st) =
  case remoteAddress `Map.lookup` st of
    Nothing -> Nothing
    Just st' ->
      case UnknownLocalAddr `Map.lookup` st' of
        Just a -> Just a
        Nothing ->
          if Map.null st'
          then Nothing
          else let (indx, _rnd') = uniformR (0, Map.size st' - 1) rnd
               in Just $ snd $ Map.elemAt indx st'


-- | Promote `UnknownLocalAddr` to `LocalAddr`.
--
updateLocalAddr
  :: Ord peerAddr
  => ConnectionId peerAddr
  -> ConnMap peerAddr a
  -> (Bool, ConnMap peerAddr a)
  -- ^ Return `True` iff the entry was updated.
updateLocalAddr ConnectionId { remoteAddress, localAddress } (ConnMap m) =
      ConnMap
  <$> Map.alterF
      (\case
        Nothing -> (False, Nothing)
        Just m' ->
          let -- delete & lookup for entry in `UnknownLocalAddr`
              (ma, m'') =
                Map.alterF
                  (\x -> (x,Nothing))
                  UnknownLocalAddr
                  m'
          in
            case ma of
              -- there was no entry, so no need to update the inner map
              Nothing -> (False, Just m')
              -- we have an entry: put it in the `LocalAddr`, but only if it's
              -- not present in the map.
              Just {} ->
                  fmap Just
                . Map.alterF
                    (\case
                      Nothing  -> (True, ma)
                      a@Just{} -> (False, a)
                    )
                    (LocalAddr localAddress)
                $ m''
      )
      remoteAddress
      m


traverseMaybe
  :: Applicative f
  => (a -> f (Maybe b))
  -> ConnMap peerAddr a
  -> f [b]
traverseMaybe fn =
    fmap (concat . Map.elems)
  . Map.traverseMaybeWithKey
      (\_ st ->
          fmap (Just . Map.elems)
        . Map.traverseMaybeWithKey (\_ -> fn)
        $ st
      )
  . getConnMap
