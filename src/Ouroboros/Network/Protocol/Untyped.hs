{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- | An view of the typed protocol where we forget the strong typing on the
-- protocol state transitions and just retain the protocol actions.
--
-- This view is useful for executing protocols.
--
module Ouroboros.Network.Protocol.Untyped where

import qualified Ouroboros.Network.Protocol.Typed as Typed

import Data.Proxy
import Data.Typeable (Typeable, (:~:)(Refl), eqT)
import Data.Kind (Type)


-- | This is like 'Typed.Peer' but without the strong typing on the protocol
-- state transitions.
--
-- Because it is not strongly typed, the 'PeerAwait' can fail if given an
-- unexpected message that is not correct for the protocol state.
--
-- This form can be used for interpreting against a channel (message or byte
-- based with a suitable codec).
--
data Peer msg m a
  = PeerDone   a
  | PeerHole  (m (Peer msg m a))
  | PeerYield (SomeMessage msg) (Peer msg m a)
  | PeerAwait (SomeMessage msg -> Maybe (Peer msg m a))


asUntypedPeer :: forall p st
                        (msg     :: st -> st -> Type)
                        (statusf :: st -> Typed.Status st)
                        (statust :: st -> Typed.Status st)
                        (from    :: st)
                        (to      :: st)
                        m a .
                 Monad m
              => Typed.Peer p msg (statusf from) (statust to) m a
              -> Peer msg m a

asUntypedPeer (Typed.PeerDone a) = PeerDone a

asUntypedPeer (Typed.PeerHole f) = PeerHole (fmap asUntypedPeer f)

asUntypedPeer (Typed.PeerYield msg k) =
    PeerYield (SomeMessage (Typed.exchangeTransition msg))
              (asUntypedPeer k)

asUntypedPeer (Typed.PeerAwait k) =
    PeerAwait $ \amsg ->
      case castTransition (Proxy :: Proxy from) amsg of
        Unexpected   -> Nothing
        Expected msg -> Just (asUntypedPeer (k msg))


data SomeMessage (msg :: st -> st -> Type) where
  -- | Must ensure Typeable here, because we can't do it at 'castTransition'.
  -- That would require stating that every type in the state kind is
  -- typeable. Quantified constraints could help, although a short attempt at
  -- that did not work.
  SomeMessage :: Typeable from => msg from to -> SomeMessage msg


data TransitionFrom msg from where
  Expected   :: msg from to -> TransitionFrom msg from
  Unexpected :: TransitionFrom msg from

castTransition
  :: forall st (msg :: st -> st -> Type) (from :: st) .
     ( Typeable from )
  => Proxy from
  -> SomeMessage msg
  -> TransitionFrom msg from
castTransition _proxy (SomeMessage (m :: msg from' to)) =
    case eqT of
      Just (Refl :: from :~: from') -> Expected m
      Nothing                       -> Unexpected

