{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeInType               #-}
{-# LANGUAGE TypeOperators            #-}


-- This is already implied by the -Wall in the .cabal file, but lets just be
-- completely explicit about it too, since we rely on the completeness
-- checking in the cases below for the completeness of our proofs.
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Proofs about the typed protocol framework.
--
-- It also provides helpful testing utilities.
--
module Network.TypedProtocol.Stateful.Proofs
  ( connect
  , TerminalStates (..)
  , removeState
  ) where

import           Control.Monad.Class.MonadSTM

import           Data.Kind (Type)
import           Data.Type.Queue
import           Data.Singletons

import           Network.TypedProtocol.Core
import qualified Network.TypedProtocol.Stateful.Peer as ST
import           Network.TypedProtocol.Peer
import           Network.TypedProtocol.Proofs (TerminalStates (..))
import qualified Network.TypedProtocol.Proofs as TP
import           Unsafe.Coerce (unsafeCoerce)

-- | Type which is used to track the protocol state while pipeline messages.
--
type F :: (ps -> Type) -> ps -> ps -> Type
data F f st st' where
  F :: forall ps f (st :: ps) (st' :: ps).
      !(f st)
    -> F f st st'


removeState
  :: forall ps (pr :: PeerRole)
            (pl :: Pipelined)
            (st :: ps)
            (f :: ps -> Type)
            m stm a.
     Functor m
  => Functor stm
  => f st
  -> ST.Peer ps pr pl Empty st f m stm a
  ->    Peer ps pr pl Empty st   m stm a
removeState = goEmpty

goEmpty
  :: forall ps (pr :: PeerRole)
            (pl :: Pipelined)
            (st :: ps)
            (f :: ps -> Type)
            m stm a.
     Functor m
  => Functor stm
  => f st
  -> ST.Peer ps pr pl Empty st f m stm a
  ->    Peer ps pr pl Empty st   m stm a
goEmpty f (ST.Effect k) = Effect (goEmpty f <$> k)
goEmpty _ (ST.Yield refl f msg k) = Yield refl msg (goEmpty f k)
goEmpty f (ST.Await refl k) = Await refl $ \msg ->
  case k f msg of
    (k', f') -> goEmpty f' k'
goEmpty _ (ST.YieldPipelined refl f msg k) =
  YieldPipelined refl msg (goPipelined (SingConsF (F f) SingEmptyF) k)
goEmpty _ (ST.Done refl a) = Done refl a

goPipelined
  :: forall ps (pr :: PeerRole)
            (q :: Queue ps)
            (st :: ps)
            (f :: ps -> Type)
            m stm a.
     Functor m
  => Functor stm
  => SingQueueF (F f) q
  -> ST.Peer ps pr 'Pipelined q st f m stm a
  ->    Peer ps pr 'Pipelined q st   m stm a
goPipelined q (ST.Effect k) = Effect (goPipelined q <$> k)

goPipelined q (ST.YieldPipelined
               refl f
               (msg :: Message ps st st')
               (k :: ST.Peer ps pr pl (q |> Tr st' st'') st'' f m stm a)) =
  YieldPipelined refl msg (goPipelined (q |> (F f :: F f st' st'')) k)

goPipelined q@(SingConsF (F f) q') (ST.Collect refl k' k) =
  Collect refl (goPipelined q <$> k')
             $ \msg -> case k f msg of
                (k'', f'') -> goPipelined (SingConsF (F f'') q') k''

goPipelined
  (SingConsF (F (f :: f stX)) SingEmptyF)
  (ST.CollectDone k :: ST.Peer ps pr 'Pipelined (Cons (Tr stX stY) Empty) stZ f m stm a) =
    -- we collected all messages, which means that we reached the type:
    -- @Peer ps pr pl (Tr st st <| Empty) st f m stm a@
    -- but we don't have an evidence that stX and stY are equal to stZ, we'd
    -- need a `Queue` type which tracks its right most state.
    CollectDone (goEmpty (coerce f) k)
  where
    coerce :: f stX -> f stZ
    coerce = unsafeCoerce

goPipelined (SingConsF (F _) q) (ST.CollectDone k) =
  CollectDone (goPipelined q k)

goPipelined q@(SingConsF (F f) q') (ST.CollectSTM refl k' k) =
  CollectSTM refl (goPipelined q <$> k')
                $ \msg -> case k f msg of
                    (k'', f'') -> goPipelined (SingConsF (F f'') q') k''

-- TODO: this is only because we haven't expressed that @q@ is non-empty.
-- This requires some simple proofs about snoc and cons.
goPipelined SingEmptyF _ = error "impossible happend!"

connect
  :: forall ps (pr :: PeerRole)
               (pl :: Pipelined)
               (pl' :: Pipelined)
               (st :: ps)
               (f :: ps -> Type)
               m a b.
       (MonadSTM m, SingI pr)
    => [Bool]
    -> [Bool]
    -> f st
    -> ST.Peer ps             pr  pl  Empty st f m (STM m) a
    -> ST.Peer ps (FlipAgency pr) pl' Empty st f m (STM m) b
    -> m (a, b, TerminalStates ps pr)
connect csA csB f a b = TP.connect csA csB (removeState f a) (removeState f b)
