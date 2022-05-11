{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}

-- | Bidirectional patterns for @'Peer' ps 'AsServer'@.   The advantage of
-- these patterns is that they automatically provide the 'RelativeAgencyEq'
-- singleton.
--
module Network.TypedProtocol.Stateful.Peer.Server
  ( Server
  , pattern Effect
  , pattern Yield
  , pattern Await
  , pattern Done
  , pattern YieldPipelined
  , pattern Collect
  , pattern CollectSTM
  , pattern CollectDone
    -- * re-exports
  , Pipelined (..)
  , Queue (..)
  ) where

import           Data.Kind (Type)
import           Data.Singletons

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Stateful.Peer (Peer)
import qualified Network.TypedProtocol.Stateful.Peer as TP


type Server :: forall ps
            -> Pipelined
            -> Queue ps
            -> ps
            -> (ps -> Type)
            -> (Type -> Type)
            -> (Type -> Type)
            -> Type
            -> Type
type Server ps pl q st f m stm a = Peer ps AsServer pl q st f m stm a


-- | Server role pattern for 'TP.Effect'.
--
pattern Effect :: forall ps pl q st f m stm a.
                  m (Server ps pl q st f m stm a)
               -- ^ monadic continuation
               -> Server ps pl q st f m stm a
pattern Effect mclient = TP.Effect mclient


-- | Server role pattern for 'TP.Yield'
--
pattern Yield :: forall ps pl st f m stm a.
                 ()
              => forall st'.
                 ( SingI st
                 , SingI st'
                 , StateAgency st ~ ServerAgency
                 )
              => f st'
              -> Message ps st st'
              -- ^ protocol message
              -> Server ps pl Empty st' f m stm a
              -- ^ continuation
              -> Server ps pl Empty st  f m stm a
pattern Yield f msg k = TP.Yield ReflServerAgency f msg k


-- | Server role pattern for 'TP.Await'
--
pattern Await :: forall ps pl st f m stm a.
                 ()
              => ( SingI st
                 , StateAgency st ~ ClientAgency
                 )
              => (forall st'.
                     f st
                  -> Message ps st st'
                  -> ( Server ps pl Empty st' f m stm a
                     , f st'
                     )
                 )
              -- ^ continuation
              -> Server ps pl Empty st  f m stm a
pattern Await k = TP.Await ReflClientAgency k


-- | Server role pattern for 'TP.Done'
--
pattern Done :: forall ps pl st f m stm a.
                ()
             => ( SingI st
                , StateAgency st ~ NobodyAgency
                )
             => a
             -- ^ protocol return value
             -> Server ps pl Empty st f m stm a
pattern Done a = TP.Done ReflNobodyAgency a


-- | Server role pattern for 'TP.YieldPipelined'
--
pattern YieldPipelined :: forall ps st q f m stm a.
                          ()
                       => forall st' st''.
                          ( SingI st
                          , SingI st'
                          , StateAgency st ~ ServerAgency
                          )
                       => f st'
                       -> Message ps st st'
                       -- ^ pipelined message
                       -> Server ps 'Pipelined (q |> Tr st' st'') st'' f m stm a
                       -- ^ continuation
                       -> Server ps 'Pipelined  q                 st   f m stm a
pattern YieldPipelined f msg k = TP.YieldPipelined ReflServerAgency f msg k


-- | Server role pattern for 'TP.Collect'
--
pattern Collect :: forall ps st' st'' q st f m stm a.
                   ()
                => ( SingI st'
                   , StateAgency st' ~ ClientAgency
                   )
                => Maybe (Server ps 'Pipelined (Tr st' st'' <| q) st f m stm a)
                -- ^ continuation, executed if no message has arrived so far
                -> (forall stNext.
                       f st'
                    -> Message ps st' stNext
                    -> ( Server ps 'Pipelined (Tr stNext st'' <| q) st f m stm a
                       , f stNext
                       )
                   )
                -- ^ continuation
                -> Server ps 'Pipelined (Tr st'    st'' <| q) st f m stm a
pattern Collect k' k = TP.Collect ReflClientAgency k' k


-- | Client role pattern for 'TP.Collect'
--
pattern CollectSTM :: forall ps st' st'' q st f m stm a.
                      ()
                   => ( SingI st'
                      , StateAgency st' ~ ClientAgency
                      )
                   => stm (Server ps 'Pipelined (Tr st' st'' <| q) st f m stm a)
                   -- ^ continuation, executed if no message has arrived so far
                   -> (forall stNext. 
                         f st'
                      -> Message ps st' stNext
                      -> ( Server ps 'Pipelined (Tr stNext st'' <| q) st f m stm a
                         , f stNext
                         )
                      )
                   -- ^ continuation
                   -> Server ps 'Pipelined (Tr st' st'' <| q) st f m stm a
pattern CollectSTM k' k = TP.CollectSTM ReflClientAgency k' k


-- | Client role pattern for 'TP.CollectDone'
--
pattern CollectDone :: forall ps st q st' f m stm a.
                       ()
                    => ()
                    => Server ps 'Pipelined              q  st' f m stm a
                    -- ^ continuation
                    -> Server ps 'Pipelined (Tr st st <| q) st' f m stm a
pattern CollectDone k = TP.CollectDone k


{-# COMPLETE Effect, Yield, Await, Done, YieldPipelined, Collect, CollectDone #-}
