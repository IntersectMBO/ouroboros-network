{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}


-- | 
--
module Network.TypedProtocol.Proofs where

import Network.TypedProtocol.Core
import Data.Void (Void, absurd)


data AgencyProofs ps = AgencyProofs {

       proofByContradiction_ClientAndServerHaveAgency
         :: forall (st :: ps).
            ClientHasAgency st
         -> ServerHasAgency st
         -> Void,

       proofByContradiction_NobodyAndClientHaveAgency
         :: forall (st :: ps).
            NobodyHasAgency st
         -> ClientHasAgency st
         -> Void,

       proofByContradiction_NobodyAndServerHaveAgency
         :: forall (st :: ps).
            NobodyHasAgency st
         -> ServerHasAgency st
         -> Void
     }

connect :: forall ps st m a b.
           Monad m
        => AgencyProofs ps
        -> Peer AsClient (st :: ps) m a
        -> Peer AsServer (st :: ps) m b
        -> m (a, b)
connect AgencyProofs{..} = go
  where
    go :: forall st'.
          Peer AsClient (st' :: ps) m a
       -> Peer AsServer (st' :: ps) m b
       -> m (a, b)
    go  (Done !_st a)      (Done !_st' b)      = return (a, b)
    go  (Effect a)          b                  = a >>= \a' -> go a' b
    go  a                  (Effect b)          = b >>= \b' -> go a  b'
    go  (Yield !_st msg a) (Await !_st' b)     = go  a     (b msg)
    go  (Await !_st a)     (Yield !_st' msg b) = go (a msg) b

    -- By appealing to the proofs about agency for this protocol we can
    -- show that these other cases are impossible
    go (Yield stA _ _) (Yield stB _ _) =
      absurd (proofByContradiction_ClientAndServerHaveAgency stA stB)

    go (Await stA _)   (Await stB _)   =
      absurd (proofByContradiction_ClientAndServerHaveAgency stB stA)

    go (Done  stA _)   (Yield stB _ _) =
      absurd (proofByContradiction_NobodyAndServerHaveAgency stA stB)

    go (Done  stA _)   (Await stB _)   =
      absurd (proofByContradiction_NobodyAndClientHaveAgency stA stB)

    go (Yield stA _ _) (Done stB _)    =
      absurd (proofByContradiction_NobodyAndClientHaveAgency stB stA)

    go (Await stA _)   (Done stB _)    =
      absurd (proofByContradiction_NobodyAndServerHaveAgency stB stA)

