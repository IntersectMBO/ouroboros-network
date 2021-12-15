{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Network.Protocol.Trans.Hello.Type where

import           Data.Void (Void)

import           Network.TypedProtocol.Core
import           Ouroboros.Network.Util.ShowProxy


-- | The 'Hello' protocol transformer reverses the initial agency of the protocol,
-- from one in which the server has the initial agency to one in which the client
-- has the initial agency.
--
-- It extends the underlying protocol with an initial extra 'MsgHello' message,
-- which reverses the agency between the two peers.
--
data Hello ps (stIdle :: ps) where
    -- | 'StHello' state is the initial state of the 'Hello' protocol.
    --
    StHello  :: Hello ps stIdle

    -- | 'StTalk' embeds states of the inner protocol.
    --
    StTalk :: ps -> Hello ps stIdle

instance ( ShowProxy ps
         , ShowProxy stIdle
         )
      => ShowProxy (Hello ps (stIdle :: ps)) where
    showProxy _ = "Hello "
               ++ showProxy (Proxy :: Proxy ps)
               ++ " "
               ++ showProxy (Proxy :: Proxy stIdle)



instance Protocol ps => Protocol (Hello ps stIdle) where
    data Message (Hello ps stIdle) from to where
      -- | Client side hello message.
      --
      MsgHello :: Message (Hello ps stIdle) StHello (StTalk stIdle)

      -- | After initial hello message one proceeds with the wrapped protocol
      -- 'ps'.
      --
      MsgTalk :: Message ps stInner stInner'
                 -> Message (Hello ps stIdle) (StTalk stInner) (StTalk stInner')


    -- | Either intial 'StHello' state or 'ps' protocol states, which have client
    -- agency.  This is embedding of the 'ps' client states into client states
    -- of the wrapper.
    --
    data ClientHasAgency (st :: Hello ps stIdle) where
      TokHello      :: ClientHasAgency StHello

      TokClientTalk :: ClientHasAgency stInner
                    -> ClientHasAgency (StTalk stInner)


    -- | States with server side agency are only the wrapped protocol states
    -- with server agency.
    --
    data ServerHasAgency (st :: Hello ps stIdle) where
      TokServerTalk :: ServerHasAgency stInner
                    -> ServerHasAgency (StTalk stInner)


    -- | Terminating states are only the terminating states of the wrapped
    -- protocol.
    --
    data NobodyHasAgency (st :: Hello ps stIdle) where
      TokDone :: NobodyHasAgency stInner
              -> NobodyHasAgency (StTalk stInner)


    exclusionLemma_ClientAndServerHaveAgency
      :: forall (st :: Hello ps stIdle).
         ClientHasAgency st
      -> ServerHasAgency st
      -> Void
    exclusionLemma_ClientAndServerHaveAgency TokHello tok = case tok of {}
    exclusionLemma_ClientAndServerHaveAgency (TokClientTalk tokClient)
                                             (TokServerTalk tokServer) =
      exclusionLemma_ClientAndServerHaveAgency tokClient tokServer


    exclusionLemma_NobodyAndClientHaveAgency
      :: forall (st :: Hello ps stIdle).
         NobodyHasAgency st
      -> ClientHasAgency st
      -> Void
    exclusionLemma_NobodyAndClientHaveAgency (TokDone tokDone)
                                             (TokClientTalk tokClient) =
      exclusionLemma_NobodyAndClientHaveAgency tokDone tokClient


    exclusionLemma_NobodyAndServerHaveAgency
      :: forall (st :: Hello ps stIdle).
         NobodyHasAgency st
      -> ServerHasAgency st
      -> Void
    exclusionLemma_NobodyAndServerHaveAgency (TokDone tokDone)
                                             (TokServerTalk tokServer) =
      exclusionLemma_NobodyAndServerHaveAgency tokDone tokServer


instance (forall (from' :: ps) (to' :: ps). Show (Message ps from' to'))
      => Show (Message (Hello ps stIdle) from to) where
    show MsgHello      = "MsgHello"
    show (MsgTalk msg) = "MsgTalk " ++ show msg

instance (forall (st' :: ps). Show (ClientHasAgency st'))
      => Show (ClientHasAgency (st :: Hello ps (stIdle :: ps))) where
    show TokHello=           "TokHello"
    show (TokClientTalk tok) = "TokClientTalk " ++ show tok

instance (forall (st' :: ps). Show (ServerHasAgency st'))
      => Show (ServerHasAgency (st :: Hello ps stIdle)) where
    show (TokServerTalk tok) = "TokServerTalk " ++ show tok
