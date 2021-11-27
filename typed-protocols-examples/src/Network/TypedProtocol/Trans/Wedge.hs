{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Network.TypedProtocol.Trans.Wedge where

import           Control.Monad.Class.MonadSTM (STM)

import           Data.Singletons

import           Network.TypedProtocol.Core

import qualified Network.TypedProtocol.Peer.Client as Client
import qualified Network.TypedProtocol.PingPong.Type as PingPong


-- | A [wedge](https://hackage.haskell.org/package/smash-0.1.2/docs/Data-Wedge.html)
-- sum of two protocols.
--
-- One can interleave both protocols using protocol pipelining.  Termination
-- must be done by terminating one of the protocols.
--
data Wedge ps (stIdle :: ps) ps' (stIdle' :: ps') where
    StIdle :: Wedge ps stIdle ps' stIdle'
    StFst  :: ps  -> Wedge ps stIdle ps' stIdle'
    StSnd  :: ps' -> Wedge ps stIdle ps' stIdle'


data SingWedge (st ::  Wedge ps (stIdle :: ps) ps' (stIdle' :: ps')) where
    SingStIdle :: SingWedge StIdle
    SingStFst  :: Sing st
               -> SingWedge (StFst st)
    SingStSnd  :: Sing st'
               -> SingWedge (StSnd st')

instance Show (SingWedge StIdle) where
    show SingStIdle    = "SingStIdle"
instance Show (Sing st) => Show (SingWedge (StFst st)) where
    show (SingStFst s) = "SingStFst " ++ show s
instance Show (Sing st) => Show (SingWedge (StSnd st)) where
    show (SingStSnd s) = "SingStSnd " ++ show s

type instance Sing = SingWedge
instance SingI StIdle where
    sing = SingStIdle
instance SingI st => SingI (StFst st) where
    sing = SingStFst (sing @st)
instance SingI st => SingI (StSnd st) where
    sing = SingStSnd (sing @st)


-- | A Ssingleton type which allows to pick the starting protocol state.
--
data SingStart (st :: Wedge ps stIdle ps' stIdle') where
    AtFst :: SingStart (StFst stIdle)
    AtSnd :: SingStart (StSnd stIdle)


-- Note: This does not require @(Protocol ps, Protocol ps')@, ghc is not
-- requirihng class constraints for associated type families / data types the
-- same way as for terms.
--
instance Protocol (Wedge ps (stIdle :: ps) ps' (stIdle' :: ps')) where

    data Message  (Wedge ps (stIdle :: ps) ps' (stIdle' :: ps')) from to where
      -- | Signal that starts one of the protocols.
      --
      MsgStart :: SingStart st
               -> Message (Wedge ps stIdle ps' stIdle')
                          StIdle st

      -- | Embed any @ps@ message.
      --
      MsgFst      :: Message ps  st st'
                  -> Message (Wedge ps stIdle ps' stIdle')
                             (StFst st) (StFst st')


      -- | Embed any @ps'@ message.
      MsgSnd      :: Message ps' st st'
                  -> Message (Wedge ps stIdle ps' stIdle')
                             (StSnd st) (StSnd st')

      -- | Switch from @ps@ to @ps'@.
      --
      MsgFstToSnd :: Message (Wedge ps stIdle ps' stIdle')
                             (StFst stIdle) (StSnd stIdle')

      -- | Switch from @ps'@ to @ps@.
      --
      MsgSndToFst :: Message (Wedge ps stIdle ps' stIdle')
                             (StSnd stIdle') (StFst stIdle)


    type StateAgency StIdle     = ClientAgency
    type StateAgency (StFst st) = StateAgency st
    type StateAgency (StSnd st) = StateAgency st


type PingPong2 = Wedge PingPong.PingPong PingPong.StIdle
                       PingPong.PingPong PingPong.StIdle


pingPong2Client :: Client.Client PingPong2 NonPipelined Empty StIdle m stm ()
pingPong2Client =
    Client.Yield (MsgStart AtFst)
  $ Client.Yield (MsgFst PingPong.MsgPing)
  $ Client.Await $ \(MsgFst PingPong.MsgPong) ->
    Client.Yield MsgFstToSnd
  $ Client.Yield (MsgSnd PingPong.MsgPing)
  $ Client.Await $ \(MsgSnd PingPong.MsgPong) ->
  -- terminate, through the second protocol
    Client.Yield (MsgSnd PingPong.MsgDone)
  $ Client.Done ()


pingPong2Client' :: forall m. Client.Client PingPong2 'Pipelined Empty StIdle m (STM m) ()
pingPong2Client' =
    --
    -- Pipeline first protocol
    --

      Client.YieldPipelined (MsgStart AtFst)
    $ Client.YieldPipelined (MsgFst PingPong.MsgPing)

    $ Client.YieldPipelined MsgFstToSnd

    --
    -- Pipeline second protocol
    --

    -- We need to provide the type of the existential @st''@ which is the state
    -- at which we continue after we pipeline the @'MsgSnd' 'PingPong.MsgPing'@.
    $ Client.YieldPipelined @_ @_ @_ @_ @_ @_ @_ @(StSnd PingPong.StIdle) (MsgSnd PingPong.MsgPing)

    --
    -- Collect reposnses from the first protocol
    --

    $ Client.CollectDone -- collect transition pushed by MsgStartFst
    $ Client.Collect Nothing $ \(MsgFst PingPong.MsgPong) ->
      Client.CollectDone

    --
    -- Collect reposnses from the sencond protocol
    --

    $ Client.CollectDone -- collect transition pushed by MsgFstToSnd
    $ Client.Collect Nothing $ \(MsgSnd PingPong.MsgPong) ->
      Client.CollectDone

    --
    -- Terminate the protocol
    --

    $ Client.Yield (MsgSnd PingPong.MsgDone)
    $ Client.Done ()
