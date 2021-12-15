{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.Protocol.TipSample.Type where

import           Cardano.Slotting.Slot (SlotNo)
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined (N (..), Nat)

import           Ouroboros.Network.Util.ShowProxy


-- | There are three of blocking requests: awiat until slot, await until
-- a tip will change; the third one has a dedicated type:
-- @'StFollowTip' :: 'TipSample' tip@.
--
data TipRequestKind where
    BlockUntilSlot    :: TipRequestKind
    BlockUntilTip     :: TipRequestKind


data TokTipRequest (tipRequestKind :: TipRequestKind) where
    TokBlockUntilSlot    :: TokTipRequest BlockUntilSlot
    TokBlockUntilTip     :: TokTipRequest BlockUntilTip

deriving instance Show (TokTipRequest tipRequestKind)


-- | `tip-sample` protocol, desigined for established peers to sample tip from
-- upstream peers.
--
data TipSample tip where
    StIdle      :: TipSample tip
    StFollowTip :: N -> TipSample tip
    StDone      :: TipSample tip

instance ShowProxy tip => ShowProxy (TipSample tip) where
    showProxy _ = "TipSample (" ++ showProxy (Proxy :: Proxy tip) ++ ")"

instance Protocol (TipSample tip) where
    data Message (TipSample tip) from to where

      -- | Request a series of tip changes starting at a given `SlotNo` (or
      -- after). The server is not obliged to send consecutive updates, what
      -- only matters is that the new tip is send as soon it becomes the
      -- server's tip.
      --
      MsgFollowTip :: Nat (S n)
                   -> SlotNo
                   -> Message (TipSample tip)
                              StIdle
                              (StFollowTip (S n))

      -- | Send a tip back to the client, hold on the agency.
      --
      MsgNextTip :: tip
                 -> Message (TipSample tip)
                            (StFollowTip (S (S n)))
                            (StFollowTip (S n))

      -- | Send last tip and pass the agency to the client.
      --
      MsgNextTipDone :: tip
                     -> Message (TipSample tip)
                                (StFollowTip (S Z))
                                StIdle

      -- | Terminating message (client side).
      --
      MsgDone :: Message (TipSample tip)
                         StIdle
                         StDone

    data ClientHasAgency st where
      TokIdle      :: ClientHasAgency StIdle

    data ServerHasAgency st where
      TokFollowTip :: Nat (S n) -> ServerHasAgency (StFollowTip (S n))

    data NobodyHasAgency st where
      TokDone      :: NobodyHasAgency StDone

    exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
    exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
    exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


--
-- Show instances
--

deriving instance Show tip => Show (Message (TipSample tip) from to)
deriving instance Show (ClientHasAgency (st :: TipSample tip))
deriving instance Show (ServerHasAgency (st :: TipSample tip))
deriving instance Show (NobodyHasAgency (st :: TipSample tip))
