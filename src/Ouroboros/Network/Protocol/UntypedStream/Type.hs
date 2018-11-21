{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Ouroboros.Network.Protocol.UntypedStream.Type where

import Data.Word (Word32)

newtype Window = Window { runWindow :: Word32 }
  deriving (Eq, Show, Ord, Enum, Num)

deriving instance Real Window
deriving instance Integral Window

newtype Threshold = Threshold { runThreshold :: Word32 }
  deriving (Eq, Show, Ord, Enum, Num)

deriving instance Real Threshold
deriving instance Integral Threshold

data StreamState where
  StIdle :: StreamState
  StBusy :: StreamState
  StFull :: StreamState
  StDone :: StreamState

data StreamMessage range a from to where
  MsgRequest      :: range -> Window -> StreamMessage range a 'StIdle 'StBusy
  MsgData         :: a -> StreamMessage range a 'StBusy 'StBusy
  -- Note: in untyped framework we dont need `MsgRenewWindow`, client can detect
  -- when it needs to send `MsgUpdateWindow` without explicit state transition.
  -- In typed setting `MsgRenewWindow` would be send as the last message withing
  -- a given window (no overhead, just additional message type). In untyped
  -- setting we can just send `MsgData`.
  --
  -- MsgRenewWindow  :: a -> StreamMessage range a 'StBusy 'StFull
  MsgUpdateWindow :: Window -> StreamMessage range a 'StFull 'StBusy
  MsgStreamEnd    :: StreamMessage range a 'StBusy 'StDone

instance (Show a, Show range) => Show (StreamMessage range a from to) where
  show (MsgRequest range window) = "MsgRequest " ++ show range ++ " " ++ show window
  show (MsgData a)               = "MsgData " ++ show a
  show (MsgUpdateWindow window)  = "MsgUpdateWindow " ++ show window
  show  MsgStreamEnd             = "MsgStreamEnd"
