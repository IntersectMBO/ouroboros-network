{-# OPTIONS_GHC -Wno-orphans     #-}
module Test.Ouroboros.Network.Orphans () where

import           Network.TypedProtocol.PingPong.Type (PingPong)
import           Network.TypedProtocol.ReqResp.Type (ReqResp)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))


instance ShowProxy PingPong where
    showProxy _ = "PingPong"

instance ShowProxy (ReqResp req resp) where
    showProxy _ = "ReqResp"
