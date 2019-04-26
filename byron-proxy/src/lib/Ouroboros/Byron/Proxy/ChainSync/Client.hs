{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}

module Ouroboros.Byron.Proxy.ChainSync.Client where

import Ouroboros.Network.Protocol.ChainSync.Client

import Ouroboros.Byron.Proxy.ChainSync.Types (Block (..), Point (..))

newtype Fold m a = Fold
  { runFold :: m (Next m a)
  }

data Next m a where
  Stop     :: a      -> Next m a
  -- | In the continuation, `Just` means it was improved, `Nothing` means
  -- none of the points were found. The second `Point` is the current tip.
  Improve  :: [Point] -> (Maybe Point -> Point -> Fold m a) -> Next m a
  Continue :: (Block -> Point -> Fold m a) -- Roll forawrd
           -> (Point -> Point -> Fold m a) -- Roll backward
           -> Next m a

-- | Repeatedly request next, and run the `Fold` until it finishes.
chainSyncClient
  :: forall m a .
     ( Monad m )
  => Fold m a
  -> ChainSyncClient Block Point m a
chainSyncClient fold = ChainSyncClient $ do
  next <- runFold fold
  case next of
    Stop a -> pure $ SendMsgDone a
    Improve pts k -> pure $ SendMsgFindIntersect pts $ ClientStIntersect
      { recvMsgIntersectImproved  = \rp tip -> chainSyncClient (k (Just rp) tip)
      , recvMsgIntersectUnchanged = \tip    -> chainSyncClient (k Nothing tip)
      }
    Continue forward backward -> pure $ SendMsgRequestNext immediate later
      where
      immediate :: ClientStNext Block Point m a
      immediate = ClientStNext
        { recvMsgRollForward = \blk point -> chainSyncClient (forward blk point)
        , recvMsgRollBackward = \point1 point2 -> chainSyncClient (backward point1 point2)
        }
      later :: m (ClientStNext Block Point m a)
      later = pure immediate
