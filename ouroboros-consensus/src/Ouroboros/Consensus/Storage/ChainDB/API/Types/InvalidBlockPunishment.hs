{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | How to punish the sender of a invalid block
module Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment (
    -- * opaque
    InvalidBlockPunishment
  , enact
    -- * combinators
  , Invalidity (..)
  , branch
  , mkPunishThisThread
  , mkUnlessImproved
  , noPunishment
  ) where

import qualified Control.Exception as Exn
import           Control.Monad (join, unless)
import           Data.Functor ((<&>))
import           NoThunks.Class
import           Ouroboros.Consensus.Block.Abstract (BlockProtocol)
import           Ouroboros.Consensus.Protocol.Abstract (SelectView)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.TentativeState

-- | Is the added block itself invalid, or is its prefix invalid?
data Invalidity =
    BlockItself
  | BlockPrefix

-- | How to handle a discovered 'Invalidity'
--
-- This type is opaque because the soundness of the punishment is subtle because
-- of where it is invoked during the chain selection. As a result, arbitrary
-- monadic actions would be foot guns. Instead, this module defines a small DSL
-- for punishment that we judge to be sound.
newtype InvalidBlockPunishment m = InvalidBlockPunishment {
    enact :: Invalidity -> m ()
  }
  deriving NoThunks via
    OnlyCheckWhnfNamed "InvalidBlockPunishment" (InvalidBlockPunishment m)

-- | A noop punishment
noPunishment :: Applicative m => InvalidBlockPunishment m
noPunishment = InvalidBlockPunishment $ \_invalidity -> pure ()

-- | Create a punishment that kills this thread
mkPunishThisThread :: IOLike m => m (InvalidBlockPunishment m)
mkPunishThisThread = do
    tid <- myThreadId
    pure $ InvalidBlockPunishment $ \_invalidity ->
      throwTo tid PeerSentAnInvalidBlockException

-- | Thrown asynchronously to the client thread that added the block whose
-- processing involved an invalid block.
--
-- See 'punishThisThread'.
data PeerSentAnInvalidBlockException = PeerSentAnInvalidBlockException
  deriving (Show)

instance Exn.Exception PeerSentAnInvalidBlockException

-- | Allocate a stateful punishment that performs the given punishment unless
-- the given header is better than the previous invocation
mkUnlessImproved :: forall proxy m blk.
     ( IOLike m
     , NoThunks (SelectView (BlockProtocol blk))
     , Ord      (SelectView (BlockProtocol blk))
     )
  => proxy blk
  -> STM m (   SelectView (BlockProtocol blk)
            -> InvalidBlockPunishment m
            -> InvalidBlockPunishment m
           )
mkUnlessImproved _prx = do
    var <- newTVar (NoLastInvalidTentative :: TentativeState blk)
    pure $ \new punish -> InvalidBlockPunishment $ \invalidity -> join $ atomically $ do
      io <- readTVar var <&> \case
        NoLastInvalidTentative   -> pure ()
        LastInvalidTentative old -> unless (new > old) $ do
          enact punish invalidity
      writeTVar var $ LastInvalidTentative new
      pure io

-- | Punish according to the 'Invalidity'
branch :: (Invalidity -> InvalidBlockPunishment m) -> InvalidBlockPunishment m
branch f = InvalidBlockPunishment $ \invalidity ->
    enact (f invalidity) invalidity
