{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Util.TentativeState (
    TentativeState (..)
  , preferToLastInvalidTentative
  ) where

import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Protocol.Abstract (SelectView,
                     preferCandidate)

-- | Tentative header state in the context of diffusion pipelining. This is used
-- to check/enforce the monotonicity requirement on invalid tentative block
-- bodies.
--
--  * During chain selection, we maintain the last invalid tentative header to
--    ensure that the stream of tentative headers we sent downstream whose
--    blocks turned out to be invalid are strictly improving.
--  * In the BlockFetch client, we use it to enforce this property for each
--    upstream peer.
data TentativeState blk =
    LastInvalidTentative !(SelectView (BlockProtocol blk))
  | NoLastInvalidTentative
  deriving stock (Generic)

deriving stock    instance Show     (SelectView (BlockProtocol blk)) => Show     (TentativeState blk)
deriving stock    instance Eq       (SelectView (BlockProtocol blk)) => Eq       (TentativeState blk)
deriving anyclass instance NoThunks (SelectView (BlockProtocol blk)) => NoThunks (TentativeState blk)

preferToLastInvalidTentative ::
     forall blk.
     LedgerSupportsProtocol blk
  => BlockConfig blk
  -> TentativeState blk
  -> Header blk
  -> Bool
preferToLastInvalidTentative bcfg ts hdr = case ts of
    LastInvalidTentative lastInvalid ->
      preferCandidate
        (Proxy @(BlockProtocol blk))
        lastInvalid
        (selectView bcfg hdr)
    NoLastInvalidTentative -> True
