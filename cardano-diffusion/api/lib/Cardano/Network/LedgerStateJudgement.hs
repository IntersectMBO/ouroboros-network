{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Network.LedgerStateJudgement (LedgerStateJudgement (..)) where

import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Wether the node is caught up or fell too far behind the chain
data LedgerStateJudgement = YoungEnough | TooOld
  deriving (Eq, Show, Generic)

instance NoThunks LedgerStateJudgement
