{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module provides the type of LocalMsgProtocol via LocalTxSubmission
--
module DMQ.Protocol.LocalMsgSubmission.Type
  ( module DMQ.Protocol.LocalMsgSubmission.Type
    -- * re-exports
  , module Core
  , module Ouroboros
  ) where

import Data.Text (Text)
import Network.TypedProtocol.Core as Core
import Ouroboros.Network.Protocol.LocalTxSubmission.Type as Ouroboros
import Ouroboros.Network.Util.ShowProxy

-- | The LocalMsgSubmission protocol is an alias for the LocalTxSubmission
--
type LocalMsgSubmission sig = Ouroboros.LocalTxSubmission sig SigMempoolFail

-- | The type of failures when adding to the mempool
--
data SigMempoolFail =
    SigInvalid Text
  | SigDuplicate
  | SigExpired
  | SigResultOther Text
  deriving (Eq, Show)

instance ShowProxy SigMempoolFail where
