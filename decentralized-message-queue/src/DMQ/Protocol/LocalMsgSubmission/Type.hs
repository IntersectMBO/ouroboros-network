{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE TypeFamilies             #-}

-- | This module provides the type of LocalMsgProtocol via LocalTxSubmission
--
module DMQ.Protocol.LocalMsgSubmission.Type
  ( module DMQ.Protocol.LocalMsgSubmission.Type
    -- * re-exports
  , module Core
  , module Ouroboros
  ) where

import Ouroboros.Network.Protocol.LocalTxSubmission.Type as Ouroboros
import Network.TypedProtocol.Core as Core

-- | The LocalMsgSubmission protocol is an alias for the LocalTxSubmission
--
type LocalMsgSubmission = Ouroboros.LocalTxSubmission
