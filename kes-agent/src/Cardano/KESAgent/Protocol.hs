{-#LANGUAGE GADTs #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE EmptyCase #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE PolyKinds #-}
module Cardano.KESAgent.Protocol
where

import Network.TypedProtocol.Core
import Cardano.Crypto.KES.Class

data KESProtocol (k :: *) where
  IdleState :: KESProtocol k

-- | The protocol for pushing KES keys.
--
-- Intended use:
--
-- - The Node acts as the Client, the Agent acts as a Server
-- - When a Node connects, the Agent will push the current key
-- - When the Agent generates a new key, it will push the new key
--
-- Hence, the Agent always has agency, and there is only one protocol state,
-- the 'IdleState'. From there, the Agent can always push keys, and the Node
-- will always accept new keys.
--
-- **OR:**
--
-- - The Agent acts as the Client, and the Control Server as a Server
-- - When the Control Server connects, it pushes a key to the Agent
-- - The Agent stores the key locally in memory and pushes it to any connected
--   Nodes.
--
instance Protocol (KESProtocol k) where
  data Message (KESProtocol k) st st' where
          Message :: SignKeyKES k
                  -> Message (KESProtocol k) IdleState IdleState

  -- | Server always has agency
  data ServerHasAgency st where
    TokIdle :: ServerHasAgency IdleState

  -- | Client never has agency
  data ClientHasAgency st where

  -- | Someone, i.e., the server, always has agency
  data NobodyHasAgency st where

  exclusionLemma_ClientAndServerHaveAgency tok TokIdle = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency tok _ = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency tok TokIdle = case tok of {}
