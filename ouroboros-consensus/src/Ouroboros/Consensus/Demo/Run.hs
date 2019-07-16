{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- | Infrastructure required to run the demo
module Ouroboros.Consensus.Demo.Run
  ( RunDemo(..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.Typeable (Typeable)

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.Demo.Byron.Elaborate as Byron
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Additional functionality required to run the demo
-------------------------------------------------------------------------------}

class RunNode blk => RunDemo blk where
  -- | Construct transaction from mock transaction
  --
  -- When we run the demo, for convenience we submit mock transactions from
  -- the command line. These then need to be translated to "real" transactions
  -- for the ledger that we are running. Of course, this translation will
  -- necessarily be limited and will rely on things like 'generatedSecrets'.
  demoMockTx :: NodeConfig (BlockProtocol blk) -> Tx -> GenTx blk

instance ( ProtocolLedgerView (SimpleBlock SimpleMockCrypto ext)
           -- The below constraint seems redundant but is not! When removed,
           -- some of the tests loop, but only when compiled with @-O2@ ; with
           -- @-O0@ it is perfectly fine. ghc bug?!
         , SupportedBlock (SimpleBlock SimpleMockCrypto ext)
         , Show ext
         , Typeable ext
         , Serialise ext
         , ForgeExt (BlockProtocol (SimpleBlock SimpleMockCrypto ext))
                    SimpleMockCrypto
                    ext
         , Serialise (ChainState (BlockProtocol (SimpleBlock SimpleMockCrypto ext)))
         ) => RunDemo (SimpleBlock SimpleMockCrypto ext) where
  demoMockTx _ = SimpleGenTx

instance ByronGiven => RunDemo (ByronBlock ByronConfig) where
  demoMockTx = Byron.elaborateTx
