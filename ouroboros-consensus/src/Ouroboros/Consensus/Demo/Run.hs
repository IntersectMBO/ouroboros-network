{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Infrastructure required to run the demo
--
-- The definitions in this module are independent from any specific protocol.
module Ouroboros.Consensus.Demo.Run (
    -- * ProtocolInfo
    ProtocolInfo(..)
  , NumCoreNodes(..)
    -- * Constraints required to run the demo
  , DemoHeaderHash(..)
  , DemoHeader(..)
  , DemoBlock(..)
  , RunDemo(..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Crypto.Random (MonadRandom)

import           Ouroboros.Network.Block (BlockNo, ChainHash (..), HasHeader,
                     HeaderHash, SlotNo)
import           Ouroboros.Network.BlockFetch (SizeInBytes)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Data required to run the specified protocol.
data ProtocolInfo b = ProtocolInfo {
        pInfoConfig     :: NodeConfig (BlockProtocol b)
      , pInfoInitState  :: NodeState  (BlockProtocol b)
        -- | The ledger state at genesis
      , pInfoInitLedger :: ExtLedgerState b
      }

newtype NumCoreNodes = NumCoreNodes Int
  deriving (Show)

{-------------------------------------------------------------------------------
  Constraints required to run the demo
-------------------------------------------------------------------------------}

class DemoHeaderHash hh where
  demoEncodeHeaderHash :: hh -> Encoding
  demoDecodeHeaderHash :: Decoder s hh

class ( DemoHeaderHash (HeaderHash hdr)
      , SupportedBlock (BlockProtocol hdr) hdr
      , HasHeader hdr
      , Condense hdr
      , Condense (ChainHash hdr)
      ) => DemoHeader hdr where
  demoEncodeHeader   :: NodeConfig (BlockProtocol hdr) -> hdr -> Encoding
  demoDecodeHeader   :: NodeConfig (BlockProtocol hdr) -> Decoder s hdr
  demoBlockFetchSize :: hdr -> SizeInBytes

class ( ProtocolLedgerView blk
      , LedgerConfigView   blk
      , Condense           blk
      , Condense          [blk]
      , ApplyTx            blk
      ) => DemoBlock blk where
  demoEncodeBlock :: NodeConfig (BlockProtocol blk) -> blk -> Encoding
  demoDecodeBlock :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s blk

  -- | Construct transaction from mock transaction
  --
  -- When we run the demo, for convenience we submit mock transactions from
  -- the command line. These then need to be translated to "real" transactions
  -- for the ledger that we are running. Of course, this translation will
  -- necessarily be limited and will rely on things like 'generatedSecrets'.
  demoMockTx :: NodeConfig (BlockProtocol blk) -> Tx -> GenTx blk

class ( DemoHeader hdr
      , DemoBlock blk
      , BlockProtocol blk ~ BlockProtocol hdr
      , HeaderHash    blk ~ HeaderHash    hdr
      ) => RunDemo blk hdr where
  demoForgeBlock         :: (HasNodeState (BlockProtocol blk) m, MonadRandom m)
                         => NodeConfig (BlockProtocol blk)
                         -> SlotNo         -- ^ Current slot
                         -> BlockNo        -- ^ Current block number
                         -> ChainHash hdr  -- ^ Previous hash
                         -> [GenTx blk]    -- ^ Txs to add in the block
                         -> IsLeader (BlockProtocol blk)
                         -> m blk
  demoGetHeader          :: blk -> hdr
  demoBlockMatchesHeader :: hdr -> blk -> Bool
