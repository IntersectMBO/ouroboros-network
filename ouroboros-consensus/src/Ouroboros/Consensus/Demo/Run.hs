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
  , RunDemo(..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Crypto.Random (MonadRandom)

import           Ouroboros.Network.Block (BlockNo, ChainHash (..), HeaderHash,
                     SlotNo)
import           Ouroboros.Network.BlockFetch (SizeInBytes)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Extended
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

class ( ProtocolLedgerView blk
      , DemoHeaderHash (HeaderHash blk)
      , Condense (Header blk)
      , Condense (ChainHash blk)
      , Condense (HeaderHash blk)
      , Condense blk
      , Condense [blk]
      , Show blk
      , ApplyTx blk
      , Show (ApplyTxErr blk)
      , Condense (GenTx blk)
      , Show (GenTx blk)
      ) => RunDemo blk where
  demoForgeBlock         :: (HasNodeState (BlockProtocol blk) m, MonadRandom m)
                         => NodeConfig (BlockProtocol blk)
                         -> SlotNo         -- ^ Current slot
                         -> BlockNo        -- ^ Current block number
                         -> ChainHash blk  -- ^ Previous hash
                         -> [GenTx blk]    -- ^ Txs to add in the block
                         -> IsLeader (BlockProtocol blk)
                         -> m blk
  demoBlockMatchesHeader :: Header blk -> blk -> Bool
  demoBlockFetchSize     :: Header blk -> SizeInBytes

  -- Encoders
  demoEncodeBlock  :: NodeConfig (BlockProtocol blk) -> blk -> Encoding
  demoEncodeHeader :: NodeConfig (BlockProtocol blk) -> Header blk -> Encoding
  demoEncodeGenTx  ::                                   GenTx  blk -> Encoding

  -- Decoders
  demoDecodeHeader :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s (Header blk)
  demoDecodeBlock  :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s blk
  demoDecodeGenTx  :: forall s.                                   Decoder s (GenTx blk)

  -- | Construct transaction from mock transaction
  --
  -- When we run the demo, for convenience we submit mock transactions from
  -- the command line. These then need to be translated to "real" transactions
  -- for the ledger that we are running. Of course, this translation will
  -- necessarily be limited and will rely on things like 'generatedSecrets'.
  demoMockTx :: NodeConfig (BlockProtocol blk) -> Tx -> GenTx blk
