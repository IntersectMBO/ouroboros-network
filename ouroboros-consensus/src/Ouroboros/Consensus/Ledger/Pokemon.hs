module Ouroboros.Consensus.Ledger.Pokemon where

import Cardano.Slotting
import Control.State.Transition.Simple
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Mock.Block
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.Praos
import Pokemon.Ledger.Tx hiding (LedgerState)
import qualified Pokemon.Ledger.Tx as Poke

data NullCrypto

data PokeBlock
  = PokeBlock
      { pbHeader :: PokeHeader,
        pbBody :: [Tx]
      }

-- Support the basic chain operations

instance GetHeader PokeBlock where

  data Header PokeBlock
    = PokeHeader
        { -- | The header hash
          --
          -- This is the hash of the header itself. This is a bit unpleasant,
          -- because it makes the hash look self-referential (when computing the
          -- hash we must ignore the 'simpleHeaderHash' field). However, the benefit
          -- is that we can give a 'HasHeader' instance that does not require
          -- a (static) 'Serialise' instance.
          simpleHeaderHash :: HeaderHash PokeBlock,
          -- | Fields required for the 'HasHeader' instance
          simpleHeaderStd :: SimpleStdHeader c ()
        }
    deriving (Generic, Show, Eq, NoUnexpectedThunks)

  getHeader = pbHeader

data SimplePokeHeader
  = SimplePokeHeader
      { simplePrev :: ChainHash PokeBlock,
        simpleSlotNo :: SlotNo,
        simpleBlockNo :: BlockNo,
        simpleBodyHash :: Hash Int [Tx]
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise, NoUnexpectedThunks)

instance HasHeader PokeHeader where

  blockHash = simpleHeaderHash

  blockPrevHash = simplePrev . simpleHeaderStd

  blockSlot = simpleSlotNo . simpleHeaderStd

  blockNo = simpleBlockNo . simpleHeaderStd

  blockInvariant = const True

type instance
  BlockProtocol PokeBlock =
    Praos (LedgerConfig PokeBlock) NullCrypto

instance UpdateLedger PokeBlock where

  data LedgerState PokeBlock = PokeState
    { psLedgerState :: Poke.LedgerState
    , psslot :: SlotNo
    }

  type LedgerError PokeBlock = PredicateFailure TXS

  data LedgerConfig PokeBlock
    = PokeStaticConfig PublicKey -- Oak's public key

  ledgerConfigView = praosExtConfig

  applyChainTick _ _ = TickedLedgerState

  applyLedgerBlock
    (PokeStaticConfig oakKey)
    block
    ls = liftEither
      $ applySTS @TXS $ TRC (oakKey, ls, pbBody block)

  reapplyLedgerBlock
    (PokeStaticConfig oakKey)
    block
    ls = fromRight
      $ applySTS @TXS $ TRC (oakKey, ls, pbBody block)
