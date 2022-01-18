{-# LANGUAGE TypeFamilies #-}
module HasAnalysis (
    HasAnalysis (..)
  , HasProtocolInfo (..)
  , SizeInBytes
  , TxIn (..)
  , TxOutputIds (..)
  , mkTxOutputIds
  , WithLedgerState (..)
  ) where

import           Data.ByteString.Short (ShortByteString)
import           Data.ByteString.Short.Base64 (encodeBase64)
import           Data.Map.Strict (Map)
import qualified Data.Text.Short as TextShort
import           Data.Word (Word32, Word64)
import           Options.Applicative

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Storage.Serialisation (SizeInBytes)

{-------------------------------------------------------------------------------
  HasAnalysis
-------------------------------------------------------------------------------}

data WithLedgerState blk = WithLedgerState
  { wlsBlk         :: blk
  , wlsStateBefore :: LedgerState blk
  , wlsStateAfter  :: LedgerState blk
  }

-- | serialization of a transaction id (eg
-- 'Ouroboros.Consensus.Ledger.SupporstMempool.TxId')
--
-- That serialization is simply the hash of the identified transaction, and, as
-- of 2022 Jan, every Cardano transaction hash has always been exactly 32
-- bytes. So that's the expected length of this field.
type TxIdBytes = ShortByteString

showTxIdBytes :: TxIdBytes -> String
showTxIdBytes = TextShort.toString . encodeBase64

data TxIn = TxIn {
    txInTxId  :: !TxIdBytes
  , txInIndex :: !Word32
    -- ^ the index of this output within the sequence of outputs created by the
    -- transaction that created this output
  }

instance Show TxIn where
  show (TxIn bs i) = showTxIdBytes bs <> "@" <> show i

data TxOutputIds = TxOutputIds {
    txOutputIdsTxId  :: !TxIdBytes
    -- ^ the transaction id that created some outputs
  , txOutputIdsSizes :: [Word64]
    -- ^ the serialization size of each tx output it created
    --
    -- INVARIANT: >0
  }

mkTxOutputIds :: TxIdBytes -> [Int] -> Maybe TxOutputIds
mkTxOutputIds txid sizes =
    if null sizes then Nothing else Just $ TxOutputIds txid (map toEnum sizes)

instance Show TxOutputIds where
  show (TxOutputIds bs sizes) =
       showTxIdBytes bs <> "#" <> show (length sizes)
    <> concatMap ((',':) . show) sizes

class (HasAnnTip blk, GetPrevHash blk) => HasAnalysis blk where

  countTxOutputs         :: blk -> Int
  -- | Is it an EBB, how many transactions, which txins consumed, which txouts created
  extractTxOutputIdDelta :: blk -> (IsEBB, Int, [TxIn], [TxOutputIds])
  -- | How many transactions, which txouts created
  genesisTxOutputIds     :: LedgerState blk -> (Int, [TxOutputIds])
  blockTxSizes           :: blk -> [SizeInBytes]
  knownEBBs              :: proxy blk -> Map (HeaderHash blk) (ChainHash blk)

  -- | Emit trace markers at points in processing.
  emitTraces :: WithLedgerState blk -> [String]

class HasProtocolInfo blk where
  data Args blk
  argsParser     :: proxy blk -> Parser (Args blk)
  mkProtocolInfo :: Args blk -> IO (ProtocolInfo IO blk)
