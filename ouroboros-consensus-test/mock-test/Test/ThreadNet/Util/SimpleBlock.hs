{-# LANGUAGE FlexibleContexts #-}

module Test.ThreadNet.Util.SimpleBlock (prop_validSimpleBlock) where

import           Data.Typeable
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Util.Condense (condense)
import           Test.QuickCheck

prop_validSimpleBlock
  :: (SimpleCrypto c, Typeable ext, Typeable ext')
  => SimpleBlock' c ext ext' -> Property
prop_validSimpleBlock blk = conjoin $ map each $ simpleTxs $ simpleBody blk
  where
    now :: SlotNo
    now = blockSlot blk

    msg :: String
    msg = "block contains expired transaction:"

    each :: Tx -> Property
    each tx@(Tx expiry _ins _outs) =
      counterexample (msg <> " " <> condense (now, tx)) $
      case expiry of
        DoNotExpire       -> True
        ExpireAtOnsetOf s -> now < s
