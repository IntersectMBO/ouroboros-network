{-# LANGUAGE FlexibleContexts #-}

module Test.ThreadNet.Util.SimpleBlock (
  prop_validSimpleBlock,
  ) where

import           Test.QuickCheck

import           Ouroboros.Network.Block (HasHeader, SlotNo (..), blockSlot)

import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Util.Condense (condense)

prop_validSimpleBlock
  :: HasHeader (SimpleBlock' c ext ext')
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
