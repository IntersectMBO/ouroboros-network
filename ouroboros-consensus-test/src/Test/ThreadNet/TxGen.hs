{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Transaction generator for testing
module Test.ThreadNet.TxGen (
    TxGen (..)
    -- * Implementation for HFC
  , WrapTxGenExtra (..)
  , testGenTxsHfc
  ) where

import           Data.Kind (Type)
import           Data.SOP.Strict

import           Test.QuickCheck (Gen)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
                     (Flip (..))

{-------------------------------------------------------------------------------
  TxGen class
-------------------------------------------------------------------------------}

class TxGen blk where

  -- | Extra information required to generate transactions
  type TxGenExtra blk :: Type
  type TxGenExtra blk = ()

  -- | Generate a number of transactions, valid or invalid, that can be
  -- submitted to a node's Mempool.
  --
  -- This function will be called to generate transactions in consensus tests.
  --
  -- Note: this function returns a list so that an empty list can be returned
  -- in case we are unable to generate transactions for a @blk@.
  testGenTxs :: CoreNodeId
             -> NumCoreNodes
             -> SlotNo
             -> TopLevelConfig blk
             -> TxGenExtra blk
             -> LedgerState blk ValuesMK
             -> Gen [GenTx blk]

{-------------------------------------------------------------------------------
  Implementation for HFC
-------------------------------------------------------------------------------}

-- | Newtypes wrapper around the 'TxGenExtra' type family so that it can be
-- partially applied.
newtype WrapTxGenExtra blk = WrapTxGenExtra {
      unwrapTxGenExtra :: TxGenExtra blk
    }

-- | Function that can be used for 'TxGen' instances for 'HardForkBlock'.
--
-- We don't provide a generic instance of 'TxGen' because it might be desirable
-- to provide custom implementations for specific instantiations of the eras of
-- 'HardForkBlock'. Instead, we provide this function that can be used when a
-- generic implemenation is desired.
--
-- Choose @NP WrapTxGenExtra xs@ for the instance of the 'TxGenExtra' type
-- family, where @xs@ matches the concrete instantiation.
testGenTxsHfc ::
     forall xs. (All TxGen xs, CanHardFork xs)
  => CoreNodeId
  -> NumCoreNodes
  -> SlotNo
  -> TopLevelConfig (HardForkBlock xs)
  -> NP WrapTxGenExtra xs
  -> LedgerState (HardForkBlock xs) ValuesMK
  -> Gen [GenTx (HardForkBlock xs)]
testGenTxsHfc coreNodeId numCoreNodes curSlotNo cfg extras state =
    hcollapse $
    hcizipWith3
      (Proxy @TxGen)
      aux
      cfgs
      extras
      (State.tip (hardForkLedgerStatePerEra state))
  where
    cfgs = distribTopLevelConfig ei cfg
    ei   = State.epochInfoLedger
             (configLedger cfg)
             (hardForkLedgerStatePerEra state)

    aux ::
         forall blk. TxGen blk
      => Index xs blk
      -> TopLevelConfig blk
      -> WrapTxGenExtra blk
      -> Flip LedgerState ValuesMK blk
      -> K (Gen [GenTx (HardForkBlock xs)]) blk
    aux index cfg' (WrapTxGenExtra extra') (Flip state') = K $
        fmap (injectNS' (Proxy @GenTx) index)
          <$> testGenTxs coreNodeId numCoreNodes curSlotNo cfg' extra' state'
