{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}

module Test.ThreadNet.Rekeying (
    Rekeying (..)
  , fromRekeyingToRekeyM
  ) where

import           Data.Functor ((<&>))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.IOLike

import           Test.ThreadNet.Network

import           Test.Util.Stream

-- | Functionality used by test node in order to update its operational key
--
-- This is the conceptual interface demanded from the test-specific logic. It
-- is used to define 'tnaRekeyM', which the test infrastructure invokes per the
-- 'NodeRestarts' schedule.
data Rekeying m blk = forall opKey. Rekeying
  { rekeyOracle
      :: CoreNodeId -> SlotNo -> Maybe SlotNo
    -- ^ The first /nominal/ slot after the given slot, assuming the given core
    -- node cannot lead.
    --
    -- IE the first slot that will result in a block successfully being forged
    -- and diffused (eg no @PBftExceededSignThreshold@).
  , rekeyUpd ::
         CoreNodeId
      -> ProtocolInfo m blk
      -> EpochNo
      -> opKey
      -> m (Maybe (TestNodeInitialization m blk))
     -- ^ new config and any corresponding delegation certificate transactions
     --
     -- The given epoch contains the first nominal slot whose block will
     -- include the redelegation certificate transaction.
     --
     -- The 'TestNodeInitialization' includes the new 'ProtocolInfo' used when
     -- the node completes restarting.
  , rekeyFreshSKs :: Stream opKey
     -- ^ a stream that only repeats itself after an *effectively* *infinite*
     -- number of iterations and also never includes an operational key from
     -- the genesis configuration
  }

fromRekeyingToRekeyM :: IOLike m => Rekeying m blk -> m (RekeyM m blk)
fromRekeyingToRekeyM Rekeying{rekeyFreshSKs, rekeyOracle, rekeyUpd} = do
    rekeyVar <- uncheckedNewTVarM rekeyFreshSKs
    pure $ \cid pInfo s mkEno -> case rekeyOracle cid s of
      Nothing -> pure $ plainTestNodeInitialization pInfo
      Just s' -> do
        x <- atomically $ do
          x :< xs <- readTVar rekeyVar
          x <$ writeTVar rekeyVar xs
        eno <- mkEno s'
        rekeyUpd cid pInfo eno x <&> \case
          Nothing  -> plainTestNodeInitialization pInfo
          Just tni -> tni
