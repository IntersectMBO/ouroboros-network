{-# LANGUAGE PatternSynonyms #-}
-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Fragment.ValidatedDiff (ValidatedChainDiff (..))
-- > import qualified Ouroboros.Consensus.Fragment.ValidatedDiff as ValidatedDiff
module Ouroboros.Consensus.Fragment.ValidatedDiff
  ( ValidatedChainDiff(ValidatedChainDiff)
  , getChainDiff
  , getLedger
  , new
  , toValidatedFragment
  ) where

import           Control.Monad.Except (throwError)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Fragment.Diff
import           Ouroboros.Consensus.Fragment.Validated (ValidatedFragment)
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Assert

-- | A 'ChainDiff' along with the ledger state after validation.
--
-- INVARIANT:
--
-- > getTip chainDiff == ledgerTipPoint ledger
data ValidatedChainDiff blk l = UnsafeValidatedChainDiff
    { getChainDiff :: ChainDiff blk
    , getLedger    :: l
    }

-- | Allow for pattern matching on a 'ValidatedChainDiff' without exposing the
-- (unsafe) constructor. Use 'new' to construct a 'ValidatedChainDiff'.
pattern ValidatedChainDiff
  :: ChainDiff blk -> l -> ValidatedChainDiff blk l
pattern ValidatedChainDiff d l <- UnsafeValidatedChainDiff d l
{-# COMPLETE ValidatedChainDiff #-}

-- | Create a 'ValidatedChainDiff'.
--
-- PRECONDITION:
--
-- > getTip chainDiff == ledgerTipPoint ledger
new
  :: (ApplyBlock l blk, HasCallStack)
  => ChainDiff blk
  -> l
  -> ValidatedChainDiff blk l
new chainDiff ledger =
    assertWithMsg precondition $
    UnsafeValidatedChainDiff chainDiff ledger
  where
    chainDiffTip = getTip chainDiff
    ledgerTip    = castPoint $ ledgerTipPoint ledger
    precondition
      | chainDiffTip == ledgerTip
      = return ()
      | otherwise
      = throwError $
          "tip of ChainDiff doesn't match ledger: " <>
          show chainDiffTip <> " /= " <> show ledgerTip

toValidatedFragment
  :: (ApplyBlock l blk, HasCallStack)
  => ValidatedChainDiff blk l
  -> ValidatedFragment blk l
toValidatedFragment (UnsafeValidatedChainDiff cs l) =
    VF.new (getSuffix cs) l
