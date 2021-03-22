{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Fragment.ValidatedDiff (ValidatedChainDiff (..))
-- > import qualified Ouroboros.Consensus.Fragment.ValidatedDiff as ValidatedDiff
module Ouroboros.Consensus.Fragment.ValidatedDiff (
    ValidatedChainDiff (ValidatedChainDiff)
  , getChainDiff
  , getLedger
  , new
  , rollbackExceedsSuffix
  , toValidatedFragment
  ) where

import           Control.Monad.Except (throwError)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Fragment.Diff (ChainDiff)
import qualified Ouroboros.Consensus.Fragment.Diff as Diff
import           Ouroboros.Consensus.Fragment.Validated (ValidatedFragment)
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Assert

-- | A 'ChainDiff' along with the ledger state after validation.
--
-- INVARIANT:
--
-- > getTip chainDiff == ledgerTipPoint ledger
data ValidatedChainDiff b l = UnsafeValidatedChainDiff
    { getChainDiff :: ChainDiff b
    , getLedger    :: l
    }

-- | Allow for pattern matching on a 'ValidatedChainDiff' without exposing the
-- (unsafe) constructor. Use 'new' to construct a 'ValidatedChainDiff'.
pattern ValidatedChainDiff
  :: ChainDiff b -> l -> ValidatedChainDiff b l
pattern ValidatedChainDiff d l <- UnsafeValidatedChainDiff d l
{-# COMPLETE ValidatedChainDiff #-}

-- | Create a 'ValidatedChainDiff'.
--
-- PRECONDITION:
--
-- > getTip chainDiff == ledgerTipPoint ledger
new ::
     forall b l.
     (IsLedger l, HasHeader b, HeaderHash l ~ HeaderHash b, HasCallStack)
  => ChainDiff b
  -> l
  -> ValidatedChainDiff b l
new chainDiff ledger =
    assertWithMsg precondition $
    UnsafeValidatedChainDiff chainDiff ledger
  where
    chainDiffTip, ledgerTip :: Point b
    chainDiffTip = Diff.getTip chainDiff
    ledgerTip    = castPoint $ getTip ledger
    precondition
      | chainDiffTip == ledgerTip
      = return ()
      | otherwise
      = throwError $
          "tip of ChainDiff doesn't match ledger: " <>
          show chainDiffTip <> " /= " <> show ledgerTip

toValidatedFragment
  :: (IsLedger l, HasHeader b, HeaderHash l ~ HeaderHash b, HasCallStack)
  => ValidatedChainDiff b l
  -> ValidatedFragment b l
toValidatedFragment (UnsafeValidatedChainDiff cs l) =
    VF.ValidatedFragment (Diff.getSuffix cs) l

rollbackExceedsSuffix :: HasHeader b => ValidatedChainDiff b l -> Bool
rollbackExceedsSuffix = Diff.rollbackExceedsSuffix . getChainDiff
