{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Fragment.Validated (ValidatedFragment)
-- > import qualified Ouroboros.Consensus.Fragment.Validated as VF
module Ouroboros.Consensus.Fragment.Validated (
    ValidatedFragment -- Opaque
  , validatedFragment
  , validatedLedger
  , new
  ) where

import           GHC.Stack

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Assert

-- | Validated chain fragment along with the ledger state after validation
--
-- INVARIANT:
--
-- > AF.headPoint validatedFragment == ledgerTipPoint validatedLedger
data ValidatedFragment l blk = ValidatedFragment {
    -- | Chain fragment
    validatedFragment :: !(AnchoredFragment (Header blk))

    -- | Ledger after after validation
  , validatedLedger   :: !l
  }

invariant :: forall l blk. ApplyBlock l blk
          => ValidatedFragment l blk -> Either String ()
invariant ValidatedFragment{..}
    | ledgerTip /= headPoint
    = Left $ concat [
          "ledger tip "
        , show ledgerTip
        , " /= head point "
        , show headPoint
        ]
    | otherwise
    = Right ()
  where
   ledgerTip, headPoint :: Point blk
   ledgerTip = ledgerTipPoint validatedLedger
   headPoint = castPoint $ AF.headPoint validatedFragment

-- | Constructor for 'ValidatedFragment' that checks the invariant
new :: forall l blk. (ApplyBlock l blk, HasCallStack)
    => AnchoredFragment (Header blk)
    -> l
    -> ValidatedFragment l blk
new fragment ledger =
    assertWithMsg (invariant validated) $
      validated
  where
    validated :: ValidatedFragment l blk
    validated = ValidatedFragment {
          validatedFragment = fragment
        , validatedLedger   = ledger
        }
