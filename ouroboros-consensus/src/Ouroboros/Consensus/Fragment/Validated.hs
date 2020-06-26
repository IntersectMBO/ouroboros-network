{-# LANGUAGE DeriveFunctor       #-}
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
  , validatedTip
  , new
  ) where

import           GHC.Stack

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Assert

-- | Validated chain fragment along with the ledger state after validation
--
-- INVARIANT:
--
-- > AF.headPoint validatedFragment == ledgerTipPoint validatedLedger
data ValidatedFragment blk l = ValidatedFragment {
      -- | Chain fragment
      validatedFragment :: !(AnchoredFragment (Header blk))

      -- | Ledger after after validation
    , validatedLedger   :: !l
    }
  deriving (Functor)

validatedTip :: HasHeader (Header blk) => ValidatedFragment blk l -> Point blk
validatedTip = castPoint . AF.headPoint . validatedFragment

invariant :: forall l blk. ApplyBlock l blk
          => ValidatedFragment blk l -> Either String ()
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
    -> ValidatedFragment blk l
new fragment ledger =
    assertWithMsg (invariant validated) $
      validated
  where
    validated :: ValidatedFragment blk l
    validated = ValidatedFragment {
          validatedFragment = fragment
        , validatedLedger   = ledger
        }
