{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

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
data ValidatedFragment b l = ValidatedFragment {
      -- | Chain fragment
      validatedFragment :: !(AnchoredFragment b)

      -- | Ledger after after validation
    , validatedLedger   :: !l
    }
  deriving (Functor)

validatedTip :: HasHeader b => ValidatedFragment b l -> Point b
validatedTip = AF.headPoint . validatedFragment

invariant ::
     forall l b.
     (IsLedger l, HasHeader b, HeaderHash b ~ HeaderHash l)
  => ValidatedFragment b l
  -> Either String ()
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
   ledgerTip, headPoint :: Point b
   ledgerTip = castPoint $ getTip validatedLedger
   headPoint = castPoint $ AF.headPoint validatedFragment

-- | Constructor for 'ValidatedFragment' that checks the invariant
new ::
     forall l b.
     (IsLedger l, HasHeader b, HeaderHash b ~ HeaderHash l, HasCallStack)
  => AnchoredFragment b
  -> l
  -> ValidatedFragment b l
new fragment ledger =
    assertWithMsg (invariant validated) $
      validated
  where
    validated :: ValidatedFragment b l
    validated = ValidatedFragment {
          validatedFragment = fragment
        , validatedLedger   = ledger
        }
