{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.Fragment.Validated (ValidatedFragment)
-- > import qualified Ouroboros.Consensus.Fragment.Validated as VF
module Ouroboros.Consensus.Fragment.Validated (
    ValidatedFragment (ValidatedFragment)
  , validatedFragment
  , validatedLedger
  , validatedTip
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
data ValidatedFragment b l = UnsafeValidatedFragment {
      -- | Chain fragment
      validatedFragment :: !(AnchoredFragment b)

      -- | Ledger after after validation
    , validatedLedger   :: !l
    }
  deriving (Functor)

{-# COMPLETE ValidatedFragment #-}

pattern ValidatedFragment ::
     (IsLedger l, HasHeader b, HeaderHash b ~ HeaderHash l, HasCallStack)
  => AnchoredFragment b -> l -> ValidatedFragment b l
pattern ValidatedFragment f l <- UnsafeValidatedFragment f l
  where
    ValidatedFragment f l = new f l

validatedTip :: HasHeader b => ValidatedFragment b l -> Point b
validatedTip = AF.headPoint . validatedFragment

invariant ::
     forall l b.
     (IsLedger l, HasHeader b, HeaderHash b ~ HeaderHash l)
  => ValidatedFragment b l
  -> Either String ()
invariant (ValidatedFragment fragment ledger)
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
   ledgerTip = castPoint $ getTip ledger
   headPoint = castPoint $ AF.headPoint fragment

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
    validated = UnsafeValidatedFragment {
          validatedFragment = fragment
        , validatedLedger   = ledger
        }
