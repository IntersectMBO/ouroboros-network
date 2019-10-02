{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module DeltaQ.OutcomesTypes where

-- TODO I don't think we actually need this

import Data.Set

class QualityAttenuationMeasure a where
  bottom     :: a
  perfection :: a
  isBottom   :: a -> Bool

class ( QualityAttenuationMeasure (QAMeasure a)
      , Eq (NodeName a) )
      => TomographyGraph a where
 type QAMeasure a :: *
 type NodeName  a :: *
      
 linkAttenutation  :: a -> NodeName a -> NodeName a -> QAMeasure a
 nodes             :: a -> Set (NodeName a)


-- | Is a bi-directional (non-bottom ∆Q in each direction) present?
biDirectionalLinkPresent :: (TomographyGraph a)
                         => a
                         -> NodeName a
                         -> NodeName a
                         -> Bool
biDirectionalLinkPresent tg x y
  | isBottom $ linkAttenutation tg x y
    = False
  | otherwise
    = not . isBottom $ linkAttenutation tg y x

-- | Find all the non-bottom links that have bottom in the opposite direction.
findUnidirectionalLinks :: (TomographyGraph a) => a -> [(NodeName a, NodeName a)]
findUnidirectionalLinks a
  = undefined -- check cartesian product, order result by numbering, filter to nub based on numbering
