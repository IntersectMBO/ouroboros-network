{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeOperators     #-}

module Test.Util.BoolProps (
    CollectReqs (..)
  , Prereq (..)
  , Requirement (..)
  , checkReqs
  , enabledIf
  , gCollectReqs
  , requiredIf
  ) where

import           Data.Kind (Type)
import           GHC.Generics

{-------------------------------------------------------------------------------
  Generic boolean properties
-------------------------------------------------------------------------------}

-- | A prerequisite
--
-- If any prereq is 'Blocked', then the ultimate observation must be @False@.
--
-- See 'Requirement'.
data Prereq = Blocked | Enabled
  deriving (Eq, Show)

enabledIf :: Bool -> Prereq
enabledIf b = if b then Enabled else Blocked

-- | A requirement
--
-- If all prereqs are 'Enabled' and all reqs are 'Required' then the ultimate
-- observation must be @True@.
data Requirement = Optional | Required
  deriving (Eq, Show)

requiredIf :: Bool -> Requirement
requiredIf b = if b then Required else Optional

-- | Collect all of the 'Prereq's and 'Requirement's from a composite type
--
-- The default definition uses "GHC.Generics" to automatically handle algebraic
-- types containing only 'Prereq', 'Requirement', 'Bool', lists, and small
-- tuples.
--
-- Note that 'collectReqs' ignores 'Bool's. It's up to the user to interpret
-- 'Bool's as either disjunctive\/conjunctive\/etc observations.
class CollectReqs a where
  collectReqs :: a -> ([Prereq], [Requirement])

  default collectReqs :: (Generic a, GCollectReqs (Rep a))
                      => a -> ([Prereq], [Requirement])
  collectReqs = gCollectReqs . from

instance CollectReqs Bool where
  collectReqs = mempty

instance CollectReqs a => CollectReqs [a] where
  collectReqs = foldMap collectReqs

instance (CollectReqs a, CollectReqs b) => CollectReqs (a, b) where
  collectReqs (a, b) = collectReqs a <> collectReqs b

instance (CollectReqs a, CollectReqs b, CollectReqs c)
      => CollectReqs (a, b, c) where
  collectReqs (a, b, c) = collectReqs a <> collectReqs b <> collectReqs c

instance (CollectReqs a, CollectReqs b, CollectReqs c, CollectReqs d)
      => CollectReqs (a, b, c, d) where
  collectReqs (a, b, c, d) =
      collectReqs a <> collectReqs b <> collectReqs c <> collectReqs d

instance CollectReqs Requirement where
  collectReqs req = ([], [req])

instance CollectReqs Prereq where
  collectReqs prereq = ([prereq], [])

-- | Via 'CollectReqs', check if the ultimate observation has a required value
checkReqs :: CollectReqs a => a -> Maybe Bool
checkReqs x
      | Blocked  `elem` prereqs = Just False
      | Optional `elem` reqs    = Nothing
      | otherwise               = Just True
    where
      (prereqs, reqs) = collectReqs x

{-------------------------------------------------------------------------------
  Generic boolean properties, generically
-------------------------------------------------------------------------------}

class GCollectReqs rep where
  gCollectReqs :: rep (x :: Type) -> ([Prereq], [Requirement])

instance GCollectReqs U1 where
  gCollectReqs U1 = mempty

instance GCollectReqs rep => GCollectReqs (M1 c meta rep) where
  gCollectReqs (M1 rep) = gCollectReqs rep

instance (GCollectReqs rep1, GCollectReqs rep2)
      => GCollectReqs (rep1 :*: rep2) where
  gCollectReqs (rep1 :*: rep2) = gCollectReqs rep1 <> gCollectReqs rep2

instance (GCollectReqs rep1, GCollectReqs rep2)
      => GCollectReqs (rep1 :+: rep2) where
  gCollectReqs = \case
      L1 rep -> gCollectReqs rep
      R1 rep -> gCollectReqs rep

instance CollectReqs c => GCollectReqs (K1 meta c) where
  gCollectReqs (K1 c) = collectReqs c
