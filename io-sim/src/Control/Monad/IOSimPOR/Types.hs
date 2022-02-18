module Control.Monad.IOSimPOR.Types where

import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Monad.IOSim.CommonTypes

--
-- Effects
--

-- | An `Effect` aggregates effects performed by a thread.  Only used by
-- *IOSimPOR*.
--
data Effect = Effect {
    effectReads  :: !(Set TVarId),
    effectWrites :: !(Set TVarId),
    effectForks  :: !(Set ThreadId),
    effectLiftST :: !Bool,
    effectThrows :: ![ThreadId],
    effectWakeup :: ![ThreadId]
  }
  deriving (Eq, Show)

instance Semigroup Effect where
  Effect r w s b ts wu <> Effect r' w' s' b' ts' wu' =
    Effect (r<>r') (w<>w') (s<>s') (b||b') (ts++ts') (wu++wu')

instance Monoid Effect where
  mempty = Effect Set.empty Set.empty Set.empty False [] []

-- readEffect :: SomeTVar s -> Effect
-- readEffect r = mempty{effectReads = Set.singleton $ someTvarId r }

readEffects :: [SomeTVar s] -> Effect
readEffects rs = mempty{effectReads = Set.fromList (map someTvarId rs)}

-- writeEffect :: SomeTVar s -> Effect
-- writeEffect r = mempty{effectWrites = Set.singleton $ someTvarId r }

writeEffects :: [SomeTVar s] -> Effect
writeEffects rs = mempty{effectWrites = Set.fromList (map someTvarId rs)}

forkEffect :: ThreadId -> Effect
forkEffect tid = mempty{effectForks = Set.singleton $ tid}

liftSTEffect :: Effect
liftSTEffect = mempty{ effectLiftST = True }

throwToEffect :: ThreadId -> Effect
throwToEffect tid = mempty{ effectThrows = [tid] }

wakeupEffects :: [ThreadId] -> Effect
wakeupEffects tids = mempty{effectWakeup = tids}

someTvarId :: SomeTVar s -> TVarId
someTvarId (SomeTVar r) = tvarId r

onlyReadEffect :: Effect -> Bool
onlyReadEffect e = e { effectReads = effectReads mempty } == mempty

racingEffects :: Effect -> Effect -> Bool
racingEffects e e' =
      (effectLiftST e  && racesWithLiftST e')
   || (effectLiftST e' && racesWithLiftST e )
   || effectThrows e `intersectsL` effectThrows e'
   || effectReads  e `intersects`  effectWrites e'
   || effectWrites e `intersects`  effectReads  e'
   || effectWrites e `intersects`  effectWrites e'
  where
    intersects :: Ord a => Set a -> Set a -> Bool
    intersects a b = not $ a `Set.disjoint` b

    intersectsL :: Eq a => [a] -> [a] -> Bool
    intersectsL a b = not $ null $ a `List.intersect` b

    racesWithLiftST eff =
         effectLiftST eff
      || not (Set.null (effectReads eff) && Set.null (effectWrites eff))

