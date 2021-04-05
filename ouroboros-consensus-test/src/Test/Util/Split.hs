{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util.Split (
    spanLeft
  , spanLeft'
  , splitAtJust
  ) where

import           Data.Bifunctor (first)
import           Data.Word (Word64)

{-------------------------------------------------------------------------------
  spanLeft
-------------------------------------------------------------------------------}

-- | The returned @b@ is the first in the list.
--
-- INVARIANT The output data is a segmentation of the given list.
spanLeft
  :: forall x a b.
     (x -> Either a b) -> [x] -> ([a], Maybe (b, [x]))
spanLeft prj xs = (reverse acc, mbBxs)
  where
    (acc, mbBxs) = spanLeft' prj xs

-- | As 'spanLeft', but the @[a]@ is reversed.
spanLeft'
  :: forall x a b.
     (x -> Either a b) -> [x] -> ([a], Maybe (b, [x]))
spanLeft' prj = go []
  where
    go acc = \case
      []     -> (acc, Nothing)
      x : xs -> case prj x of
        Left a  -> go (a : acc) xs
        Right b -> (acc, Just (b, xs))

{-------------------------------------------------------------------------------
  splitAtJust
-------------------------------------------------------------------------------}

-- | INVARIANT: The second is a function of the first.
data Prj a b = Prj !a !b

-- | The returned @b@ is either the @n@th @b@ or else the last in the given
-- list.
--
-- INVARIANT The output data is a segmentation of the given list.
splitAtJust
  :: forall x b.
     (x -> Maybe b) -> Word64 -> [x] -> (Maybe ([x], b), [x])
splitAtJust prj = \n xs ->
  if 0 == n then (Nothing, xs)
  else case peel xs of
    (pre, Just (xb, xs')) -> Just `first` go pre xb (n - 1) xs'
    (_, Nothing)          -> (Nothing, xs)
  where
    peel :: [x] -> ([x], Maybe (Prj x b, [x]))
    peel = spanLeft' prj'
      where
        prj' x = case prj x of
          Nothing -> Left x
          Just b  -> Right (Prj x b)

    go pre (Prj x b) n xs
      | 0 == n    = ((reverse pre, b), xs)
      | otherwise = case peel xs of
      (pre', Nothing       ) -> ((reverse pre, b), reverse pre')
      (pre', Just (xb, xs')) -> go (pre' ++ x : pre) xb (n - 1) xs'
