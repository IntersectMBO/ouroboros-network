{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wwarn -fshow-valid-hole-fits #-}
module Main (main) where

import           Prelude hiding (lines)

import qualified Control.Foldl as L

import           Data.Map.Strict (Map)
import           Data.Maybe (mapMaybe)
import           Data.Profunctor
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text (hGetContents)
import qualified Data.Text.Lazy.Read as Read
import           Data.Time (DiffTime)
import           System.IO (IOMode (ReadMode), withFile)

type Slot = Int

data Result value = Result {
      applyChainTick     :: !value
    , tickHeaderState    :: !value
    , applyLedgerBlock   :: !value
    , reapplyLedgerBlock :: !value
    , validateHeader     :: !value
    , revalidateHeader   :: !value
    }
  deriving (Show, Functor, Foldable, Traversable)

instance Applicative Result where
  pure a = Result a a a a a a
  Result f1 f2 f3 f4 f5 f6 <*> Result a1 a2 a3 a4 a5 a6 =
    Result (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6)

mainAnalysis :: L.Fold (Slot, Result DiffTime)
                       (Pair
                         (Result DiffTime)
                         (Map Ordering (Result DiffTime)))
mainAnalysis = Pair
    <$> (lmap snd (L.nest (meanWithoutOutliers 10)))
    <*> L.groupBy ((`compare` firstShelleySlot) . fst) (lmap snd (L.nest L.mean))

data Pair a b = Pair !a !b
  deriving (Show)

-- | Ignore values that are @x@ times larger than the current mean. We call @x@
-- the /outlier factor.
--
-- NOTE: be careful with this: if the first value is very tiny, then all
-- subsequent values will be considered outliers.
meanWithoutOutliers ::
     (Fractional a, Ord a)
  => a  -- ^ Outlier factor
  -> L.Fold a a
meanWithoutOutliers outlierFactor = L.Fold step begin done
  where
    begin = Pair 0 0
    step (Pair x n) y
        | x /= 0, y > x * outlierFactor
        = Pair x n
        | otherwise
        = let n' = n+1 in Pair (x + (y - x) /n') n'
    done (Pair x _) = x

firstShelleySlot :: Slot
firstShelleySlot = 208 * 21600

isShelleyEpochTransition :: Slot -> Bool
isShelleyEpochTransition s
    | let inShelleySlot = s - firstShelleySlot
    , inShelleySlot > 0
    = inShelleySlot `mod` 43200 == 0
    | otherwise
    = False

parseResult :: Text -> Maybe (Slot, Result DiffTime)
parseResult line = case Text.splitOn "," line of
    [s, a, b, c, d, e, f] -> toMaybe $ do
        slot <- parse Read.decimal s
        result <- traverse (parse Read.rational) (Result a b c d e f)
        return (slot, result)

    _ -> Nothing
  where
    parse :: Read.Reader a -> Text -> Either String a
    parse rdr txt = fst <$> rdr txt

    toMaybe :: Either e a -> Maybe a
    toMaybe (Left _)  = Nothing
    toMaybe (Right a) = Just a

analyse :: Show a => FilePath -> L.Fold (Slot, Result DiffTime) a -> IO ()
analyse file analysis = withFile file ReadMode $ \h -> do
    conclusion <-
        L.fold analysis
      . mapMaybe parseResult
      . Text.lines
      <$> Text.hGetContents h
    print conclusion

main :: IO ()
main = analyse "ledger-validation.log" mainAnalysis
