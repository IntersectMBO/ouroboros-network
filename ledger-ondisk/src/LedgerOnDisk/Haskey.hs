{-# LANGUAGE LambdaCase #-}
-- |

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module LedgerOnDisk.Haskey where

import qualified Data.BTree.Pure as Haskey
import LedgerOnDisk.Diff
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Coerce
import qualified Data.BTree.Primitives.Key


applyDiffToPureBTree :: (Data.BTree.Primitives.Key.Key k, Semigroup v) => HashMap k (Diff v) -> Haskey.Tree k v -> Haskey.Tree k v
applyDiffToPureBTree diff_map tree = let
  (Endo (($ Map.empty) -> map_to_insert), Endo (($ []) -> to_delete)) = HashMap.foldMapWithKey go diff_map
  go k = coerce $ \case
    DChangeTo v -> ( Map.insert k v, id)
    DRemove -> (id , (:) k)
    DMappend v -> (Map.insert k (maybe v (<> v) $ Haskey.lookup k tree), id)
    DNoChange -> (id, id)
  in Haskey.insertMany map_to_insert $ foldr Haskey.delete tree to_delete
