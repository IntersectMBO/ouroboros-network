{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.TipSample.TipFragment (tests) where

import           Control.Monad.Class.MonadTime

import qualified Data.Foldable as Foldable
import qualified Data.FingerTree.Strict as FT

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))

import           Ouroboros.Network.Block (Tip (..), getTipSlotNo)
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.TipSample.TipFragment ( TipFragment
                                                         , TipFragment' (Empty, (:<), (:>))
                                                         , Timed (..))
import qualified Ouroboros.Network.TipSample.TipFragment as TF

import           Ouroboros.Network.Testing.ConcreteBlock
import           Test.ChainFragment hiding (tests)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Ouroboros.Network.TipSample"
  [ testGroup "TipFragment"
    [ testProperty "toOldestFirst"         prop_toOldestFirst
    , testProperty "lookupBySlotFT"        prop_lookupBySlotFT
    , testProperty "lookupBySlotNo"        prop_lookupBySlotNo
    , testProperty "dropNewestUntilSlotNo" prop_dropNewestUntilSlotNo
    , testProperty "dropOldestUntilSlotNo" prop_dropOldestUntilSlotNo
    ]
  ]


newtype TestTipFragment = TestTipFragment (TipFragment BlockHeader)
  deriving Show

instance Arbitrary TestTipFragment where
    arbitrary = do
      TestHeaderChainFragment cf <- arbitrary
      (diffs :: [DiffTime])
        <- vectorOf (CF.length cf) (fromRational . getPositive <$> arbitrary)
      pure $ TestTipFragment
           $ fst
           $ Foldable.foldl'
               (\(tf, t) (hdr, diff) ->
                  let t' =  diff `addTime` t in
                  ( tf :>
                    Timed {
                      timedAt   = t',
                      timedData = Tip (headerSlot hdr) (headerHash hdr) (headerBlockNo hdr)
                    }
                  , t'))
                (Empty, Time 0)
                (zip (CF.toOldestFirst cf) diffs)

data TestTipFragmentAndSlotNo = TestTipFragmentAndSlotNo SlotNo (TipFragment BlockHeader)
  deriving Show

instance Arbitrary TestTipFragmentAndSlotNo where
    arbitrary = do
      TestTipFragment tf <- arbitrary
      let maxSlotNo =
            case TF.viewRight tf of
              FT.EmptyR -> 0
              _ FT.:> (Timed _ (Tip (SlotNo slotNo) _ _)) -> slotNo
              _ FT.:> (Timed _ TipGenesis) -> 0
      slotNo <-
        SlotNo <$>
          frequency
            -- 80% of cases the `SlotNo` will be smaller than the last slot in
            -- the generated 'TipFragment'
            [ (8, choose (0, maxSlotNo))
            , (2, choose (maxSlotNo, 2 * maxSlotNo))
            ]
      pure (TestTipFragmentAndSlotNo slotNo tf)


prop_toOldestFirst :: TestTipFragment
                   -> Bool
prop_toOldestFirst (TestTipFragment tf) =
    tf == foldr (:<) Empty (TF.toOldestFirst tf)


prop_lookupBySlotFT :: TestTipFragmentAndSlotNo
                    -> Bool
prop_lookupBySlotFT (TestTipFragmentAndSlotNo slotNo tf) =
    case TF.lookupBySlotFT (At slotNo) tf of
      FT.Position _ (Timed _ tip) _
                 -> getTipSlotNo tip >= At slotNo
      FT.OnLeft  -> False
      FT.OnRight -> At slotNo > lastSlotNo || isEmpty
      FT.Nowhere -> False
  where
    lastSlotNo = case tf of
      _ :> Timed _ tip -> getTipSlotNo tip
      Empty            -> Origin

    isEmpty = case tf of
      Empty -> True
      _     -> False


prop_lookupBySlotNo :: TestTipFragmentAndSlotNo
                    -> Bool
prop_lookupBySlotNo (TestTipFragmentAndSlotNo slotNo tf) =
    case TF.lookupBySlotNo (At slotNo) tf of
      Just (Timed _ tip, _) -> getTipSlotNo tip == At slotNo
      Nothing               -> not (At slotNo `elem` slots)
  where
    slots :: [WithOrigin SlotNo]
    slots = Foldable.foldr (\a as -> getTipSlotNo (timedData a) : as) [] tf


prop_dropNewestUntilSlotNo :: TestTipFragmentAndSlotNo
                           -> Bool
prop_dropNewestUntilSlotNo (TestTipFragmentAndSlotNo slotNo tf) =
       TF.toOldestFirst tf' == filter filterPred (TF.toOldestFirst tf)
    &&
        case tf' of
          _ :> Timed _ tip -> getTipSlotNo tip <= At slotNo
          Empty            -> At slotNo < firstSlotNo || isEmpty
  where
    tf' = TF.dropNewestUntilSlotNo slotNo tf

    filterPred :: Timed (Tip BlockHeader) -> Bool
    filterPred (Timed _ tip) = getTipSlotNo tip <= At slotNo

    firstSlotNo = case tf of
      Timed _ tip :< _ -> getTipSlotNo tip
      Empty            -> Origin

    isEmpty = case tf of
      Empty -> True
      _     -> False


prop_dropOldestUntilSlotNo :: TestTipFragmentAndSlotNo
                           -> Bool
prop_dropOldestUntilSlotNo (TestTipFragmentAndSlotNo slotNo tf) =
        TF.toOldestFirst tf' == filter filterPred (TF.toOldestFirst tf)
    &&
        case tf' of
          Timed _ tip :< _ -> getTipSlotNo tip >= At slotNo
          Empty            -> At slotNo > lastSlotNo || isEmpty
  where
    tf' = TF.dropOldestUntilSlotNo slotNo tf

    filterPred :: Timed (Tip BlockHeader) -> Bool
    filterPred (Timed _ tip) = getTipSlotNo tip >= At slotNo

    lastSlotNo = case tf of
      _ :> Timed _ tip -> getTipSlotNo tip
      Empty            -> Origin

    isEmpty = case tf of
      Empty -> True
      _     -> False
