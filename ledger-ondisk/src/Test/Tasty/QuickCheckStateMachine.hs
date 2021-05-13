{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS -fno-warn-unused-imports #-}
module Test.Tasty.QuickCheckStateMachine where

import Test.Tasty.Ingredients
import Test.Tasty.QuickCheck
import Test.Tasty.Options
import qualified Test.Tasty.Patterns.Types as Awk
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Function
import Data.Either
import Data.Tagged
import Test.Tasty.Runners
import Data.Proxy
import Control.Applicative
import Test.Tasty.Providers
import Test.StateMachine
import Test.StateMachine.Labelling
import Data.Coerce
import Data.Functor
import qualified Test.StateMachine.Types.Rank2 as Rank2
import Data.Monoid
import Test.StateMachine.Lockstep.NAry (StateMachineTest, prop_sequential, RealMonad)
import Test.Tasty
import Control.Monad.Trans.Cont

-- newtype TagFilter = TagFilter { unChosenTag :: Maybe (Awk.Expr) }
--   deriving stock (Eq, Show)

data TagFilter = TagFilter
  { mustHave :: Maybe (HashSet String)
  , cantHave :: HashSet String
  } | NoLabelling deriving (Eq, Show)


instance Semigroup TagFilter where
  tf1 <> tf2 = TagFilter mh ch where
    mh = case (mustHave tf1, mustHave tf2) of
      (Just x, Just y) -> Just $ x `HashSet.union` y
      (x, y) -> x <|> y
    ch = ((<>) `on` cantHave) tf1 tf2

instance Monoid TagFilter where
  mempty = TagFilter Nothing mempty

instance IsOption TagFilter where
  defaultValue = NoLabelling
  parseValue s = Just $ TagFilter (if null mh then Nothing else Just mh) ch where
    (HashSet.fromList -> mh, HashSet.fromList -> ch) = partitionEithers $
      [ x
      | Just x <-
        [ case w of
            '!' : xs@(_ : _) -> Just $ Right xs
            xs@(_ : _) -> Just $ Left xs
            _ -> Nothing
        | w <- words s
        ]
      ]
  optionName = Tagged "quicheck-tags"
  optionHelp = Tagged "space separated list of tags that must, or (prefixed with !) must not, be matched"

newtype QSMMinSize = QSMMinSize (Maybe Int)
  deriving (Monoid, Semigroup) via (Last Int)
  deriving stock (Eq, Show)


instance IsOption QSMMinSize where
  defaultValue = mempty
  parseValue = fmap (QSMMinSize . pure) . safeRead
  optionName = Tagged "qsm-minsize"
  optionHelp = Tagged "The minimum length of generated command sequences"

-- labellingTests :: Monoid a => OptionSet -> TestTree -> (forall model cmd LabellingTest -> a) -> a
-- labellingTests os tree go = foldTestTree the_fold os tree where
--   relevant t = getAny . foldMap (maybe mempty go) . cast $ t where
--   foldSingle os n t
--     | relevant t = pure . go $ t
--     | otherwise = mempty
--   the_fold = trivialFold {foldSingle}

data LabellingTest = forall model cmd m resp. (Show (model Symbolic), Show (cmd Symbolic), Show (resp Symbolic), Rank2.Traversable cmd, Rank2.Foldable resp) => LabellingTest
   { stateMachine :: StateMachine model cmd m resp
   , labelEvents :: [Event model cmd resp Symbolic] -> [String]
   }


instance IsTest LabellingTest where
  run os LabellingTest{..} _ = case lookupOption os of
    NoLabelling -> undefined
    TagFilter{..} -> showLabelledExamples' stateMachine mb_replay n labelEvents the_filter $> testPassed "dougrulz" 
      where
        QuickCheckReplay mb_replay = lookupOption os
        QuickCheckTests n = lookupOption os
        the_filter s = maybe True (s `HashSet.member`) mustHave && not (s `HashSet.member` cantHave)

  testOptions = coerce $
    [ Option $ Proxy @ TagFilter
    , Option $ Proxy @ QuickCheckReplay
    , Option $ Proxy @ QuickCheckTests
    , Option $ Proxy @ QSMMinSize
    ]

labellingTest :: forall model cmd m resp. (Show (model Symbolic), Show (cmd Symbolic), Show (resp Symbolic), Rank2.Traversable cmd, Rank2.Foldable resp) => StateMachine model cmd m resp -> ([Event model cmd resp Symbolic] -> [String]) -> LabellingTest
labellingTest  = LabellingTest

newtype QSMTest = QSMTest { unQSMTest :: QC }

instance IsTest QSMTest where
  run os QSMTest{..} = run os unQSMTest

  testOptions = coerce $
    [ Option $ Proxy @ QSMMinSize
    ] ++ (coerce $ testOptions @ QC)
  

testQSM :: (RealMonad t ~ IO) => TestName -> Cont Property (StateMachineTest t) -> TestTree
testQSM name smt_m = askOption $ \(QSMMinSize mb_sz) ->
  singleTest name . QSMTest . QC . runCont smt_m $ \smt -> prop_sequential smt mb_sz
