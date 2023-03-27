{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial (
  tests,
  ) where

import           Control.Applicative ((<|>))
import qualified Control.Monad.Except as Exn
import           Data.Functor ((<&>))
import           Data.Functor.Identity (runIdentity)
import           Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import           Data.Proxy (Proxy (Proxy))
import           Data.Word (Word8)
import qualified System.Random as R
import qualified System.Random.Stateful as R
import qualified System.Timeout as IO (timeout)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Adversarial as A
import qualified Test.Ouroboros.Consensus.ChainGenerator.BitVector as BV
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc, Delta (Delta), Kcp (Kcp), Len (Len), Scg (Scg), ascFromBits)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Honest as H
import qualified Test.Ouroboros.Consensus.ChainGenerator.RaceIterator as RI
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some
import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.Honest as H
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot (E (ActiveSlotE, SlotE))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Random (QCGen)
import qualified Test.Tasty as TT
import qualified Test.Tasty.QuickCheck as TT

-----

tests :: [TT.TestTree]
tests = [
    TT.testProperty "prop_adversarialChain" prop_adversarialChain
  ,
    TT.testProperty "prop_adversarialChainMutation" prop_adversarialChainMutation
  ]

-----

data SomeTestAdversarial =
    forall base hon.
    SomeTestAdversarial
        !(Proxy base)
        !(Proxy hon)
        !(TestAdversarial base hon)

instance Show SomeTestAdversarial where
    showsPrec p (SomeTestAdversarial base hon test) =
        Some.runShowsPrec p
      $ Some.showCtor SomeTestAdversarial "SomeTestAdversarial"
            `Some.showArg` base
            `Some.showArg` hon
            `Some.showArg` test

instance Read SomeTestAdversarial where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor SomeTestAdversarial "SomeTestAdversarial"
            <*> Some.readArg
            <*> Some.readArg
            <*> Some.readArg

data TestAdversarial base hon = TestAdversarial {
    testAscA     :: !Asc
  ,
    testAscH     :: !Asc
  ,
    testRecipeA  :: !(A.AdversarialRecipe base hon)
  ,
    testRecipeA' :: !(A.SomeCheckedAdversarialRecipe base hon)
  ,
    testRecipeH  :: !H.HonestRecipe
  ,
    testRecipeH' :: !(H.CheckedHonestRecipe base hon)
  ,
    testSeedH    :: !QCGen
  }
  deriving (Read, Show)

genArPrefix :: H.ChainSchedule base hon -> QC.Gen (C.Var hon ActiveSlotE)
genArPrefix schedH =
    if C.toVar numChoices < 2 then pure (C.Count 0) {- can always pick genesis -} else do
        g <- QC.arbitrary
        pure $ C.toVar $ R.runSTGen_ (g :: QCGen) $ C.uniformIndex numChoices
  where
    H.ChainSchedule _slots v = schedH

    numChoices = pc C.- 1   -- can't pick the last active slot

    -- 'H.uniformTheHonestChain' ensures 0 < pc
    pc = BV.countActivesInV S.notInverted v

instance QC.Arbitrary SomeTestAdversarial where
    arbitrary = H.unsafeMapSuchThatJust $ do
        H.TestHonest {
            H.testAsc     = testAscH
          ,
            H.testRecipe  = testRecipeH
          ,
            H.testRecipe' = someTestRecipeH'
          } <- QC.arbitrary

        H.SomeCheckedHonestRecipe Proxy Proxy testRecipeH' <- pure someTestRecipeH'

        testSeedH <- QC.arbitrary

        let arHonest = H.uniformTheHonestChain (Just testAscH) testRecipeH' testSeedH

        arPrefix <- genArPrefix arHonest

        let H.HonestRecipe kcp scg delta _len = testRecipeH

            testRecipeA = A.AdversarialRecipe {
                A.arPrefix
              ,
                A.arParams = (kcp, scg, delta)
              ,
                A.arHonest
              }

        testAscA <- ascFromBits <$> QC.choose (1 :: Word8, maxBound - 1)

        case Exn.runExcept $ A.checkAdversarialRecipe testRecipeA of
            Left e -> case e of
                A.NoSuchAdversarialBlock -> pure Nothing
                A.NoSuchCompetitor       -> error $ "impossible! " <> show e
                A.NoSuchIntersection     -> error $ "impossible! " <> show e

            Right testRecipeA' -> do
                pure $ Just $ SomeTestAdversarial Proxy Proxy $ TestAdversarial {
                    testAscA
                  ,
                    testAscH
                  ,
                    testRecipeA
                  ,
                    testRecipeA'
                  ,
                    testRecipeH
                  ,
                    testRecipeH'
                  ,
                    testSeedH
                  }

-- | No seed exists such that each 'A.checkAdversarialChain' rejects the result of 'A.uniformAdversarialChain'
prop_adversarialChain :: SomeTestAdversarial -> QCGen -> QC.Property
prop_adversarialChain someTestAdversarial testSeedA = runIdentity $ do
    SomeTestAdversarial Proxy Proxy TestAdversarial {
        testAscA
      ,
        testRecipeA
      ,
        testRecipeA'
      } <- pure someTestAdversarial
    A.SomeCheckedAdversarialRecipe Proxy recipeA' <- pure testRecipeA'

    let A.AdversarialRecipe { A.arHonest = schedH } = testRecipeA

        schedA = A.uniformAdversarialChain (Just testAscA) recipeA' testSeedA

    let H.ChainSchedule winA _vA = schedA

    C.SomeWindow Proxy stabWin <- do
        let A.AdversarialRecipe { A.arParams = (_kcp, scg, _delta) } = testRecipeA
        pure $ calculateStability scg schedA

    pure $ case Exn.runExcept $ A.checkAdversarialChain testRecipeA schedA of
        Right () -> QC.property ()
        Left e   -> case e of
            A.BadAnchor{} -> QC.counterexample (show e) False
            A.BadCount{}  -> QC.counterexample (show e) False
            A.BadRace rv  -> case rv of
                A.AdversaryWonRace {
                    A.rvAdv = RI.Race (C.SomeWindow Proxy rAdv)
                  ,
                    A.rvHon = RI.Race (C.SomeWindow Proxy rHon)
                  } -> id
                    $ QC.counterexample (advCounterexample schedH schedA winA stabWin rv)
                    $ QC.counterexample ("arPrefix = " <> show (A.arPrefix testRecipeA))
                    $ QC.counterexample ("rvAdv    = " <> show rAdv)
                    $ QC.counterexample ("rvAdv'   = " <> show (C.joinWin winA rAdv))
                    $ QC.counterexample ("rvHon    = " <> show rHon)
                    $ QC.counterexample ("stabWin  = " <> show stabWin)
                    $ QC.counterexample ("stabWin' = " <> show (C.joinWin winA stabWin))
                    $ QC.counterexample (show e)
                    $ False

data AdvStabLbl

-- | Calculate the interval in which the adversary can not yet have accelerated
calculateStability :: Scg -> H.ChainSchedule base adv -> C.SomeWindow AdvStabLbl adv SlotE
calculateStability (Scg s) schedA =
    C.withWindow (C.windowSize winA) (C.Lbl @AdvStabLbl) (C.Count 0) (C.Count $ firstActive + theBlockItself + s)
  where
    H.ChainSchedule winA vA = schedA

    C.Count firstActive = case BV.findIthEmptyInV S.inverted vA (C.Count 0) of
            BV.JustFound x  -> x
            BV.NothingFound -> error $ "impossible! " <> H.unlines' (H.prettyChainSchedule schedA "A")
    theBlockItself = 1

-- | A nice rendering for failures of 'prop_adversarialChain'
advCounterexample ::
     A.ChainSchedule base hon
  -> A.ChainSchedule base adv
  -> C.Contains 'SlotE base adv
  -> C.Contains 'SlotE adv stab
  -> A.RaceViolation hon adv
  -> String
advCounterexample schedH schedA winA stabWin rv =
    case rv of
        A.AdversaryWonRace {
            A.rvAdv = RI.Race (C.SomeWindow Proxy rAdv)
          ,
            A.rvHon = RI.Race (C.SomeWindow Proxy rHon)
          } ->
            H.unlines' $ []
             <> [H.prettyWindow rHon "rHon"]
             <> reverse (H.prettyChainSchedule schedH "H")
             <> H.prettyChainSchedule schedA "A"
             <> [H.prettyWindow (C.joinWin winA rAdv) "rAdv'"]
             <> [H.prettyWindow (C.joinWin winA stabWin) "stabWin'"]

-----

-- | A mutation that causes some honest Race Windows to end one slot sooner
data AdversarialMutation =
    -- | Increasing 'Delta' by one may cause the adversary to win a race
    AdversarialMutateDelta
  |
    -- | Decreasing 'Kcp' by one may cause the adversary to win a race
    AdversarialMutateKcp
{-
  |
    -- | Decreasing 'Scg' by one may case the adversary to win a (conservative) race
    AdversarialMutateScgNeg
  |
    -- | Increasing 'Scg' by one may case the adversary to accelerate prematurely
    AdversarialMutateScgPos
-}
  deriving (Bounded, Eq, Enum, Read, Show)

data TestAdversarialMutation base hon =
    TestAdversarialMutation
        !H.HonestRecipe
        !(H.CheckedHonestRecipe          base hon)
        !(A.AdversarialRecipe            base hon)
        !(A.SomeCheckedAdversarialRecipe base hon)
        !AdversarialMutation
  deriving (Read, Show)

data SomeTestAdversarialMutation =
    forall base hon.
    SomeTestAdversarialMutation
        !(Proxy base)
        !(Proxy hon)
        !(TestAdversarialMutation base hon)

instance Show SomeTestAdversarialMutation where
    showsPrec p (SomeTestAdversarialMutation base hon testMut) =
        Some.runShowsPrec p
      $ Some.showCtor SomeTestAdversarialMutation "SomeTestAdversarialMutation"
            `Some.showArg` base
            `Some.showArg` hon
            `Some.showArg` testMut

instance Read SomeTestAdversarialMutation where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor SomeTestAdversarialMutation "SomeTestAdversarialMutation"
            <*> Some.readArg
            <*> Some.readArg
            <*> Some.readArg

mutateAdversarial :: A.AdversarialRecipe base hon -> AdversarialMutation -> A.AdversarialRecipe base hon
mutateAdversarial recipe mut =
    A.AdversarialRecipe { A.arHonest, A.arParams = (Kcp k', Scg s', Delta d'), A.arPrefix }
  where
    A.AdversarialRecipe { A.arHonest, A.arParams = (Kcp k,  Scg s,  Delta d ), A.arPrefix } = recipe

    (k', s', d') = case mut of
        AdversarialMutateDelta  -> (k,     s,     d + 1)
        AdversarialMutateKcp    -> (k - 1, s,     d    )
--        AdversarialMutateScgNeg -> (k,     s - 1, d    )
--        AdversarialMutateScgPos -> (k,     s + 1, d    )

instance QC.Arbitrary SomeTestAdversarialMutation where
    arbitrary = do
        mut <- QC.elements [minBound .. maxBound :: AdversarialMutation]
        H.unsafeMapSuchThatJust $ do
            (kcp, scg, delta, len) <- H.sized1 $ \sz -> do
                (kcp, Scg s, delta) <- H.genKSD

                l <- (+ s) <$> QC.choose (0, 5 * sz)

                pure (kcp, Scg s, delta, Len l)

            let recipeH = H.HonestRecipe kcp scg delta len

            someTestRecipeH' <- case Exn.runExcept $ H.checkHonestRecipe recipeH of
                Left e  -> error $ "impossible! " <> show (recipeH, e)
                Right x -> pure x

            H.SomeCheckedHonestRecipe Proxy Proxy recipeH' <- pure someTestRecipeH'

            seedH <- QC.arbitrary @QCGen

            let arHonest = H.uniformTheHonestChain Nothing recipeH' seedH

            arPrefix <- genArPrefix arHonest

            let recipeA = A.AdversarialRecipe {
                    A.arPrefix
                  ,
                    A.arParams = (kcp, scg, delta)
                  ,
                    A.arHonest
                  }

            pure $ case Exn.runExcept $ A.checkAdversarialRecipe recipeA of
                Left e -> case e of
                    A.NoSuchAdversarialBlock -> Nothing
                    A.NoSuchCompetitor       -> error $ "impossible! " <> show e
                    A.NoSuchIntersection     -> error $ "impossible! " <> show e

                Right recipeA' -> case Exn.runExcept $ A.checkAdversarialRecipe $ mutateAdversarial recipeA mut of
                    Left{} -> Nothing

                    Right (A.SomeCheckedAdversarialRecipe Proxy mutRecipeA)
                        -- no mutation matters if the adversary doesn't even
                        -- have k+1 slots of any kind, let alone active slots
                      | let A.UnsafeCheckedAdversarialRecipe {
                                A.carParams = (Kcp k', _scg', _delta')
                              ,
                                A.carWin
                              } = mutRecipeA
                      , C.getCount (C.windowSize carWin) < k' + 1
                      -> Nothing

                      -- ASSUMPTION: the adversary has k blocks in the first race.
                      --
                      -- If there's no slot after the first race, then the
                      -- increment of delta cannot make a difference.
                      | AdversarialMutateDelta <- mut
                      , let H.ChainSchedule _winH vH = arHonest
                        -- slot of the k+1 honest active in adv
                      , let Kcp k   = kcp
                      , let Scg s   = scg
                      , let Delta d = delta
                      , let Len l   = len
                      , let slot =
                                if 0 == C.toVar arPrefix then 0 else
                                case BV.findIthEmptyInV S.inverted vH (C.toIndex arPrefix C.- 1) of
                                    BV.NothingFound{} -> error "impossible!"
                                    BV.JustFound x    -> C.getCount x
                      , endOfFirstRace <- case BV.findIthEmptyInV S.inverted vH (C.toIndex arPrefix C.+ k) of
                            BV.NothingFound{} -> slot + s
                            BV.JustFound x    -> C.getCount x + d
                      , l <= endOfFirstRace + 1   -- adding one for the mutation
                      -> Nothing

                      | otherwise ->
                        Just
                      $ SomeTestAdversarialMutation Proxy Proxy
                      $ TestAdversarialMutation
                            recipeH
                            recipeH'
                            recipeA
                            recipeA'
                            mut

-- | There exists a seed such that each 'TestAdversrialMutation' causes
-- 'A.checkAdversrialChain' to reject the result of 'A.uniformAdversarialChain'
--
-- TODO this did fail after >500,000 tests. Is that amount of flakiness acceptable?
prop_adversarialChainMutation :: SomeTestAdversarialMutation -> QCGen -> QC.Property
prop_adversarialChainMutation (SomeTestAdversarialMutation Proxy Proxy testAdversarialMut) testSeedAsSeed0 =
  QC.ioProperty $ do
    A.SomeCheckedAdversarialRecipe Proxy recipeA' <- pure someRecipeA'

    counter <- newIORef @Int 0
    catch   <- newIORef @(QCGen, [String]) (undefined, [])

    -- we're willing to wait up to 500ms to find a failure for each 'TestHonestMutation'
    IO.timeout
        (5 * 10^(5::Int))
        (go catch counter recipeA' testSeedAsSeed0) >>= \case
            Just bool -> pure $ QC.property bool
            Nothing   ->    -- did not find a failure caused by the mutation
                ((,) <$> readIORef catch <*> readIORef counter) <&> \((seedA, schedA'), n) -> id
                  $ QC.counterexample ("n = " <> show n)
                  $ QC.counterexample
                        (advMutCounterexample testAdversarialMut mutatedRecipe schedA' seedA)
                  $ False
  where
    TestAdversarialMutation recipeH _recipeH' recipeA someRecipeA' mut = testAdversarialMut

    H.HonestRecipe _kcp scg _delta _len = recipeH

    mutatedRecipe = mutateAdversarial recipeA mut

    go catch counter recipeA' testSeedAsSeed = do
        modifyIORef' counter (+1)
        let -- TODO is this a low quality random stream? Why is there no @'R.Random' 'QCGen'@ instance?
            (testSeedA, testSeedAsSeed') = R.split testSeedAsSeed

            schedA = A.uniformAdversarialChain Nothing recipeA' (testSeedA :: QCGen)
            m      = A.checkAdversarialChain mutatedRecipe schedA
        case Exn.runExcept m of
            Right () -> do
                let A.UnsafeCheckedAdversarialRecipe { A.carWin } = recipeA'
                    pretty = case calculateStability scg schedA of
                        C.SomeWindow Proxy win ->
                            [H.prettyWindow (C.joinWin carWin win) "no accel"]
                         <> [show (H.countChainSchedule schedA)]
                writeIORef catch (testSeedA,  H.prettyChainSchedule schedA "A" <> pretty)
                go catch counter recipeA' testSeedAsSeed'
            Left e   -> case e of
                A.BadAnchor{} -> error $ "impossible! " <> show e
                A.BadCount{}  -> error $ "impossible! " <> show e
                A.BadRace{}   -> pure True

-----

-- | A nice rendering for failures of 'prop_adversarialChainMutation'
advMutCounterexample ::
     TestAdversarialMutation base hon
  -> A.AdversarialRecipe base hon
  -> [String]
  -> QCGen
  -> String
advMutCounterexample testAdversarialMut mutatedRecipe schedA' seedA =
    H.unlines' $ []
     <> [show seedA]
     <> [show ch <> " - " <> show arPrefix <> " = " <> show (C.toVar ch - arPrefix) <> "   vs " <> show (kcp, mut)]
     <> schedH'
     <> schedA'
     <> go' (((,) False <$> RI.init kcp' vH) <|> Just (True, RI.initConservative scg delta winH))
  where
    TestAdversarialMutation recipeH _recipeH' recipeA someRecipeA' mut = testAdversarialMut

    H.HonestRecipe kcp scg delta _len = recipeH

    A.AdversarialRecipe { A.arHonest, A.arPrefix } = recipeA

    H.ChainSchedule winH vH = arHonest

    A.AdversarialRecipe { A.arParams = (kcp', _scg', _delta') } = mutatedRecipe

    schedH' = H.prettyChainSchedule arHonest "H"

    ch = H.countChainSchedule arHonest

    next iter =
            ((,) False <$> RI.next vH iter)
        <|>
            ((,) True <$> RI.nextConservative scg delta vH iter)

    go' = \case
        Nothing -> []
        Just (cons, iter@(RI.Race (C.SomeWindow Proxy win)))
          | A.SomeCheckedAdversarialRecipe Proxy recipeA' <- someRecipeA'
          , A.UnsafeCheckedAdversarialRecipe { A.carWin } <- recipeA'
          , C.windowStart win < C.windowStart carWin
          -> go' (next iter)

          | otherwise ->
              ""
            : head (tail schedH')
            : H.prettyWindow win ("raceH" <> if cons then " (conservative)" else "")
            : (take 2 (tail schedA') <> go' (next iter))
