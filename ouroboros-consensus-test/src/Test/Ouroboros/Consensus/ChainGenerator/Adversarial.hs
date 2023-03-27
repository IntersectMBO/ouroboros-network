{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Ouroboros.Consensus.ChainGenerator.Adversarial (
  -- * Generating
  AdversarialRecipe (AdversarialRecipe, arHonest, arParams, arPrefix),
  CheckedAdversarialRecipe (UnsafeCheckedAdversarialRecipe, carHonest, carParams, carWin),
  NoSuchAdversarialChainSchedule (NoSuchAdversarialBlock, NoSuchCompetitor, NoSuchIntersection),
  SomeCheckedAdversarialRecipe (SomeCheckedAdversarialRecipe),
  checkAdversarialRecipe,
  uniformAdversarialChain,
  -- * Testing
  AdversarialViolation (BadAnchor, BadCount, BadRace),
  AnchorViolation (HonestActiveMustAnchorAdversarial),
  ChainSchedule (ChainSchedule),
  RaceViolation (AdversaryWonRace, rvAdv, rvHon),
  checkAdversarialChain,
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (void, when)
import qualified Control.Monad.Except as Exn
import           Data.Proxy (Proxy (Proxy))
import qualified System.Random.Stateful as R
import qualified Test.Ouroboros.Consensus.ChainGenerator.BitVector as BV
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import           Test.Ouroboros.Consensus.ChainGenerator.Honest (ChainSchedule (ChainSchedule))
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc, Delta (Delta), Kcp (Kcp), Scg (Scg))
import qualified Test.Ouroboros.Consensus.ChainGenerator.RaceIterator as RI
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot (E (ActiveSlotE, EmptySlotE, SlotE))
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some

-----

data AnchorViolation =
    -- | An honest active slot must immediately precede the adversarial interval
    HonestActiveMustAnchorAdversarial
  |
    -- | The were not exactly 'arPrefix' many active slots preceding @adv@
    WrongNumberOfHonestPredecessors
  deriving (Eq, Read, Show)

-- | A violation of the Extended Praos Race Assumption
--
-- INVARIANT: @'C.windowLast' 'rvAdv' < 'C.windowLast' 'rvHon' + 'Delta'@
--
-- INVARIANT: @'C.windowStart' 'rvHon' <= 'C.windowStart' 'rvAdv'@
data RaceViolation hon adv = AdversaryWonRace {
    -- | The adversarial race window
    rvAdv :: !(RI.Race adv)
  ,
    -- | The honest race window
    rvHon :: !(RI.Race hon)
  }
  deriving (Eq, Read, Show)

data AdversarialViolation hon adv =
    BadAnchor !AnchorViolation
  |
    BadCount
  |
    BadRace !(RaceViolation hon adv)
  deriving (Eq, Read, Show)

-- | Check the chain matches the given 'AdversarialRecipe', especially the Extended Praos Race Assumption
--
-- Definition of a /Praos Race Window/. Such a window includes exactly 'Delta'
-- slots after the @k+1@st honest block in the window.
--
-- Definition of the /Praos Race Assumption/. We assume every adversarial chain
-- contains at most @k@ blocks in the Praos Race Window anchored at the
-- intersection.
--
-- Definition of the /Extended Praos Race Assumption/. Beyond the Praos Race
-- Assumption, we assume that every adversarial chain also has at most @k@
-- blocks in every subsequent Praos Race Window that ends within 'Delta' of the
-- Stability Window anchored at the first adverarial block. (Crucially, these
-- are the Praos Race Windows in which the the adversary could not yet have
-- drastically accelerated its rate of election.)
--
-- Remark. The Praos Enriched Honest Chain Growth Assumption ensures the
-- adversary cannot accelerate until after it has lost the first race.
--
-- Definition of the /Genesis Implication Conjecture/. We conjecture that
-- assuming that the Genesis window length is no greater than the Stability
-- Window and that every possible adversarial chain satisfies the Extended Praos
-- Race Assumption implies the honest chain strictly wins every possible density
-- comparison from the Ouroboros Genesis paper. The key intuition is that a less
-- dense chain would have to lose at least one race within the Genesis window.
checkAdversarialChain ::
  forall base hon adv.
     AdversarialRecipe base hon
  -> ChainSchedule base adv
  -> Exn.Except (AdversarialViolation hon adv) ()
checkAdversarialChain recipe adv = do
    checkStart
    checkCount
    checkRaces
  where
    AdversarialRecipe {
        arHonest = ChainSchedule winH vH
      ,
        arParams = (Kcp k, Scg s, Delta d)
      ,
        arPrefix
      } = recipe

    ChainSchedule winA vA = adv

    checkStart = do
        let startA       = C.windowStart winA :: C.Index base SlotE
            intersection = startA C.- 1       :: C.Index base SlotE

        case C.toWin winH intersection of
            Nothing -> do
              -- genesis block is the only permissible anchor outside of @hon@
              when (startA   /= C.Count 0) $ Exn.throwError $ BadAnchor HonestActiveMustAnchorAdversarial
              when (arPrefix /= C.Count 0) $ Exn.throwError $ BadAnchor WrongNumberOfHonestPredecessors

            Just i -> do
                let _ = i :: C.Index hon SlotE

                when (BV.testV S.inverted vH i) $ do
                    Exn.throwError $ BadAnchor HonestActiveMustAnchorAdversarial

                C.SomeWindow Proxy precedingSlots <-
                    pure $ C.withWindowBetween (C.windowSize winH) (C.Lbl @"foo") (C.Count 0) i
                let pc = BV.countActivesInV S.notInverted (C.sliceV precedingSlots vH)

                when (C.frWinVar precedingSlots (C.toVar pc) /= arPrefix) $ do
                    Exn.throwError $ BadAnchor WrongNumberOfHonestPredecessors

    checkCount = do
        let pc = BV.countActivesInV S.notInverted vA
        when (C.toVar pc <= 0) $ Exn.throwError BadCount

    -- the youngest slot in the adversarial schedule that cannot have accelerated
    --
    -- (IE s past the first active adversarial slot.)
    youngestStableA :: C.Index adv SlotE
    youngestStableA =
        case BV.findIthEmptyInV S.inverted vA (C.Count 0) of
            BV.JustFound firstActiveA ->
                -- if s=0, then the slot of their block is the youngest stable slot
                firstActiveA C.+ s
            BV.NothingFound           ->
                -- the rest of the function won' force this since there are no
                -- adverarial active slots
                error "dead code"

    checkRaces = do
        let iterH =
                maybe (RI.initConservative (Scg s) (Delta d) winH) id
              $ RI.init (Kcp k) vH
        case RI.init (Kcp (k - 1)) vA >>= RI.next vA of
            Nothing    -> pure ()   -- there are <= k total adversarial active slots
            Just iterA ->
                -- TODO optimization: how to skip all the honest Race Windows that
                -- don't reach beyond the intersection? Perhaps skip to i - s + d?
                go iterH iterA

    -- INVARIANT iterH spans k+1 active slots (possibly conservatively)
    --
    -- INVARIANT iterA spans k active slots (actually, not conservatively)
    go !iterH !iterA = do
        C.SomeWindow Proxy raceWinH <- pure $ let RI.Race x = iterH in x
        C.SomeWindow Proxy raceWinA <- pure $ let RI.Race x = iterA in x

        -- lift both windows to @base@ so that they're comparable
        let raceWinH' = C.joinWin winH raceWinH
            raceWinA' = C.joinWin winA raceWinA

        if

          -- any Race Window that ends /after/ the adversary can accelerate is unconstrained
          | C.frWin winA youngestStableA < C.windowLast raceWinH' -> pure ()

          -- advance the adversarial Race Window if its start is <= the honest Race Window's start
          | C.windowStart raceWinA' <= C.windowStart raceWinH' ->
            case RI.next vA iterA of
                Just iterA' -> go iterH iterA'
                Nothing     -> pure ()   -- there are < k remaining adversarial active slots

          -- fail if the adversary won or tied the race
          | C.toVar (C.windowLast raceWinA') <= C.toVar (C.windowLast raceWinH') + C.Count d ->
                -- iterA contains exactly k active slots, but A) it's anchored
                -- in an active slot and B) iterH contains that anchor. Thus
                -- adv has k+1 in iterH.
                Exn.throwError $ BadRace AdversaryWonRace {
                    rvAdv = iterA
                  ,
                    rvHon = iterH
                  }

          -- advance the honest Race Window
          | otherwise -> case RI.next vH iterH <|> RI.nextConservative (Scg s) (Delta d) vH iterH of
                Just iterH' -> go iterH' iterA
                Nothing     -> pure ()   -- there are no remaining honest active slots
                                         --
                                         -- TODO hpc shows this never executes

-----

-- | Named arguments for 'checkAdversarialRecipe'
data AdversarialRecipe base hon =
    AdversarialRecipe {
        -- | The honest chain to branch off of
        arHonest :: !(ChainSchedule base hon)
      ,
        -- | protocol parameters
        arParams :: (Kcp, Scg, Delta)
      ,
        -- | The number of (real) honest active slots as of the intersection
        --
        -- @0@ means the genesis block and @i > 0@ means @i - 1 :: C.Index hon ActiveSlotE@
        arPrefix :: !(C.Var hon ActiveSlotE)
      }
  deriving (Eq, Read, Show)

-- | See 'CheckedAdversarialRecipe'
data SomeCheckedAdversarialRecipe base hon =
    forall adv.
    SomeCheckedAdversarialRecipe
        !(Proxy adv)
        !(CheckedAdversarialRecipe base hon adv)

instance Show (SomeCheckedAdversarialRecipe base hon) where
    showsPrec p (SomeCheckedAdversarialRecipe adv car) =
        Some.runShowsPrec p
      $ Some.showCtor SomeCheckedAdversarialRecipe "SomeCheckedAdversarialRecipe"
            `Some.showArg` adv
            `Some.showArg` car

instance Read (SomeCheckedAdversarialRecipe base hon) where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor SomeCheckedAdversarialRecipe "SomeCheckedAdversarialRecipe"
            <*> Some.readArg
            <*> Some.readArg

-- | Image of 'checkAdversarialRecipe' when it accepts the recipe
data CheckedAdversarialRecipe base hon adv =
    UnsafeCheckedAdversarialRecipe {
        carHonest :: !(ChainSchedule base hon)
      ,
        carParams :: (Kcp, Scg, Delta)
      ,
        -- | INVARIANT: there is at least one active honest slot in @adv@
        --
        -- In other words, the adversarial leader schedule does not /extend/ the
        -- chain represented by 'carHonest', it competes with it.
        carWin    :: !(C.Contains SlotE hon adv)
      }
  deriving (Eq, Read, Show)

-- | Image of 'checkAdversarialRecipe' when it rejects the recipe
data NoSuchAdversarialChainSchedule =
    -- | There is no slot the adversary can lead
    --
    -- Two possible reasons, where @X@ is the slot of 'arPrefix' and Y is the youngest slot of 'arHonest'.
    --
    --    * The interval @[X, Y)@ is empty.
    --
    --    * @k=0@
    --
    -- Suppose k=0 and slot x and slot y are two honest active slots with only
    -- honest empty slots between them.
    --
    -- The Praos Race Assumption prohibits the adversary from leading in the interval (x,y].
    --
    -- In fact, by induction, the adversary can never lead: suppose the same y
    -- and a slot z are two honest active slots with only honest empty slots
    -- between them...
    NoSuchAdversarialBlock
  |
    -- | @not (arPrefix' < C)@ where @C@ is the number of active slots in 'arHonest'
    NoSuchCompetitor
  |
    -- | @not (0 <= 'arPrefix' <= C)@ where @C@ is the number of active slots in 'arHonest'
    NoSuchIntersection
  deriving (Eq, Show)

-----

-- | The suffix of the slots starting strictly /after/ the intersection
data AdvLbl

-- | The interval of slots that have most recently attained their final value
data SettledLbl

-- | Reject a bad 'AdversarialRecipe'
checkAdversarialRecipe ::
  forall base hon.
     AdversarialRecipe base hon
  -> Exn.Except
         NoSuchAdversarialChainSchedule
         (SomeCheckedAdversarialRecipe base hon)
checkAdversarialRecipe recipe = do
    when (0 == k) $ Exn.throwError NoSuchAdversarialBlock

    -- validate 'arPrefix'
    firstAdvSlot <- case compare arPrefix 0 of
        LT -> Exn.throwError NoSuchIntersection
        EQ -> pure $ C.Count 0
        GT -> case BV.findIthEmptyInV S.inverted vH $ C.toIndex $ arPrefix - 1 of
            BV.NothingFound -> Exn.throwError NoSuchIntersection
            BV.JustFound x  -> do
                when (x == C.lastIndex (C.windowSize winH)) $ Exn.throwError NoSuchAdversarialBlock
                pure (x C.+ 1)

    C.SomeWindow Proxy winA <- pure $ C.withSuffixWindow (C.windowSize winH) (C.Lbl @AdvLbl) firstAdvSlot

    -- there must be at least one honest active slot in @adv@
    case BV.findIthEmptyInV S.inverted (C.sliceV winA vH) (C.Count 0) of
        BV.NothingFound -> Exn.throwError NoSuchCompetitor
        BV.JustFound{}  -> pure ()

    pure $ SomeCheckedAdversarialRecipe Proxy $ UnsafeCheckedAdversarialRecipe {
        carHonest = arHonest
      ,
        carParams = arParams
      ,
        carWin    = winA
      }
  where
    AdversarialRecipe {
        arHonest
      ,
        arParams
      ,
        arPrefix
      } = recipe

    (Kcp k, _scg, _delta) = arParams

    ChainSchedule winH vH = arHonest

-----

data RaceAssumptionLbl
data UntouchableLbl
data TouchableLbl

-- | Generate an adversarial 'ChainSchedule' that satifies 'checkExtendedRaceAssumption'
--
-- The distribution this function samples from is not simple to describe. It
-- begins by drawing a sample of length 'adv' from the Bernoulli process
-- induced by the active slot coefficient 'Asc'. (IE 'adv' many i.i.d. samples
-- from @Uniform(f)@). Then it visits the Extended Praos Race Windows that
-- overlap with the prefix of that leader schedule that ends one stability
-- window after the first (remaining) active adversarial slot in it. In each
-- such Window, it removes the youngest active slots from the adversarial
-- leader schedule until it loses that Race.
uniformAdversarialChain ::
  forall g base hon adv.
     R.RandomGen g
  => Maybe Asc   -- ^ 'Nothing' means @1@
  -> CheckedAdversarialRecipe base hon adv
  -> g
  -> ChainSchedule base adv
{-# INLINABLE uniformAdversarialChain #-}
uniformAdversarialChain mbAsc recipe g0 = wrap $ C.createV $ do
    g <- R.newSTGenM g0

    let sz = C.windowSize carWin :: C.Size adv SlotE

    -- randomly initialize the bitstring
    mv <- C.replicateMV sz $ case mbAsc of
        Nothing  -> pure $ S.mkActive S.notInverted
        Just asc -> S.genAsc asc `R.applySTGen` g

    -- ensure the adversarial leader schedule is not empty
    do  void $ BV.fillInWindow
            S.notInverted
            (BV.SomeDensityWindow (C.Count 1) (C.windowSize carWin))
            g
            mv

    -- ensure the adversarial leader schedule does not win any of the races for
    -- which the honest Race Window fits within the Stability Window anchored at
    -- the first adversarial active slot
    --
    -- TODO Why is it ok to skip early honest races, some of which overlap with
    -- @adv@? Is it because not having >k in some prefix [0, n] of adv ensures
    -- you can't have >k in the interval [0, C.frWindow adv n] either?
    let iterH :: RI.Race adv
        iterH =
            maybe (RI.initConservative scg delta carWin) id
          $ RI.init kcp vA
    unfillRaces (C.Count 0) UnknownYS iterH g mv

    pure mv
  where
    UnsafeCheckedAdversarialRecipe {
        carHonest
      ,
        carParams = (kcp, scg, delta)
      ,
        carWin
      } = recipe

    wrap v = ChainSchedule (C.joinWin winH carWin) v

    Kcp   k = kcp
    Scg   s = scg
    Delta d = delta

    ChainSchedule winH vH = carHonest

    vA = C.sliceV carWin vH

    -- ensure the adversary loses this 'RI.Race' and each subsequent race that ends before it can accelerate
    unfillRaces !scope !mbYS !iter !g !mv = when (withinYS mbYS iter) $ do
        C.SomeWindow Proxy rwin <- pure $ let RI.Race x = iter in x

        C.SomeWindow (Proxy :: Proxy skolem) win <-
            pure
          $ C.withWindowBetween
                (C.windowSize carWin)
                (C.Lbl @RaceAssumptionLbl)
                (C.windowStart rwin)
                (C.windowLast  rwin C.+ d)   -- rwin ends in a block, so if d=0
                                             -- then the slot after that block
                                             -- is unconstrained; hence no +1

        -- INVARIANT: @win@ contains @scope@
        let _ = scope :: C.Index adv SlotE

        -- remove adversarial active slots as needed
        --
        -- But only remove them from /after/ @scope@ (ie do not remove them
        -- from slots in the previous Race Window).
        do  untouchZeroCount <- do
                C.SomeWindow Proxy untouch <-   -- untouchable slots
                    pure
                  $ C.withWindowBetween
                        (C.windowSize carWin)
                        (C.Lbl @UntouchableLbl)
                        (C.windowStart rwin)
                        (scope C.- 1)   -- window will be empty if scope is 0
                C.frWinVar untouch <$> BV.countActivesInMV S.inverted (C.sliceMV untouch mv)

            C.SomeWindow (Proxy :: Proxy skolem2) touch <-   -- touchable slots
                pure
              $ C.withWindowBetween
                    (C.windowSize carWin)
                    (C.Lbl @TouchableLbl)
                    scope
                    (C.windowLast rwin C.+ d)

            -- at most k can be active in this race window, so at least size - k must be empty
            let req =
                    C.Count
                  $ max 0
                  $ C.getCount (C.windowSize win) - k
                  :: C.Var (C.Win RaceAssumptionLbl skolem) EmptySlotE

            -- Discount that basic requirement by the number of zeros already
            -- in the untouchable portion of this race window.
            let req' =
                    C.Count
                  $ max 0
                  $ C.getCount req - C.getCount untouchZeroCount
                  :: C.Var (C.Win TouchableLbl skolem2) EmptySlotE

            void $ BV.fillInWindow
                S.inverted
                (BV.SomeDensityWindow req' (C.windowSize touch))
                g
                (C.sliceMV touch mv)

        case RI.next vA iter <|> RI.nextConservative scg delta vA iter of
            Nothing -> pure ()   -- there are no remaining honest active slots

            Just iter' -> do
                C.SomeWindow Proxy rwin' <- pure $ let RI.Race x = iter' in x
                mbYS' <- case mbYS of
                    KnownYS{} -> pure mbYS
                    UnknownYS -> do
                        -- check whether the slots that are settled as of just
                        -- now contain the first adversarial active slot
                        C.SomeWindow Proxy settledSlots <-
                            pure
                          $ C.withWindowBetween
                                (C.windowSize carWin)
                                (C.Lbl @SettledLbl)
                                (C.windowStart rwin)
                                (C.windowStart rwin' C.- 1)
                        mbFound <- BV.findIthEmptyInMV S.inverted (C.sliceMV settledSlots mv) (C.Count 0)
                        pure $! case mbFound of
                            BV.NothingFound -> UnknownYS
                            BV.JustFound x  ->
                                -- x is the first settled adversarial slot, so
                                -- the adversary can accelerate its growth as
                                -- of x+s+1 (If s were 0, it could accelerate
                                -- in the very next slot, thus the plus 1.)
                                KnownYS $! C.frWin settledSlots x C.+ s C.+ 1
                unfillRaces (C.windowLast win C.+ 1) mbYS' iter' g mv

-- | The youngest stable slot
--
-- If @x@ is the first adversarial active slot, then @x+s@ (see 'Scg') is the youngest stable slot.
data MaybeYS base = UnknownYS | KnownYS !(C.Index base SlotE)
  deriving (Eq, Read, Show)

-- | Does the Race Window end in a stable slot?
withinYS :: MaybeYS base -> RI.Race base -> Bool
withinYS !mbYS !(RI.Race (C.SomeWindow Proxy win)) = case mbYS of
    KnownYS ys -> C.windowLast win <= ys
    UnknownYS  -> True   -- Honest Chain Growth ensures every Race Window is at most @'Scg' - 'Delta'@ slots wide
