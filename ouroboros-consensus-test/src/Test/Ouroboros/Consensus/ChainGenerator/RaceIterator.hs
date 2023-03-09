{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Ouroboros.Consensus.ChainGenerator.RaceIterator (
  RaceLbl,
  Race (Race, UnsafeRace),
  init,
  initConservative,
  next,
  nextConservative,
  ) where

import           Control.Monad (when)
import           Data.Proxy (Proxy (Proxy))
import           Prelude hiding (init)
import qualified Test.Ouroboros.Consensus.ChainGenerator.BitVector as BV
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Delta (Delta), Kcp (Kcp), Scg (Scg))
import           Test.Ouroboros.Consensus.ChainGenerator.Slot (E (SlotE), S)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S

-----

-- | See 'Race'
data RaceLbl

-- | A window whose last slot contains the @k+1@st active slot in it
newtype Race base = UnsafeRace (C.SomeWindow RaceLbl base SlotE)
  deriving (Eq, Read, Show)

pattern Race :: C.SomeWindow RaceLbl base SlotE -> Race base
pattern Race x <- UnsafeRace x

{-# COMPLETE Race #-}

-----

init :: Kcp -> C.Vector base SlotE S -> Maybe (Race base)
init (Kcp k) v = do
    -- find the @k+1@st active slot in the given race window
    case BV.findIthEmptyInV S.inverted v (C.Count k) of
        BV.NothingFound       -> Nothing
        BV.JustFound kPlus1st ->
            Just
         $! UnsafeRace
         $  C.withWindowBetween
                (C.lengthV v)
                (C.Lbl @RaceLbl)
                (C.Count 0)
                kPlus1st

initConservative :: Scg -> Delta -> C.Contains SlotE outer inner -> Race inner
initConservative (Scg s) (Delta d) win =
    UnsafeRace
  $ C.withWindow
        (C.windowSize win)
        (C.Lbl @RaceLbl)
        (C.Count 0)
        (C.Count (s - d))

data RaceStepLbl

next ::
  forall base.
     C.Vector base SlotE S
  -> Race base
  -> Maybe (Race base)
next v (UnsafeRace (C.SomeWindow Proxy raceWin)) = do
    -- find the first active slot /in/ the given race window
    --
    -- Race windows are anchored in an active slot, and so could start with an empty or active slot.
    next0 <- do
        -- the given race window has at least k+1 blocks in it and 0<=k, so this pattern can't fail
        case BV.findIthEmptyInV S.inverted (C.sliceV raceWin v) (C.Count 0) of
            BV.NothingFound   -> Nothing   -- would be impossible if we never called next after *Conservative
            BV.JustFound slot -> pure $! C.frWin raceWin slot

    let _ = next0 :: C.Index base SlotE

    -- find the first active slot /after/ the given race window
    --
    -- Race windows end in an active slot.
    nextK <- do
        C.SomeWindow Proxy searchWin <-
            pure
          $ C.withWindowBetween
                sz
                (C.Lbl @RaceStepLbl)
                (C.windowLast raceWin)
                (C.lastIndex sz)
        case BV.findIthEmptyInV S.inverted (C.sliceV searchWin v) (C.Count 1) of
            BV.NothingFound   -> Nothing
            BV.JustFound slot -> pure $ C.frWin searchWin slot

    let _ = nextK :: C.Index base SlotE

    pure $! UnsafeRace $ C.withWindowBetween
        sz
        (C.Lbl @RaceLbl)
        (next0 C.+ 1)
        nextK
  where
    sz = C.lengthV v

nextConservative ::
  forall base.
     Scg
  -> Delta
  -> C.Vector base SlotE S
  -> Race base
  -> Maybe (Race base)
nextConservative (Scg s) (Delta d) v (UnsafeRace (C.SomeWindow Proxy raceWin)) = do
    -- find the first active slot /in/ the given race window
    --
    -- Race windows are anchored in an active slot, and so could start with an empty or active slot.
    next0 <- do
        -- the given race window has at least k+1 blocks in it and 0<=k, so this pattern can't fail
        case BV.findIthEmptyInV S.inverted (C.sliceV raceWin v) (C.Count 0) of
            BV.NothingFound   -> Nothing
            BV.JustFound slot -> pure $! C.frWin raceWin slot

    -- do not return a Race Window that starts after 'Len'
    when (next0 == C.lastIndex sz) Nothing

    pure $! UnsafeRace $ C.withWindowBetween
        sz
        (C.Lbl @RaceLbl)
        (next0 C.+ 1)
        (next0 C.+ s C.- d)
  where
    sz = C.lengthV v
