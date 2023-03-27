{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Test.Ouroboros.Consensus.ChainGenerator.BitVector (
    -- * Finding
    MaybeFound (JustFound, NothingFound)
  , findIthEmptyInMV
  , findIthEmptyInV
    -- * Counting
  , countActivesInMV
  , countActivesInV
    -- * Slots
  , setMV
  , testMV
  , testV
    -- * Generating
  , SomeDensityWindow (SomeDensityWindow)
  , fillInWindow
  ) where

import           Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified System.Random.Stateful as R
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot
                     (E (ActiveSlotE, EmptySlotE, SlotE), POL, PreImage, S)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some

-----

data MaybeFound base =
    NothingFound
  |
    JustFound
      {-# UNPACK #-} !(C.Index base SlotE)
  deriving (Eq, Read, Show)

-- | Trivial wrapper around 'findIthEmptyInMV'
findIthEmptyInV ::
     POL   pol
  => proxy pol
  -> C.Vector base SlotE S
  -> C.Index base (PreImage pol EmptySlotE)
  -> MaybeFound base
findIthEmptyInV pol v i =
    runST $ C.unsafeThawV v >>= \mv -> findIthEmptyInMV pol mv i

-- | Find the ith empty slot in a window
findIthEmptyInMV ::
  forall proxy pol base s.
     POL   pol
  => proxy pol
  -> C.MVector base SlotE s S
  -> C.Index base (PreImage pol EmptySlotE)
  -> ST s (MaybeFound base)
findIthEmptyInMV pol mv i =
    go 0 (C.toVar i)
  where
    go !j !toSkip = if C.getCount (C.lengthMV mv) <= j then pure NothingFound else do
        w <- C.readMV mv (C.Count j)
        if
          | S.test pol w -> go (j + 1) toSkip
          | 0 == toSkip  -> pure $ JustFound (C.Count j)
          | otherwise    -> go (j + 1) (toSkip - 1)

-----

-- | Trivial wrapper around 'countActivesInMV'
countActivesInV ::
     POL   pol
  => proxy pol
  -> C.Vector base SlotE S
  -> C.Size base (PreImage pol ActiveSlotE)
countActivesInV pol v =
    C.toSize $ runST $ C.unsafeThawV v >>= \mv -> countActivesInMV pol mv

-- | The number of active slots in the vector
countActivesInMV ::
     POL   pol
  => proxy pol
  -> C.MVector base SlotE s S
  -> ST s (C.Var base (PreImage pol ActiveSlotE))
countActivesInMV pol mv =
    MV.foldl'
        (\acc w -> if S.test pol w then acc + 1 else acc)
        0
        mv'
  where
    C.MVector mv' = mv

-----

-- | A requirement of @numerator@-many active slots per @denominator@-many slots
data SomeDensityWindow pol =
  forall slidingWindow.
    SomeDensityWindow
        !(C.Var  slidingWindow (PreImage pol ActiveSlotE))
        !(C.Size slidingWindow SlotE)

instance Eq (SomeDensityWindow pol) where
    SomeDensityWindow l1 l2 == SomeDensityWindow r1 r2 =
        Some.runEq
      $ Some.eqCtor (SomeDensityWindow @pol) (SomeDensityWindow @pol)
            `Some.eqArg` (C.forgetBase, l1, C.forgetBase, r1)
            `Some.eqArg` (C.forgetBase, l2, C.forgetBase, r2)

instance Show (SomeDensityWindow pol) where
    showsPrec p (SomeDensityWindow numer denom) =
        Some.runShowsPrec p
      $ Some.showCtor (SomeDensityWindow @pol) "SomeDensityWindow"
            `Some.showArg` numer
            `Some.showArg` denom

instance Read (SomeDensityWindow pol) where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor SomeDensityWindow "SomeDensityWindow"
            <*> Some.readArg
            <*> Some.readArg

fillInWindow ::
  forall proxy pol base g s.
     (POL pol, R.StatefulGen g (ST s))
  => proxy pol
  -> SomeDensityWindow pol
  -> g
  -> C.MVector base SlotE s S
  -> ST s (C.Var base (PreImage pol ActiveSlotE))   -- ^ the count after filling
fillInWindow pol (SomeDensityWindow k s) g mv = do
    -- how many active polarized slots @actual@ currently has
    initialActives <- countActivesInMV pol mv

    let sz = C.lengthMV mv :: C.Size base SlotE

    -- discount the numerator accordingly if @actual@ is smaller than @s@
    --
    -- EG when a full-size @actual@ would reach past the 'Len'.
    --
    -- This discount reflects that we (very conservatively!) assume every
    -- truncated slot would be an active polarized slot.
    let discountedK :: C.Var base (PreImage pol ActiveSlotE)
        discountedK = C.Count $ C.getCount k - (C.getCount s - C.getCount sz)   -- TODO assert sz <= s

    -- how many active polarized slots need to be added to @actual@
    let adding = C.toVar discountedK - initialActives :: C.Var base (PreImage pol ActiveSlotE)

    -- draw from the empty polarized slots uniformly without replacement, a la Fisher-Yates shuffle
    C.forRange_ (C.toSize adding) $ \alreadyAdded -> do
        let currentActives = C.toSize $ initialActives + C.toVar alreadyAdded
            currentEmpties = S.complementActive pol sz currentActives

        whichEmptyToFlip <- C.uniformIndex currentEmpties g

        JustFound slot <- findIthEmptyInMV pol mv whichEmptyToFlip

        setMV pol mv slot

    pure $ initialActives + adding

-----

testV :: POL pol => proxy pol -> C.Vector base SlotE S -> C.Index base SlotE -> Bool
testV pol mv i = S.test pol (C.readV mv i)

testMV :: POL pol => proxy pol -> C.MVector base SlotE s S -> C.Index base SlotE -> ST s Bool
testMV pol mv i = do
     w <- C.readMV mv i
     pure $ S.test pol w

setMV :: POL pol => proxy pol -> C.MVector base SlotE s S -> C.Index base SlotE -> ST s ()
setMV pol mv i = C.writeMV mv i $ S.mkActive pol
