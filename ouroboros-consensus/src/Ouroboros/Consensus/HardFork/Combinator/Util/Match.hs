{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Util.Match (Mismatch(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
module Ouroboros.Consensus.HardFork.Combinator.Util.Match (
    Mismatch (..)
  , flip
  , matchNS
  , matchTelescope
    -- * Utilities
  , mismatchNotEmpty
  , mismatchNotFirst
  , mismatchOne
  , mismatchToNS
  , mismatchTwo
  , mkMismatchTwo
  , mustMatchNS
    -- * SOP operators
  , bihap
  , bihcmap
  , bihmap
  ) where

import           Prelude hiding (flip)

import           Data.Bifunctor
import           Data.Functor.Product
import           Data.Kind (Type)
import           Data.SOP.Strict
import           Data.Void
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks (..), allNoThunks)

import           Ouroboros.Consensus.Util.SOP ()

import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

data Mismatch :: (k -> Type) -> (k -> Type) -> [k] -> Type where
  ML :: f x -> NS g xs -> Mismatch f g (x ': xs)
  MR :: NS f xs -> g x -> Mismatch f g (x ': xs)
  MS :: Mismatch f g xs -> Mismatch f g (x ': xs)

flip :: Mismatch f g xs -> Mismatch g f xs
flip = go
  where
    go :: Mismatch f g xs -> Mismatch g f xs
    go (ML fx gy) = MR gy fx
    go (MR fy gx) = ML gx fy
    go (MS m)     = MS (go m)

matchNS :: NS f xs -> NS g xs -> Either (Mismatch f g xs) (NS (Product f g) xs)
matchNS = go
  where
    go :: NS f xs -> NS g xs -> Either (Mismatch f g xs) (NS (Product f g) xs)
    go (Z fx) (Z gx) = Right (Z (Pair fx gx))
    go (S l)  (S r)  = bimap MS S $ go l r
    go (Z fx) (S r)  = Left $ ML fx r
    go (S l)  (Z gx) = Left $ MR l gx

matchTelescope :: NS h xs
               -> Telescope g f xs
               -> Either (Mismatch h f xs) (Telescope g (Product h f) xs)
matchTelescope = go
  where
    go :: NS h xs
       -> Telescope g f xs
       -> Either (Mismatch h f xs) (Telescope g (Product h f) xs)
    go (Z l)  (TZ fx)    = Right (TZ (Pair l fx))
    go (S r)  (TS  gx t) = bimap MS (TS gx) $ go r t
    go (Z hx) (TS _gx t) = Left $ ML hx (Telescope.tip t)
    go (S l)  (TZ fx)    = Left $ MR l fx

{-------------------------------------------------------------------------------
  SOP class instances for 'Mismatch'
-------------------------------------------------------------------------------}

type instance Prod    (Mismatch f)   = NP
type instance SListIN (Mismatch f)   = SListI
type instance AllN    (Mismatch f) c = All c

instance HAp (Mismatch f) where
  hap = go
    where
      go :: NP (g -.-> g') xs -> Mismatch f g xs -> Mismatch f g' xs
      go (_ :* fs) (MS m)     = MS (go fs m)
      go (_ :* fs) (ML fx gy) = ML fx (hap fs gy)
      go (f :* _)  (MR fy gx) = MR fy (apFn f gx)

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

-- | We cannot give a mismatch if we have only one type variable
mismatchOne :: Mismatch f g '[x] -> Void
mismatchOne = either aux aux . mismatchNotFirst
  where
    aux :: NS f '[] -> Void
    aux ns = case ns of {}

-- | If we only have two eras, only two possibilities for a mismatch
mismatchTwo :: Mismatch f g '[x, y] -> Either (f x, g y) (f y, g x)
mismatchTwo (ML fx gy) = Left (fx, unZ gy)
mismatchTwo (MR fy gx) = Right (unZ fy, gx)
mismatchTwo (MS m)     = absurd $ mismatchOne m

mkMismatchTwo :: Either (f x, g y) (f y, g x) -> Mismatch f g '[x, y]
mkMismatchTwo (Left  (fx, gy)) = ML fx (Z gy)
mkMismatchTwo (Right (fy, gx)) = MR (Z fy) gx

-- | Project two 'NS' from a 'Mismatch'
--
-- We should have the property that
--
-- > uncurry matchNS (mismatchToNS m) == Left m
mismatchToNS :: Mismatch f g xs -> (NS f xs, NS g xs)
mismatchToNS = go
  where
    go :: Mismatch f g xs -> (NS f xs, NS g xs)
    go (ML fx gy) = (Z fx, S gy)
    go (MR fy gx) = (S fy, Z gx)
    go (MS m)     = bimap S S $ go m

mismatchNotEmpty :: Mismatch f g xs
                 -> (forall x xs'. xs ~ (x ': xs')
                                => Mismatch f g (x ': xs') -> a)
                 -> a
mismatchNotEmpty = go
  where
    go :: Mismatch f g xs
       -> (forall x xs'. xs ~ (x ': xs') => Mismatch f g (x ': xs') -> a)
       -> a
    go (ML fx gy) k = k (ML fx gy)
    go (MR fy gx) k = k (MR fy gx)
    go (MS m)     k = go m (k . MS)

mismatchNotFirst :: Mismatch f g (x ': xs) -> Either (NS f xs) (NS g xs)
mismatchNotFirst = go
  where
    go :: Mismatch f g (x' ': xs') -> Either (NS f xs') (NS g xs')
    go (ML _  gy) = Right gy
    go (MR fy _ ) = Left fy
    go (MS m)     = mismatchNotEmpty m $ \m' ->
                      bimap S S $ go m'

-- | Variant of 'matchNS' for when we know the two 'NS's must match. Otherwise
-- an error, mentioning the given 'String', is thrown.
mustMatchNS ::
     forall f g xs. HasCallStack
  => String -> NS f xs -> NS g xs -> NS (Product f g) xs
mustMatchNS lbl f g = case matchNS f g of
    Left _mismatch -> error $ lbl <> " from wrong era"
    Right matched  -> matched

{-------------------------------------------------------------------------------
  Subset of the (generalized) SOP operators
-------------------------------------------------------------------------------}

bihap :: NP (f -.-> f') xs
      -> NP (g -.-> g') xs
      -> Mismatch f g xs -> Mismatch f' g' xs
bihap = \gs fs t -> go t gs fs
  where
    go :: Mismatch f g xs
       -> NP (f -.-> f') xs
       -> NP (g -.-> g') xs
       -> Mismatch f' g' xs
    go (ML fx r) (f :* _)  (_ :* gs) = ML (apFn f fx) (hap gs r)
    go (MR l gx) (_ :* fs) (g :* _)  = MR (hap fs l) (apFn g gx)
    go (MS m)    (_ :* fs) (_ :* gs) = MS (go m fs gs)

bihmap :: SListI xs
       => (forall x. f x -> f' x)
       -> (forall x. g x -> g' x)
       -> Mismatch f g xs -> Mismatch f' g' xs
bihmap = bihcmap (Proxy @Top)

-- | Bifunctor analogue of 'hcmap'
bihcmap :: All c xs
        => proxy c
        -> (forall x. c x => f x -> f' x)
        -> (forall x. c x => g x -> g' x)
        -> Mismatch f g xs -> Mismatch f' g' xs
bihcmap p g f = bihap (hcpure p (fn g)) (hcpure p (fn f))

{-------------------------------------------------------------------------------
  Standard type class instances
-------------------------------------------------------------------------------}

deriving stock instance ( All (Compose Eq f) xs
                        , All (Compose Eq g) xs
                        ) => Eq (Mismatch f g xs)

deriving stock instance ( All (Compose Eq  f) xs
                        , All (Compose Ord f) xs
                        , All (Compose Eq  g) xs
                        , All (Compose Ord g) xs
                        ) => Ord (Mismatch f g xs)

deriving stock instance ( All (Compose Show f) xs
                        , All (Compose Show g) xs
                        ) => Show (Mismatch f g xs)

instance ( All (Compose NoThunks f) xs
         , All (Compose NoThunks g) xs
         ) => NoThunks (Mismatch f g xs) where
  showTypeOf _ = "Mismatch"
  wNoThunks ctxt = \case
    ML l r -> allNoThunks [
                  noThunks ("l" : "ML" : ctxt) l
                , noThunks ("r" : "ML" : ctxt) r
                ]
    MR l r -> allNoThunks [
                  noThunks ("l" : "MR" : ctxt) l
                , noThunks ("r" : "MR" : ctxt) r
                ]
    MS m   -> noThunks ("MS" : ctxt) m
