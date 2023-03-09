{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Utilities that make it easier to declare types that merely existential
-- quantify some type arguments, and actually for any other type that is not
-- amenable to @deriving@
module Test.Ouroboros.Consensus.ChainGenerator.Some (
  -- * 'Show'
  runShowsPrec,
  showArg,
  showCtor,
  showCtorProxy,
  -- * 'Read'
  readArg,
  readCtor,
  Read.readPrec,
  runReadPrec,
  -- * 'Eq'
  eqArg,
  eqCtor,
  eqCtorProxy,
  Forgotten,
  forgotten,
  runEq,
  ) where

import           Data.Kind (Constraint, Type)
import           Data.Proxy (Proxy (Proxy))
import           Data.Void (Void)
import qualified GHC.Read as Read
import qualified GHC.TypeLits as TE
import           GHC.TypeLits (Symbol)
import qualified Text.ParserCombinators.ReadPrec as Read
import qualified Text.Read.Lex as Read

-----

type family AbsError (s :: Symbol) (a :: Type) :: Void where AbsError s a = TE.TypeError (TE.Text "You have accidentaly applied `" TE.:<>: TE.Text s TE.:<>: TE.Text "' to a non-concrete type: " TE.:<>: TE.ShowType a)

type family NoFun (s :: Symbol) (a :: Type) (absError :: Void) :: Constraint where
    NoFun s (a -> b) abs = TE.TypeError (TE.Text "You have accidentaly applied `" TE.:<>: TE.Text s TE.:<>: TE.Text "' to a function type: " TE.:<>: TE.ShowType (a -> b))
    NoFun s t        abs = ()

-----

newtype ShowBuilder a = ShowBuilder ShowS

infixl 1 `showArg`

-- | The context is satisfied by any type @a@ that is manifestly apart from @->@
runShowsPrec :: NoFun "runShowsPrec" a (AbsError "runShowsPrec" a) => Int -> ShowBuilder a -> ShowS
runShowsPrec p (ShowBuilder x) = showParen (p >= 11) x

showCtor :: a -> String -> ShowBuilder a
showCtor a s =
    showCtorProxy (toProxy a) s
  where
    toProxy :: a -> Proxy a
    toProxy = const Proxy

showCtorProxy :: proxy a -> String -> ShowBuilder a
showCtorProxy _a s = ShowBuilder $ showString s

showArg :: Show a => ShowBuilder (a -> b) -> a -> ShowBuilder b
ShowBuilder l `showArg` r = ShowBuilder $ l .  showString " " . showsPrec 11 r

-----

newtype ReadBuilder a = ReadBuilder (Read.ReadPrec a)
  deriving (Applicative, Functor)

-- | The context is satisfied by any type @a@ that is manifestly apart from @->@
runReadPrec :: NoFun "runReadPrec" a (AbsError "runReadPrec" a) => ReadBuilder a -> Read.ReadPrec a
runReadPrec (ReadBuilder x) = Read.parens $ Read.prec 10 x

readCtor :: a -> String -> ReadBuilder a
readCtor a s = ReadBuilder $ a <$ Read.expectP (Read.Ident s)

readArg :: Read a => ReadBuilder a
readArg = ReadBuilder $ Read.step Read.readPrec

-----

-- | An opaque type that only allows for 'Eq' and human inspection
newtype Forgotten a = Forgotten a
  deriving (Eq, Show)

forgotten :: a -> Forgotten a
forgotten = Forgotten

newtype EqBuilder a b = EqBuilder Bool

-- | The context is satisfied by any type @a@ that is manifestly apart from @->@
runEq :: NoFun "runEq" a (AbsError "runEq" a) => EqBuilder a a -> Bool
runEq (EqBuilder x) = x

eqCtor :: a -> b -> EqBuilder a b
eqCtor a b =
    eqCtorProxy (toProxy a) (toProxy b)
  where
    toProxy :: x -> Proxy x
    toProxy = const Proxy

eqCtorProxy :: proxy a -> proxy b -> EqBuilder a b
eqCtorProxy _a _b = EqBuilder True

eqArg :: Eq x => EqBuilder (a -> a') (b -> b') -> (a -> x, a, b -> x, b) -> EqBuilder a' b'
EqBuilder acc `eqArg` (f, a, g, b) = EqBuilder $ acc && f a == g b
