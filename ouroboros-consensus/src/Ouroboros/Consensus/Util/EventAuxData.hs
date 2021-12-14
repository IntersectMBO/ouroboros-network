{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.Util.EventAuxData (
    EventAuxData (..)
  , EventConstrainedData (..)
  ) where

import           Data.Kind (Constraint)
import           Data.Proxy (Proxy (..))
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

{-------------------------------------------------------------------------------
  Auxiliary data
-------------------------------------------------------------------------------}

-- | A wrapper for which '==' always returns 'True' and 'show' is a placeholder.
--
-- It is convenient for tracer event types to be particularly self-contained,
-- which means they may contain more fields than absolutely necessary. In
-- particular, some of those fields might be uninteresting in general. To
-- emphasize these _auxiliaryness_ of these fields, to avoid the sometimes
-- onerous instances required for such field's types, etc, wrap them in this
-- data type.
--
-- In general, such a field should either have a value that never changes during
-- a program execution (eg a pointer to some static config data) or else its
-- value is effectively determined by other fields in the same tracer event data
-- constructor by that constructor's invariants.
--
-- Most tracer events instantiate 'Eq' and 'Show', so we provide those trivial
-- instances for this type.
newtype EventAuxData (sym :: Symbol) a = EventAuxData { getEventAuxData :: a }

instance Eq (EventAuxData sym a) where
  (==) = \_ _ -> True

-- | A string that includes @"EventAuxData"@ and @sym@ and moreover is correct
-- Haskell syntax.
instance KnownSymbol sym => Show (EventAuxData sym a) where
  show _ = "(EventAuxData @\"" <> symbolVal (Proxy @sym) <> "\" _h)"

{-------------------------------------------------------------------------------
  Constrained data
-------------------------------------------------------------------------------}

-- | A type for data that is only availble if certain constraints are satsified
--
-- Because these constraints aren't necessarily available (otherwise this type
-- would be useless), this is often an argument to 'EventAuxData'.
newtype EventConstrainedData (c :: Constraint) a =
  EventConstrainedData { getEventConstrainedData :: c => a }

instance Functor (EventConstrainedData c) where
  fmap f (EventConstrainedData a) = EventConstrainedData (f a)
