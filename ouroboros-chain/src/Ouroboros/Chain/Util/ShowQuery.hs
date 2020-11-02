{-# LANGUAGE QuantifiedConstraints #-}
module Ouroboros.Chain.Util.ShowQuery (
    ShowQuery (..)
  ) where

-- | Useful in combination with 'Ouroboros.Network.Protocol.LocalStateQuery.Type'.
--
-- Defined in this package to avoid a dependency from @ouroboros-consensus@ on @ouroboros-network@.
--
-- To implement 'Show' for:
--
-- > (Message (LocalStateQuery block query) st st')
--
-- we need a way to print the @query@ GADT and its type index, @result@. This
-- class contain the method we need to provide this 'Show' instance.
--
-- We use a type class for this, as this 'Show' constraint propagates to a lot
-- of places.
class (forall result. Show (query result)) => ShowQuery query where
    showResult :: forall result. query result -> result -> String
