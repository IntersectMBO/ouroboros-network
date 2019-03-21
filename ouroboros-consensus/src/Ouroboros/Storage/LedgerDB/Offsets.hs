{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Storage.LedgerDB.Offsets (
    Offsets(..)
  , offsetsFromPairs
  , offsetsDropValues
  , offsetsWithoutValues
  ) where

import           Data.Word
import           GHC.Stack

-- | List of @a@s paired with offsets
--
-- The offsets must be strictly and monotonically increasing.
newtype Offsets a = Offsets { offsetsToPairs :: [(Word64, a)] }
  deriving (Show, Eq, Functor)

offsetsFromPairs :: forall a. HasCallStack => [(Word64, a)] -> Offsets a
offsetsFromPairs pairs =
    case pairs of
      []          -> error validationErr
      (o, a):rest -> Offsets $ (o, a) : validate o rest
  where
    validate :: Word64 -> [(Word64, a)] -> [(Word64, a)]
    validate _    []            = []
    validate prev ((o, a):rest)
                    | prev < o  = (o, a) : validate o rest
                    | otherwise = error validationErr

    validationErr :: String
    validationErr = "offsetsFromPairs: invalid " ++ show (map fst pairs)

offsetsDropValues :: Offsets a -> [Word64]
offsetsDropValues = map fst . offsetsToPairs

offsetsWithoutValues :: [Word64] -> Offsets ()
offsetsWithoutValues = offsetsFromPairs . map (, ())
