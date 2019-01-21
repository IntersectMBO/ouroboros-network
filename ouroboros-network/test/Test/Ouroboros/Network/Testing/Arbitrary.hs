module Test.Ouroboros.Network.Testing.Arbitrary where

import Ouroboros.Network.Testing.ConcreteBlock
import Ouroboros.Network.Block (Slot (..), Hash (..))
import Ouroboros.Network.Chain (Point (..))

import           Test.QuickCheck

newtype ArbitraryPoint = ArbitraryPoint (Point BlockHeader)
  deriving (Show, Eq)

instance Arbitrary ArbitraryPoint where
  arbitrary = do
    slot <- Slot <$> arbitrary
    hash <- HeaderHash <$> arbitrary
    return $ ArbitraryPoint $ Point slot (BlockHash hash)
