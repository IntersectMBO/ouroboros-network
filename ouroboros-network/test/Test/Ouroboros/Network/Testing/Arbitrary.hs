module Test.Ouroboros.Network.Testing.Arbitrary where

import Ouroboros.Network.Testing.ConcreteBlock
import Ouroboros.Network.Block (Slot (..), Hash (..))
import Ouroboros.Network.Chain (Point (..))

import           Test.QuickCheck

newtype ArbitraryPoint = ArbitraryPoint {
    getArbitraryPoint :: Point BlockHeader
  }
  deriving (Show, Eq)

instance Arbitrary ArbitraryPoint where
  arbitrary = do
    slot <- Slot <$> arbitrary
    hash <- HeaderHash <$> arbitrary
    return $ ArbitraryPoint $ Point slot (BlockHash hash)

newtype ArbitraryBlockBody = ArbitraryBlockBody {
    getArbitraryBlockBody :: BlockBody
  }
  deriving (Show, Eq)

instance Arbitrary ArbitraryBlockBody where
    arbitrary = ArbitraryBlockBody . BlockBody <$> vectorOf 4 (choose ('A', 'Z'))
    -- probably no need for shrink, the content is arbitrary and opaque
    -- if we add one, it might be to shrink to an empty block
