module Chain.Update where

import Block (Block)
import Chain (Point)

data ChainUpdate = AddBlock Block
                 | RollBack Point
  deriving Show
