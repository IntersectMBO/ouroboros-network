{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel (HardForkSelectView(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel as ChainSel
module Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel (
    HardForkSelectView(..)
  ) where

import           Data.Functor.Product
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match

data HardForkSelectView xs = HardForkSelectView {
      hardForkSelectViewBlockNo :: BlockNo
    , hardForkSelectViewOneEra  :: OneEraSelectView xs
    }
  deriving (Show)

-- | Chain selection across eras
--
-- TODO: If the two chains are from different eras, we simply pick the longer
-- one. This may not be okay for all hard fork transitions; we might need to
-- generalize this later.
instance CanHardFork xs => ChainSelection (HardForkProtocol xs) where
  type ChainSelConfig (HardForkProtocol xs) = PerEraChainSelConfig xs
  type SelectView     (HardForkProtocol xs) = HardForkSelectView   xs

  -- We leave 'preferCandidate' at the default

  compareCandidates _ (PerEraChainSelConfig cfgs) =
      either (uncurry different) same .: matchView
    where
      -- If the two views are from the same era, just use 'compareCandidates'
      same :: NS (Product WrapSelectView WrapSelectView) xs -> Ordering
      same = hcollapse . hczipWith proxySingle compareCandidates' cfgs

      -- If the two tips are in different eras, just compare chain length
      different :: BlockNo -> BlockNo -> Ordering
      different = Prelude.compare

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

compareCandidates' :: forall blk. SingleEraBlock blk
                   => WrapChainSelConfig blk
                   -> Product WrapSelectView WrapSelectView blk
                   -> K Ordering blk
compareCandidates' (WrapChainSelConfig cfg)
                   (Pair (WrapSelectView view1)
                         (WrapSelectView view2)) = K $
    compareCandidates (Proxy @(BlockProtocol blk)) cfg view1 view2

matchView :: HardForkSelectView xs
          -> HardForkSelectView xs
          -> Either (BlockNo, BlockNo)
                    (NS (Product WrapSelectView WrapSelectView) xs)
matchView cand1 cand2 =
    case matchNS (getOneEraSelectView $ hardForkSelectViewOneEra cand1)
                 (getOneEraSelectView $ hardForkSelectViewOneEra cand2) of
      Right matched  -> Right matched
      Left _mismatch -> Left  ( hardForkSelectViewBlockNo cand1
                              , hardForkSelectViewBlockNo cand2
                              )
