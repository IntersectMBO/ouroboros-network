{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Condense instances
--
-- These are for the benefit of integration and tests. We do not rely on them
-- within consensus.
--
-- NOTE: No guarantees are made about what these condense instances look like.
module Ouroboros.Consensus.HardFork.Combinator.Condense (CondenseConstraints) where

import           Data.Coerce
import           Data.SOP.Strict

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

class ( Condense blk
      , Condense (Header blk)
      , Condense (GenTx blk)
      , Condense (GenTxId blk)
      ) => CondenseConstraints blk

pCondense :: Proxy CondenseConstraints
pCondense = Proxy

defaultCondenseNS :: ( All CondenseConstraints xs
                     , forall blk. CondenseConstraints blk => Condense (f blk)
                     )
                  => Proxy f -> NS f xs -> String
defaultCondenseNS _ = hcollapse . hcmap pCondense (K . condense)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance All CondenseConstraints xs => Condense (HardForkBlock xs) where
  condense = defaultCondenseNS (Proxy @I) . coerce

instance All CondenseConstraints xs => Condense (Header (HardForkBlock xs)) where
  condense = defaultCondenseNS (Proxy @Header) . coerce

instance All CondenseConstraints xs => Condense (GenTx (HardForkBlock xs)) where
  condense = defaultCondenseNS (Proxy @GenTx) . coerce

instance All CondenseConstraints xs => Condense (TxId (GenTx (HardForkBlock xs))) where
  condense = defaultCondenseNS (Proxy @WrapGenTxId) . coerce

{-------------------------------------------------------------------------------
  Forwarding
-------------------------------------------------------------------------------}

instance Condense a => Condense (I a) where
  condense = condense . unI

instance Condense (GenTxId blk) => Condense (WrapGenTxId blk) where
  condense = condense . unwrapGenTxId
