{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock (
    -- * Single era block
    SingleEraBlock(..)
  , singleEraTransition'
  , proxySingle
    -- * Era index
  , EraIndex(..)
  , emptyEraIndex
  ) where

import           Codec.Serialise
import           Data.Either (isRight)
import           Data.Proxy
import           Data.SOP.BasicFunctors (K (..))
import           Data.SOP.Strict
import qualified Data.Text as Text
import           Data.Void

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.History (Bound, EraParams)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match

{-------------------------------------------------------------------------------
  SingleEraBlock
-------------------------------------------------------------------------------}

-- | Blocks from which we can assemble a hard fork
class ( LedgerSupportsProtocol blk
      , LedgerSupportsMempool blk
      , HasTxId (GenTx blk)
      , QueryLedger blk
      , CanForge blk
      , HasPartialConsensusConfig (BlockProtocol blk)
      , HasPartialLedgerConfig blk
      , ConvertRawHash blk
      , ReconstructNestedCtxt Header blk
      , CommonProtocolParams blk
        -- Instances required to support testing
      , Eq   (GenTx blk)
      , Eq   (ApplyTxErr blk)
      , Show blk
      , Show (Header blk)
      ) => SingleEraBlock blk where

  -- | Era transition
  --
  -- This should only report the transition point once it is stable (rollback
  -- cannot affect it anymore).
  --
  -- Since we need this to construct the 'HardForkSummary' (and hence the
  -- 'EpochInfo'), this takes the /partial/ config, not the full config
  -- (or we'd end up with a catch-22).
  singleEraTransition :: PartialLedgerConfig blk
                      -> EraParams -- ^ Current era parameters
                      -> Bound     -- ^ Start of this era
                      -> LedgerState blk
                      -> Maybe EpochNo

  -- | Era information (for use in error messages)
  singleEraInfo       :: proxy blk -> SingleEraInfo blk

proxySingle :: Proxy SingleEraBlock
proxySingle = Proxy

singleEraTransition' :: SingleEraBlock blk
                     => WrapPartialLedgerConfig blk
                     -> EraParams
                     -> Bound
                     -> LedgerState blk -> Maybe EpochNo
singleEraTransition' = singleEraTransition . unwrapPartialLedgerConfig

{-------------------------------------------------------------------------------
  Era index
-------------------------------------------------------------------------------}

newtype EraIndex xs = EraIndex {
      getEraIndex :: NS (K ()) xs
    }

instance Eq (EraIndex xs) where
  EraIndex era == EraIndex era' = isRight (matchNS era era')

instance All SingleEraBlock xs => Show (EraIndex xs) where
  show = hcollapse . hcmap proxySingle getEraName . getEraIndex
    where
      getEraName :: forall blk. SingleEraBlock blk
                 => K () blk -> K String blk
      getEraName _ =
            K
          . ("EraIndex " <>)
          . Text.unpack
          . singleEraName
          $ singleEraInfo (Proxy @blk)

instance SListI xs => Serialise (EraIndex xs) where
  encode = encode . nsToIndex . getEraIndex
  decode = do
    idx <- decode
    case nsFromIndex idx of
      Nothing       -> fail $ "EraIndex: invalid index " <> show idx
      Just eraIndex -> return (EraIndex eraIndex)

emptyEraIndex :: EraIndex '[] -> Void
emptyEraIndex (EraIndex ns) = case ns of {}
