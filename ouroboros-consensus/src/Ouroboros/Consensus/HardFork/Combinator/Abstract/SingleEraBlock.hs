{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock (
    -- * Single era block
    SingleEraBlock (..)
  , proxySingle
  , singleEraTransition'
    -- * Era index
  , EraIndex (..)
  , eraIndexEmpty
  , eraIndexFromIndex
  , eraIndexFromNS
  , eraIndexSucc
  , eraIndexToInt
  , eraIndexZero
  ) where

import           Codec.Serialise
import           Data.Either (isRight)
import           Data.Proxy
import           Data.SOP.Strict
import qualified Data.Text as Text
import           Data.Void

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HardFork.History (Bound, EraParams)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsPeerSelection
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match

{-------------------------------------------------------------------------------
  SingleEraBlock
-------------------------------------------------------------------------------}

-- | Blocks from which we can assemble a hard fork
class ( LedgerSupportsProtocol blk
      , InspectLedger blk
      , LedgerSupportsMempool blk
      , HasTxId (GenTx blk)
      , QueryLedger blk
      , HasPartialConsensusConfig (BlockProtocol blk)
      , HasPartialLedgerConfig blk
      , ConvertRawHash blk
      , ReconstructNestedCtxt Header blk
      , CommonProtocolParams blk
      , LedgerSupportsPeerSelection blk
      , ConfigSupportsNode blk
      , NodeInitStorage blk
      , BlockSupportsMetrics blk
        -- Instances required to support testing
      , Eq   (GenTx blk)
      , Eq   (Validated (GenTx blk))
      , Eq   (ApplyTxErr blk)
      , Show blk
      , Show (Header blk)
      , Show (CannotForge blk)
      , Show (ForgeStateInfo blk)
      , Show (ForgeStateUpdateError blk)
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
          . (\name -> "<EraIndex " <> name <> ">")
          . Text.unpack
          . singleEraName
          $ singleEraInfo (Proxy @blk)

instance All SingleEraBlock xs => Condense (EraIndex xs) where
  condense = hcollapse . hcmap proxySingle getEraName . getEraIndex
    where
      getEraName :: forall blk. SingleEraBlock blk
                 => K () blk -> K String blk
      getEraName _ =
            K
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

eraIndexEmpty :: EraIndex '[] -> Void
eraIndexEmpty (EraIndex ns) = case ns of {}

eraIndexFromNS :: SListI xs => NS f xs -> EraIndex xs
eraIndexFromNS = EraIndex . hmap (const (K ()))

eraIndexFromIndex :: Index xs blk -> EraIndex xs
eraIndexFromIndex index = EraIndex $ injectNS index (K ())

eraIndexZero :: EraIndex (x ': xs)
eraIndexZero = EraIndex (Z (K ()))

eraIndexSucc :: EraIndex xs -> EraIndex (x ': xs)
eraIndexSucc (EraIndex ix) = EraIndex (S ix)

eraIndexToInt :: EraIndex xs -> Int
eraIndexToInt = index_NS . getEraIndex
