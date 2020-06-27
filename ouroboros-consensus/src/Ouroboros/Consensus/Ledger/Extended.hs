{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.Ledger.Extended (
    -- * Extended ledger state
    ExtLedgerState(..)
  , ExtValidationError(..)
  , extLedgerCfgToTopLevel
  , extLedgerCfgFromTopLevel
    -- * Serialisation
  , encodeExtLedgerState
  , decodeExtLedgerState
    -- * Casts
  , castExtLedgerState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.Except
import           Data.Coerce
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

-- | Extended ledger state
--
-- This is the combination of the header state and the ledger state proper.
data ExtLedgerState blk = ExtLedgerState {
      ledgerState :: !(LedgerState blk)
    , headerState :: !(HeaderState blk)
    }
  deriving (Generic)

data ExtValidationError blk =
    ExtValidationErrorLedger !(LedgerError blk)
  | ExtValidationErrorHeader !(HeaderError blk)
  deriving (Generic)

instance LedgerSupportsProtocol blk => NoUnexpectedThunks (ExtValidationError blk)

deriving instance LedgerSupportsProtocol blk => Show (ExtLedgerState     blk)
deriving instance LedgerSupportsProtocol blk => Show (ExtValidationError blk)
deriving instance LedgerSupportsProtocol blk => Eq   (ExtValidationError blk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance LedgerSupportsProtocol blk => NoUnexpectedThunks (ExtLedgerState blk) where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState blk))

deriving instance ( LedgerSupportsProtocol blk
                  , Eq (ChainDepState (BlockProtocol blk))
                  ) => Eq (ExtLedgerState blk)

-- | Lemma:
--
-- > let tickedLedger = applyChainTick cfg (blockSlot blk) st
-- > in Right st' = runExcept $
-- >   applyLedgerBlock cfg blk tickedLedger ->
-- >      protocolLedgerView st'
-- >   == protocolLedgerView (tickedLedgerState tickedLedger)
--
-- In other words: 'applyLedgerBlock' doesn't affect the result of
-- 'protocolLedgerView'.
--
-- This should be true for each ledger because consensus depends on it.
_lemma_protocoLedgerView_applyLedgerBlock
  :: (LedgerSupportsProtocol blk, Eq (LedgerView (BlockProtocol blk)))
  => FullBlockConfig (LedgerState blk) blk
  -> blk
  -> LedgerState blk
  -> Either String ()
_lemma_protocoLedgerView_applyLedgerBlock cfg blk st
    | Right lhs' <- runExcept lhs
    , lhs' /= rhs
    = Left $ unlines
      [ "protocolLedgerView /= protocolLedgerView . applyLedgerBlock"
      , show lhs'
      , " /= "
      , show rhs
      ]
    | otherwise
    = Right ()
  where
    tickedLedger = applyChainTick lcfg (blockSlot blk) st
    lcfg = blockConfigLedger cfg
    lhs = protocolLedgerView lcfg <$> applyLedgerBlock cfg blk tickedLedger
    rhs = protocolLedgerView lcfg  $  tickedLedgerState        tickedLedger

{-------------------------------------------------------------------------------
  The extended ledger can behave like a ledger
-------------------------------------------------------------------------------}

-- | " Ledger " configuration for the extended ledger
--
-- Since the extended ledger also does the consensus protocol validation, we
-- also need the consensus config.
data ExtLedgerCfg blk = ExtLedgerCfg {
      extLedgerCfgProtocol :: !(FullProtocolConfig (BlockProtocol blk))
    , extLedgerCfgLedger   :: !(LedgerConfig blk)
    }
  deriving (Generic)

instance ( ConsensusProtocol (BlockProtocol blk)
         , NoUnexpectedThunks (LedgerConfig blk)
         ) => NoUnexpectedThunks (ExtLedgerCfg blk)

type instance LedgerCfg (ExtLedgerState blk) = ExtLedgerCfg blk

-- | The addition of the 'FullProtocolConfig' means we have the full config.
extLedgerCfgToTopLevel :: FullBlockConfig (ExtLedgerState blk) blk
                       -> TopLevelConfig blk
extLedgerCfgToTopLevel FullBlockConfig{..} = TopLevelConfig {
      topLevelConfigProtocol = extLedgerCfgProtocol
    , topLevelConfigBlock    = FullBlockConfig {
          blockConfigLedger = extLedgerCfgLedger
        , blockConfigBlock  = blockConfigBlock
        , blockConfigCodec  = blockConfigCodec
        }
    }
  where
    ExtLedgerCfg{..} = blockConfigLedger

extLedgerCfgFromTopLevel :: TopLevelConfig blk
                         -> FullBlockConfig (ExtLedgerState blk) blk
extLedgerCfgFromTopLevel tlc = FullBlockConfig {
      blockConfigLedger = ExtLedgerCfg {
          extLedgerCfgProtocol = topLevelConfigProtocol tlc
        , extLedgerCfgLedger   = configLedger tlc
        }
    , blockConfigBlock  = configBlock tlc
    , blockConfigCodec  = configCodec tlc
    }

type instance HeaderHash (ExtLedgerState blk) = HeaderHash (LedgerState blk)

instance ( IsLedger (LedgerState  blk)
         , LedgerSupportsProtocol blk
         )
      => IsLedger (ExtLedgerState blk) where
  type LedgerErr (ExtLedgerState blk) = ExtValidationError blk

  applyChainTick cfg slot (ExtLedgerState ledger header) =
      Ticked slot $ ExtLedgerState ledger' header
    where
      Ticked _slot ledger' = applyChainTick (extLedgerCfgLedger cfg) slot ledger

  ledgerTipPoint = castPoint . ledgerTipPoint . ledgerState

instance ( LedgerSupportsProtocol blk
         ) => ApplyBlock (ExtLedgerState blk) blk where
  applyLedgerBlock cfg blk (Ticked {
                                tickedSlotNo      = slot
                              , tickedLedgerState = ExtLedgerState lgr hdr
                              }) = do
      hdr' <- withExcept ExtValidationErrorHeader $
                validateHeader
                  tlc
                  (Ticked slot ledgerView)
                  (getHeader blk)
                  hdr
      lgr' <- withExcept ExtValidationErrorLedger $
                applyLedgerBlock
                  (mapLedgerCfg extLedgerCfgLedger cfg)
                  blk
                  (Ticked slot lgr)

      return $! ExtLedgerState lgr' hdr'
    where
      tlc :: TopLevelConfig blk
      tlc = extLedgerCfgToTopLevel cfg

      ledgerView :: LedgerView (BlockProtocol blk)
      ledgerView = protocolLedgerView (configLedger tlc) lgr

  reapplyLedgerBlock cfg blk (Ticked {
                                tickedSlotNo      = slot
                              , tickedLedgerState = ExtLedgerState lgr hdr
                              }) =
      ExtLedgerState {
          ledgerState = reapplyLedgerBlock
                          (mapLedgerCfg extLedgerCfgLedger cfg)
                          blk
                          (Ticked slot lgr)
        , headerState = cantBeError $
                         validateHeader
                           tlc
                           (Ticked slot ledgerView)
                           (getHeader blk)
                           hdr
        }
    where
      tlc :: TopLevelConfig blk
      tlc = extLedgerCfgToTopLevel cfg

      cantBeError :: Except e a -> a
      cantBeError = either (error "reapplyLedgerBlock: impossible") id
                  . runExcept

      ledgerView :: LedgerView (BlockProtocol blk)
      ledgerView = protocolLedgerView (configLedger tlc) lgr

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState :: (LedgerState   blk -> Encoding)
                     -> (ChainDepState (BlockProtocol blk) -> Encoding)
                     -> (AnnTip        blk -> Encoding)
                     -> ExtLedgerState blk -> Encoding
encodeExtLedgerState encodeLedgerState
                     encodeChainDepState
                     encodeAnnTip
                     ExtLedgerState{..} = mconcat [
      encodeLedgerState  ledgerState
    , encodeHeaderState' headerState
    ]
  where
    encodeHeaderState' = encodeHeaderState
                           encodeChainDepState
                           encodeAnnTip

decodeExtLedgerState :: (forall s. Decoder s (LedgerState    blk))
                     -> (forall s. Decoder s (ChainDepState  (BlockProtocol blk)))
                     -> (forall s. Decoder s (AnnTip         blk))
                     -> (forall s. Decoder s (ExtLedgerState blk))
decodeExtLedgerState decodeLedgerState
                     decodeChainDepState
                     decodeAnnTip = do
    ledgerState <- decodeLedgerState
    headerState <- decodeHeaderState'
    return ExtLedgerState{..}
  where
    decodeHeaderState' = decodeHeaderState
                           decodeChainDepState
                           decodeAnnTip

{-------------------------------------------------------------------------------
  Casts
-------------------------------------------------------------------------------}

castExtLedgerState
  :: ( Coercible (LedgerState blk)
                 (LedgerState blk')
     , Coercible (ChainDepState (BlockProtocol blk))
                 (ChainDepState (BlockProtocol blk'))
     , TipInfo blk ~ TipInfo blk'
     )
  => ExtLedgerState blk -> ExtLedgerState blk'
castExtLedgerState ExtLedgerState{..} = ExtLedgerState {
      ledgerState = coerce ledgerState
    , headerState = castHeaderState headerState
    }
