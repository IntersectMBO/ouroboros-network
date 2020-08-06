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
  , ExtLedgerCfg(..)
  , extLedgerCfgToTopLevel
  , extLedgerCfgFromTopLevel
    -- * Serialisation
  , encodeExtLedgerState
  , decodeExtLedgerState
    -- * Casts
  , castExtLedgerState
    -- * Type family instances
  , Ticked(..)
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

{-------------------------------------------------------------------------------
  The extended ledger can behave like a ledger
-------------------------------------------------------------------------------}

data instance Ticked (ExtLedgerState blk) = TickedExtLedgerState {
      tickedLedgerState :: Ticked (LedgerState blk)
    , tickedLedgerView  :: Ticked (LedgerView (BlockProtocol blk))
    , tickedHeaderState :: Ticked (HeaderState blk)
    }

-- | " Ledger " configuration for the extended ledger
--
-- Since the extended ledger also does the consensus protocol validation, we
-- also need the consensus config.
data ExtLedgerCfg blk = ExtLedgerCfg {
      extLedgerCfgProtocol :: !(ConsensusConfig (BlockProtocol blk))
    , extLedgerCfgLedger   :: !(LedgerConfig blk)
    }
  deriving (Generic)

instance ( ConsensusProtocol (BlockProtocol blk)
         , NoUnexpectedThunks (LedgerConfig blk)
         ) => NoUnexpectedThunks (ExtLedgerCfg blk)

type instance LedgerCfg (ExtLedgerState blk) = ExtLedgerCfg blk

-- | The addition of the 'ConsensusConfig' means we have the full config.
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

instance IsLedger (LedgerState blk) => GetTip (ExtLedgerState blk) where
  getTip = castPoint . getTip . ledgerState

instance IsLedger (LedgerState blk) => GetTip (Ticked (ExtLedgerState blk)) where
  getTip = castPoint . getTip . tickedLedgerState

instance ( IsLedger (LedgerState  blk)
         , LedgerSupportsProtocol blk
         )
      => IsLedger (ExtLedgerState blk) where
  type LedgerErr (ExtLedgerState blk) = ExtValidationError blk

  applyChainTick cfg slot (ExtLedgerState ledger header) =
      TickedExtLedgerState {..}
    where
      lcfg :: LedgerConfig blk
      lcfg = extLedgerCfgLedger cfg

      tickedLedgerState :: Ticked (LedgerState blk)
      tickedLedgerState = applyChainTick lcfg slot ledger

      tickedLedgerView :: Ticked (LedgerView (BlockProtocol blk))
      tickedLedgerView = protocolLedgerView lcfg tickedLedgerState

      tickedHeaderState :: Ticked (HeaderState blk)
      tickedHeaderState =
          tickHeaderState
            (extLedgerCfgProtocol cfg)
            tickedLedgerView
            slot
            header

instance LedgerSupportsProtocol blk => ApplyBlock (ExtLedgerState blk) blk where
  applyLedgerBlock cfg blk TickedExtLedgerState{..} = ExtLedgerState
      <$> (withExcept ExtValidationErrorLedger $
             applyLedgerBlock
               (mapLedgerCfg extLedgerCfgLedger cfg)
               blk
               tickedLedgerState)
      <*> (withExcept ExtValidationErrorHeader $
             validateHeader
               (extLedgerCfgToTopLevel cfg)
               tickedLedgerView
               (getHeader blk)
               tickedHeaderState)

  reapplyLedgerBlock cfg blk TickedExtLedgerState{..} = ExtLedgerState {
        ledgerState =
             reapplyLedgerBlock
               (mapLedgerCfg extLedgerCfgLedger cfg)
               blk
               tickedLedgerState
      , headerState = cantBeError $
             validateHeader
               (extLedgerCfgToTopLevel cfg)
               tickedLedgerView
               (getHeader blk)
               tickedHeaderState
      }
    where
      cantBeError :: Show e => Except e a -> a
      cantBeError =
            either
              (\e ->
                  error $
                    "reapplyLedgerBlock " <>
                    show (blockPoint blk) <>
                    ": " <> show e)
              id
          . runExcept

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
