{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.State.Instances (
    -- * Serialisation support
    decodeCurrent
  , decodePast
  , encodeCurrent
  , encodePast
  ) where

import           Prelude hiding (sequence)

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise
import           Data.SOP.Strict hiding (shape)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (enforceSize)

import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.State.Lift
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.DerivingVia
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  SOP class instances

  These are convenient, allowing us to treat the 'HardForkState' just like any
  other SOP type; in particular, they deal with lifting functions to 'Current'.
-------------------------------------------------------------------------------}

type instance Prod       HardForkState   = NP
type instance SListIN    HardForkState   = SListI
type instance AllN       HardForkState c = All c
type instance CollapseTo HardForkState a = a

instance HAp HardForkState where
  hap np (HardForkState st) = HardForkState $
      hap (map_NP' (Fn . lift . apFn) np) st

instance HSequence HardForkState where
  hctraverse' = \p f (HardForkState st) -> HardForkState <$>
                                              hctraverse' p (liftM f) st
  htraverse' = hctraverse' (Proxy @Top)
  hsequence' = htraverse' unComp

instance HCollapse HardForkState where
  hcollapse = hcollapse . hmap currentState . Telescope.tip . getHardForkState

{-------------------------------------------------------------------------------
  Eq, Show, NoThunks
-------------------------------------------------------------------------------}

deriving instance Eq       (f blk) => Eq       (Current f blk)
deriving instance Show     (f blk) => Show     (Current f blk)
deriving instance NoThunks (f blk) => NoThunks (Current f blk)

deriving via LiftTelescope (K Past) (Current f) xs
         instance ( All SingleEraBlock xs
                  , forall blk. SingleEraBlock blk => Show (f blk)
                  ) => Show (HardForkState f xs)

deriving via LiftTelescope (K Past) (Current f) xs
         instance ( All SingleEraBlock xs
                  , forall blk. SingleEraBlock blk => Eq (f blk)
                  ) => Eq (HardForkState f xs)

deriving via LiftNamedTelescope "HardForkState" (K Past) (Current f) xs
         instance ( All SingleEraBlock xs
                  , forall blk. SingleEraBlock blk => NoThunks (f blk)
                  ) => NoThunks (HardForkState f xs)

{-------------------------------------------------------------------------------
  Serialisation

  The 'Serialise' instances are primarily useful for the tests, but the general
  encoders/decoders are used by the HFC to store the ledger state.
-------------------------------------------------------------------------------}

encodeCurrent :: (f blk -> Encoding) -> Current f blk -> Encoding
encodeCurrent f Current{..} = mconcat [
      encodeListLen 2
    , encode currentStart
    , f currentState
    ]

decodeCurrent :: Decoder s (f blk) -> Decoder s (Current f blk)
decodeCurrent f = do
    enforceSize "decodeCurrent" 2
    currentStart <- decode
    currentState <- f
    return Current{..}

encodePast :: Past -> Encoding
encodePast Past{..} = mconcat [
      encodeListLen 2
    , encode pastStart
    , encode pastEnd
    ]

decodePast :: Decoder s Past
decodePast = do
    enforceSize "decodePast" 2
    pastStart <- decode
    pastEnd   <- decode
    return Past{..}

instance Serialise (f blk) => Serialise (Current f blk) where
  encode = encodeCurrent encode
  decode = decodeCurrent decode

instance Serialise Past where
  encode = encodePast
  decode = decodePast
