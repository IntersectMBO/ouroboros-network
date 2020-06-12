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
    encodeCurrent
  , decodeCurrent
  , encodePast
  , decodePast
  ) where

import           Prelude hiding (sequence)

import           Codec.CBOR.Decoding (Decoder, decodeListLen)
import           Codec.CBOR.Encoding (Encoding, encodeListLen)
import           Codec.Serialise
import           Data.SOP.Strict hiding (shape)

import           Cardano.Binary (enforceSize)
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.State.Lift
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.DerivingVia

{-------------------------------------------------------------------------------
  SOP class instances

  These are convenient, allowing us to treat the 'HardForkState' just like any
  other SOP type; in particular, they deal with lifting functions to 'Current'.
-------------------------------------------------------------------------------}

type instance Prod    (HardForkState_ g)   = NP
type instance SListIN (HardForkState_ g)   = SListI
type instance AllN    (HardForkState_ g) c = All c

instance HAp (HardForkState_ g) where
  hap np (HardForkState st) = HardForkState $
      hap (map_NP' (Fn . lift . apFn) np) st

instance HSequence (HardForkState_ g) where
  hctraverse' = \p f (HardForkState st) -> HardForkState <$>
                                              hctraverse' p (liftM f) st
  htraverse' = hctraverse' (Proxy @Top)
  hsequence' = htraverse' unComp

{-------------------------------------------------------------------------------
  Eq, Show, NoUnexpectedThunks
-------------------------------------------------------------------------------}

deriving instance Eq                 (f blk) => Eq                 (Current f blk)
deriving instance Show               (f blk) => Show               (Current f blk)
deriving instance NoUnexpectedThunks (f blk) => NoUnexpectedThunks (Current f blk)

deriving instance Eq                 (f blk) => Eq                 (Past f blk)
deriving instance Show               (f blk) => Show               (Past f blk)
deriving instance NoUnexpectedThunks (f blk) => NoUnexpectedThunks (Past f blk)

deriving instance Eq                 (f blk) => Eq                 (Snapshot f blk)
deriving instance Show               (f blk) => Show               (Snapshot f blk)
deriving instance NoUnexpectedThunks (f blk) => NoUnexpectedThunks (Snapshot f blk)

deriving via LiftTelescope (Past g) (Current f) xs
         instance ( All SingleEraBlock xs
                  , forall blk. SingleEraBlock blk => Show (f blk)
                  , forall blk. SingleEraBlock blk => Show (g blk)
                  ) => Show (HardForkState_ g f xs)

deriving via LiftTelescope (Past g) (Current f) xs
         instance ( All SingleEraBlock xs
                  , forall blk. SingleEraBlock blk => Eq (f blk)
                  , forall blk. SingleEraBlock blk => Eq (g blk)
                  ) => Eq (HardForkState_ g f xs)

deriving via LiftNamedTelescope "HardForkState" (Past g) (Current f) xs
         instance ( All SingleEraBlock xs
                  , forall blk. SingleEraBlock blk => NoUnexpectedThunks (f blk)
                  , forall blk. SingleEraBlock blk => NoUnexpectedThunks (g blk)
                  ) => NoUnexpectedThunks (HardForkState_ g f xs)

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

encodePast :: (f blk -> Encoding) -> Past f blk -> Encoding
encodePast f Past{..} = mconcat [
      encodeListLen 3
    , encode pastStart
    , encode pastEnd
    , encodeSnapshot f pastSnapshot
    ]

decodePast :: Decoder s (f blk) -> Decoder s (Past f blk)
decodePast f = do
    enforceSize "decodePast" 3
    pastStart    <- decode
    pastEnd      <- decode
    pastSnapshot <- decodeSnapshot f
    return Past{..}

encodeSnapshot :: (f blk -> Encoding) -> Snapshot f blk -> Encoding
encodeSnapshot f = \case
    NoSnapshot    -> encodeListLen 0
    Snapshot n ss -> mconcat [
        encodeListLen 2
      , encode n
      , f ss
      ]

decodeSnapshot :: Decoder s (f blk) -> Decoder s (Snapshot f blk)
decodeSnapshot f = do
    n <- decodeListLen
    case n of
      0 -> return NoSnapshot
      2 -> Snapshot <$> decode <*> f
      _ -> fail "decodeSnapshot: invalid tag"

instance Serialise (f blk) => Serialise (Current f blk) where
  encode = encodeCurrent encode
  decode = decodeCurrent decode

instance Serialise (f blk) => Serialise (Past f blk) where
  encode = encodePast encode
  decode = decodePast decode

instance Serialise (f blk) => Serialise (Snapshot f blk) where
  encode = encodeSnapshot encode
  decode = decodeSnapshot decode
