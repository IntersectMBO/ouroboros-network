{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.SingleEra.Combined (
    -- * Value for /each/ era
    PerEraConsensusConfig(..)
  , PerEraChainSelConfig(..)
  , PerEraLedgerConfig(..)
  , PerEraBlockConfig(..)
  , PerEraCodecConfig(..)
  , PerEraForgeState(..)
    -- * Value for /one/ era
  , OneEraBlock(..)
  , OneEraHeader(..)
  , OneEraHash(..)
  , OneEraTipInfo(..)
  , OneEraEnvelopeErr(..)
  , OneEraValidationErr(..)
  , OneEraLedgerError(..)
  , OneEraValidateView(..)
  , OneEraSelectView(..)
  , OneEraIsLeader(..)
  , OneEraGenTx(..)
  , OneEraGenTxId(..)
  , OneEraApplyTxErr(..)
    -- * Value for two /different/ eras
  , MismatchEraInfo(..)
  , mismatchOneEra
    -- * Utility
  , oneEraBlockHeader
    -- * Distributive properties
  , distribPoint
  ) where

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import qualified Codec.Serialise.Encoding as Enc
import           Data.FingerTree.Strict (Measured (..))
import           Data.SOP.Strict hiding (shift)
import           Data.Void
import           Data.Word

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Mempool.API

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Info
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Wrappers
import           Ouroboros.Consensus.HardFork.Combinator.Util.DerivingVia
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match (Mismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

{-------------------------------------------------------------------------------
  Value for /each/ era

  TODO: Here and elsewhere we should use a strict variant of sop-core.
-------------------------------------------------------------------------------}

newtype PerEraConsensusConfig xs = PerEraConsensusConfig { getPerEraConsensusConfig :: NP SingleEraConsensusConfig xs }
newtype PerEraChainSelConfig  xs = PerEraChainSelConfig  { getPerEraChainSelConfig  :: NP SingleEraChainSelConfig  xs }
newtype PerEraLedgerConfig    xs = PerEraLedgerConfig    { getPerEraLedgerConfig    :: NP SingleEraLedgerConfig    xs }
newtype PerEraBlockConfig     xs = PerEraBlockConfig     { getPerEraBlockConfig     :: NP BlockConfig              xs }
newtype PerEraCodecConfig     xs = PerEraCodecConfig     { getPerEraCodecConfig     :: NP CodecConfig              xs }
newtype PerEraForgeState      xs = PerEraForgeState      { getPerEraForgeState      :: NP SingleEraForgeState      xs }

{-------------------------------------------------------------------------------
  Value for /one/ era
-------------------------------------------------------------------------------}

newtype OneEraBlock         xs = OneEraBlock         { getOneEraBlock         :: NS I                        xs }
newtype OneEraHeader        xs = OneEraHeader        { getOneEraHeader        :: NS Header                   xs }
newtype OneEraHash          xs = OneEraHash          { getOneEraHash          :: NS SingleEraHash            xs }
newtype OneEraTipInfo       xs = OneEraTipInfo       { getOneEraTipInfo       :: NS SingleEraTipInfo         xs }
newtype OneEraEnvelopeErr   xs = OneEraEnvelopeErr   { getOneEraEnvelopeErr   :: NS SingleEraEnvelopeErr     xs }
newtype OneEraValidationErr xs = OneEraValidationErr { getOneEraValidationErr :: NS SingleEraValidationErr   xs }
newtype OneEraLedgerError   xs = OneEraLedgerError   { getOneEraLedgerError   :: NS SingleEraLedgerError     xs }
newtype OneEraValidateView  xs = OneEraValidateView  { getOneEraValidateView  :: NS SingleEraValidateView    xs }
newtype OneEraSelectView    xs = OneEraSelectView    { getOneEraSelectView    :: NS SingleEraSelectView      xs }
newtype OneEraIsLeader      xs = OneEraIsLeader      { getOneEraIsLeader      :: NS SingleEraIsLeader        xs }
newtype OneEraGenTx         xs = OneEraGenTx         { getOneEraGenTx         :: NS GenTx                    xs }
newtype OneEraGenTxId       xs = OneEraGenTxId       { getOneEraGenTxId       :: NS SingleEraGenTxId         xs }
newtype OneEraApplyTxErr    xs = OneEraApplyTxErr    { getOneEraApplyTxErr    :: NS SingleEraApplyTxErr      xs }

{-------------------------------------------------------------------------------
  Value for two /different/ eras
-------------------------------------------------------------------------------}

newtype MismatchEraInfo xs = MismatchEraInfo {
      -- | Era mismatch
      --
      -- We have an era mismatch between the era of a block/header/tx/query
      -- and the era of the current ledger.
      getMismatchEraInfo :: Mismatch SingleEraInfo LedgerEraInfo xs
    }

mismatchOneEra :: MismatchEraInfo '[b] -> Void
mismatchOneEra = Match.mismatchOne . getMismatchEraInfo

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

oneEraBlockHeader :: CanHardFork xs => OneEraBlock xs -> OneEraHeader xs
oneEraBlockHeader =
      OneEraHeader
    . hcmap proxySingle (getHeader . unI)
    . getOneEraBlock

{-------------------------------------------------------------------------------
  HasHeader instance for OneEraHeader
-------------------------------------------------------------------------------}

type instance HeaderHash (OneEraHeader xs) = OneEraHash xs

instance CanHardFork xs => StandardHash (OneEraHeader xs)

instance CanHardFork xs => Measured BlockMeasure (OneEraHeader xs) where
  measure = blockMeasure

instance CanHardFork xs => HasHeader (OneEraHeader xs) where
  blockHash     = OneEraHash
                . hcmap proxySingle (SingleEraHash . blockHash)
                . getOneEraHeader
  blockPrevHash = distribChainHash
                . hcmap proxySingle (Comp . blockPrevHash)
                . getOneEraHeader

  blockSlot = hcollapse . hcmap proxySingle (K . blockSlot) . getOneEraHeader
  blockNo   = hcollapse . hcmap proxySingle (K . blockNo)   . getOneEraHeader

  blockInvariant = const True

{-------------------------------------------------------------------------------
  HasHeader instance for OneEraBlock
-------------------------------------------------------------------------------}

type instance HeaderHash (OneEraBlock xs) = OneEraHash xs

instance CanHardFork xs => StandardHash (OneEraBlock xs)

instance CanHardFork xs => Measured BlockMeasure (OneEraBlock xs) where
  measure = blockMeasure

instance CanHardFork xs => HasHeader (OneEraBlock xs) where
  blockHash      =            blockHash     . oneEraBlockHeader
  blockPrevHash  = castHash . blockPrevHash . oneEraBlockHeader
  blockSlot      =            blockSlot     . oneEraBlockHeader
  blockNo        =            blockNo       . oneEraBlockHeader
  blockInvariant = const True

{-------------------------------------------------------------------------------
  Distributive properties
-------------------------------------------------------------------------------}

distribChainHash :: NS (ChainHash :.: Header) xs -> ChainHash (OneEraHeader xs)
distribChainHash = go
  where
    go :: NS (ChainHash :.: Header) xs -> ChainHash (OneEraHeader xs)
    go (Z (Comp GenesisHash))   = GenesisHash
    go (Z (Comp (BlockHash h))) = BlockHash (OneEraHash (Z $ SingleEraHash h))
    go (S h)                    = shiftChainHash $ go h

distribPoint :: NS Point xs -> Point (OneEraBlock xs)
distribPoint = go
  where
    go :: NS Point xs -> Point (OneEraBlock xs)
    go (Z GenesisPoint)     = GenesisPoint
    go (Z (BlockPoint s h)) = BlockPoint s (OneEraHash $ Z $ SingleEraHash h)
    go (S p)                = shiftPoint (go p)

shiftChainHash :: ChainHash (OneEraHeader xs) -> ChainHash (OneEraHeader (x ': xs))
shiftChainHash GenesisHash   = GenesisHash
shiftChainHash (BlockHash h) = BlockHash (shiftHash h)

shiftPoint :: Point (OneEraBlock xs) -> Point (OneEraBlock (x ': xs))
shiftPoint GenesisPoint     = GenesisPoint
shiftPoint (BlockPoint s h) = BlockPoint s (shiftHash h)

shiftHash :: OneEraHash xs -> OneEraHash (x ': xs)
shiftHash (OneEraHash h) = OneEraHash (S h)

{-------------------------------------------------------------------------------
  NoUnexpectedThunks instances
-------------------------------------------------------------------------------}

deriving via LiftNamedNP "PerEraBlockConfig" BlockConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraBlockConfig xs)

deriving via LiftNamedNP "PerEraConsensusConfig" SingleEraConsensusConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraConsensusConfig xs)

deriving via LiftNamedNP "PerEraChainSelConfig" SingleEraChainSelConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraChainSelConfig xs)

deriving via LiftNamedNP "PerEraLedgerConfig" SingleEraLedgerConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraLedgerConfig xs)

deriving via LiftNamedNP "PerEraForgeState" SingleEraForgeState xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraForgeState xs)

deriving via LiftNamedNS "OneEraHeader" Header xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraHeader xs)

deriving via LiftNamedNS "OneEraHash" SingleEraHash xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraHash xs)

deriving via LiftNamedNS "OneEraEnvelopeErr" SingleEraEnvelopeErr xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraEnvelopeErr xs)

deriving via LiftNamedNS "OneEraTipInfo" SingleEraTipInfo xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraTipInfo xs)

deriving via LiftNamedNS "OneEraGenTxId" SingleEraGenTxId xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraGenTxId xs)

deriving via LiftNamedNS "OneEraLedgerError" SingleEraLedgerError xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraLedgerError xs)

deriving via LiftNamedNS "OneEraValidationErr" SingleEraValidationErr xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraValidationErr xs)

deriving via LiftNamedNS "OneEraGenTx" GenTx xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraGenTx xs)

deriving via LiftNamedMismatch "MismatchEraInfo" SingleEraInfo LedgerEraInfo xs
         instance CanHardFork xs => NoUnexpectedThunks (MismatchEraInfo xs)

{-------------------------------------------------------------------------------
  Other instances
-------------------------------------------------------------------------------}

deriving via LiftNS SingleEraHash          xs instance CanHardFork xs => Eq   (OneEraHash xs)
deriving via LiftNS SingleEraHash          xs instance CanHardFork xs => Ord  (OneEraHash xs)
deriving via LiftNS SingleEraHash          xs instance CanHardFork xs => Show (OneEraHash xs)

deriving via LiftNS SingleEraTipInfo       xs instance CanHardFork xs => Eq   (OneEraTipInfo xs)
deriving via LiftNS SingleEraTipInfo       xs instance CanHardFork xs => Show (OneEraTipInfo xs)

deriving via LiftNS SingleEraEnvelopeErr   xs instance CanHardFork xs => Eq   (OneEraEnvelopeErr xs)
deriving via LiftNS SingleEraEnvelopeErr   xs instance CanHardFork xs => Show (OneEraEnvelopeErr xs)

deriving via LiftNS SingleEraGenTxId       xs instance CanHardFork xs => Eq   (OneEraGenTxId xs)
deriving via LiftNS SingleEraGenTxId       xs instance CanHardFork xs => Ord  (OneEraGenTxId xs)

deriving via LiftNS SingleEraLedgerError   xs instance CanHardFork xs => Eq   (OneEraLedgerError xs)
deriving via LiftNS SingleEraLedgerError   xs instance CanHardFork xs => Show (OneEraLedgerError xs)

deriving via LiftNS SingleEraValidationErr xs instance CanHardFork xs => Eq   (OneEraValidationErr xs)
deriving via LiftNS SingleEraValidationErr xs instance CanHardFork xs => Show (OneEraValidationErr xs)

deriving via LiftMismatch SingleEraInfo LedgerEraInfo xs instance CanHardFork xs => Eq   (MismatchEraInfo xs)
deriving via LiftMismatch SingleEraInfo LedgerEraInfo xs instance CanHardFork xs => Show (MismatchEraInfo xs)

{-------------------------------------------------------------------------------
  Show instances used in tests only
-------------------------------------------------------------------------------}

deriving via LiftNS I                   xs instance CanHardFork xs => Show (OneEraBlock      xs)
deriving via LiftNS Header              xs instance CanHardFork xs => Show (OneEraHeader     xs)
deriving via LiftNS GenTx               xs instance CanHardFork xs => Show (OneEraGenTx      xs)
deriving via LiftNS SingleEraGenTxId    xs instance CanHardFork xs => Show (OneEraGenTxId    xs)
deriving via LiftNS SingleEraApplyTxErr xs instance CanHardFork xs => Show (OneEraApplyTxErr xs)

{-------------------------------------------------------------------------------
  Serialisation

  This is only required for the PBFT hack (see 'rewindConsensusState')
-------------------------------------------------------------------------------}

deriving instance SingleEraBlock blk => Serialise (SingleEraHash blk)

deriving via SerialiseOne SingleEraHash xs
         instance CanHardFork xs => Serialise (OneEraHash xs)

{-------------------------------------------------------------------------------
  Serialise support
-------------------------------------------------------------------------------}

newtype SerialiseOne f xs = SerialiseOne (NS f xs)

instance ( All SingleEraBlock xs
         , forall blk. SingleEraBlock blk => Serialise (f blk)
         ) => Serialise (SerialiseOne f xs) where
  encode (SerialiseOne ns) =
      hcollapse $ hczipWith proxySingle aux indices ns
    where
      aux :: SingleEraBlock blk => K Word8 blk -> f blk -> K Encoding blk
      aux (K i) x = K (Enc.encodeWord8 i <> encode x)

      indices :: NP (K Word8) xs
      indices = go 0 sList
        where
          go :: Word8 -> SList xs' -> NP (K Word8) xs'
          go !_ SNil  = Nil
          go !i SCons = K i :* go (i + 1) sList

  decode = do
      i <- fromIntegral <$> Dec.decodeWord8
      if i < length decoders
        then SerialiseOne <$> decoders !! i
        else fail "decode: invalid index"
    where
      decoders :: [Decoder s (NS f xs)]
      decoders = hcollapse $ hcmap proxySingle aux injections

      aux :: SingleEraBlock blk
          => Injection f xs blk -> K (Decoder s (NS f xs)) blk
      aux inj = K (unK . apFn inj <$> decode)
