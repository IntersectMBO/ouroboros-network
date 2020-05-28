{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.AcrossEras (
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
  , getSameValue
  ) where

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import qualified Codec.Serialise.Encoding as Enc
import           Control.Monad.Except (throwError)
import qualified Data.ByteString as Strict
import           Data.FingerTree.Strict (Measured (..))
import           Data.SOP.Strict hiding (shift)
import           Data.Void
import           Data.Word
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (allEqual)
import           Ouroboros.Consensus.Util.Assert

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Util.DerivingVia
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match (Mismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

{-------------------------------------------------------------------------------
  Value for /each/ era

  TODO: Here and elsewhere we should use a strict variant of sop-core.
-------------------------------------------------------------------------------}

newtype PerEraConsensusConfig xs = PerEraConsensusConfig { getPerEraConsensusConfig :: NP WrapPartialConsensusConfig xs }
newtype PerEraChainSelConfig  xs = PerEraChainSelConfig  { getPerEraChainSelConfig  :: NP WrapChainSelConfig         xs }
newtype PerEraLedgerConfig    xs = PerEraLedgerConfig    { getPerEraLedgerConfig    :: NP WrapPartialLedgerConfig    xs }
newtype PerEraBlockConfig     xs = PerEraBlockConfig     { getPerEraBlockConfig     :: NP BlockConfig                xs }
newtype PerEraCodecConfig     xs = PerEraCodecConfig     { getPerEraCodecConfig     :: NP CodecConfig                xs }
newtype PerEraForgeState      xs = PerEraForgeState      { getPerEraForgeState      :: NP WrapForgeState             xs }

{-------------------------------------------------------------------------------
  Value for /one/ era
-------------------------------------------------------------------------------}

newtype OneEraBlock         xs = OneEraBlock         { getOneEraBlock         :: NS I                 xs }
newtype OneEraHeader        xs = OneEraHeader        { getOneEraHeader        :: NS Header            xs }
newtype OneEraTipInfo       xs = OneEraTipInfo       { getOneEraTipInfo       :: NS WrapTipInfo       xs }
newtype OneEraEnvelopeErr   xs = OneEraEnvelopeErr   { getOneEraEnvelopeErr   :: NS WrapEnvelopeErr   xs }
newtype OneEraValidationErr xs = OneEraValidationErr { getOneEraValidationErr :: NS WrapValidationErr xs }
newtype OneEraLedgerError   xs = OneEraLedgerError   { getOneEraLedgerError   :: NS WrapLedgerErr     xs }
newtype OneEraValidateView  xs = OneEraValidateView  { getOneEraValidateView  :: NS WrapValidateView  xs }
newtype OneEraSelectView    xs = OneEraSelectView    { getOneEraSelectView    :: NS WrapSelectView    xs }
newtype OneEraIsLeader      xs = OneEraIsLeader      { getOneEraIsLeader      :: NS WrapIsLeader      xs }
newtype OneEraGenTx         xs = OneEraGenTx         { getOneEraGenTx         :: NS GenTx             xs }
newtype OneEraGenTxId       xs = OneEraGenTxId       { getOneEraGenTxId       :: NS WrapGenTxId       xs }
newtype OneEraApplyTxErr    xs = OneEraApplyTxErr    { getOneEraApplyTxErr    :: NS WrapApplyTxErr    xs }

{-------------------------------------------------------------------------------
  Hash
-------------------------------------------------------------------------------}

-- | The hash for an era
--
-- This type is special: we don't use an NS here, because the hash by itself
-- should not allow us to differentiate between eras. If it did, the /size/
-- of the hash would necessarily have to increase, and that leads to trouble.
-- So, the type parameter @xs@ here is merely a phantom one, and we just store
-- the underlying raw hash.
newtype OneEraHash (xs :: [k]) = OneEraHash { getOneEraHash :: Strict.ByteString }
  deriving newtype (Eq, Ord, Show, NoUnexpectedThunks, Serialise)

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

getSameValue
  :: forall xs a. (IsNonEmpty xs, Eq a, SListI xs, HasCallStack)
  => NP (K a) xs
  -> a
getSameValue values =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty _ ->
        assertWithMsg allEqualCheck (unK (hd values))
  where
    allEqualCheck :: Either String ()
    allEqualCheck
        | allEqual (hcollapse values)
        = return ()
        | otherwise
        = throwError "differing values across hard fork"

{-------------------------------------------------------------------------------
  HasHeader instance for OneEraHeader
-------------------------------------------------------------------------------}

type instance HeaderHash (OneEraHeader xs) = OneEraHash xs

instance CanHardFork xs => StandardHash (OneEraHeader xs)

instance CanHardFork xs => Measured BlockMeasure (OneEraHeader xs) where
  measure = blockMeasure

instance CanHardFork xs => HasHeader (OneEraHeader xs) where
  blockHash     = hcollapse
                . hcmap proxySingle (K . getOneHash)
                . getOneEraHeader
   where
     getOneHash :: forall blk. SingleEraBlock blk
                => Header blk -> OneEraHash xs
     getOneHash = OneEraHash . toRawHash (Proxy @blk) . blockHash

  blockPrevHash = hcollapse
                . hcmap proxySingle (K . getOnePrev)
                . getOneEraHeader
    where
      getOnePrev :: forall blk. SingleEraBlock blk
                 => Header blk -> ChainHash (OneEraHeader xs)
      getOnePrev hdr =
          case blockPrevHash hdr of
            GenesisHash -> GenesisHash
            BlockHash h -> BlockHash (OneEraHash $ toRawHash (Proxy @blk) h)

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
  NoUnexpectedThunks instances
-------------------------------------------------------------------------------}

deriving via LiftNamedNP "PerEraBlockConfig" BlockConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraBlockConfig xs)

deriving via LiftNamedNP "PerEraConsensusConfig" WrapPartialConsensusConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraConsensusConfig xs)

deriving via LiftNamedNP "PerEraChainSelConfig" WrapChainSelConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraChainSelConfig xs)

deriving via LiftNamedNP "PerEraLedgerConfig" WrapPartialLedgerConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraLedgerConfig xs)

deriving via LiftNamedNP "PerEraForgeState" WrapForgeState xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraForgeState xs)

deriving via LiftNamedNS "OneEraHeader" Header xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraHeader xs)

deriving via LiftNamedNS "OneEraEnvelopeErr" WrapEnvelopeErr xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraEnvelopeErr xs)

deriving via LiftNamedNS "OneEraTipInfo" WrapTipInfo xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraTipInfo xs)

deriving via LiftNamedNS "OneEraGenTxId" WrapGenTxId xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraGenTxId xs)

deriving via LiftNamedNS "OneEraLedgerError" WrapLedgerErr xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraLedgerError xs)

deriving via LiftNamedNS "OneEraValidationErr" WrapValidationErr xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraValidationErr xs)

deriving via LiftNamedNS "OneEraGenTx" GenTx xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraGenTx xs)

deriving via LiftNamedMismatch "MismatchEraInfo" SingleEraInfo LedgerEraInfo xs
         instance CanHardFork xs => NoUnexpectedThunks (MismatchEraInfo xs)

{-------------------------------------------------------------------------------
  Other instances
-------------------------------------------------------------------------------}

deriving via LiftNS WrapTipInfo       xs instance CanHardFork xs => Eq   (OneEraTipInfo xs)
deriving via LiftNS WrapTipInfo       xs instance CanHardFork xs => Show (OneEraTipInfo xs)

deriving via LiftNS WrapEnvelopeErr   xs instance CanHardFork xs => Eq   (OneEraEnvelopeErr xs)
deriving via LiftNS WrapEnvelopeErr   xs instance CanHardFork xs => Show (OneEraEnvelopeErr xs)

deriving via LiftNS WrapGenTxId       xs instance CanHardFork xs => Eq   (OneEraGenTxId xs)
deriving via LiftNS WrapGenTxId       xs instance CanHardFork xs => Ord  (OneEraGenTxId xs)

deriving via LiftNS WrapLedgerErr     xs instance CanHardFork xs => Eq   (OneEraLedgerError xs)
deriving via LiftNS WrapLedgerErr     xs instance CanHardFork xs => Show (OneEraLedgerError xs)

deriving via LiftNS WrapValidationErr xs instance CanHardFork xs => Eq   (OneEraValidationErr xs)
deriving via LiftNS WrapValidationErr xs instance CanHardFork xs => Show (OneEraValidationErr xs)

deriving via LiftNP WrapForgeState    xs instance CanHardFork xs => Show (PerEraForgeState xs)

deriving via LiftMismatch SingleEraInfo LedgerEraInfo xs instance CanHardFork xs => Eq   (MismatchEraInfo xs)
deriving via LiftMismatch SingleEraInfo LedgerEraInfo xs instance CanHardFork xs => Show (MismatchEraInfo xs)

{-------------------------------------------------------------------------------
  Show instances used in tests only
-------------------------------------------------------------------------------}

deriving via LiftNS I              xs instance CanHardFork xs => Show (OneEraBlock      xs)
deriving via LiftNS Header         xs instance CanHardFork xs => Show (OneEraHeader     xs)
deriving via LiftNS GenTx          xs instance CanHardFork xs => Show (OneEraGenTx      xs)
deriving via LiftNS WrapGenTxId    xs instance CanHardFork xs => Show (OneEraGenTxId    xs)
deriving via LiftNS WrapApplyTxErr xs instance CanHardFork xs => Show (OneEraApplyTxErr xs)

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
