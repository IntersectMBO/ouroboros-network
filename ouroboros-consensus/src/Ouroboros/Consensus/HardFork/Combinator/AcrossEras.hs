{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
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
  , PerEraForgeStateInfo(..)
    -- * Value for /one/ era
  , OneEraBlock(..)
  , OneEraHeader(..)
  , OneEraHash(..)
  , OneEraTipInfo(..)
  , OneEraEnvelopeErr(..)
  , OneEraValidationErr(..)
  , OneEraLedgerError(..)
  , OneEraLedgerUpdate(..)
  , OneEraLedgerWarning(..)
  , OneEraValidateView(..)
  , OneEraSelectView(..)
  , OneEraIsLeader(..)
  , OneEraCannotForge(..)
  , OneEraGenTx(..)
  , OneEraGenTxId(..)
  , OneEraApplyTxErr(..)
    -- * Value for two /different/ eras
  , MismatchEraInfo(..)
  , mismatchOneEra
  , mismatchFutureEra
  , EraMismatch(..)
  , mkEraMismatch
    -- * Utility
  , oneEraBlockHeader
  , getSameValue
  ) where

import           Codec.Serialise (Serialise (..))
import           Control.Monad.Except (throwError)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.SOP.Strict hiding (shift)
import           Data.Text (Text)
import           Data.Void
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (allEqual)
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.SOP (OptNP)

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Util.DerivingVia
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match (Mismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

{-------------------------------------------------------------------------------
  Value for /each/ era
-------------------------------------------------------------------------------}

newtype PerEraConsensusConfig xs = PerEraConsensusConfig { getPerEraConsensusConfig :: NP WrapPartialConsensusConfig xs }
newtype PerEraChainSelConfig  xs = PerEraChainSelConfig  { getPerEraChainSelConfig  :: NP WrapChainSelConfig         xs }
newtype PerEraLedgerConfig    xs = PerEraLedgerConfig    { getPerEraLedgerConfig    :: NP WrapPartialLedgerConfig    xs }
newtype PerEraBlockConfig     xs = PerEraBlockConfig     { getPerEraBlockConfig     :: NP BlockConfig                xs }
newtype PerEraCodecConfig     xs = PerEraCodecConfig     { getPerEraCodecConfig     :: NP CodecConfig                xs }

-- | We might not be a leader, but /when/ we need the 'ForgeStateInfo', e.g.,
-- in 'checkIsLeader', then 'ForgeStateInfo' will be non-empty.
newtype PerEraForgeStateInfo xs = PerEraForgeStateInfo { getPerEraForgeStateInfo :: OptNP 'False WrapForgeStateInfo xs }

{-------------------------------------------------------------------------------
  Value for /one/ era
-------------------------------------------------------------------------------}

newtype OneEraBlock         xs = OneEraBlock         { getOneEraBlock         :: NS I                 xs }
newtype OneEraHeader        xs = OneEraHeader        { getOneEraHeader        :: NS Header            xs }
newtype OneEraTipInfo       xs = OneEraTipInfo       { getOneEraTipInfo       :: NS WrapTipInfo       xs }
newtype OneEraEnvelopeErr   xs = OneEraEnvelopeErr   { getOneEraEnvelopeErr   :: NS WrapEnvelopeErr   xs }
newtype OneEraValidationErr xs = OneEraValidationErr { getOneEraValidationErr :: NS WrapValidationErr xs }
newtype OneEraLedgerError   xs = OneEraLedgerError   { getOneEraLedgerError   :: NS WrapLedgerErr     xs }
newtype OneEraLedgerUpdate  xs = OneEraLedgerUpdate  { getOneEraLedgerUpdate  :: NS WrapLedgerUpdate  xs }
newtype OneEraLedgerWarning xs = OneEraLedgerWarning { getOneEraLedgerWarning :: NS WrapLedgerWarning xs }
newtype OneEraValidateView  xs = OneEraValidateView  { getOneEraValidateView  :: NS WrapValidateView  xs }
newtype OneEraSelectView    xs = OneEraSelectView    { getOneEraSelectView    :: NS WrapSelectView    xs }
newtype OneEraIsLeader      xs = OneEraIsLeader      { getOneEraIsLeader      :: NS WrapIsLeader      xs }
newtype OneEraCannotForge   xs = OneEraCannotForge   { getOneEraCannotForge   :: NS WrapCannotForge   xs }
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
newtype OneEraHash (xs :: [k]) = OneEraHash { getOneEraHash :: ShortByteString }
  deriving newtype (Eq, Ord, NoUnexpectedThunks, Serialise)

instance Show (OneEraHash xs) where
  show = BSC.unpack . B16.encode . Short.fromShort . getOneEraHash

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

-- | A mismatch _must_ involve a future era
mismatchFutureEra :: SListI xs
                  => MismatchEraInfo (x ': xs) -> NS SingleEraInfo xs
mismatchFutureEra =
      either id (hmap getLedgerEraInfo)
    . Match.mismatchNotFirst
    . getMismatchEraInfo

{-------------------------------------------------------------------------------
  Untyped version of 'MismatchEraInfo'
-------------------------------------------------------------------------------}

-- | Extra info for errors caused by applying a block, header, transaction, or
-- query from one era to a ledger from a different era.
data EraMismatch = EraMismatch {
      -- | Name of the era of the ledger ("Byron" or "Shelley").
      ledgerEraName :: !Text
      -- | Era of the block, header, transaction, or query.
    , otherEraName  :: !Text
    }
  deriving (Eq, Show, Generic)

-- | When a transaction or block from a certain era was applied to a ledger
-- from another era, we get a 'MismatchEraInfo'.
--
-- Given such a 'MismatchEraInfo', return the name of the era of the
-- transaction/block and the name of the era of the ledger.
mkEraMismatch :: SListI xs => MismatchEraInfo xs -> EraMismatch
mkEraMismatch (MismatchEraInfo mismatch) =
    go mismatch
  where
    go :: SListI xs => Mismatch SingleEraInfo LedgerEraInfo xs -> EraMismatch
    go (Match.ML otherEra ledgerEra) = EraMismatch {
          ledgerEraName = hcollapse $ hmap (K . ledgerName) ledgerEra
        , otherEraName  = otherName otherEra
        }
    go (Match.MR otherEra ledgerEra) = EraMismatch {
          ledgerEraName = ledgerName ledgerEra
        , otherEraName  = hcollapse $ hmap (K . otherName) otherEra
        }
    go (Match.MS m) = go m

    ledgerName :: LedgerEraInfo blk -> Text
    ledgerName = singleEraName . getLedgerEraInfo

    otherName :: SingleEraInfo blk -> Text
    otherName = singleEraName

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
      ProofNonEmpty {} ->
        assertWithMsg allEqualCheck (unK (hd values))
  where
    allEqualCheck :: Either String ()
    allEqualCheck
        | allEqual (hcollapse values)
        = return ()
        | otherwise
        = throwError "differing values across hard fork"

{-------------------------------------------------------------------------------
  NoUnexpectedThunks instances
-------------------------------------------------------------------------------}

deriving via LiftNamedNP "PerEraBlockConfig" BlockConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraBlockConfig xs)

deriving via LiftNamedNP "PerEraCodecConfig" CodecConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraCodecConfig xs)

deriving via LiftNamedNP "PerEraConsensusConfig" WrapPartialConsensusConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraConsensusConfig xs)

deriving via LiftNamedNP "PerEraChainSelConfig" WrapChainSelConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraChainSelConfig xs)

deriving via LiftNamedNP "PerEraLedgerConfig" WrapPartialLedgerConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraLedgerConfig xs)

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

deriving via LiftNP WrapChainSelConfig xs instance CanHardFork xs => Eq (PerEraChainSelConfig xs)
deriving via LiftNS GenTx              xs instance CanHardFork xs => Eq (OneEraGenTx          xs)
deriving via LiftNS WrapApplyTxErr     xs instance CanHardFork xs => Eq (OneEraApplyTxErr     xs)
deriving via LiftNS WrapEnvelopeErr    xs instance CanHardFork xs => Eq (OneEraEnvelopeErr    xs)
deriving via LiftNS WrapGenTxId        xs instance CanHardFork xs => Eq (OneEraGenTxId        xs)
deriving via LiftNS WrapLedgerErr      xs instance CanHardFork xs => Eq (OneEraLedgerError    xs)
deriving via LiftNS WrapLedgerUpdate   xs instance CanHardFork xs => Eq (OneEraLedgerUpdate   xs)
deriving via LiftNS WrapLedgerWarning  xs instance CanHardFork xs => Eq (OneEraLedgerWarning  xs)
deriving via LiftNS WrapTipInfo        xs instance CanHardFork xs => Eq (OneEraTipInfo        xs)
deriving via LiftNS WrapValidationErr  xs instance CanHardFork xs => Eq (OneEraValidationErr  xs)

deriving via LiftNS WrapGenTxId xs instance CanHardFork xs => Ord (OneEraGenTxId xs)

deriving via LiftNP WrapChainSelConfig xs instance CanHardFork xs => Show (PerEraChainSelConfig xs)
deriving via LiftNS WrapEnvelopeErr    xs instance CanHardFork xs => Show (OneEraEnvelopeErr    xs)
deriving via LiftNS WrapLedgerErr      xs instance CanHardFork xs => Show (OneEraLedgerError    xs)
deriving via LiftNS WrapLedgerUpdate   xs instance CanHardFork xs => Show (OneEraLedgerUpdate   xs)
deriving via LiftNS WrapLedgerWarning  xs instance CanHardFork xs => Show (OneEraLedgerWarning  xs)
deriving via LiftNS WrapTipInfo        xs instance CanHardFork xs => Show (OneEraTipInfo        xs)
deriving via LiftNS WrapValidationErr  xs instance CanHardFork xs => Show (OneEraValidationErr  xs)

deriving via LiftOptNP 'False WrapForgeStateInfo xs instance CanHardFork xs => Show (PerEraForgeStateInfo xs)

deriving via LiftMismatch SingleEraInfo LedgerEraInfo xs instance All SingleEraBlock xs => Eq   (MismatchEraInfo xs)
deriving via LiftMismatch SingleEraInfo LedgerEraInfo xs instance All SingleEraBlock xs => Show (MismatchEraInfo xs)

{-------------------------------------------------------------------------------
  Show instances used in tests only
-------------------------------------------------------------------------------}

deriving via LiftNS I               xs instance CanHardFork xs => Show (OneEraBlock       xs)
deriving via LiftNS Header          xs instance CanHardFork xs => Show (OneEraHeader      xs)
deriving via LiftNS GenTx           xs instance CanHardFork xs => Show (OneEraGenTx       xs)
deriving via LiftNS WrapGenTxId     xs instance CanHardFork xs => Show (OneEraGenTxId     xs)
deriving via LiftNS WrapApplyTxErr  xs instance CanHardFork xs => Show (OneEraApplyTxErr  xs)
deriving via LiftNS WrapCannotForge xs instance CanHardFork xs => Show (OneEraCannotForge xs)
deriving via LiftNS WrapSelectView  xs instance CanHardFork xs => Show (OneEraSelectView  xs)
