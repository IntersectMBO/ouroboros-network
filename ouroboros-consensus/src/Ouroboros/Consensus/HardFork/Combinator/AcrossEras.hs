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
    PerEraBlockConfig (..)
  , PerEraCodecConfig (..)
  , PerEraConsensusConfig (..)
  , PerEraLedgerConfig (..)
  , PerEraStorageConfig (..)
    -- * Values for /some/ eras
  , SomeErasCanBeLeader (..)
    -- * Value for /one/ era
  , OneEraApplyTxErr (..)
  , OneEraBlock (..)
  , OneEraCannotForge (..)
  , OneEraEnvelopeErr (..)
  , OneEraForgeStateInfo (..)
  , OneEraForgeStateUpdateError (..)
  , OneEraGenTx (..)
  , OneEraGenTxId (..)
  , OneEraHash (..)
  , OneEraHeader (..)
  , OneEraIsLeader (..)
  , OneEraLedgerError (..)
  , OneEraLedgerEvent (..)
  , OneEraLedgerUpdate (..)
  , OneEraLedgerWarning (..)
  , OneEraSelectView (..)
  , OneEraTipInfo (..)
  , OneEraValidateView (..)
  , OneEraValidatedGenTx (..)
  , OneEraValidationErr (..)
    -- * Value for two /different/ eras
  , EraMismatch (..)
  , MismatchEraInfo (..)
  , mismatchFutureEra
  , mismatchOneEra
  , mkEraMismatch
    -- * Utility
  , getSameValue
  , oneEraBlockHeader
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
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (allEqual)
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.OptNP (OptNP)

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Util.DerivingVia
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match (Mismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

{-------------------------------------------------------------------------------
  Value for /each/ era
-------------------------------------------------------------------------------}

newtype PerEraBlockConfig     xs = PerEraBlockConfig     { getPerEraBlockConfig     :: NP BlockConfig                xs }
newtype PerEraCodecConfig     xs = PerEraCodecConfig     { getPerEraCodecConfig     :: NP CodecConfig                xs }
newtype PerEraConsensusConfig xs = PerEraConsensusConfig { getPerEraConsensusConfig :: NP WrapPartialConsensusConfig xs }
newtype PerEraLedgerConfig    xs = PerEraLedgerConfig    { getPerEraLedgerConfig    :: NP WrapPartialLedgerConfig    xs }
newtype PerEraStorageConfig   xs = PerEraStorageConfig   { getPerEraStorageConfig   :: NP StorageConfig              xs }

{-------------------------------------------------------------------------------
  Values for /some/ eras

  The reason for using @OptNP 'False f xs@ as opposed to @NP (Maybe :.: f) xs@
  is to maintain the isomorphism between @blk@ and @HardForkBlock '[blk]@ in
  "Ouroboros.Consensus.HardFork.Combinator.Embed.Unary"
-------------------------------------------------------------------------------}

newtype SomeErasCanBeLeader xs = SomeErasCanBeLeader { getSomeErasCanBeLeader :: OptNP 'False WrapCanBeLeader xs }

{-------------------------------------------------------------------------------
  Value for /one/ era
-------------------------------------------------------------------------------}

newtype OneEraApplyTxErr            xs = OneEraApplyTxErr            { getOneEraApplyTxErr            :: NS WrapApplyTxErr            xs }
newtype OneEraBlock                 xs = OneEraBlock                 { getOneEraBlock                 :: NS I                         xs }
newtype OneEraCannotForge           xs = OneEraCannotForge           { getOneEraCannotForge           :: NS WrapCannotForge           xs }
newtype OneEraEnvelopeErr           xs = OneEraEnvelopeErr           { getOneEraEnvelopeErr           :: NS WrapEnvelopeErr           xs }
newtype OneEraForgeStateInfo        xs = OneEraForgeStateInfo        { getOneEraForgeStateInfo        :: NS WrapForgeStateInfo        xs }
newtype OneEraForgeStateUpdateError xs = OneEraForgeStateUpdateError { getOneEraForgeStateUpdateError :: NS WrapForgeStateUpdateError xs }
newtype OneEraGenTx                 xs = OneEraGenTx                 { getOneEraGenTx                 :: NS GenTx                     xs }
newtype OneEraGenTxId               xs = OneEraGenTxId               { getOneEraGenTxId               :: NS WrapGenTxId               xs }
newtype OneEraHeader                xs = OneEraHeader                { getOneEraHeader                :: NS Header                    xs }
newtype OneEraIsLeader              xs = OneEraIsLeader              { getOneEraIsLeader              :: NS WrapIsLeader              xs }
newtype OneEraLedgerError           xs = OneEraLedgerError           { getOneEraLedgerError           :: NS WrapLedgerErr             xs }
newtype OneEraLedgerEvent           xs = OneEraLedgerEvent           { getOneEraLedgerEvent           :: NS WrapLedgerEvent           xs }
newtype OneEraLedgerUpdate          xs = OneEraLedgerUpdate          { getOneEraLedgerUpdate          :: NS WrapLedgerUpdate          xs }
newtype OneEraLedgerWarning         xs = OneEraLedgerWarning         { getOneEraLedgerWarning         :: NS WrapLedgerWarning         xs }
newtype OneEraSelectView            xs = OneEraSelectView            { getOneEraSelectView            :: NS WrapSelectView            xs }
newtype OneEraTipInfo               xs = OneEraTipInfo               { getOneEraTipInfo               :: NS WrapTipInfo               xs }
newtype OneEraValidateView          xs = OneEraValidateView          { getOneEraValidateView          :: NS WrapValidateView          xs }
newtype OneEraValidatedGenTx        xs = OneEraValidatedGenTx        { getOneEraValidatedGenTx        :: NS WrapValidatedGenTx        xs }
newtype OneEraValidationErr         xs = OneEraValidationErr         { getOneEraValidationErr         :: NS WrapValidationErr         xs }

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
  deriving newtype (Eq, Ord, NoThunks, Serialise)

instance Show (OneEraHash xs) where
  show = BSC.unpack . B16.encode . Short.fromShort . getOneEraHash

instance Condense (OneEraHash xs) where
  condense = show

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
  NoThunks instances
-------------------------------------------------------------------------------}

deriving via LiftNamedNP "PerEraBlockConfig" BlockConfig xs
         instance CanHardFork xs => NoThunks (PerEraBlockConfig xs)

deriving via LiftNamedNP "PerEraCodecConfig" CodecConfig xs
         instance CanHardFork xs => NoThunks (PerEraCodecConfig xs)

deriving via LiftNamedNP "PerEraConsensusConfig" WrapPartialConsensusConfig xs
         instance CanHardFork xs => NoThunks (PerEraConsensusConfig xs)

deriving via LiftNamedNP "PerEraLedgerConfig" WrapPartialLedgerConfig xs
         instance CanHardFork xs => NoThunks (PerEraLedgerConfig xs)

deriving via LiftNamedNP "PerEraStorageConfig" StorageConfig xs
         instance CanHardFork xs => NoThunks (PerEraStorageConfig xs)

deriving via LiftNamedNS "OneEraEnvelopeErr" WrapEnvelopeErr xs
         instance CanHardFork xs => NoThunks (OneEraEnvelopeErr xs)

deriving via LiftNamedNS "OneEraGenTx" GenTx xs
         instance CanHardFork xs => NoThunks (OneEraGenTx xs)

deriving via LiftNamedNS "OneEraGenTxId" WrapGenTxId xs
         instance CanHardFork xs => NoThunks (OneEraGenTxId xs)

deriving via LiftNamedNS "OneEraHeader" Header xs
         instance CanHardFork xs => NoThunks (OneEraHeader xs)

deriving via LiftNamedNS "OneEraLedgerError" WrapLedgerErr xs
         instance CanHardFork xs => NoThunks (OneEraLedgerError xs)

deriving via LiftNamedNS "OneEraTipInfo" WrapTipInfo xs
         instance CanHardFork xs => NoThunks (OneEraTipInfo xs)

deriving via LiftNamedNS "OneEraValidated" WrapValidatedGenTx xs
         instance CanHardFork xs => NoThunks (OneEraValidatedGenTx xs)

deriving via LiftNamedNS "OneEraValidationErr" WrapValidationErr xs
         instance CanHardFork xs => NoThunks (OneEraValidationErr xs)

deriving via LiftNamedMismatch "MismatchEraInfo" SingleEraInfo LedgerEraInfo xs
         instance CanHardFork xs => NoThunks (MismatchEraInfo xs)

{-------------------------------------------------------------------------------
  Other instances
-------------------------------------------------------------------------------}

deriving via LiftNS WrapApplyTxErr     xs instance CanHardFork xs => Eq (OneEraApplyTxErr     xs)
deriving via LiftNS WrapEnvelopeErr    xs instance CanHardFork xs => Eq (OneEraEnvelopeErr    xs)
deriving via LiftNS GenTx              xs instance CanHardFork xs => Eq (OneEraGenTx          xs)
deriving via LiftNS WrapGenTxId        xs instance CanHardFork xs => Eq (OneEraGenTxId        xs)
deriving via LiftNS WrapLedgerErr      xs instance CanHardFork xs => Eq (OneEraLedgerError    xs)
deriving via LiftNS WrapLedgerUpdate   xs instance CanHardFork xs => Eq (OneEraLedgerUpdate   xs)
deriving via LiftNS WrapLedgerWarning  xs instance CanHardFork xs => Eq (OneEraLedgerWarning  xs)
deriving via LiftNS WrapSelectView     xs instance CanHardFork xs => Eq (OneEraSelectView     xs)
deriving via LiftNS WrapTipInfo        xs instance CanHardFork xs => Eq (OneEraTipInfo        xs)
deriving via LiftNS WrapValidatedGenTx xs instance CanHardFork xs => Eq (OneEraValidatedGenTx xs)
deriving via LiftNS WrapValidationErr  xs instance CanHardFork xs => Eq (OneEraValidationErr  xs)

deriving via LiftNS WrapGenTxId xs instance CanHardFork xs => Ord (OneEraGenTxId xs)

deriving via LiftNS WrapEnvelopeErr           xs instance CanHardFork xs => Show (OneEraEnvelopeErr           xs)
deriving via LiftNS WrapForgeStateInfo        xs instance CanHardFork xs => Show (OneEraForgeStateInfo        xs)
deriving via LiftNS WrapForgeStateUpdateError xs instance CanHardFork xs => Show (OneEraForgeStateUpdateError xs)
deriving via LiftNS WrapLedgerErr             xs instance CanHardFork xs => Show (OneEraLedgerError           xs)
deriving via LiftNS WrapLedgerUpdate          xs instance CanHardFork xs => Show (OneEraLedgerUpdate          xs)
deriving via LiftNS WrapLedgerWarning         xs instance CanHardFork xs => Show (OneEraLedgerWarning         xs)
deriving via LiftNS WrapTipInfo               xs instance CanHardFork xs => Show (OneEraTipInfo               xs)
deriving via LiftNS WrapValidatedGenTx        xs instance CanHardFork xs => Show (OneEraValidatedGenTx        xs)
deriving via LiftNS WrapValidationErr         xs instance CanHardFork xs => Show (OneEraValidationErr         xs)

deriving via LiftMismatch SingleEraInfo LedgerEraInfo xs instance All SingleEraBlock xs => Eq   (MismatchEraInfo xs)
deriving via LiftMismatch SingleEraInfo LedgerEraInfo xs instance All SingleEraBlock xs => Show (MismatchEraInfo xs)

{-------------------------------------------------------------------------------
  Show instances used in tests only
-------------------------------------------------------------------------------}

deriving via LiftNS WrapApplyTxErr  xs instance CanHardFork xs => Show (OneEraApplyTxErr  xs)
deriving via LiftNS I               xs instance CanHardFork xs => Show (OneEraBlock       xs)
deriving via LiftNS WrapCannotForge xs instance CanHardFork xs => Show (OneEraCannotForge xs)
deriving via LiftNS GenTx           xs instance CanHardFork xs => Show (OneEraGenTx       xs)
deriving via LiftNS WrapGenTxId     xs instance CanHardFork xs => Show (OneEraGenTxId     xs)
deriving via LiftNS Header          xs instance CanHardFork xs => Show (OneEraHeader      xs)
deriving via LiftNS WrapSelectView  xs instance CanHardFork xs => Show (OneEraSelectView  xs)
