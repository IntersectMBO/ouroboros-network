{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Protocol.PBFT.Crypto (
    PBftCrypto (..)
  , PBftMockCrypto
  , PBftMockVerKeyHash (..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.Kind (Type)
import           Data.Typeable
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN, VerKeyDSIGN (..))

import           Ouroboros.Consensus.Util.Condense

-- | Crypto primitives required by BFT
--
-- Cardano stores a map of stakeholder IDs rather than the verification key
-- directly. We make this family injective for convenience - whilst it's
-- _possible_ that there could be non-injective instances, the chances of there
-- being more than the two instances here are basically non-existent.
class ( Typeable c
      , DSIGNAlgorithm (PBftDSIGN c)
      , Condense (SigDSIGN (PBftDSIGN c))
      , Show (PBftVerKeyHash c)
      , Ord  (PBftVerKeyHash c)
      , Eq   (PBftVerKeyHash c)
      , Show (PBftVerKeyHash c)
      , NoThunks (PBftVerKeyHash c)
      , NoThunks (PBftDelegationCert c)
      ) => PBftCrypto c where
  type family PBftDSIGN          c :: Type
  type family PBftDelegationCert c = (d :: Type) | d -> c
  type family PBftVerKeyHash     c = (d :: Type) | d -> c

  dlgCertGenVerKey :: PBftDelegationCert c -> VerKeyDSIGN (PBftDSIGN c)
  dlgCertDlgVerKey :: PBftDelegationCert c -> VerKeyDSIGN (PBftDSIGN c)
  hashVerKey       :: VerKeyDSIGN (PBftDSIGN c) -> PBftVerKeyHash c


data PBftMockCrypto

instance PBftCrypto PBftMockCrypto where
  type PBftDSIGN          PBftMockCrypto = MockDSIGN
  type PBftDelegationCert PBftMockCrypto = (VerKeyDSIGN MockDSIGN, VerKeyDSIGN MockDSIGN)
  type PBftVerKeyHash     PBftMockCrypto = PBftMockVerKeyHash

  dlgCertGenVerKey = fst
  dlgCertDlgVerKey = snd
  hashVerKey       = PBftMockVerKeyHash

-- | We don't hash and just use the underlying 'Word64'.
newtype PBftMockVerKeyHash = PBftMockVerKeyHash {
      getPBftMockVerKeyHash :: VerKeyDSIGN MockDSIGN
    }
  deriving (Eq, Show, Generic, NoThunks)
  deriving (Serialise, Ord) via Word64
