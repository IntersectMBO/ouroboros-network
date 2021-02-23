{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Cardano.Orphans () where


import qualified Cardano.Binary
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.UTxO
import qualified Cardano.Crypto.Hashing
import qualified Cardano.Crypto.ProtocolMagic
import           Codec.Serialise (Serialise)
-- import qualified Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Util
-- import           Shelley.Spec.Ledger.API
import qualified Shelley.Spec.Ledger.API as SL
import           Shelley.Spec.Ledger.BaseTypes (ActiveSlotCoeff, UnitInterval)

-- deriving anyclass instance Serialise Ouroboros.Consensus.Shelley.Protocol.TPraosParams
-- deriving newtype instance Serialise Ouroboros.Consensus.Shelley.Protocol.MaxMajorProtVer
-- deriving anyclass instance Serialise Nonce
deriving anyclass instance Serialise ActiveSlotCoeff
deriving anyclass instance Serialise SL.Network
deriving anyclass instance Serialise Shelley.Spec.Ledger.BaseTypes.UnitInterval
deriving anyclass instance Serialise CC.Genesis.Config
deriving via (SerialiseViaCanonicalJSON CC.Genesis.GenesisData) instance Serialise CC.Genesis.GenesisData
deriving newtype instance Serialise CC.Genesis.GenesisHash
deriving anyclass instance Serialise (Cardano.Crypto.Hashing.Hash Cardano.Binary.Raw)
deriving anyclass instance Serialise Cardano.Crypto.ProtocolMagic.RequiresNetworkMagic
deriving anyclass instance Serialise CC.CompactAddress
deriving anyclass instance Serialise Cardano.Chain.UTxO.UTxOConfiguration
