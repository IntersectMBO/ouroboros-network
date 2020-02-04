{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans            #-}

module Ouroboros.Consensus.Ledger.Shelley.TPraos where

import           BlockChain (BHBody (..), bhbody)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Shelley.Block
import           Ouroboros.Consensus.Ledger.Shelley.Config
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Protocol.TPraos

instance SupportedBlock ShelleyBlock where
  validateView _cfg (ShelleyHeader hdr _) = hdr

{-------------------------------------------------------------------------------
  Support for Praos consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol ShelleyBlock
  = TPraos ShelleyNodeConfig TPraosStandardCrypto

type instance Signed (Header ShelleyBlock) = BHBody TPraosStandardCrypto

instance SignedHeader (Header ShelleyBlock) where
  headerSigned = bhbody . shelleyHeader
