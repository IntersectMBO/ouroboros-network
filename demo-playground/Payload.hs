{-# LANGUAGE DataKinds #-}

module Payload (
      Payload
    , fixupBlock
    , chainFrom
    ) where

import           Block
import           MockPayload
import           Ouroboros

type Payload = Block 'MockLedgerDomain 'OuroborosBFT
