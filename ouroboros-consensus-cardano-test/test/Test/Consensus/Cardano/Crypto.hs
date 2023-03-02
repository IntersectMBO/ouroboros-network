{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}


{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Tests consensus-specific crypto operations in relationship with blocks/headers.
module Test.Consensus.Cardano.Crypto (tests) where


import           Cardano.Crypto.VRF (sizeOutputVRF, sizeCertVRF)
import           Ouroboros.Consensus.Cardano.Block (CardanoHeader,
                     StandardCrypto, pattern HeaderBabbage,
                     pattern HeaderConway)
import           Ouroboros.Consensus.Shelley.Ledger.Block (Header (..))
import           Ouroboros.Consensus.Shelley.Protocol.Abstract
                     (pTieBreakVRFValue)
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Test.Consensus.Cardano.Generators ()
import           Test.Consensus.Cardano.MockCrypto (MockCryptoCompatByron)
import           Test.QuickCheck (Property, property, (===))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup "Cardano Crypto" [
          testProperty "era-dependent VRF" prop_VRFCryptoDependsOnBlockEra
    ]


prop_VRFCryptoDependsOnBlockEra :: CardanoHeader StandardCrypto -> Property
prop_VRFCryptoDependsOnBlockEra h =
  case h of
    HeaderBabbage ShelleyHeader {shelleyHeaderRaw} -> sizeCertVRF (pTieBreakVRFValue shelleyHeaderRaw) === 8
    HeaderConway ShelleyHeader {shelleyHeaderRaw} -> sizeCertVRF (pTieBreakVRFValue shelleyHeaderRaw) === 42
    _ -> property True

  -- 1. generate (forge?) babbage or conway headers
  --   - those should contain different VRF proofs (because they are supposed to use different algorithms)
  --   - the VRF proof should be invalid for the other era
  -- 2. call some header validation functions that's calling VRF certificate check
  -- 3. assert that different VRF function is called for each era
  --    - the header should be valid
  --
  -- why not mock everything? because it does not check that we are implementing things correctly for Conway
  --
  -- we would like to test the dispatchign induced by this type, to make clear Conway relies on different
  -- crypto primitives
  --
  -- type CardanoShelleyEras c =
  --   '[ ShelleyBlock (TPraos c) (ShelleyEra c)
  --    , ShelleyBlock (TPraos c) (AllegraEra c)
  --    , ShelleyBlock (TPraos c) (MaryEra c)
  --    , ShelleyBlock (TPraos c) (AlonzoEra c)
  --    , ShelleyBlock (Praos c)  (BabbageEra c)
  --    , ShelleyBlock (Praos c)  (ConwayEra c)
  --    ]
  --
  -- is it enough to test a single specialised crypto function?
  -- - yes, becausae that's a start! but also because using the high levle HFBlock even with a single function
  --   would be evidence we are doing the dispatching right => we don't test the actual crypto functions, only
  --   the "dispatching" logic that requires different instances for different eras
