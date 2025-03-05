{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.Cardano.Instances where

import Cardano.Network.ConsensusMode (ConsensusMode (..))
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Cardano.Network.Types (LedgerStateJudgement (..))
import Test.Ouroboros.Network.PeerSelection.Instances ()
import Test.QuickCheck


--
-- Arbitrary LedgerStateJudgement instance
--

newtype ArbitraryLedgerStateJudgement =
  ArbitraryLedgerStateJudgement {
    getArbitraryLedgerStateJudgement :: LedgerStateJudgement
  } deriving Show

instance Arbitrary ArbitraryLedgerStateJudgement where
    arbitrary =
      ArbitraryLedgerStateJudgement <$>
        oneof [ pure YoungEnough
              , pure TooOld
              ]
    shrink (ArbitraryLedgerStateJudgement YoungEnough) =
      [ArbitraryLedgerStateJudgement TooOld]
    shrink (ArbitraryLedgerStateJudgement TooOld)      =
      []

--
-- Arbitrary PeerTrustable instance
--

instance Arbitrary PeerTrustable where
  arbitrary = elements [ IsNotTrustable, IsTrustable ]

  shrink IsTrustable    = [ IsNotTrustable ]
  shrink IsNotTrustable = []

--
-- Arbitrary UseBootstrapPeers instance
--

instance Arbitrary UseBootstrapPeers where
  arbitrary = frequency [ (1, pure DontUseBootstrapPeers)
                        , (1, UseBootstrapPeers <$> arbitrary)
                        ]

  shrink DontUseBootstrapPeers = []
  shrink (UseBootstrapPeers bp) | [] <- bp = [DontUseBootstrapPeers]
                                | [_] <- bp = [DontUseBootstrapPeers]
  shrink (UseBootstrapPeers (hd : _)) = [UseBootstrapPeers [hd]]

--
-- Arbitrary ConsensusMode instance
--

instance Arbitrary ConsensusMode where
  arbitrary = elements [PraosMode, GenesisMode]
  shrink GenesisMode = [PraosMode]
  shrink PraosMode   = []

