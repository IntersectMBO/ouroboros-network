{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.Cardano.Instances where

import Cardano.Node.ArgumentsExtra (ConsensusModePeerTargets (..))
import Cardano.Node.ConsensusMode (ConsensusMode (..))
import Cardano.Node.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Cardano.Node.PeerSelection.PeerTrustable (PeerTrustable (..))
import Cardano.Node.Types (LedgerStateJudgement (..))
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
-- Arbitrary ConsensusModePeerTargets instance
--

-- GovernorMockEnvironment is responsible for generating valid targets
-- which account for local roots from random peer graph, but a shrink
-- is useful here for recursively shrinking TimedScript.
--
instance Arbitrary ConsensusModePeerTargets where
  arbitrary = error "not implemented"

  shrink ConsensusModePeerTargets { deadlineTargets, syncTargets } =
    let syncTargets'     = shrink syncTargets
        deadlineTargets' = shrink deadlineTargets
    in [ConsensusModePeerTargets { deadlineTargets = deadlineTargets'', syncTargets = syncTargets'' }
       | deadlineTargets'' <- deadlineTargets',
         syncTargets'' <- syncTargets']

--
-- Arbitrary PeerTrustable instance
--

instance Arbitrary PeerTrustable where
  arbitrary = elements [ IsNotTrustable, IsTrustable ]

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

