-- | QuickCheck generators and shrinkers for 'NodeToNodeVersion' and
-- 'NodeToNodeVersionData', including helpers for generating valid and invalid
-- version + data combinations.
module Cardano.Network.NodeToNode.Version.TestUtils
  ( genNodeToNodeVersion
  , shrinkNodeToNodeVersion
  , genNodeToNodeVersionData
  , shrinkNodeToNodeVersionData
  , genValidNtnVersionDataForVersion
  , genInvalidNtnVersionAndDataPair
  , fixNtnVersionDataForVersion
  ) where

import Cardano.Network.NodeToNode.Version
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Test.QuickCheck (Gen, arbitrary, arbitraryBoundedEnum, elements, oneof,
           shrink)


-- | Generator for 'NodeToNodeVersion'.
genNodeToNodeVersion :: Gen NodeToNodeVersion
genNodeToNodeVersion = arbitraryBoundedEnum

-- | Shrinker for 'NodeToNodeVersion'.
shrinkNodeToNodeVersion :: NodeToNodeVersion -> [NodeToNodeVersion]
shrinkNodeToNodeVersion v
  | v == minBound = []
  | otherwise     = [pred v]

-- | Generator for 'NodeToNodeVersionData'.
genNodeToNodeVersionData :: Gen NodeToNodeVersionData
genNodeToNodeVersionData =
      NodeToNodeVersionData
  <$> (NetworkMagic <$> arbitrary)
  <*> oneof [ pure InitiatorOnlyDiffusionMode
            , pure InitiatorAndResponderDiffusionMode
            ]
  <*> elements [ PeerSharingDisabled
               , PeerSharingEnabled
               ]
  <*> arbitrary
  <*> elements [ PerasUnsupported
               , PerasSupported
               ]

-- | Shrinker for 'NodeToNodeVersionData'.
shrinkNodeToNodeVersionData :: NodeToNodeVersionData -> [NodeToNodeVersionData]
shrinkNodeToNodeVersionData (NodeToNodeVersionData magic mode ps qry psup) =
       [ NodeToNodeVersionData magic' mode    ps  qry  psup  | magic'        <- shrinkNetworkMagic magic ]
    ++ [ NodeToNodeVersionData magic  mode'   ps  qry  psup  | mode'         <- shrinkDiffusionMode mode ]
    ++ [ NodeToNodeVersionData magic  mode    ps' qry  psup  | ps'           <- shrinkPeerSharing ps ]
    ++ [ NodeToNodeVersionData magic  mode    ps  qry' psup  | qry'        <- shrink qry ]
    ++ [ NodeToNodeVersionData magic  mode    ps  qry  psup' | psup' <- shrinkPerasSupport psup ]
  where
    shrinkNetworkMagic :: NetworkMagic -> [NetworkMagic]
    shrinkNetworkMagic (NetworkMagic nm) = NetworkMagic <$> shrink nm

    shrinkDiffusionMode :: DiffusionMode -> [DiffusionMode]
    shrinkDiffusionMode InitiatorOnlyDiffusionMode           = []
    shrinkDiffusionMode InitiatorAndResponderDiffusionMode   = [InitiatorOnlyDiffusionMode]

    shrinkPeerSharing :: PeerSharing -> [PeerSharing]
    shrinkPeerSharing PeerSharingDisabled = []
    shrinkPeerSharing PeerSharingEnabled  = [PeerSharingDisabled]

    shrinkPerasSupport :: PerasSupport -> [PerasSupport]
    shrinkPerasSupport PerasUnsupported = []
    shrinkPerasSupport PerasSupported   = [PerasUnsupported]

-- | Generate valid 'NodeToNodeVersionData' for a given version.
-- For versions before 'NodeToNodeV_16', 'perasSupport' is set to 'PerasUnsupported'.
genValidNtnVersionDataForVersion :: NodeToNodeVersion -> Gen NodeToNodeVersionData
genValidNtnVersionDataForVersion version =
  fixNtnVersionDataForVersion version <$> genNodeToNodeVersionData

-- | For versions before 'NodeToNodeV_16', set 'perasSupport' to 'PerasUnsupported'
-- to ensure the data is valid for the version.
fixNtnVersionDataForVersion :: NodeToNodeVersion -> NodeToNodeVersionData -> NodeToNodeVersionData
fixNtnVersionDataForVersion version ntnData =
  if version < NodeToNodeV_16
    then ntnData { perasSupport = PerasUnsupported }
    else ntnData

-- | Generate an invalid (version, data) pair.
--
-- So far only Peras support can generate an invalid case, so this function
-- returns a pair where 'PerasSupported' is used in the data with a version
-- that doesn't support it.
genInvalidNtnVersionAndDataPair :: Gen (NodeToNodeVersion, NodeToNodeVersionData)
genInvalidNtnVersionAndDataPair = do
  rawNtnData <- genNodeToNodeVersionData
  v <- elements [minBound .. pred NodeToNodeV_16]
  pure (v, rawNtnData { perasSupport = PerasSupported })
