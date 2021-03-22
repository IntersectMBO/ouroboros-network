{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.ThreadNet.Infra.Byron.TrackUpdates (
    ProtocolVersionUpdateLabel (..)
  , SoftwareVersionUpdateLabel (..)
  , mkProtocolByronAndHardForkTxs
  , mkUpdateLabels
  ) where

import           Control.Exception (assert)
import           Control.Monad (guard)
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import qualified Cardano.Binary
import qualified Cardano.Chain.Block as Block
import qualified Cardano.Chain.Byron.API as ByronAPI
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.MempoolPayload as MempoolPayload
import           Cardano.Chain.Slotting (EpochSlots (..), SlotNumber (..))
import qualified Cardano.Chain.Update as Update
import           Cardano.Chain.Update.Proposal (AProposal)
import qualified Cardano.Chain.Update.Proposal as Proposal
import qualified Cardano.Chain.Update.Validation.Interface as Update
import qualified Cardano.Chain.Update.Validation.Registration as Registration
import           Cardano.Chain.Update.Vote (AVote)
import qualified Cardano.Chain.Update.Vote as Vote
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..),
                     ProtocolInfo (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.PBFT

import qualified Ouroboros.Consensus.Byron.Crypto.DSIGN as Crypto
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron

import           Test.ThreadNet.Network (TestNodeInitialization (..))
import qualified Test.ThreadNet.Ref.PBFT as Ref
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeTopology

import           Test.ThreadNet.Infra.Byron.ProtocolInfo

import           Test.Util.Slots (NumSlots (..))

-- | The expectation and observation regarding whether the hard-fork proposal
-- successfully updated the protocol version
--
data ProtocolVersionUpdateLabel = ProtocolVersionUpdateLabel
  { pvuObserved :: Bool
    -- ^ whether the proposed protocol version is adopted or not adopted by the
    -- end of the test
  , pvuRequired :: Maybe Bool
    -- ^ @Just b@ indicates whether the final chains must have adopted or must
    -- have not adopted the proposed protocol version. @Nothing@ means there is
    -- no requirement.
  }
  deriving (Show)

-- | As 'ProtocolVersionUpdateLabel', but for software version updates
--
-- Note that software version updates are adopted sooner than and perhaps
-- independently of protocol version updates, even when they are introduced by
-- the same proposal transaction.
--
data SoftwareVersionUpdateLabel = SoftwareVersionUpdateLabel
  { svuObserved :: Bool
  , svuRequired :: Maybe Bool
  }
  deriving (Show)

-- | Classify the a @QuickCheck@ test's input and output with respect to
-- whether the protocol\/software version should have been\/was updated
--
mkUpdateLabels
  :: PBftParams
  -> NumSlots
  -> Genesis.Config
  -> NodeJoinPlan
  -> NodeTopology
  -> Ref.Result
  -> Byron.LedgerState ByronBlock
     -- ^ from 'nodeOutputFinalLedger'
  -> (ProtocolVersionUpdateLabel, SoftwareVersionUpdateLabel)
mkUpdateLabels params numSlots genesisConfig nodeJoinPlan topology result
  ldgr =
    (pvuLabel, svuLabel)
  where
    PBftParams{pbftNumNodes, pbftSecurityParam} = params

    -- the slot immediately after the end of the simulation
    sentinel :: SlotNumber
    sentinel = SlotNumber t
      where
        NumSlots t = numSlots

    -- a block forged in slot @s@ becomes immutable/stable in slot @s + twoK@
    -- according to the Byron Chain Density invariant
    twoK :: SlotNo
    twoK = SlotNo $ 2 * maxRollbacks pbftSecurityParam

    -- the number of slots in an epoch
    epochSlots :: SlotNo
    epochSlots = coerce $ Genesis.configEpochSlots genesisConfig

    -- the protocol parameters
    --
    -- ASSUMPTION: These do not change during the test.
    pp0 :: Update.ProtocolParameters
    pp0 = Genesis.configProtocolParameters genesisConfig

    -- how many votes/endorsements the proposal needs to gain
    quorum :: Word64
    quorum =
      (\x -> assert (x > 0) x) $
      fromIntegral $ Update.upAdptThd (fromIntegral n) pp0
      where
        NumCoreNodes n = pbftNumNodes

    -- how many slots the proposal has to gain sufficient votes before it
    -- expires
    ttl :: SlotNo
    ttl = coerce $ Update.ppUpdateProposalTTL pp0

    -- the first slot of the epoch after the epoch containing the given slot
    ebbSlotAfter :: SlotNo -> SlotNo
    ebbSlotAfter (SlotNo s) =
        SlotNo (denom * div s denom) + epochSlots
      where
        SlotNo denom = epochSlots

    finalState :: [Ref.Outcome] -> ProposalState
    finalState outcomes = go Proposing (SlotNo 0) outcomes

    -- compute the @Just@ case of 'pvuRequired' from the simulated outcomes
    go
      :: ProposalState
         -- ^ the state before the next outcome
      -> SlotNo
         -- ^ the slot described by the next outcome
      -> [Ref.Outcome]
      -> ProposalState
    go !st !s = \case
      []   -> assert (coerce sentinel == s) st
      o:os -> case o of
          Ref.Absent  -> continueWith st
          Ref.Unable  -> continueWith st
          Ref.Wasted  -> continueWith st
          Ref.Nominal -> case st of
              -- the proposal is in this slot
              Proposing                    ->
                  let -- if this leader just joined, it will forge before the
                      -- proposal reaches its mempool, unless it's node 0
                      lostRace = s == leaderJoinSlot &&
                                 leader /= CoreNodeId 0
                  in
                  if lostRace then continueWith st else
                  -- votes can be valid immediately and at least one should
                  -- also be in this block
                  go (Voting s Set.empty) s (o:os)
              Voting proposalSlot votes    ->
                  let votesInTheNewBlock =
                          -- an exception to the rule: the proposal and c0's
                          -- own vote always has time to reach its mempool
                          (if leader == c0 then Set.insert c0 else id) $
                          -- if the leader is joining in this slot, then no
                          -- votes will reach its mempool before it forges:
                          -- other nodes' votes will be delayed via
                          -- communication and its own vote is not valid
                          -- because it will forge before its ledger/mempool
                          -- contains the proposal
                          if s == leaderJoinSlot then Set.empty else
                          -- only votes from nodes that joined prior to this
                          -- slot can reach the leader's mempool before it
                          -- forges
                          Map.keysSet $ Map.filter (< s) m
                        where
                          NodeJoinPlan m = nodeJoinPlan
                          c0 = CoreNodeId 0

                      votes'    = Set.union votesInTheNewBlock votes
                      confirmed = fromIntegral (Set.size votes') >= quorum
                      expired   = proposalSlot + ttl < s
                  in
                  if  -- TODO cardano-ledger-byron checks for quorum before it checks
                      -- for expiry, so we do mimick that here. But is that
                      -- correct?
                    | confirmed -> continueWith $ Endorsing s Set.empty
                      -- c0 will re-propose the same proposal again at the next
                      -- opportunity
                    | expired   -> continueWith $ Proposing
                    | otherwise -> continueWith $ Voting proposalSlot votes'
              Endorsing finalVoteSlot ends ->
                  continueWith $
                  if s < finalVoteSlot + twoK
                  then st  -- ignore endorsements until final vote is stable
                  else
                    let ends' = Set.insert (Ref.mkLeaderOf params s) ends
                    in
                    if fromIntegral (Set.size ends) < quorum
                    then Endorsing finalVoteSlot ends'
                    else Adopting s   -- enough endorsements
              Adopting{}                   -> continueWith st
        where
          leader         = Ref.mkLeaderOf params s
          leaderJoinSlot = coreNodeIdJoinSlot nodeJoinPlan leader

          continueWith st' = go st' (succ s) os

    pvuLabel = ProtocolVersionUpdateLabel
        { pvuObserved =
            (== theProposedProtocolVersion) $
            Update.adoptedProtocolVersion $
            Block.cvsUpdateState $
            -- tick the chain over into the slot after the final simulated slot
            ByronAPI.applyChainTick genesisConfig sentinel $
            Byron.byronLedgerState ldgr
        , pvuRequired = case result of
            -- 'Ref.Forked' means there's only 1-block chains, and that's not enough
            -- for a proposal to succeed
            Ref.Forked{}           -> Just False
            -- we wouldn't necessarily be able to anticipate when the last
            -- endorsement happens, so give up
            Ref.Nondeterministic{} -> Nothing
            Ref.Outcomes outcomes  -> do
                checkTopo params topology
                Just $ case finalState outcomes of
                  Proposing{}                   -> False
                  Voting{}                      -> False
                  Endorsing{}                   -> False
                  Adopting finalEndorsementSlot ->
                      ebbSlotAfter (finalEndorsementSlot + twoK) <= s
                    where
                      s = coerce sentinel
        }

    svuLabel = SoftwareVersionUpdateLabel
        { svuObserved = fromMaybe False $ do
            let nm = Update.svAppName theProposedSoftwareVersion
            (Registration.ApplicationVersion vn _slot _metadata) <- Map.lookup nm $
              Update.appVersions $
              Block.cvsUpdateState $
              -- unlike for protocol version updates, there is no need to tick
              -- since the passage of time isn't a prerequisite
              Byron.byronLedgerState ldgr
            pure $ vn == Update.svNumber theProposedSoftwareVersion
        , svuRequired = case result of
            -- 'Ref.Forked' means all blocks except perhaps the first were
            -- forged in the slot in which the forging node joined, which means
            -- nodes other than c0 never forged after receiving the proposal. A
            -- block forged by node c0 will have proposed and might have
            -- confirmed it (depending on quorum), but the other nodes will not
            -- have. This is very much a corner case, so we ignore it.
            Ref.Forked{}           -> Nothing
            -- We wouldn't necessarily be able to anticipate if the proposal is
            -- confirmed or even in all of the final chains, so we ignore it.
            Ref.Nondeterministic{} -> Nothing
            Ref.Outcomes outcomes  -> do
                checkTopo params topology
                Just $ case finalState outcomes of
                  Proposing{} -> False
                  Voting{}    -> False
                  Endorsing{} -> True
                  Adopting{}  -> True
        }

-- if the topology is not mesh, then some assumptions in 'finalState' about
-- races maybe be wrong
--
-- In particular, if the proposal is already on the chain, and the leader of
-- the next slot, call it node L, is joining and is only directly connected to
-- other nodes that are also joining, then those other node's votes will not
-- reach L's mempool before it forges in the next slot. In fact, those votes
-- will arrive to node L via TxSub during the current slot but /before/ the
-- block containing the proposal does, so node L's mempool will reject the
-- votes as invalid. The votes are not resent (at least not before node L
-- leads).
checkTopo :: PBftParams -> NodeTopology -> Maybe ()
checkTopo params topology = do
    let PBftParams{pbftNumNodes} = params
    guard $ topology == meshNodeTopology pbftNumNodes

-- | The state of a proposal within a linear timeline
--
data ProposalState =
    Proposing
    -- ^ submitting the proposal (possibly not for the first time, if it has
    -- previously expired)
  | Voting !SlotNo !(Set CoreNodeId)
    -- ^ accumulating sufficient votes
    --
    -- The slot is when the proposal was submitted; it might expire during
    -- voting. The set is who has voted.
  | Endorsing !SlotNo !(Set CoreNodeId)
    -- ^ accumulating sufficient endorsements
    --
    -- The slot is when the first sufficient vote was submitted. The set is the
    -- endorsements seen so far.
  | Adopting !SlotNo
    -- ^ waiting for epoch transition
    --
    -- The slot is when the first sufficient endorsement was submitted.
  deriving (Show)

{-------------------------------------------------------------------------------
  ProtocolVersion update proposals
-------------------------------------------------------------------------------}

-- | The protocol info for a node as well as some initial transactions
--
-- The transactions implement a smoke test for the hard-fork from Byron to
-- Shelley. See PR #1741 for details on how that hard-fork will work. The key
-- fact is that last thing the nodes will ever do while running the Byron
-- protocol is adopt a specific (but as of yet to-be-determined) protocol
-- version. So this smoke test ensures that the nodes can in fact adopt a new
-- protocol version.
--
-- Adopting a new protocol version requires four kinds of event in Byron.
-- Again, see PR #1741 for more details.
--
--  * Proposal transaction. A protocol parameter update proposal transaction
--    makes it onto the chain (it doesn't have to actually change any
--    parameters, just increase the protocol version). Proposals are
--    'MempoolPayload.MempoolUpdateProposal' transactions; one is included in
--    the return value of this function. In the smoke test, we immediately and
--    repeatedly throughout the test add the proposal to @CoreNodeId 0@'s
--    mempool; this seems realistic.
--
--  * Vote transactions. A sufficient number of nodes (@floor (0.6 *
--    'pbftNumNodes')@ as of this writing) must vote for the proposal. Votes
--    are 'MempoolPayload.MempoolUpdateVote' transactions; one per node is
--    included in the return value of this function. In the smoke test, we
--    immediately and repeatedly throughout the test add each node's vote to
--    its own mempool; this seems realistic.
--
--  * Endorsement header field. After enough votes are 2k slots old, a
--    sufficient number of nodes (@floor (0.6 * 'pbftNumNodes')@ as of this
--    writing) must then endorse the proposal. Endorsements are not
--    transactions. Instead, every Byron header includes a field that specifies
--    a protocol version to endorse. At a particular stage of a corresponding
--    proposal's lifetime, that field constitutes an endorsement. At all other
--    times, it is essentially ignored. In the smoke test, we take advantage of
--    that to avoid having to restart our nodes: the nodes' initial
--    configuration causes them to immediately and always attempt to endorse
--    the proposed protocol version; this seems only slightly unrealistic.
--
--  * Epoch transition. After enough endorsements are 2k slots old, the
--    protocol version will be adopted at the next epoch transition, unless
--    something else prevents it. In the smoke test, we check the validation
--    state of the final chains for the new protocol version when we detect no
--    mitigating circumstances, such as the test not even being scheduled to
--    reach the second epoch.
--
mkProtocolByronAndHardForkTxs
  :: forall m. (Monad m, HasCallStack)
  => PBftParams
  -> CoreNodeId
  -> Genesis.Config
  -> Genesis.GeneratedSecrets
  -> Update.ProtocolVersion
     -- ^ the protocol version that triggers the hard fork
  -> TestNodeInitialization m ByronBlock
mkProtocolByronAndHardForkTxs
  params cid genesisConfig genesisSecrets propPV =
    TestNodeInitialization
      { tniCrucialTxs   = proposals ++ votes
      , tniProtocolInfo = pInfo
      }
  where
    ProtocolInfo{pInfoConfig} = pInfo
    bcfg = configBlock pInfoConfig

    pInfo :: ProtocolInfo m ByronBlock
    opKey :: Crypto.SigningKey
    (pInfo, Crypto.SignKeyByronDSIGN opKey) =
        mkProtocolByron params cid genesisConfig genesisSecrets

    proposals :: [Byron.GenTx ByronBlock]
    proposals =
        if cid /= CoreNodeId 0 then [] else
        (:[]) $
        Byron.fromMempoolPayload $
        MempoolPayload.MempoolUpdateProposal proposal

    votes :: [Byron.GenTx ByronBlock]
    votes =
        (:[]) $
        Byron.fromMempoolPayload $
        MempoolPayload.MempoolUpdateVote vote

    vote :: AVote ByteString
    vote =
        loopbackAnnotations $
        -- signed by delegate SK
        Vote.signVote
          (Byron.byronProtocolMagicId bcfg)
          (Update.recoverUpId proposal)
          True   -- the serialization hardwires this value anyway
          (Crypto.noPassSafeSigner opKey)

    proposal :: AProposal ByteString
    proposal =
        loopbackAnnotations $
        mkHardForkProposal params genesisConfig genesisSecrets propPV

-- | A protocol parameter update proposal that doesn't actually change any
-- parameter value but does propose 'theProposedProtocolVersion'
--
-- Without loss of generality, the proposal is signed by @'CoreNodeId' 0@.
--
mkHardForkProposal
  :: HasCallStack
  => PBftParams
  -> Genesis.Config
  -> Genesis.GeneratedSecrets
  -> Update.ProtocolVersion
  -> AProposal ()
mkHardForkProposal params genesisConfig genesisSecrets propPV =
    -- signed by delegate SK
    Proposal.signProposal
      (Byron.byronProtocolMagicId bcfg)
      propBody
      (Crypto.noPassSafeSigner opKey)
  where
    pInfo :: ProtocolInfo Identity ByronBlock
    opKey :: Crypto.SigningKey
    (pInfo, Crypto.SignKeyByronDSIGN opKey) =
        mkProtocolByron params (CoreNodeId 0) genesisConfig genesisSecrets

    ProtocolInfo{pInfoConfig} = pInfo
    bcfg = configBlock pInfoConfig

    propBody :: Proposal.ProposalBody
    propBody = Proposal.ProposalBody
      { Proposal.protocolVersion          = propPV
      , Proposal.protocolParametersUpdate = Update.ProtocolParametersUpdate
        { Update.ppuScriptVersion     = Nothing
        , Update.ppuSlotDuration      = Nothing
        , Update.ppuMaxBlockSize      = Nothing
        , Update.ppuMaxHeaderSize     = Nothing
        , Update.ppuMaxTxSize         = Nothing
        , Update.ppuMaxProposalSize   = Nothing
        , Update.ppuMpcThd            = Nothing
        , Update.ppuHeavyDelThd       = Nothing
        , Update.ppuUpdateVoteThd     = Nothing
        , Update.ppuUpdateProposalThd = Nothing
        , Update.ppuUpdateProposalTTL = Nothing
        , Update.ppuSoftforkRule      = Nothing
        , Update.ppuTxFeePolicy       = Nothing
        , Update.ppuUnlockStakeEpoch  = Nothing
        }
      , Proposal.softwareVersion          = theProposedSoftwareVersion
      , Proposal.metadata                 = Map.empty
      }

-- | Add the bytestring annotations that would be present if we were to
-- serialize the argument, send it to ourselves, receive it, and deserialize it
--
-- The mempool payloads require the serialized bytes as annotations. It's
-- tricky to get right, and this function lets use reuse the existing CBOR
-- instances.
--
loopbackAnnotations
  :: ( Cardano.Binary.FromCBOR (f Cardano.Binary.ByteSpan)
     , Cardano.Binary.ToCBOR (f ())
     , Functor f
     )
  => f ()
  -> f ByteString
loopbackAnnotations =
    ByronAPI.reAnnotateUsing
      Cardano.Binary.toCBOR
      Cardano.Binary.fromCBOR
