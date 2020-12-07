{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-name-shadowing  #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

module Scratch.Shelley (
  _main,
  tests,
  ) where

-- the general definitions
import           Control.Exception (Exception, SomeException)
import           Control.Monad (foldM, replicateM, when)
import           Control.Monad.ST.Lazy (ST, runST)
import qualified Data.ByteString as BS
import           Data.Dynamic (fromDynamic)
import           Data.Foldable (forM_)
import           Data.Functor ((<&>))
import           Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ratio ((%), Ratio)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Proxy (Proxy (..))
import           Data.Semigroup (Min (..))
import qualified Data.Sequence.Strict as StrictSeq
import           Data.Traversable (forM)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Quiet (Quiet (..))
import           System.Random (mkStdGen)
import           Test.QuickCheck

import           Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)

import           Control.Monad.Class.MonadSay (MonadSay, say)
import           Control.Monad.IOSim (Failure, IOSim, STMSim, TraceEvent (..), Trace (..), runSimTraceST, traceM)

import           Cardano.Slotting.Slot (EpochNo, EpochSize (..), SlotNo (..), WithOrigin (..))

import           Ouroboros.Network.Block (BlockNo, pattern GenesisPoint, Point, StandardHash)
import           Ouroboros.Network.Protocol.Limits (longWait, shortWait)

import           Ouroboros.Consensus.Block.Abstract (toShortRawHash)
import           Ouroboros.Consensus.BlockchainTime (SlotLength, SystemStart (..), getSlotLength, slotLengthFromMillisec)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract (hardForkSummary,)
import           Ouroboros.Consensus.HardFork.History.EpochInfo (snapshotEpochInfo)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (NodeToNodeVersion, supportedNodeToNodeVersions)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..), ProtocolInfo (..), enumCoreNodes)
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, Time)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import           Ouroboros.Consensus.Util.Time (nominalDelay)

import           Test.ThreadNet.Util.Expectations (NumBlocks (..))
import qualified Test.Util.HardFork.Future as HFF
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Slots (NumSlots (..))

import           Scratch.ThreadNet.MockNet (Arguments (Arguments), NetArguments (NetArguments), NodeArguments (NodeArguments), SomeNodeArguments (SomeNodeArguments))
import qualified Scratch.ThreadNet.MockNet as MockNet
import           Scratch.ThreadNet.Types
import qualified Scratch.ThreadNet.Usher as Usher
import           Scratch.Toolbox

----

import           Control.Monad.IOSim (Failure (..))

import           Test.Tasty
import           Test.Tasty.QuickCheck

-----

import           Cardano.Ledger.Crypto (DSIGN)
import           Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), seedSizeDSIGN)
import           Cardano.Crypto.Hash (ShortHash)
import           Cardano.Crypto.Seed (mkSeedFromBytes)

import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL (mkNonceFromNumber)
import qualified Shelley.Spec.Ledger.PParams as SL (emptyPParamsUpdate)
import qualified Shelley.Spec.Ledger.Tx as SL (WitnessSetHKD (..))
import qualified Shelley.Spec.Ledger.TxBody as SL (eraIndTxBodyHash)
import qualified Shelley.Spec.Ledger.UTxO as SL (makeWitnessesVKey)

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger (GenTx, mkShelleyTx)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Ledger

import           Test.Consensus.Shelley.MockCrypto (MockCrypto, MockShelley)

import qualified Test.ThreadNet.Infra.Shelley as ShelleyThreadNetInfra
import           Test.ThreadNet.Util.Seed (Seed (..), runGen)
import           Test.Util.Time (dawnOfTime)

import           Control.Tracer (showTracing)
import           Scratch.ThreadNet.MockNode (VertexTracers (..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN

import Debug.Trace (trace)

deriving instance Read SL.Nonce

type MyEra = MockShelley ShortHash

type MyBlock = ShelleyBlock MyEra
type MyCrypto = EraCrypto MyEra

{-------------------------------------------------------------------------------
  Shelley Generator
-------------------------------------------------------------------------------}

data ShelleyTestSetup = ShelleyTestSetup
  { activeSlotCoeff       :: Ratio Word64
  , decentralizationParam :: ShelleyThreadNetInfra.DecentralizationParam
  , initialNonce          :: SL.Nonce
  }
  deriving (Read, Show)

genShelleyTestSetup :: Gen ShelleyTestSetup
genShelleyTestSetup = do
    activeSlotCoeff       <- do
      let denom = 1000 :: Word64 -- TODO magic number
      elements $
        filter (>= 0.02) $ -- TODO magic number
        map (% denom) $
        [1 .. denom - 1]
    decentralizationParam <- arbitrary
    initialNonce          <- frequency
        [ (1, pure SL.NeutralNonce)
        , (9, SL.mkNonceFromNumber <$> arbitrary)
        ]
    pure ShelleyTestSetup
      { activeSlotCoeff
      , decentralizationParam
      , initialNonce
      }

-- | @ceiling (10k/f)@
mkShelleyEpochSize :: ShelleyTestSetup -> SecurityParam -> EpochSize
mkShelleyEpochSize shelleyTestSetup k =
    ShelleyThreadNetInfra.mkEpochSize
      k
      (ShelleyThreadNetInfra.toActiveSlotCoeff activeSlotCoeff)
  where
    ShelleyTestSetup
      { activeSlotCoeff
      } = shelleyTestSetup

deriving instance Show (Ticked (LedgerState MyBlock))

mkShelleyNodeArguments ::
     forall m. IOLike m
  => Tracer m String
  -> Map CoreNodeId ((Tracer m (SelectionEvent MyBlock), Tracer (IOLike.STM m) (StmEvent MyBlock)))
  -> ShelleyTestSetup
  -> NumCoreNodes
  -> SecurityParam
  -> HFF.Future
  -> CoreNodeId
  -> SomeNodeArguments m
mkShelleyNodeArguments
  dbgTracer
  tracers
  shelleyTestSetup
  numCoreNodes
  k
  hffFuture
  coreNodeId
  = SomeNodeArguments $ NodeArguments
  { MockNet.protocolInfo
  , MockNet.vertexTracers   = VertexTracers
      { tracerBtime      = nullTracer
      , tracerChainDB    = nullTracer `asTypeOf` showTracing dbgTracer'
      , tracerHarness    = showTracing dbgTracer'
      , tracerLedgerDB   = nullTracer
      , tracersConsensus = Consensus.nullTracers `asTypeOf` Consensus.showTracers dbgTracer' nullTracer
      , tracersNTN       = NTN.nullTracers `asTypeOf` NTN.showTracers dbgTracer'
      } <>
      usefulEventSubsetTracer
        `uncurry`
        (Map.findWithDefault (nullTracer, nullTracer) coreNodeId tracers)
  , MockNet.wrapForgeBlock  =
      Usher.wrapForgeBlock topLevelConfigLedger Usher.UsherBlockConfig
        { Usher.blockEra                = \_blk ->
            -- if MyBlock were a HardForkBlock, this would be the summand tag
            Usher.PseudoEraNo 0
        , Usher.getEpochInfo            = \tlSt ->
            let TickedShelleyLedgerState
                  { tickedShelleyLedgerState
                  , tickedShelleyLedgerTransition
                  , untickedShelleyLedgerTip
                  } = tlSt
                lSt = ShelleyLedgerState
                  { shelleyLedgerState      = tickedShelleyLedgerState
                  , shelleyLedgerTip        = untickedShelleyLedgerTip
                  , shelleyLedgerTransition = tickedShelleyLedgerTransition
                  }
            in
            snapshotEpochInfo $ hardForkSummary topLevelConfigLedger lSt
        , Usher.mkEraEndingTransactions = \currentEra lockedInEpo txin ->
            flip (NE.:|) [] $
            mkEraEndingTx
              coreNodes
              lockedInEpo
              SL.ProtVer
                { SL.pvMajor =
                    succ $ fromIntegral $ Usher.unPseudoEraNo currentEra
                , SL.pvMinor = 0
                }
              txin
        , Usher.tickedLedgerEra         = \tlSt ->
            let TickedShelleyLedgerState
                  { tickedShelleyLedgerState
                  } = tlSt
            in getSlEra tickedShelleyLedgerState
        , Usher.toUniversalBlockHash    =
            Usher.UniversalBlockHash . toShortRawHash (Proxy :: Proxy MyBlock)
        , Usher.toUsherTxIn             = \tlSt ->
            -- There should always be one txin at the usher's address.
            --
            -- TODO We'll need some extra logic to handle the case when the
            -- usher's funds start off in a bootstrap (ie Byron) address for
            -- the Cardano test etc.
            let TickedShelleyLedgerState
                  { tickedShelleyLedgerState
                  } = tlSt
                usherUtxo =
                    SL.getFilteredUTxO
                      tickedShelleyLedgerState
                      (Set.singleton usherAddr)
            in
            case Map.toList $ SL.unUTxO usherUtxo of
              [(txin, _txout)] -> txin
              _                -> error "impossible!"
        }
        . (<> showTracing dbgTracer)
  }
  where
    ShelleyTestSetup
      { activeSlotCoeff
      , decentralizationParam
      , initialNonce
      } = shelleyTestSetup

    dbgTracer' = contramap (\s -> show coreNodeId <> " " <> s) dbgTracer

    (slotLength, _epochSize) = case hffFuture of
      HFF.EraCons{}           -> error "exampleMockShelleyArguments: MyEraCons"
      HFF.EraFinal slen esize -> (slen, esize)

    genesisConfig :: SL.ShelleyGenesis MyEra
    genesisConfig =
        extendGenesisConfig $
        ShelleyThreadNetInfra.mkGenesisConfig
          genesisProtVer
          k
          (ShelleyThreadNetInfra.toActiveSlotCoeff activeSlotCoeff)
          decentralizationParam
          maxLovelaceSupply
          slotLength
          ( ShelleyThreadNetInfra.mkKesConfig
              (Proxy @MyCrypto)
              (NumSlots maxBound)
          )
          coreNodes

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    initSeed :: Seed
    initSeed = Seed 1923851092385

    coreNodes :: [ShelleyThreadNetInfra.CoreNode MyCrypto]
    coreNodes =
        runGen initSeed $
        replicateM (fromIntegral n) $
        ShelleyThreadNetInfra.genCoreNode initialKESPeriod
      where
        NumCoreNodes n = numCoreNodes

    maxLovelaceSupply :: Word64
    maxLovelaceSupply =
        fromIntegral (length coreNodes)
      * ShelleyThreadNetInfra.initialLovelacePerCoreNode

    genesisProtVer :: SL.ProtVer
    genesisProtVer = SL.ProtVer 0 0

    -- Which protocol version to endorse
    nextProtVer :: SL.ProtVer
    nextProtVer =
        ShelleyThreadNetInfra.incrementMajorProtVer genesisProtVer

    protocolInfo :: ProtocolInfo m MyBlock
    protocolInfo =
        ShelleyThreadNetInfra.mkProtocolShelley
          genesisConfig
          initialNonce
          nextProtVer
          (coreNodes !! fromIntegral (unCoreNodeId coreNodeId))

    ProtocolInfo{pInfoConfig}            = protocolInfo
    TopLevelConfig{topLevelConfigLedger} = pInfoConfig

    getSlEra :: SL.NewEpochState MyEra -> Usher.PseudoEraNo
    getSlEra =
        Usher.PseudoEraNo . fromIntegral .
        SL.pvMajor . SL._protocolVersion .
        SL.esPp . SL.nesEs

mkEraEndingTx ::
     [ShelleyThreadNetInfra.CoreNode MyCrypto]
  -> EpochNo
  -> SL.ProtVer
  -> SL.TxIn MyCrypto
  -> GenTx MyBlock
mkEraEndingTx coreNodes epo pVer txin =
    mkShelleyTx $
    SL.Tx
      { SL._body       = body
      , SL._witnessSet = witnessSet
      , SL._metadata   = SL.SNothing
      }
  where
    witnessSet :: SL.WitnessSet MyEra
    witnessSet = SL.WitnessSet signatures mempty mempty

    -- Every node signs the transaction body, since it includes a " vote " from
    -- every node.
    --
    -- The usher also signs, since it's the one paying.
    signatures :: Set (SL.WitVKey 'SL.Witness MyCrypto)
    signatures =
        SL.makeWitnessesVKey (SL.eraIndTxBodyHash body) $
          SL.KeyPair (SL.VKey $ deriveVerKeyDSIGN usherKey) usherKey :
          [ SL.KeyPair (SL.VKey vk) sk
          | cn <- coreNodes
          , let sk = ShelleyThreadNetInfra.cnDelegateKey cn
          , let vk = deriveVerKeyDSIGN sk
          ]

    -- Nothing but the parameter update and the obligatory touching of an
    -- input.
    body :: SL.TxBody MyEra
    body = SL.TxBody
      { SL._certs    = StrictSeq.empty
      , SL._inputs   = Set.singleton txin
      , SL._mdHash   = SL.SNothing
      , SL._outputs  =
          -- We're merely touching the funds, since the minfee is 0.
          StrictSeq.singleton $ SL.TxOut usherAddr (SL.Coin usherFunds)
      , SL._ttl      = SlotNo maxBound   -- TODO what is this?
      , SL._txfee    = SL.Coin 0
      , SL._txUpdate = SL.SJust update
      , SL._wdrls    = SL.Wdrl Map.empty
      }

    -- One replicant of the parameter update per each node.
    update :: SL.Update MyEra
    update =
        flip SL.Update epo $ SL.ProposedPPUpdates $
        Map.fromList $
        [ ( SL.hashKey $ SL.VKey $
            deriveVerKeyDSIGN $ ShelleyThreadNetInfra.cnGenesisKey cn
          , SL.emptyPParamsUpdate
              { SL._protocolVersion = SL.SJust pVer
              }
          )
        | cn <- coreNodes
        ]

-- | The secret key of the usher
--
-- This key allows use of the funds set aside for paying for era-transition
-- transactions.
usherKey :: SL.SignKeyDSIGN (MockCrypto ShortHash)
usherKey =
    runGen (Seed 43110) $
    fmap (genKeyDSIGN . mkSeedFromBytes . BS.pack) $
    vectorOf (fromIntegral nBytes) arbitrary
  where
    nBytes :: Word
    nBytes = seedSizeDSIGN (Proxy @(DSIGN (MockCrypto ShortHash)))
  
usherAddr :: SL.Addr MyCrypto
usherAddr = SL.Addr
    ShelleyThreadNetInfra.networkId
    (ShelleyThreadNetInfra.mkCredential usherKey)
    SL.StakeRefNull

-- | Add enough funds for the usher to afford all necessary era transition
-- transactions
--
-- NOTE These funds are never staked. They do not affect the stake distribution
-- nor therefore the leader schedule.
extendGenesisConfig :: SL.ShelleyGenesis MyEra -> SL.ShelleyGenesis MyEra
extendGenesisConfig x = x {
    SL.sgInitialFunds      =
      Map.insert usherAddr (SL.Coin usherFunds) sgInitialFunds
  , SL.sgMaxLovelaceSupply = sgMaxLovelaceSupply + fromIntegral usherFunds
  }
  where
    SL.ShelleyGenesis
      { SL.sgInitialFunds
      , SL.sgMaxLovelaceSupply
      } = x

usherFunds :: Integer
usherFunds = 1   -- TODO magic number, something like minFree * numEras

{-------------------------------------------------------------------------------
  Net Generator
-------------------------------------------------------------------------------}

data NetTestSetup = NetTestSetup
  { commonK  :: SecurityParam
  , future   :: HFF.Future   -- TODO replace with HF.Summary?
  , plan     :: NetPlan
  }
  deriving (Read, Show)

genDelays :: IOLike.DiffTime -> Gen Delays
genDelays maxDelay = do
    n <- choose (0, 999)   -- TODO magic number
    Delays <$> nePlus1 n genDelay
  where
    -- TODO add occasional large ones to simulate network partitions
    genDelay :: Gen IOLike.DiffTime
    genDelay =
        ((* maxDelay) . realToFrac) <$> choose (0, 1 :: Double)

    nePlus1 :: forall a. Int -> Gen a -> Gen (NE.NonEmpty a)
    nePlus1 n x = (NE.:|) <$> x <*> vectorOf n x

-- | The large number of delays are illegible
removeAllDelays :: NetPlan -> NetPlan
removeAllDelays (NetPlan m) =
    NetPlan $ fmap (fmap go) m
  where
    go ev = case ev of
      PlannedDisconnect{}                   -> ev
      PlannedNodeUp{}                       -> ev
      PlannedConnect connId _delays version ->
          PlannedConnect connId delays version
        where
          delays = mpConst $ csPairConst $ Delays $ 0 NE.:| []

genNodeUpData :: Gen NodeUpData
genNodeUpData = do
    keepAliveRng <- mkStdGen <$> arbitrary
    pure NodeUpData
      { keepAliveRng
      }

shortestTimeouts :: MpTuple (Maybe IOLike.DiffTime)
shortestTimeouts = MpTuple
  { mpBF = longWait
  , mpCS = Nothing
  , mpKA = Just 60   -- TODO: #2505 should be 10s.
  , mpTS = shortWait
  }

genPlan :: [NodeToNodeVersion] -> NumCoreNodes -> SlotLength -> Gen NetPlan
genPlan ntnVersions numCoreNodes slotLength = do
    initialJoinEvents <- forM coreNodeIds $ \coreNodeId -> do
      -- WLOG each node is connected bidirectionally to some previous nodes,
      -- which ensures the initial topology is connected.
      elders <- do
        let CoreNodeId i = coreNodeId
        if 0 == i then pure [] else
          flip suchThat (not . null) $
            sublistOf (enumCoreNodes (NumCoreNodes i))  -- NB excludes i

      let
        firstInstance = 0   -- TODO generate later instances too

        -- TODO should we /somehow/ align the occasional large delays so that
        -- the different mini protocols protocol channels and/or the
        -- client-server pair channels simulate contemporaneous network
        -- partitions?
        genDelays' :: Gen (MpTuple (ClientServerPair Delays))
        genDelays' =
            sequence $ fmap sequence $
                mpConst (\maxDelay -> csPairConst (genDelays maxDelay))
            <*> fmap f shortestTimeouts
          where
            -- TODO magic number
            --
            -- TODO should this depend on the active slot coefficient?
            dflt = 20 * nominalDelay (getSlotLength slotLength)

            -- TODO why must we divide by two? are these timeouts actually
            -- round-trip timeouts?
            f x = maybe id (min . (/2)) x dflt

      initialConnectionEvents <- foldM
        ( \acc elder -> do
          d1 <- genDelays'
          d2 <- genDelays'
          v1 <- elements ntnVersions   -- TODO fix one version per node pair?
          v2 <- elements ntnVersions
          pure $
            PlannedConnect
              (ConnectionId (ClientServerPair coreNodeId elder) firstInstance)
              d1
              v1 :
            PlannedConnect
              (ConnectionId (ClientServerPair elder coreNodeId) firstInstance)
              d2
              v2 :
            acc
        )
        []
        elders

      nodeUpData <- genNodeUpData

      pure $
        PlannedNodeUp (NodeId coreNodeId firstInstance) nodeUpData :
        initialConnectionEvents

    -- TODO dynamic connects and disconnects

    pure $
      -- all core nodes join and connect at the very beginning of the
      -- simulation
      eventsNetPlan 0 $ concat initialJoinEvents
  where
    coreNodeIds = enumCoreNodes numCoreNodes

genNetTestSetup ::
     [NodeToNodeVersion]
     -- ^ the set of versions supported by all nodes
  -> SecurityParam
  -> EpochSize
  -> Gen NetTestSetup
genNetTestSetup ntnVersions commonK epochSize = do
    -- TODO magic number
    numCoreNodes <- NumCoreNodes           <$> choose (2, 5)
    -- TODO magic number
    slotLength   <- slotLengthFromMillisec <$> choose (100, 10000)

    plan         <- genPlan ntnVersions numCoreNodes slotLength

    pure NetTestSetup
      { commonK
      , future   = HFF.singleEraFuture slotLength epochSize
      , plan
      }

exampleMockShelleyArguments :: forall m.
     IOLike m
  => Tracer m String
  -> (Tracer m (), Tracer m (ThrottlerState m))
  -> NetTestSetup
  -> ShelleyTestSetup
  -> Map CoreNodeId (Tracer m (SelectionEvent MyBlock), Tracer (IOLike.STM m) (StmEvent MyBlock))
  -> Tracer m PlannedEvent
  -> Arguments m
exampleMockShelleyArguments dbgTracer throttlerTracers netTestSetup shelleyTestSetup tracers tracerPlannedEvent =
    Arguments
      netArguments
      (mkShelleyNodeArguments dbgTracer tracers shelleyTestSetup (plannedNumCoreNodes plan))
  where
    NetTestSetup
      { commonK
      , future
      , plan
      } = netTestSetup
    ShelleyTestSetup
      { activeSlotCoeff
      } = shelleyTestSetup

    (enterObserving, endOfDays) = throttlerTracers

    netArguments = NetArguments
      { MockNet.commonK
      , MockNet.firstProperBlockNo   = 0
      , MockNet.future
      , MockNet.mbEndOfDays          = Nothing
      , MockNet.plan
      , MockNet.systemStart          = SystemStart dawnOfTime
      , MockNet.tracerThrottlerEvent = Tracer $ \ev -> case ev of
          EndOfDays prevStat
            -> do
            traceWith endOfDays prevStat
            traceWith dbgTracer $
              replicate 200 '-' <> " TRUNCATED TO LastFlush from " <> show prevStat <> " " <> replicate 200 '-'
          FlushingOnce{}
            -> traceWith dbgTracer $ show ev
          FlushingNoneInflight{}
            -> traceWith dbgTracer $ show ev
          FlushingNewMessages{}
            -> traceWith dbgTracer $ show ev
          FlushingQuiescence flushingState
            -> do
            traceWith dbgTracer $ show ev
            case flushingState of
              FlushingState1{}    -> pure ()
              FlushingState2{}    -> traceWith enterObserving ()
              FlushingStateLast{} -> pure ()
          ForgeTransition Observing{}
            -> error "impossible!"   -- only the flusher transitions to this state
          ForgeTransition Flushing1{}
            -> pure ()
          ForgeTransition Extending{}
            -> pure ()
                 -- a forge exit transitions to this state when the leader did
                 -- not succeed (eg was suppressed)
          ForgeTransition Flushing2{}
            -> pure ()
          ForgeTransition LastFlush{}
            -> error "impossible!"   -- only the timeout transitions to this state
          ForgeTransition Shutdown{}
            -> error "impossible!"   -- only the flusher transitions to this state
      , MockNet.tracerPlannedEvent
      , MockNet.usherNetConfig       =
          Usher.UsherNetConfig $
          flip (NE.:|) [] $
          Usher.UsherEraConfig
            { Usher.desiredEraSize  = Usher.EraSize 3   -- TODO magic number
            , Usher.stabilityWindow =
                NumSlots $
                SL.computeStabilityWindow
                  (maxRollbacks commonK)
                  (ShelleyThreadNetInfra.toActiveSlotCoeff activeSlotCoeff)
            , Usher.whetherIsByron  = Usher.IsNotByron
            }
      }

{-------------------------------------------------------------------------------
  Entry points
-------------------------------------------------------------------------------}

data Unhandled = Unhandled !Time SomeException
 deriving (Show)

instance Exception Unhandled

{-------------------------------------------------------------------------------
  Instrumented simulation
-------------------------------------------------------------------------------}

data Observation
  = ObserveEnterObserving
  | ObserveEndOfDays String

  | ObserveSpan NumSlots

  | ObserveImmutablePoint CoreNodeId (Point MyBlock)
  | ObserveVolatileBlockNo CoreNodeId (WithOrigin BlockNo)

  | ObserveSwitch NumBlocks NumSlots

-- | NOTE this is the /lazy/ ST monad
mkTrace :: forall s.
     NetTestSetup
  -> ShelleyTestSetup
  -> ST s (Trace ())
{-# INLINE mkTrace #-}
mkTrace netTestSetup shelleyTestSetup = runSimTraceST $ do
    let dbgTracer :: forall m. MonadSay m => Tracer m String
        dbgTracer = nullTracer `asTypeOf` Tracer say
        
    let updBlockedness :: StmEvent MyBlock -> STMSim s ()
        updBlockedness =
            traceWith dbgTracer . show

    let updWorstSpan :: AnchoredFragmentStats -> IOSim s ()
        updWorstSpan afStats =
            when (maxRollbacks commonK == unNumBlocks afLength) $
            traceM $ ObserveSpan afSpan
          where
            AnchoredFragmentStats
              { afLength
              , afSpan
              } = afStats

    let tracers ::
          Map CoreNodeId
          ( Tracer (IOSim s) (SelectionEvent MyBlock)
          , Tracer (STMSim s) (StmEvent MyBlock)
          )
        tracers =
            Map.fromList $
            [ (,) coreNodeId $
              flip (,) (Tracer updBlockedness) $
              (<>) (Tracer (traceWith dbgTracer . (\s -> show coreNodeId <> " " <> s) . show)) $
              Tracer $ \case
                AmLeader{}
                  -> pure ()
                DecidedToFetch{}
                  -> pure ()
                DownloadedBlock{}
                  -> pure ()
                ev@DownloadedBlockCS{}
                  -> traceWith dbgTracer (show ev)
                DownloadedHeader{}
                  -> pure ()
                ExtendedSelection _p bno afStats _prev
                  -> do
                  traceM $ ObserveVolatileBlockNo coreNodeId bno
                  updWorstSpan afStats
                FinalSelection immPoint _volPoint
                  -> do
                  traceM $ ObserveImmutablePoint coreNodeId immPoint
                ForgedBlock _sl _p _bno _prev
                  -> pure ()
                HarnessEvent _ev
                  -> pure ()
                ev@Humoring{}
                  -> traceWith dbgTracer (show ev)
                SwitchedSelection _p bno afStats _prev (oldSuf, newSuf)
                  -> do
                  traceM $ ObserveVolatileBlockNo coreNodeId bno
                  updWorstSpan afStats
                  do
                    let AnchoredFragmentStats
                          { afLength
                          } = oldSuf
                    let AnchoredFragmentStats
                          { afSpan
                          } = newSuf
                    traceM $ ObserveSwitch afLength afSpan
          | coreNodeId <- enumCoreNodes $ plannedNumCoreNodes (plan netTestSetup)
          ]

    let throttlerTracers =
            ( Tracer $ \() -> traceM ObserveEnterObserving
            , Tracer $ \st -> traceM $ ObserveEndOfDays $ case st of
                Observing{}       -> "Observing"
                Flushing1{}       -> "Flushing1"
                Extending Nothing -> "ExtendingNothing"
                Extending Just{}  -> "ExtendingJust"
                Flushing2{}       -> "Flushing2"
                LastFlush{}       -> "LastFlush"
                Shutdown{}        -> error "impossible!"
            )
    MockNet.simulateNet $ exampleMockShelleyArguments
      dbgTracer
      throttlerTracers
      netTestSetup
      shelleyTestSetup
      tracers
      nullTracer
  where
    NetTestSetup
      { commonK
      } = netTestSetup

{-------------------------------------------------------------------------------
  QC Main
-------------------------------------------------------------------------------}

data TestSetup = TestSetup
  { netTestSetup   :: NetTestSetup
  , shelleyTestSetup :: ShelleyTestSetup
  }
  deriving (Read, Show)

instance Arbitrary TestSetup where
  arbitrary = do
      -- TODO magic number
      commonK      <- SecurityParam <$> choose (2, 8)

      shelleyTestSetup <- genShelleyTestSetup

      netTestSetup   <- genNetTestSetup
        shelleyNtnVersions
        commonK
        (mkShelleyEpochSize shelleyTestSetup commonK)

      pure TestSetup
        { netTestSetup
        , shelleyTestSetup
        }
    where
      shelleyNtnVersions =
          Map.keys $
          supportedNodeToNodeVersions (Proxy :: Proxy MyBlock)

-- | Simulates via 'mkTrace' and then checks each of the following
--
-- o 'prop_FinalNodeDataMap'
prop_ShelleySimulation :: TestSetup -> Property
prop_ShelleySimulation testSetup =
    counterexample "==================== CONTEXT ====================" $
    contextShow "k" (maxRollbacks commonK) $
    contextShowOnFAIL
      "plan with the illegible delays removed"
      ( case removeAllDelays plan of
          NetPlan m ->
              [ (s, ev) | (s, evs) <- Map.toAscList m, ev <- NE.toList evs ]
      ) $
    contextShowOnFAIL
      "legible netTestSetup"
      netTestSetup{plan = emptyNetPlan} $
    contextShowOnFAIL "shelleyTestSetup" shelleyTestSetup $
    contextShow "stability window" stabilityWindow $
    ioProperty $
    flip fmap (processTraceIO netTestSetup trace) $ \(_numEvents, result, mbError) ->
    let CumulativeObservations
          { numEnterObserving
          , tips
          , truncatedState
--          , worstSpan
          , worstSwitchLength
--          , worstSwitchSpan
          } = result

        minBno = foldMap (Just . Min . volTipBno) finalNodeDataMap

        finalNodeDataMap :: Map CoreNodeId (FinalNodeData MyBlock)
        finalNodeDataMap =
            tips <&> \(Id volTipBno, Id immTipPoint) -> FinalNodeData
              { volTipBno
              , immTipPoint
              }
    in
    contextShowOnFAIL "simulation result" result $
    contextShow "numEnterObserving" (unId numEnterObserving) $
    context ("truncatedState, " <> show numCoreNodes) (maybe "aborted" id $ unId truncatedState) $
    context ("truncatedState, " <> show commonK) (maybe "aborted" id $ unId truncatedState) $
    contextShowOnFAIL ("minimum final bno, " <> show commonK) minBno $
    contextShow ("worstSwitchLength, " <> show commonK) (unId worstSwitchLength) $
    case mbError of
      Just e  -> testFAIL $ "io-sim failure! " <> show e
      Nothing ->
          prop_FinalNodeDataMap netTestSetup shelleyTestSetup finalNodeDataMap
  where
    TestSetup
      { netTestSetup
      , shelleyTestSetup
      } = testSetup
    NetTestSetup
      { commonK
      , plan
      } = netTestSetup
    ShelleyTestSetup
      { activeSlotCoeff
      } = shelleyTestSetup

    numCoreNodes = plannedNumCoreNodes plan

    trace = runST $ mkTrace netTestSetup shelleyTestSetup

    -- Shelley stability window
    stabilityWindow :: Word64
    stabilityWindow =
        SL.computeStabilityWindow
          (maxRollbacks commonK)
          (ShelleyThreadNetInfra.toActiveSlotCoeff activeSlotCoeff)

data FinalNodeData blk = FinalNodeData
  { immTipPoint :: Point blk
    -- ^ The point of the node's final immutable tip
  , volTipBno   :: WithOrigin BlockNo
    -- ^ The bno of the node's final volatile tip
  }
  deriving (Eq, Generic, Ord)
  deriving Show via Quiet (FinalNodeData blk)

-- | Check that all nodes have the exact same 'FinalNodeData'.
prop_FinalNodeDataMap ::
     StandardHash blk
  => NetTestSetup
  -> ShelleyTestSetup
  -> Map CoreNodeId (FinalNodeData blk)
  -> Property
prop_FinalNodeDataMap _netTestSetup _shelleyTestSetup finalNodeDataMap =
    case Set.maxView $ Set.fromList $ Map.elems finalNodeDataMap of
      Nothing
        -> testFAIL "No final node data!"
      Just (_finalNodeData, rest)
        | not $ Set.null rest
        -> testFAIL "Differing final immutable tip points!"
        | otherwise
        ->
        testPASS
        where
{-          FinalNodeData
            { volTipBno
            } = finalNodeData -}
  where
{-    ShelleyTestSetup
      { activeSlotCoeff
      } = shelleyTestSetup -}

tests :: TestTree
tests = testGroup "Shelley ThreadNet simulations" $
    [ testProperty "arbitrary TestSetup" $ \testSetup ->
        trace ("a TestSetup with"
               <> " " <>
               show (commonK $ netTestSetup testSetup)
               <> " " <>
               show (plannedNumCoreNodes $ plan $ netTestSetup testSetup)
              ) $
        prop_ShelleySimulation testSetup
    ]

_main :: IO ()
_main = IOLike.withAsync (defaultMain tests) IOLike.wait

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

data CumulativeObservations f = CumulativeObservations
  { numEnterObserving :: f Word64
    -- ^ How many times the Throttler entered 'Observing', excluding the fact
    -- that the simulation begins there.

  , tips :: Map CoreNodeId (f (WithOrigin BlockNo), f (Point MyBlock))

  , truncatedState    :: f (Maybe String)
    -- ^ What was the state at the end of days?

  , worstSpan         :: f (Maybe NumSlots)
    -- ^ How many slots it took some selected chain to span k blocks
    --
    -- 'Nothing' iff no such chain ever spanned k blocks

  , worstSwitchLength :: f NumBlocks
    -- ^ The length of the old suffix
  , worstSwitchSpan   :: f NumSlots
    -- ^ The span of the new suffix
  }

deriving instance Show (CumulativeObservations Id)
  
traverseCO ::
     Applicative i
  => (forall x. f x -> i (g x))
  -> CumulativeObservations f
  -> i (CumulativeObservations g)
traverseCO f x = do
    numEnterObserving <- f numEnterObserving

    tips <- traverse (\(x0, x1) -> (,) <$> f x0 <*> f x1) tips

    truncatedState <- f truncatedState

    worstSpan <- f worstSpan

    worstSwitchLength <- f worstSwitchLength
    worstSwitchSpan   <- f worstSwitchSpan

    pure CumulativeObservations
      { numEnterObserving
      , tips
      , truncatedState
      , worstSpan
      , worstSwitchLength
      , worstSwitchSpan
      }
  where
    CumulativeObservations
      { numEnterObserving
      , tips
      , truncatedState
      , worstSpan
      , worstSwitchLength
      , worstSwitchSpan
      } = x
    
newtype Id a = Id {unId :: a}
  deriving (Foldable, Functor, Generic, Traversable)
  deriving Show via Quiet (Id a)

processTraceIO :: NetTestSetup -> Trace () -> IO (Int, CumulativeObservations Id, Maybe Failure)
processTraceIO netTestSetup trace = do
    tips <- Map.fromList <$> sequence [ do
        bnoRef      <- newIORef Origin
        immPointRef <- newIORef GenesisPoint
        pure (coreNodeId, (bnoRef, immPointRef))
      | coreNodeId <- enumCoreNodes $ plannedNumCoreNodes (plan netTestSetup)
      ]

    numEnterObserving <- newIORef 0
    truncatedState    <- newIORef Nothing

    worstSpan         <- newIORef Nothing
    worstSwitchLength <- newIORef $ NumBlocks 0
    worstSwitchSpan   <- newIORef $ NumSlots 0

    go1
      0
      CumulativeObservations
        { numEnterObserving
        , truncatedState
        , tips
        , worstSpan
        , worstSwitchLength
        , worstSwitchSpan
        }
      trace
  where
    finish acc refsCO x = do
        result <- traverseCO (fmap Id . readIORef) refsCO
        pure (acc, result, x)

    go1 !acc refsCO thunk =
        (IOLike.try $ IOLike.evaluate thunk) >>= \case
          Left exn -> finish acc refsCO $ Just $ FailureException exn
          Right t  -> go2 (acc + 1) refsCO t

    go2 !acc refsCO = \case
        Trace _ _ _ ev t -> do
            go3 refsCO ev
            go1 (acc + 1) refsCO t
        TraceMainReturn _ _ tids@(_:_) ->
            finish acc refsCO $ Just $ FailureSloppyShutdown tids
        TraceMainReturn _ () _         -> finish acc refsCO Nothing
        TraceMainException _ e _       ->
            finish acc refsCO $ Just $ FailureException e
        TraceDeadlock _ x              ->
            finish acc refsCO $ Just $ FailureDeadlock x


    go3 refsCO = \case
        EventSay msg ->
            putStrLn msg
        EventLog dyn -> forM_ (fromDynamic dyn) $ \case
            ObserveEnterObserving -> modifyIORef' numEnterObserving (+1)
            ObserveEndOfDays s    -> writeIORef truncatedState $ Just s

            ObserveSpan span ->
                modifyIORef' worstSpan $ Just . maybe span (max span)

            ObserveImmutablePoint coreNodeId immPoint ->
                forM_ (Map.lookup coreNodeId tips) $
                \(_volBnoRef, immPointRef) -> writeIORef immPointRef immPoint
            ObserveVolatileBlockNo coreNodeId volBno ->
                forM_ (Map.lookup coreNodeId tips) $
                \(volBnoRef, _immPointRef) -> writeIORef volBnoRef volBno

            ObserveSwitch numDropped numSpanned -> do
                modifyIORef' worstSwitchLength $ max numDropped
                modifyIORef' worstSwitchSpan   $ max numSpanned
        _ -> pure ()
      where
        CumulativeObservations
          { numEnterObserving
          , tips
          , truncatedState
          , worstSpan
          , worstSwitchLength
          , worstSwitchSpan
          } = refsCO

testPASS :: Property
testPASS = property True

testFAIL :: String -> Property
testFAIL s =
    counterexample "==================== FAILURE ====================" $
    counterexample s $
    property False

contextShow :: (Testable prop, Show a) => String -> a -> prop -> Property
contextShow key val = context key (show val)

-- | Include key and value in QC output for PASS and for FAIL
context :: Testable prop => String -> String -> prop -> Property
context key val =
    tabulate key [val] .
    contextOnFAIL key val

contextShowOnFAIL :: (Testable prop, Show a) => String -> a -> prop -> Property
contextShowOnFAIL key val = contextOnFAIL key (show val)

-- | Include key and value in QC output only for FAIL
contextOnFAIL :: Testable prop => String -> String -> prop -> Property
contextOnFAIL key val =
    counterexample (key <> ": " <> val)

_showOrigin :: (a -> String) -> WithOrigin a -> String
_showOrigin f = \case
    Origin -> "Origin"
    At x   -> f x

_showMaybe :: (a -> String) -> Maybe a -> String
_showMaybe f = \case
    Nothing -> "Nothing"
    Just x  -> f x

_showTens :: (Integral a, Show a) => a -> String
_showTens x =
    "in [" <> show (10 * y) <> ", " <> show (10 * (y + 1)) <> ")"
  where
    y = div x 10
