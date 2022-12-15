{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | See 'MakeAtomic'.
module Test.Consensus.Mempool.StateMachine (tests) where

import           Control.Arrow (first)
import           Control.Monad (void)
import           Control.Monad.Except (runExcept)
import           Data.Bool (bool)
import           Data.Foldable hiding (toList)
import           Data.Function (on)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.TreeDiff (Expr (..), genericToExpr)
import qualified Data.TreeDiff.OMap as TD
import           GHC.Generics

import           Control.Tracer (nullTracer)

import           Cardano.Crypto.Hash.Class
import           Cardano.Crypto.Hash.SHA256
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool hiding (getTxSize)
import           Ouroboros.Consensus.Mempool.Impl.Types (ForgeLedgerState (..),
                     tickLedgerState)
import           Ouroboros.Consensus.Mempool.TxSeq
import           Ouroboros.Consensus.Storage.ChainDB.API hiding
                     (getLedgerTablesAtFor)
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike hiding (bracket)

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.StateMachine hiding ((:>))
import           Test.StateMachine.DotDrawing
import qualified Test.StateMachine.Types as QC
import           Test.StateMachine.Types (History (..), HistoryEvent (..))
import qualified Test.StateMachine.Types.Rank2 as Rank2

import           Ouroboros.Consensus.Mock.Ledger.Address
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.State
import           Ouroboros.Consensus.Mock.Ledger.UTxO (Expiry, Tx, TxIn, TxOut)

import           Test.Consensus.Mempool.Util (TestBlock, applyTxToLedger,
                     genTxs, genValidTxs, testInitLedger, testLedgerConfig)

import           Test.Tasty
import           Test.Tasty.QuickCheck

{-------------------------------------------------------------------------------
  Datatypes
-------------------------------------------------------------------------------}

-- | Whether the LedgerDB should be wiped out
data ModifyDB = KeepDB | ClearDB deriving (Generic, ToExpr, NoThunks)

instance Arbitrary ModifyDB where
  arbitrary = bool KeepDB ClearDB <$> arbitrary

keepsDB :: ModifyDB -> Bool
keepsDB KeepDB  = True
keepsDB ClearDB = False

-- | The model
data Model blk r = Model {
    -- | The current tip on the mempool
    modelMempoolIntermediateState :: !(TickedLedgerState blk ValuesMK)

    -- | The old states which are still on the LedgerDB
  , modelOldReachableStates       :: !(Set (LedgerState blk ValuesMK))

    -- | The old states which are no more on the LedgerDB
  , modelOldUnreachableStates     :: !(Set (LedgerState blk ValuesMK))

    -- | The current tip on the ledgerdb
  , modelLedgerDBTip              :: !(LedgerState blk ValuesMK)

    -- | The current list of transactions
  , modelTxs                      :: ![(GenTx blk, TicketNo)]

    -- | The maximum capacity of the mempool
  , modelRemainingCapacity        :: !MempoolCapacityBytes

    -- | Last seen ticket number
    --
    -- This indicates how many transactions have ever been added to the mempool.
  , modelLastSeenTicketNo         :: !TicketNo

    -- | Used only for tagging
  , modelMempoolWasFilled         :: !Bool
  }

-- | The commands used by QSM
--
-- We divide them in 'Action' which are the ones that we on purpose perform on
-- the mempool, and 'Event's which happen by external triggers. This is a mere
-- convenience, in the eyes of QSM they are the same thing.
data Command blk r =
    Action !(Action blk r)
  | Event  !(Event blk r)
  deriving (Generic1)
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

-- | Actions on the mempool
data Action blk r =
    -- | Add some transactions to the mempool
    TryAddTxs ![GenTx blk]
  | -- | Unconditionally sync with the ledger db
    SyncLedger
  | -- | Ask for the current snapshot
    GetSnapshot
  -- TODO: maybe add 'GetSnapshotFor (Point blk)', but this requires to keep
  -- track of some more states to make it meaningful.
  deriving (Generic1)
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

-- | Events external to the mempool
data Event blk r = ChangeLedger
    !(LedgerState blk ValuesMK)
    !ModifyDB
  deriving (Generic1)
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

instance CommandNames (Command blk) where
  cmdName (Action action) = cmdName action
  cmdName (Event event)   = cmdName event

  cmdNames :: forall r. Proxy (Command blk r) -> [String]
  cmdNames _ = cmdNames (Proxy @(Action blk r))
            ++ cmdNames (Proxy @(Event blk r))

-- | Wether or not this test must be atomic.
--
-- The reason behind this data type is that 'TryAddTxs' is on its nature prone
-- to race-conditions. And that is OK with us. For example take the following
-- sequence of commands:
--
-- @@@
--  TryAddTxs [Tx1, Tx2] || GetSnapshot
-- @@@
--
-- If we happen to hit the following interleaving:
--
-- @@@
--  AddTx Tx1; GetSnapshot; AddTx Tx2
-- @@@
--
-- the model will never be able to reproduce the result of the snapshot.
--
-- So in order to do a meaningful testing, what we do is:
--
-- 1. Run a sequential test of actions ensuring that the responses of the model
--    and SUT match on 'GetSnaphsot'. This provides us with assurance that the
--    model works as expected on single-threaded/sequential scenarios.
--
-- 2. Run a parallel test where 'TryAddTxs' is unitary (i.e. use the 'Atomic'
--    modifier) ensuring that the responses of the model and SUT match on
--    'GetSnaphsot'. This ensures that there are no race conditions on this
--    case, or rephrased, that the operations on the mempool remain atomic even
--    if executed on separate threads.
--
-- 3. Run a parallel test where 'TryAddTxs' is not unitary (using the
--    'NonAtomic' modifier) and **NOT** checking the responses of the model
--    versus the SUT. This ensures that there are no deadlocks and no
--    errors/exceptions thrown when running in parallel.
--
-- We believe that these test cover all the interesting cases and provide enough
-- assurance on the implementation of the Mempool.
data MakeAtomic = Atomic | NonAtomic | DontCare

generator ::
  ( Arbitrary (LedgerState blk ValuesMK)
  , UnTick blk
  )
  => MakeAtomic
  -> (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk])
     -- ^ Transaction generator based on an state
  -> Model blk Symbolic
  -> Maybe (Gen (Command blk Symbolic))
generator ma gTxs Model{ modelMempoolIntermediateState } =
   Just $
    frequency
      [ (100,
          case ma of
            Atomic -> do
              Action . TryAddTxs <$> (gTxs 1 . unTick $ modelMempoolIntermediateState)
            _ -> do
              n <- getPositive <$> arbitrary
              Action . TryAddTxs <$> (gTxs n . unTick $ modelMempoolIntermediateState)
        )
      , (10, pure $ Action SyncLedger)
      , (10, do
            ls <- arbitrary
            b  <- arbitrary
            pure $ Event $ ChangeLedger ls b)
      , (10, pure $ Action GetSnapshot)
      ]

data Response blk r =
    -- | Nothing to tell
    Void
  | -- | Return the contents of a snapshot
    GotSnapshot ![(GenTx blk, TicketNo)]
  deriving (Generic1)
  deriving (Rank2.Functor, Rank2.Foldable, Rank2.Traversable)

{-------------------------------------------------------------------------------
  Model side
-------------------------------------------------------------------------------}

initModel ::
  ( LedgerSupportsMempool blk
  , ValidateEnvelope blk
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> Model blk r
initModel cfg initialState =
  Model {
    modelMempoolIntermediateState = ticked
  , modelOldReachableStates       = Set.empty
  , modelOldUnreachableStates     = Set.empty
  , modelLedgerDBTip              = initialState
  , modelTxs                      = []
  , modelRemainingCapacity        = txMaxBytes'
  , modelLastSeenTicketNo         = zeroTicketNo
  , modelMempoolWasFilled         = False
  }
  where ticked = tick cfg initialState

mock ::
     Model blk Symbolic
  -> Command blk Symbolic
  -> GenSym (Response blk Symbolic)
mock model = \case
  Action (TryAddTxs _)     -> pure Void
  Action SyncLedger        -> pure Void
  Action GetSnapshot       -> pure $ GotSnapshot $ modelTxs model
  Event (ChangeLedger _ _) -> pure Void

{-------------------------------------------------------------------------------
  Transitions
-------------------------------------------------------------------------------}

doSync ::
  ( ValidateEnvelope blk
  , LedgerSupportsMempool blk
  , Eq (TickedLedgerState blk ValuesMK)
  , Eq (GenTx blk)
  )
  => LedgerCfg (LedgerState blk)
  -> Model blk r
  -> Model blk r
doSync cfg model =
    if modelMempoolIntermediateState == tick cfg modelLedgerDBTip
    then model
    else
      let modelTxs'          = fmap fst modelTxs
          (res, _, cap', st) = foldTxs cfg zeroTicketNo txMaxBytes'
                               (tick cfg modelLedgerDBTip)
                               modelTxs'
          filteredTxSeq      =
            [ (tx, tk) | (tx, tk) <- modelTxs
                       , tx
                            `elem` [ txForgetValidated vtx
                                   | MempoolTxAdded vtx <- fst res
                                   ]
           ]
      in
        model {
          modelMempoolIntermediateState = st
        , modelTxs                      = filteredTxSeq
        , modelRemainingCapacity        = cap'
        }
  where
      Model {
        modelMempoolIntermediateState
      , modelLedgerDBTip
      , modelTxs
      } = model

doChangeLedger ::
  (StandardHash blk, GetTip (LedgerState blk ValuesMK))
  => Model blk r
  -> LedgerState blk ValuesMK
  -> ModifyDB
  -> Model blk r
doChangeLedger model l' b'
 | any ((getTip l' ==) . getTip)
       (Set.union modelOldUnreachableStates modelOldReachableStates)
   || getTip l' == getTip modelLedgerDBTip
 = model
 | otherwise
 = model { modelLedgerDBTip = l'
         , modelOldReachableStates =
            if keepsDB b'
            then Set.insert modelLedgerDBTip modelOldReachableStates
            else Set.empty
         , modelOldUnreachableStates =
            if keepsDB b'
            then modelOldUnreachableStates
            else Set.insert modelLedgerDBTip
               $ Set.union modelOldUnreachableStates modelOldReachableStates
         }
 where
    Model {
        modelLedgerDBTip
      , modelOldReachableStates
      , modelOldUnreachableStates
      } = model

doTryAddTxs ::
  ( LedgerSupportsMempool blk
  , ValidateEnvelope blk
  , Eq (TickedLedgerState blk ValuesMK)
  , Eq (GenTx blk)
  )
  => LedgerCfg (LedgerState blk)
  -> Model blk r
  -> [GenTx blk]
  -> Model blk r
doTryAddTxs _ model [] = model
doTryAddTxs cfg model txs =
    case find ((castPoint (getTip modelMempoolIntermediateState) ==) . getTip)
              (Set.insert modelLedgerDBTip modelOldReachableStates) of
      Nothing -> doTryAddTxs cfg (doSync cfg model) txs
      Just _ ->
        let nextTicket          = succ $ modelLastSeenTicketNo model
            (res, tk, cap', st) = foldTxs cfg nextTicket modelRemainingCapacity
                                  modelMempoolIntermediateState
                                  $ txs
            modelTxs'           = modelTxs ++
                                  zip [ txForgetValidated vtx
                                      | MempoolTxAdded vtx <- fst res
                                      ]
                                      [nextTicket..]
        in
          model {
            modelMempoolIntermediateState = st
          , modelTxs                      = modelTxs'
          , modelLastSeenTicketNo         = pred tk
          , modelRemainingCapacity        = cap'
          , modelMempoolWasFilled         =
               modelMempoolWasFilled || (not . null . snd $ res)
          }
  where
    Model {
        modelMempoolIntermediateState
      , modelTxs
      , modelMempoolWasFilled
      , modelRemainingCapacity
      , modelOldReachableStates
      , modelLedgerDBTip
      } = model

transition ::
     ( Eq (GenTx blk)
     , Eq (TickedLedgerState blk ValuesMK)
     , LedgerSupportsMempool blk
     , ToExpr (GenTx blk)
     , ToExpr (LedgerState blk ValuesMK)
     , ValidateEnvelope blk
     )
  => LedgerConfig blk
  -> Model blk r
  -> Command blk r
  -> Response blk r
  -> Model blk r
transition cfg model cmd resp = case (cmd, resp) of
  (Action (TryAddTxs txs), Void) -> doTryAddTxs cfg model txs
  (Event (ChangeLedger l b), Void) -> doChangeLedger model l b
  (Action GetSnapshot, GotSnapshot{}) -> model
  (Action SyncLedger, Void) -> doSync cfg model
  _ -> error $ "mismatched command "
             <> show cmd
             <> " and response "
             <> show resp

{-------------------------------------------------------------------------------
  Ledger helper functions
-------------------------------------------------------------------------------}

-- | Apply a list of transactions short-circuiting if the mempool gets full.
-- Emulates almost exactly the behaviour of 'implTryTryAddTxs'.
foldTxs ::
  forall blk.
  ( LedgerSupportsMempool blk
  , BasicEnvelopeValidation blk
  )
  => LedgerConfig blk
  -> TicketNo
  -> MempoolCapacityBytes
  -> TickedLedgerState blk ValuesMK
  -> [GenTx blk]
  -> ( ([MempoolAddTxResult blk], [GenTx blk])
     , TicketNo
     , MempoolCapacityBytes
     , TickedLedgerState blk ValuesMK
     )
foldTxs cfg nextTk remainingCap initialState  =
    go ( []
       , nextTk
       , getMempoolCapacityBytes remainingCap
       , initialState
       )
  where
    go (acc, tk, cap, st) [] = ((reverse acc, [])
                               , tk
                               , MempoolCapacityBytes cap
                               , st
                               )
    go (acc, tk, cap, st) txs@(tx:next) =
      if cap < txInBlockSize tx
      then ((reverse acc, txs), tk, MempoolCapacityBytes cap, st)
      else
        let slot = case getTipSlot st of
              Origin -> minimumPossibleSlotNo (Proxy @blk)
              At v   -> v + 1
        in
        case runExcept $ applyTx cfg DoNotIntervene slot tx st of
          Left e           ->
            go ( MempoolTxRejected tx e:acc
               , tk
               , cap
               , st
               )
               next
          Right (st', vtx) ->
            go ( MempoolTxAdded vtx:acc
               , succ tk
               , cap - txInBlockSize tx
               , forgetLedgerTablesDiffsTicked st'
               )
               next

tick ::
  ( TickedTableStuff (LedgerState blk)
  , ValidateEnvelope blk
  , LedgerSupportsMempool blk
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> TickedLedgerState blk ValuesMK
tick cfg st =
  zipOverLedgerTablesTicked
    (flip rawApplyDiffs)
    ticked
    (projectLedgerTables st)
  where
    ticked = snd
           . tickLedgerState cfg
           . ForgeInUnknownSlot
           . forgetLedgerTables
           $ st

{-------------------------------------------------------------------------------
  SUT side
-------------------------------------------------------------------------------}

-- | The System Under Test
data SUT m blk =
  SUT
  !(Mempool m blk)
    -- ^ A Mempool
  !(StrictTVar m (MockedLedgerDB blk))
    -- ^ Emulates a ledger db to the extent needed by the ledger interface.
  deriving Generic

deriving instance ( NoThunks (Mempool m blk)
                  , NoThunks (StrictTVar m (MockedLedgerDB blk))
                  ) =>  NoThunks (SUT m blk)

-- | A very minimal mock of the ledger db.
--
-- The ledger interface will serve the values from this datatype.
data MockedLedgerDB blk = MockedLedgerDB {
    -- | The current LedgerDB tip
    ldbTip             :: !(LedgerState blk ValuesMK)
    -- | States which are still reachable in the LedgerDB
  , oldReachableTips   :: !(Set (LedgerState blk ValuesMK))
    -- | States which are no longer reachable in the LedgerDB
  , oldUnreachableTips :: !(Set (LedgerState blk ValuesMK))
  } deriving (Generic)

-- | Create a ledger interface and provide the tvar to modify it when switching
-- ledgers.
newLedgerInterface ::
  ( MonadSTM m
  , NoThunks (MockedLedgerDB blk)
  , GetTip (LedgerState blk ValuesMK)
  , LedgerSupportsMempool blk
  )
  => LedgerState blk ValuesMK
  -> m (LedgerInterface m blk, StrictTVar m (MockedLedgerDB blk))
newLedgerInterface initialLedger = do
  t <- newTVarIO $ MockedLedgerDB initialLedger Set.empty Set.empty
  pure $ (,t) $ LedgerInterface {
      getCurrentLedgerState = forgetLedgerTables . ldbTip <$> readTVar t
    , getLedgerTablesAtFor  = \pt txs -> do
        let keys = foldl' (zipLedgerTables (<>)) polyEmptyLedgerTables
                 $ map getTransactionKeySets txs
        MockedLedgerDB ti oldReachableTips _ <- atomically $ readTVar t
        if pt == castPoint (getTip ti) -- if asking for tables at the tip of the
                                       -- ledger db
        then
          let tbs = zipLedgerTables f keys $ projectLedgerTables ti
          in  pure $ Right $ tbs
        else case find ((castPoint pt ==). getTip) oldReachableTips of
           Nothing -> pure $ Left $ PointNotFound pt
           Just mtip ->
             if pt == castPoint (getTip mtip)
             -- if asking for tables at some still reachable state
             then
               let tbs = zipLedgerTables f keys $ projectLedgerTables mtip
               in  pure $ Right $ tbs
             else
               -- if asking for tables at other point or at the mempool tip but
               -- it is not reachable
               pure $ Left $ PointNotFound pt
    }
 where
   f :: Ord k => KeysMK k v -> ValuesMK k v -> ValuesMK k v
   f (ApplyKeysMK (DS.Keys s)) (ApplyValuesMK (DS.Values v)) =
      ApplyValuesMK (DS.Values (Map.restrictKeys v s))

-- | Make a SUT
mkSUT ::
  ( MonadSTM m
  , NoThunks (MockedLedgerDB blk)
  , GetTip (LedgerState blk ValuesMK)
  , IOLike m
  , LedgerSupportsProtocol blk
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> m (SUT m blk)
mkSUT cfg initialLedger = do
  (lif, t) <- newLedgerInterface initialLedger
  mempool <- openMempoolWithoutSyncThread
               lif
               cfg
               (MempoolCapacityBytesOverride txMaxBytes')
               nullTracer
               txInBlockSize
  pure (SUT mempool t)

semantics ::
     (MonadSTM m, LedgerSupportsMempool blk) =>
     Command blk Concrete
  -> StrictTVar m (SUT m blk)
  -> m (Response blk Concrete)
semantics cmd r = do
  SUT m t <- atomically $ readTVar r
  case cmd of
    Action (TryAddTxs txs) -> do
      void $ tryAddTxs m DoNotIntervene txs
      pure Void

    Action SyncLedger   -> do
     void $ syncWithLedger m
     pure Void

    Action GetSnapshot -> do
      snap <- atomically (getSnapshot m)
      pure
        $ GotSnapshot
        $ first txForgetValidated
        <$> snapshotTxs snap

    Event (ChangeLedger l' newReachable) ->
      atomically $ do
       MockedLedgerDB ledgerTip oldReachableTips oldUnreachableTips <- readTVar t
       if any ((getTip l' ==) . getTip)
              (Set.toList $ Set.union oldUnreachableTips oldReachableTips)
         || getTip l' == getTip ledgerTip
       then pure ()
       else
         let
           (newReachableTips, newUnreachableTips) =
             if keepsDB newReachable
              then (Set.insert ledgerTip oldReachableTips, oldUnreachableTips)
              else (Set.empty,
                      Set.insert ledgerTip
                    $ Set.union oldUnreachableTips oldReachableTips
                   )
         in
           writeTVar t (MockedLedgerDB l' newReachableTips newUnreachableTips)
       pure Void

{-------------------------------------------------------------------------------
  Conditions
-------------------------------------------------------------------------------}

precondition :: Model blk Symbolic -> Command blk Symbolic -> Logic
precondition _ _ = Top

postcondition ::
  ( LedgerSupportsMempool blk
  , Eq (GenTx blk)
  )
  => Model    blk Concrete
  -> Command  blk Concrete
  -> Response blk Concrete
  -> Logic
postcondition model (Action GetSnapshot) (GotSnapshot txs) =
  modelTxs model .== txs
postcondition _ _ _ = Top

noPostcondition ::
     Model    blk Concrete
  -> Command  blk Concrete
  -> Response blk Concrete
  -> Logic
noPostcondition _ _ _ = Top

shrinker :: Model blk Symbolic
         -> Command blk Symbolic
         -> [Command blk Symbolic]
shrinker _ (Action (TryAddTxs txs)) =
  Action . TryAddTxs <$> shrinkList shrinkNothing txs
shrinker _ _ = []

{-------------------------------------------------------------------------------
  State Machine
-------------------------------------------------------------------------------}

sm ::
  ( HasTxId (GenTx blk)
  , LedgerSupportsMempool blk
  , Eq (GenTx blk)
  , Arbitrary (LedgerState blk ValuesMK)
  , ToExpr (LedgerState blk ValuesMK)
  , ToExpr (GenTx blk)
  , Eq (TickedLedgerState blk ValuesMK)
  , LedgerSupportsProtocol blk
  , UnTick blk
  , IOLike m
  , NoThunks (Mempool m blk)
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> MakeAtomic
  -> (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk])
  -> m (StateMachine (Model blk) (Command blk) m (Response blk))
sm cfg initialState ma gTxs = do
  ior <- newTVarIO =<< mkSUT cfg initialState
  pure $ StateMachine {
      QC.initModel     = initModel cfg initialState
    , QC.transition    = transition cfg
    , QC.precondition  = precondition
    , QC.postcondition =
        case ma of
          NonAtomic -> noPostcondition
          Atomic    -> postcondition
          DontCare  -> postcondition
    , QC.invariant     = Nothing
    , QC.generator     = generator ma gTxs
    , QC.shrinker      = shrinker
    , QC.semantics     = \c -> semantics c ior
    , QC.mock          = mock
    , QC.cleanup       = noCleanup
    }

smUnused ::
  ( LedgerSupportsMempool blk
  , Eq (GenTx blk)
  , Arbitrary (LedgerState blk ValuesMK)
  , ToExpr (LedgerState blk ValuesMK)
  , ToExpr (GenTx blk)
  , Eq (TickedLedgerState blk ValuesMK)
  , LedgerSupportsProtocol blk
  , UnTick blk
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> MakeAtomic
  -> (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk])
  -> StateMachine (Model blk) (Command blk) IO (Response blk)
smUnused cfg initialState ma gTxs = StateMachine {
    QC.initModel     = initModel cfg initialState
  , QC.transition    = transition cfg
  , QC.precondition  = precondition
  , QC.postcondition =
        case ma of
          NonAtomic -> noPostcondition
          Atomic    -> postcondition
          DontCare  -> postcondition
  , QC.invariant     = Nothing
  , QC.generator     = generator ma gTxs
  , QC.shrinker      = shrinker
  , QC.semantics     = undefined
  , QC.mock          = mock
  , QC.cleanup       = noCleanup
  }

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

prop_mempoolSequential ::
  forall blk . ( HasTxId (GenTx blk)
  , LedgerSupportsMempool blk
  , Eq (GenTx blk)
  , Arbitrary (LedgerState blk ValuesMK)
  , ToExpr (LedgerState blk ValuesMK)
  , ToExpr (GenTx blk)
  , Eq (TickedLedgerState blk ValuesMK)
  , LedgerSupportsProtocol blk
  , ToExpr (TickedLedgerState blk ValuesMK)
  , UnTick blk
  , NoThunks (Mempool IO blk)
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
     -- ^ Initial state
  -> (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk])
     -- ^ Transaction generator
  -> Property
prop_mempoolSequential cfg initialState gTxs = forAllCommands smUnused' Nothing $
  \cmds -> monadicIO
    (do
        (hist, model, res) <- runCommandsWithSetup sm' cmds
        prettyCommands smUnused' hist
          $ checkCommandNames cmds
          $ tabulate "Command sequence length"
              [QC.lengthCommands cmds `bucketiseBy` 10]
          $ tabulate "Maximum ticket number"
              [(\(TicketNo t) -> t) (modelLastSeenTicketNo model) `bucketiseBy` 5]
          $ tabulate "Number of txs to add"
              [ length txs `bucketiseBy` 10
              | (_, Invocation (Action (TryAddTxs txs)) _) <- unHistory hist
              ]
          $ tabulate "Mempool was filled"
              [ show $ modelMempoolWasFilled model ]
          $ res === Ok
    )
  where
    smUnused' = smUnused cfg initialState DontCare gTxs
    sm'       = sm       cfg initialState DontCare gTxs

    bucketiseBy v n =
      let
        l = (v `div` n) * n
      in
        "[" <> show l <> "-" <> show (l + n) <> ")"

prop_mempoolParallel ::
  ( HasTxId (GenTx blk)
  , LedgerSupportsMempool blk
  , Eq (GenTx blk)
  , Arbitrary (LedgerState blk ValuesMK)
  , ToExpr (LedgerState blk ValuesMK)
  , ToExpr (GenTx blk)
  , Eq (TickedLedgerState blk ValuesMK)
  , LedgerSupportsProtocol blk
  , ToExpr (TickedLedgerState blk ValuesMK)
  , UnTick blk
  , NoThunks (Mempool IO blk)
  )
  => LedgerConfig blk
  -> LedgerState blk ValuesMK
  -> MakeAtomic
  -> (Int -> LedgerState blk ValuesMK -> Gen [GenTx blk])
  -> Property
prop_mempoolParallel cfg initialState ma gTxs = forAllParallelCommands smUnused' Nothing $
  \cmds -> monadicIO $ do
        res <- runParallelCommandsWithSetup sm' cmds
        prettyParallelCommandsWithOpts
          cmds
          (Just (GraphOptions "./mempoolParallel.png" Png))
          res
 where
   smUnused' = smUnused cfg initialState ma gTxs
   sm'       = sm       cfg initialState ma gTxs

-- | See 'MakeAtomic' on the reasoning behind having these tests.
tests :: TestTree
tests = testGroup "QSM"
        [ testProperty "sequential"
          $ withMaxSuccess 10000 $ prop_mempoolSequential testLedgerConfig testInitLedger
          $ \i -> fmap (fmap fst . fst) . genTxs i
        , testGroup "parallel"
          [ testProperty "atomic"
            $ withMaxSuccess 1000 $ prop_mempoolParallel testLedgerConfig testInitLedger Atomic
            $ \i -> fmap (fmap fst . fst) . genTxs i
          , testProperty "non atomic"
            $ withMaxSuccess 1000 $ prop_mempoolParallel testLedgerConfig testInitLedger NonAtomic
            $ \i -> fmap (fmap fst . fst) . genTxs i
          ]
        ]

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

-- | The 'TestBlock' txMaxBytes is fixed to a very high number. We use this
-- local declaration to have a mempool that sometimes fill but still don't make
-- it configurable.
txMaxBytes' :: MempoolCapacityBytes
txMaxBytes' = MempoolCapacityBytes 400

instance (StandardHash blk, GetTip (LedgerState blk ValuesMK)) =>
         Eq (LedgerState blk ValuesMK) where
  (==) = (==) `on` getTip

instance (StandardHash blk, GetTip (LedgerState blk ValuesMK)) =>
         Ord (LedgerState blk ValuesMK) where
  compare = compare `on` getTip

instance Eq (Validated (GenTx blk)) => Eq (TxSeq (Validated (GenTx blk))) where
  s1 == s2 = toList s1 == toList s2

instance NoThunks (Mempool IO TestBlock) where
  showTypeOf _  = showTypeOf (Proxy @(Mempool IO TestBlock))
  wNoThunks _ _ = return Nothing

instance Show (TxId (GenTx blk)) => ToExpr (TxId (GenTx blk)) where
  toExpr x = App (show x) []

instance ( ToExpr (TxId (GenTx blk))
         , ToExpr (GenTx blk)
         , ToExpr (LedgerState blk ValuesMK)
         , ToExpr (TickedLedgerState blk ValuesMK)
         , LedgerSupportsMempool blk
         ) => ToExpr (Model blk r) where

  toExpr model = Rec "Model" $ TD.fromList
    [ ("mempoolTip", toExpr $ modelMempoolIntermediateState model)
    , ("ledgerTip", toExpr $ modelLedgerDBTip model)
    , ("txs", toExpr $ modelTxs model)
    , ("capacity", toExpr $ getMempoolCapacityBytes $ modelRemainingCapacity model)
    , ("lastTicket", toExpr $ modelLastSeenTicketNo model)]

instance ( ToExpr (TxId (GenTx blk))
         , ToExpr (GenTx blk)
         , ToExpr (TickedLedgerState blk ValuesMK)
         , ToExpr (LedgerState blk ValuesMK)
         , LedgerSupportsMempool blk) => Show (Model blk r) where
  show = show . toExpr

instance ToExpr (GenTx blk) => ToExpr (Action blk r) where
  toExpr (TryAddTxs txs) = App "TryAddTxs" $ map toExpr txs
  toExpr SyncLedger      = App "SyncLedger" []
  toExpr GetSnapshot     = App "GetSnapshot" []

instance ToExpr (LedgerState blk ValuesMK) => ToExpr (Event blk r) where
  toExpr (ChangeLedger ls b) =
      Rec "ChangeLedger" $ TD.fromList [ ("tip",  toExpr ls)
                                       , ("newFork", toExpr b) ]

instance ( ToExpr (LedgerState blk ValuesMK)
         , ToExpr (GenTx blk)) => ToExpr (Command blk r) where
  toExpr (Action act) = toExpr act
  toExpr (Event ev)   = toExpr ev

instance ToExpr (Command blk r) => Show (Command blk r) where
  show = show . toExpr

instance ( ToExpr (GenTx blk)
         , LedgerSupportsMempool blk
         ) => ToExpr (TxTicket (Validated (GenTx blk))) where
  toExpr tkt =
    Rec "Ticket"
    $ TD.fromList [ ("number", toExpr $ txTicketNo tkt)
                  , ("tx", toExpr $ txForgetValidated $ txTicketTx tkt)
                  , ("size", toExpr $ txTicketTxSizeInBytes tkt)]

instance ( Show (ApplyTxErr blk)
         , ToExpr (GenTx blk)
         , LedgerSupportsMempool blk
         , ToExpr (Validated (GenTx blk))) => ToExpr (MempoolAddTxResult blk) where
  toExpr (MempoolTxAdded vtx)     = App "Added" [toExpr vtx]
  toExpr (MempoolTxRejected tx e) = App "Rejected" [toExpr tx, App (show e) [] ]

instance ( ToExpr (GenTx blk)
         , LedgerSupportsMempool blk) => ToExpr (Response blk r) where

  toExpr Void = App "Void" []
  toExpr (GotSnapshot s) =
    Rec "GotSnapshot" $
      TD.fromList [ ("txs", toExpr s) ]

instance ( ToExpr (GenTx blk)
         , LedgerSupportsMempool blk) => Show (Response blk r) where
  show = show . toExpr

deriving newtype instance ToExpr (TicketNo)

deriving instance NoThunks (LedgerState blk ValuesMK) => NoThunks (MockedLedgerDB blk)

instance Arbitrary (LedgerState TestBlock ValuesMK) where
  arbitrary = do
    n <- getPositive <$> arbitrary
    (txs, _) <- genValidTxs n testInitLedger
    case runExcept $ repeatedlyM (flip applyTxToLedger) txs testInitLedger of
      Left _   -> error "Must not happen"
      Right st -> pure st

instance ToExpr (TickedLedgerState TestBlock ValuesMK) where
   toExpr (TickedSimpleLedgerState st) = App "Ticked" [ toExpr st ]

instance ToExpr (LedgerState TestBlock ValuesMK) where
   toExpr (SimpleLedgerState st tbs) = Rec "LedgerState" $ TD.fromList
      [ ("state", toExpr $ mockTip st)
      , ("tables", toExpr tbs)]

deriving instance ToExpr (MockState TestBlock)
instance ToExpr (Point TestBlock) where
  toExpr p = App (show p) []

instance ToExpr (Hash SHA256 Tx) where
  toExpr s = App (show s) []

instance ToExpr Addr where
  toExpr a = App (show a) []

deriving instance ToExpr (GenTx TestBlock)
deriving instance ToExpr (Tx)
deriving instance ToExpr (Expiry)
deriving newtype instance ToExpr SlotNo

instance ToExpr (LedgerTables (LedgerState TestBlock) ValuesMK) where
  toExpr = genericToExpr

instance ToExpr (ValuesMK TxIn TxOut) where
  toExpr (ApplyValuesMK (DS.Values m)) = App "Values" [ toExpr m ]

class UnTick blk where
  unTick :: forall mk. TickedLedgerState blk mk ->  LedgerState blk mk

instance UnTick TestBlock where
  unTick = getTickedSimpleLedgerState

deriving instance NoThunks MempoolSize

deriving instance NoThunks (TraceEventMempool TestBlock)
