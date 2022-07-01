{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans            #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Having a useful block consists of a bunch of instances and datatypes. In
-- particular, a useful block (@blk@) should have:
--
-- (1) Header:
--
--     The header identifies the block in the chain, references the parent
--     block, and carries the signature of the block.
--
--     * Declared by @data instance Header blk@
--     * Accessed by @instance GetHeader blk@
--     * Abstract interface through @instance HasHeader blk@
--
-- (2) Body:
--
--     The body contains the transactions that were included in this block.
--
--     Can have a payload or not, depending on whether we only need a
--     sequence of possibly empty blocks.
--
--     A special case is the EBBs, which are boundary blocks that share the
--     block number with their predecesor and do not contain payloads.
--
-- (3) Ledger state:
--
--     The ledger state is what the state we carry when we want to run blocks.
--     If the blocks don't contain transactions/payloads then this ledger state
--     might be a degenerate case where applying a block is practically trivial,
--     but otherwise it has to keep track of some state that evolves by applying
--     the block.
--
--     * Declared by @data instance LedgerState blk mk@
--     * Also in its ticked variant by @data instance Ticked1 (LedgerState blk) mk@
--
-- (4) Header hash:
--
--     In order to identify blocks, we refer to them by their header hash.
--
--     * Declared by @type instance HeaderHash blk@
--     * To be useful usually needs @instance StandardHash blk@
--     * To access the parent's hash @instance GetPrevHash blk@
--
-- (5) Point and tip:
--
--     The typeclass @GetTip@ provide means for extracting a @Point@ from a
--     header\/ledger\/block. The @HasAnnTip@ typeclass provides @AnnTip@s and means to
--     extract them from the header of a block.
--
-- (6) Signatures:
--
--     Some constructions that only depend on the header to extract the
--     signature because one should be able to identify whether a block is not
--     forged by the wrong person just by the header.
--
--     * @type family Signed hdr@
--     * @instance SignedHeader hdr@
--
-- (7) Header Validation:
--
--     * @instance BasicEnvelopeValidation blk@ provides very basic validation
--       (first slot and block numbers, and expected next block and slot
--       numbers)
--     * @instance ValidateEnvelope blk@ provides further validation checks.
--
-- (8) Configurations:
--
--     * @type instance LedgerCfg (LedgerState blk)@
--     * @data instance BlockConfig blk@
--     * @data instance CodecConfig blk@
--     * @data instance StorageConfig blk@
--     * @instance ConfigSupportsNode blk@
--
-- (9) UTxO-HD related:
--
--     UTxO-HD introduces the 'LedgerTables' and related classes that allow to
--     store the UTxO in a backend (possibly on disk) and carry a sequence of
--     differences instead.
--
--     * @instance TableStuff@
--     * @instance TickedTableStuff@
--     * @instance StowableLedgerTables@
--     * @instance SufficientSerializationForAnyBackingStore@
--     * If applicable because the block has no tables, @instance InMemory@
--     * @instance ReadsKeySets m (LedgerState blk)@
--
-- (10) Ticking:
--
--     The Ledger requires ticking in order to advance time. In particular, when
--     we apply a block to a ledger state, we tick the ledger state to the slot
--     of the block and then apply the transactions.
--
--     Requires an instance for @IsLedger (LedgerState blk)@
--
-- (11) Applying blocks:
--
--     Applying blocks means first ticking the ledger state to the slot of the
--     block and then applying the transactions in the block body one by one.
--     This is provided by @ApplyBlock@, but as our blocks can have different
--     types of payloads, the @PayloadSemantics@ class abstracts the step of
--     "applying a payload" to the ledger state.
--
--     * @instance PayloadSemantics@
--     * @instance ApplyBlock@
--     * Trivially we have an instance for @UpdateLedger@
--
--
-- (12) Applying transactions:
--
--      The typeclass @LedgerSupportsMempool@ specifies how to apply or reapply
--      transactions as well as how to extract @KeysMK@ from a transaction to
--      later query the UTxO-HD functions.
--
-- (13) Inspecting the ledger: compares two ledger states to extract events via
--      @InspectLedger blk@
--
-- (14) Network:
--
--     * @instance HasNetworkProtocolVersion@
--
-- (15) Consensus protocol:
--
--     Chains of blocks are resolved via the Consensus protocols. A Consensus
--     protocol is something that is an instance of @ConsensusProtocol@
--     typeclass, which defines @LedgerView\/SelectView\/ValidateView@.
--
--     * @type instance BlockProtocol blk@
--     * @instance BlockSupportsProtocol blk@ defines functions for retrieving:
--
--         * @type family ValidateView (BlockProtocol blk)@
--         * @type family SelectView (BlockProtocol blk)@
--
--     * @instance LedgerSupportsProtocol blk@ defines ledger view and forecasting:
--
--         * @type family LedgerView (BlockProtocol blk)@
--
-- (16) Hard fork:
--
--     The hard fork is informed on when to perform transitions via the
--     @HasHardForkHistory@ typeclass. For testing this will probably be
--     trivially empty.
--
-- (17) Serialization:
--
--     Again mostly related to when we have several block types, the
--     @NestedCtxt_@ data family is used to identify what kind of block we are
--     deserializing. If we have just one block this is trivial.
--
module Test.Util.TheTestBlock (
    NaturalBlock
  , PayloadSemantics(..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.Foldable (Foldable (toList))
import           Data.Hashable (Hashable)
import           Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.TreeDiff (Expr (..), ToExpr (..), genericToExpr)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))

import           Ouroboros.Consensus.Block.Abstract 
import           Ouroboros.Consensus.Ledger.Basics (
                     ApplyMapKind' (..), DiffMK, EmptyMK, InMemory (..), KeysMK,
                     LedgerState, MapKind, NameMK (NameMK), SeqDiffMK,
                     ShowLedgerState (..), StowableLedgerTables (..),
                     SufficientSerializationForAnyBackingStore (..),
                     TableStuff (..), TickedTableStuff (..), TrackingMK,
                     ValuesMK, emptyLedgerTables, isCandidateForUnstowDefault,
                     polyEmptyLedgerTables, rawCalculateDifference,
                     showsApplyMapKind, IsLedger(..), LedgerResult (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes)
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import           Ouroboros.Consensus.Ticked (Ticked1)
import           Ouroboros.Consensus.Util.Condense (Condense)

import           Test.Util.Orphans.ToExpr ()
import Data.Maybe (isJust)
import Ouroboros.Consensus.Ledger.Abstract (ApplyBlock(..), GetTip (..), LedgerCfg)
import Cardano.Slotting.Slot
import Ouroboros.Network.Point (Block(..))
import Ouroboros.Network.Block (Point(..))
-- import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
-- import Ouroboros.Consensus.Block (BlockSupportsProtocol, NestedCtxt (NestedCtxt))
-- import Ouroboros.Consensus.HeaderValidation (BasicEnvelopeValidation)

{-------------------------------------------------------------------------------
  TestBlock
-------------------------------------------------------------------------------}

class GetBodyHash b where
  getBodyHash :: b -> TestBodyHash

data TrivialHeader = TrivialHeader { 
      thHash        :: HeaderHash TrivialHeader
    , thPrevHash    :: !(ChainHash TrivialHeader)
    , thBodyHash    :: !TestBodyHash
    , thSlotNo      :: !SlotNo
    , thBlockNo     :: !BlockNo
    , thChainLength :: !ChainLength
    , thForkNo      :: !Word
    } deriving (Generic)
      deriving (Eq)

type instance HeaderHash TrivialHeader = TestHeaderHash
instance StandardHash TrivialHeader
newtype instance Header TrivialHeader = TrivialHeader' { trivialHeader :: TrivialHeader }
instance HasHeader TrivialHeader where
  getHeaderFields = getBlockHeaderFields
instance HasHeader (Header TrivialHeader) where
  getHeaderFields (TrivialHeader' TrivialHeader{..}) = HeaderFields thSlotNo thBlockNo thHash
instance GetHeader TrivialHeader where
  getHeader = TrivialHeader'
  blockMatchesHeader _ _ = error "Header-only instance, should never be called"
  headerIsEBB _ = Nothing
instance GetPrevHash TrivialHeader where
  headerPrevHash = thPrevHash . trivialHeader
instance GetBodyHash TrivialHeader where
  getBodyHash = thBodyHash
instance GetTip TrivialHeader where
  getTip l = Point $ At (Block (thSlotNo l) (thHash l))
  
data WithEBBInfo header = WithEBBInfo {
    weiHeader :: header
  , weiEpochBoundary :: Maybe EpochNo
  } deriving (Generic)
    deriving (Eq)

type instance HeaderHash (WithEBBInfo header) = HeaderHash header
instance StandardHash header => StandardHash (WithEBBInfo header)
newtype instance Header (WithEBBInfo header) = WithEBBInfo' { weiHeader' :: WithEBBInfo header }
instance HasHeader header => HasHeader (WithEBBInfo header) where
  getHeaderFields = getBlockHeaderFields
instance HasHeader header => HasHeader (Header (WithEBBInfo header)) where
  getHeaderFields = castHeaderFields . getHeaderFields . weiHeader . weiHeader'
instance HasHeader header => GetHeader (WithEBBInfo header) where
  getHeader = WithEBBInfo'
  blockMatchesHeader _ _ = error "Header-only instance, should never be called"
  headerIsEBB = weiEpochBoundary . weiHeader'
instance GetBodyHash header => GetBodyHash (WithEBBInfo header) where
  getBodyHash = getBodyHash . weiHeader 
instance GetPrevHash header => GetPrevHash (WithEBBInfo header) where
  headerPrevHash = castChainHash . headerPrevHash . getHeader . weiHeader . weiHeader' 
instance GetTip header => GetTip (WithEBBInfo header) where
  getTip = castPoint . getTip . weiHeader

castChainHash :: HeaderHash h ~ HeaderHash h' => ChainHash h -> ChainHash h'
castChainHash GenesisHash = GenesisHash
castChainHash (BlockHash h) = BlockHash h

data WithBlockBody header body = WithBlockBody {
    wbbHeader :: header
  , wbbBody :: body
  } deriving (Generic)
    deriving (Eq)

type instance HeaderHash (WithBlockBody header body) = HeaderHash header
instance StandardHash header => StandardHash (WithBlockBody header body)
newtype instance Header (WithBlockBody header body) = WithBlockBodyHeader { wbbhHeader :: header }
instance ( GetBodyHash header
         , GetHeader header
         , HasHeader header
         , Typeable body
         ) => HasHeader (WithBlockBody header body) where
  getHeaderFields = getBlockHeaderFields
instance ( GetBodyHash header
         , GetHeader header
         , HasHeader header
         , Typeable body
         ) => HasHeader (Header (WithBlockBody header body)) where
  getHeaderFields = castHeaderFields . getHeaderFields . getHeader . wbbhHeader
instance ( GetBodyHash header
         , GetHeader header
         , HasHeader header
         , Typeable body
         ) => GetHeader (WithBlockBody header body) where
  getHeader = WithBlockBodyHeader . wbbHeader
  blockMatchesHeader hdr blk = getBodyHash (wbbhHeader hdr) == undefined -- hash (wbbBody blk)
  headerIsEBB = headerIsEBB . getHeader . wbbhHeader
instance ( GetBodyHash header
         , GetPrevHash header
         , Typeable body
         ) => GetPrevHash (WithBlockBody header body) where
  headerPrevHash = castChainHash . headerPrevHash . getHeader . wbbhHeader
instance GetTip header => GetTip (WithBlockBody header body) where
  getTip = castPoint . getTip . wbbHeader
  
data WithValidity body = WithValidity {
    wvValid :: Validity
  , wvBody :: body
}

type EBBBlocksWithBody payload = WithBlockBody (WithEBBInfo TrivialHeader) (WithValidity payload)
type EBBBlocksWithoutBody = WithEBBInfo TrivialHeader
type NonEBBBlocksWithBody payload = WithBlockBody TrivialHeader (WithValidity payload)
type NonEBBBlocksWithoutBody = TrivialHeader

----------------------------------------------------------------------------------------------------

newtype instance LedgerState TrivialHeader mk = TrivialLedgerState {
  lastAppliedPoint :: Point TrivialHeader
  } deriving (Generic, Eq, Show)
    deriving anyclass (NoThunks)

instance GetTip (LedgerState TrivialHeader mk) where
  getTip = castPoint . lastAppliedPoint

type instance LedgerCfg (LedgerState TrivialHeader) = ()

newtype instance Ticked1 (LedgerState TrivialHeader) mk = TickedTrivialLedgerState (LedgerState TrivialHeader mk)

instance GetTip (Ticked1 (LedgerState TrivialHeader) mk) where
  getTip (TickedTrivialLedgerState l) = castPoint . lastAppliedPoint $ l

instance ShowLedgerState (LedgerTables (LedgerState TrivialHeader)) where
  showsLedgerState _sing = shows
instance ShowLedgerState (LedgerState TrivialHeader) where
  showsLedgerState _sing = shows  

instance InMemory (LedgerState TrivialHeader) where
  convertMapKind (TrivialLedgerState l) = TrivialLedgerState l

instance IsLedger (LedgerState TrivialHeader) where
  type AuxLedgerEvent (LedgerState TrivialHeader) = ()
  type LedgerErr (LedgerState TrivialHeader) = ()

  applyChainTickLedgerResult _cfg _slot = LedgerResult [] . TickedTrivialLedgerState . convertMapKind

instance StowableLedgerTables (LedgerState TrivialHeader) where
  stowLedgerTables = convertMapKind
  unstowLedgerTables = convertMapKind
  isCandidateForUnstow = isCandidateForUnstowDefault

instance TableStuff (LedgerState TrivialHeader) where
  data LedgerTables (LedgerState TrivialHeader) mk = NoLedgerTables
    deriving stock    (Generic, Eq, Show)
    deriving anyclass (NoThunks)

  projectLedgerTables _                  = NoLedgerTables
  withLedgerTables st NoLedgerTables = convertMapKind st

  pureLedgerTables     _                                              = NoLedgerTables
  mapLedgerTables      _                               NoLedgerTables = NoLedgerTables
  traverseLedgerTables _                               NoLedgerTables = pure NoLedgerTables
  zipLedgerTables      _                NoLedgerTables NoLedgerTables = NoLedgerTables
  zipLedgerTables2     _ NoLedgerTables NoLedgerTables NoLedgerTables = NoLedgerTables
  zipLedgerTablesA     _                NoLedgerTables NoLedgerTables = pure NoLedgerTables
  zipLedgerTables2A    _ NoLedgerTables NoLedgerTables NoLedgerTables = pure NoLedgerTables
  foldLedgerTables     _                               NoLedgerTables = mempty
  foldLedgerTables2    _                NoLedgerTables NoLedgerTables = mempty
  namesLedgerTables                                                   = NoLedgerTables

instance ApplyBlock (LedgerState TrivialHeader) TrivialHeader where
  applyBlockLedgerResult _cfg blk _l = return $ LedgerResult [] (TrivialLedgerState $ getTip blk)
  reapplyBlockLedgerResult _cfg blk _l = LedgerResult [] (TrivialLedgerState $ getTip blk)
  getBlockKeySets = const polyEmptyLedgerTables

---------------------------------------------------------------------------

newtype instance LedgerState (WithEBBInfo hdr) mk = WithEBBInfoLedgerState (LedgerState hdr mk)
newtype instance Ticked1 (LedgerState (WithEBBInfo hdr)) mk = TickedWithEBBInfoLedgerState (LedgerState (WithEBBInfo hdr) mk)

data instance LedgerState (WithBlockBody hdr bdy) mk = WithBlockBodyLedgerState {
    headerLedgerState :: LedgerState hdr mk
  , bodyLedgerState :: PayloadDependentState bdy mk
  }
newtype instance Ticked1 (LedgerState (WithBlockBody hdr bdy)) mk = TickedWithBlockBodyLedgerState (LedgerState (WithBlockBody hdr bdy) mk)



----------------------------------------------------------------------------------------------------

-- | A Test block is just a header and a body
type TheTestBlock :: IsEBB Type -> Type
data TheTestBlock  e = TheTestBlock { -- proto!!
      testHeader :: !(Header   (TheTestBlock e))
    , testBody   :: !(TestBody (TheTestBlock e))
    }
    deriving (Generic)

-- | A Tag to identify an EBB or a regular block. Intended to be used with DataKinds
data IsEBB ptype = EBB | Regular ptype

-- | A NaturalBlock (as nature intended) is a normal block
newtype NaturalBlock ptype = NaturalBlock (TheTestBlock (Regular ptype))
  deriving (Generic)

-- | For tests that have to do with EBBs (poor souls), this type provides an
-- Either-like structure.
data TestBlockWithEBBs ptype =
    TestRegularBlock (NaturalBlock ptype)
  | TestEBBlock      (TheTestBlock EBB)
  deriving (Generic)

-- | The header parametrized by the type of block
-- data instance Header (TheTestBlock (Regular ptype)) = TestRegularHeader {
--       thHash        :: HeaderHash (Header (TheTestBlock (Regular ptype)))
--     , thPrevHash    :: !(ChainHash (Header (TheTestBlock (Regular ptype))))
--     , thBodyHash    :: !TestBodyHash
--     , thSlotNo      :: !SlotNo
--     , thBlockNo     :: !BlockNo
--     , thChainLength :: !ChainLength
--     , thForkNo      :: !Word
--     } deriving (Generic)
data instance Header (TheTestBlock  EBB           ) = TestEBBHeader {
    regularHeader :: Header (TheTestBlock (Regular ()))
  , epochBoundary :: EpochNo
  } deriving (Generic)

type family TestBody blk where
  TestBody (TheTestBlock (Regular ptype)) = TestRegularBody ptype
  TestBody (TheTestBlock  EBB           ) = ()


-- | A Body
data TestRegularBody ptype = TestRegularBody {
    tbValid   :: !Validity
  , tbPayload :: !ptype
  } deriving (Generic)

type instance HeaderHash (TheTestBlock e) = TestHeaderHash

newtype TestHeaderHash = TestHeaderHash Int
  deriving stock    (Generic)
  deriving newtype  (Eq, Ord, Serialise, ToExpr, Show)
  deriving anyclass (NoThunks)

newtype ChainLength = ChainLength Int
  deriving stock   (Show, Generic)
  deriving newtype (Eq, Ord, Enum, NoThunks, Serialise, Hashable)

newtype TestBodyHash = TestBodyHash Int
  deriving stock    (Eq, Ord, Show, Generic)
  deriving newtype  (Condense, NoThunks, Hashable, Serialise)

newtype TestHash = UnsafeTestHash {
      unTestHash :: NE.NonEmpty Word64
    }
  deriving stock    (Generic)
  deriving newtype  (Eq, Ord, Serialise, ToExpr)
  deriving anyclass (NoThunks)

data Validity = Valid | Invalid
  deriving stock    (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Serialise, NoThunks, ToExpr)

instance StandardHash (TheTestBlock e)

{-------------------------------------------------------------------------------
  BlockConfig
-------------------------------------------------------------------------------}

newtype instance BlockConfig (NaturalBlock ptype) = NaturalTestBlockConfig {
    -- | Number of core nodes
    --
    -- We need this in order to compute the 'ValidateView', which must
    -- conjure up a validation key out of thin air
    testBlockNumCoreNodes :: NumCoreNodes
  } deriving (Generic)

data instance BlockConfig (TestBlockWithEBBs ptype) = TestBlockConfig {
  -- | Whether the test block can be EBBs or not. This can vary per test
  -- case. It will be used by 'validateEnvelope' to forbid EBBs 'False'.
    testBlockEBBsAllowed  :: !Bool
  , naturalConfig         :: !(BlockConfig (NaturalBlock ptype))
  } deriving (Generic)

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

data instance LedgerState (TheTestBlock (Regular ptype)) mk =
      TestLedger {
        -- -- | The ledger state simply consists of the last applied block
      --   lastAppliedPoint      :: Point (TheTestBlock (Regular ptype))
      --   -- | State that depends on the application of the block payload to the
      --   -- state.
      -- , 
        payloadDependentState :: PayloadDependentState ptype mk
      } deriving (Generic)

deriving instance Eq (PayloadDependentState ptype EmptyMK) => Eq (LedgerState (TheTestBlock (Regular ptype)) EmptyMK)

instance ShowLedgerState (LedgerTables (LedgerState (TheTestBlock e))) where
  showsLedgerState _sing = undefined

instance ShowLedgerState (LedgerState (TheTestBlock e)) where
  showsLedgerState _sing = undefined

-- | The 'TestBlock' does not need any codec config
data instance CodecConfig (TheTestBlock e) = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | The 'TestBlock' does not need any storage config
data instance StorageConfig (TheTestBlock e) = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

newtype instance Ticked1 (LedgerState (TheTestBlock ptype)) mk = TickedTestLedger {
      getTickedTestLedger :: LedgerState (TheTestBlock ptype) mk
    }

{-------------------------------------------------------------------------------
  PayloadSemantics
-------------------------------------------------------------------------------}

class ( Typeable ptype
      , Eq       ptype
      , NoThunks ptype

      , Eq        (PayloadDependentState ptype EmptyMK)
      , Eq        (PayloadDependentState ptype DiffMK)
      , Eq        (PayloadDependentState ptype ValuesMK)

      , forall mk'. Show (PayloadDependentState ptype (ApplyMapKind' mk'))

      , forall mk. Generic   (PayloadDependentState ptype mk)
      ,            Serialise (PayloadDependentState ptype EmptyMK)

      , NoThunks  (PayloadDependentState ptype EmptyMK)
      , NoThunks  (PayloadDependentState ptype ValuesMK)
      , NoThunks  (PayloadDependentState ptype SeqDiffMK)
      , NoThunks  (PayloadDependentState ptype DiffMK)

      , TickedTableStuff     (LedgerState (TheTestBlock (Regular ptype)))
      , StowableLedgerTables (LedgerState (TheTestBlock (Regular ptype)))
      , ShowLedgerState      (LedgerState (TheTestBlock (Regular ptype)))

      , NoThunks (LedgerTables (LedgerState (TheTestBlock (Regular ptype))) SeqDiffMK)
      , NoThunks (LedgerTables (LedgerState (TheTestBlock (Regular ptype))) ValuesMK)

      , Eq        (PayloadDependentError ptype)
      , Show      (PayloadDependentError ptype)
      , Generic   (PayloadDependentError ptype)
      , ToExpr    (PayloadDependentError ptype)
      , Serialise (PayloadDependentError ptype)
      , NoThunks  (PayloadDependentError ptype)

      , NoThunks (CodecConfig (TheTestBlock (Regular ptype)))

      , NoThunks (StorageConfig (TheTestBlock (Regular ptype)))
      ) => PayloadSemantics ptype where

  data PayloadDependentState ptype (mk :: MapKind) :: Type

  type PayloadDependentError ptype :: Type

  applyPayload ::
       PayloadDependentState ptype ValuesMK
    -> ptype
    -> Either (PayloadDependentError ptype) (PayloadDependentState ptype TrackingMK)

  -- | This function is used to implement the 'getBlockKeySets' function of the
  -- 'ApplyBlock' class. Thus we assume that the payload contains all the
  -- information needed to determine which keys should be retrieved from the
  -- backing store to apply a 'TestBlockWith'.
  getPayloadKeySets :: ptype -> LedgerTables (LedgerState (TheTestBlock (Regular ptype))) KeysMK

{-------------------------------------------------------------------------------
  LedgerTables for blocks without tables
-------------------------------------------------------------------------------}

-- type BlockWithoutPayload = TheTestBlock (Regular ())

-- instance InMemory (LedgerState BlockWithoutPayload) where
--   convertMapKind TestLedger {lastAppliedPoint} = TestLedger lastAppliedPoint EmptyPLDS

-- instance StowableLedgerTables (LedgerState BlockWithoutPayload) where
--   stowLedgerTables   (TestLedger p EmptyPLDS) = TestLedger p EmptyPLDS
--   unstowLedgerTables (TestLedger p EmptyPLDS) = TestLedger p EmptyPLDS
--   isCandidateForUnstow = isCandidateForUnstowDefault

-- instance TickedTableStuff (LedgerState BlockWithoutPayload) where
--   projectLedgerTablesTicked _                         = NoTestLedgerTables
--   withLedgerTablesTicked (TickedTestLedger st) tables =
--       TickedTestLedger $ withLedgerTables st tables

-- instance SufficientSerializationForAnyBackingStore (LedgerState BlockWithoutPayload) where
--     codecLedgerTables = NoTestLedgerTables

-- instance TableStuff (LedgerState BlockWithoutPayload) where
--   data LedgerTables (LedgerState BlockWithoutPayload) mk = NoTestLedgerTables
--     deriving stock    (Generic, Eq, Show)
--     deriving anyclass (NoThunks)

--   projectLedgerTables _                  = NoTestLedgerTables
--   withLedgerTables st NoTestLedgerTables = convertMapKind st

--   pureLedgerTables     _                                                          = NoTestLedgerTables
--   mapLedgerTables      _                                       NoTestLedgerTables = NoTestLedgerTables
--   traverseLedgerTables _                                       NoTestLedgerTables = pure NoTestLedgerTables
--   zipLedgerTables      _                    NoTestLedgerTables NoTestLedgerTables = NoTestLedgerTables
--   zipLedgerTables2     _ NoTestLedgerTables NoTestLedgerTables NoTestLedgerTables = NoTestLedgerTables
--   zipLedgerTablesA     _                    NoTestLedgerTables NoTestLedgerTables = pure NoTestLedgerTables
--   zipLedgerTables2A    _ NoTestLedgerTables NoTestLedgerTables NoTestLedgerTables = pure NoTestLedgerTables
--   foldLedgerTables     _                                       NoTestLedgerTables = mempty
--   foldLedgerTables2    _                    NoTestLedgerTables NoTestLedgerTables = mempty
--   namesLedgerTables                                                               = NoTestLedgerTables

-- {-------------------------------------------------------------------------------
--   LedgerTables for blocks with tables
-- -------------------------------------------------------------------------------}

-- type BlockWithPayload = TheTestBlock (Regular Tx)

-- instance TableStuff (LedgerState BlockWithPayload) where
--   newtype LedgerTables (LedgerState BlockWithPayload) mk =
--     TokenToTValue { testUtxtokTable :: mk Token TValue }
--     deriving stock (Generic)

--   projectLedgerTables st       = utxtoktables $ payloadDependentState st
--   withLedgerTables    st table = st { payloadDependentState =
--                                         (payloadDependentState st) {utxtoktables = table}
--                                     }

--   pureLedgerTables = TokenToTValue

--   mapLedgerTables      f                                     (TokenToTValue x) = TokenToTValue    (f x)
--   traverseLedgerTables f                                     (TokenToTValue x) = TokenToTValue <$> f x
--   zipLedgerTables      f                   (TokenToTValue x) (TokenToTValue y) = TokenToTValue    (f x y)
--   zipLedgerTables2     f (TokenToTValue x) (TokenToTValue y) (TokenToTValue z) = TokenToTValue    (f x y z)
--   zipLedgerTablesA     f                   (TokenToTValue x) (TokenToTValue y) = TokenToTValue <$> f x y
--   zipLedgerTables2A    f (TokenToTValue x) (TokenToTValue y) (TokenToTValue z) = TokenToTValue <$> f x y z
--   foldLedgerTables     f                                     (TokenToTValue x) =                   f x
--   foldLedgerTables2    f                   (TokenToTValue x) (TokenToTValue y) =                   f x y
--   namesLedgerTables                                                            = TokenToTValue $ NameMK "testblocktables"

-- deriving newtype  instance Eq       (LedgerTables (LedgerState BlockWithPayload) EmptyMK)
-- deriving newtype  instance Eq       (LedgerTables (LedgerState BlockWithPayload) DiffMK)
-- deriving newtype  instance Eq       (LedgerTables (LedgerState BlockWithPayload) ValuesMK)
-- deriving newtype  instance Eq       (LedgerTables (LedgerState BlockWithPayload) SeqDiffMK)
-- deriving newtype  instance Show     (LedgerTables (LedgerState BlockWithPayload) (ApplyMapKind' mk))
-- deriving anyclass instance ToExpr   (LedgerTables (LedgerState BlockWithPayload) ValuesMK)
-- deriving anyclass instance NoThunks (LedgerTables (LedgerState BlockWithPayload) EmptyMK)
-- deriving anyclass instance NoThunks (LedgerTables (LedgerState BlockWithPayload) ValuesMK)
-- deriving anyclass instance NoThunks (LedgerTables (LedgerState BlockWithPayload) DiffMK)
-- deriving anyclass instance NoThunks (LedgerTables (LedgerState BlockWithPayload) SeqDiffMK)

-- instance StowableLedgerTables (LedgerState BlockWithPayload) where
--   stowLedgerTables   (TestLedger lap (UTxTok tbs h _  ))  = TestLedger lap (UTxTok emptyLedgerTables h tbs)
--   unstowLedgerTables (TestLedger lap (UTxTok _   h sto))  = TestLedger lap (UTxTok sto h polyEmptyLedgerTables)
--   isCandidateForUnstow = const True

-- instance TickedTableStuff (LedgerState BlockWithPayload) where
--   projectLedgerTablesTicked (TickedTestLedger st)        = projectLedgerTables st
--   withLedgerTablesTicked    (TickedTestLedger st) tables =
--     TickedTestLedger $ withLedgerTables st tables

-- {-------------------------------------------------------------------------------
--   PayloadSemantics for block without tables
-- -------------------------------------------------------------------------------}

-- instance PayloadSemantics () where
--   data PayloadDependentState () mk = EmptyPLDS
--     deriving stock (Eq, Show, Generic)
--     deriving anyclass (Serialise, NoThunks)

--   type PayloadDependentError () = ()

--   applyPayload _ _ = Right EmptyPLDS

--   getPayloadKeySets = const NoTestLedgerTables

-- {-------------------------------------------------------------------------------
--   PayloadSemantics for block with tables
-- -------------------------------------------------------------------------------}

-- -- | Mock of a UTxO transaction where exactly one (transaction) input is
-- -- consumed and exactly one output is produced.
-- --
-- data Tx = Tx {
--     -- | Input that the transaction consumes.
--     consumed :: Token
--     -- | Ouptupt that the transaction produces.
--   , produced :: (Token, TValue)
--   }
--   deriving stock (Show, Eq, Ord, Generic)
--   deriving anyclass (Serialise, NoThunks)

-- -- | A token is an identifier for the values produced and consumed by the
-- -- 'TestBlock' transactions.
-- --
-- -- This is analogous to @TxId@: it's how we identify what's in the table. It's
-- -- also analogous to @TxIn@, since we trivially only have one output per 'Tx'.
-- newtype Token = Token { unToken :: Point BlockWithPayload }
--   deriving stock (Show, Eq, Ord, Generic)
--   deriving newtype (Serialise, NoThunks, ToExpr)

-- -- | Unit of value associated with the output produced by a transaction.
-- --
-- -- This is analogous to @TxOut@: it's what the table maps 'Token's to.
-- newtype TValue = TValue (WithOrigin SlotNo)
--   deriving stock (Show, Eq, Ord, Generic)
--   deriving newtype (Serialise, NoThunks, ToExpr)

-- data TxErr
--   = TokenWasAlreadyCreated Token
--   | TokenDoesNotExist      Token
--   deriving stock (Generic, Eq, Show)
--   deriving anyclass (NoThunks, Serialise, ToExpr)

-- instance PayloadSemantics Tx where
--   data PayloadDependentState Tx mk =
--     UTxTok { utxtoktables :: LedgerTables (LedgerState BlockWithPayload) mk
--              -- | All the tokens that ever existed. We use this to
--              -- make sure a token is not created more than once. See
--              -- the definition of 'applyPayload' in the
--              -- 'PayloadSemantics' of 'Tx'.
--            , utxhist      :: Set.Set Token
--            , stowed       :: LedgerTables (LedgerState BlockWithPayload) ValuesMK
--            }
--     deriving stock    (Generic)

--   type PayloadDependentError Tx = TxErr

--   -- We need to exercise the HD backend. This requires that we store key-values
--   -- ledger tables and the block application semantics satisfy:
--   --
--   -- * a key is deleted at most once
--   -- * a key is inserted at most once
--   --
--   applyPayload st Tx{consumed, produced} =
--       fmap track $ delete consumed st >>= uncurry insert produced
--     where
--       insert ::
--            Token
--         -> TValue
--         -> PayloadDependentState Tx ValuesMK
--         -> Either TxErr (PayloadDependentState Tx ValuesMK)
--       insert tok val st'@UTxTok{utxtoktables, utxhist} =
--           if tok `Set.member` utxhist
--           then Left  $ TokenWasAlreadyCreated tok
--           else Right $ st' { utxtoktables = Map.insert tok val `onValues` utxtoktables
--                            , utxhist      = Set.insert tok utxhist
--                            }
--       delete ::
--            Token
--         -> PayloadDependentState Tx ValuesMK
--         -> Either TxErr (PayloadDependentState Tx ValuesMK)
--       delete tok st'@UTxTok{utxtoktables} =
--           if Map.member tok `queryKeys` utxtoktables
--           then Right $ st' { utxtoktables = Map.delete tok `onValues` utxtoktables
--                            }
--           else Left  $ TokenDoesNotExist tok

--       track :: PayloadDependentState Tx ValuesMK -> PayloadDependentState Tx TrackingMK
--       track stAfter =
--           stAfter { utxtoktables =
--                       TokenToTValue $ rawCalculateDifference utxtokBefore utxtokAfter
--                   }
--         where
--           utxtokBefore = testUtxtokTable $ utxtoktables st
--           utxtokAfter  = testUtxtokTable $ utxtoktables stAfter

--   getPayloadKeySets Tx{consumed} =
--     TokenToTValue $ ApplyKeysMK $ HD.UtxoKeys $ Set.singleton consumed

-- deriving stock    instance (Eq        (PayloadDependentState Tx EmptyMK))
-- deriving stock    instance (Eq        (PayloadDependentState Tx DiffMK))
-- deriving stock    instance (Eq        (PayloadDependentState Tx ValuesMK))
-- deriving stock    instance (Show      (PayloadDependentState Tx (ApplyMapKind' mk)))
-- deriving anyclass instance (Serialise (PayloadDependentState Tx EmptyMK))
-- deriving anyclass instance (ToExpr    (PayloadDependentState Tx ValuesMK))
-- deriving anyclass instance (NoThunks  (PayloadDependentState Tx EmptyMK))
-- deriving anyclass instance (NoThunks  (PayloadDependentState Tx DiffMK))
-- deriving anyclass instance (NoThunks  (PayloadDependentState Tx ValuesMK))
-- deriving anyclass instance (NoThunks  (PayloadDependentState Tx SeqDiffMK))

-- onValues ::
--      (Map.Map Token TValue -> Map.Map Token TValue)
--   -> LedgerTables (LedgerState BlockWithPayload) ValuesMK
--   -> LedgerTables (LedgerState BlockWithPayload) ValuesMK
-- onValues f TokenToTValue {testUtxtokTable} = TokenToTValue $ updateMap testUtxtokTable
--   where
--     updateMap :: ValuesMK Token TValue -> ValuesMK Token TValue
--     updateMap (ApplyValuesMK (HD.UtxoValues utxovals)) =
--       ApplyValuesMK $ HD.UtxoValues $ f utxovals

-- queryKeys ::
--      (Map.Map Token TValue -> a)
--   -> LedgerTables (LedgerState BlockWithPayload) ValuesMK
--   -> a
-- queryKeys f (TokenToTValue (ApplyValuesMK (HD.UtxoValues utxovals))) = f utxovals

-- {-------------------------------------------------------------------------------
--   Instances
-- -------------------------------------------------------------------------------}

-- instance Show (ApplyMapKind' mk' Token TValue) where
--   show ap = showsApplyMapKind ap ""

-- -- About this instance: we have that the use of
-- --
-- -- > genericToExpr UtxoDiff
-- --
-- -- in instance ToExpr (ApplyMapKind mk Token TValue) requires
-- --
-- -- >  ToExpr Map k (UtxoEntryDiff v )
-- --
-- -- requires
-- --
-- -- > ToExpr (UtxoEntryDiff v )
-- --
-- -- requires
-- --
-- -- > ToExpr UtxoEntryDiffState
-- --
-- instance ToExpr HD.UtxoEntryDiffState where
--   toExpr = genericToExpr

-- -- See instance ToExpr HD.UtxoEntryDiffState
-- instance ToExpr (HD.UtxoEntryDiff TValue) where
--   toExpr = genericToExpr

-- -- Required by the ToExpr (SeqUtxoDiff k v) instance
-- instance ToExpr (HD.SudElement Token TValue) where
--   toExpr = genericToExpr

-- -- Required by the ToExpr (HD.SudElement Token TValue) instance
-- instance ToExpr (HD.UtxoDiff Token TValue) where
--   toExpr = genericToExpr

-- instance ToExpr (ApplyMapKind' mk' Token TValue) where
--   toExpr ApplyEmptyMK                 = App "ApplyEmptyMK"     []
--   toExpr (ApplyDiffMK diffs)          = App "ApplyDiffMK"      [genericToExpr diffs]
--   toExpr (ApplyKeysMK keys)           = App "ApplyKeysMK"      [genericToExpr keys]
--   toExpr (ApplySeqDiffMK (HD.SeqUtxoDiff seqdiff))
--                                       = App "ApplySeqDiffMK"   [genericToExpr $ toList seqdiff]
--   toExpr (ApplyTrackingMK vals diffs) = App "ApplyTrackingMK"  [ genericToExpr vals
--                                                                , genericToExpr diffs
--                                                                ]
--   toExpr (ApplyValuesMK vals)         = App "ApplyValuesMK"    [genericToExpr vals]
--   toExpr ApplyQueryAllMK              = App "ApplyQueryAllMK"  []
--   toExpr (ApplyQuerySomeMK keys)      = App "ApplyQuerySomeMK" [genericToExpr keys]


-- instance (FromCBOR (mk Token TValue), ToCBOR (mk Token TValue)) => Serialise (LedgerTables (LedgerState BlockWithPayload) mk) where
--   encode TokenToTValue {testUtxtokTable} = toCBOR testUtxtokTable
--   decode = fmap TokenToTValue fromCBOR

-- instance ToCBOR Token where
--   toCBOR (Token pt) = encode pt

-- instance FromCBOR Token where
--   fromCBOR = fmap Token decode

-- instance ToCBOR TValue where
--   toCBOR (TValue v) = encode v

-- instance FromCBOR TValue where
--   fromCBOR = fmap TValue decode

