{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

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
import           Data.Hashable (Hashable)
import           Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import           Data.TreeDiff (ToExpr (..))
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.Slot
import           Ouroboros.Network.Point (Block(..))
import           Ouroboros.Network.Block (Point(..))

import           Ouroboros.Consensus.Block.Abstract 
import           Ouroboros.Consensus.Ledger.Abstract (ApplyBlock(..), GetTip (..), LedgerCfg)
import           Ouroboros.Consensus.Ledger.Basics (
                     ApplyMapKind' (..), DiffMK, EmptyMK, InMemory (..), KeysMK,
                     LedgerState, MapKind, SeqDiffMK,
                     ShowLedgerState (..), StowableLedgerTables (..),
                     TableStuff (..), TickedTableStuff (..), TrackingMK,
                     ValuesMK, isCandidateForUnstowDefault,
                     polyEmptyLedgerTables, IsLedger(..), LedgerResult (..), LedgerStateKind)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes)
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import           Ouroboros.Consensus.Ticked (Ticked1)
import           Ouroboros.Consensus.Util.Condense (Condense)

import           Test.Util.Orphans.ToExpr ()

{-------------------------------------------------------------------------------
  TestBlock
-------------------------------------------------------------------------------}

class GetBodyHash b where
  getBodyHash :: b -> TestBodyHash

-- | A Trivial header that serves as block when we don't need a body
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

-- | Add information about this header being an EBB
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

-- | Combine a header and a body into a block
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

-- | Wrap a body using a validity field
data WithValidity body = WithValidity {
    wvValid :: Validity
  , wvBody :: body
}

type EBBBlocksWithBody payload    = WithBlockBody (WithEBBInfo TrivialHeader) (WithValidity payload)
type EBBBlocksWithoutBody         = WithBlockBody (WithEBBInfo TrivialHeader) ()
type NonEBBBlocksWithBody payload = WithBlockBody              TrivialHeader  (WithValidity payload)
type NonEBBBlocksWithoutBody      = WithBlockBody              TrivialHeader  ()

----------------------------------------------------------------------------------------------------

-- | The ledger state of a trivial header only carries the last applied point
newtype instance LedgerState TrivialHeader mk = TrivialLedgerState {
  lastAppliedPoint :: Point TrivialHeader
  } deriving (Generic, Eq, Show)
    deriving anyclass (NoThunks)

newtype instance Ticked1 (LedgerState TrivialHeader) mk = TickedTrivialLedgerState (LedgerState TrivialHeader mk)

instance GetTip (LedgerState TrivialHeader mk) where
  getTip = castPoint . lastAppliedPoint
instance GetTip (Ticked1 (LedgerState TrivialHeader) mk) where
  getTip (TickedTrivialLedgerState l) = castPoint . lastAppliedPoint $ l

instance ShowLedgerState (LedgerTables (LedgerState TrivialHeader)) where
  showsLedgerState _sing = shows
instance ShowLedgerState (LedgerState TrivialHeader) where
  showsLedgerState _sing = shows  

instance InMemory (LedgerState TrivialHeader) where
  convertMapKind (TrivialLedgerState l) = TrivialLedgerState l
  
type instance LedgerCfg (LedgerState TrivialHeader) = ()

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

-- | Being an EBB doesn't add anything in terms of the ledger state
newtype instance LedgerState (WithEBBInfo hdr) mk = WithEBBInfoLedgerState (LedgerState hdr mk)
  deriving (Generic)

deriving newtype instance Eq              (LedgerState hdr mk) => Eq              (LedgerState (WithEBBInfo hdr) mk)
deriving newtype instance Show            (LedgerState hdr mk) => Show            (LedgerState (WithEBBInfo hdr) mk)
deriving newtype instance NoThunks        (LedgerState hdr mk) => NoThunks        (LedgerState (WithEBBInfo hdr) mk)
deriving newtype instance GetTip          (LedgerState hdr mk) => GetTip          (LedgerState (WithEBBInfo hdr) mk)
deriving newtype instance ShowLedgerState (LedgerState hdr)    => ShowLedgerState (LedgerState (WithEBBInfo hdr))

newtype instance Ticked1 (LedgerState (WithEBBInfo hdr)) mk = TickedWithEBBInfoLedgerState (Ticked1 (LedgerState hdr) mk)
deriving newtype instance GetTip (Ticked1 (LedgerState hdr) mk) => GetTip (Ticked1 (LedgerState (WithEBBInfo hdr)) mk)

type instance LedgerCfg (LedgerState (WithEBBInfo hdr)) = LedgerCfg (LedgerState hdr)

deriving newtype instance InMemory        (LedgerState hdr)    => InMemory        (LedgerState (WithEBBInfo hdr))

deriving newtype instance StowableLedgerTables (LedgerState blk) => StowableLedgerTables (LedgerState (WithEBBInfo blk))

instance TableStuff (LedgerState blk) => TableStuff (LedgerState (WithEBBInfo blk)) where
  data LedgerTables (LedgerState (WithEBBInfo blk)) mk = WithEBBInfoLedgerTables (LedgerTables (LedgerState blk) mk)
    deriving stock (Generic)

  projectLedgerTables (WithEBBInfoLedgerState t) = WithEBBInfoLedgerTables $ projectLedgerTables t
  withLedgerTables (WithEBBInfoLedgerState st) (WithEBBInfoLedgerTables tbs) = WithEBBInfoLedgerState $ withLedgerTables st tbs

  pureLedgerTables     p                                                                                       = WithEBBInfoLedgerTables $ pureLedgerTables p
  mapLedgerTables      f                                                         (WithEBBInfoLedgerTables tbs) = WithEBBInfoLedgerTables $ mapLedgerTables f tbs
  traverseLedgerTables f                                                         (WithEBBInfoLedgerTables tbs) = WithEBBInfoLedgerTables <$> traverseLedgerTables f tbs
  zipLedgerTables      f                             (WithEBBInfoLedgerTables l) (WithEBBInfoLedgerTables r)   = WithEBBInfoLedgerTables $ zipLedgerTables f l r
  zipLedgerTables2     f (WithEBBInfoLedgerTables l) (WithEBBInfoLedgerTables m) (WithEBBInfoLedgerTables r)   = WithEBBInfoLedgerTables $ zipLedgerTables2 f l m r
  zipLedgerTablesA     f                             (WithEBBInfoLedgerTables l) (WithEBBInfoLedgerTables r)   = WithEBBInfoLedgerTables <$> zipLedgerTablesA f l r
  zipLedgerTables2A    f (WithEBBInfoLedgerTables l) (WithEBBInfoLedgerTables m) (WithEBBInfoLedgerTables r)   = WithEBBInfoLedgerTables <$> zipLedgerTables2A f l m r
  foldLedgerTables     f                                                         (WithEBBInfoLedgerTables tbs) = foldLedgerTables f tbs
  foldLedgerTables2    f                             (WithEBBInfoLedgerTables l) (WithEBBInfoLedgerTables r)   = foldLedgerTables2 f l r
  namesLedgerTables                                                                                            = WithEBBInfoLedgerTables $ namesLedgerTables

deriving instance Eq (LedgerTables (LedgerState blk) mk) => Eq (LedgerTables (LedgerState (WithEBBInfo blk)) mk)
instance ShowLedgerState (LedgerTables (LedgerState blk)) => ShowLedgerState (LedgerTables (LedgerState (WithEBBInfo blk))) where
  showsLedgerState s (WithEBBInfoLedgerTables tbs) = showsLedgerState s tbs
deriving instance NoThunks (LedgerTables (LedgerState blk) mk) => NoThunks (LedgerTables (LedgerState (WithEBBInfo blk)) mk)

instance IsLedger (LedgerState hdr) => IsLedger (LedgerState (WithEBBInfo hdr)) where
  type AuxLedgerEvent (LedgerState (WithEBBInfo hdr)) = AuxLedgerEvent (LedgerState hdr)
  type LedgerErr (LedgerState (WithEBBInfo hdr)) = LedgerErr (LedgerState hdr)
  applyChainTickLedgerResult cfg slot (WithEBBInfoLedgerState ls) = let LedgerResult evs l = applyChainTickLedgerResult cfg slot ls in
    LedgerResult evs (TickedWithEBBInfoLedgerState l)

instance ( LedgerErr (LedgerState blk) ~ LedgerErr (LedgerState (WithEBBInfo blk))
         , AuxLedgerEvent (LedgerState (WithEBBInfo blk)) ~ AuxLedgerEvent (LedgerState blk)
         , ApplyBlock (LedgerState blk) blk) => ApplyBlock (LedgerState (WithEBBInfo blk)) (WithEBBInfo blk) where
  applyBlockLedgerResult   cfg (WithEBBInfo blk _) (TickedWithEBBInfoLedgerState l) =
    fmap WithEBBInfoLedgerState <$> applyBlockLedgerResult cfg blk l
  reapplyBlockLedgerResult cfg (WithEBBInfo blk _) (TickedWithEBBInfoLedgerState l) =
    WithEBBInfoLedgerState <$> reapplyBlockLedgerResult cfg blk l
  getBlockKeySets (WithEBBInfo blk _) = WithEBBInfoLedgerTables $ getBlockKeySets blk

----------------------------------------------------------------------

data instance LedgerState (WithBlockBody hdr bdy) mk = WithBlockBodyLedgerState {
    headerLedgerState :: LedgerState hdr mk
  , bodyLedgerState   :: PayloadDependentState bdy mk
  } deriving (Generic)

deriving instance (Eq   (LedgerState hdr mk), Eq   (PayloadDependentState bdy mk)) => Eq   (LedgerState (WithBlockBody hdr bdy) mk)
deriving instance (Show (LedgerState hdr mk), Show (PayloadDependentState bdy mk)) => Show (LedgerState (WithBlockBody hdr bdy) mk) 
instance (NoThunks (LedgerState hdr mk), NoThunks (PayloadDependentState bdy mk)) => NoThunks (LedgerState (WithBlockBody hdr bdy) mk)
instance GetTip (LedgerState hdr mk) => GetTip (LedgerState (WithBlockBody hdr bdy) mk) where
  getTip = castPoint . getTip . headerLedgerState
instance (ShowLedgerState (LedgerState hdr), ShowLedgerState (PayloadDependentState bdy)) => ShowLedgerState (LedgerState (WithBlockBody hdr bdy)) where
  showsLedgerState sing (WithBlockBodyLedgerState h b) = showsLedgerState sing h <> showsLedgerState sing b

data instance Ticked1 (LedgerState (WithBlockBody hdr bdy)) mk = TickedWithBlockBodyLedgerState
  { tickedHeader :: Ticked1 (LedgerState hdr) mk
  , tickedBody :: Ticked1 (PayloadDependentState bdy) mk
  }
instance StandardHash (Ticked1 (LedgerState hdr) mk) => StandardHash (Ticked1 (LedgerState (WithBlockBody hdr bdy)) mk)
instance (StandardHash (Ticked1 (LedgerState hdr) mk), HeaderHash hdr ~ HeaderHash (PayloadDependentState bdy), GetTip (Ticked1 (LedgerState hdr) mk), GetTip (Ticked1 (PayloadDependentState bdy) mk))  => GetTip (Ticked1 (LedgerState (WithBlockBody hdr bdy)) mk) where
  getTip (TickedWithBlockBodyLedgerState h b) =
    let hp = castPoint $ getTip h
        bp = castPoint $ getTip b
    in
      if hp == bp then hp else error "mismatch in points!"

data WithBlockBodyLedgerCfg hdr bdy  = WithBlockBodyLedgerCfg {
  headerCfg :: LedgerCfg (LedgerState hdr)
  , bodyCfg :: LedgerCfg (PayloadDependentState bdy)
  } deriving (Generic)
type instance LedgerCfg (LedgerState (WithBlockBody hdr bdy)) = WithBlockBodyLedgerCfg hdr bdy
deriving instance (NoThunks (LedgerCfg (LedgerState hdr)), NoThunks (LedgerCfg (PayloadDependentState bdy))) => NoThunks (WithBlockBodyLedgerCfg hdr bdy)

instance (InMemory (LedgerState hdr), InMemory (PayloadDependentState bdy)) => InMemory (LedgerState (WithBlockBody hdr bdy)) where
  convertMapKind (WithBlockBodyLedgerState a b) = WithBlockBodyLedgerState (convertMapKind a) (convertMapKind b)

instance (StowableLedgerTables (LedgerState hdr), StowableLedgerTables (PayloadDependentState bdy)) => StowableLedgerTables (LedgerState (WithBlockBody hdr bdy)) where
  stowLedgerTables (WithBlockBodyLedgerState h b) = WithBlockBodyLedgerState (stowLedgerTables h) (stowLedgerTables b)
  unstowLedgerTables (WithBlockBodyLedgerState h b) = WithBlockBodyLedgerState (unstowLedgerTables h) (unstowLedgerTables b)
  isCandidateForUnstow (WithBlockBodyLedgerState h b) = isCandidateForUnstow h && isCandidateForUnstow b

instance (TableStuff (LedgerState hdr), TableStuff (PayloadDependentState bdy)) => TableStuff (LedgerState (WithBlockBody hdr bdy)) where
  data LedgerTables (LedgerState (WithBlockBody hdr bdy)) mk = WithBlockBodyLedgerTables {
       headerLedgerTables :: LedgerTables (LedgerState hdr) mk
     , bodyLedgerTables :: LedgerTables (PayloadDependentState bdy) mk
  } deriving Generic

  projectLedgerTables (WithBlockBodyLedgerState h b) = WithBlockBodyLedgerTables (projectLedgerTables h) (projectLedgerTables b)
  withLedgerTables (WithBlockBodyLedgerState hst bst) (WithBlockBodyLedgerTables htbs btbs) = WithBlockBodyLedgerState (withLedgerTables hst htbs) (withLedgerTables bst btbs)

  pureLedgerTables     p                                                                                                           = pureLedgerTables p
  mapLedgerTables      f                                                                     (WithBlockBodyLedgerTables htbs btbs) = WithBlockBodyLedgerTables (mapLedgerTables f htbs) (mapLedgerTables f btbs)
  traverseLedgerTables f                                                                     (WithBlockBodyLedgerTables htbs btbs) = WithBlockBodyLedgerTables <$> traverseLedgerTables f htbs <*> traverseLedgerTables f btbs
  zipLedgerTables      f                                   (WithBlockBodyLedgerTables lh lb) (WithBlockBodyLedgerTables rh rb)     = WithBlockBodyLedgerTables (zipLedgerTables f lh rh) (zipLedgerTables f lb rb)
  zipLedgerTables2     f (WithBlockBodyLedgerTables lh lb) (WithBlockBodyLedgerTables mh mb) (WithBlockBodyLedgerTables rh rb)     = WithBlockBodyLedgerTables (zipLedgerTables2 f lh mh rh) (zipLedgerTables2 f lb mb rb)
  zipLedgerTablesA     f                                   (WithBlockBodyLedgerTables lh lb) (WithBlockBodyLedgerTables rh rb)     = WithBlockBodyLedgerTables <$> zipLedgerTablesA f lh rh <*> zipLedgerTablesA f lb rb
  zipLedgerTables2A    f (WithBlockBodyLedgerTables lh lb) (WithBlockBodyLedgerTables mh mb) (WithBlockBodyLedgerTables rh rb)     = WithBlockBodyLedgerTables <$> zipLedgerTables2A f lh mh rh <*> zipLedgerTables2A f lb mb rb
  foldLedgerTables     f                                                                     (WithBlockBodyLedgerTables htbs btbs) = foldLedgerTables f htbs <> foldLedgerTables f btbs
  foldLedgerTables2    f                                   (WithBlockBodyLedgerTables lh lb) (WithBlockBodyLedgerTables rh rb)     = foldLedgerTables2 f lh rh <> foldLedgerTables2 f lb rb
  namesLedgerTables                                                                                                                = undefined

deriving instance (Eq (LedgerTables (LedgerState blk) mk), Eq (LedgerTables (PayloadDependentState bdy) mk)) => Eq (LedgerTables (LedgerState (WithBlockBody blk bdy)) mk)
instance (ShowLedgerState (LedgerTables (LedgerState blk)), ShowLedgerState (LedgerTables (PayloadDependentState bdy))) => ShowLedgerState (LedgerTables (LedgerState (WithBlockBody blk bdy))) where
  showsLedgerState s (WithBlockBodyLedgerTables htbs btbs) = showsLedgerState s htbs <> showsLedgerState s btbs

data WithBlockBodyAuxLedgerEvent hdr bdy = WithBlockBodyAuxLedgerEvent {
  auxLedgerEventHeader :: AuxLedgerEvent (LedgerState hdr)
  , auxLedgerEventBody :: AuxLedgerEvent (PayloadDependentState bdy)
  } deriving (Generic)

deriving instance (Eq (AuxLedgerEvent (LedgerState hdr)), Eq (AuxLedgerEvent (PayloadDependentState bdy))) => Eq (WithBlockBodyAuxLedgerEvent hdr bdy)
deriving instance (NoThunks (AuxLedgerEvent (LedgerState hdr)), NoThunks (AuxLedgerEvent (PayloadDependentState bdy))) => NoThunks (WithBlockBodyAuxLedgerEvent hdr bdy)  

data WithBlockBodyLedgerErr hdr bdy = WithBlockBodyLedgerErr {
  ledgerErrHeader :: LedgerErr (LedgerState hdr)
  , ledgerErrBody :: LedgerErr (PayloadDependentState bdy)
  } deriving Generic

deriving instance (Eq (LedgerErr (LedgerState hdr)), Eq (LedgerErr (PayloadDependentState bdy))) => Eq (WithBlockBodyLedgerErr hdr bdy)
deriving instance (NoThunks (LedgerErr (LedgerState hdr)), NoThunks (LedgerErr (PayloadDependentState bdy))) => NoThunks (WithBlockBodyLedgerErr hdr bdy)
deriving instance (Show (LedgerErr (LedgerState hdr)), Show (LedgerErr (PayloadDependentState bdy))) => Show (WithBlockBodyLedgerErr hdr bdy)
deriving instance (NoThunks (LedgerTables (LedgerState blk) mk), NoThunks (LedgerTables (PayloadDependentState bdy) mk)) => NoThunks (LedgerTables (LedgerState (WithBlockBody blk bdy)) mk)

instance (forall mk. StandardHash (Ticked1 (LedgerState hdr) mk), HeaderHash hdr ~ HeaderHash (PayloadDependentState bdy), IsLedger (LedgerState hdr), IsLedger (PayloadDependentState bdy)) => IsLedger (LedgerState (WithBlockBody hdr bdy)) where
  type AuxLedgerEvent (LedgerState (WithBlockBody hdr bdy)) = WithBlockBodyAuxLedgerEvent hdr bdy
  type LedgerErr (LedgerState (WithBlockBody hdr bdy)) = WithBlockBodyLedgerErr hdr bdy
  applyChainTickLedgerResult cfg slot (WithBlockBodyLedgerState hls bls) =
    let
      LedgerResult hevs hl = applyChainTickLedgerResult (headerCfg cfg) slot hls
      LedgerResult bevs bl = applyChainTickLedgerResult (bodyCfg cfg) slot bls      
    in
      LedgerResult (zipWith WithBlockBodyAuxLedgerEvent hevs bevs) (TickedWithBlockBodyLedgerState hl bl)
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

  data PayloadDependentState ptype :: LedgerStateKind

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

data instance LedgerState (PayloadDependentState ptype mk) mk = PayloadLedgerState {

                                                                                   }

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

instance TableStuff (LedgerState (PayloadDependentState pld mk)) where
  newtype LedgerTables (LedgerState (PayloadDependentState pld mk)) mk =
    TokenToTValue { testUtxtokTable :: mk (Token pld) TValue }
    deriving stock (Generic)

  projectLedgerTables st       = utxtoktables $ payloadDependentState st
  withLedgerTables    st table = st { payloadDependentState =
                                        (payloadDependentState st) {utxtoktables = table}
                                    }

  pureLedgerTables = TokenToTValue

  mapLedgerTables      f                                     (TokenToTValue x) = TokenToTValue    (f x)
  traverseLedgerTables f                                     (TokenToTValue x) = TokenToTValue <$> f x
  zipLedgerTables      f                   (TokenToTValue x) (TokenToTValue y) = TokenToTValue    (f x y)
  zipLedgerTables2     f (TokenToTValue x) (TokenToTValue y) (TokenToTValue z) = TokenToTValue    (f x y z)
  zipLedgerTablesA     f                   (TokenToTValue x) (TokenToTValue y) = TokenToTValue <$> f x y
  zipLedgerTables2A    f (TokenToTValue x) (TokenToTValue y) (TokenToTValue z) = TokenToTValue <$> f x y z
  foldLedgerTables     f                                     (TokenToTValue x) =                   f x
  foldLedgerTables2    f                   (TokenToTValue x) (TokenToTValue y) =                   f x y
  namesLedgerTables                                                            = TokenToTValue $ NameMK "testblocktables"

deriving newtype  instance Eq       (LedgerTables (LedgerState (PayloadDependentState pld EmptyMK)) EmptyMK)
deriving newtype  instance Eq       (LedgerTables (LedgerState (PayloadDependentState pld DiffMK)) DiffMK)
deriving newtype  instance Eq       (LedgerTables (LedgerState (PayloadDependentState pld ValuesMK )) ValuesMK)
deriving newtype  instance Eq       (LedgerTables (LedgerState (PayloadDependentState pld SeqDiffMK)) SeqDiffMK)
deriving newtype  instance Show     (LedgerTables (LedgerState (PayloadDependentState pld (ApplyMapKind' mk))) (ApplyMapKind' mk))
deriving anyclass instance ToExpr   (LedgerTables (LedgerState (PayloadDependentState pld ValuesMK)) ValuesMK)
deriving anyclass instance NoThunks (LedgerTables (LedgerState (PayloadDependentState pld EmptyMK)) EmptyMK)
deriving anyclass instance NoThunks (LedgerTables (LedgerState (PayloadDependentState pld ValuesMK)) ValuesMK)
deriving anyclass instance NoThunks (LedgerTables (LedgerState (PayloadDependentState pld DiffMK)) DiffMK)
deriving anyclass instance NoThunks (LedgerTables (LedgerState (PayloadDependentState pld SeqDiffMK)) SeqDiffMK)

instance StowableLedgerTables (LedgerState (PayloadDependentState pld mk)) where
  stowLedgerTables   (TestLedger lap (UTxTok tbs h _  ))  = TestLedger lap (UTxTok emptyLedgerTables h tbs)
  unstowLedgerTables (TestLedger lap (UTxTok _   h sto))  = TestLedger lap (UTxTok sto h polyEmptyLedgerTables)
  isCandidateForUnstow = const True

instance TickedTableStuff (LedgerState (PayloadDependentState pld mk)) where
  projectLedgerTablesTicked (TickedTestLedger st)        = projectLedgerTables st
  withLedgerTablesTicked    (TickedTestLedger st) tables =
    TickedTestLedger $ withLedgerTables st tables

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

-- | Mock of a UTxO transaction where exactly one (transaction) input is
-- consumed and exactly one output is produced.
--
data Tx pld = Tx {
    -- | Input that the transaction consumes.
    consumed :: Token pld
    -- | Ouptupt that the transaction produces.
  , produced :: (Token pld, TValue)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NoThunks)

-- | A token is an identifier for the values produced and consumed by the
-- 'TestBlock' transactions.
--
-- This is analogous to @TxId@: it's how we identify what's in the table. It's
-- also analogous to @TxIn@, since we trivially only have one output per 'Tx'.
newtype Token pld = Token { unToken :: Point (PayloadDependentState pld) }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Serialise, NoThunks, ToExpr)

-- | Unit of value associated with the output produced by a transaction.
--
-- This is analogous to @TxOut@: it's what the table maps 'Token's to.
newtype TValue = TValue (WithOrigin SlotNo)
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Serialise, NoThunks, ToExpr)

data TxErr pld
  = TokenWasAlreadyCreated (Token pld)
  | TokenDoesNotExist      (Token pld)
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, Serialise, ToExpr)

instance PayloadSemantics pld where
  data PayloadDependentState pld (mk :: MapKind) =
    UTxTok { utxtoktables :: LedgerTables (LedgerState (PayloadDependentState pld mk)) mk
             -- | All the tokens that ever existed. We use this to
             -- make sure a token is not created more than once. See
             -- the definition of 'applyPayload' in the
             -- 'PayloadSemantics' of 'Tx'.
           , utxhist      :: Set.Set (Token pld)
           , stowed       :: LedgerTables (LedgerState (PayloadDependentState pld mk)) ValuesMK
           }
    deriving stock    (Generic)

  type PayloadDependentError pld = TxErr pld

  -- We need to exercise the HD backend. This requires that we store key-values
  -- ledger tables and the block application semantics satisfy:
  --
  -- * a key is deleted at most once
  -- * a key is inserted at most once
  --
  applyPayload st Tx{consumed, produced} =
      fmap track $ delete consumed st >>= uncurry insert produced
    where
      insert ::
           Token
        -> TValue
        -> PayloadDependentState Tx ValuesMK
        -> Either TxErr (PayloadDependentState Tx ValuesMK)
      insert tok val st'@UTxTok{utxtoktables, utxhist} =
          if tok `Set.member` utxhist
          then Left  $ TokenWasAlreadyCreated tok
          else Right $ st' { utxtoktables = Map.insert tok val `onValues` utxtoktables
                           , utxhist      = Set.insert tok utxhist
                           }
      delete ::
           Token
        -> PayloadDependentState Tx ValuesMK
        -> Either TxErr (PayloadDependentState Tx ValuesMK)
      delete tok st'@UTxTok{utxtoktables} =
          if Map.member tok `queryKeys` utxtoktables
          then Right $ st' { utxtoktables = Map.delete tok `onValues` utxtoktables
                           }
          else Left  $ TokenDoesNotExist tok

      track :: PayloadDependentState Tx ValuesMK -> PayloadDependentState Tx TrackingMK
      track stAfter =
          stAfter { utxtoktables =
                      TokenToTValue $ rawCalculateDifference utxtokBefore utxtokAfter
                  }
        where
          utxtokBefore = testUtxtokTable $ utxtoktables st
          utxtokAfter  = testUtxtokTable $ utxtoktables stAfter

  getPayloadKeySets Tx{consumed} =
    TokenToTValue $ ApplyKeysMK $ HD.UtxoKeys $ Set.singleton consumed

deriving stock    instance (Eq        (PayloadDependentState pld EmptyMK))
deriving stock    instance (Eq        (PayloadDependentState pld DiffMK))
deriving stock    instance (Eq        (PayloadDependentState pld ValuesMK))
deriving stock    instance (Show      (PayloadDependentState pld (ApplyMapKind' mk)))
deriving anyclass instance (Serialise (PayloadDependentState pld EmptyMK))
deriving anyclass instance (ToExpr    (PayloadDependentState pld ValuesMK))
deriving anyclass instance (NoThunks  (PayloadDependentState pld EmptyMK))
deriving anyclass instance (NoThunks  (PayloadDependentState pld DiffMK))
deriving anyclass instance (NoThunks  (PayloadDependentState pld ValuesMK))
deriving anyclass instance (NoThunks  (PayloadDependentState pld SeqDiffMK))

onValues ::
     (Map.Map (Token pld) TValue -> Map.Map (Token pld) TValue)
  -> LedgerTables (LedgerState (PayloadDependentState pld ValuesMK)) ValuesMK
  -> LedgerTables (LedgerState (PayloadDependentState pld ValuesMK)) ValuesMK
onValues f TokenToTValue {testUtxtokTable} = TokenToTValue $ updateMap testUtxtokTable
  where
    updateMap :: ValuesMK Token TValue -> ValuesMK Token TValue
    updateMap (ApplyValuesMK (HD.UtxoValues utxovals)) =
      ApplyValuesMK $ HD.UtxoValues $ f utxovals

queryKeys ::
     (Map.Map (Token pld) TValue -> a)
  -> LedgerTables (LedgerState (PayloadDependentState pld ValuesMK)) ValuesMK
  -> a
queryKeys f (TokenToTValue (ApplyValuesMK (HD.UtxoValues utxovals))) = f utxovals

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

