{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

-- | Witness isomorphism between @b@ and @HardForkBlock '[b]@
module Ouroboros.Consensus.HardFork.Combinator.Unary (
    Isomorphic(..)
  , project'
  , inject'
    -- * Dependent types
  , projQuery
  , injQuery
  , projNestedCtxt
  , injNestedCtxt
    -- * Convenience exports
  , Proxy(..)
  , I(..)
  ) where

import           Data.Bifunctor
import           Data.Coerce
import           Data.Proxy
import           Data.SOP.Strict
import           Data.Type.Equality
import           Data.Void

import           Cardano.Slotting.EpochInfo

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Forge ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Projection/injection for a single block into degenerate HardForkBlock
-------------------------------------------------------------------------------}

class Isomorphic f where
  project :: NoHardForks blk => f (HardForkBlock '[blk]) -> f blk
  inject  :: NoHardForks blk => f blk -> f (HardForkBlock '[blk])

project' :: forall proxy f x y blk. (
              Isomorphic f
            , NoHardForks blk
            , Coercible x (f (HardForkBlock '[blk]))
            , Coercible y (f blk)
            )
         => proxy (f blk) -> x -> y
project' _ =
      (coerce :: f blk -> y)
    . project
    . (coerce :: x -> f (HardForkBlock '[blk]))

inject' :: forall proxy f x y blk. (
              Isomorphic f
            , NoHardForks blk
            , Coercible x (f blk)
            , Coercible y (f (HardForkBlock '[blk]))
            )
         => proxy (f blk) -> x -> y
inject' _ =
      (coerce :: f (HardForkBlock '[blk]) -> y)
    . inject
    . (coerce :: x -> f blk)

{-------------------------------------------------------------------------------
  Defaults (to ease implementation)

  It'd be nicer to use deriving-via here, but we cannot due to a GHC bug
  (resulting in @No family instance for ‘GenTx’@ errors).
  See <https://gitlab.haskell.org/ghc/ghc/issues/13154#note_224287> .
-------------------------------------------------------------------------------}

defaultProjectNS :: forall f blk.
                    Coercible (f (HardForkBlock '[blk])) (NS f '[blk])
                 => f (HardForkBlock '[blk]) -> f blk
defaultProjectNS = unZ . (coerce :: f (HardForkBlock '[blk]) -> NS f '[blk])

defaultInjectNS :: forall f blk.
                   Coercible (f (HardForkBlock '[blk])) (NS f '[blk])
                => f blk -> f (HardForkBlock '[blk])
defaultInjectNS = (coerce :: NS f '[blk] -> f (HardForkBlock '[blk])) . Z

defaultProjectNP :: forall f blk.
                    Coercible (f (HardForkBlock '[blk])) (NP f '[blk])
                 => f (HardForkBlock '[blk]) -> f blk
defaultProjectNP = hd . (coerce :: f (HardForkBlock '[blk]) -> NP f '[blk])

defaultInjectNP :: forall f blk.
                   Coercible (f (HardForkBlock '[blk])) (NP f '[blk])
                => f blk -> f (HardForkBlock '[blk])
defaultInjectNP = (coerce :: NP f '[blk] -> f (HardForkBlock '[blk])) . (:* Nil)

defaultProjectSt :: forall f blk.
                    Coercible (f (HardForkBlock '[blk])) (HardForkState f '[blk])
                 => f (HardForkBlock '[blk]) -> f blk
defaultProjectSt =
      State.currentState
    . Telescope.fromTZ
    . getHardForkState
    . (coerce :: f (HardForkBlock '[blk]) -> HardForkState f '[blk])

defaultInjectSt :: forall f blk.
                   Coercible (f (HardForkBlock '[blk])) (HardForkState f '[blk])
                => f blk -> f (HardForkBlock '[blk])
defaultInjectSt =
      (coerce :: HardForkState f '[blk] -> f (HardForkBlock '[blk]))
    . HardForkState
    . Telescope.TZ
    . State.Current History.initBound

{-------------------------------------------------------------------------------
  Forwarding instances
-------------------------------------------------------------------------------}

instance Isomorphic ((->) a) where
  project f = coerce (project @I) . f
  inject  f = coerce (inject  @I) . f

instance (Functor f, Isomorphic g) => Isomorphic (f :.: g) where
  project (Comp fg) = Comp (project <$> fg)
  inject  (Comp fg) = Comp (inject  <$> fg)

{-------------------------------------------------------------------------------
  Simple instances
-------------------------------------------------------------------------------}

instance Isomorphic WrapIsLeader where
  project = defaultProjectNS
  inject  = defaultInjectNS

instance Isomorphic WrapGenTxId where
  project = defaultProjectNS
  inject  = defaultInjectNS

instance Isomorphic I where
  project = defaultProjectNS
  inject  = defaultInjectNS

instance Isomorphic GenTx where
  project = defaultProjectNS
  inject  = defaultInjectNS

instance Isomorphic Header where
  project = defaultProjectNS
  inject  = defaultInjectNS

instance Isomorphic BlockConfig where
  project = defaultProjectNP
  inject  = defaultInjectNP

instance Isomorphic CodecConfig where
  project = defaultProjectNP
  inject  = defaultInjectNP

instance Isomorphic LedgerState where
  project = defaultProjectSt
  inject  = defaultInjectSt

instance Isomorphic WrapChainDepState where
  project = defaultProjectSt
  inject  = defaultInjectSt

instance Isomorphic WrapChainIndepState where
  project = defaultProjectNP
  inject  = defaultInjectNP

instance Isomorphic WrapChainIndepStateConfig where
  project = defaultProjectNP
  inject  = defaultInjectNP

instance Isomorphic WrapExtraForgeState where
  project = defaultProjectNP
  inject  = defaultInjectNP

instance Isomorphic WrapTipInfo where
  project = defaultProjectNS
  inject  = defaultInjectNS

{-------------------------------------------------------------------------------
  Hash
-------------------------------------------------------------------------------}

instance Isomorphic WrapHeaderHash where
  project :: forall blk. ConvertRawHash blk
          => WrapHeaderHash (HardForkBlock '[blk]) -> WrapHeaderHash blk
  project =
        WrapHeaderHash
      . fromRawHash (Proxy @blk) . getOneEraHash
      . unwrapHeaderHash

  inject :: forall blk. ConvertRawHash blk
      => WrapHeaderHash blk -> WrapHeaderHash (HardForkBlock '[blk])
  inject =
        WrapHeaderHash
      . OneEraHash . toRawHash (Proxy @blk)
      . unwrapHeaderHash

instance Isomorphic ChainHash where
  project :: forall blk. NoHardForks blk
          => ChainHash (HardForkBlock '[blk]) -> ChainHash blk
  project GenesisHash   = GenesisHash
  project (BlockHash h) = BlockHash (project' (Proxy @(WrapHeaderHash blk)) h)

  inject :: forall blk. NoHardForks blk
         => ChainHash blk -> ChainHash (HardForkBlock '[blk])
  inject GenesisHash   = GenesisHash
  inject (BlockHash h) = BlockHash (inject' (Proxy @(WrapHeaderHash blk)) h)

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

-- | Projection/injection for 'TopLevelConfig'
--
-- NOTE: We do not define one for 'LedgerConfig' or 'ConsensusConfig', since
-- we need the 'EraParams' for their injections, which we can only derive if
-- we have the top-level config.
instance Isomorphic TopLevelConfig where
  project :: forall blk. NoHardForks blk
          => TopLevelConfig (HardForkBlock '[blk]) -> TopLevelConfig blk
  project TopLevelConfig{..} = TopLevelConfig{
        configConsensus  = auxConsensus configConsensus
      , configIndep      = auxIndep     configIndep
      , configLedger     = auxLedger    configLedger
      , configBlock      = project      configBlock
      }
    where
      ei :: EpochInfo Identity
      ei = fixedSizeEpochInfo
         . History.eraEpochSize
         . unK . hd
         . History.getShape
         . hardForkLedgerConfigShape
         $ configLedger

      auxLedger :: LedgerConfig (HardForkBlock '[blk]) -> LedgerConfig blk
      auxLedger =
            completeLedgerConfig (Proxy @blk) ei
          . unwrapPartialLedgerConfig
          . hd
          . getPerEraLedgerConfig
          . hardForkLedgerConfigPerEra

      auxConsensus :: ConsensusConfig (BlockProtocol (HardForkBlock '[blk]))
                   -> ConsensusConfig (BlockProtocol blk)
      auxConsensus =
            completeConsensusConfig (Proxy @(BlockProtocol blk)) ei
          . unwrapPartialConsensusConfig
          . hd
          . getPerEraConsensusConfig
          . hardForkConsensusConfigPerEra

      auxIndep :: ChainIndepStateConfig (BlockProtocol (HardForkBlock '[blk]))
               -> ChainIndepStateConfig (BlockProtocol blk)
      auxIndep = project' (Proxy @(WrapChainIndepStateConfig blk))

  inject :: forall blk. NoHardForks blk
         => TopLevelConfig blk -> TopLevelConfig (HardForkBlock '[blk])
  inject tlc@TopLevelConfig{..} = TopLevelConfig{
        configConsensus = auxConsensus configConsensus
      , configIndep     = auxIndep     configIndep
      , configLedger    = auxLedger    configLedger
      , configBlock     = inject       configBlock
      }
    where
      eraParams = getEraParams tlc
      k         = protocolSecurityParam configConsensus

      auxLedger :: LedgerConfig blk -> LedgerConfig (HardForkBlock '[blk])
      auxLedger cfg = HardForkLedgerConfig {
            hardForkLedgerConfigK      = k
          , hardForkLedgerConfigShape  = History.singletonShape eraParams
          , hardForkLedgerConfigPerEra = PerEraLedgerConfig $
                 WrapPartialLedgerConfig (toPartialLedgerConfig (Proxy @blk) cfg )
              :* Nil
          }

      auxConsensus :: ConsensusConfig (BlockProtocol blk)
                   -> ConsensusConfig (BlockProtocol (HardForkBlock '[blk]))
      auxConsensus cfg = HardForkConsensusConfig {
            hardForkConsensusConfigK      = protocolSecurityParam cfg
          , hardForkConsensusConfigShape  = History.singletonShape eraParams
          , hardForkConsensusConfigPerEra = PerEraConsensusConfig $
                 WrapPartialConsensusConfig (toPartialConsensusConfig (Proxy @blk) cfg)
              :* Nil
          }

      auxIndep :: ChainIndepStateConfig (BlockProtocol blk)
               -> ChainIndepStateConfig (BlockProtocol (HardForkBlock '[blk]))
      auxIndep = inject' (Proxy @(WrapChainIndepStateConfig blk))

{-------------------------------------------------------------------------------
  Various kinds of records
-------------------------------------------------------------------------------}

instance Isomorphic HeaderState where
  project :: forall blk. NoHardForks blk
          => HeaderState (HardForkBlock '[blk]) -> HeaderState blk
  project HeaderState{..} = HeaderState {
        headerStateConsensus = project' (Proxy @(WrapChainDepState blk)) headerStateConsensus
      , headerStateTips      = project <$> headerStateTips
      , headerStateAnchor    = project <$> headerStateAnchor
      }

  inject :: forall blk. NoHardForks blk
         => HeaderState blk -> HeaderState (HardForkBlock '[blk])
  inject HeaderState{..} = HeaderState {
        headerStateConsensus = inject' (Proxy @(WrapChainDepState blk)) headerStateConsensus
      , headerStateTips      = inject <$> headerStateTips
      , headerStateAnchor    = inject <$> headerStateAnchor
      }

instance Isomorphic ExtLedgerState where
  project ExtLedgerState{..} = ExtLedgerState {
        ledgerState = project ledgerState
      , headerState = project headerState
      }

  inject ExtLedgerState{..} = ExtLedgerState {
        ledgerState = inject ledgerState
      , headerState = inject headerState
      }

instance Isomorphic ForgeState where
  project :: forall blk. NoHardForks blk
          => ForgeState (HardForkBlock '[blk]) -> ForgeState blk
  project ForgeState{..} = ForgeState {
        chainIndepState = project' (Proxy @(WrapChainIndepState blk)) chainIndepState
      , extraForgeState = project' (Proxy @(WrapExtraForgeState blk)) extraForgeState
      }

  inject :: forall blk. NoHardForks blk
         => ForgeState blk -> ForgeState (HardForkBlock '[blk])
  inject ForgeState{..} = ForgeState {
        chainIndepState = inject' (Proxy @(WrapChainIndepState blk)) chainIndepState
      , extraForgeState = inject' (Proxy @(WrapExtraForgeState blk)) extraForgeState
      }

instance Functor m => Isomorphic (MaintainForgeState m) where
  project :: forall blk. NoHardForks blk
          => MaintainForgeState m (HardForkBlock '[blk])
          -> MaintainForgeState m blk
  project mfs = MaintainForgeState {
        initForgeState   = project $ initForgeState mfs
      , updateForgeState = \cfg slotNo ->
            fmap project
          . updateForgeState
              mfs
              (inject' (Proxy @(WrapChainIndepStateConfig blk)) cfg)
              slotNo
          . inject
      }

  inject :: forall blk. NoHardForks blk
         => MaintainForgeState m blk
         -> MaintainForgeState m (HardForkBlock '[blk])
  inject mfs = MaintainForgeState {
        initForgeState   = inject $ initForgeState mfs
      , updateForgeState = \cfg slotNo ->
            fmap inject
          . updateForgeState
              mfs
              (project' (Proxy @(WrapChainIndepStateConfig blk)) cfg)
              slotNo
          . project
      }

instance Isomorphic AnnTip where
  project :: forall blk. NoHardForks blk => AnnTip (HardForkBlock '[blk]) -> AnnTip blk
  project (AnnTip s b nfo) = AnnTip s b (project' (Proxy @(WrapTipInfo blk)) nfo)

  inject (AnnTip s b nfo) = AnnTip s b (OneEraTipInfo (Z (WrapTipInfo nfo)))

instance Isomorphic (InitChainDB m) where
  project :: forall blk. NoHardForks blk
          => InitChainDB m (HardForkBlock '[blk]) -> InitChainDB m blk
  project initDB = InitChainDB.InitChainDB {
        InitChainDB.checkEmpty = InitChainDB.checkEmpty initDB
      , InitChainDB.addBlock   = InitChainDB.addBlock   initDB . inject' (Proxy @(I blk))
      }

  inject :: forall blk. NoHardForks blk
         => InitChainDB m blk -> InitChainDB m (HardForkBlock '[blk])
  inject initDB = InitChainDB.InitChainDB {
        InitChainDB.checkEmpty = InitChainDB.checkEmpty initDB
      , InitChainDB.addBlock   = InitChainDB.addBlock   initDB . project' (Proxy @(I blk))
      }

instance Isomorphic ProtocolClientInfo where
  project ProtocolClientInfo{..} = ProtocolClientInfo {
        pClientInfoCodecConfig = project pClientInfoCodecConfig
      }

  inject ProtocolClientInfo{..} = ProtocolClientInfo {
        pClientInfoCodecConfig = inject pClientInfoCodecConfig
      }

instance Functor m => Isomorphic (ProtocolInfo m) where
  project :: forall blk. NoHardForks blk
          => ProtocolInfo m (HardForkBlock '[blk]) -> ProtocolInfo m blk
  project ProtocolInfo {..} = ProtocolInfo {
        pInfoConfig      = project pInfoConfig
      , pInfoInitLedger  = project pInfoInitLedger
      , pInfoLeaderCreds = bimap (project' (Proxy @(WrapCanBeLeader blk))) project <$>
                             pInfoLeaderCreds
      }

  inject :: forall blk. NoHardForks blk
         => ProtocolInfo m blk -> ProtocolInfo m (HardForkBlock '[blk])
  inject ProtocolInfo {..} = ProtocolInfo {
        pInfoConfig      = inject pInfoConfig
      , pInfoInitLedger  = inject pInfoInitLedger
      , pInfoLeaderCreds = bimap (inject' (Proxy @(WrapCanBeLeader blk))) inject <$>
                             pInfoLeaderCreds
      }

{-------------------------------------------------------------------------------
  Types that require take advantage of the fact that we have a single era
-------------------------------------------------------------------------------}

instance Isomorphic WrapApplyTxErr where
  project = aux . unwrapApplyTxErr
    where
      aux :: ApplyTxErr (HardForkBlock '[blk]) -> WrapApplyTxErr blk
      aux (HardForkApplyTxErrFromEra  err) = unZ $ getOneEraApplyTxErr err
      aux (HardForkApplyTxErrWrongEra err) = absurd $ mismatchOneEra err

  inject = WrapApplyTxErr . aux
    where
      aux :: WrapApplyTxErr blk -> ApplyTxErr (HardForkBlock '[blk])
      aux = HardForkApplyTxErrFromEra . OneEraApplyTxErr . Z

instance Isomorphic WrapEnvelopeErr where
  project = aux . unwrapEnvelopeErr
    where
      aux :: OtherHeaderEnvelopeError (HardForkBlock '[blk])
          -> WrapEnvelopeErr blk
      aux (HardForkEnvelopeErrFromEra  err) = unZ $ getOneEraEnvelopeErr err
      aux (HardForkEnvelopeErrWrongEra err) = absurd $ mismatchOneEra err

  inject = WrapEnvelopeErr . aux
    where
      aux :: WrapEnvelopeErr b
          -> OtherHeaderEnvelopeError (HardForkBlock '[b])
      aux = HardForkEnvelopeErrFromEra . OneEraEnvelopeErr . Z

instance Isomorphic WrapCanBeLeader where
  project = fromSingletonOptNP . unwrapCanBeLeader
  inject  = WrapCanBeLeader . singletonOptNP

instance Isomorphic WrapLedgerView where
  project = State.fromTZ . hardForkLedgerViewPerEra . unwrapLedgerView
  inject  = WrapLedgerView
          . HardForkLedgerView TransitionImpossible
          . HardForkState
          . Telescope.TZ
          . Current History.initBound

instance Isomorphic (SomeBlock (NestedCtxt f)) where
  project (SomeBlock ctxt) = SomeBlock $ projNestedCtxt ctxt
  inject  (SomeBlock ctxt) = SomeBlock $ injNestedCtxt  ctxt

{-------------------------------------------------------------------------------
  Serialised
-------------------------------------------------------------------------------}

instance Isomorphic SerialisedHeader where
  project =
        SerialisedHeaderFromDepPair
      . depPairFirst projNestedCtxt
      . serialisedHeaderToDepPair

  inject =
        SerialisedHeaderFromDepPair
      . depPairFirst injNestedCtxt
      . serialisedHeaderToDepPair

{-------------------------------------------------------------------------------
  Dependent types

  TODO: Class?
-------------------------------------------------------------------------------}

-- | Project 'Query'
--
-- Not an instance of 'Isomorphic' because the types change.
projQuery :: Query (HardForkBlock '[b]) result
          -> (forall result'.
                  (result :~: HardForkQueryResult '[b] result')
               -> Query b result'
               -> a)
          -> a
projQuery qry k = getHardForkQuery qry $ \Refl -> k Refl . aux
  where
    aux :: HardForkQuery '[b] result -> Query b result
    aux (QZ q) = q
    aux (QS q) = case q of {}

-- | Inject 'Query'
--
-- Not an instance of 'Isomorphic' because the types change.
injQuery :: Query b result
         -> Query (HardForkBlock '[b]) (HardForkQueryResult '[b] result)
injQuery = HardForkQuery . QZ

projNestedCtxt :: NestedCtxt f (HardForkBlock '[blk]) a -> NestedCtxt f blk a
projNestedCtxt = NestedCtxt . aux . flipNestedCtxt
  where
    aux :: NestedCtxt_ (HardForkBlock '[blk]) f a -> NestedCtxt_ blk f a
    aux (NCZ ctxt) = ctxt
    aux (NCS ctxt) = case ctxt of {}

injNestedCtxt :: NestedCtxt f blk a -> NestedCtxt f (HardForkBlock '[blk]) a
injNestedCtxt = NestedCtxt . NCZ . flipNestedCtxt
