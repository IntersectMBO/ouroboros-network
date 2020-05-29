{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Witness isomorphism between @b@ and @HardForkBlock '[b]@
module Ouroboros.Consensus.HardFork.Combinator.Unary (
    -- * Projections
    projAnnTip
  , projApplyTxErr
  , projBlock
  , projBlockConfig
  , projChainHash
  , projCodecConfig
  , projConsensusConfig
  , projConsensusState
  , projExtLedgerState
  , projForgeState
  , projGenTx
  , projGenTxId
  , projHeader
  , projHeaderHash
  , projHeaderState
  , projInitChainDB
  , projIsLeader
  , projLedgerConfig
  , projLedgerState
  , projLedgerView
  , projQuery
  , projTipInfo
  , projTopLevelConfig
  , projUpdateForgeState
    -- * Injections
  , injAnnTip
  , injApplyTxErr
  , injBlock
  , injBlockConfig
  , injCodecConfig
  , injConsensusConfig
  , injConsensusState
  , injEnvelopeErr
  , injExtLedgerState
  , injForgeState
  , injGenTx
  , injGenTxId
  , injHeader
  , injHeaderHash
  , injHeaderState
  , injLedgerConfig
  , injLedgerState
  , injMaintainForgeState
  , injProtocolClientInfo
  , injProtocolInfo
  , injQuery
  , injTopLevelConfig
  ) where

import           Data.Bifunctor
import           Data.SOP.Strict
import           Data.Type.Equality
import           Data.Void

import           Cardano.Slotting.EpochInfo

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Block.Forge
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

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
import           Ouroboros.Consensus.HardFork.Combinator.Node
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol
                     (HardForkEraLedgerView (..))
import           Ouroboros.Consensus.HardFork.Combinator.State
                     (HardForkState_ (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Projections
-------------------------------------------------------------------------------}

projIsLeader :: IsLeader (BlockProtocol (HardForkBlock '[b]))
             -> IsLeader (BlockProtocol b)
projIsLeader = unwrapIsLeader . unZ . getOneEraIsLeader

projGenTx :: GenTx (HardForkBlock '[b]) -> GenTx b
projGenTx = unZ . getOneEraGenTx . getHardForkGenTx

injGenTx :: GenTx b -> GenTx (HardForkBlock '[b])
injGenTx = HardForkGenTx . OneEraGenTx . Z

projGenTxId :: GenTxId (HardForkBlock '[b]) -> GenTxId b
projGenTxId = unwrapGenTxId . unZ . getOneEraGenTxId . getHardForkGenTxId

injGenTxId :: GenTxId b -> GenTxId (HardForkBlock '[b])
injGenTxId = HardForkGenTxId . OneEraGenTxId . Z . WrapGenTxId

projBlock :: HardForkBlock '[b] -> b
projBlock = unI . unZ . getOneEraBlock . getHardForkBlock

injBlock :: b -> HardForkBlock '[b]
injBlock = HardForkBlock . OneEraBlock . Z . I

projHeaderHash :: forall b. ConvertRawHash b
               => HeaderHash (HardForkBlock '[b]) -> HeaderHash b
projHeaderHash = fromRawHash (Proxy @b) . getOneEraHash

injHeaderHash :: forall b. ConvertRawHash b
              => HeaderHash b -> HeaderHash (HardForkBlock '[b])
injHeaderHash = OneEraHash . toRawHash (Proxy @b)

projChainHash :: ConvertRawHash b
              => ChainHash (HardForkBlock '[b]) -> ChainHash b
projChainHash = \case
    GenesisHash -> GenesisHash
    BlockHash h -> BlockHash (projHeaderHash h)

projHeader :: Header (HardForkBlock '[b]) -> Header b
projHeader = unZ . getOneEraHeader . getHardForkHeader

injHeader :: Header b -> Header (HardForkBlock '[b])
injHeader = HardForkHeader . OneEraHeader . Z

projBlockConfig :: BlockConfig (HardForkBlock '[b]) -> BlockConfig b
projBlockConfig = hd . getPerEraBlockConfig . hardForkBlockConfigPerEra

injBlockConfig :: BlockConfig b -> BlockConfig (HardForkBlock '[b])
injBlockConfig = HardForkBlockConfig . PerEraBlockConfig . (:* Nil)

projCodecConfig :: CodecConfig (HardForkBlock '[b]) -> CodecConfig b
projCodecConfig = hd . getPerEraCodecConfig . hardForkCodecConfigPerEra

injCodecConfig :: CodecConfig b -> CodecConfig (HardForkBlock '[b])
injCodecConfig = HardForkCodecConfig . PerEraCodecConfig . (:* Nil)

projLedgerConfig :: forall b. SingleEraBlock b
                 => LedgerConfig (HardForkBlock '[b])
                 -> (EpochInfo Identity, LedgerConfig b)
projLedgerConfig =
      complete
    . unwrapPartialLedgerConfig
    . hd
    . getPerEraLedgerConfig
    . hardForkLedgerConfigPerEra
  where
    complete :: PartialLedgerConfig b -> (EpochInfo Identity, LedgerConfig b)
    complete cfg = (ei, completeLedgerConfig (Proxy @b) ei cfg)
      where
        ei :: EpochInfo Identity
        ei = fixedSizeEpochInfo $
               History.eraEpochSize (singleEraParams (Proxy @b) cfg)

-- TODO generalise this function to no longer require this constraint
injLedgerConfig :: PartialLedgerConfig b ~ LedgerConfig b
                => SecurityParam
                -> History.EraParams
                -> LedgerConfig b
                -> LedgerConfig (HardForkBlock '[b])
injLedgerConfig k eraParams cfg = HardForkLedgerConfig {
      hardForkLedgerConfigK      = k
    , hardForkLedgerConfigShape  = History.singletonShape eraParams
    , hardForkLedgerConfigPerEra = PerEraLedgerConfig (WrapPartialLedgerConfig cfg :* Nil)
    }

projConsensusConfig :: forall b. SingleEraBlock b
                    => EpochInfo Identity
                    -> ConsensusConfig (BlockProtocol (HardForkBlock '[b]))
                    -> ConsensusConfig (BlockProtocol b)
projConsensusConfig ei =
      completeConsensusConfig (Proxy @(BlockProtocol b)) ei
    . unwrapPartialConsensusConfig
    . hd
    . getPerEraConsensusConfig
    . hardForkConsensusConfigPerEra

injConsensusConfig :: forall b.
                      ( SingleEraBlock b
                        -- TODO generalise this function to no longer require
                        -- this constraint
                      , PartialConsensusConfig (BlockProtocol b) ~ ConsensusConfig (BlockProtocol b)
                      )
                   => History.EraParams
                   -> ConsensusConfig (BlockProtocol b)
                   -> ConsensusConfig (BlockProtocol (HardForkBlock '[b]))
injConsensusConfig eraParams cfg = HardForkConsensusConfig {
      hardForkConsensusConfigK      = protocolSecurityParam cfg
    , hardForkConsensusConfigShape  = History.singletonShape eraParams
    , hardForkConsensusConfigPerEra = PerEraConsensusConfig (WrapPartialConsensusConfig cfg :* Nil)
    }

projLedgerState :: LedgerState (HardForkBlock '[b]) -> LedgerState b
projLedgerState =
      State.currentState
    . Telescope.fromTZ
    . getHardForkState
    . getHardForkLedgerState

injLedgerState :: LedgerState b -> LedgerState (HardForkBlock '[b])
injLedgerState =
      HardForkLedgerState
    . HardForkState
    . Telescope.TZ
    . State.Current History.initBound

projLedgerView :: proxy b
               -> LedgerView (BlockProtocol (HardForkBlock '[b]))
               -> LedgerView (BlockProtocol b)
projLedgerView _ =
      hardForkEraLedgerView
    . State.fromTZ

projConsensusState :: ConsensusState (BlockProtocol (HardForkBlock '[b]))
                   -> ConsensusState (BlockProtocol b)
projConsensusState =
      unwrapConsensusState
    . State.currentState
    . Telescope.fromTZ
    . getHardForkState

injConsensusState :: ConsensusState (BlockProtocol b)
                  -> ConsensusState (BlockProtocol (HardForkBlock '[b]))
injConsensusState =
      HardForkState
    . Telescope.TZ
    . State.Current History.initBound
    . WrapConsensusState

projHeaderState :: HeaderState (HardForkBlock '[b]) -> HeaderState b
projHeaderState HeaderState{..} = HeaderState {
      headerStateConsensus = projConsensusState headerStateConsensus
    , headerStateTips      = projAnnTip <$> headerStateTips
    , headerStateAnchor    = projAnnTip <$> headerStateAnchor
    }

injHeaderState :: HeaderState b -> HeaderState (HardForkBlock '[b])
injHeaderState HeaderState{..} = HeaderState {
      headerStateConsensus = injConsensusState headerStateConsensus
    , headerStateTips      = injAnnTip <$> headerStateTips
    , headerStateAnchor    = injAnnTip <$> headerStateAnchor
    }

projExtLedgerState :: ExtLedgerState (HardForkBlock '[b]) -> ExtLedgerState b
projExtLedgerState ExtLedgerState{..} = ExtLedgerState {
      ledgerState = projLedgerState ledgerState
    , headerState = projHeaderState headerState
    }

injExtLedgerState :: ExtLedgerState b -> ExtLedgerState (HardForkBlock '[b])
injExtLedgerState ExtLedgerState{..} = ExtLedgerState {
      ledgerState = injLedgerState ledgerState
    , headerState = injHeaderState headerState
    }

projTopLevelConfig :: forall b. SingleEraBlock b
                   => TopLevelConfig (HardForkBlock '[b]) -> TopLevelConfig b
projTopLevelConfig TopLevelConfig{..} = TopLevelConfig{
      configConsensus  = projConsensusConfig ei configConsensus
    , configLedger     = configLedger'
    , configBlock      = projBlockConfig configBlock
    }
  where
    (ei, configLedger') = projLedgerConfig configLedger

injTopLevelConfig :: forall b.
                     ( SingleEraBlock b
                     , PartialConsensusConfig (BlockProtocol b) ~ ConsensusConfig (BlockProtocol b)
                     , PartialLedgerConfig b ~ LedgerConfig b
                     )
                  => TopLevelConfig b -> TopLevelConfig (HardForkBlock '[b])
injTopLevelConfig TopLevelConfig{..} = TopLevelConfig{
      configConsensus = injConsensusConfig eraParams configConsensus
    , configLedger    = injLedgerConfig
                          (protocolSecurityParam configConsensus)
                          eraParams
                          configLedger
    , configBlock     = injBlockConfig configBlock
    }
  where
    eraParams = singleEraParams (Proxy @b) configLedger

projForgeState :: proxy b -> ForgeState (HardForkBlock '[b]) -> ForgeState b
projForgeState _ = unwrapForgeState . hd . getPerEraForgeState

injForgeState :: proxy b -> ForgeState b -> ForgeState (HardForkBlock '[b])
injForgeState _ = PerEraForgeState . (:* Nil) . WrapForgeState

injMaintainForgeState :: forall m b. Functor m
                      => MaintainForgeState m b
                      -> MaintainForgeState m (HardForkBlock '[b])
injMaintainForgeState maintainForgeState = MaintainForgeState {
      initForgeState   = injForgeState (Proxy @b)
                       $ initForgeState maintainForgeState
    , updateForgeState = updateForgeState maintainForgeState
                       . projUpdateForgeState
    }

projInitChainDB :: InitChainDB m (HardForkBlock '[b]) -> InitChainDB m b
projInitChainDB initDB = InitChainDB.InitChainDB {
      InitChainDB.checkEmpty = InitChainDB.checkEmpty initDB
    , InitChainDB.addBlock   = InitChainDB.addBlock initDB . injBlock
    }

projApplyTxErr :: ApplyTxErr (HardForkBlock '[b]) -> ApplyTxErr b
projApplyTxErr (HardForkApplyTxErrFromEra err) =
      unwrapApplyTxErr
    . unZ
    . getOneEraApplyTxErr
    $ err
projApplyTxErr (HardForkApplyTxErrWrongEra err) =
      absurd
    . mismatchOneEra
    $ err

injApplyTxErr :: ApplyTxErr b -> ApplyTxErr (HardForkBlock '[b])
injApplyTxErr =
      HardForkApplyTxErrFromEra
    . OneEraApplyTxErr
    . Z
    . WrapApplyTxErr

projTipInfo :: TipInfo (HardForkBlock '[b]) -> TipInfo b
projTipInfo =
      unwrapTipInfo
    . unZ
    . getOneEraTipInfo

projAnnTip :: AnnTip (HardForkBlock '[b]) -> AnnTip b
projAnnTip (AnnTip s b nfo) = AnnTip s b (projTipInfo nfo)

injAnnTip :: AnnTip b -> AnnTip (HardForkBlock '[b])
injAnnTip (AnnTip s b nfo) =
    AnnTip s b (OneEraTipInfo (Z (WrapTipInfo nfo)))

projQuery :: Query (HardForkBlock '[b]) result
          -> (forall result'.
                  (result :~: HardForkQueryResult '[b] result')
               -> Query b result'
               -> a)
          -> a
projQuery qry k = getHardForkQuery qry $ \Refl -> k Refl . projHardForkQuery

projHardForkQuery :: HardForkQuery '[b] result -> Query b result
projHardForkQuery (QZ qry) = qry
projHardForkQuery (QS qry) = case qry of {}

injQuery :: Query b result
         -> Query (HardForkBlock '[b]) (HardForkQueryResult '[b] result)
injQuery = HardForkQuery . QZ

injEnvelopeErr :: OtherHeaderEnvelopeError b
               -> OtherHeaderEnvelopeError (HardForkBlock '[b])
injEnvelopeErr =
      HardForkEnvelopeErrFromEra
    . OneEraEnvelopeErr
    . Z
    . WrapEnvelopeErr

projUpdateForgeState :: forall b m. Functor m
                     => Update m (ForgeState (HardForkBlock '[b]))
                     -> Update m (ForgeState b)
projUpdateForgeState = liftUpdate get set
  where
    get :: PerEraForgeState '[b] -> ForgeState b
    get = projForgeState (Proxy @b)

    set :: ForgeState b -> PerEraForgeState '[b] -> PerEraForgeState '[b]
    set = const . injForgeState (Proxy @b)

-- TODO generalise this function to no longer require the equality constraints
injProtocolInfo :: forall m b.
                   ( Functor m
                   , SingleEraBlock b
                   , PartialConsensusConfig (BlockProtocol b) ~ ConsensusConfig (BlockProtocol b)
                   , PartialLedgerConfig b ~ LedgerConfig b
                   )
                => ProtocolInfo m b -> ProtocolInfo m (HardForkBlock '[b])
injProtocolInfo ProtocolInfo {..} = ProtocolInfo {
      pInfoConfig      = injTopLevelConfig
                           pInfoConfig
    , pInfoInitLedger  = injExtLedgerState
                           pInfoInitLedger
    , pInfoLeaderCreds = bimap (injCanBeLeader (Proxy @b)) injMaintainForgeState <$>
                           pInfoLeaderCreds
    }

injProtocolClientInfo :: ProtocolClientInfo b -> ProtocolClientInfo (HardForkBlock '[b])
injProtocolClientInfo ProtocolClientInfo{..} = ProtocolClientInfo {
      pClientInfoCodecConfig = injCodecConfig pClientInfoCodecConfig
    }

injCanBeLeader :: proxy b
               -> CanBeLeader (BlockProtocol b)
               -> CanBeLeader (BlockProtocol (HardForkBlock '[b]))
injCanBeLeader _ proof = Comp (Just (WrapCanBeLeader proof)) :* Nil
