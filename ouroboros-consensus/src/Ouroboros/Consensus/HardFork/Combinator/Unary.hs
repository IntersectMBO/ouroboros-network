{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
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
  , projForgeState
  , projGenTx
  , projGenTxId
  , projHeader
  , projHeaderHash
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
  , injConsensusState
  , injEnvelopeErr
  , injForgeState
  , injGenTx
  , injGenTxId
  , injHashInfo
  , injHeader
  , injHeaderHash
  , injLedgerState
  , injQuery
  ) where

import           Data.SOP.Strict
import           Data.Type.Equality
import           Data.Void

import           Cardano.Slotting.EpochInfo

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Block.Forge
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
import           Ouroboros.Consensus.Storage.ImmutableDB (HashInfo (..))

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Forge ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol
                     (HardForkEraLedgerView (..))
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Combined
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Wrappers
import           Ouroboros.Consensus.HardFork.Combinator.State
                     (HardForkState_ (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Projections
-------------------------------------------------------------------------------}

projIsLeader :: IsLeader (BlockProtocol (HardForkBlock '[b]))
             -> IsLeader (BlockProtocol b)
projIsLeader = getSingleEraIsLeader . unZ . getOneEraIsLeader

projGenTx :: GenTx (HardForkBlock '[b]) -> GenTx b
projGenTx = unZ . getOneEraGenTx . getHardForkGenTx

injGenTx :: GenTx b -> GenTx (HardForkBlock '[b])
injGenTx = HardForkGenTx . OneEraGenTx . Z

projGenTxId :: GenTxId (HardForkBlock '[b]) -> GenTxId b
projGenTxId = getSingleEraGenTxId . unZ . getOneEraGenTxId . getHardForkGenTxId

injGenTxId :: GenTxId b -> GenTxId (HardForkBlock '[b])
injGenTxId = HardForkGenTxId . OneEraGenTxId . Z . SingleEraGenTxId

projBlock :: HardForkBlock '[b] -> b
projBlock = unI . unZ . getOneEraBlock . getHardForkBlock

injBlock :: b -> HardForkBlock '[b]
injBlock = HardForkBlock . OneEraBlock . Z . I

projHeaderHash :: HeaderHash (HardForkBlock '[b]) -> HeaderHash b
projHeaderHash = getSingleEraHash . unZ . getOneEraHash

injHeaderHash :: HeaderHash b -> HeaderHash (HardForkBlock '[b])
injHeaderHash = OneEraHash . Z . SingleEraHash

projChainHash :: ChainHash (HardForkBlock '[b]) -> ChainHash b
projChainHash GenesisHash   = GenesisHash
projChainHash (BlockHash h) = BlockHash (projHeaderHash h)

projHeader :: Header (HardForkBlock '[b]) -> Header b
projHeader = unZ . getOneEraHeader . getHardForkHeader

injHeader :: Header b -> Header (HardForkBlock '[b])
injHeader = HardForkHeader . OneEraHeader . Z

projBlockConfig :: BlockConfig (HardForkBlock '[b]) -> BlockConfig b
projBlockConfig = hd . getPerEraBlockConfig . hardForkBlockConfigPerEra

projCodecConfig :: CodecConfig (HardForkBlock '[b]) -> CodecConfig b
projCodecConfig = hd . getPerEraCodecConfig . hardForkCodecConfigPerEra

projLedgerConfig :: forall b. SingleEraBlock b
                 => LedgerConfig (HardForkBlock '[b])
                 -> (EpochInfo Identity, LedgerConfig b)
projLedgerConfig =
      complete
    . getSingleEraLedgerConfig
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

projConsensusConfig :: forall b. SingleEraBlock b
                    => EpochInfo Identity
                    -> ConsensusConfig (BlockProtocol (HardForkBlock '[b]))
                    -> ConsensusConfig (BlockProtocol b)
projConsensusConfig ei =
      completeConsensusConfig (Proxy @(BlockProtocol b)) ei
    . getSingleEraConsensusConfig
    . hd
    . getPerEraConsensusConfig
    . hardForkConsensusConfigPerEra

projLedgerState :: LedgerState (HardForkBlock '[b]) -> LedgerState b
projLedgerState =
      State.currentState
    . Telescope.fromTZ
    . getHardForkState
    . getHardForkLedgerState

injLedgerState :: SystemStart -> LedgerState b -> LedgerState (HardForkBlock '[b])
injLedgerState systemStart =
      HardForkLedgerState
    . HardForkState
    . Telescope.TZ
    . State.Current (History.initBound systemStart)

projLedgerView :: proxy b
               -> LedgerView (BlockProtocol (HardForkBlock '[b]))
               -> LedgerView (BlockProtocol b)
projLedgerView _ =
      hardForkEraLedgerView
    . State.fromTZ

projConsensusState :: ConsensusState (BlockProtocol (HardForkBlock '[b]))
                   -> ConsensusState (BlockProtocol b)
projConsensusState =
      getSingleEraConsensusState
    . State.currentState
    . Telescope.fromTZ
    . getHardForkState

injConsensusState :: SystemStart
                  -> ConsensusState (BlockProtocol b)
                  -> ConsensusState (BlockProtocol (HardForkBlock '[b]))
injConsensusState systemStart =
      HardForkState
    . Telescope.TZ
    . State.Current (History.initBound systemStart)
    . SingleEraConsensusState

projTopLevelConfig :: forall b. SingleEraBlock b
                   => TopLevelConfig (HardForkBlock '[b]) -> TopLevelConfig b
projTopLevelConfig TopLevelConfig{..} = TopLevelConfig{
      configConsensus  = projConsensusConfig ei configConsensus
    , configLedger     = configLedger'
    , configBlock      = projBlockConfig configBlock
    }
  where
    (ei, configLedger') = projLedgerConfig configLedger

projForgeState :: proxy b -> ForgeState (HardForkBlock '[b]) -> ForgeState b
projForgeState _ = getSingleEraForgeState . hd . getPerEraForgeState

injForgeState :: proxy b -> ForgeState b -> ForgeState (HardForkBlock '[b])
injForgeState _ = PerEraForgeState . (:* Nil) . SingleEraForgeState

injHashInfo :: HashInfo (HeaderHash b)
            -> HashInfo (HeaderHash (HardForkBlock '[b]))
injHashInfo info = HashInfo {
      hashSize = hashSize info
    , getHash  = injHeaderHash <$> getHash info
    , putHash  = putHash info . projHeaderHash
    }

projInitChainDB :: InitChainDB m (HardForkBlock '[b]) -> InitChainDB m b
projInitChainDB initDB = InitChainDB.InitChainDB {
      InitChainDB.checkEmpty = InitChainDB.checkEmpty initDB
    , InitChainDB.addBlock   = InitChainDB.addBlock initDB . injBlock
    }

projApplyTxErr :: ApplyTxErr (HardForkBlock '[b]) -> ApplyTxErr b
projApplyTxErr (HardForkApplyTxErrFromEra err) =
      getSingleEraApplyTxErr
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
    . SingleEraApplyTxErr

projTipInfo :: TipInfo (HardForkBlock '[b]) -> TipInfo b
projTipInfo =
      getSingleEraTipInfo
    . unZ
    . getOneEraTipInfo

projAnnTip :: AnnTip (HardForkBlock '[b]) -> AnnTip b
projAnnTip (AnnTip s b nfo) = AnnTip s b (projTipInfo nfo)

injAnnTip :: AnnTip b -> AnnTip (HardForkBlock '[b])
injAnnTip (AnnTip s b nfo) =
    AnnTip s b (OneEraTipInfo (Z (SingleEraTipInfo nfo)))

projQuery :: Query (HardForkBlock '[b]) result
          -> (forall result'.
                  (result :~: HardForkQueryResult '[b] result')
               -> Query b result'
               -> a)
          -> a
projQuery (HardForkQuery (QZ qry)) k = k Refl qry
projQuery (HardForkQuery (QS qry)) _ = case qry of {}

injQuery :: Query b result
         -> Query (HardForkBlock '[b]) (HardForkQueryResult '[b] result)
injQuery = HardForkQuery . QZ

injEnvelopeErr :: OtherHeaderEnvelopeError b
               -> OtherHeaderEnvelopeError (HardForkBlock '[b])
injEnvelopeErr =
      HardForkEnvelopeErrFromEra
    . OneEraEnvelopeErr
    . Z
    . SingleEraEnvelopeErr

projUpdateForgeState :: forall b m.
                        Update m (ForgeState (HardForkBlock '[b]))
                     -> Update m (ForgeState b)
projUpdateForgeState = liftUpdate get set
  where
    get :: PerEraForgeState '[b] -> ForgeState b
    get = projForgeState (Proxy @b)

    set :: ForgeState b -> PerEraForgeState '[b] -> PerEraForgeState '[b]
    set = const . injForgeState (Proxy @b)
