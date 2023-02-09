module Cardano.Tools.DB (withImmutableDB) where

import           Cardano.Tools.DBAnalyser.Block.Cardano
                     (Args (CardanoBlockArgs))
import           Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import           Cardano.Tools.DBAnalyser.Types ()
import           Control.Tracer (nullTracer)
import           Ouroboros.Consensus.Cardano (CardanoBlock)
import           Ouroboros.Consensus.Config (configSecurityParam, configStorage)
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as InitStorage
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Shelley.Crypto (StandardCrypto)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (fromChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (SnapshotInterval (DefaultSnapshotInterval),
                     defaultDiskPolicy)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import           Ouroboros.Consensus.Util.ResourceRegistry (runWithTempRegistry,
                     withRegistry)

withImmutableDB ::
    FilePath ->
    FilePath ->
    (ImmutableDB.ImmutableDB IO (CardanoBlock StandardCrypto) -> IO a) ->
    IO a
withImmutableDB dbDirectory configFilePath action =
    withRegistry $ \registry -> do
        ProtocolInfo{pInfoInitLedger = genesisLedger, pInfoConfig = cfg} <-
            mkProtocolInfo (CardanoBlockArgs configFilePath Nothing)
        let chunkInfo = InitStorage.nodeImmutableDbChunkInfo (configStorage cfg)
            k = configSecurityParam cfg
            diskPolicy = defaultDiskPolicy k DefaultSnapshotInterval
            args' =
                Node.mkChainDbArgs
                    registry
                    InFuture.dontCheck
                    cfg
                    genesisLedger
                    chunkInfo
                    $ ChainDB.defaultArgs (Node.stdMkChainDbHasFS dbDirectory) diskPolicy
            chainDbArgs =
                args'
                    { ChainDB.cdbImmutableDbValidation = ImmutableDB.ValidateMostRecentChunk
                    , ChainDB.cdbVolatileDbValidation = VolatileDB.NoValidation
                    , ChainDB.cdbTracer = nullTracer
                    }
            (immutableDbArgs, _, _, _) = fromChainDbArgs chainDbArgs

        ImmutableDB.withDB (ImmutableDB.openDB immutableDbArgs runWithTempRegistry) action
