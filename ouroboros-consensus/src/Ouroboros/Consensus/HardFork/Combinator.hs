{-# LANGUAGE PatternSynonyms #-}

-- | The hard fork combinator
--
-- Intended for unqualified import
module Ouroboros.Consensus.HardFork.Combinator (module X) where

-- Defines 'SingleEraInfo' and 'LedgerEraInfo'
import           Ouroboros.Consensus.HardFork.Combinator.Info as X

-- Defines 'SingleEraBlock' and 'CanHardFork'
import           Ouroboros.Consensus.HardFork.Combinator.Abstract as X

-- Defines 'HardForkProtocol', 'HardForkBlock', and 'LedgerState',
-- as well as various config types.
import           Ouroboros.Consensus.HardFork.Combinator.Basics as X

-- Defines 'HasPartialConsensusConfig' and 'HasPartialLedgerConfig'
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig as X

-- Instances for 'BlockHasCodecConfig', 'GetHeader', 'HasHeader',
-- 'BasicEnvelopeValidation'
import           Ouroboros.Consensus.HardFork.Combinator.Block as X

-- Instances for 'IsLedger', 'ApplyBlock', 'UpdateLedger',
-- 'LedgerSupportsProtocol', 'HasHardForkHistory', 'ValidateEnvelope'
import           Ouroboros.Consensus.HardFork.Combinator.Ledger as X

-- Instances for 'ApplyTx', 'HasTxId', 'HasTxs'
import           Ouroboros.Consensus.HardFork.Combinator.Mempool as X

-- Instance for 'ConsensusProtocol'
import           Ouroboros.Consensus.HardFork.Combinator.Protocol as X

-- Instance for 'CommonProtocolParams'
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams as X ()

-- Instance for 'LedgerSupportsPeerSelection'
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.PeerSelection as X ()

-- Instances for 'ShowQuery' and 'QueryLedger'
-- Definition of 'Query', required for serialisation code
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query as X

-- Instance for 'ChainSelection'
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel as X

-- Re-export only the types that are useful in RunNode instances for HFC blocks
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras as X
                     (MismatchEraInfo (..), OneEraApplyTxErr (..),
                     OneEraBlock (..), OneEraGenTx (..), OneEraGenTxId (..),
                     OneEraHash (..), OneEraHeader (..), OneEraTipInfo (..),
                     PerEraCodecConfig (..), PerEraLedgerConfig (..))

-- Re-export types required to initialize 'ProtocolInfo'
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras as X
                     (PerEraBlockConfig (..), PerEraConsensusConfig (..),
                     PerEraStorageConfig (..))

-- Defines the various translation types required for concrete HFC instances
import           Ouroboros.Consensus.HardFork.Combinator.Translation as X

-- Combinator for 'BlockForging'
import           Ouroboros.Consensus.HardFork.Combinator.Forging as X
                     (HardForkForgeStateInfo (..), hardForkBlockForging)

-- Instances for 'RunNode' and 'ConfigSupportsNode'
import           Ouroboros.Consensus.HardFork.Combinator.Node as X ()

-- Instance for 'NodeInitStorage'
import           Ouroboros.Consensus.HardFork.Combinator.Node.InitStorage as X ()

-- Instance for 'BlockSupportsMetrics'
import           Ouroboros.Consensus.HardFork.Combinator.Node.Metrics as X ()

-- Definition of InPairs (required to define translations)
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as X
                     (InPairs (..))

-- Definition of Telescope (required to define serialisation code)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as X
                     (Telescope (..))

-- Definition of 'Mismatch' (required to define serialisation code)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match as X
                     (Mismatch (..))

-- Definition of HardForkState (required to define serialisation code)
-- Also export functions required to define 'protocolInfo'.
import           Ouroboros.Consensus.HardFork.Combinator.State as X
                     (HardForkState (..), initHardForkState)

-- Definition of 'InjectTx' (required to define tx injections)
import           Ouroboros.Consensus.HardFork.Combinator.InjectTxs as X
                     (InjectTx, InjectValidatedTx, cannotInjectTx,
                     cannotInjectValidatedTx, pattern InjectTx,
                     pattern InjectValidatedTx)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors as X
                     (Product2 (..))

-- Omitted from this export:
--
-- * "Ouroboros.Consensus.HardFork.Combinator.State"
--   This defines 'HardForkState', a wrapper around a 'Telescope'. We use this
--   to define 'HardForkLedgerState', 'HardForkLedgerView' as well as
--   'HardForkChainDepState', but the type itself should mostly be internal to
--   the hard fork combinator. We do export the constructor for it, as this may
--   be required for serialisation code.
--
-- * "module Ouroboros.Consensus.HardFork.Combinator.State.Infra"
--   This module is only separate from @.State@ to avoid some cyclic module
--   dependencies. Most modules internally to the HFC should import from
--   @.State@ directly, and outside of the HFC not even @.State@ should be
--   needed (see above).
--
-- * "Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView"
--   This is internal to "Ouroboros.Consensus.HardFork.Combinator.Protocol"
--
-- * "Ouroboros.Consensus.HardFork.Combinator.Protocol.State"
--   This is internal to "Ouroboros.Consensus.HardFork.Combinator.Protocol"
--
-- * "Ouroboros.Consensus.HardFork.Combinator.Degenerate"
--   This defines 'DegenFork', which is useful as a test case that the hard
--   fork combinator when applied to a single block results in a system
--   that is equivalent to just using that single block directly.
--
-- * "Ouroboros.Consensus.HardFork.Combinator.Embed.Unary"
--   Mostly used in combination with 'DegenFork'.
--
-- * "Ouroboros.Consensus.HardFork.Combinator.Embed.Nary"
--   Used for injection into n-ary sums. Alternative to @Unary@.
--
-- * Most of @Ouroboros.Consensus.HardFork.Combinator.SingleEra.*@
--   These types are primarily used internally to define the HFC types.
--   In a few cases some of the HFC types /are/ types from the SingleEra
--   module hierarchy directly; in such cases, we should export them from
--   this module.
--   TODO: Currently we only do this for @SingleEra.Info@, but we might also
--   need to do it for other types.
--
-- * Ouroboros.Consensus.HardFork.Combinator.Util.*
--   We omit most utility functions and types, which are for internal use. Some
--   exceptions the defintion of InPairs, which will be required to define
--   translations, and the definition of a Telescope, which might be needed to
--   define serialisation code.
