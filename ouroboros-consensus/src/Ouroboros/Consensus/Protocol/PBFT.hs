{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns            #-}

module Ouroboros.Consensus.Protocol.PBFT (
    PBft
  , PBftLedgerView(..)
  , PBftFields(..)
  , PBftParams(..)
  , PBftIsLeader(..)
  , PBftIsLeaderOrNot(..)
  , forgePBftFields
  , genesisKeyCoreNodeId
    -- * Classes
  , PBftCrypto(..)
  , PBftMockCrypto
  , PBftCardanoCrypto
  , HeaderSupportsPBft(..)
  , ConstructContextDSIGN(..)
    -- * Type instances
  , NodeConfig(..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Genesis as CC.Genesis
import           Cardano.Crypto.DSIGN.Class
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (At))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT.ChainState (PBftChainState)
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS
import           Ouroboros.Consensus.Protocol.PBFT.Crypto
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  Fields that PBFT requires present in a block
-------------------------------------------------------------------------------}

data PBftFields c toSign = PBftFields {
      -- | The actual issuer of a block
      pbftIssuer    :: VerKeyDSIGN (PBftDSIGN c)
      -- | The stakeholder on whose behalf the block is being issued
    , pbftGenKey    :: VerKeyDSIGN (PBftDSIGN c)
    , pbftSignature :: SignedDSIGN (PBftDSIGN c) toSign
    }
  deriving (Generic)

deriving instance PBftCrypto c => Show (PBftFields c toSign)
deriving instance PBftCrypto c => Eq   (PBftFields c toSign)

instance (PBftCrypto c, Typeable toSign) => NoUnexpectedThunks (PBftFields c toSign)
  -- use generic instance

-- | Headers that can support PBFT
--
-- PBFT is the only protocol does can't use the standard 'Signed' class, because
-- it is the only protocol in which signatures are optional: not all blocks in
-- PBFT have a signature! This is necessary in order to support (what else)
-- epoch boundary blocks (EBBs), which are unsigned. Of course the intention
-- here is that 'headerPBftFields' will return 'Just' for regular blocks.
class ( HasHeader hdr
      , Signable (PBftDSIGN c) (OptSigned hdr)
      , BlockProtocol hdr ~ PBft cfg c
      ) => HeaderSupportsPBft cfg c hdr where
  type family OptSigned hdr :: *
  headerPBftFields :: cfg
                   -> hdr
                   -> Maybe (PBftFields c (OptSigned hdr), OptSigned hdr)

forgePBftFields :: ( MonadRandom m
                   , PBftCrypto c
                   , Signable (PBftDSIGN c) toSign
                   , ConstructContextDSIGN cfg c
                   )
                => NodeConfig (PBft cfg c)
                -> IsLeader (PBft cfg c)
                -> toSign
                -> m (PBftFields c toSign)
forgePBftFields cfg PBftIsLeader{..} toSign = do
    signature <- signedDSIGN ctxtDSIGN toSign pbftSignKey
    return $ PBftFields {
        pbftIssuer    = issuer
      , pbftGenKey    = genKey
      , pbftSignature = signature
      }
  where
    issuer = dlgCertDlgVerKey pbftDlgCert
    genKey = dlgCertGenVerKey pbftDlgCert
    ctxtDSIGN = constructContextDSIGN cfg (pbftExtConfig cfg) genKey

{-------------------------------------------------------------------------------
  Information PBFT requires from the ledger
-------------------------------------------------------------------------------}

data PBftLedgerView c = PBftLedgerView {
    -- | ProtocolParameters: map from genesis to delegate keys.
    pbftDelegates :: !(Bimap (PBftVerKeyHash c) (PBftVerKeyHash c))
  }
  deriving (Generic)

deriving instance PBftCrypto c => NoUnexpectedThunks (PBftLedgerView c)
  -- use generic instance

deriving instance Eq (PBftVerKeyHash c) => Eq (PBftLedgerView c)
deriving instance Show (PBftVerKeyHash c) => Show (PBftLedgerView c)

instance (Serialise (PBftVerKeyHash c), Ord (PBftVerKeyHash c))
      => Serialise (PBftLedgerView c) where
  encode (PBftLedgerView ds) = encode (Bimap.toList ds)
  decode = PBftLedgerView . Bimap.fromList <$> decode

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | Permissive BFT
--
-- As defined in https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec
data PBft cfg c

-- | Protocol parameters
data PBftParams = PBftParams {
      -- | Security parameter
      --
      -- Although the protocol proper does not have such a security parameter,
      -- we insist on it.
      pbftSecurityParam      :: !SecurityParam

      -- | Number of core nodes
    , pbftNumNodes           :: !Word64

      -- | Signature threshold
      --
      -- This bounds the proportion of the latest 'pbftSecurityParam'-many
      -- blocks which is allowed to be signed by any single key. The protocol
      -- proper is parameterized over the size of this window of recent blocks,
      -- but this implementation follows the specification by fixing that
      -- parameter to the ambient security parameter @k@.
    , pbftSignatureThreshold :: !Double
    }
  deriving (Generic, NoUnexpectedThunks)

-- | If we are a core node (i.e. a block producing node) we know which core
-- node we are, and we have the operational key pair and delegation certificate.
--
data PBftIsLeader c = PBftIsLeader {
      pbftCoreNodeId :: !CoreNodeId
    , pbftSignKey    :: !(SignKeyDSIGN (PBftDSIGN c))
    , pbftDlgCert    :: !(PBftDelegationCert c)
    }
  deriving (Generic)

instance PBftCrypto c => NoUnexpectedThunks (PBftIsLeader c)
 -- use generic instance

data PBftIsLeaderOrNot c
  = PBftIsALeader !(PBftIsLeader c)
  | PBftIsNotALeader
  deriving (Generic, NoUnexpectedThunks)

-- | (Static) node configuration
--
-- We explicitly allow for additional context here, so that we can provide
-- this context to the crypto functions.
data instance NodeConfig (PBft cfg c) = PBftNodeConfig {
      pbftParams    :: !PBftParams
    , pbftIsLeader  :: !(PBftIsLeaderOrNot c)
    , pbftExtConfig :: !cfg
    }
  deriving (Generic, NoUnexpectedThunks)

instance ( PBftCrypto c
         , Typeable c
         , NoUnexpectedThunks cfg
         , Typeable cfg
         , ConstructContextDSIGN cfg c
         )
      => OuroborosTag (PBft cfg c) where
  type ValidationErr (PBft cfg c) = PBftValidationErr c
  type CanValidate   (PBft cfg c) = HeaderSupportsPBft cfg c
  type CanSelect     (PBft cfg c) = HasHeader
  type NodeState     (PBft cfg c) = ()

  -- | We require two things from the ledger state:
  --
  --   - Protocol parameters, for the signature window and threshold.
  --   - The delegation map.
  type LedgerView     (PBft cfg c) = PBftLedgerView c
  type IsLeader       (PBft cfg c) = PBftIsLeader   c
  type ChainState     (PBft cfg c) = PBftChainState c

  protocolSecurityParam = pbftSecurityParam . pbftParams

  checkIsLeader PBftNodeConfig{pbftIsLeader, pbftParams} (SlotNo n) _l _cs =
      case pbftIsLeader of
        PBftIsNotALeader                           -> return Nothing

        -- We are the slot leader based on our node index, and the current
        -- slot number. Our node index depends which genesis key has delegated
        -- to us, see 'genesisKeyCoreNodeId'.
        PBftIsALeader credentials
          | n `mod` pbftNumNodes == fromIntegral i -> return (Just credentials)
          | otherwise                              -> return Nothing
          where
            PBftIsLeader{pbftCoreNodeId = CoreNodeId i} = credentials
            PBftParams{pbftNumNodes}                    = pbftParams

  applyChainState cfg@PBftNodeConfig{..} lv@(PBftLedgerView dms) (b :: hdr) chainState =
      case headerPBftFields pbftExtConfig b of
        Nothing ->
          -- EBB. Nothing to do
          return chainState
        Just (PBftFields{..}, signed) -> do
          -- Check that the issuer signature verifies, and that it's a delegate of a
          -- genesis key, and that genesis key hasn't voted too many times.
          case verifySignedDSIGN
                 (constructContextDSIGN cfg pbftExtConfig pbftGenKey)
                 pbftIssuer
                 signed
                 pbftSignature of
            Right () -> return ()
            Left err -> throwError $ PBftInvalidSignature err

          -- FIXME confirm that non-strict inequality is ok in general.
          -- It's here because EBBs have the same slot as the first block of their
          -- epoch.
          unless (At (blockSlot b) >= CS.lastSlot chainState)
            $ throwError PBftInvalidSlot

          case Bimap.lookupR (hashVerKey pbftIssuer) dms of
            Nothing -> throwError $ PBftNotGenesisDelegate (hashVerKey pbftIssuer) lv
            Just gk -> do
              let chainState'  = CS.prune winSize $ CS.insert gk (blockSlot b) $ chainState
                  totalSigners = CS.size          chainState'
                  gkSigners    = CS.countSignedBy chainState' gk
              when (totalSigners >= winSize && gkSigners > wt)
                $ throwError (PBftExceededSignThreshold (show (pbftSecurityParam, chainState')) totalSigners gkSigners)
              return $! chainState'
    where
      PBftParams{..} = pbftParams
      winSize = k
      SecurityParam (fromIntegral -> k) = pbftSecurityParam
      wt = floor $ pbftSignatureThreshold * fromIntegral winSize

  rewindChainState _ cs mSlot = CS.rewind mSlot cs

{-------------------------------------------------------------------------------
  Extract necessary context
-------------------------------------------------------------------------------}

class ConstructContextDSIGN cfg c where
  constructContextDSIGN :: proxy (PBft cfg c)
                        -> cfg
                        -> VerKeyDSIGN (PBftDSIGN c)
                        -> ContextDSIGN (PBftDSIGN c)

instance ConstructContextDSIGN ext PBftMockCrypto where
  constructContextDSIGN _p _cfg _genKey = ()

instance ConstructContextDSIGN ByronConfig PBftCardanoCrypto where
  constructContextDSIGN _p cfg genKey = (cfg, genKey)

{-------------------------------------------------------------------------------
  PBFT node order
-------------------------------------------------------------------------------}

-- | Determine the 'CoreNodeId' for a code node, based on the genesis key it
-- will sign blocks on behalf of.
--
-- In PBFT, the 'CoreNodeId' index is determined by the 0-based position in
-- the sort order of the genesis key hashes.
genesisKeyCoreNodeId :: CC.Genesis.Config
                     -> VerKeyDSIGN CardanoDSIGN
                        -- ^ The genesis verification key
                     -> Maybe CoreNodeId
genesisKeyCoreNodeId gc vkey =
  CoreNodeId <$> Set.lookupIndex (hashVerKey vkey) genesisKeyHashes
  where
    genesisKeyHashes :: Set.Set CC.Common.KeyHash
    genesisKeyHashes = CC.Genesis.unGenesisKeyHashes
                     . CC.Genesis.configGenesisKeyHashes
                     $ gc

{-------------------------------------------------------------------------------
  PBFT specific types
-------------------------------------------------------------------------------}

data PBftValidationErr c
  = PBftInvalidSignature String
  | PBftNotGenesisDelegate (PBftVerKeyHash c) (PBftLedgerView c)
  -- | The first number is the total number of signers observed.
  -- The second is the number of genesis key signers.
  -- This is given if both
  -- - The former is greater than or equal to the PBFT signature window.
  -- - The latter exceeds (strictly) the PBFT signature window multiplied by
  --   the PBFT signature threshold (rounded down).
  | PBftExceededSignThreshold String Int Int
  | PBftInvalidSlot

deriving instance (Show (PBftLedgerView c), PBftCrypto c) => Show (PBftValidationErr c)

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance PBftCrypto c => Condense (PBftFields c toSign) where
  condense PBftFields{..} = condense pbftSignature
