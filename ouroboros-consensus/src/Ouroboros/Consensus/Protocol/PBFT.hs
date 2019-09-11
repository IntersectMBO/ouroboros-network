{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
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
  , forgePBftFields
  , genesisKeyCoreNodeId
    -- * Classes
  , PBftCrypto(..)
  , PBftMockCrypto
  , PBftCardanoCrypto
  , HeaderSupportsPBft(..)
    -- * Type instances
  , NodeConfig(..)
  ) where

import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Reflection (give)
import qualified Data.Set as Set
import           Data.Typeable (Proxy (..), Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Genesis as CC.Genesis
import           Cardano.Crypto.DSIGN.Class

import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..))
import           Ouroboros.Network.Point (WithOrigin (At))

import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT.ChainState (PBftChainState)
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS
import           Ouroboros.Consensus.Protocol.PBFT.Crypto
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

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

class ( HasHeader hdr
      , SignedHeader hdr
      , PBftSigningConstraints c hdr
      ) => HeaderSupportsPBft c hdr where
  headerPBftFields :: proxy (PBft c) -> hdr -> PBftFields c (Signed hdr)

forgePBftFields :: ( MonadRandom m
                   , PBftCrypto c
                   , Signable (PBftDSIGN c) toSign
                   )
                => IsLeader (PBft c)
                -> toSign
                -> m (PBftFields c toSign)
forgePBftFields PBftIsLeader{..} toSign = do
    signature <- signedDSIGN toSign pbftSignKey
    return $ PBftFields {
        pbftIssuer    = dlgCertDlgVerKey pbftDlgCert
      , pbftGenKey    = dlgCertGenVerKey pbftDlgCert
      , pbftSignature = signature
      }

{-------------------------------------------------------------------------------
  Information PBFT requires from the ledger
-------------------------------------------------------------------------------}

data PBftLedgerView c = PBftLedgerView {
    -- | ProtocolParameters: map from genesis to delegate keys.
    pbftDelegates :: Bimap (PBftVerKeyHash c) (PBftVerKeyHash c)
  }

deriving instance (Show (PBftVerKeyHash c)) => Show (PBftLedgerView c)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | Permissive BFT
--
-- As defined in https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec
data PBft c

-- | Protocol parameters
data PBftParams = PBftParams {
      -- | Security parameter
      --
      -- Although the protocol proper does not have such a security parameter,
      -- we insist on it.
      pbftSecurityParam      :: SecurityParam

      -- | Number of core nodes
    , pbftNumNodes           :: Word64

      -- | Signature threshold
      --
      -- This bounds the proportion of the latest 'pbftSecurityParam'-many
      -- blocks which is allowed to be signed by any single key. The protocol
      -- proper is parameterized over the size of this window of recent blocks,
      -- but this implementation follows the specification by fixing that
      -- parameter to the ambient security parameter @k@.
    , pbftSignatureThreshold :: Double
    }

-- | If we are a core node (i.e. a block producing node) we know which core
-- node we are, and we have the operational key pair and delegation certificate.
--
data PBftIsLeader c = PBftIsLeader {
      pbftCoreNodeId :: CoreNodeId
    , pbftSignKey    :: SignKeyDSIGN (PBftDSIGN c)
    , pbftDlgCert    :: PBftDelegationCert c
    }

instance (PBftCrypto c, Typeable c) => OuroborosTag (PBft c) where
  -- | (Static) node configuration
  data NodeConfig (PBft c) = PBftNodeConfig {
        pbftParams   :: PBftParams
      , pbftIsLeader :: Maybe (PBftIsLeader c)
      }

  type ValidationErr   (PBft c) = PBftValidationErr c
  type SupportedHeader (PBft c) = HeaderSupportsPBft c
  type NodeState       (PBft c) = ()

  -- | We require two things from the ledger state:
  --
  --   - Protocol parameters, for the signature window and threshold.
  --   - The delegation map.
  type LedgerView     (PBft c) = PBftLedgerView c
  type IsLeader       (PBft c) = PBftIsLeader   c
  type ChainState     (PBft c) = PBftChainState c

  protocolSecurityParam = pbftSecurityParam . pbftParams

  checkIsLeader PBftNodeConfig{pbftIsLeader, pbftParams} (SlotNo n) _l _cs =
      case pbftIsLeader of
        Nothing                                    -> return Nothing

        -- We are the slot leader based on our node index, and the current
        -- slot number. Our node index depends which genesis key has delegated
        -- to us, see 'genesisKeyCoreNodeId'.
        Just credentials
          | n `mod` pbftNumNodes == fromIntegral i -> return (Just credentials)
          | otherwise                              -> return Nothing
          where
            PBftIsLeader{pbftCoreNodeId = CoreNodeId i} = credentials
            PBftParams{pbftNumNodes}                    = pbftParams

  applyChainState cfg@PBftNodeConfig{..} lv@(PBftLedgerView dms) (b :: hdr) chainState = do
      -- Check that the issuer signature verifies, and that it's a delegate of a
      -- genesis key, and that genesis key hasn't voted too many times.
      case verifyPBftSigned
             (Proxy :: Proxy (c, hdr))
             pbftGenKey
             pbftIssuer
             (headerSigned b)
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
      PBftFields{..} = headerPBftFields cfg b
      winSize = k
      SecurityParam (fromIntegral -> k) = pbftSecurityParam
      wt = floor $ pbftSignatureThreshold * fromIntegral winSize

  rewindChainState _ cs mSlot = CS.rewind mSlot cs

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
  Data.Reflection.give (CC.Genesis.configProtocolMagicId gc) $
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
