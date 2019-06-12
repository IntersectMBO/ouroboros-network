{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
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
  , forgePBftFields
    -- * Classes
  , PBftCrypto(..)
  , PBftMockCrypto
  , PBftCardanoCrypto
  , HeaderSupportsPBft(..)
    -- * Type instances
  , NodeConfig(..)
  ) where

import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Constraint
import           Data.Functor.Identity
import           Data.Reflection (Given (..), give)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Typeable (Proxy(..), Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import qualified Cardano.Chain.Common as CC.Common
import           Cardano.Crypto (ProtocolMagicId)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.NodeId (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Fields that PBFT requires present in a block
-------------------------------------------------------------------------------}

data PBftFields c toSign = PBftFields {
      pbftIssuer    :: VerKeyDSIGN (PBftDSIGN c)
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
                => NodeConfig (PBft c)
                -> (toSign -> Encoding)
                -> toSign
                -> m (PBftFields c toSign)
forgePBftFields PBftNodeConfig{..} encodeToSign toSign = do
    signature <- signedDSIGN encodeToSign toSign pbftSignKey
    return $ PBftFields {
        pbftIssuer    = pbftVerKey
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

      -- TODO These will ultimately be protocol parameters, but at the moment such
      -- parameters are missing in the ledger.

      -- | Size of the window over which to check the proportion of signed keys.
    , pbftSignatureWindow    :: Word64

      -- | Signature threshold. This represents the proportion of blocks in a
      -- pbftSignatureWindow-sized window which may be signed by any single key.
    , pbftSignatureThreshold :: Double
    }

instance (PBftCrypto c, Typeable c) => OuroborosTag (PBft c) where
  -- | (Static) node configuration
  data NodeConfig (PBft c) = PBftNodeConfig {
        pbftParams   :: PBftParams
      , pbftNodeId   :: NodeId
      , pbftSignKey  :: SignKeyDSIGN (PBftDSIGN c)
      , pbftVerKey   :: VerKeyDSIGN (PBftDSIGN c)
      }

  type ValidationErr   (PBft c) = PBftValidationErr c
  type SupportedHeader (PBft c) = HeaderSupportsPBft c
  type NodeState       (PBft c) = ()

  -- | We require two things from the ledger state:
  --
  --   - Protocol parameters, for the signature window and threshold.
  --   - The delegation map.
  type LedgerView     (PBft c) = PBftLedgerView c

  type IsLeader       (PBft c) = ()

  -- | Chain state consists of two things:
  --   - a list of the last 'pbftSignatureWindow' signatures.
  --   - The last seen block slot
  type ChainState     (PBft c) =
    Seq (PBftVerKeyHash c, SlotNo)

  protocolSecurityParam = pbftSecurityParam . pbftParams

  checkIsLeader PBftNodeConfig{..} (SlotNo n) _l _cs = do
      return $ case pbftNodeId of
                 RelayId _ -> Nothing -- relays are never leaders
                 CoreId  i -> if n `mod` pbftNumNodes == fromIntegral i
                                then Just ()
                                else Nothing
    where
      PBftParams{..}  = pbftParams

  applyChainState cfg@PBftNodeConfig{..} lv@(PBftLedgerView dms) (b :: hdr) chainState = do
      -- Check that the issuer signature verifies, and that it's a delegate of a
      -- genesis key, and that genesis key hasn't voted too many times.
      let proxy = Identity b
      case verifyPBftSigned
             (Proxy :: Proxy (c, hdr))
             pbftIssuer -- TODO genesis
             (encodeSigned proxy)
             pbftIssuer
             (headerSigned b)
             pbftSignature of
        Right () -> return ()
        Left err -> throwError $ PBftInvalidSignature err

      let (signers, lastSlot) = ( takeR winSize $ fst <$> chainState
                                , maybe (SlotNo 0) snd $ Seq.lookup (Seq.length chainState) chainState
                                )

      unless (blockSlot b > lastSlot)
        $ throwError PBftInvalidSlot

      case Bimap.lookupR (hashVerKey pbftIssuer) dms of
        Nothing -> throwError $ PBftNotGenesisDelegate (hashVerKey pbftIssuer) lv
        Just gk -> do
          when (Seq.length signers >= winSize
                && Seq.length (Seq.filter (== gk) signers) > wt)
            $ do throwError PBftExceededSignThreshold
          return $! takeR (winSize + 2*k) chainState Seq.|> (gk, blockSlot b)
    where
      PBftParams{..} = pbftParams
      PBftFields{..} = headerPBftFields cfg b
      winSize = fromIntegral pbftSignatureWindow
      SecurityParam (fromIntegral -> k) = pbftSecurityParam
      wt = floor $ pbftSignatureThreshold * fromIntegral winSize
      -- Take the rightmost n elements of a sequence
      takeR :: Integral i => i -> Seq a -> Seq a
      takeR (fromIntegral -> n) s = Seq.drop (Seq.length s - n - 1) s

  rewindChainState _ cs slot = if slot == SlotNo 0 then Just Seq.empty else
    case Seq.takeWhileL (\(_, s) -> s <= slot) cs of
        _ Seq.:<| _ -> Just cs
        _           -> Nothing

{-------------------------------------------------------------------------------
  PBFT specific types
-------------------------------------------------------------------------------}

data PBftValidationErr c
  = PBftInvalidSignature String
  | PBftNotGenesisDelegate (PBftVerKeyHash c) (PBftLedgerView c)
  | PBftExceededSignThreshold
  | PBftInvalidSlot

deriving instance (Show (PBftLedgerView c), PBftCrypto c) => Show (PBftValidationErr c)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

-- | Crypto primitives required by BFT
class ( Typeable c
      , DSIGNAlgorithm (PBftDSIGN c)
      , Show (PBftVerKeyHash c)
      , Ord (PBftVerKeyHash c)
      , Eq (PBftVerKeyHash c)
      , Show (PBftVerKeyHash c)
      ) => PBftCrypto c where
  type family PBftDSIGN c :: *

  -- Cardano stores a map of stakeholder IDs rather than the verification key
  -- directly. We make this family injective for convenience - whilst it's
  -- _possible_ that there could be non-injective instances, the chances of there
  -- being more than the two instances here are basically non-existent.
  type family PBftVerKeyHash c = (d :: *) | d -> c

  type family PBftSigningConstraints c hdr :: Constraint

  hashVerKey :: VerKeyDSIGN (PBftDSIGN c) -> PBftVerKeyHash c

  -- Abstracted version of `verifySignedDSIGN`
  --
  -- Since our signing constraints differ, we abstract this here such that we can
  -- correctly assemble the constraints in the real crypto case. See the
  -- documentation in Crypto/DSIGN/Cardano for more details.
  verifyPBftSigned :: forall hdr proxy. (PBftSigningConstraints c hdr)
                   => proxy (c, hdr)
                   -> VerKeyDSIGN (PBftDSIGN c) -- Genesis key - only used in the real impl
                   -> (Signed hdr -> Encoding)
                   -> VerKeyDSIGN (PBftDSIGN c)
                   -> Signed hdr
                   -> SignedDSIGN (PBftDSIGN c) (Signed hdr) -> Either String ()

data PBftMockCrypto

instance PBftCrypto PBftMockCrypto where
  type PBftDSIGN      PBftMockCrypto = MockDSIGN
  type PBftVerKeyHash PBftMockCrypto = VerKeyDSIGN MockDSIGN

  type PBftSigningConstraints PBftMockCrypto hdr = Signable MockDSIGN (Signed hdr)

  hashVerKey = id

  verifyPBftSigned _ _ = verifySignedDSIGN

data PBftCardanoCrypto

instance (Given ProtocolMagicId) => PBftCrypto PBftCardanoCrypto where
  type PBftDSIGN PBftCardanoCrypto      = CardanoDSIGN
  type PBftVerKeyHash PBftCardanoCrypto = CC.Common.KeyHash

  type PBftSigningConstraints PBftCardanoCrypto hdr
    = Given (VerKeyDSIGN CardanoDSIGN) :=> Signable CardanoDSIGN (Signed hdr)

  hashVerKey (VerKeyCardanoDSIGN pk) = CC.Common.hashKey pk

  -- This uses some hackery from the 'constraints' package to assemble a
  -- `HasSignTag` constraint from a `Given` constraint and a reified instance of
  -- the instance head/body relationship between the two.
  --
  -- See
  -- https://hackage.haskell.org/package/constraints-0.10.1/docs/Data-Constraint.html#v:-92--92-
  -- for details.
  verifyPBftSigned (_ :: proxy (PBftCardanoCrypto, hdr)) gkVerKey pSig issuer hSig sig
    = give gkVerKey $
      (verifySignedDSIGN
        pSig
        issuer
        hSig
        sig \\
        (ins :: Given (VerKeyDSIGN CardanoDSIGN) :- Signable CardanoDSIGN (Signed hdr) ))

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance PBftCrypto c => Condense (PBftFields c toSign) where
  condense PBftFields{..} = condense pbftSignature
