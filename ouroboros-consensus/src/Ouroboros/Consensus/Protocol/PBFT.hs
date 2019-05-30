{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns            #-}

module Ouroboros.Consensus.Protocol.PBFT (
    PBft
  , PBftLedgerView(..)
  , PBftParams(..)
    -- * Classes
  , PBftCrypto(..)
  , PBftMockCrypto
  , PBftCardanoCrypto
  , BlockSupportsPBft
    -- * Type instances
  , NodeConfig(..)
  , Payload(..)
  ) where

import           Codec.Serialise (Serialise (..))
import qualified Codec.Serialise.Decoding as Dec
import qualified Codec.Serialise.Encoding as Enc
import           Control.Monad.Except
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Functor.Identity
import           Data.Reflection (Given (..))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Genesis as CC.Genesis
import           Cardano.Crypto (ProtocolMagicId)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.Node (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Test
import           Ouroboros.Consensus.Util.Condense

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

      -- | Genesis config
      --
      -- TODO: This doesn't really belong here; PBFT the consensus algorithm
      -- does not require it.
    , pbftGenesisConfig      :: CC.Genesis.Config
    }

class ( HasPayload (PBft c) b
      , Signable (PBftDSIGN c) (PreHeader b)
      ) => BlockSupportsPBft c b where

instance (PBftCrypto c, Typeable c) => OuroborosTag (PBft c) where
  -- | The BFT payload is just the issuer and signature
  data Payload (PBft c) ph = PBftPayload {
        pbftIssuer    :: VerKeyDSIGN (PBftDSIGN c)
      , pbftSignature :: SignedDSIGN (PBftDSIGN c) ph
      }
    deriving (Generic)

  -- | (Static) node configuration
  data NodeConfig (PBft c) = PBftNodeConfig {
        pbftParams   :: PBftParams
      , pbftNodeId   :: NodeId
      , pbftSignKey  :: SignKeyDSIGN (PBftDSIGN c)
      , pbftVerKey   :: VerKeyDSIGN (PBftDSIGN c)
      }

  type ValidationErr  (PBft c) = PBftValidationErr c
  type SupportedBlock (PBft c) = BlockSupportsPBft c
  type NodeState      (PBft c) = ()

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

  mkPayload proxy PBftNodeConfig{..} _proof preheader = do
      signature <- signedDSIGN (encodePreHeader proxy) preheader pbftSignKey
      return $ PBftPayload {
          pbftIssuer = pbftVerKey
        , pbftSignature = signature
        }

  checkIsLeader PBftNodeConfig{..} (SlotNo n) _l _cs = do
      return $ case pbftNodeId of
                 RelayId _ -> Nothing -- relays are never leaders
                 CoreId  i -> if n `mod` pbftNumNodes == fromIntegral i
                                then Just ()
                                else Nothing
    where
      PBftParams{..}  = pbftParams

  applyChainState cfg@PBftNodeConfig{..} lv@(PBftLedgerView dms) b chainState = do
      -- Check that the issuer signature verifies, and that it's a delegate of a
      -- genesis key, and that genesis key hasn't voted too many times.
      let proxy = Identity b
      case verifySignedDSIGN
             (encodePreHeader proxy)
             (pbftIssuer payload)
             (blockPreHeader b)
             (pbftSignature payload) of
        Right () -> return ()
        Left err -> throwError $ PBftInvalidSignature err

      let (signers, lastSlot) = ( takeR winSize $ fst <$> chainState
                                , maybe (SlotNo 0) snd $ Seq.lookup (Seq.length chainState) chainState
                                )

      unless (blockSlot b > lastSlot)
        $ throwError PBftInvalidSlot

      case Bimap.lookup (hashVerKey $ pbftIssuer payload) $ Bimap.twist dms of
        Nothing -> throwError $ PBftNotGenesisDelegate (hashVerKey $ pbftIssuer payload) lv
        Just gk -> do
          when (Seq.length signers >= winSize
                && Seq.length (Seq.filter (== gk) signers) > wt)
            $ do throwError PBftExceededSignThreshold
          return $! takeR (winSize + 2*k) chainState Seq.|> (gk, blockSlot b)
    where
      PBftParams{..}  = pbftParams
      payload = blockPayload cfg b
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


deriving instance PBftCrypto c => Show     (Payload (PBft c) ph)
deriving instance PBftCrypto c => Eq       (Payload (PBft c) ph)
deriving instance PBftCrypto c => Ord      (Payload (PBft c) ph)
instance PBftCrypto c => Condense (Payload (PBft c) ph) where
    condense (PBftPayload _ sig) = condense sig

instance (DSIGNAlgorithm (PBftDSIGN c)) => Serialise (Payload (PBft c) ph) where
  encode (PBftPayload issuer sig) = mconcat
    [ Enc.encodeListLen 2
    , encodeVerKeyDSIGN issuer
    , encodeSignedDSIGN sig
    ]
  decode = do
    Dec.decodeListLenOf 2
    PBftPayload <$> decodeVerKeyDSIGN <*> decodeSignedDSIGN

{-------------------------------------------------------------------------------
  BFT specific types
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

  hashVerKey :: VerKeyDSIGN (PBftDSIGN c) -> PBftVerKeyHash c

data PBftMockCrypto

instance PBftCrypto PBftMockCrypto where
  type PBftDSIGN      PBftMockCrypto = MockDSIGN
  type PBftVerKeyHash PBftMockCrypto = VerKeyDSIGN MockDSIGN

  hashVerKey = id

data PBftCardanoCrypto

instance (Given ProtocolMagicId) => PBftCrypto PBftCardanoCrypto where
  type PBftDSIGN PBftCardanoCrypto      = CardanoDSIGN
  type PBftVerKeyHash PBftCardanoCrypto = CC.Common.KeyHash

  hashVerKey (VerKeyCardanoDSIGN pk) = CC.Common.hashKey pk
