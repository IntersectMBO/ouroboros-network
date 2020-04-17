{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Mock.Ledger.Block.PBFT (
    SimplePBftBlock
  , SimplePBftHeader
  , SimplePBftExt(..)
  , SignedSimplePBft(..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.DSIGN
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (HasHeader (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Instantiate the @ext@ to suit PBFT
-------------------------------------------------------------------------------}

-- | Simple block extended with the fields required for PBFT
--
-- @c@  is crypto used for the block itself
-- @c'@ is crypto used for the consensus protocol
type SimplePBftBlock c c' = SimpleBlock c (SimplePBftExt c c')

-- | Header for PBFT
type SimplePBftHeader c c' = SimpleHeader c (SimplePBftExt c c')

-- | Block extension required for PBFT
newtype SimplePBftExt c c' = SimplePBftExt {
      simplePBftExt :: PBftFields c' (SignedSimplePBft c c')
    }
  deriving stock    (Generic, Show, Eq)
  deriving newtype  (Condense)
  deriving anyclass (NoUnexpectedThunks)

-- | Part of the block that gets signed
--
-- We just sign the standard header, i.e., without the PBFT extensions.
-- In particular, the signature does not cover the issuer.
--
-- The signature does not cover the body explicitly, but since the standard
-- header includes a hash of the body, the signature covers the body implicitly.
data SignedSimplePBft c c' = SignedSimplePBft {
      signedSimplePBft :: SimpleStdHeader c (SimplePBftExt c c')
    }
  deriving (Generic)

type instance NodeState     (SimplePBftBlock c c') = ()
type instance BlockProtocol (SimplePBftBlock c c') = PBft c'

-- | Sanity check that block and header type synonyms agree
_simplePBftHeader :: SimplePBftBlock c c' -> SimplePBftHeader c c'
_simplePBftHeader = simpleHeader

{-------------------------------------------------------------------------------
  Customization of the generic infrastructure
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, PBftCrypto c')
      => MockProtocolSpecific c (SimplePBftExt c c') where
  -- | PBFT requires the ledger view; for the mock ledger, this is constant
  type MockLedgerConfig c (SimplePBftExt c c') = PBftLedgerView c'

{-------------------------------------------------------------------------------
  Evidence that SimpleBlock can support PBFT
-------------------------------------------------------------------------------}

type instance Signed (SimplePBftHeader c c') = SignedSimplePBft c c'

instance SignedHeader (SimplePBftHeader c c') where
  headerSigned = SignedSimplePBft . simpleHeaderStd

instance ( SimpleCrypto c
         , PBftCrypto c'
         , Signable (PBftDSIGN c') (SignedSimplePBft c c')
         , ContextDSIGN (PBftDSIGN c') ~ ()
         , Serialise (PBftVerKeyHash c')
         ) => RunMockBlock c (SimplePBftExt c c') where
  forgeExt _cfg _updateState isLeader SimpleBlock{..} = do
      ext :: SimplePBftExt c c' <- fmap SimplePBftExt $
        forgePBftFields
          (const ())
          isLeader
          SignedSimplePBft { signedSimplePBft = simpleHeaderStd }
      return SimpleBlock {
          simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
        , simpleBody   = simpleBody
        }
    where
      SimpleHeader{..} = simpleHeader

  mockProtocolMagicId      = const constructMockProtocolMagicId
  mockEncodeConsensusState = const S.encodePBftState
  mockDecodeConsensusState = \cfg -> let k = configSecurityParam cfg
                                     in S.decodePBftState k (pbftWindowSize k)

instance ( SimpleCrypto c
         , Signable MockDSIGN (SignedSimplePBft c PBftMockCrypto)
         ) => BlockSupportsProtocol (SimplePBftBlock c PBftMockCrypto) where
  validateView _     = pbftValidateRegular () (simplePBftExt . simpleHeaderExt)
  selectView   _ hdr = (blockNo hdr, IsNotEBB)

-- | The ledger view is constant for the mock instantiation of PBFT
-- (mock blocks cannot change delegation)
instance ( SimpleCrypto c
         , Signable MockDSIGN (SignedSimplePBft c PBftMockCrypto)
         ) => LedgerSupportsProtocol (SimplePBftBlock c PBftMockCrypto) where
  protocolLedgerView    cfg _ =                           cfg
  ledgerViewForecastAt_ cfg _ = Just . constantForecastOf cfg

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance PBftCrypto c' => Serialise (SimplePBftExt c c') where
  encode (SimplePBftExt PBftFields{..}) = mconcat [
        encodeVerKeyDSIGN pbftIssuer
      , encodeVerKeyDSIGN pbftGenKey
      , encodeSignedDSIGN pbftSignature
      ]
  decode = do
      pbftIssuer    <- decodeVerKeyDSIGN
      pbftGenKey    <- decodeVerKeyDSIGN
      pbftSignature <- decodeSignedDSIGN
      return $ SimplePBftExt PBftFields{..}

instance SimpleCrypto c => Serialise (SignedSimplePBft c c')
instance (Typeable c', SimpleCrypto c) => ToCBOR (SignedSimplePBft c c') where
  toCBOR = encode
