{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Mock.Ledger.Block.BFT (
    SignedSimpleBft (..)
  , SimpleBftBlock
  , SimpleBftExt (..)
  , SimpleBftHeader
  , forgeBftExt
  ) where

import           Codec.Serialise (Serialise (..), serialise)
import qualified Data.ByteString.Lazy as BSL
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.DSIGN
import           Cardano.Crypto.Util

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.Forge
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.Storage.Serialisation

{-------------------------------------------------------------------------------
  Instantiate the @ext@ to suit BFT
-------------------------------------------------------------------------------}

-- | Simple block extended with the fields required for BFT
--
-- @c@  is crypto used for the block itself
-- @c'@ is crypto used for the consensus protocol
type SimpleBftBlock c c' = SimpleBlock c (SimpleBftExt c c')

-- | Header for BFT
type SimpleBftHeader c c' = SimpleHeader c (SimpleBftExt c c')

-- | Block extension required for BFT
newtype SimpleBftExt c c' = SimpleBftExt {
      simpleBftExt :: BftFields c' (SignedSimpleBft c c')
    }
  deriving stock   (Show, Eq)
  deriving newtype (Condense, NoThunks)

-- | Part of the block that gets signed
data SignedSimpleBft c c' = SignedSimpleBft {
      signedSimpleBft :: SimpleStdHeader c (SimpleBftExt c c')
    }
  deriving (Generic)

type instance BlockProtocol (SimpleBftBlock c c') = Bft c'

-- | Sanity check that block and header type synonyms agree
_simpleBFtHeader :: SimpleBftBlock c c' -> SimpleBftHeader c c'
_simpleBFtHeader = simpleHeader

{-------------------------------------------------------------------------------
  Customization of the generic infrastructure
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable c')
      => MockProtocolSpecific c (SimpleBftExt c c') where
  type MockLedgerConfig c (SimpleBftExt c c') = ()

{-------------------------------------------------------------------------------
  Evidence that SimpleBlock can support BFT
-------------------------------------------------------------------------------}

type instance Signed (SimpleBftHeader c c') = SignedSimpleBft c c'

instance SignedHeader (SimpleBftHeader c c') where
  headerSigned = SignedSimpleBft . simpleHeaderStd

instance ( SimpleCrypto c
         , BftCrypto c'
         ) => RunMockBlock c (SimpleBftExt c c') where
  mockNetworkMagic = const constructMockNetworkMagic

instance ( SimpleCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedSimpleBft c c')
         ) => BlockSupportsProtocol (SimpleBftBlock c c') where
  validateView _ = bftValidateView (simpleBftExt . simpleHeaderExt)

instance ( SimpleCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedSimpleBft c c')
         ) => LedgerSupportsProtocol (SimpleBftBlock c c') where
  protocolLedgerView   _ _ = TickedTrivial
  ledgerViewForecastAt _   = trivialForecast

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

type instance CannotForge (SimpleBftBlock c c') = Void

type instance ForgeStateInfo (SimpleBftBlock c c') = ()

type instance ForgeStateUpdateError (SimpleBftBlock c c') = Void

forgeBftExt :: forall c c'.
               ( SimpleCrypto c
               , BftCrypto c'
               , Signable (BftDSIGN c') (SignedSimpleBft c c')
               )
            => ForgeExt c (SimpleBftExt c c')
forgeBftExt = ForgeExt $ \cfg _ SimpleBlock{..} ->
    let SimpleHeader{..} = simpleHeader
        ext :: SimpleBftExt c c'
        ext = SimpleBftExt $
          forgeBftFields (configConsensus cfg) $
            SignedSimpleBft {
                signedSimpleBft = simpleHeaderStd
              }
    in SimpleBlock {
         simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
       , simpleBody   = simpleBody
       }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance BftCrypto c' => Serialise (SimpleBftExt c c') where
  encode (SimpleBftExt BftFields{..}) = mconcat [
        encodeSignedDSIGN bftSignature
      ]
  decode = do
      bftSignature <- decodeSignedDSIGN
      return $ SimpleBftExt BftFields{..}

instance SimpleCrypto c => Serialise (SignedSimpleBft c c')
instance SimpleCrypto c => SignableRepresentation (SignedSimpleBft c c') where
  getSignableRepresentation = BSL.toStrict . serialise

instance (Typeable c', SimpleCrypto c) => ToCBOR (SignedSimpleBft c c') where
  toCBOR = encode

instance EncodeDisk (SimpleBftBlock c c') ()
  -- Default instance

instance DecodeDisk (SimpleBftBlock c c') ()
  -- Default instance
