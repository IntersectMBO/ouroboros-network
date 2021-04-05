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

module Ouroboros.Consensus.Mock.Ledger.Block.Praos (
    SignedSimplePraos (..)
  , SimplePraosBlock
  , SimplePraosExt (..)
  , SimplePraosHeader
  , forgePraosExt
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), serialize')
import           Cardano.Crypto.KES
import           Cardano.Crypto.Util

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mock.Ledger.Address
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.Forge
import           Ouroboros.Consensus.Mock.Ledger.Stake
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.Storage.Serialisation

{-------------------------------------------------------------------------------
  Instantiate the @ext@ to suit Praos
-------------------------------------------------------------------------------}

-- | Simple block extended with the fields required for Praos
--
-- @c@  is crypto used for the block itself
-- @c'@ is crypto used for the consensus protocol
type SimplePraosBlock c c' = SimpleBlock c (SimplePraosExt c c')

-- | Header for Proas
type SimplePraosHeader c c' = SimpleHeader c (SimplePraosExt c c')

-- | Block extension required for Praos
newtype SimplePraosExt c c' = SimplePraosExt {
    simplePraosExt :: PraosFields c' (SignedSimplePraos c c')
  }
  deriving stock    (Generic, Show, Eq)
  deriving newtype  (Condense)
  deriving anyclass (NoThunks)

-- | Part of the block that gets signed
--
-- TODO: Right now we sign all of the extra Praos fields. This may or may not
-- be needed. <https://github.com/input-output-hk/cardano-ledger-specs/issues/530>
-- Of course, this Praos is merely a proof of concept so it doesn't really
-- matter either way; we include them here primarily to show that we can.
data SignedSimplePraos c c' = SignedSimplePraos {
      signedSimplePraos :: SimpleStdHeader c (SimplePraosExt c c')
    , signedPraosFields :: PraosExtraFields c'
    }

type instance BlockProtocol (SimplePraosBlock c c') = Praos c'

-- | Sanity check that block and header type synonyms agree
_simplePraosHeader :: SimplePraosBlock c c' -> SimplePraosHeader c c'
_simplePraosHeader = simpleHeader

{-------------------------------------------------------------------------------
  Customization of the generic infrastructure
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable c')
      => MockProtocolSpecific c (SimplePraosExt c c') where
  -- | See 'LedgerSupportsProtocol' instance for why we need the 'AddrDist'
  type MockLedgerConfig c (SimplePraosExt c c') = AddrDist

{-------------------------------------------------------------------------------
  Evidence that SimpleBlock can support Praos
-------------------------------------------------------------------------------}

type instance Signed (SimplePraosHeader c c') = SignedSimplePraos c c'

instance PraosCrypto c' => SignedHeader (SimplePraosHeader c c') where
  headerSigned SimpleHeader{..} = SignedSimplePraos {
        signedSimplePraos = simpleHeaderStd
      , signedPraosFields = praosExtraFields (simplePraosExt simpleHeaderExt)
      }

instance ( SimpleCrypto c
         , PraosCrypto c'
         ) => RunMockBlock c (SimplePraosExt c c') where
  mockNetworkMagic = const constructMockNetworkMagic

instance ( SimpleCrypto c
         , PraosCrypto c'
         , Signable (PraosKES c') (SignedSimplePraos c c')
         ) => BlockSupportsProtocol (SimpleBlock c (SimplePraosExt c c')) where
  validateView _ = praosValidateView (simplePraosExt . simpleHeaderExt)

instance ( SimpleCrypto c
         , PraosCrypto c'
         , Signable (PraosKES c') (SignedSimplePraos c c')
         ) => LedgerSupportsProtocol (SimplePraosBlock c c') where
  protocolLedgerView   cfg _  = pretendTicked $ stakeDist cfg
  ledgerViewForecastAt cfg st = constantForecastOf
                                 (pretendTicked $ stakeDist cfg)
                                 (getTipSlot st)

-- | Praos needs a ledger that can give it the "active stake distribution"
--
-- TODO: Currently our mock ledger does not do this, and just assumes that all
-- the leaders have equal stake at all times. In a way this is not wrong: it
-- is just a different instantiation of the same consensus algorithm (see
-- documentation of 'LedgerView'). Ideally we'd change this however, but it
-- may not be worth it; it would be a bit of work, and after we have integrated
-- the Shelley rules, we'll have a proper instance anyway.
stakeDist :: LedgerConfig (SimplePraosBlock c c') -> StakeDist
stakeDist = equalStakeDist . simpleMockLedgerConfig

pretendTicked :: StakeDist -> Ticked StakeDist
pretendTicked (StakeDist sd) = TickedStakeDist sd

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

type instance CannotForge (SimplePraosBlock c c') = Void

type instance ForgeStateInfo (SimplePraosBlock c c') = HotKey c'

type instance ForgeStateUpdateError (SimplePraosBlock c c') =
  HotKeyEvolutionError

forgePraosExt :: forall c c'.
                 ( SimpleCrypto c
                 , PraosCrypto c'
                 , Signable (PraosKES c') (SignedSimplePraos c c')
                 )
              => HotKey c'
              -> ForgeExt c (SimplePraosExt c c')
forgePraosExt hotKey = ForgeExt $ \_cfg isLeader SimpleBlock{..} ->
    let SimpleHeader{..} = simpleHeader

        ext :: SimplePraosExt c c'
        ext = SimplePraosExt $
          forgePraosFields isLeader hotKey $ \praosExtraFields ->
            SignedSimplePraos {
                signedSimplePraos = simpleHeaderStd
              , signedPraosFields = praosExtraFields
              }
    in SimpleBlock {
        simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
      , simpleBody   = simpleBody
      }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance PraosCrypto c' => Serialise (SimplePraosExt c c') where
  encode (SimplePraosExt PraosFields{..}) = mconcat [
        encodeSignedKES        praosSignature
      , encodePraosExtraFields praosExtraFields
      ]
  decode = do
      praosSignature   <- decodeSignedKES
      praosExtraFields <- decodePraosExtraFields
      return $ SimplePraosExt PraosFields{..}

instance (SimpleCrypto c, PraosCrypto c')
                        => ToCBOR (SignedSimplePraos c c') where
  toCBOR SignedSimplePraos{..} = mconcat [
        encode                 signedSimplePraos
      , encodePraosExtraFields signedPraosFields
      ]

instance (SimpleCrypto c, PraosCrypto c')
    => SignableRepresentation (SignedSimplePraos c c') where
  getSignableRepresentation = serialize'

encodePraosExtraFields :: PraosCrypto c' => PraosExtraFields c' -> CBOR.Encoding
encodePraosExtraFields PraosExtraFields{..} = mconcat [
      encode praosCreator
    , toCBOR praosRho
    , toCBOR praosY
    ]

decodePraosExtraFields :: forall s c'. PraosCrypto c'
                       => CBOR.Decoder s (PraosExtraFields c')
decodePraosExtraFields = do
    praosCreator <- decode
    praosRho     <- fromCBOR
    praosY       <- fromCBOR
    return PraosExtraFields{..}

instance PraosCrypto c' => EncodeDisk (SimplePraosBlock c c') (PraosChainDepState c')
  -- Default instance

instance PraosCrypto c' => DecodeDisk (SimplePraosBlock c c') (PraosChainDepState c')
  -- Default instance
