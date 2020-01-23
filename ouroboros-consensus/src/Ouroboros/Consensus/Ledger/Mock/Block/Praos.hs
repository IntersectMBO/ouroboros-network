{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.Mock.Block.Praos (
    SimplePraosBlock
  , SimplePraosHeader
  , SimplePraosExt(..)
  , SignedSimplePraos(..)
  ) where

import           Codec.CBOR.Decoding (decodeListLenOf)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (encodeListLen)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           GHC.Generics (Generic)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Crypto.KES
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock.Address
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Ledger.Mock.Run
import           Ouroboros.Consensus.Ledger.Mock.Stake
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

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
  deriving (Generic, Condense, Show, Eq, NoUnexpectedThunks)

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

-- | See 'ProtocolLedgerView' instance for why we need the 'AddrDist'
type instance BlockProtocol (SimplePraosBlock  c c') = Praos AddrDist c'
type instance BlockProtocol (SimplePraosHeader c c') = BlockProtocol (SimplePraosBlock c c')

-- | Sanity check that block and header type synonyms agree
_simplePraosHeader :: SimplePraosBlock c c' -> SimplePraosHeader c c'
_simplePraosHeader = simpleHeader

{-------------------------------------------------------------------------------
  Evidence that SimpleBlock can support BFT
-------------------------------------------------------------------------------}

instance PraosCrypto c' => SignedHeader (SimplePraosHeader c c') where
  type Signed (SimplePraosHeader c c') = SignedSimplePraos c c'

  headerSigned SimpleHeader{..} = SignedSimplePraos {
        signedSimplePraos = simpleHeaderStd
      , signedPraosFields = praosExtraFields (simplePraosExt simpleHeaderExt)
      }

instance ( SimpleCrypto c
         , PraosCrypto c'
         , Signable (PraosKES c') (SignedSimplePraos c c')
         ) => HeaderSupportsPraos AddrDist c' (SimplePraosHeader c c') where
  headerPraosFields _ = simplePraosExt . simpleHeaderExt

instance PraosCrypto c' => RunMockProtocol (Praos ext c') where
  mockProtocolMagicId  = const constructMockProtocolMagicId
  mockEncodeChainState = const encode
  mockDecodeChainState = const decode

instance PraosCrypto c' => Serialise (BlockInfo c') where
  encode BlockInfo {..} = mconcat
    [ encodeListLen 3
    , encode biSlot
    , toCBOR biRho
    , encode biStake
    ]
  decode = do
    decodeListLenOf 3
    biSlot  <- decode
    biRho   <- fromCBOR
    biStake <- decode
    return BlockInfo {..}

instance ( SimpleCrypto c
         , PraosCrypto c'
         , Signable (PraosKES c') (SignedSimplePraos c c')
         ) => RunMockBlock (Praos ext c') c (SimplePraosExt c c') where
  forgeExt cfg isLeader SimpleBlock{..} = do
      ext :: SimplePraosExt c c' <- fmap SimplePraosExt $
        forgePraosFields cfg
                         isLeader
                         $ \praosExtraFields ->
          SignedSimplePraos {
              signedSimplePraos = simpleHeaderStd
            , signedPraosFields = praosExtraFields
            }
      return SimpleBlock {
          simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
        , simpleBody   = simpleBody
        }
    where
      SimpleHeader{..} = simpleHeader

instance ( SimpleCrypto c
         , PraosCrypto c'
         , Signable (PraosKES c') (SignedSimplePraos c c')
         ) => SupportedBlock (SimpleBlock c (SimplePraosExt c c'))

-- | Praos needs a ledger that can give it the "active stake distribution"
--
-- TODO: Currently our mock ledger does not do this, and just assumes that all
-- the leaders have equal stake at all times. In a way this is not wrong: it
-- is just a different instantiation of the same consensus algorithm (see
-- documentation of 'LedgerView'). Ideally we'd change this however, but it
-- may not be worth it; it would be a bit of work, and after we have integrated
-- the Shelley rules, we'll have a proper instance anyway.
instance ( SimpleCrypto c
         , PraosCrypto c'
         , Signable (PraosKES c') (SignedSimplePraos c c')
         ) => ProtocolLedgerView (SimplePraosBlock c c') where
  ledgerConfigView _ =
      SimpleLedgerConfig

  protocolLedgerView PraosNodeConfig{..} _ =
      equalStakeDist praosExtConfig

  anachronisticProtocolLedgerView PraosNodeConfig{..} _ _ =
      Right $ equalStakeDist praosExtConfig

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
