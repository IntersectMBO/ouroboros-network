{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Mock.Block.TPraos (
    SimpleTPraosBlock
  , SimpleTPraosHeader
  , SimpleTPraosExt(..)
  , SignedSimpleTPraos(..)
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Data.Coerce (coerce)
import           GHC.Generics (Generic)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF.Class (CertifiedVRF(..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock.Address
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Ledger.Mock.Forge
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Protocol.TPraos.Util
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense
import qualified Ouroboros.Consensus.Util.SlotBounded as SB

import BlockChain (BHBody(..))
import Keys (VKey, pattern VKey)
import OCert (OCert(..))

{-------------------------------------------------------------------------------
  Instantiate the @ext@ to suit TPraos
-------------------------------------------------------------------------------}

type TPraosSigned c = BHBody (TPraosHash c) (TPraosDSIGN c) (TPraosKES c)

-- | Simple block extended with the fields required for TPraos
--
-- @c@  is crypto used for the block itself
-- @c'@ is crypto used for the consensus protocol
type SimpleTPraosBlock c c' = SimpleBlock c (SimpleTPraosExt c')

-- | Header for Proas
type SimpleTPraosHeader c c' = SimpleHeader c (SimpleTPraosExt c')

-- | Block extension required for TPraos
newtype SimpleTPraosExt c = SimpleTPraosExt {
    simpleTPraosExt :: TPraosFields c (TPraosSigned c)
  }
  deriving (Generic, Show)

-- | Since the simple ledger does not evaluate the various concerns we need for
-- operation of Praos (including things like calculating the epoch nonce, the
-- overlay schedule, and the stake distribution), we instead hard-code these
-- things in the node config.
--
-- As a result, these will not change as the chain evolves; we will have the
-- same nonce for each epoch, the same leader selection threshold and the same
-- overlay schedule.
type instance BlockProtocol (SimpleTPraosBlock c c') =
  ExtNodeConfig (TPraosLedgerView c') (TPraos c')

-- | Sanity check that block and header type synonyms agree
_simpleTPraosHeader :: SimpleTPraosBlock c c' -> SimpleTPraosHeader c c'
_simpleTPraosHeader = simpleHeader

{-------------------------------------------------------------------------------
  Evidence that SimpleBlock can support BFT
-------------------------------------------------------------------------------}

instance TPraosCrypto c' => SignedHeader (SimpleTPraosHeader c c') where
  type Signed (SimpleTPraosHeader c c')
    = TPraosSigned c'

  headerSigned SimpleHeader{..} = undefined

instance ( SimpleCrypto c
         , TPraosCrypto c'
         , Signable (TPraosKES c') (TPraosSigned c')
         ) => HeaderSupportsTPraos c' (SimpleTPraosHeader c c') where
  headerToBHeader _ = undefined

instance ( SimpleCrypto c
         , TPraosCrypto c'
         , Signable (TPraosKES c') (TPraosSigned c')
         ) => ForgeExt (ExtNodeConfig ext (TPraos c')) c (SimpleTPraosExt c') where
  forgeExt cfg isLeader SimpleBlock{..} = do
      ext :: SimpleTPraosExt c' <- fmap SimpleTPraosExt $
        forgeTPraosFields (encNodeConfigP cfg)
                         isLeader
                         $ \praosToSign ->
          BHBody
            { bheaderPrev = fudge simpleHeaderHash
            , bheaderVk = ocertVkCold
                        . tpraosIsCoreNodeOpCert
                        . tpraosIsCoreNode
                        $ isLeader
              -- TODO This needs to be a VRF VKey, but at the moment there is no
              -- such thing
            , bheaderVrfVk = fudge $ tptsVrfVk praosToSign
            , bheaderSlot = convertSlot
                          . simpleSlotNo
                          $ simpleHeaderStd
            , bheaderEta = fudge $ certifiedNatural
                         $ tptsEta praosToSign
            , bheaderPrfEta = certifiedProof
                            $ tptsEta praosToSign


            }
      return SimpleBlock {
          simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
        , simpleBody   = simpleBody
        }
    where
      SimpleHeader{..} = simpleHeader
      fudge = const undefined

instance ( SimpleCrypto c
         , TPraosCrypto c'
         , Signable (TPraosKES c') (TPraosSigned c')
         ) => SupportedBlock (SimpleBlock c (SimpleTPraosExt c'))

-- | TPraos needs a ledger that can give it the "active stake distribution"
--
-- TODO: Currently our mock ledger does not do this, and just assumes that all
-- the leaders have equal stake at all times. In a way this is not wrong: it
-- is just a different instantiation of the same consensus algorithm (see
-- documentation of 'LedgerView'). Ideally we'd change this however, but it
-- may not be worth it; it would be a bit of work, and after we have integrated
-- the Shelley rules, we'll have a proper instance anyway.
instance ( SimpleCrypto c
         , TPraosCrypto c'
         , Signable (TPraosKES c') (TPraosSigned c')
         ) => ProtocolLedgerView (SimpleTPraosBlock c c') where
  protocolLedgerView (EncNodeConfig _ lv) _ = lv

  anachronisticProtocolLedgerView (EncNodeConfig _ lv) _ _ =
      Just $ SB.unbounded lv

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance TPraosCrypto c' => Serialise (SimpleTPraosExt c') where
  encode (SimpleTPraosExt TPraosFields{..}) = mconcat [
        encodeSignedKES         tpraosSignature
      , encodeTPraosToSign      tpraosToSign
      ]
  decode = do
      praosSignature   <- decodeSignedKES
      praosExtraFields <- decodeTPraosToSign
      return $ SimpleTPraosExt TPraosFields{..}

encodeTPraosToSign :: TPraosCrypto c' => TPraosToSign c' -> CBOR.Encoding
encodeTPraosToSign TPraosToSign{..} = mconcat [
      encode praosCreator
    , toCBOR praosRho
    , toCBOR praosY
    ]

decodeTPraosToSign :: forall s c'. TPraosCrypto c'
                       => CBOR.Decoder s (TPraosToSign c')
decodeTPraosToSign = do
    praosCreator <- decode
    praosRho     <- fromCBOR
    praosY       <- fromCBOR
    return TPraosToSign{..}
