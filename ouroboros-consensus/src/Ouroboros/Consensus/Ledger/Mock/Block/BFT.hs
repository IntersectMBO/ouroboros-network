{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.Mock.Block.BFT (
    SimpleBftBlock
  , SimpleBftHeader
  , SimpleBftExt(..)
  , SignedSimpleBft(..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.DSIGN
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Ledger.Mock.Run
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

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
  deriving (Condense, Show, Eq, NoUnexpectedThunks)

-- | Part of the block that gets signed
data SignedSimpleBft c c' = SignedSimpleBft {
      signedSimpleBft :: SimpleStdHeader c (SimpleBftExt c c')
    }
  deriving (Generic)

type instance BlockProtocol (SimpleBftBlock  c c') = Bft c'
type instance BlockProtocol (SimpleBftHeader c c') = BlockProtocol (SimpleBftBlock c c')

-- | Sanity check that block and header type synonyms agree
_simpleBFtHeader :: SimpleBftBlock c c' -> SimpleBftHeader c c'
_simpleBFtHeader = simpleHeader

{-------------------------------------------------------------------------------
  Evidence that SimpleBlock can support BFT
-------------------------------------------------------------------------------}

instance SignedHeader (SimpleBftHeader c c') where
  type Signed (SimpleBftHeader c c') = SignedSimpleBft c c'

  headerSigned = SignedSimpleBft . simpleHeaderStd

instance ( SimpleCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedSimpleBft c c')
         ) => HeaderSupportsBft c' (SimpleBftHeader c c') where
  headerBftFields _ = simpleBftExt . simpleHeaderExt

instance RunMockProtocol (Bft c') where
  mockProtocolMagicId  = const constructMockProtocolMagicId
  mockEncodeChainState = const encode
  mockDecodeChainState = const decode

instance ( SimpleCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedSimpleBft c c')
         )
      => RunMockBlock (Bft c') c (SimpleBftExt c c') where
  forgeExt cfg () SimpleBlock{..} = do
      ext :: SimpleBftExt c c' <- fmap SimpleBftExt $
        forgeBftFields cfg $
          SignedSimpleBft {
              signedSimpleBft = simpleHeaderStd
            }
      return SimpleBlock {
          simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
        , simpleBody   = simpleBody
        }
    where
      SimpleHeader{..} = simpleHeader

instance ( SimpleCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedSimpleBft c c')
         ) => SupportedBlock (SimpleBftBlock c c')

instance ( SimpleCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedSimpleBft c c')
         ) => ProtocolLedgerView (SimpleBftBlock c c') where
  ledgerConfigView _ = SimpleLedgerConfig
  protocolLedgerView _ _ = ()
  anachronisticProtocolLedgerView _ _ _ = Right ()

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
instance (Typeable c', SimpleCrypto c) => ToCBOR (SignedSimpleBft c c') where
  toCBOR = encode
