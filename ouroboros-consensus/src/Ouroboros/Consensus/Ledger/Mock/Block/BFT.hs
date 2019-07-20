{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Mock.Block.BFT (
    SimpleBftBlock
  , SimpleBftHeader
  , SimpleBftExt(..)
  , SignedSimpleBft(..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR(..))
import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Ledger.Mock.Forge
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense
import qualified Ouroboros.Consensus.Util.SlotBounded as SB

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
  deriving (Condense, Show, Eq)

instance SerialiseTag (SimpleBftExt c c') where
  serialiseTag = 0

-- | Part of the block that gets signed
data SignedSimpleBft c c' = SignedSimpleBft {
      signedSimpleBft :: SimpleStdHeader c (SimpleBftExt c c')
    }
  deriving (Generic)

type instance BlockProtocol (SimpleBftBlock c c') =
  Bft c'

-- | Sanity check that block and header type synonyms agree
_simpleBFtHeader :: SimpleBftBlock c c' -> SimpleBftHeader c c'
_simpleBFtHeader = simpleHeader

{-------------------------------------------------------------------------------
  Evidence that SimpleBlock can support BFT
-------------------------------------------------------------------------------}

instance SimpleCrypto c => SignedHeader (SimpleBftHeader c c') where
  type Signed (SimpleBftHeader c c') = SignedSimpleBft c c'

  headerSigned = SignedSimpleBft . simpleHeaderStd

instance ( SimpleCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedSimpleBft c c')
         ) => HeaderSupportsBft c' (SimpleBftHeader c c') where
  headerBftFields _ = simpleBftExt . simpleHeaderExt

instance ( SimpleCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedSimpleBft c c')
         )
      => ForgeExt (Bft c') c (SimpleBftExt c c') where
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
  protocolLedgerView _ _ = ()
  anachronisticProtocolLedgerView _ _ _ = Just $ SB.unbounded ()

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
