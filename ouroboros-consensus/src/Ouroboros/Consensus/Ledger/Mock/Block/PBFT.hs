{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Mock.Block.PBFT (
    SimplePBftBlock
  , SimplePBftHeader
  , SimplePBftExt(..)
  , SignedSimplePBft(..)
  ) where

import           Codec.Serialise (Serialise (..))
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Ledger.Mock.Forge
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util (Empty)
import           Ouroboros.Consensus.Util.Condense
import qualified Ouroboros.Consensus.Util.SlotBounded as SB

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
  deriving (Generic, Condense, Show, Eq)

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

-- | PBFT requires the ledger view; for the mock ledger, this is constant
type instance BlockProtocol (SimplePBftBlock c c') =
  ExtNodeConfig (PBftLedgerView c') (PBft c')

-- | Sanity check that block and header type synonyms agree
_simplePBftHeader :: SimplePBftBlock c c' -> SimplePBftHeader c c'
_simplePBftHeader = simpleHeader

{-------------------------------------------------------------------------------
  Evidence that SimpleBlock can support PBFT
-------------------------------------------------------------------------------}

instance SimpleCrypto c => SignedHeader (SimplePBftHeader c c') where
  type Signed (SimplePBftHeader c c') = SignedSimplePBft c c'

  headerSigned = SignedSimplePBft . simpleHeaderStd
  encodeSigned = const encode

instance ( SimpleCrypto c
         , Signable MockDSIGN ~ Empty
         ) => HeaderSupportsPBft PBftMockCrypto (SimplePBftHeader c PBftMockCrypto) where
  headerPBftFields _ = simplePBftExt . simpleHeaderExt

instance ( SimpleCrypto c
         , PBftCrypto c'
         , Signable (PBftDSIGN c') ~ Empty
         ) => ForgeExt (ExtNodeConfig ext (PBft c'))
                       c
                       (SimplePBftExt c c') where
  forgeExt cfg () SimpleBlock{..} = do
      ext :: SimplePBftExt c c' <- fmap SimplePBftExt $
        forgePBftFields (encNodeConfigP cfg) encode $
          SignedSimplePBft {
              signedSimplePBft = simpleHeaderStd
            }
      return SimpleBlock {
          simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
        , simpleBody   = simpleBody
        }
    where
      SimpleHeader{..} = simpleHeader

instance ( SimpleCrypto c
         , Signable MockDSIGN ~ Empty
         ) => SupportedBlock (SimplePBftBlock c PBftMockCrypto)

-- | The ledger view is constant for the mock instantiation of PBFT
-- (mock blocks cannot change delegation)
instance ( SimpleCrypto c
         , Signable MockDSIGN ~ Empty
         ) => ProtocolLedgerView (SimplePBftBlock c PBftMockCrypto) where
  protocolLedgerView (EncNodeConfig _ pbftParams) _ls =
      pbftParams
  anachronisticProtocolLedgerView (EncNodeConfig _ pbftParams) _ _ =
      Just $ SB.unbounded pbftParams

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance PBftCrypto c' => Serialise (SimplePBftExt c c') where
  encode (SimplePBftExt PBftFields{..}) = mconcat [
        encodeVerKeyDSIGN pbftIssuer
      , encodeSignedDSIGN pbftSignature
      ]
  decode = do
      pbftIssuer    <- decodeVerKeyDSIGN
      pbftSignature <- decodeSignedDSIGN
      return $ SimplePBftExt PBftFields{..}

instance SimpleCrypto c => Serialise (SignedSimplePBft c c')
