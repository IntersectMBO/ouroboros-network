
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}



module Main (main) where

import Prelude

import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )

import Control.Monad.Class.MonadST
    ( MonadST )
import Network.TypedProtocol.Codec
    ( Codec (..), PeerHasAgency (..), runDecoder )
import Ouroboros.Consensus.Byron.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock )
import Ouroboros.Consensus.Cardano.Block
    ( CodecConfig (..), HardForkBlock (..) )
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs, Codecs' (..), clientCodecs )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
    ( SupportedNetworkProtocolVersion (..) )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.Shelley.Protocol
    ( StandardCrypto )
import Ouroboros.Network.Block
    ( Tip (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersion (..) )
import Ouroboros.Network.Protocol.ChainSync.Type
    ( Message (..), ServerHasAgency (..), TokNextKind (..) )
import Test.Consensus.Cardano.Generators
    ()

-- cardano-ledger-byron
import Cardano.Chain.Slotting
    ( EpochSlots (..) )

-- containers
import Data.Map.Strict
    ( (!) )

-- QuickCheck
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )



import Data.List (isInfixOf)
import qualified Shelley.Spec.Ledger.API as SL
import Ouroboros.Consensus.Cardano.Block (AlonzoEra)
import Cardano.Binary (fromCBOR, toCBOR)
import qualified Codec.CBOR.Read  as CBOR
import qualified Codec.CBOR.Write as CBOR
import Cardano.Binary (Annotator)






import           Cardano.Crypto.Libsodium (sodiumInit)

import           Test.Tasty
import           Test.Util.Nightly

import qualified Test.Consensus.Cardano.ByronCompatibility (tests)
import qualified Test.Consensus.Cardano.Golden (tests)
import qualified Test.Consensus.Cardano.Serialisation (tests)
import qualified Test.ThreadNet.AllegraMary (tests)
import qualified Test.ThreadNet.Cardano (tests)
import qualified Test.ThreadNet.MaryAlonzo (tests)
import qualified Test.ThreadNet.ShelleyAllegra (tests)

main :: IO ()
main = sodiumInit >> defaultMainWithIohkNightly tests

tests :: TestTree
tests =
  testGroup "cardano"
  [ Test.Consensus.Cardano.ByronCompatibility.tests
  , Test.Consensus.Cardano.Golden.tests
  , Test.Consensus.Cardano.Serialisation.tests
  , Test.ThreadNet.AllegraMary.tests
  , Test.ThreadNet.Cardano.tests
  , Test.ThreadNet.MaryAlonzo.tests
  , Test.ThreadNet.ShelleyAllegra.tests
  , testProperty "adhoc" spec
  , testProperty "propRoundTripBlock" propRoundTripBlock
  ]

spec :: Property
spec = do
        forAll ((,) <$> genBlock <*> genTip) $ \(blk, tip) ->
            let
                codec = codecs (EpochSlots 432000) NodeToClientV_9 & cChainSyncCodec
                msg = MsgRollForward blk tip
                agency = ServerAgency (TokNext TokCanAwait)
             in
                monadicIO $ do
                    decoder <- run $ decode codec agency
                    run (runDecoder [encode codec agency msg] decoder) >>= \case
                        Right{} -> assert True
                        Left e  -> monitor (counterexample (show e)) >> assert False

type Block = CardanoBlock StandardCrypto

codecs
    :: (MonadST m)
    => EpochSlots
    -> NodeToClientVersion
    -> ClientCodecs Block m
codecs epochSlots nodeToClientV =
    clientCodecs cfg (supportedVersions ! nodeToClientV) nodeToClientV
  where
    supportedVersions = supportedNodeToClientVersions (Proxy @Block)
    cfg = CardanoCodecConfig byron shelley allegra mary alonzo
      where
        byron   = ByronCodecConfig epochSlots
        shelley = ShelleyCodecConfig
        allegra = ShelleyCodecConfig
        mary    = ShelleyCodecConfig
        alonzo  = ShelleyCodecConfig

genBlock :: Gen Block
genBlock =
    BlockAlonzo <$> arbitrary

genTip :: Gen (Tip Block)
genTip = frequency
    [ (1, pure TipGenesis)
    , (10, Tip <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

-----

propRoundTripBlock :: SL.Block (AlonzoEra StandardCrypto) -> Property
propRoundTripBlock blk =
      case CBOR.deserialiseFromBytes decoder bs of
        Right{} -> property True
        Left e@(CBOR.DeserialiseFailure _ _) ->
            counterexample (show e)
          $ label (show $ (isInfixOf "PlutusScript" (show blk), isInfixOf "TimelockScript" (show blk)))
          $ property False
  where
    bs = CBOR.toLazyByteString encoding

    encoding = toCBOR blk

    decoder = do
      _ <- fromCBOR @(Annotator (SL.Block (AlonzoEra StandardCrypto)))
      return ()
