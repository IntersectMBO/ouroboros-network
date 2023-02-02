{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | A tool to inspect the content of a block from various sources.
module Cardano.Tools.Block (BlockOptions (..), run, readChainPoint) where

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Tools.Block.JSON ()
import Cardano.Tools.DB (withImmutableDB)
import qualified Codec.CBOR.Read as CBOR
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as SHex
import qualified Data.ByteString.Base16.Lazy as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Ouroboros.Consensus.Block (
    CodecConfig,
    Point (..),
    SlotNo (SlotNo),
    WithOrigin (..),
    fromRawHash,
    pointToWithOriginRealPoint,
 )
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Cardano.Block (CodecConfig (..))
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.Common (BlockComponent (GetBlock))
import Ouroboros.Consensus.Storage.FS.API (Handle, HasFS, hGetAll, hPutAll, withFile)
import Ouroboros.Consensus.Storage.FS.API.Types (FsPath, OpenMode (ReadMode))
import Ouroboros.Consensus.Storage.ImmutableDB.API (getBlockComponent)
import Ouroboros.Consensus.Storage.Serialisation (decodeDisk)
import Ouroboros.Consensus.Util.IOLike (Exception, MonadThrow (throwIO))
import Text.Read (readMaybe)

data BlockOptions
    = ViewBlock
        { blockFile :: Maybe FsPath
        }
    | ExtractBlock
        { dbDirectory :: FilePath
        , cardanoConfigPath :: FilePath
        , point :: Point CBlock
        }

run :: HasFS IO h -> Handle h -> Handle h -> BlockOptions -> IO ()
run hasFS stdinIO stdoutIO options = do
    block <- case options of
        ViewBlock{blockFile} -> do
            case blockFile of
                Just file -> withFile hasFS file ReadMode $ viewBlock hasFS
                Nothing -> viewBlock hasFS stdinIO
        ExtractBlock{dbDirectory, cardanoConfigPath, point} ->
            Aeson.encode <$> readBlockFromDB dbDirectory cardanoConfigPath point
    void $ hPutAll hasFS stdoutIO block

readBlockFromDB :: FilePath -> FilePath -> Point CBlock -> IO CBlock
readBlockFromDB dbDirectory configFilePath point =
    withImmutableDB dbDirectory configFilePath $ \immutableDB ->
        case pointToWithOriginRealPoint point of
            Origin -> error "Cannot get block at origin"
            NotOrigin realPoint ->
                fromRight (error $ "Block at " <> show point <> " not found")
                    <$> getBlockComponent immutableDB GetBlock realPoint

data ViewBlockException
    = BadHexEncoding {reason :: String}
    | BadCBOREncoding {reason :: String}
    deriving (Show)

instance Exception ViewBlockException

type CBlock = CardanoBlock StandardCrypto

{- | Reads a hex-encoded representation of a CBOR encoded block and dump it to
 `stdout`.
-}
viewBlock ::
    (MonadThrow m) =>
    HasFS m h ->
    Handle h ->
    m LBS.ByteString
viewBlock hasFS hdl = do
    bytes <- hGetAll hasFS hdl
    cbor <- either (throwIO . BadHexEncoding) pure $ Hex.decode bytes
    Aeson.encode <$> parseBlock cbor

parseBlock ::
    MonadThrow m =>
    LBS.ByteString ->
    m CBlock
parseBlock bytes =
    throwParseErrors bytes $
        CBOR.deserialiseFromBytes (decodeDisk defaultCodecConfig) bytes

throwParseErrors ::
    MonadThrow m =>
    LBS.ByteString ->
    Either CBOR.DeserialiseFailure (LBS.ByteString, LBS.ByteString -> CBlock) ->
    m CBlock
throwParseErrors fullBytes = \case
    Right (trailing, f)
        | LBS.null trailing ->
            return $ f fullBytes
        | otherwise ->
            throwIO $
                BadCBOREncoding
                    ("Remaining unconsumed input: " <> show trailing)
    Left err -> throwIO $ BadCBOREncoding $ show err

defaultCodecConfig :: CodecConfig CBlock
defaultCodecConfig =
    CardanoCodecConfig
        (Byron.ByronCodecConfig $ EpochSlots 100)
        Shelley.ShelleyCodecConfig
        Shelley.ShelleyCodecConfig
        Shelley.ShelleyCodecConfig
        Shelley.ShelleyCodecConfig
        Shelley.ShelleyCodecConfig

readChainPoint :: String -> Maybe (Point (CardanoBlock StandardCrypto))
readChainPoint = \case
    "0" -> Just GenesisPoint
    chainPointStr ->
        case Text.splitOn "." (Text.pack chainPointStr) of
            [slotNoTxt, headerHashTxt] -> do
                slotNo <- SlotNo <$> readMaybe (Text.unpack slotNoTxt)
                pure $
                    BlockPoint slotNo $
                        fromRawHash
                            (Proxy @CBlock)
                            ( fromRight (error $ Text.unpack $ "Not a valid hash: " <> headerHashTxt) $
                                SHex.decode $ encodeUtf8 headerHashTxt
                            )
            _emptyOrSingularList ->
                Nothing
