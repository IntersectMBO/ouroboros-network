{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | A tool to inspect the content of a block from various sources.
module Cardano.Tools.Block where

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Codec.CBOR.Read as CBOR
import Control.Concurrent.MVar (newMVar)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16.Lazy as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (encodeUtf8)
import Ouroboros.Consensus.Block (CodecConfig)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Cardano.Block (CodecConfig (..))
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Storage.FS.API (Handle, HasFS, hGetAll, hPutAll, hPutAllStrict, withFile)
import Ouroboros.Consensus.Storage.FS.API.Types (FsPath, OpenMode (ReadMode))
import Ouroboros.Consensus.Storage.FS.Handle (HandleOS (..))
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk, decodeDisk)
import Ouroboros.Consensus.Util.IOLike (Exception, MonadThrow (throwIO))
import System.IO (stdin, stdout)

data BlockOptions = ViewBlock
    { blockFile :: Maybe FsPath
    , -- TODO remove if not useful in the long run?
      cardanoConfigPath :: FsPath
    }

run :: MonadThrow m => HasFS m h -> Handle h -> Handle h -> BlockOptions -> m ()
run hasFS stdinIO stdoutIO options = do
    block <- case options of
        ViewBlock{blockFile, cardanoConfigPath} ->
            case blockFile of
                Just file -> withFile hasFS file ReadMode $ viewBlock hasFS cardanoConfigPath
                Nothing -> viewBlock hasFS cardanoConfigPath stdinIO
    void $ hPutAll hasFS stdoutIO block

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
    FsPath ->
    Handle h ->
    m LBS.ByteString
viewBlock hasFS _configFile hdl = do
    bytes <- hGetAll hasFS hdl
    cbor <- either (throwIO . BadHexEncoding) pure $ Hex.decode bytes
    encodeUtf8 . Text.pack . show <$> parseBlock cbor

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
