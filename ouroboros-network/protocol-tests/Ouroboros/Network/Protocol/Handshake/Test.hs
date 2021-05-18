{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Network.Protocol.Handshake.Test where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List (nub)
import           Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import           GHC.Generics

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR

import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadThrow ( MonadCatch
                                                , MonadThrow
                                                )
import           Control.Tracer (nullTracer)

import           Network.TypedProtocol.Proofs

import           Test.Ouroboros.Network.Testing.Utils (prop_codec_cborM,
                     prop_codec_valid_cbor_encoding, splits2, splits3)

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec
import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Driver.Simple ( runConnectedPeers
                                                 , runConnectedPeersAsymmetric
                                                 )

import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Client
import           Ouroboros.Network.Protocol.Handshake.Server
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Direct

import qualified Codec.CBOR.Write    as CBOR

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol"
    [ testGroup "Handshake"
        [ testProperty "connect"               prop_connect
        , testProperty "channel ST"            prop_channel_ST
        , testProperty "channel IO"            prop_channel_IO
        , testProperty "pipe IO"               prop_pipe_IO
        , testProperty "channel asymmetric ST" prop_channel_asymmetric_ST
        , testProperty "channel asymmetric IO" prop_channel_asymmetric_IO
        , testProperty "pipe asymmetric IO"    prop_pipe_asymmetric_IO
        , testProperty "codec RefuseReason"    prop_codec_RefuseReason
        , testProperty "codec"                 prop_codec_Handshake
        , testProperty "codec 2-splits"        prop_codec_splits2_Handshake
        , testProperty "codec 3-splits"      $ withMaxSuccess 30
                                               prop_codec_splits3_Handshake
        , testProperty "codec cbor"            prop_codec_cbor
        , testProperty "codec valid cbor"      prop_codec_valid_cbor
        , testGroup "Generators"
          [ testProperty "ArbitraryVersions" $
              checkCoverage prop_arbitrary_ArbitraryVersions
          , testProperty "arbitrary ArbitraryValidVersions"
              prop_arbitrary_ArbitraryValidVersions
          , testProperty "shrink ArbitraryValidVersions"
              prop_shrink_ArbitraryValidVersions
          ]
        ]
    ]

--
-- Test Versions
--
-- Notes: Associated data are choosen in such a way that a decoder will fail
-- interpreting one of them as the other.  This is done on purpose for testing
-- missencoded data (protocol version & associated version data mismatch)
--

-- |
-- Testing version number
--
data VersionNumber
  = Version_0
  | Version_1
  | Version_2
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Arbitrary VersionNumber where
  arbitrary = elements [minBound .. maxBound]

versionNumberCodec :: CodecCBORTerm (String, Maybe Int) VersionNumber
versionNumberCodec = CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm Version_0 = CBOR.TInt 0
    encodeTerm Version_1 = CBOR.TInt 1
    encodeTerm Version_2 = CBOR.TInt 2

    decodeTerm (CBOR.TInt 0) = Right Version_0
    decodeTerm (CBOR.TInt 1) = Right Version_1
    decodeTerm (CBOR.TInt 2) = Right Version_2
    decodeTerm (CBOR.TInt n) = Left ("unknown version", Just n)
    decodeTerm _             = Left ("unknown tag", Nothing)


versionNumberHandshakeCodec :: ( MonadST    m
                               , MonadThrow m
                               )
                            => Codec (Handshake VersionNumber CBOR.Term)
                                      CBOR.DeserialiseFailure m ByteString
versionNumberHandshakeCodec = codecHandshake versionNumberCodec

data VersionData = VersionData {
    dataVersion0 :: Int,
    dataVersion1 :: Bool,
    dataVersion2 :: Bool
  }
  deriving (Eq, Show, Generic)

instance Acceptable VersionData where
    acceptableVersion d d' =
      if dataVersion0 d /= dataVersion0 d'
        then Refuse (T.pack "incompatible")
        else Accept $ VersionData { dataVersion0 = dataVersion0 d,
                                    dataVersion1 = dataVersion1 d
                                                && dataVersion1 d',
                                    dataVersion2 = dataVersion2 d
                                                || dataVersion2 d' }

dataCodecCBORTerm :: VersionNumber -> CodecCBORTerm Text VersionData
dataCodecCBORTerm Version_0 = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      -- We are using @CBOR.TInt@ instead of @CBOR.TInteger@, since for small
      -- integers generated by QuickCheck they will be encoded as @TkInt@ and then
      -- are decoded back to @CBOR.TInt@ rather than @COBR.TInteger@.  The same for
      -- other @CodecCBORTerm@ records.
      encodeTerm VersionData { dataVersion0 } =
        CBOR.TInt dataVersion0

      decodeTerm (CBOR.TInt dataVersion0) =
        Right VersionData { dataVersion0,
                            dataVersion1 = False,
                            dataVersion2 = False }
      decodeTerm n =
        Left $ T.pack $ "decodeTerm VersionData: unrecognised tag: " ++ show n

dataCodecCBORTerm Version_1 = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm VersionData { dataVersion0,
                               dataVersion1 } =
        CBOR.TList [ CBOR.TInt dataVersion0,
                     CBOR.TBool dataVersion1 ]

      decodeTerm (CBOR.TList [ CBOR.TInt dataVersion0,
                               CBOR.TBool dataVersion1 ])
        = Right VersionData { dataVersion0,
                              dataVersion1,
                              dataVersion2 = False }
      decodeTerm n
        = Left $ T.pack $ "decodeTerm VersionData: unrecognised tag: " ++ show n

dataCodecCBORTerm Version_2 = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm VersionData { dataVersion0,
                               dataVersion1,
                               dataVersion2 } =
        CBOR.TList [ CBOR.TInt dataVersion0,
                     CBOR.TBool dataVersion1,
                     CBOR.TBool dataVersion2 ]

      decodeTerm (CBOR.TList [ CBOR.TInt  dataVersion0,
                               CBOR.TBool dataVersion1,
                               CBOR.TBool dataVersion2 ])
        = Right VersionData { dataVersion0,
                              dataVersion1,
                              dataVersion2 }
      decodeTerm n
        = Left $ T.pack $ "decodeTerm Data: unrecognised tag: " ++ show n

arbitraryVersionData :: VersionNumber -> Gen VersionData
arbitraryVersionData Version_0 = (\a -> VersionData a False False)
                              <$> arbitrary 
arbitraryVersionData Version_1 = (\a b -> VersionData a b False)
                              <$> arbitrary
                              <*> arbitrary
arbitraryVersionData Version_2 = VersionData
                              <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary

instance Arbitrary VersionData where
  arbitrary = arbitrary >>= arbitraryVersionData

instance CoArbitrary VersionData where

--
-- ProtocolVersion generators
--

application :: VersionData -> VersionData -> Bool
application d = \d' ->
     (dataVersion0 d == dataVersion0 d')
     -- we take `&&` (see `acceptableVersions`
     -- below), which is like `*` in Z₂
  && (dataVersion1 d >= dataVersion1 d')
     -- we take `||` (see `acceptableVersions`
     -- below), which is like `+` in Z₂
  && (dataVersion2 d <= dataVersion2 d')

-- |
-- Generate a valid @'ProtocolVersion' 'VersionNumber' r@
--
genValidVersion
  :: VersionNumber
  -> Gen (Version VersionData Bool)
genValidVersion version = do
  d <- arbitraryVersionData version
  return $ Version (application d) d


-- |
-- Generate an invalid @'ProtocolVersion' 'VersionNumber' r@.
--
genInvalidVersion
  :: VersionNumber
  -> Gen (Version VersionData Bool)
genInvalidVersion Version_0 =
    oneof [ genValidVersion Version_1
          , genValidVersion Version_2 ]
genInvalidVersion Version_1 =
    oneof [ genValidVersion Version_0
          , genValidVersion Version_2 ]
genInvalidVersion Version_2 =
    oneof [ genValidVersion Version_0
          , genValidVersion Version_1 ]

-- |
-- Generate valid @Versions@.
--
genValidVersions :: Gen (Versions VersionNumber VersionData Bool)
genValidVersions = do
  vns <- nub <$> resize 3 (listOf1 (arbitrary :: Gen VersionNumber))
  vs <- traverse genValidVersion vns
  return $ Versions $ Map.fromList $ zip vns vs

-- |
-- Generate possibly invalid @Versions@.
--
genVersions :: Gen (Versions VersionNumber VersionData Bool)
genVersions = do
  vns <- nub <$> resize 3 (listOf1 (arbitrary :: Gen VersionNumber))
  vs <- traverse (\v -> oneof [genValidVersion v, genInvalidVersion v]) vns
  return $ Versions $ Map.fromList $ zip vns vs

newtype ArbitraryValidVersions = ArbitraryValidVersions {
      runArbitraryValidVersions :: Versions VersionNumber VersionData Bool
    }

instance Show ArbitraryValidVersions where
    show (ArbitraryValidVersions (Versions vs)) = show (Map.map versionData vs)

instance Arbitrary ArbitraryValidVersions where
    arbitrary = ArbitraryValidVersions <$> genValidVersions
    -- TODO: shrink

prop_arbitrary_ArbitraryValidVersions
  :: ArbitraryValidVersions
  -> Bool
prop_arbitrary_ArbitraryValidVersions (ArbitraryValidVersions vs) =
    Map.foldlWithKey' (\r vn s -> r && validVersion vn s) True (getVersions vs)

prop_shrink_ArbitraryValidVersions
  :: ArbitraryValidVersions
  -> Bool
prop_shrink_ArbitraryValidVersions a = all id
  [ Map.foldlWithKey' (\r vn s -> r && validVersion vn s) True (getVersions vs')
  | ArbitraryValidVersions vs' <- shrink a
  ]

-- |
-- Generators for pairs of arbitrary list of versions.
--
data ArbitraryVersions =
  ArbitraryVersions
    (Versions VersionNumber VersionData Bool)
    (Versions VersionNumber VersionData Bool)

instance Show ArbitraryVersions where
    show (ArbitraryVersions (Versions vs) (Versions vs'))
      = "ArbitraryVersions " ++ show (Map.map versionData vs) ++ " " ++ show (Map.map versionData vs')

instance Arbitrary ArbitraryVersions where
    arbitrary = frequency
      [ (1, (\v -> ArbitraryVersions v v) <$> genVersions)
      , (2, ArbitraryVersions <$> genVersions <*> genVersions)
      ]
    shrink (ArbitraryVersions (Versions vs) (Versions vs')) = 
      [ ArbitraryVersions (Versions $ Map.fromList vs'') (Versions vs')
      | vs'' <- shrinkList (const []) (Map.toList vs)
      ] ++
      [ ArbitraryVersions (Versions vs) (Versions $ Map.fromList vs'')
      | vs'' <- shrinkList (const []) (Map.toList vs')
      ]


-- |
-- Check if a @'ProtocolVersion' 'VersionNumber' r@ is valid.
--
validVersion :: VersionNumber -> Version VersionData Bool -> Bool
validVersion Version_0 ((Version _ d)) = dataVersion1 d == False
                                      && dataVersion2 d == False 
validVersion Version_1 ((Version _ d)) = dataVersion2 d == False
validVersion Version_2 ((Version _ _)) = True


prop_arbitrary_ArbitraryVersions :: ArbitraryVersions -> Property
prop_arbitrary_ArbitraryVersions (ArbitraryVersions (Versions vs) (Versions vs')) =
    -- in 75% of cases the intersection is non-empty
    cover 75 intersect "non-empty intersection" $

    -- in 10% of cases the intersection is empty
    cover 10 (not intersect) "empty intersection" $

    -- in 25% of cases the common max version is valid
    cover 25 (case Map.lookupMax intersection of
               Nothing -> False
               Just (vn, s)  -> validVersion vn s)
               "valid common max version" $

    -- in 25% of cases all the versions in @vs'@ are either not in @vs@ or are
    -- not valid
    cover 25
      (Map.foldlWithKey' (\r vn s -> r && (not (vn `elem` knownVersionNumbers) || not (validVersion vn s))) True vs)
      "all versions are either unknown or not valid" $

    property True
  where
    intersection = vs `Map.intersection` vs'
    intersect    = not (Map.null intersection)

    knownVersionNumbers = Map.keys vs'

maybeAccept :: Accept a -> Maybe a
maybeAccept (Accept a) = Just a
maybeAccept (Refuse _) = Nothing

-- | Run a handshake protocol, without going via a channel.
--
prop_connect :: ArbitraryVersions -> Property
prop_connect (ArbitraryVersions clientVersions serverVersions) =
  let (serverRes, clientRes) =
        pureHandshake
          ((maybeAccept .) . acceptableVersion)
          serverVersions
          clientVersions
  in case runSimOrThrow
           (connect
              (handshakeClientPeer
                (cborTermVersionDataCodec dataCodecCBORTerm)
                acceptableVersion
                clientVersions)
              (handshakeServerPeer
                (cborTermVersionDataCodec dataCodecCBORTerm)
                acceptableVersion
                serverVersions)) of
      (clientRes', serverRes', TerminalStates TokDone TokDone) ->
           fromMaybe False clientRes === either (const False) (\(a,_,_) -> a) clientRes'
        .&&.
           fromMaybe False serverRes === either (const False) (\(a,_,_) -> a) serverRes'
--
-- Properties using a channel
--

-- | Run a simple block-fetch client and server using connected channels.
--
prop_channel :: ( MonadAsync m
                , MonadCatch m
                , MonadST m
                )
             => m (Channel m ByteString, Channel m ByteString)
             -> Versions VersionNumber VersionData Bool
             -> Versions VersionNumber VersionData Bool
             -> m Property
prop_channel createChannels clientVersions serverVersions =
  let (serverRes, clientRes) =
        pureHandshake
          ((maybeAccept .) . acceptableVersion)
          serverVersions
          clientVersions
  in do
    (clientRes', serverRes') <-
      runConnectedPeers
        createChannels nullTracer versionNumberHandshakeCodec
        (handshakeClientPeer
          (cborTermVersionDataCodec dataCodecCBORTerm)
          acceptableVersion
          clientVersions)
        (handshakeServerPeer
          (cborTermVersionDataCodec dataCodecCBORTerm)
          acceptableVersion
          serverVersions)
    pure $
      case (clientRes', serverRes') of
        -- buth succeeded, we just check that the application (which is
        -- a boolean value) is the one that was put inside 'Version'
        (Right (c,_,_), Right (s,_,_)) -> Just c === clientRes
                                     .&&. Just s === serverRes

        -- both failed
        (Left{}, Left{})   -> property True

        -- it should not happen that one protocol succeeds and the other end
        -- fails
        _                  -> property False


-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_ST :: ArbitraryVersions -> Property
prop_channel_ST (ArbitraryVersions clientVersions serverVersions) =
    runSimOrThrow (prop_channel createConnectedChannels clientVersions serverVersions)


-- | Run 'prop_channel' in the IO monad.
--
prop_channel_IO :: ArbitraryVersions -> Property
prop_channel_IO (ArbitraryVersions clientVersions serverVersions) =
    ioProperty (prop_channel createConnectedChannels clientVersions serverVersions)


-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_IO :: ArbitraryVersions -> Property
prop_pipe_IO (ArbitraryVersions clientVersions serverVersions) =
    ioProperty (prop_channel createPipeConnectedChannels clientVersions serverVersions)

--
-- Asymmetric tests
--


-- | Run a simple handshake client and server using connected channels.
-- The server can only decode a subset of versions send by client.
-- This test is using a fixed server 'Versions' which can only accept
-- a single version 'Version_1' (it cannot decode any other version).
--
prop_channel_asymmetric
    :: ( MonadAsync m
       , MonadCatch m
       , MonadST m
       )
    => m (Channel m ByteString, Channel m ByteString)
    -> Versions VersionNumber VersionData Bool
    -- ^ client versions
    -> m Property
prop_channel_asymmetric createChannels clientVersions = do
    (clientRes', serverRes') <-
      runConnectedPeersAsymmetric
        createChannels
        nullTracer
        versionNumberHandshakeCodec
        (codecHandshake versionNumberCodec')
        (handshakeClientPeer
          (cborTermVersionDataCodec dataCodecCBORTerm)
          acceptableVersion
          clientVersions)
        (handshakeServerPeer
          (cborTermVersionDataCodec dataCodecCBORTerm)
          acceptableVersion
          serverVersions)
    pure $
      case (clientRes', serverRes') of
        (Right (c,_,_), Right (s,_,_))
                           -> Just c === clientRes
                         .&&. Just s === serverRes
        (Left{}, Left{})   -> property True
        _                  -> property False

  where
    -- server versions
    serverVersions :: Versions VersionNumber VersionData Bool
    serverVersions =
      let d = VersionData 0 True True in
      Versions
        $ Map.singleton
            Version_1
            (Version (application d) d)
              

    -- This codec does not know how to decode 'Version_0' and 'Version_2'.
    versionNumberCodec' :: CodecCBORTerm (String, Maybe Int) VersionNumber
    versionNumberCodec' = CodecCBORTerm { encodeTerm, decodeTerm }
      where
        encodeTerm Version_1 = CBOR.TInt 1
        encodeTerm _         = error "server encoder error"

        decodeTerm (CBOR.TInt 1) = Right Version_1
        decodeTerm (CBOR.TInt n) = Left ("unknown version", Just n)
        decodeTerm _             = Left ("unknown tag", Nothing)

    (serverRes, clientRes) =
      pureHandshake
        ((maybeAccept .) . acceptableVersion)
        serverVersions
        clientVersions



-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_asymmetric_ST :: ArbitraryVersions -> Property
prop_channel_asymmetric_ST (ArbitraryVersions clientVersions _serverVersions) =
    runSimOrThrow (prop_channel_asymmetric createConnectedChannels clientVersions)


-- | Run 'prop_channel' in the IO monad.
--
prop_channel_asymmetric_IO :: ArbitraryVersions -> Property
prop_channel_asymmetric_IO (ArbitraryVersions clientVersions _serverVersions) =
    ioProperty (prop_channel_asymmetric createConnectedChannels clientVersions)


-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_asymmetric_IO :: ArbitraryVersions -> Property
prop_pipe_asymmetric_IO (ArbitraryVersions clientVersions _serverVersions) =
    ioProperty (prop_channel_asymmetric createPipeConnectedChannels clientVersions)


--
-- Codec tests
--

instance Eq (AnyMessage (Handshake VersionNumber CBOR.Term)) where
  AnyMessage (MsgProposeVersions vs) == AnyMessage (MsgProposeVersions vs')
    = vs == vs'

  AnyMessage (MsgProposeVersions' vs) == AnyMessage (MsgProposeVersions' vs')
    = vs == vs'

  AnyMessage (MsgAcceptVersion vNumber vParams) == AnyMessage (MsgAcceptVersion vNumber' vParams')
    = vNumber == vNumber' && vParams == vParams'

  AnyMessage (MsgRefuse vReason) == AnyMessage (MsgRefuse vReason')
    = vReason == vReason'

  _ == _ = False

instance Arbitrary (AnyMessageAndAgency (Handshake VersionNumber CBOR.Term)) where
  arbitrary = oneof
    [     AnyMessageAndAgency (ClientAgency TokPropose)
        . MsgProposeVersions
        . Map.mapWithKey (\v -> encodeTerm (dataCodecCBORTerm v) . versionData)
        . getVersions
      <$> genVersions

    ,     AnyMessageAndAgency (ServerAgency TokConfirm)
        . MsgProposeVersions'
        . Map.mapWithKey (\v -> encodeTerm (dataCodecCBORTerm v) . versionData)
        . getVersions
      <$> genVersions

    ,     AnyMessageAndAgency (ServerAgency TokConfirm)
        . uncurry MsgAcceptVersion
      <$> genValidVersion'

    ,     AnyMessageAndAgency (ServerAgency TokConfirm)
        . MsgRefuse
        . runArbitraryRefuseReason
      <$> arbitrary
    ]
    where
      genValidVersion' :: Gen (VersionNumber, CBOR.Term)
      genValidVersion' = do
        vn <- arbitrary
        Version _ vData <- genValidVersion vn
        pure (vn, encodeTerm (dataCodecCBORTerm vn) vData)


newtype ArbitraryRefuseReason = ArbitraryRefuseReason {
    runArbitraryRefuseReason :: RefuseReason VersionNumber
  }
  deriving (Eq, Show)


instance Arbitrary ArbitraryRefuseReason where
    arbitrary = ArbitraryRefuseReason <$> oneof
      [ VersionMismatch
          <$> arbitrary
          -- this argument is not supposed to be sent, only received:
          <*> pure []
      , HandshakeDecodeError <$> arbitrary <*> arbitraryText
      , Refused <$> arbitrary <*> arbitraryText
      ]
      where
        arbitraryText = T.pack <$> arbitrary


--
--  TODO: these tests should be moved to 'ouroboros-network-framework'
--

-- TODO: we are not testing the cases where we decode version numbers that we do
-- not know about.
prop_codec_RefuseReason
  :: ArbitraryRefuseReason
  -> Bool
prop_codec_RefuseReason (ArbitraryRefuseReason vReason) = 
  case CBOR.deserialiseFromBytes
        (decodeRefuseReason versionNumberCodec)
        (CBOR.toLazyByteString $ encodeRefuseReason versionNumberCodec vReason) of
    Left _ -> False
    Right (bytes, vReason') -> BL.null bytes && vReason' == vReason

prop_codec_Handshake
  :: AnyMessageAndAgency (Handshake VersionNumber CBOR.Term)
  -> Bool
prop_codec_Handshake msg =
  runSimOrThrow (prop_codecM (codecHandshake versionNumberCodec) msg)

prop_codec_splits2_Handshake
  :: AnyMessageAndAgency (Handshake VersionNumber CBOR.Term)
  -> Bool
prop_codec_splits2_Handshake msg =
  runSimOrThrow (prop_codec_splitsM splits2 (codecHandshake versionNumberCodec) msg)

prop_codec_splits3_Handshake
  :: AnyMessageAndAgency (Handshake VersionNumber CBOR.Term)
  -> Bool
prop_codec_splits3_Handshake msg =
  runSimOrThrow (prop_codec_splitsM splits3 (codecHandshake versionNumberCodec) msg)

prop_codec_cbor
  :: AnyMessageAndAgency (Handshake VersionNumber CBOR.Term)
  -> Bool
prop_codec_cbor msg =
  runSimOrThrow (prop_codec_cborM (codecHandshake versionNumberCodec) msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: AnyMessageAndAgency (Handshake VersionNumber CBOR.Term)
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding (codecHandshake versionNumberCodec)
