{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Ouroboros.Network.Data.AbsBearerInfo
  ( AbsBearerInfoScript (..)
  , canFail
  , NonFailingAbsBearerInfo (..)
  , NonFailingAbsBearerInfoScript (..)
  , AbsDelay (..)
  , delay
  , AbsSpeed (..)
  , speedToRational
  , delayAtSpeed
  , AbsSDUSize (..)
  , toSduSize
  , AbsIOError (..)
  , AbsAttenuation (..)
  , attenuation
  , absNoAttenuation
  , AbsBearerInfo (..)
  , toNonFailingAbsBearerInfoScript
  , AbsIOErrType (..)
  ) where

import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..), addTime)

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid (Any (..))
import Foreign.C.Error (Errno (..), eCONNABORTED)
import GHC.IO.Exception (IOErrorType (..), IOException (..))

import Network.Mux.Bearer.AttenuatedChannel (Size, SuccessOrFailure (..))
import Network.Mux.Types (SDUSize (..))

import Test.Ouroboros.Network.Data.Script (Script (..))
import Test.Ouroboros.Network.Utils (Delay (..))

import Test.QuickCheck hiding (Result (..))


data AbsDelay = SmallDelay
              | NormalDelay
              | LargeDelay

  deriving (Eq, Ord, Show)

delay :: AbsDelay -> DiffTime
delay SmallDelay  = 0.1
delay NormalDelay = 1
delay LargeDelay  = 20

instance Arbitrary AbsDelay where
    arbitrary = frequency
      [ (1, pure SmallDelay)
      , (2, pure NormalDelay)
      , (1, pure LargeDelay)
      ]
    shrink SmallDelay  = []
    shrink NormalDelay = [SmallDelay]
    shrink LargeDelay  = [SmallDelay, NormalDelay]

data AbsSpeed = SlowSpeed
              | NormalSpeed
              | FastSpeed
    deriving (Eq, Ord, Show)

instance Arbitrary AbsSpeed where
    arbitrary = frequency
      [ (1, pure SlowSpeed)
      , (2, pure NormalSpeed)
      , (1, pure FastSpeed)
      ]
    shrink SlowSpeed   = [FastSpeed, NormalSpeed]
    shrink NormalSpeed = [FastSpeed]
    shrink FastSpeed   = []

speedToRational :: AbsSpeed -> Rational
speedToRational SlowSpeed   = 3057    -- 12228 / 4
speedToRational NormalSpeed = 48912   -- 12228 * 4
speedToRational FastSpeed   = 1048576 -- 1Mb/s

delayAtSpeed :: AbsSpeed -> Size -> DiffTime
delayAtSpeed speed size = fromRational (toRational size / speedToRational speed)


data AbsSDUSize = SmallSDU
                | NormalSDU
                | LargeSDU

  deriving (Eq, Show)

instance Arbitrary AbsSDUSize where
    arbitrary = elements [SmallSDU, NormalSDU, LargeSDU]
    shrink SmallSDU  = [LargeSDU, NormalSDU]
    shrink NormalSDU = [LargeSDU]
    shrink LargeSDU  = []

toSduSize :: AbsSDUSize -> SDUSize
toSduSize SmallSDU  = SDUSize 1_024
toSduSize NormalSDU = SDUSize 12_228
toSduSize LargeSDU  = SDUSize 32_768

data AbsAttenuation =
    NoAttenuation    AbsSpeed
  | SpeedAttenuation AbsSpeed Time DiffTime
  | ErrorInterval    AbsSpeed Time DiffTime IOError
  deriving (Eq, Show)

-- | At most `Time 20s`.
--
genTime :: Gen Time
genTime = Time . getDelay <$> arbitrary

-- | At most `1_000`s.
--
genLongDelay :: Gen DiffTime
genLongDelay = getDelay <$> resize 1_000 arbitrary

newtype AbsIOError = AbsIOError { getIOError :: IOError }
  deriving Show

instance Arbitrary AbsIOError where
  arbitrary = AbsIOError <$> elements
      [ mkIOError ResourceVanished
      , mkIOError ResourceExhausted
      , mkIOError UnsupportedOperation
      , mkIOError InvalidArgument
      , mkIOError ProtocolError
      , connectionAbortedError
      ]
    where
      -- `ECONNABORTED` error which appears in `Ouroboros.Network.Server`
      connectionAbortedError :: IOError
      connectionAbortedError = IOError
        { ioe_handle      = Nothing
        , ioe_type        = OtherError
        , ioe_location    = "Ouroboros.Network.Snocket.Sim.accept"
          -- Note: this matches the `iseCONNABORTED` on Windows, see
          -- 'Ouroboros.Network.Server`
        , ioe_description = "Software caused connection abort (WSAECONNABORTED)"
        , ioe_errno       = Just (case eCONNABORTED of Errno errno -> errno)
        , ioe_filename    = Nothing
        }

      mkIOError :: IOErrorType -> IOError
      mkIOError ioe_type = IOError
        { ioe_handle      = Nothing
        , ioe_type
        , ioe_location    = "AttenuationChannel"
        , ioe_description = "attenuation"
        , ioe_errno       = Nothing
        , ioe_filename    = Nothing
        }

instance Arbitrary AbsAttenuation where
    arbitrary =
        frequency
          [ (2, NoAttenuation <$> arbitrary)
          , (1, SpeedAttenuation <$> arbitrary `suchThat` (> SlowSpeed)
                                 <*> genTime
                                 <*> genLongDelay
            )
          , (1, ErrorInterval <$> arbitrary
                              <*> genTime
                              <*> genLongDelay
                              <*> (getIOError <$> arbitrary)
            )
          ]
      where

    shrink (NoAttenuation speed) =
      [NoAttenuation speed' | speed' <- shrink speed ]
    shrink (SpeedAttenuation speed time len) =
      [ if len' < 1
         then NoAttenuation speed
         else SpeedAttenuation speed time len'
      | Delay len' <- shrink (Delay len)
      ]
    shrink (ErrorInterval speed time len _ioe) =
      [ if len' < 1
         then NoAttenuation speed
         else SpeedAttenuation speed time len'
      | Delay len' <- shrink (Delay len)
      ]


attenuation :: AbsAttenuation
            -> Time -> Size -> (DiffTime, SuccessOrFailure)
attenuation (NoAttenuation speed) =
   \_ size -> (delayAtSpeed speed size, Success)
attenuation (SpeedAttenuation normalSpeed from len) =
    \t size ->
      let speed = if t < from || t >= len `addTime` from
                    then normalSpeed
                    else SlowSpeed
      in ( delayAtSpeed speed size
         , Success
         )
attenuation (ErrorInterval speed from len ioe) =
    \t size ->
        ( delayAtSpeed speed size
        , if t < from || t >= len `addTime` from
            then Success
            else Failure ioe
        )

data AbsIOErrType = AbsIOErrConnectionAborted
                  | AbsIOErrResourceExhausted
  deriving (Eq, Show)

instance Arbitrary AbsIOErrType where
    arbitrary = elements [ AbsIOErrConnectionAborted
                         , AbsIOErrResourceExhausted
                         ]
    shrink AbsIOErrConnectionAborted = [AbsIOErrResourceExhausted]
    shrink AbsIOErrResourceExhausted = []

data AbsBearerInfo = AbsBearerInfo
    { abiConnectionDelay      :: !AbsDelay
    , abiInboundAttenuation   :: !AbsAttenuation
    , abiOutboundAttenuation  :: !AbsAttenuation
    , abiInboundWriteFailure  :: !(Maybe Int)
    , abiOutboundWriteFailure :: !(Maybe Int)
    , abiAcceptFailure        :: !(Maybe (AbsDelay, IOError))
    , abiSDUSize              :: !AbsSDUSize
    }
  deriving (Eq, Show)

absNoAttenuation :: AbsBearerInfo
absNoAttenuation = AbsBearerInfo
    { abiConnectionDelay      = NormalDelay
    , abiInboundAttenuation   = NoAttenuation NormalSpeed
    , abiOutboundAttenuation  = NoAttenuation NormalSpeed
    , abiInboundWriteFailure  = Nothing
    , abiOutboundWriteFailure = Nothing
    , abiAcceptFailure        = Nothing
    , abiSDUSize              = NormalSDU
    }

canFail :: AbsBearerInfo -> Bool
canFail abi = getAny $
       case abiInboundAttenuation abi of
         NoAttenuation {} -> Any False
         _                -> Any True
    <> case abiOutboundAttenuation abi of
         NoAttenuation {} -> Any False
         _                -> Any True
    <> case abiInboundWriteFailure abi of
         Nothing -> Any False
         _       -> Any True
    <> case abiOutboundWriteFailure abi of
         Nothing -> Any False
         _       -> Any True
    <> case abiAcceptFailure abi of
         Nothing -> Any False
         _       -> Any True

instance Arbitrary AbsBearerInfo where
    arbitrary =
        AbsBearerInfo <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> genWriteFailure
                      <*> genWriteFailure
                      <*> genAcceptFailure
                      <*> arbitrary
      where
        genWriteFailure =
          frequency
            [ (2, pure Nothing)
            , (1, Just <$> arbitrarySizedNatural)
            ]
        genAcceptFailure =
          frequency
            [ (2, pure Nothing)
            , (1, (\a b -> Just (a,b))
                <$> arbitrary
                <*> (getIOError <$> arbitrary))
            ]

    shrink abi =
      [ abi { abiConnectionDelay = connDelay }
      | connDelay <- shrink (abiConnectionDelay abi) ]
      ++
      [ abi { abiInboundAttenuation = a }
      | a <- shrink (abiInboundAttenuation abi) ]
      ++
      [ abi { abiOutboundAttenuation = a }
      | a <- shrink (abiOutboundAttenuation abi) ]
      ++
      [ abi { abiInboundWriteFailure = a }
      | a <- shrink (abiInboundWriteFailure abi) ]
      ++
      [ abi { abiOutboundWriteFailure = a }
      | a <- shrink (abiOutboundWriteFailure abi) ]
      ++
      [ abi { abiSDUSize = a }
      | a <- shrink (abiSDUSize abi)
      ]

newtype AbsBearerInfoScript = AbsBearerInfoScript {
    unBIScript :: Script AbsBearerInfo
  }
  deriving       Show via (Script AbsBearerInfo)
  deriving stock Eq

fixupAbsBearerInfos :: [AbsBearerInfo] -> [AbsBearerInfo]
fixupAbsBearerInfos bis =
    if canFail (last bis)
      then bis ++ [absNoAttenuation]
      else bis

instance Arbitrary AbsBearerInfoScript where
  arbitrary = AbsBearerInfoScript
            . Script
            . NonEmpty.fromList
            . fixupAbsBearerInfos
          <$> listOf1 arbitrary

  shrink (AbsBearerInfoScript (Script script)) =
    [ AbsBearerInfoScript (Script script')
    | script'
        <- map (NonEmpty.fromList . fixupAbsBearerInfos)
        . filter (not . List.null)
         -- TODO: shrinking of 'AbsBearerInfo' needs to be more aggressive to use
         -- @shrinkList shrink@
         $ shrinkList (const []) (NonEmpty.toList script)
    , script' /= script
    ]

newtype NonFailingAbsBearerInfo = NonFailingAbsBearerInfo {
    unNFBI :: AbsBearerInfo
  }
  deriving       Show via AbsBearerInfo
  deriving stock Eq

toNonFailingAbsBearerInfo :: AbsBearerInfo
                          -> NonFailingAbsBearerInfo
toNonFailingAbsBearerInfo script =
    NonFailingAbsBearerInfo $ unfail script
  where
    unfail :: AbsBearerInfo -> AbsBearerInfo
    unfail bi =
      bi { abiInboundWriteFailure  = Nothing
         , abiOutboundWriteFailure = Nothing
         , abiInboundAttenuation   = unfailAtt $ abiInboundAttenuation bi
         , abiOutboundAttenuation  = unfailAtt $ abiOutboundAttenuation bi
         , abiAcceptFailure        = Nothing
         }

    unfailAtt (ErrorInterval    speed _ _ _) = NoAttenuation speed
    unfailAtt (SpeedAttenuation speed _ _)   = NoAttenuation speed
    unfailAtt a                              = a

instance Arbitrary NonFailingAbsBearerInfo where
  arbitrary = toNonFailingAbsBearerInfo <$> arbitrary
  shrink (NonFailingAbsBearerInfo script) =
    NonFailingAbsBearerInfo <$> shrink script

newtype NonFailingAbsBearerInfoScript = NonFailingAbsBearerInfoScript {
    unNFBIScript :: Script AbsBearerInfo
  }
  deriving       Show via (Script AbsBearerInfo)
  deriving stock Eq

toNonFailingAbsBearerInfoScript :: AbsBearerInfoScript
                                -> NonFailingAbsBearerInfoScript
toNonFailingAbsBearerInfoScript (AbsBearerInfoScript script) =
    NonFailingAbsBearerInfoScript
    $ fmap (unNFBI . toNonFailingAbsBearerInfo) script

instance Arbitrary NonFailingAbsBearerInfoScript where
  arbitrary = toNonFailingAbsBearerInfoScript <$> arbitrary
  shrink (NonFailingAbsBearerInfoScript script) =
    toNonFailingAbsBearerInfoScript <$> shrink (AbsBearerInfoScript script)
