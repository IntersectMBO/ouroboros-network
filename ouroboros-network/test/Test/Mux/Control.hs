{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Mux.Control (
      tests) where

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Codec.CBOR.FlatTerm as CBOR
import           Codec.Serialise (Serialise (..))
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Mux.Control

tests :: TestTree
tests =
  testGroup "MuxControl"
  [ testProperty "MuxControl encode/decode"      prop_mux_encode_decode
  , testProperty "MuxControl decoding"           prop_decode_ControlMsg
  , testGroup "generators"
     [ testProperty "ArbitraryFlatTerm is valid" prop_ArbitraryFlatTerm
     ]
  ]

instance Arbitrary Mx.NetworkMagic where
    arbitrary = Mx.NetworkMagic <$> arbitrary

instance Arbitrary Mx.Version where
    arbitrary = do
        nm <- arbitrary
        elements [Mx.Version0 nm, Mx.Version1 nm]

instance Arbitrary ControlMsg where
    arbitrary = do
        vs <- arbitrary
        v  <- arbitrary
        f  <- arbitrary
        elements [ MsgInitReq vs
                 , MsgInitRsp v
                 , MsgInitFail f]

-- | Verify that decoder . encoder = id for ControlMsg.
prop_mux_encode_decode :: ControlMsg
                       -> Property
prop_mux_encode_decode msg =
    let bs = toLazyByteString $ encode msg
        res_e = deserialiseFromBytes decode bs in
    case res_e of
         Left  _   -> property False
         Right res -> return msg === res

data ArbitraryVersionFT = ArbitraryVersionFT (Maybe Mx.Version) CBOR.FlatTerm
    deriving Show

instance Arbitrary ArbitraryVersionFT where

  arbitrary = oneof
      [ (\v -> ArbitraryVersionFT (Just v) (CBOR.toFlatTerm $ encode v)) <$> arbitrary
      , toArbitraryVersionFT . unArbitraryFlatTerm <$> resize 10 arbitrary
      ]
    where
      toArbitraryVersionFT :: CBOR.FlatTerm -> ArbitraryVersionFT
      toArbitraryVersionFT t = ArbitraryVersionFT (toVersion t) t

      -- We allow to encode a version as a list of length >= 2 with two first
      -- fields as integers, either encoded as `TkInt` or `TkInteger`.
      toVersion (CBOR.TkListLen n : CBOR.TkInt x : CBOR.TkInt y : _) | n >= 2 && y >= 0 = case x of
        0 -> Just (Mx.Version0 (Mx.NetworkMagic (fromIntegral y)))
        1 -> Just (Mx.Version1 (Mx.NetworkMagic (fromIntegral y)))
        _ -> Nothing
      toVersion (CBOR.TkListLen n : CBOR.TkInt x : CBOR.TkInteger y : _) | n >= 2 && y >= 0 = case x of
        0 -> Just (Mx.Version0 (Mx.NetworkMagic (fromIntegral y)))
        1 -> Just (Mx.Version1 (Mx.NetworkMagic (fromIntegral y)))
        _ -> Nothing
      toVersion (CBOR.TkListLen n : CBOR.TkInteger x : CBOR.TkInt y : _) | n >= 2 && y >= 0 = case x of
        0 -> Just (Mx.Version0 (Mx.NetworkMagic (fromIntegral y)))
        1 -> Just (Mx.Version1 (Mx.NetworkMagic (fromIntegral y)))
        _ -> Nothing
      toVersion (CBOR.TkListLen n : CBOR.TkInteger x : CBOR.TkInteger y : _) | n >= 2 && y >= 0 = case x of
        0 -> Just (Mx.Version0 (Mx.NetworkMagic (fromIntegral y)))
        1 -> Just (Mx.Version1 (Mx.NetworkMagic (fromIntegral y)))
        _ -> Nothing
      toVersion _ = Nothing

-- |
-- Generate arbitrary valid @'CBOR.FlatTerm'@.  It needs to be resized so it
-- does not loop to deep.
--
genArbitraryFlatTerm :: Gen CBOR.FlatTerm
genArbitraryFlatTerm = sized $ \s ->
  let s' = if s > 0 then pred s else 0
  in oneof
    [ (:[]) . CBOR.TkInt <$> arbitrary
    , (:[]) . CBOR.TkInteger <$> arbitrary
    , (\x  -> [CBOR.TkBytesBegin, CBOR.TkBytes x, CBOR.TkBreak]) . BSC.pack <$> arbitrary
    , (\x  -> [CBOR.TkStringBegin, CBOR.TkString x, CBOR.TkBreak]) . T.pack <$>  arbitrary
    , (\xs -> CBOR.TkListLen (fromIntegral $ length xs) : concat xs) <$> (resize s' $ listOf genArbitraryFlatTerm)
    , (\xs -> CBOR.TkListBegin : concat xs ++ [CBOR.TkBreak]) <$> (resize s' $ listOf genArbitraryFlatTerm)
    , (\xs -> CBOR.TkMapLen (fromIntegral $ length xs) : concat xs) <$> (resize s' $ listOf genArbitraryPairs)
    , (\xs -> CBOR.TkMapBegin : concat xs ++ [CBOR.TkBreak]) <$> (resize s' $ listOf genArbitraryPairs)
    , (:[]) . CBOR.TkBool <$> arbitrary
    ,  return [CBOR.TkNull]
    , (:[]) . CBOR.TkSimple <$> arbitrary
    , (:[]) . CBOR.TkFloat16 <$> arbitrary
    , (:[]) . CBOR.TkFloat32 <$> arbitrary
    , (:[]) . CBOR.TkFloat64 <$> arbitrary
    ]
  where
    genArbitraryPairs :: Gen CBOR.FlatTerm
    genArbitraryPairs = sized $ \s  ->
      let s' = if s > 0 then pred s else 0
      in (\x y -> x ++ y)
          <$> resize s' genArbitraryFlatTerm
          <*> resize s' genArbitraryFlatTerm

data ArbitraryFlatTerm = ArbitraryFlatTerm { unArbitraryFlatTerm :: CBOR.FlatTerm }
    deriving Show

instance Arbitrary ArbitraryFlatTerm where
    arbitrary = resize 10 $ ArbitraryFlatTerm <$> genArbitraryFlatTerm
    -- TODO shrink

prop_ArbitraryFlatTerm :: ArbitraryFlatTerm -> Bool
prop_ArbitraryFlatTerm (ArbitraryFlatTerm t) = CBOR.validFlatTerm t

data ArbitraryControlMsgFT = ArbitraryControlMsgFT (Maybe ControlMsg) CBOR.FlatTerm
    deriving Show

-- |
-- The arbitrary instance which generates a @'ControlMessage@' encoding (and
-- possible message from which it was generated).  All the generated CBOR terms
-- are valid; the generator will include unknown encodings of @'Mx.Version'@.
--
instance Arbitrary ArbitraryControlMsgFT where
    arbitrary = oneof
        [ (\x -> ArbitraryControlMsgFT (Just x) (CBOR.toFlatTerm $ encode x)) <$> arbitrary
        , msgInitReq <$> listOf arbitrary
        ,  msgInitRsp <$> arbitrary
        , invalidControlMsgTag
        ]
      where
        -- |
        -- Generate @'MsgInitReq'@ with a list of versions which includes unknown
        -- terms.
        msgInitReq :: [ArbitraryVersionFT] -> ArbitraryControlMsgFT
        msgInitReq xs =
          let ts :: CBOR.FlatTerm
              ts = concatMap (\(ArbitraryVersionFT _ t) -> t) xs 
              vs :: [Mx.Version]
              vs = mapMaybe (\(ArbitraryVersionFT v _) -> v) xs
          in ArbitraryControlMsgFT
            (Just (MsgInitReq vs))
            (CBOR.TkListLen 2
              : CBOR.TkInt 0
              : CBOR.TkListLen (fromIntegral $ length xs)
              : ts)

        -- |
        -- Generate @'MsgInitRsp'@ which includes unknown term.
        msgInitRsp :: ArbitraryVersionFT -> (ArbitraryControlMsgFT)
        msgInitRsp (ArbitraryVersionFT (Just v) _) =
          ArbitraryControlMsgFT
            (Just (MsgInitRsp v))
            (CBOR.toFlatTerm $ encode (MsgInitRsp v))
        msgInitRsp (ArbitraryVersionFT Nothing ts) =
          ArbitraryControlMsgFT
            Nothing
            (CBOR.TkListLen 2
              : CBOR.TkInt 1
              : ts)

        -- |
        -- Generate @'ControlMessage'@ with an unknown tag, which includes
        -- unknown terms.
        invalidControlMsgTag :: Gen (ArbitraryControlMsgFT)
        invalidControlMsgTag = do
          NonNegative x <- arbitrary
          (xs :: [CBOR.FlatTerm]) <- listOf (unArbitraryFlatTerm <$> arbitrary)
          return $ ArbitraryControlMsgFT Nothing
            (CBOR.TkListLen (fromIntegral $ (length xs) + 1)
              : CBOR.TkInt (x + 2)
              : concat xs)

-- |
-- Check that we can decode all version that we know ignoring version's that
-- are unkown.
--
prop_decode_ControlMsg :: ArbitraryControlMsgFT -> Property
prop_decode_ControlMsg (ArbitraryControlMsgFT msg t) =
  case CBOR.fromFlatTerm decode t of
    Left _     -> property $ maybe True (const False) msg
    Right msg' -> msg === Just msg'
