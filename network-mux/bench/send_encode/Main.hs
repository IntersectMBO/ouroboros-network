-- | How long a mini-protocol's 'send' spends encoding before the message is
-- queued, by how much of it is forced there: the first chunk (what a lazy
-- send leaves for the muxer), the first 256 KiB ('forcePrefix' at the egress
-- soft limit), or all of it. The encodings mirror the sizes that matter: a
-- block, a Leios EB body (hash and size per transaction), and a Leios
-- block-transactions response.
module Main (main) where

import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Internal qualified as BLI
import Data.Word (Word16, Word32)
import Test.Tasty.Bench

import Network.Mux.Egress (forcePrefix)

main :: IO ()
main = do
  block <- evaluate (force (BS.replicate 90_000 7))
  body  <- evaluate (force [ (BS.replicate 32 (fromIntegral i), fromIntegral (200 + i) :: Word32)
                           | i <- [0 .. 3_699 :: Int] ])
  txs   <- evaluate (force (takeBytes 12_000_000 [ BS.replicate (200 + (i * 7919) `mod` 15_800) 3
                                                 | i <- [0 :: Int ..] ]))
  defaultMain
    [ sizes "block, 90 kB" (CBOR.encodeBytes block)
    , sizes "EB body, 3700 txs" (encodeBody body)
    , sizes "block txs, 12 MB" (encodeTxs txs)
    ]
  where
    takeBytes :: Int -> [BS.ByteString] -> [BS.ByteString]
    takeBytes n (b : bs) | n > 0 = b : takeBytes (n - BS.length b) bs
    takeBytes _ _ = []

    encodeBody :: [(BS.ByteString, Word32)] -> Encoding
    encodeBody items = CBOR.encodeMapLen (fromIntegral (length items))
                    <> foldMap (\(h, s) -> CBOR.encodeBytes h <> CBOR.encodeWord32 s) items

    encodeTxs :: [BS.ByteString] -> Encoding
    encodeTxs ts = CBOR.encodeListLen (fromIntegral (length ts))
                <> foldMap (\(i, t) -> CBOR.encodeListLen 2 <> CBOR.encodeWord16 i <> CBOR.encodeBytes t)
                           (zip [0 :: Word16 ..] ts)

    sizes :: String -> Encoding -> Benchmark
    sizes name enc = bgroup name
      [ bench "first chunk (lazy send)"   $ whnf (firstChunk . CBOR.toLazyByteString) enc
      , bench "first 256 KiB (forcePrefix)" $ whnf (forcePrefix 0x3ffff . CBOR.toLazyByteString) enc
      , bench "whole message (eager send)" $ whnf (BL.length . CBOR.toLazyByteString) enc
      ]

    firstChunk :: BL.ByteString -> Int
    firstChunk BLI.Empty         = 0
    firstChunk (BLI.Chunk c _)   = BS.length c
