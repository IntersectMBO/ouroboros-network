-- | How long a mini-protocol's 'send' spends encoding before the message is
-- queued, by how much of it is forced there: the first chunk, which a send
-- left to the muxer before, the soft limit's worth, or all of it. The
-- encodings mirror the sizes that matter: a block, a Leios EB body (hash and
-- size per transaction) within the soft limit and one of 1 MB, and a Leios
-- block-transactions response.
module Main (main) where

import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word16, Word32)
import Test.Tasty.Bench

import Network.Mux.Egress (egressSoftBufferLimit)

main :: IO ()
main = do
  block <- evaluate (force (BS.replicate 90_000 7))
  body  <- evaluate (force [ (BS.replicate 32 (fromIntegral i), fromIntegral (200 + i) :: Word32)
                           | i <- [0 .. 3_699 :: Int] ])
  body1M <- evaluate (force [ (BS.replicate 32 (fromIntegral i), fromIntegral (200 + i) :: Word32)
                            | i <- [0 .. 26_999 :: Int] ])
  txs   <- evaluate (force (takeBytes 12_000_000 [ BS.replicate (200 + (i * 7919) `mod` 15_800) 3
                                                 | i <- [0 :: Int ..] ]))
  defaultMain
    [ sizes "block, 90 kB" (CBOR.encodeBytes block)
    , sizes "EB body, 3700 txs" (encodeBody body)
    , sizes "EB body, 27000 txs, 1 MB" (encodeBody body1M)
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
      [ bench "first chunk (left to the muxer)" $ whnf (firstChunk . CBOR.toLazyByteString) enc
      , bench "the soft limit" $ whnf (BL.length . BL.take egressSoftBufferLimit . CBOR.toLazyByteString) enc
      , bench "whole message (eager send)" $ whnf (BL.length . CBOR.toLazyByteString) enc
      ]

    firstChunk :: BL.ByteString -> Int
    firstChunk bs = case BL.toChunks bs of
                         []    -> 0
                         c : _ -> BS.length c
