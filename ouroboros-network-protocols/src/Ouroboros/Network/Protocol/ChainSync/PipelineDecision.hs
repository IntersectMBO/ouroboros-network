{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.ChainSync.PipelineDecision
  ( PipelineDecision (..)
  , MkPipelineDecision (..)
  , runPipelineDecision
  , constantPipelineDecision
  , pipelineDecisionMax
  , pipelineDecisionMin
  , pipelineDecisionLowHighMark
  ) where

import           Control.Exception (assert)
import           Data.Word

import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.Block (BlockNo)
import           Ouroboros.Network.Point (WithOrigin (..))

-- | Pipeline decision: we can do either one of these:
--
-- * non-pipelined request
-- * pipeline a request
-- * collect or pipeline, but only when there are pipelined requests
-- * collect, as above, only when there are pipelined requests
--
-- There might be other useful pipelining scenarios: collect a given number of
-- requests (which also can be used to collect all outstanding requests).
--
data PipelineDecision n where
    Request           :: PipelineDecision Z
    Pipeline          :: PipelineDecision n
    CollectOrPipeline :: PipelineDecision (S n)
    Collect           :: PipelineDecision (S n)


-- | The callback gets the following arguments:
--
-- * how many requests are not yet collected (in flight or
--   already queued)
-- * block number of client's tip
-- * block number of server's tip
--
-- Client's tip block number and server's tip block number can only be equal
-- (from the client's perspective) when both the client's and the server's tip
-- headers agree. If they would not agree (server forked), then the server
-- sends 'MsgRollBackward', which rolls back one block and causes the client's
-- tip and the server's tip to differ.
--
-- In this module we implement three pipelining strategies:
--
-- * 'pipelineDecisionMax'
-- * 'pipelineDecisionMin'
-- * 'pipelineDecisionLowHighMark'
--
data MkPipelineDecision where
     MkPipelineDecision
       :: (forall n. Nat n
                  -> WithOrigin BlockNo
                  -> WithOrigin BlockNo
                  -> (PipelineDecision n, MkPipelineDecision))
       -> MkPipelineDecision

runPipelineDecision
    :: MkPipelineDecision
    -> Nat n -> WithOrigin BlockNo -> WithOrigin BlockNo
    -> (PipelineDecision n, MkPipelineDecision)
runPipelineDecision (MkPipelineDecision f) n clientTipBlockNo serverTipBlockNo =
    f n clientTipBlockNo serverTipBlockNo


constantPipelineDecision
   :: (forall n. Nat n -> WithOrigin BlockNo -> WithOrigin BlockNo -> PipelineDecision n)
   -> MkPipelineDecision
constantPipelineDecision f = MkPipelineDecision
  $ \n clientTipBlockNo serverTipBlockNo ->
    (f n clientTipBlockNo serverTipBlockNo, constantPipelineDecision f)


-- | Present maximal pipelining of at most @omax@ requests.  Collect responses
-- either when we are at the same block number as the server or when we sent
-- more than @omax@ requests.
--
-- If @omax = 3@ this pipelining strategy will generate a sequence:
-- @
--    Pipeline
--    Pipeline
--    Pipeline
--    Collect
--    Pipeline
--    Collect
--    ....
--    Pipeline
--    Collect
--    Collect
--    Collect
-- @
--
pipelineDecisionMax :: Word32 -> Nat n -> WithOrigin BlockNo -> WithOrigin BlockNo
                    -> PipelineDecision n
pipelineDecisionMax omax n cliTipBlockNo srvTipBlockNo =
    case n of
      Zero   -- We are insync with the server's tip. We use
             -- equality so that this does not get triggered when we are ahead
             -- of the producer, and it will send us 'MsgRollBackward'.
             | cliTipBlockNo == srvTipBlockNo
             -> Request

             | otherwise
             -> Pipeline

      Succ{} -- We pipelined some requests and we are now synchronised or we
             -- exceeded the pipelining limit, and thus we should await for a
             -- response.
             --
             -- Note: we add @omax'@ to avoid a deadlock in tests. This
             -- pipelining strategy collects at this stage a single result,
             -- this causes @n'@ to drop and we will pipeline the next
             -- message. This assures that when we approach the end of the
             -- chain we will collect all outstanding requests without
             -- pipelining a request.
             | cliTipBlockNo `bnoPlus` n' >= srvTipBlockNo || n' >= omax'
             -> Collect

             | otherwise
             -> Pipeline
  where
    n' :: BlockNo
    n' = fromIntegral (natToInt n)

    omax' :: BlockNo
    omax' = fromIntegral omax


-- | Present minimum pipelining of at most @omax@ requests, collect responses
-- eagerly.
--
pipelineDecisionMin :: Word32 -> Nat n -> WithOrigin BlockNo -> WithOrigin BlockNo
                    -> PipelineDecision n
pipelineDecisionMin omax n cliTipBlockNo srvTipBlockNo =
    case n of
      Zero   -- We are insync with the server's tip. We use
             -- equality so that this does not get triggered when we are ahead
             -- of the producer, and it will send us 'MsgRollBackward'.
             | cliTipBlockNo == srvTipBlockNo
             -> Request

             | otherwise
             -> Pipeline

      Succ{} -- We pipelined some requests and we are now synchronised or we
             -- exceeded the pipelining limit, and thus we should wait for a
             -- response.
             | cliTipBlockNo `bnoPlus` n' >= srvTipBlockNo || n' >= omax'
             -> Collect

             | otherwise
             -> CollectOrPipeline
  where
    n' :: BlockNo
    n' = fromIntegral (natToInt n)

    omax' :: BlockNo
    omax' = fromIntegral omax


-- | Pipelining strategy which pipelines up to @highMark@ requests; if the
-- number of pipelined messages exceeds the high mark, it collects messages
-- until there are at most @lowMark@ outstanding requests.
--
pipelineDecisionLowHighMark :: Word32 -> Word32-> MkPipelineDecision
pipelineDecisionLowHighMark lowMark highMark =
    assert (lowMark <= highMark) goLow
  where
    goZero :: Nat Z -> WithOrigin BlockNo -> WithOrigin BlockNo
           -> (PipelineDecision Z, MkPipelineDecision)
    goZero Zero clientTipBlockNo serverTipBlockNo
      | clientTipBlockNo == serverTipBlockNo
      = (Request, goLow)

      | otherwise
      = (Pipeline, goLow)

    -- Mutually recursive pipeline decision strategies; we start with 'goLow',
    -- when we go above the high mark, switch to 'goHigh', switch back to
    -- 'goLow' when we go below the low mark.
    goLow, goHigh  :: MkPipelineDecision

    goLow = MkPipelineDecision $
      \n clientTipBlockNo serverTipBlockNo ->
        case n of
          Zero   -> goZero n clientTipBlockNo serverTipBlockNo

          Succ{} | clientTipBlockNo `bnoPlus` n' >= serverTipBlockNo
                 -> (Collect, goLow)

                 | n' >= highMark'
                 -> (Collect, goHigh)

                 | otherwise
                 -> (CollectOrPipeline, goLow)
            where
              n' :: BlockNo
              n' = fromIntegral (natToInt n)

    goHigh = MkPipelineDecision $
      \n clientTipBlockNo serverTipBlockNo ->
      case n of
        Zero   -> goZero n clientTipBlockNo serverTipBlockNo

        Succ{} ->
            if n' > lowMark'
              then (Collect,           goHigh)
              else (CollectOrPipeline, goLow)
          where
            n' :: BlockNo
            n' = fromIntegral (natToInt n)

    lowMark' :: BlockNo
    lowMark' = fromIntegral lowMark

    highMark' :: BlockNo
    highMark' = fromIntegral highMark

bnoPlus :: WithOrigin BlockNo -> BlockNo -> WithOrigin BlockNo
bnoPlus (At x) y = At (x + y)
bnoPlus Origin y = At (1 + y)
