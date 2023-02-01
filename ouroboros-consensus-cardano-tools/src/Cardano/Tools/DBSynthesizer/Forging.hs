{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Tools.DBSynthesizer.Forging (runForge) where

import           Control.Monad (when)
import           Control.Monad.Except (runExcept)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Either (isRight)
import           Data.Maybe (isJust)
import           Data.Proxy
import           Data.Word (Word64)

import           Control.Tracer as Trace (nullTracer)

import           Ouroboros.Consensus.Block.Abstract as Block
import           Ouroboros.Consensus.Block.Forging as Block (BlockForging (..),
                     ShouldForge (..), checkShouldForge)
import           Ouroboros.Consensus.Config (TopLevelConfig, configConsensus,
                     configLedger)
import           Ouroboros.Consensus.Forecast (forecastFor)
import           Ouroboros.Consensus.HeaderValidation
                     (BasicEnvelopeValidation (..), HeaderState (..))
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState,
                     tickChainDepState)
import           Ouroboros.Consensus.Storage.ChainDB.API as ChainDB (ChainDB,
                     addBlockAsync, blockProcessed, getCurrentChain,
                     getPastLedger)
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
                     (noPunishment)
import           Ouroboros.Consensus.Util.IOLike (atomically)
import           Ouroboros.Network.AnchoredFragment as AF (Anchor (..),
                     AnchoredFragment, AnchoredSeq (..), headPoint)

import           Cardano.Tools.DBSynthesizer.Types (ForgeLimit (..),
                     ForgeResult (..))


data ForgeState =
  ForgeState {
    currentSlot  :: !SlotNo
  , forged       :: !Word64
  , currentEpoch :: !Word64
  , processed    :: !SlotNo
  }

initialForgeState :: ForgeState
initialForgeState = ForgeState 0 0 0 0

-- DUPLICATE: runForge mirrors forging loop from ouroboros-consensus/src/Ouroboros/Consensus/NodeKernel.hs
-- For an extensive commentary of the forging loop, see there.

runForge
  :: forall blk.
    ( LedgerSupportsProtocol blk )
    => EpochSize
    -> SlotNo
    -> ForgeLimit
    -> ChainDB IO blk
    -> [BlockForging IO blk]
    -> TopLevelConfig blk
    -> IO ForgeResult
runForge epochSize_ nextSlot opts chainDB blockForging cfg = do
    putStrLn $ "--> epoch size: " ++ show epochSize_
    putStrLn $ "--> will process until: " ++ show opts
    endState <- go initialForgeState {currentSlot = nextSlot}
    putStrLn $ "--> forged and adopted " ++ show (forged endState) ++ " blocks; reached " ++ show (currentSlot endState)
    pure $ ForgeResult $ fromIntegral $ forged endState
  where
    epochSize = unEpochSize epochSize_

    forgingDone :: ForgeState -> Bool
    forgingDone = case opts of
        ForgeLimitSlot s  -> (s == ) . processed
        ForgeLimitBlock b -> (b == ) . forged
        ForgeLimitEpoch e -> (e == ) . currentEpoch

    go :: ForgeState -> IO ForgeState
    go forgeState
      | forgingDone forgeState = pure forgeState
      | otherwise = go . nextForgeState forgeState . isRight
          =<< runExceptT (goSlot $ currentSlot forgeState)

    nextForgeState :: ForgeState -> Bool -> ForgeState
    nextForgeState ForgeState{currentSlot, forged, currentEpoch, processed} didForge = ForgeState {
          currentSlot = currentSlot + 1
        , forged = forged + if didForge then 1 else 0
        , currentEpoch = epoch'
        , processed = processed'
        }
      where
        processed' = processed + 1
        epoch' = currentEpoch + if unSlotNo processed' `rem` epochSize == 0 then 1 else 0


    -- just some shims; in this ported code, we use ExceptT instead of WithEarlyExit
    exitEarly'  = throwE
    lift        = liftIO

    goSlot :: SlotNo -> ExceptT String IO ()
    goSlot currentSlot = do
        -- Figure out which block to connect to
        BlockContext{bcBlockNo, bcPrevPoint} <- do
          eBlkCtx <- lift $ atomically $
            mkCurrentBlockContext currentSlot
                <$> ChainDB.getCurrentChain chainDB
          case eBlkCtx of
            Right blkCtx -> return blkCtx
            Left{}       -> exitEarly' "no block context"

        -- Get corresponding ledger state, ledgder view and ticked 'ChainDepState'
        unticked <- do
          mExtLedger <- lift $ atomically $ ChainDB.getPastLedger chainDB bcPrevPoint
          case mExtLedger of
            Just l  -> return l
            Nothing -> exitEarly' "no ledger state"

        ledgerView <-
          case runExcept $ forecastFor
                           (ledgerViewForecastAt
                              (configLedger cfg)
                              (ledgerState unticked))
                           currentSlot of
            Left err -> exitEarly' $ "no ledger view: " ++ show err
            Right lv -> return lv

        let tickedChainDepState :: Ticked (ChainDepState (BlockProtocol blk))
            tickedChainDepState =
                tickChainDepState
                  (configConsensus cfg)
                  ledgerView
                  currentSlot
                  (headerStateChainDep (headerState unticked))

        -- Check if any forger is slot leader
        let
            checkShouldForge' f =
              checkShouldForge f nullTracer cfg currentSlot tickedChainDepState

        checks <- zip blockForging <$> liftIO (mapM checkShouldForge' blockForging)

        (blockForging', proof) <- case [(f, p) | (f, ShouldForge p) <- checks] of
          x:_ -> pure x
          _   -> exitEarly' "NoLeader"

        -- Tick the ledger state for the 'SlotNo' we're producing a block for
        let tickedLedgerState :: Ticked1 (LedgerState blk) Canonical
            tickedLedgerState =
              applyChainTick
                (configLedger cfg)
                currentSlot
                (ledgerState unticked)

        -- Block won't contain any transactions
        let txs = []

        -- Actually produce the block
        newBlock <- lift $
          Block.forgeBlock blockForging'
            cfg
            bcBlockNo
            currentSlot
            tickedLedgerState
            txs
            proof

        -- Add the block to the chain DB (synchronously) and verify adoption
        let noPunish = InvalidBlockPunishment.noPunishment
        result <- lift $ ChainDB.addBlockAsync chainDB noPunish newBlock
        curTip <- lift $ atomically $ ChainDB.blockProcessed result

        when (curTip /= blockPoint newBlock) $
            exitEarly' "block not adopted"

-- | Context required to forge a block
data BlockContext blk = BlockContext
  { bcBlockNo   :: !BlockNo
  , bcPrevPoint :: !(Point blk)
  }

-- | Create the 'BlockContext' from the header of the previous block
blockContextFromPrevHeader ::
     HasHeader (Header blk)
  => Header blk
  -> BlockContext blk
blockContextFromPrevHeader hdr =
    BlockContext (succ (blockNo hdr)) (headerPoint hdr)

-- | Determine the 'BlockContext' for a block about to be forged from the
-- current slot, ChainDB chain fragment, and ChainDB tip block number
mkCurrentBlockContext
  :: forall blk.
     ( GetHeader blk
     , BasicEnvelopeValidation blk )
  => SlotNo
  -> AnchoredFragment (Header blk)
  -> Either () (BlockContext blk)
mkCurrentBlockContext currentSlot c = case c of
    Empty AF.AnchorGenesis ->
      Right $ BlockContext (expectedFirstBlockNo (Proxy @blk)) GenesisPoint

    Empty (AF.Anchor anchorSlot anchorHash anchorBlockNo) ->
      let p :: Point blk = BlockPoint anchorSlot anchorHash
      in if anchorSlot < currentSlot
           then Right $ BlockContext (succ anchorBlockNo) p
           else Left ()

    c' :> hdr -> case blockSlot hdr `compare` currentSlot of
      LT -> Right $ blockContextFromPrevHeader hdr
      GT -> Left ()
      EQ -> Right $ if isJust (headerIsEBB hdr)
        then blockContextFromPrevHeader hdr
        else BlockContext (blockNo hdr) $ castPoint $ AF.headPoint c'
