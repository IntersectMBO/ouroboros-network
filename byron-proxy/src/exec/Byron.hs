{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Byron
  ( download
  , announce
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Lens ((^.))
import Control.Monad (forM_, unless)
import Control.Tracer (Tracer, traceWith)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy.Builder as Text
import System.Random (StdGen, getStdGen, randomR)

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Block as Cardano
import qualified Cardano.Chain.Slotting as Cardano

import qualified Pos.Binary.Class as CSL (decodeFull)
import qualified Pos.Chain.Block as CSL (Block, BlockHeader (..), GenesisBlock,
                                         HeaderHash, MainBlock, gbHeader,
                                         headerHash)
import qualified Pos.Infra.Diffusion.Types as CSL

import qualified Ouroboros.Byron.Proxy.DB as DB
import Ouroboros.Byron.Proxy.Main

-- | Download the best available chain from Byron peers and write to the
-- database, over and over again.
--
-- No exception handling is done.
download
  :: Tracer IO Text.Builder
  -> CSL.GenesisBlock -- ^ For use as checkpoint when DB is empty. Also will
                      -- be put into an empty DB.
                      -- Sadly, old Byron net API doesn't give any meaning to an
                      -- empty checkpoint set; it'll just fall over.
  -> Cardano.EpochSlots
  -> DB.DB IO
  -> ByronProxy
  -> IO x
download tracer genesisBlock epochSlots db bp = getStdGen >>= mainLoop Nothing

  where

  waitForNext
    :: Maybe (BestTip CSL.BlockHeader)
    -> STM (Either (BestTip CSL.BlockHeader) Atom)
  waitForNext mBt = do
    mBt' <- bestTip bp
    if mBt == mBt'
    -- If recvAtom retires then the whole STM will retry and we'll check again
    -- for the best tip to have changed.
    then fmap Right (recvAtom bp)
    else case mBt' of
        Nothing -> retry
        Just bt -> pure (Left bt)

  mainLoop :: Maybe (BestTip CSL.BlockHeader) -> StdGen -> IO x
  mainLoop mBt rndGen = do
    -- Wait until the best tip has changed from the last one we saw. That can
    -- mean the header changed and/or the list of peers who announced it
    -- changed.
    next <- atomically $ waitForNext mBt
    case next of
      -- TODO we don't get to know from where it was received. Problem? Maybe
      -- not.
      Right atom -> do
        traceWith tracer $ mconcat
          [ "Got atom: "
          , Text.fromString (show atom)
          ]
        mainLoop mBt rndGen
      Left bt -> do
        -- Get the tip from the database.
        -- It must not be empty; the DB must be seeded with the genesis block.
        -- FIXME throw exception, don't use error.
        tip <- DB.readTip db
        (isEBB, tipSlot, tipHash) <- case tip of
          -- If the DB is empty, we use the genesis hash as our tip, but also
          -- we need to put the genesis block into the database, because the
          -- Byron peer _will not serve it to us_!
          DB.TipGenesis -> do
            DB.appendBlocks db $ \dbwrite ->
              DB.appendBlock dbwrite (DB.LegacyBlockToWrite (Left genesisBlock))
            pure (True, 0, CSL.headerHash genesisBlock)
          DB.TipEBB   slot hash _ -> pure (True, slot, DB.coerceHashToLegacy hash)
          DB.TipBlock slot bytes -> case Binary.decodeFullAnnotatedBytes "Block or boundary" (Cardano.fromCBORABlockOrBoundary epochSlots) bytes of
            Left decoderError -> error $ "failed to decode block: " ++ show decoderError
            Right (Cardano.ABOBBoundary _) -> error $ "Corrput DB: got EBB where block expected"
            -- We have a cardano-ledger `HeaderHash` but we need a cardano-sl
            -- `HeaderHash`.
            -- FIXME should not come from DB module
            Right (Cardano.ABOBBlock blk) ->
              pure (False, slot, DB.coerceHashToLegacy (Cardano.blockHashAnnotated blk))
        -- Pick a peer from the list of announcers at random and download
        -- the chain.
        let (peer, rndGen') = pickRandom rndGen (btPeers bt)
        traceWith tracer $ mconcat
          [ "Using tip with hash "
          , Text.fromString (show tipHash)
          , " at slot "
          , Text.fromString (show tipSlot)
          , if isEBB then " (EBB)" else ""
          ]
        traceWith tracer $ mconcat
          [ "Downloading the chain with tip hash "
          , Text.fromString (show tipHash)
          , " from "
          , Text.fromString (show peer)
          ]
        _ <- downloadChain
              bp
              peer
              (CSL.headerHash (btTip bt))
              [tipHash]
              streamer
        mainLoop (Just bt) rndGen'

  -- If it ends at an EBB, the EBB will _not_ be written. The tip will be the
  -- parent of the EBB.
  -- This should be OK.
  streamer :: CSL.StreamBlocks CSL.Block IO ()
  streamer = CSL.StreamBlocks
    { CSL.streamBlocksMore = \blocks -> DB.appendBlocks db $ \dbwrite -> do
        -- List comes in newest-to-oldest order.
        let orderedBlocks = NE.toList (NE.reverse blocks)
        forM_ orderedBlocks (DB.appendBlock dbwrite . DB.LegacyBlockToWrite)
        pure streamer
    , CSL.streamBlocksDone = pure ()
    }

  pickRandom :: StdGen -> NonEmpty t -> (t, StdGen)
  pickRandom rndGen ne =
    let (idx, rndGen') = randomR (0, NE.length ne - 1) rndGen
    in  (ne NE.!! idx, rndGen')

-- | Repeatedly check the database for the latest block, and announce it
-- whenever it changes.
-- NB: only proper main blocks can be announced, not EBBs.
-- The poll interval is hard-coded to 10 seconds.
-- FIXME polling won't be needed, once we have a DB layer that can notify on
-- tip change.
-- No exception handling is done.
announce
  :: Tracer IO Text.Builder
  -> Maybe CSL.HeaderHash -- ^ Of block most recently announced.
  -> DB.DB IO
  -> ByronProxy
  -> IO x
announce tracer mHashOfLatest db bp = do
  tip <- DB.readTip db
  mHashOfLatest' <- case tip of
    -- Genesis means empty database. Announce nothing.
    DB.TipGenesis         -> pure mHashOfLatest
    -- EBBs are not announced.
    DB.TipEBB   _ _ _     -> pure mHashOfLatest
    -- Main blocks must be decoded to CSL blocks.
    DB.TipBlock _   bytes -> case CSL.decodeFull bytes of
      Left txt                               -> error "announce: could not decode block"
      Right (Left (ebb :: CSL.GenesisBlock)) -> error "announce: ebb where block expected"
      Right (Right (blk :: CSL.MainBlock))   -> do
        let header = blk ^. CSL.gbHeader
            hash   = Just (CSL.headerHash header)
        unless (hash == mHashOfLatest) (announceChain bp header)
        pure hash
  threadDelay 10000000
  announce tracer mHashOfLatest' db bp
