{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Ouroboros.Storage.ChainDB.Paths (
    tests
  ) where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Fragment.Diff (ChainDiff (..))
import qualified Ouroboros.Consensus.Fragment.Diff as Diff

import           Ouroboros.Consensus.Storage.ChainDB.Impl.Paths (isReachable)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Ouroboros.Storage.TestBlock
import           Test.Util.Orphans.Slotting.Arbitrary ()

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Paths" [
      testProperty "isReachable" prop_isReachable
    ]

{-------------------------------------------------------------------------------
  Test properties
-------------------------------------------------------------------------------}

prop_isReachable :: ReachableSetup TestBlock -> Property
prop_isReachable ReachableSetup { currentChain, forkTip, fork, blockInfo } =
        isReachable (flip Map.lookup blockInfo) currentChain forkTip
    === fork

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data ReachableSetup blk = ReachableSetup {
      currentChain :: AnchoredFragment (Header blk)
    , forkTip      :: RealPoint blk
    , fork         :: Maybe (ChainDiff (HeaderFields blk))
    , blockInfo    :: Map (HeaderHash blk) (VolatileDB.BlockInfo blk)
    }

deriving instance (HasHeader blk, Show (Header blk)) => Show (ReachableSetup blk)

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- | All blocks on the current chain use this value as for their body. This
-- allows us to statically convert a header from the current chain to a block.
currentChainBody :: TestBody
currentChainBody = TestBody 0 True

genFirstBlock ::
     Gen TestBody
  -> Gen TestBlock
genFirstBlock genBody = oneof
    [ firstBlock 0            <$> genBody
    , firstEBB   (const True) <$> genBody
    ]

genSuccessor ::
     (HeaderFields TestBlock, IsEBB, ChainLength)
  -> Gen TestBody
  -> Gen TestBlock
genSuccessor (prevHeaderFields, prevIsEBB, prevChainLength) genBody = frequency
    [ ( if prevWasEBB then 0 else 1
      , mkNextEBB' (const True) (prevHeaderFields, prevChainLength)
          <$> chooseSlot (prevSlot + 1) (prevSlot + 2)
          -- We don't care about EpochNo
          <*> return 0
          <*> genBody
      )
    , (3
      , mkNextBlock' (prevHeaderFields, prevChainLength)
          <$> chooseSlot (prevSlot + if prevWasEBB then 0 else 1) (prevSlot + 2)
          <*> genBody
      )
    ]
  where
    prevSlot  = headerFieldSlot prevHeaderFields
    prevWasEBB = case prevIsEBB of
        IsEBB    -> True
        IsNotEBB -> False

    chooseSlot :: SlotNo -> SlotNo -> Gen SlotNo
    chooseSlot (SlotNo start) (SlotNo end) = SlotNo <$> choose (start, end)

genChainHelper ::
     Gen TestBody
  -> Int
     -- ^ Number of headers to generate
  -> WithOrigin (HeaderFields TestBlock, IsEBB, ChainLength)
     -- ^ Optional previous block
  -> Gen (AnchoredFragment (Header TestBlock))
genChainHelper genBody = \n optPrevBlk ->
    case optPrevBlk of
      Origin | n == 0 ->
        return $ AF.Empty AF.AnchorGenesis

      Origin -> do
        firstBlk <- genFirstBlock genBody
        let initAcc  = AF.Empty AF.AnchorGenesis AF.:> getHeader firstBlk
            prevInfo = ( getBlockHeaderFields firstBlk
                       , blockToIsEBB firstBlk
                       , ChainLength 1
                       )
        go (n - 1) prevInfo initAcc

      NotOrigin prevInfo@(prevHeaderFields, _, _) -> do
        let anchor  = AF.Anchor
                        (headerFieldSlot    prevHeaderFields)
                        (headerFieldHash    prevHeaderFields)
                        (headerFieldBlockNo prevHeaderFields)
            initAcc = AF.Empty anchor
        go n prevInfo initAcc
  where
    go ::
         Int
      -> (HeaderFields TestBlock, IsEBB, ChainLength)
      -> AnchoredFragment (Header TestBlock)
      -> Gen (AnchoredFragment (Header TestBlock))
    go n prevInfo@(_, _, prevChainLength) acc = case n of
      0 -> return acc
      _ -> do
        blk <- genSuccessor prevInfo genBody
        let isEBB     = blockToIsEBB blk
            prevInfo' = (getBlockHeaderFields blk, isEBB, succ prevChainLength)
        go (n - 1) prevInfo' (acc AF.:> getHeader blk)

-- | Also returns the header corresponding to the anchor, unless the anchor is
-- genesis.
generateChain :: Gen ( AnchoredFragment (Header TestBlock)
                     , Maybe (Header TestBlock)
                     )
generateChain = sized $ \size -> do
    takeNewestN <- choose (0, size)
    fullChain   <- genChainHelper (return currentChainBody) size Origin
    let mbAnchorHdr = case AF.head (AF.dropNewest takeNewestN fullChain) of
                        Left _          -> Nothing
                        Right anchorHdr -> Just anchorHdr
    return (AF.anchorNewest (fromIntegral takeNewestN) fullChain, mbAnchorHdr)

-- | NOTE: the fork must be the minimal fork, i.e., it must not contain headers
-- that are also part of the current chain.
generateFork ::
     ( AnchoredFragment (Header TestBlock)
     , Maybe (Header TestBlock)
     )
  -> Gen (ChainDiff (HeaderFields TestBlock), [Header TestBlock])
generateFork (chain, mbAnchorHdr) = sized $ \size -> do
    -- Roll back 0 or more headers and add 0 or more headers
    rollback <- choose (0, AF.length chain)
    let mbPrevHdr = case AF.head (AF.dropNewest rollback chain) of
          Left _    -> mbAnchorHdr
          Right hdr -> Just hdr
        wiPrevInfo = headerToPrevInfo <$> withOriginFromMaybe mbPrevHdr
    toAdd <- choose (0, size)
    suffix <- genChainHelper genBody toAdd wiPrevInfo
    let diff = ChainDiff
                 (fromIntegral rollback)
                 (AF.mapAnchoredFragment
                   (castHeaderFields . getHeaderFields)
                   suffix)
    return (diff, AF.toOldestFirst suffix)
  where
    genBody :: Gen TestBody
    genBody = (`TestBody` True) <$> choose (1, 3)

    headerToPrevInfo ::
         Header TestBlock
      -> (HeaderFields TestBlock, IsEBB, ChainLength)
    headerToPrevInfo hdr =
        ( castHeaderFields $ getHeaderFields hdr
        , headerToIsEBB hdr
        , thChainLength $ unTestHeader hdr
        )

-- | Generate headers that don't fit onto the given chain, but that may fit
-- onto each other.
generateDisconnectedHeaders ::
     ( AnchoredFragment (Header TestBlock)
     , Maybe (Header TestBlock)
     )
  -> Gen (NonEmpty (Header TestBlock))
generateDisconnectedHeaders (chain, mbAnchorHdr) =
    generateFork (chain, mbAnchorHdr) `suchThatMap` dropConnectingBlock
  where
    -- 'Maybe' because the suffix might be empty or contain only a single
    -- block, which is the connecting one which we have to drop
    dropConnectingBlock ::
         (ChainDiff (HeaderFields TestBlock), [Header TestBlock])
      -> Maybe (NonEmpty (Header TestBlock))
    dropConnectingBlock (ChainDiff _ suffix, hdrs) = do
        connectingPt <- either (const Nothing) (Just . castPoint . blockPoint) $
                          AF.last suffix
        NE.nonEmpty (filter ((/= connectingPt) . headerPoint) hdrs)

chainDiffForkTip ::
     ChainDiff (HeaderFields TestBlock)
  -> Maybe (RealPoint TestBlock)
chainDiffForkTip =
      withOriginToMaybe
    . pointToWithOriginRealPoint
    . castPoint
    . Diff.getTip

headerToBlockInfo ::
     (GetHeader blk, GetPrevHash blk)
  => Header blk
  -> VolatileDB.BlockInfo blk
headerToBlockInfo hdr = VolatileDB.BlockInfo {
      biHash         = headerHash hdr
    , biSlotNo       = blockSlot  hdr
    , biBlockNo      = blockNo    hdr
    , biPrevHash     = headerPrevHash hdr
    , biIsEBB        = headerToIsEBB hdr
    -- We don't care about those two
    , biHeaderOffset = 0
    , biHeaderSize   = 0
    }

headersToBlockInfo ::
     (GetHeader blk, GetPrevHash blk, Foldable f)
  => f (Header blk)
  -> Map (HeaderHash blk) (VolatileDB.BlockInfo blk)
headersToBlockInfo = foldMap $ \hdr ->
    Map.singleton (headerHash hdr) (headerToBlockInfo hdr)

instance Arbitrary (ReachableSetup TestBlock) where
  arbitrary = do
      (currentChain, mbAnchorHdr) <- generateChain
      (forkTip, fork, forkBlockInfo) <- frequency
        [ (3, generateConnectedFork    (currentChain, mbAnchorHdr))
        , (1, generateDisconnectedFork (currentChain, mbAnchorHdr))
        ]
      let blockInfo = forkBlockInfo
                   <> headersToBlockInfo mbAnchorHdr
                   <> headersToBlockInfo (AF.toOldestFirst currentChain)
      return ReachableSetup { currentChain, forkTip, fork, blockInfo }
    where
      generateConnectedFork (currentChain, mbAnchorHdr) = do
          (forkTip, fork, headersOnFork) <-
            generateFork (currentChain, mbAnchorHdr)
              `suchThatMap` \(fork, headersOnFork) ->
                (, fork, headersOnFork) <$> chainDiffForkTip fork
          return (
              forkTip
            , Just fork
            , headersToBlockInfo headersOnFork
            )

      generateDisconnectedFork (currentChain, mbAnchorHdr) = do
          disconnectedHeaders <- generateDisconnectedHeaders (currentChain, mbAnchorHdr)
          forkTip <- elements (map headerRealPoint (NE.toList disconnectedHeaders))
          return (
              forkTip
            , Nothing
            , headersToBlockInfo disconnectedHeaders
            )

  shrink ReachableSetup { currentChain, forkTip, fork, blockInfo } =
      shrinkFork <> shrinkCurrentChain
    where
      shrinkFork = case fork of
          -- We can't shrink the fork if there is no fork
          Nothing -> []

          Just (ChainDiff rollback suffix) ->
            case suffix of
              suffix'@(_ AF.:> forkTip') AF.:> _ -> return $
                ReachableSetup {
                    currentChain
                  , forkTip   = RealPoint (blockSlot forkTip') (blockHash forkTip')
                  , fork      = Just $ ChainDiff rollback suffix'
                  , blockInfo = Map.delete (realPointHash forkTip) blockInfo
                  }
              _ -> []

      shrinkCurrentChain = case fork of
          -- We don't care much about shrinking this case: the fork is not
          -- connected.
          Nothing -> []

          Just (ChainDiff rollback suffix)
            | currentChain' AF.:> curTip <- currentChain
            , AF.anchorFromBlock curTip /= AF.castAnchor (AF.anchor suffix)
            -> return $ ReachableSetup {
                currentChain = currentChain'
              , forkTip      = forkTip
              , fork         = Just $ ChainDiff (rollback - 1) suffix
              , blockInfo    = Map.delete (headerHash curTip) blockInfo
              }
            | otherwise
            -> []
