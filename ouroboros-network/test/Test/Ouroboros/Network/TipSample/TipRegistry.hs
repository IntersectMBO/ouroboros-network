{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.TipSample.TipRegistry
  ( tests

  , TestTipFragments (..)
  )
  where

import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadSTM
import           Control.Monad.IOSim
import           Control.Tracer (nullTracer)
import           Cardano.Slotting.Slot (WithOrigin (..))

import           Data.List (sort)
import           Data.Foldable (find, foldl', traverse_)
import           Data.Maybe (isNothing)

import           Ouroboros.Network.Block
import           Ouroboros.Network.AnchoredFragment ( AnchoredFragment )
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.ChainFragment as CF
import           Ouroboros.Network.TipSample.TipFragment ( TipFragment
                                                         , TipFragment' ((:<), (:>), Empty)
                                                         , Timed (..) )
import qualified Ouroboros.Network.TipSample.TipFragment as TF
import           Ouroboros.Network.TipSample.TipRegistry

import           Test.ChainFragment (TestHeaderChainFragment (..))
import           Ouroboros.Network.Testing.ConcreteBlock

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Ouroboros.Network.TipSample"
  [ testGroup "TipRegistry"
    [ testProperty "getPeerResults" prop_tipRegistry
    , testProperty "unregisterPeer" prop_unregisterPeer
    , testGroup "Generators"
      [ testProperty "TestTipFragments generator" prop_TestFragmentsGenerator
      , testProperty "TestTipFragments shrinker"  prop_TestFragmentsShrinker
      ]
    ]
  ]


data TestTipFragments = TestTipFragments (AnchoredFragment BlockHeader)
                                         (TipFragment BlockHeader)
                                         (TipFragment BlockHeader)
  deriving Show


data TipMask
  = ValidTip DiffTime
  | InvalidTip DiffTime (HeaderHash BlockHeader)
  | IgnoredTip


generateTipMask :: Gen TipMask
generateTipMask =
    frequency
      [ (6, ValidTip . fromRational . getPositive <$> arbitrary)
      , (2, InvalidTip
              <$> (fromRational . getPositive <$> arbitrary)
              <*> arbitrary
        )
      , (2, pure IgnoredTip)
      ]

-- Use the diff times and masks and transform 'ChainFragment' to
-- a 'TipFragment'.
makeTipFragment :: ChainFragment BlockHeader
                -> [TipMask]
                -> TipFragment BlockHeader
makeTipFragment cf tipMasks =
      fst
    $ foldl'
       (\(tf, t) (hdr, tipMask) ->
          case tipMask of
            IgnoredTip -> (tf, t)
            ValidTip diff ->
              let t' =  diff `addTime` t in
              ( tf :>
                Timed {
                    timedAt   = t',
                    timedData = Tip (headerSlot hdr) (headerHash hdr) (headerBlockNo hdr)
                  }
              , t')
            InvalidTip diff invalidHash ->
              let t' =  diff `addTime` t in
              ( tf :>
                Timed {
                    timedAt   = t',
                    timedData = Tip (headerSlot hdr) invalidHash (headerBlockNo hdr)
                  }
              , t'))
       (Empty, Time 0)
       (zip (CF.toOldestFirst cf) tipMasks)

instance Arbitrary TestTipFragments where
    arbitrary = do
        TestHeaderChainFragment cf <- arbitrary
        let cf' = fixCF cf
            -- generate diff times & a mask at the same time.
            mdiffs :: Gen [TipMask]
            mdiffs = vectorOf (CF.length cf') generateTipMask

        TestTipFragments (AF.mkAnchoredFragment AF.AnchorGenesis cf')
          <$> (makeTipFragment cf' <$> mdiffs)
          <*> (makeTipFragment cf' <$> mdiffs)
      where
        -- we only deal with chains that are monotonic in 'SlotNo'
        fixCF :: ChainFragment BlockHeader
              -> ChainFragment BlockHeader
        fixCF (a CF.:< as@(a' CF.:< _))
          | headerSlot a == headerSlot a'
          = fixCF as
          | otherwise
          = a CF.:< fixCF as
        fixCF as@(_ CF.:< CF.Empty) = as
        fixCF CF.Empty = CF.Empty


    -- a very basic shrinker
    shrink (TestTipFragments af tf1 tf2) =
        -- subtract head of the chain
        (case af of
          AF.Empty{}  -> []
          af' AF.:> _ ->
            case AF.headSlot af' of
              At slotNo -> [ TestTipFragments
                               af'
                               (TF.dropNewestUntilSlotNo slotNo tf1)
                               (TF.dropNewestUntilSlotNo slotNo tf2)
                           ]
              Origin    -> [ TestTipFragments af' Empty Empty ])
      ++
        -- subtract tail of the chain
        (case af of
          AF.Empty{}  -> []
          _ AF.:< af' ->
            case AF.headSlot af' of
              At slotNo -> [ TestTipFragments
                               af'
                               (TF.dropOldestUntilSlotNo (succ slotNo) tf1)
                               (TF.dropOldestUntilSlotNo (succ slotNo) tf2)
                           ]
              Origin    -> [ TestTipFragments af' Empty Empty ])

--
-- Generator tests
--

prop_TestFragmentsGenerator :: TestTipFragments -> Bool
prop_TestFragmentsGenerator (TestTipFragments _af tf1 tf2) =
       fst (foldl' monotonicPred (True, Nothing) tf1)
    && fst (foldl' monotonicPred (True, Nothing) tf2)
  where
    monotonicPred (a, Nothing) (Timed _ tip) =
      (a, Just tip)
    monotonicPred (a, Just tip) (Timed _ tip') =
      ( a && getTipSlotNo tip < getTipSlotNo tip'
      , Just tip')

prop_TestFragmentsShrinker :: TestTipFragments -> Bool
prop_TestFragmentsShrinker = all prop_TestFragmentsGenerator . shrink

--
-- TipRegistry tests
--

tipRegistryProperty :: forall m.
                       MonadSTM m
                    => TestTipFragments
                    -> m Property
tipRegistryProperty (TestTipFragments af a b) = do
    tipRegistry
      <- makeTipRegistry TipRegistryArguments {
                            traChainOffset = 0,
                            traGetCurrentChain = pure af,
                            traTracer = nullTracer
                          }
    registerPeer tipRegistry 1 >>= registerTips a
    registerPeer tipRegistry 2 >>= registerTips b

    ((pureResults a b ===) . map (\(x, _, y) -> (x, y)))
      <$> getPeerResults tipRegistry
  where

    registerTips :: TipFragment BlockHeader
                 -> TipFragmentVar Int m BlockHeader
                 -> m ()
    registerTips tf (TipFragmentVar fn) =
      traverse_ (\tt -> atomically $ fn (\tf' -> ((), tf' :> tt))) tf

    -- 'pureResults' relies on the invariant that 'TipFragment' is monotonic in
    -- 'SlotNo'.
    pureResults :: TipFragment BlockHeader
                -> TipFragment BlockHeader
                -> [(NumberOfWins, Int)]
    pureResults x y = go (NumberOfWins 0) (NumberOfWins 0)
                         (filterValidTips x) (filterValidTips y)
      where
        filterValidTips :: TipFragment BlockHeader
                        -> TipFragment BlockHeader
        filterValidTips =
            foldl' (\zs z@(Timed _ tip) ->
                     if AF.pointOnFragment (getTipPoint tip) af
                       then zs :> z
                       else zs
                   ) Empty


        go :: NumberOfWins
           -> NumberOfWins
           -> TipFragment BlockHeader
           -> TipFragment BlockHeader
           -> [(NumberOfWins, Int)]

        go !r1 !r2 Empty Empty =
            filter (\(n, _) -> n > NumberOfWins 0) $ sort [(r1, 1), (r2, 2)]

        go !r1 !r2 tf1@(tt1 :< tf1') tf2@(tt2 :< tf2') =
            case getTipSlotNo (timedData tt1) `compare` getTipSlotNo (timedData tt2) of
              EQ -> if timedAt tt1 <= timedAt tt2
                      -- biased towards peers with smaller address
                      then go (succ r1) r2 tf1' tf2'
                      else go r1 (succ r2) tf1' tf2'
              LT -> go r1 r2 tf1' tf2
              GT -> go r1 r2 tf1  tf2'

        go !r1  r2 (_ :< tf1') tf2@Empty   = go r1 r2 tf1' tf2

        go  r1 !r2 tf1@Empty   (_ :< tf2') = go r1 r2 tf1  tf2'


prop_tipRegistry :: TestTipFragments
                 -> Property
prop_tipRegistry ttf = runSimOrThrow (tipRegistryProperty ttf)


-- | Check that an unregistered peer does not appear in the results.
--
unregisterPeerProperty :: forall m. 
                          MonadSTM m
                       => TestTipFragments
                       -> m Bool
unregisterPeerProperty (TestTipFragments af a b) = do
    tipRegistry
      <- makeTipRegistry TipRegistryArguments {
                            traChainOffset = 0,
                            traGetCurrentChain = pure af,
                            traTracer = nullTracer
                          }
    registerPeer tipRegistry 1 >>= registerTips a
    registerPeer tipRegistry 2 >>= registerTips b
    unregisterPeer tipRegistry 2
    isValid <$> getPeerResults tipRegistry
  where
    isValid :: [(NumberOfWins, NumberOfPeers, Int)] -> Bool
    isValid = isNothing . find (\(_, _, peerAddr) -> peerAddr == 2)

    registerTips :: TipFragment BlockHeader
                 -> TipFragmentVar Int m BlockHeader
                 -> m ()
    registerTips tf (TipFragmentVar fn) =
      traverse_ (\tt -> atomically $ fn (\tf' -> ((), tf' :> tt))) tf

prop_unregisterPeer :: TestTipFragments
                    -> Bool
prop_unregisterPeer ttf = runSimOrThrow (unregisterPeerProperty ttf)
