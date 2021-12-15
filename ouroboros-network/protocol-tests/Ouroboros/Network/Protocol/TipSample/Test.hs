{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.TipSample.Test where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import qualified Control.Monad.ST as ST
import           Control.Tracer (nullTracer)

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise (DeserialiseFailure,
                     Serialise (..))
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Identity (Identity (..))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Typeable (Typeable)

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.IOSim (runSimOrThrow)
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Pipelined (Nat (Succ, Zero), natToInt,
                     unsafeIntToNat)
import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)

import           Ouroboros.Network.Protocol.TipSample.Client
import           Ouroboros.Network.Protocol.TipSample.Codec
import           Ouroboros.Network.Protocol.TipSample.Direct
import           Ouroboros.Network.Protocol.TipSample.Examples
import           Ouroboros.Network.Protocol.TipSample.Server
import           Ouroboros.Network.Protocol.TipSample.Type

import           Test.Ouroboros.Network.Testing.Utils (splits2, splits3)

import           Test.QuickCheck hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


instance Arbitrary SlotNo where
    arbitrary = SlotNo <$> arbitrary
    shrink (SlotNo a) = SlotNo `map` shrink a


tests :: TestTree
tests = testGroup "Ouroboros.Network.Protocol.TipSampleProtocol"
  [ testProperty "direct"  propSampleDirect
  , testProperty "connect" propSampleConnect
  , testProperty "codec"   prop_codec_TipSample
  , testProperty "codecId" prop_codecId_TipSample
  , testProperty "codec 2-splits" prop_codec_splits2_TipSample
  , testProperty "codec 3-splits" $ withMaxSuccess 30 prop_codec_splits3_TipSample
  , testProperty "demo ST" propTipSampleDemoST
  , testProperty "demo IO" propTipSampleDemoIO
  ]

--
-- Pure tests using either 'direct', 'directPipelined', 'connect' or
-- 'connectPipelined'.
--

tipSampleExperiment
  :: ( Eq tip
     , Show tip
     , Monad m
     )
  => (forall a b. TipSampleClient tip m a -> TipSampleServer tip m b -> m (a, b))
  -> [Request tip]
  -> NonEmpty tip
  -> m Property
tipSampleExperiment run reqs resps = do
    (resps', reqs') <-
      tipSampleClientExample reqs
      `run`
      tipSampleServerExample resps
    pure $
           counterexample "requests"  (reqs'  === reqs)
      .&&. counterexample "responses" (resps' === pureClient reqs resps)


pureClient :: [Request tip] -> NonEmpty tip -> [tip]
pureClient reqs resps = go reqs (cycle (NonEmpty.toList resps))
  where
    go :: [Request tip] -> [tip] -> [tip]
    go [] _ = []
    go (RequestFollowTips n _slotNo : rs) as =
      case splitAt (natToInt n) as of
        (bs, as') -> bs ++ go rs as'


propSampleDirect :: [Request Int]
                 -> NonEmptyList Int
                 -> Property
propSampleDirect reqs (NonEmpty resps) =
    runIdentity $ tipSampleExperiment direct reqs (NonEmpty.fromList resps)


propSampleConnect :: [Request Int]
                  -> NonEmptyList Int
                  -> Property
propSampleConnect reqs (NonEmpty resps) =
    runIdentity $
      tipSampleExperiment
        (\client server -> do
          (a, b, TerminalStates TokDone TokDone) <-
            tipSampleClientPeer client
            `connect`
            tipSampleServerPeer server
          pure (a, b))
        reqs (NonEmpty.fromList resps)

--
-- Codec tests
--

instance Eq tip => Eq (AnyMessage (TipSample tip)) where
    AnyMessage (MsgFollowTip n slotNo) == AnyMessage (MsgFollowTip n' slotNo') =
         natToInt n == natToInt n'
      && slotNo == slotNo'

    AnyMessage (MsgNextTip tip) == AnyMessage (MsgNextTip tip') =
      tip == tip'

    AnyMessage (MsgNextTipDone tip) == AnyMessage (MsgNextTipDone tip') =
      tip == tip'

    AnyMessage MsgDone == AnyMessage MsgDone = True

    _ == _ = False


instance Eq tip => Eq (AnyMessageAndAgency (TipSample tip)) where
    AnyMessageAndAgency (ClientAgency TokIdle) (MsgFollowTip n slotNo)
      == AnyMessageAndAgency (ClientAgency TokIdle) (MsgFollowTip n' slotNo') =
         natToInt n == natToInt n'
      &&
         slotNo == slotNo'

    AnyMessageAndAgency (ServerAgency (TokFollowTip n)) (MsgNextTip tip)
      == AnyMessageAndAgency (ServerAgency (TokFollowTip n')) (MsgNextTip tip') =
           natToInt n == natToInt n'
        &&
           tip == tip'

    AnyMessageAndAgency (ServerAgency (TokFollowTip n)) (MsgNextTipDone tip)
      == AnyMessageAndAgency (ServerAgency (TokFollowTip n')) (MsgNextTipDone tip') =
         natToInt n == natToInt n'
      &&
         tip == tip'

    AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
      == AnyMessageAndAgency (ClientAgency TokIdle) MsgDone =
        True

    _ == _ =
      False

instance Arbitrary tip => Arbitrary (AnyMessageAndAgency (TipSample tip)) where
    arbitrary = oneof
      [ (\n slotNo -> AnyMessageAndAgency (ClientAgency TokIdle) (MsgFollowTip n slotNo))
          <$> (unsafeIntToNat . getPositive <$> arbitrary)
          <*> (SlotNo <$> arbitrary)
      , (\n tip -> AnyMessageAndAgency (ServerAgency (TokFollowTip n)) (MsgNextTip tip))
          <$> (unsafeIntToNat . (+1) . getPositive <$> arbitrary)
          <*> arbitrary
      , (\tip -> AnyMessageAndAgency (ServerAgency (TokFollowTip (Succ Zero))) (MsgNextTipDone tip))
          <$> arbitrary
      , pure (AnyMessageAndAgency (ClientAgency TokIdle) MsgDone)
      ]

codec :: ( MonadST m
         , Serialise tip
         )
      => Codec (TipSample tip)
               Serialise.DeserialiseFailure
               m ByteString
codec = codecTipSample Serialise.encode Serialise.decode

prop_codec_TipSample
  :: AnyMessageAndAgency (TipSample Int)
  -> Bool
prop_codec_TipSample msg =
    ST.runST $ prop_codecM codec msg

prop_codecId_TipSample
  :: AnyMessageAndAgency (TipSample Int)
  -> Bool
prop_codecId_TipSample msg =
    ST.runST $ prop_codecM codecTipSampleId msg

prop_codec_splits2_TipSample
  :: AnyMessageAndAgency (TipSample Int)
  -> Bool
prop_codec_splits2_TipSample msg =
    ST.runST $ prop_codec_splitsM
      splits2
      codec
      msg

prop_codec_splits3_TipSample
  :: AnyMessageAndAgency (TipSample Int)
  -> Bool
prop_codec_splits3_TipSample msg =
    ST.runST $ prop_codec_splitsM
      splits3
      codec
      msg

--
-- Network demos
--

tipSampleDemo
  :: forall tip m.
     ( MonadST    m
     , MonadSTM   m
     , MonadAsync m
     , MonadThrow m
     , Typeable  tip
     , Serialise tip
     , Eq        tip
     , Typeable  tip
     , ShowProxy tip
     )
  => Channel m ByteString
  -> Channel m ByteString
  -> [Request tip]
  -> NonEmpty tip
  -> m Bool
tipSampleDemo clientChan serverChan reqs resps = do
  let client :: TipSampleClient tip m [tip]
      client = tipSampleClientExample reqs

      server :: TipSampleServer tip m [Request tip]
      server = tipSampleServerExample resps

  ((reqs', serBS), (resps', cliBS)) <-
    runPeer nullTracer codec serverChan (tipSampleServerPeer server)
    `concurrently`
    runPeer nullTracer codec clientChan (tipSampleClientPeer client)

  pure $ reqs   == reqs'
      && resps' == pureClient reqs resps
      && serBS  == Nothing
      && cliBS  == Nothing


propTipSampleDemoST
  :: [Request Int]
  -> NonEmptyList Int
  -> Bool
propTipSampleDemoST reqs (NonEmpty resps) =
  runSimOrThrow $ do
    (clientChan, serverChan) <- createConnectedChannels
    tipSampleDemo clientChan serverChan reqs (NonEmpty.fromList resps)


propTipSampleDemoIO
  :: [Request Int]
  -> NonEmptyList Int
  -> Property
propTipSampleDemoIO reqs (NonEmpty resps) =
  ioProperty $ do
    (clientChan, serverChan) <- createConnectedChannels
    tipSampleDemo clientChan serverChan reqs (NonEmpty.fromList resps)
