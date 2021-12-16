{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.TipSample.Examples where

import           Cardano.Slotting.Slot (SlotNo (..))

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

import           Network.TypedProtocol.Pipelined (N (..), Nat (Succ, Zero),
                     natToInt, unsafeIntToNat)

import           Ouroboros.Network.Protocol.TipSample.Client
import           Ouroboros.Network.Protocol.TipSample.Server

import           Test.QuickCheck


data Request tip
    = forall (n :: N). RequestFollowTips (Nat (S n)) SlotNo


instance Show tip => Show (Request tip) where
    show (RequestFollowTips n slotNo) =
      concat
        [ "RequestFollowTips "
        , show (natToInt n)
        , " "
        , show slotNo
        ]

instance Eq tip => Eq (Request tip) where
    RequestFollowTips n slotNo == RequestFollowTips n' slotNo' =
         natToInt n == natToInt n'
      && slotNo == slotNo'

instance Arbitrary tip => Arbitrary (Request tip) where
    arbitrary = oneof
      [ RequestFollowTips <$> (unsafeIntToNat . getPositive <$> arbitrary)
                          <*> (SlotNo <$> arbitrary)
      ]

-- | Given a list of requests record all the responses.
--
tipSampleClientExample :: forall tip m. Applicative m
                       => [Request tip]
                       -> TipSampleClient tip m [tip]
tipSampleClientExample reqs =
    TipSampleClient $ pure (goIdle [] reqs)
  where
    goIdle
      :: [tip]
      -> [Request tip]
      -> ClientStIdle tip m [tip]
    goIdle !acc [] =
      SendMsgDone (reverse acc)
    goIdle !acc (RequestFollowTips n slotNo : as) =
      SendMsgFollowTip n slotNo (goFollowTips acc as n)

    goFollowTips
      :: [tip]
      -> [Request tip]
      -> Nat (S n)
      -> HandleTips (S n) tip m [tip]
    goFollowTips !acc as (Succ p@(Succ _)) =
      (ReceiveTip $ \tip -> pure $ goFollowTips (tip : acc) as p)
    goFollowTips !acc as (Succ Zero) =
      (ReceiveLastTip $ \tip -> pure $ goIdle (tip : acc) as)



-- | A server which sends replies from a list (used cyclicly) and returns all
-- requests.
--
tipSampleServerExample :: forall tip m. Applicative m
                       => NonEmpty tip
                       -> TipSampleServer tip m [Request tip]
tipSampleServerExample tips =
    TipSampleServer $ pure (go [] tiplist)
  where
    tiplist = cycle $ NonEmpty.toList tips

    go :: [Request tip]
       -> [tip]
       -> ServerStIdle tip m [Request tip]
    go _acc [] = error "tipSampleServerExample: impossible happened"
    go !acc as@(_ : _) =
      ServerStIdle {
          handleFollowTip = \n slotNo -> pure $ goFollowTip n (RequestFollowTips n slotNo : acc) as,
          handleDone      = pure $ reverse acc
        }

    goFollowTip :: Nat (S n)
                -> [Request tip]
                -> [tip]
                -> SendTips (S n) tip m [Request tip]
    goFollowTip n@(Succ Zero)       !acc (a : as) =
      SendLastTip n a (go acc as)
    goFollowTip n@(Succ p@(Succ _)) !acc (a : as) =
      SendNextTip n a (pure $ goFollowTip p acc as)
    goFollowTip _ _ [] =
      error "tipSampleServerExample: impossible happened"
