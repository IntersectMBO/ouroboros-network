{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.TipSample.Client
  ( -- * 'TipSampleClient'
    TipSamplePolicy (..)
  , simpleTipSamplePolicy
  , TipSampleActions (..)
  , tipSampleClient

    -- * Errors & traces
  , TipSampleClientTrace (..)
  , TipSampleClientValidationError (..)

    -- Exported for testing
  , TipSampleDecision (..)
  , randomTipSampleDecision 
  ) where

import           Control.Arrow (second)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime

import           Control.Tracer (Tracer, traceWith)


import           Control.Exception (Exception (..))
import           Data.Foldable (traverse_)
import           Data.Typeable (Typeable)
import           Data.Word (Word)
import           System.Random

import           Network.TypedProtocol.Pipelined ( N (..)
                                                 , Nat (Succ, Zero)
                                                 , unsafeIntToNat )

import           Cardano.Slotting.Slot ( SlotNo (..), WithOrigin (..) )

import           Ouroboros.Network.Block ( Tip (..)
                                         , StandardHash
                                         , getTipSlotNo )
import           Ouroboros.Network.Mux (ControlMessageSTM, ControlMessage (..))
import           Ouroboros.Network.TipSample.TipFragment ( TipFragment
                                                         , TipFragment' ((:>), Empty)
                                                         , Timed (..)
                                                         )
import qualified Ouroboros.Network.TipSample.TipFragment as TF
import           Ouroboros.Network.Protocol.TipSample.Client




-- | Policies which drive the 'TipSample' client.
--
data TipSamplePolicy = TipSamplePolicy {
      -- | Max slot history kept by the client.
      --
      tspMaxHistory               :: Word,

      -- | Bounds on uniform random choice of a slot ahead of the current slot.
      --
      tspSlotRange                :: (Word, Word),

      -- | Weight for asking a single tip.
      --
      tspFollowSingleTipWeight    :: Word,

      -- | Weight for asking a multiple tips.
      --
      tspFollowMultipleTipsWeight :: Word,

      -- | Range from which we randomly choose how many tips to follow.  The
      -- minimum range should be greater or equal @2@.
      --
      tspMultipleTipsRange        :: (Word, Word)
    }
  deriving Show


data TipSampleDecision =
      -- | Ask for a single tip after that many slots of the current slot.
      FollowSingleTip Word

      -- | Ask for multiple tips
    | FollowMultipleTips
         Word -- ^ after that many slots of the current slot.
         Word -- ^ number of tips to receive


randomTipSampleDecision
    :: RandomGen g
    => TipSamplePolicy
    -> g
    -> (TipSampleDecision, g)
randomTipSampleDecision
    TipSamplePolicy {
      tspFollowSingleTipWeight,
      tspFollowMultipleTipsWeight,
      tspSlotRange,
      tspMultipleTipsRange
    }
    g =
      case randomR (0, 100) g of
        (a, g') | a <= 100 * tspFollowSingleTipWeight `div` total
                -> case randomR tspSlotRange g' of
                    (n, g'') -> (FollowSingleTip n , g'')

                | otherwise
                -> case randomR tspSlotRange g' of
                    (n, g'') -> case randomR tspMultipleTipsRange g'' of
                      (r, g''') -> (FollowMultipleTips n r, g''')
  where
    total = tspFollowSingleTipWeight + tspFollowMultipleTipsWeight


data TipSampleState header = TipSampleState {
    tssLastTip :: !(Tip header),
    tssGen     :: !StdGen
  }


data TipSampleClientValidationError header =
    TipSampleWrongSlot SlotNo (Tip header)
  | TipSampleWrongTip (Tip header) (Tip header)
  deriving Show

instance (Typeable header, StandardHash header)
    => Exception (TipSampleClientValidationError header)

afterSlotNo :: Tip header
            -> Word
            -> SlotNo
afterSlotNo (Tip (SlotNo slotNo) _ _) x = SlotNo (slotNo + fromIntegral x)
afterSlotNo TipGenesis                x = SlotNo (fromIntegral x)


compareTipsBySlotNo :: Tip header
                    -> Tip header
                    -> Ordering
compareTipsBySlotNo (Tip slotNo _ _) (Tip slotNo' _ _) = slotNo `compare` slotNo'
compareTipsBySlotNo Tip{}            TipGenesis        = GT
compareTipsBySlotNo TipGenesis       Tip{}             = LT
compareTipsBySlotNo TipGenesis       TipGenesis        = EQ


data TipSampleActions m header = TipSampleActions {
    -- | Modify 'TipFragment' and return trace of the operation.
    --
    tsaModifyTipFragment :: forall a. (TipFragment header -> (a, TipFragment header)) -> STM m a,

    -- | Get current 'SlotNo'.  The client maintains 'TipFragment' only for
    -- a 'tspMaxHistory' of slots.
    --
    tsaGetCurrentSlotNo  :: STM m SlotNo,

    -- | Convert 'SlotNo' to 'Time.
    --
    tsaSlotTime          :: m (SlotNo -> Maybe Time),

    -- | 'tipSampleClient' tracer.
    --
    tsaTracer            :: Tracer m (TipSampleClientTrace header)
  }


-- | We request tip for a given slot (at most) `10` slots before it will become
-- the current slot.
--
networkTipSlack :: SlotNo
networkTipSlack = SlotNo 10


-- | 'TipSample' client application.  It sends requests according to
-- 'TipSamplePolicy', and records received tips.
--
-- It maintains the invariant that the slots increase monotonically in
-- 'TipFragment'.  When we request multiple tips, the 'SlotNo' can go backward
-- (if the remote peer rollbacked its chain).  In this case we rollback the
-- 'TipFragment' too.
--
tipSampleClient :: forall header m.
                   ( MonadMonotonicTime m
                   , MonadSTM           m
                   , MonadThrow         m
                   , StandardHash header
                   , Typeable     header
                   )
                => TipSamplePolicy
                -> TipSampleActions m header
                -> ControlMessageSTM m
                -> StdGen
                -- ^ Assuming that the `StdGen` is a result of a `split`.
                -> TipSampleClient (Tip header) m ()
tipSampleClient tipSamplePolicy@TipSamplePolicy { tspMaxHistory }
                TipSampleActions {
                  tsaModifyTipFragment,
                  tsaGetCurrentSlotNo,
                  tsaSlotTime,
                  tsaTracer
                }
                controlMessageSTM g =
    TipSampleClient $ clientStIdle (TipSampleState TipGenesis g)
  where
    -- We only validate 'SlotNo'; at this point we cannot trust the received
    -- hash or 'BlockNo'.  Its validation is deffered until we actually use the
    -- tip.
    --
    -- Note: we only validate the 'Tip' if we expect it to move forward.
    validateTip :: Tip header
                -> SlotNo
                -> m ()
    validateTip tip requestedSlotNo
      | getTipSlotNo tip >= At requestedSlotNo
      = pure ()
      | otherwise
      = do traceWith tsaTracer
                     (TipSampleClientProtocolValidationError requestedSlotNo tip)
           throwM (TipSampleWrongSlot requestedSlotNo tip)


    -- Await for slot or stop if we're asked for.  This is useful in 'StIdle'
    -- state, in which case we await some slot keeping the agency before
    -- deciding wheather to send a request or send the termination message.
    awaitForSlot :: SlotNo
                 -> m ControlMessage
    awaitForSlot slotNo =
      atomically $ do
        currentSlot <- tsaGetCurrentSlotNo
        if currentSlot >= slotNo
          then do
            cntrlMsg <- controlMessageSTM
            case cntrlMsg of
              Continue  -> pure Continue
              -- we are relaying on the fact that 'Quiesce' is never returned.
              Quiesce   -> retry
              Terminate -> pure Terminate
          else do
            cntrlMsg <- controlMessageSTM
            case cntrlMsg of
              Continue  -> retry
              Quiesce   -> retry
              Terminate -> pure Terminate


    clientStIdle :: TipSampleState header
                 -> m (ClientStIdle (Tip header) m ())
    clientStIdle st@TipSampleState { tssLastTip, tssGen } =

      case randomTipSampleDecision tipSamplePolicy tssGen of
        (FollowSingleTip n, tssGen') -> do
          let requestedSlotNo = afterSlotNo tssLastTip n
              r = Succ Zero
          -- We send the message at most 'networkTipSlack' slots before the
          -- requested slot no.  This give a change to send it early enough;
          -- ideally, the other end should not have a header at the requested
          -- slot when it receives the message.
          cntrlMsg <- awaitForSlot (requestedSlotNo - networkTipSlack)
          case cntrlMsg of
            Continue -> pure $
              SendMsgFollowTip
                r requestedSlotNo
                (receiveTips (Just requestedSlotNo) r st { tssGen = tssGen' })
            -- 'Quiesce' is never returned by 'awaitForSlot'
            Quiesce -> error "TipSampleClient: impossible happened."
            Terminate -> pure $ SendMsgDone ()

        (FollowMultipleTips n r, tssGen') -> do
          let requestedSlotNo = afterSlotNo tssLastTip n
              r' = unsafeIntToNat (fromIntegral r)
          -- As in the 'FollowSingleTip' case.
          cntrlMsg <- awaitForSlot (requestedSlotNo - networkTipSlack)
          case cntrlMsg of
            Continue -> pure $
              SendMsgFollowTip
                r' requestedSlotNo
                (receiveTips (Just requestedSlotNo) r' st { tssGen = tssGen' })
            Quiesce -> error "TipSampleClient: impossible happened."
            Terminate -> pure $ SendMsgDone ()


    receiveTips :: Maybe SlotNo
                -- used to validate the first tip
                -> Nat (S n)
                -> TipSampleState header
                -> HandleTips (S n) (Tip header) m ()

    receiveTips mbSlotNo (Succ Zero) st =
      ReceiveLastTip $ \tip -> do
        t <- getMonotonicTime
        -- validate the tip but only if 'mvSlotNo' is 'Just'
        traverse_ (validateTip tip) mbSlotNo
        mbOriginTime <-
          case getTipSlotNo tip of
            Origin -> pure Nothing
            At slotNo -> fmap ($ slotNo) tsaSlotTime
        let !mbPropagationTime = diffTime t <$> mbOriginTime
        traceMessage
          <- atomically $ rollbackOrRollforward st (Timed t tip) mbPropagationTime
        traceWith tsaTracer traceMessage
        clientStIdle st { tssLastTip = tip }

    receiveTips mbSlotNo (Succ n@Succ {}) st =
      ReceiveTip $ \tip -> do
        t <- getMonotonicTime
        -- validate the tip but only if 'mvSlotNo' is 'Just'
        traverse_ (validateTip tip) mbSlotNo
        mbOriginTime <-
          case getTipSlotNo tip of
            Origin -> pure Nothing
            At slotNo -> fmap ($ slotNo) tsaSlotTime
        let !mbPropagationTime = diffTime t <$> mbOriginTime
        traceMessage <- atomically (rollbackOrRollforward st (Timed t tip) mbPropagationTime)
        traceWith tsaTracer traceMessage
        pure $ receiveTips Nothing n st { tssLastTip = tip }


    -- Trim the 'TipFragment' whenever we modify the shared value.  We only
    -- keep tips 'tpsRange' from the current 'SlotNo'.
    modifyTipFragment
      :: forall a.
        (TipFragment header -> (a, TipFragment header))
      -> STM m a
    modifyTipFragment f = do
      currentSlotNo <- tsaGetCurrentSlotNo
      let initialSlotNo = case currentSlotNo of
            SlotNo a | a >= fromIntegral tspMaxHistory
                     -> SlotNo (a - fromIntegral tspMaxHistory)
                     | otherwise
                     -> SlotNo 0
      tsaModifyTipFragment (second (TF.dropOldestUntilSlotNo initialSlotNo) . f)


    rollbackOrRollforward
        :: TipSampleState header
        -> Timed (Tip header)
        -> Maybe DiffTime
        -> STM m (TipSampleClientTrace header)
    rollbackOrRollforward TipSampleState { tssLastTip } tt mbPropagationTime =
      modifyTipFragment $ \tf ->
        case tssLastTip `compareTipsBySlotNo` timedData tt of
          GT -> ( TipSampleClientAddTip tt mbPropagationTime
                , tf :> tt
                )

            -- There was a rollback.
            -- TODO: Currently we rollback our state, but in the future we
            -- could have a more clever solution.
          _ -> case timedData tt of
                  Tip slotNo _ _ -> ( TipSampleClientRollback tt mbPropagationTime
                                    , TF.dropNewestUntilSlotNo (pred slotNo) tf :> tt
                                    )
                  TipGenesis     -> ( TipSampleClientRollback tt mbPropagationTime
                                    , Empty
                                    )

--
-- Policy
--


simpleTipSamplePolicy
    :: Word
    -- ^ 'tspMaxHistory'
    -> Word
    -- ^ overall number of warm peers for which the policy is designed.
    -> Word
    -- ^ number of peers that should be challenged at each slot.
    -> Word
    -- ^ width of 'tspSlotRange'
    -> TipSamplePolicy
simpleTipSamplePolicy tspMaxHistory numberOfPeers bucketSize width =
    TipSamplePolicy {
        tspMaxHistory,
        tspSlotRange =
          let m = numberOfPeers `div` bucketSize
              w = width `div` 2
          in (m - w, m + w),
        tspFollowSingleTipWeight    = 19,
        tspFollowMultipleTipsWeight = 1,
        tspMultipleTipsRange        = (4, 6)
      }


--
-- Trace
--

data TipSampleClientTrace header =
    TipSampleClientProtocolValidationError SlotNo !(Tip header)
  | TipSampleClientAddTip   !(Timed (Tip header)) !(Maybe DiffTime)
  | TipSampleClientRollback !(Timed (Tip header)) !(Maybe DiffTime)
  deriving (Eq, Show)
