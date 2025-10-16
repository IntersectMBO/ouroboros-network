{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

-- | Encapsulates signature validation utilities leveraged by the mempool writer
--
module DMQ.Protocol.SigSubmission.Validate where

import Control.Monad
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Except.Extra
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable
import Data.Word

import Cardano.Crypto.DSIGN.Class (ContextDSIGN)
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.KES.Class (KESAlgorithm (..))
import Cardano.KESAgent.KES.Crypto as KES
import Cardano.KESAgent.KES.Evolution as KES
import Cardano.KESAgent.KES.OCert (OCert (..), OCertSignable, validateOCert)
import Cardano.Ledger.BaseTypes.NonZero
import Cardano.Ledger.Hashes

import DMQ.Diffusion.NodeKernel (PoolValidationCtx (..))
import DMQ.Protocol.SigSubmission.Type
import Ouroboros.Consensus.Shelley.Ledger.Query
import Ouroboros.Network.TxSubmission.Mempool.Simple
import Ouroboros.Network.Util.ShowProxy


-- | The type of non-fatal failures reported by the mempool writer
-- for invalid messages
--
data instance MempoolAddFail (Sig crypto) =
    SigInvalid SigValidationError
  | SigDuplicate
  | SigExpired
  | SigResultOther Text
  deriving (Eq, Show)

instance (Typeable crypto) => ShowProxy (MempoolAddFail (Sig crypto))

instance ToJSON (MempoolAddFail (Sig crypto)) where
  toJSON SigDuplicate = String "duplicate"
  toJSON SigExpired   = String "expired"
  toJSON (SigInvalid e) = object
    [ "type" .= String "invalid"
    , "reason" .= show e
    ]
  toJSON (SigResultOther txt) = object
    [ "type" .= String "other"
    , "reason" .= txt
      ]


data SigValidationError =
    InvalidKESSignature KESPeriod KESPeriod String
  | InvalidSignatureOCERT
      !Word64    -- OCert counter
      !KESPeriod -- OCert KES period
      !String    -- DSIGN error message
  | InvalidOCertCounter
      Word64 -- last seen
      Word64 -- received
  | KESBeforeStartOCERT KESPeriod KESPeriod
  | KESAfterEndOCERT KESPeriod KESPeriod
  | UnrecognizedPool
  | NotInitialized
  | ClockSkew
  deriving (Eq, Show)


-- TODO:
--  We don't validate ocert numbers, since we might not have necessary
--  information to do so, but we can validate that they are growing.
validateSig :: forall crypto m.
               ( Crypto crypto
               , ContextDSIGN (KES.DSIGN crypto) ~ ()
               , DSIGN.Signable (DSIGN crypto) (OCertSignable crypto)
               , ContextKES (KES crypto) ~ ()
               , Signable (KES crypto) ByteString
               , MonadSTM m
               )
            => KES.EvolutionConfig
            -> (DSIGN.VerKeyDSIGN (DSIGN crypto) -> KeyHash StakePool)
            -> [Sig crypto]
            -> PoolValidationCtx m
            -- ^ cardano pool id verification
            -> ExceptT (Sig crypto, MempoolAddFail (Sig crypto)) m
                       [(Sig crypto, Either (MempoolAddFail (Sig crypto)) ())]
validateSig _ec verKeyHashingFn sigs ctx = traverse process' sigs
  where
    DMQPoolValidationCtx now mNextEpoch pools ocertCountersVar = ctx

    process' sig = bimapExceptT (sig,) (sig,) $ process sig

    process Sig { sigSignedBytes = signedBytes,
                  sigKESPeriod,
                  sigOpCertificate = SigOpCertificate ocert@OCert {
                    ocertKESPeriod,
                    ocertVkHot,
                    ocertN
                    },
                  sigColdKey = SigColdKey coldKey,
                  sigKESSignature = SigKESSignature kesSig
                } = do
      sigKESPeriod < endKESPeriod
         ?! KESAfterEndOCERT endKESPeriod sigKESPeriod
      sigKESPeriod >= startKESPeriod
         ?! KESBeforeStartOCERT startKESPeriod sigKESPeriod
      e <- case Map.lookup (verKeyHashingFn coldKey) pools of
        Nothing | isNothing mNextEpoch
                  -> invalid SigResultOther $ Text.pack "not initialized yet"
                | otherwise
                  -> left $ SigInvalid UnrecognizedPool
        -- TODO make 5 a constant
        Just ss | not (isZero (ssSetPool ss)) ->
                    if | now < nextEpoch -> success
                         -- localstatequery is late, but the pool is about to expire
                       | isZero (ssMarkPool ss)
                       , now > addUTCTime 5 nextEpoch -> left SigExpired
                         -- we bound the time we're willing to approve a message
                         -- in case smth happened to localstatequery and it's taking
                         -- too long to update our state
                       | now <= addUTCTime 5 nextEpoch -> success
                       | otherwise -> left $ SigInvalid ClockSkew
                | not (isZero (ssMarkPool ss)) ->
                    -- we take abs time in case we're late with our own
                    -- localstatequery update, and/or the other side's clock
                    -- is ahead, and we're just about or have just crossed the epoch
                    -- and the pool is expected to move into the set mark
                    if | abs (diffUTCTime nextEpoch now) <= 5 -> success
                       | diffUTCTime nextEpoch now > 5 ->
                           left . SigResultOther $ Text.pack "pool not eligible yet"
                       | otherwise -> right . Left $ SigInvalid ClockSkew
                  -- pool is deregistered and ineligible to mint blocks
                | isZero (ssSetPool ss) ->
                    left SigExpired
                | otherwise -> error "validateSig: impossible pool validation error"
          where
            -- mNextEpoch and pools are initialized in one STM transaction
            -- and fromJust will not fail here
            nextEpoch = fromJust mNextEpoch
      -- validate OCert, which includes verifying its signature
      validateOCert coldKey ocertVkHot ocert
         ?!: InvalidSignatureOCERT ocertN sigKESPeriod
      -- validate KES signature of the payload
      verifyKES () ocertVkHot
                (unKESPeriod sigKESPeriod - unKESPeriod startKESPeriod)
                (LBS.toStrict signedBytes)
                kesSig
         ?!: InvalidKESSignature ocertKESPeriod sigKESPeriod
      join . lift . atomically $ stateTVar ocertCountersVar \ocertCounters ->
        let f = \case
              Nothing -> Right $ Just ocertN
              Just n | n <= ocertN -> Right $ Just ocertN
                     | otherwise   -> Left . throwE . SigInvalid $ InvalidOCertCounter n ocertN
        in case Map.alterF f (verKeyHashingFn coldKey) ocertCounters of
          Right ocertCounters' -> (void success, ocertCounters')
          Left  err            -> (err, ocertCounters)
      -- for eg. remember to run all results with possibly non-fatal errors
      right e
      where
        success = right $ Right ()
        invalid tag = right . Left . tag

        startKESPeriod, endKESPeriod :: KESPeriod

        startKESPeriod = ocertKESPeriod
        -- TODO: is `totalPeriodsKES` the same as `praosMaxKESEvo`
        -- or `sgMaxKESEvolution` in the genesis file?
        endKESPeriod   = KESPeriod $ unKESPeriod startKESPeriod
                                   + totalPeriodsKES (Proxy :: Proxy (KES crypto))

        (?!:) :: Either e1 ()
              -> (e1 -> SigValidationError)
              -> ExceptT (MempoolAddFail (Sig crypto)) m ()
        (?!:) result f = firstExceptT (SigInvalid . f) . hoistEither $ result

        (?!) :: Bool
             -> SigValidationError
             -> ExceptT (MempoolAddFail (Sig crypto)) m ()
        (?!) flag sve = if flag then void success else left (SigInvalid sve)

        infix 1 ?!
        infix 1 ?!:
