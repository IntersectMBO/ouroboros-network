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

import Control.Exception
import Control.Monad.Class.MonadTime.SI
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
import Text.Printf

import Cardano.Crypto.DSIGN.Class (ContextDSIGN)
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.KES.Class (KESAlgorithm (..))
import Cardano.KESAgent.KES.Crypto as KES
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
  toJSON (SigInvalid txt) = object
    [ "type" .= String "invalid"
    -- , "reason" .= txt
    ]
  toJSON (SigResultOther txt) = object
    [ "type" .= String "other"
    , "reason" .= txt
      ]


-- | The policy which is realized by the mempool writer when encountering
-- an invalid message.
--
data ValidationPolicy =
    FailDefault | FailSoft

data SigValidationError =
    InvalidKESSignature KESPeriod KESPeriod String
  | InvalidSignatureOCERT
      !Word64    -- OCert counter
      !KESPeriod -- OCert KES period
      !String    -- DSIGN error message
  | KESBeforeStartOCERT KESPeriod KESPeriod
  | KESAfterEndOCERT KESPeriod KESPeriod
  | UnrecognizedPool
  | ExpiredPool
  | NotInitialized
  | ClockSkew
  deriving (Eq, Show)

-- TODO fine tune policy
sigValidationPolicy
  :: SigValidationError
  -> Either (MempoolAddFail (Sig crypto)) (MempoolAddFail (Sig crypto))
sigValidationPolicy sve = case sve of
  InvalidKESSignature {} -> Left $ SigInvalid sve
  InvalidSignatureOCERT {} -> Left $ SigInvalid sve
  KESAfterEndOCERT {} -> Left SigExpired
  KESBeforeStartOCERT start sig ->
    Left . SigResultOther . Text.pack
    $ printf "KESBeforeStartOCERT %s %s" (show start) (show sig)
  UnrecognizedPool -> Left $ SigInvalid sve
  ClockSkew -> Left $ SigInvalid sve
  ExpiredPool -> Left $ SigInvalid sve
  NotInitialized -> Right . SigResultOther $ Text.pack "not initialized yet"

-- TODO:
--  We don't validate ocert numbers, since we might not have necessary
--  information to do so, but we can validate that they are growing.
validateSig :: forall crypto m.
               ( Crypto crypto
               , ContextDSIGN (KES.DSIGN crypto) ~ ()
               , DSIGN.Signable (DSIGN crypto) (OCertSignable crypto)
               , ContextKES (KES crypto) ~ ()
               , Signable (KES crypto) ByteString
               , Monad m
               )
            => ValidationPolicy
            -> (DSIGN.VerKeyDSIGN (DSIGN crypto) -> KeyHash StakePool)
            -> [Sig crypto]
            -> PoolValidationCtx
            -- ^ cardano pool id verification
            -> ExceptT (Sig crypto, MempoolAddFail (Sig crypto)) m
                       [(Sig crypto, Either (MempoolAddFail (Sig crypto)) ())]
validateSig policy verKeyHashingFn sigs ctx = traverse process' sigs
  where
    DMQPoolValidationCtx now mNextEpoch pools = ctx

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
      e1 <- sigKESPeriod < endKESPeriod
         ?! KESAfterEndOCERT endKESPeriod sigKESPeriod
      e2 <- sigKESPeriod >= startKESPeriod
         ?! KESBeforeStartOCERT startKESPeriod sigKESPeriod
      e3 <- case Map.lookup (verKeyHashingFn coldKey) pools of
        Nothing | isNothing mNextEpoch -> classifyError NotInitialized
                | otherwise            -> classifyError UnrecognizedPool
        -- TODO make 5 a constant
        Just ss | not (isZero (ssSetPool ss)) ->
                    if | now < nextEpoch -> success
                         -- localstatequery is late, but the pool is about to expire
                       | isZero (ssMarkPool ss) -> classifyError ExpiredPool
                         -- we bound the time we're willing to approve a message
                         -- in case smth happened to localstatequery and it's taking
                         -- too long to update our state
                       | now <= addUTCTime 5 nextEpoch -> success
                       | otherwise -> classifyError ClockSkew
                | not (isZero (ssMarkPool ss)) ->
                    -- we take abs time in case we're late with our own
                    -- localstatequery update, and/or the other side's clock
                    -- is ahead, and we're just about or have just crossed the epoch
                    -- and the pool is expected to move into the set mark
                    if abs (diffUTCTime nextEpoch now) <= 5
                      then success
                      else classifyError ClockSkew
                  -- pool is deregistered and ineligible to mint blocks
                | isZero (ssMarkPool ss) && isZero (ssSetPool ss) ->
                    classifyError ExpiredPool
                | otherwise -> error "validateSig unexpected pool validation error"
          where
            -- mNextEpoch and pools are initialized in one STM transaction
            -- and fromJust will not fail here
            nextEpoch = fromJust mNextEpoch

      -- validate OCert, which includes verifying its signature
      e4 <- validateOCert coldKey ocertVkHot ocert
         ?!: InvalidSignatureOCERT ocertN sigKESPeriod
      -- validate KES signature of the payload
      e5 <- verifyKES () ocertVkHot
                   (unKESPeriod sigKESPeriod - unKESPeriod startKESPeriod)
                   (LBS.toStrict signedBytes)
                   kesSig
         ?!: InvalidKESSignature ocertKESPeriod sigKESPeriod
      -- for eg. remember to run all results with possibly non-fatal errors
      right $ e1 >> e2 >> e3 >> e4 >> e5
      where
        success = right $ Right ()

        startKESPeriod, endKESPeriod :: KESPeriod

        startKESPeriod = ocertKESPeriod
        -- TODO: is `totalPeriodsKES` the same as `praosMaxKESEvo`
        -- or `sgMaxKESEvolution` in the genesis file?
        endKESPeriod   = KESPeriod $ unKESPeriod startKESPeriod
                                   + totalPeriodsKES (Proxy :: Proxy (KES crypto))

        classifyError sigValidationError = case policy of
          FailSoft ->
            let mempoolAddFail = either id id (sigValidationPolicy sigValidationError)
             in right $ Left mempoolAddFail
          FailDefault ->
            either throwE (right . Left)
                   (sigValidationPolicy sigValidationError)

        (?!:) :: Either e1 ()
              -> (e1 -> SigValidationError)
              -> ExceptT (MempoolAddFail (Sig crypto)) m
                         (Either (MempoolAddFail (Sig crypto)) ())
        (?!:) = (handleE classifyError .) . flip firstExceptT . hoistEither . fmap Right

        (?!) :: Bool
             -> SigValidationError
             -> ExceptT (MempoolAddFail (Sig crypto)) m
                        (Either (MempoolAddFail (Sig crypto)) ())
        (?!) flag sve = if flag then success else classifyError sve

        infix 1 ?!
        infix 1 ?!:
