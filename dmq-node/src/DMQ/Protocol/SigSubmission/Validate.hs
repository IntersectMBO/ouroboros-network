{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    SigInvalid Text
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
    , "reason" .= txt
    ]
  toJSON (SigResultOther txt) = object
    [ "type" .= String "other"
    , "reason" .= txt
      ]

-- | The type of exception raised by the mempool writer for invalid messages
-- as determined by the validation procedure and policy
--
newtype instance InvalidTxsError SigValidationError = InvalidTxsError SigValidationError

deriving instance Show (InvalidTxsError SigValidationError)
instance Exception (InvalidTxsError SigValidationError)

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
  deriving Show

-- TODO fine tune policy
sigValidationPolicy
  :: SigValidationError
  -> Either (MempoolAddFail (Sig crypto)) (MempoolAddFail (Sig crypto))
sigValidationPolicy sve = case sve of
  InvalidKESSignature {} -> Left . SigInvalid . Text.pack . show $ sve
  InvalidSignatureOCERT {} -> Left . SigInvalid . Text.pack . show $ sve
  KESAfterEndOCERT {} -> Left SigExpired
  KESBeforeStartOCERT start sig ->
    Left . SigResultOther . Text.pack
    $ printf "KESBeforeStartOCERT %s %s" (show start) (show sig)
  UnrecognizedPool -> Left . SigInvalid $ Text.pack "unrecognized pool id"
  ClockSkew -> Left . SigInvalid $ Text.pack "clock skew out of range"
  ExpiredPool -> Left . SigInvalid $ Text.pack "expired pool"
  NotInitialized -> Right . SigResultOther $ Text.pack "not initialized yet"

-- TODO:
--  We don't validate ocert numbers, since we might not have necessary
--  information to do so, but we can validate that they are growing.
validateSig :: forall crypto.
               ( Crypto crypto
               , ContextDSIGN (KES.DSIGN crypto) ~ ()
               , DSIGN.Signable (DSIGN crypto) (OCertSignable crypto)
               , ContextKES (KES crypto) ~ ()
               , Signable (KES crypto) ByteString
               )
            => ValidationPolicy
            -> (DSIGN.VerKeyDSIGN (DSIGN crypto) -> KeyHash StakePool)
            -> [Sig crypto]
            -> PoolValidationCtx
            -- ^ cardano pool id verification
            -> Except (InvalidTxsError SigValidationError) [Either (MempoolAddFail (Sig crypto)) ()]
validateSig policy verKeyHashingFn sigs ctx = firstExceptT InvalidTxsError $ traverse process sigs
  where
    DMQPoolValidationCtx now mNextEpoch pools = ctx

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
        Just ss | not (isZero (ssSetPool ss))
                -- we bound the time we're willing to approve a message
                -- in case smth happened to localstatequery and it's taking
                -- too long to update our state
                , now <= addUTCTime 5 nextEpoch -> right $ Right ()
                | not (isZero (ssMarkPool ss))
                -- we take abs time in case we're late with our own
                -- localstatequery update, and/or the other side's clock
                -- is ahead, and we're just about or have just crossed the epoch
                -- and the pool is expected to move into the set mark
                , abs (diffUTCTime nextEpoch now) <= 5 -> right $ Right ()
                -- pool is deregistered and ineligible to mint blocks
                | isZero (ssMarkPool ss) && isZero (ssSetPool ss) ->
                    classifyError ExpiredPool
                | otherwise -> classifyError ClockSkew
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
        startKESPeriod, endKESPeriod :: KESPeriod

        startKESPeriod = ocertKESPeriod
        -- TODO: is `totalPeriodsKES` the same as `praosMaxKESEvo`
        -- or `sgMaxKESEvolution` in the genesis file?
        endKESPeriod   = KESPeriod $ unKESPeriod startKESPeriod
                                   + totalPeriodsKES (Proxy :: Proxy (KES crypto))

        classifyError sigValidationError = case policy of
          FailSoft ->
            let mempoolAddFail = either id id (sigValidationPolicy sigValidationError)
             in right . Left $ mempoolAddFail
          FailDefault ->
            either (const $ throwE sigValidationError) (right . Left)
                   (sigValidationPolicy sigValidationError)

        (?!:) :: Either e1 ()
              -> (e1 -> SigValidationError)
              -> Except SigValidationError (Either (MempoolAddFail (Sig crypto)) ())
        (?!:) = (handleE classifyError .) . flip firstExceptT . hoistEither . fmap Right

        (?!) :: Bool
             -> SigValidationError
             -> Except SigValidationError (Either (MempoolAddFail (Sig crypto)) ())
        (?!) flag sve = if flag then right $ Right () else classifyError sve

        infix 1 ?!
        infix 1 ?!:
