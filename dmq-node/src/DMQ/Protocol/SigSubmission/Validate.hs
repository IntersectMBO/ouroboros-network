{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
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
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
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

-- | The type of exception raised by the mempool writer for invalid messages
-- as determined by the validation procedure and severity policy
--
newtype instance InvalidTxsError SigValidationError = InvalidTxsError SigValidationError

deriving instance Show (InvalidTxsError SigValidationError)
instance Exception (InvalidTxsError SigValidationError)

-- | The policy which is realized by the mempool writer when encountering
-- an invalid message.
--
data ValidationSeverity =
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
            => ValidationSeverity
            -> (DSIGN.VerKeyDSIGN (DSIGN crypto) -> KeyHash StakePool)
            -> [Sig crypto]
            -> PoolValidationCtx
            -- ^ cardano pool id verification
            -> Except (InvalidTxsError SigValidationError) [Either (MempoolAddFail (Sig crypto)) ()]
validateSig severity verKeyHashingFn sigs ctx = firstExceptT InvalidTxsError $ traverse process sigs
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
        Just ss | not (isZero (ssSetPool ss)) -> right $ Right ()
                | not (isZero (ssMarkPool ss))
                , Just nextEpoch <- mNextEpoch
                -- TODO make this a constant
                , diffUTCTime nextEpoch now <= 5 -> right $ Right ()
                | otherwise -> classifyError ClockSkew
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

        classifyError sigValidationError = case severity of
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
