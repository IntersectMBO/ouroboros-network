{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module DMQ.Protocol.SigSubmission.Validate where

import Control.Exception
import Control.Monad.Trans.Except
import Control.Monad.Trans.Except.Extra
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Typeable
import Data.Word

import Cardano.Crypto.DSIGN.Class (ContextDSIGN)
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.KES.Class (KESAlgorithm (..))
import Cardano.KESAgent.KES.Crypto as KES
import Cardano.KESAgent.KES.OCert (OCert (..), OCertSignable, validateOCert)

import DMQ.Diffusion.NodeKernel (PoolValidationCtx)
import DMQ.Protocol.SigSubmission.Type
import Ouroboros.Network.Util.ShowProxy
import Ouroboros.Network.TxSubmission.Mempool.Simple


-- | The type of non-fatal failures when adding to the mempool
--
data instance MempoolAddFail (Sig crypto) =
    SigInvalid Text
  | SigDuplicate
  | SigExpired
  | SigResultOther Text
  deriving (Eq, Show)

instance (Typeable crypto) => ShowProxy (MempoolAddFail (Sig crypto))

newtype instance InvalidTxsError SigValidationError = InvalidTxsError SigValidationError

deriving instance Show (InvalidTxsError SigValidationError)
instance Exception (InvalidTxsError SigValidationError)

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
  deriving Show

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
            -> [Sig crypto]
            -> PoolValidationCtx
            -> Except (InvalidTxsError SigValidationError) [(Either (MempoolAddFail (Sig crypto)) ())]
validateSig severity sigs ctx = firstExceptT InvalidTxsError $ traverse process sigs
  where
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
      -- validate OCert, which includes verifying its signature
      e3 <- validateOCert coldKey ocertVkHot ocert
         ?!: InvalidSignatureOCERT ocertN sigKESPeriod
      -- validate KES signature of the payload
      e4 <- verifyKES () ocertVkHot
                   (unKESPeriod sigKESPeriod - unKESPeriod startKESPeriod)
                   (LBS.toStrict signedBytes)
                   kesSig
         ?!: InvalidKESSignature ocertKESPeriod sigKESPeriod
      -- for eg. remember to run all results with possibly non-fatal errors
      right $ e1 >> e2 >> e3 >> e4
      where
        startKESPeriod, endKESPeriod :: KESPeriod

        startKESPeriod = ocertKESPeriod
        -- TODO: is `totalPeriodsKES` the same as `praosMaxKESEvo`
        -- or `sgMaxKESEvolution` in the genesis file?
        endKESPeriod   = KESPeriod $ unKESPeriod startKESPeriod
                                   + totalPeriodsKES (Proxy :: Proxy (KES crypto))

        -- TODO fine tune policy
        policy :: SigValidationError -> Either (MempoolAddFail (Sig crypto)) (MempoolAddFail (Sig crypto))
        policy _sve = Left undefined

        (?!:) :: Either e1 a
              -> (e1 -> SigValidationError)
              -> Except SigValidationError (Either (MempoolAddFail (Sig crypto)) a)
        (?!:) = (handleE handler .) . (flip firstExceptT) . hoistEither . fmap Right
          where
            handler sigValidError = case severity of
              FailSoft ->
                let mempoolAddFail = either id id (policy sigValidError)
                 in except . Right . Left $ mempoolAddFail
              FailDefault ->
                either (const $ throwE sigValidError) (except . Right . Left) (policy sigValidError)

        (?!) :: Bool
             -> SigValidationError
             -> Except SigValidationError (Either (MempoolAddFail (Sig crypto)) ())
        (?!) flag sve = case (flag, severity) of
          (True, _)        -> right $ Right ()
          (_, FailSoft)    -> right . Left $ either id id (policy sve)
          (_, FailDefault) -> left sve

        infix 1 ?!
        infix 1 ?!:
