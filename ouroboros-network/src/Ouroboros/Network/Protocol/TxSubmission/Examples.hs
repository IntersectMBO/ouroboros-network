{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.TxSubmission.Examples where

import           Ouroboros.Network.Protocol.TxSubmission.Server


-- |
-- An example @'TxSubmissionServer'@ which sends trasactions from a fixed pool
-- of transactions.  It returns a list of pairs @(hash, tx)@ send back to the
-- client.
--
txSubmissionServerFixed
  :: forall hash tx m.
     Applicative m
  => [hash]
  -> (hash -> m tx)
  -> TxSubmissionServer hash tx m [(hash, tx)]
txSubmissionServerFixed hashes0 getTx = TxSubmissionServer (pure $ handlers [] hashes0)
    where
      handlers :: [(hash, tx)] -> [hash] -> TxSubmissionHandlers hash tx m [(hash, tx)]
      handlers !as hs =
        TxSubmissionHandlers {
          getHashes = \n -> 
            let (resp, hs') = splitAt (fromIntegral n) hs
            in pure (resp, handlers as hs'),
          getTx    = \hash -> (\tx -> (tx, handlers ((hash, tx):as) hs)) <$> getTx hash,
          done     = as
        }


