{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module LedgerOnDisk.App.WWB where

import Streaming
import LedgerOnDisk.Class
import LedgerOnDisk.WWB
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Monad.RWS.Strict as Strict
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.Coerce

-- | querySize < keySpace
data Config = Config
  { lag :: !Int
  , querySize :: !Integer
  , keySpace :: !Integer
  , ticks :: !Integer
  , yellInterval :: !Integer
  } deriving stock (Show, Eq)

opForTick :: MonadReader Config m => Integer -> WWBT Int Int m (KVOperation Int Int ())
opForTick t = do
  Config{} <- ask
  let go = \case
        Nothing
          | t `mod` 3 == 0 -> Just $ DChangeTo . fromIntegral $ t
          | otherwise -> Nothing
        Just _x
          | t `mod` 5 == 0 -> Just $ DRemove
          | t `mod` 5 == 1 -> Just . DChangeTo . fromIntegral $ t
          | otherwise -> Nothing
  pure $ \m -> (HashMap.mapMaybe go m, ())

queryForTick :: MonadReader Config m => Integer -> WWBT Int Int m (QueryScope Int)
queryForTick t = do
  Config{..} <- ask
  let qs = foldMap querySingle [ fromIntegral $ (x + t * querySize) `mod` keySpace | x <-[0 .. querySize - 1] ]
  pure qs

tick :: (MonadFail m, MonadIO m, MonadState (HashMap Integer (WWBResultSet  Int Int)) m, MonadReader Config m) => Integer -> WWBT Int Int m ()
tick t = do
  Config{..} <- ask
  when (t `mod` yellInterval == 0) $ do
    liftIO . putStrLn $ "Ticking tick " <> show t

  q <- queryForTick t
  prepareOperation  q >>= \r -> modify' (t `HashMap.insert` coerce r)

  let submit_tick = t - fromIntegral lag
  when (submit_tick >= 0) $ do
    o <- opForTick submit_tick
    Just rs <- gets $ fmap coerce . HashMap.lookup submit_tick
    submitOperation rs o >>= \case
      Right () -> pure ()
      Left _e -> do
        liftIO . putStrLn $ "oops: tick" <> show t
        queryForTick submit_tick >>= prepareOperation >>= flip submitOperation o >>= \case
          Left {} -> do
            liftIO . putStrLn $ "Failed Again! " <> show t
          _ -> pure ()
    modify' $ HashMap.delete submit_tick

main :: IO ()
main = do
  -- TODO validate Config
  let
    lag = 10
    query_on_prepare = True
    querySize = 90
    keySpace = 1000
    ticks = 10000
    yellInterval = 1000

    flush_policy = FPMaxWidth 100

  let c = Config{..}

  let mainLoop = do
        liftIO . putStrLn $ "Starting"
        foldl' (\o t -> o *> tick t) (pure ()) [0..ticks-1]
        liftIO . putStrLn $ "Finished"

  (_a, _s, ()) <- Strict.runRWST (runWWBT mainLoop query_on_prepare flush_policy mempty) c HashMap.empty
  pure ()
