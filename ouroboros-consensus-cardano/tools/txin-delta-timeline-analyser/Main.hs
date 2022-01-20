{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import qualified Control.Monad as M
import           Control.Exception (Exception, throw)
import           Data.Bits (shiftL)
import qualified Data.ByteString as BS
import           Data.ByteString.Base64 (decodeBase64)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as MM
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word32, Word64)
import qualified Data.Vector as V
import           GHC.Clock (getMonotonicTimeNSec)
import qualified Options.Applicative as O

import qualified Codec.Serialise as S
import qualified Data.Time.Clock as Time
import qualified Database.LMDB.Simple as LMDB
import           System.Directory (createDirectoryIfMissing)

import qualified Database.RocksDB as RocksDB
import qualified System.Random.Stateful as R

import           Types
import           GenesisUTxO

main :: IO ()
main = do
  analyze <- chooseAnalysis

  -- TODO Switch to conduit. That'll make pipelining a bit harder to arrange,
  -- but it's probably worth it to avoid the lazy IO footguns.
  entireStdin <- Char8.getContents
  analyze $ map parseRow $ Char8.lines entireStdin

-- Just the one for now
--
-- TODO parse the command line options to choose different analysis (eg
-- measuring different on-disk back-ends)
chooseAnalysis :: IO ([Row] -> IO ())
chooseAnalysis = do
    commandLine <- O.execParser opts
    case whichAnalysis commandLine of
      InMemSim       -> pure inMemSim
      MeasureAge     -> pure measureAge
      LmdbSim        -> pure lmdbSim
      RocksDbSim     -> pure rocksDbSim
      RocksDbInspect -> pure rocksDbInspect
  where
    opts = O.info (commandLineParser O.<**> O.helper) $
         O.fullDesc
      <> O.progDesc "Analyse a txin delta timeline file"
      <> O.header "txin-delta-timeline-analyser - a tool for empirical UTxO HD design"

data AnalysisName =
    InMemSim
  | LmdbSim
  | MeasureAge
  | RocksDbSim
  | RocksDbInspect
  deriving (Bounded, Enum, Show)

data CommandLine = CommandLine {
    whichAnalysis :: AnalysisName
  }

commandLineParser :: O.Parser CommandLine
commandLineParser =
        CommandLine
    <$> O.argument
          readerAnalysisName
          (   O.metavar "ANALYSIS_NAME"
           <> O.help ("one of: " <> unwords (map fst analysisMap))
          )

readerAnalysisName :: O.ReadM AnalysisName
readerAnalysisName = O.eitherReader $ \s ->
  case lookup s analysisMap of
    Nothing -> Left $ "no such analysis: " <> s
    Just nm -> Right nm
  where

analysisMap :: [(String, AnalysisName)]
analysisMap =
  [ (show analysisName, analysisName)
  | analysisName <- [minBound..maxBound]
  ]

{- DEBUGGING tip

$ echo 'm1dAJQVEGuOSqJ6Vc7p0nnlovsfdBLuPb26/a7VjwlI=' | base64 --decode | hexdump -ve '1/1 "%.2x"'
9b57402505441ae392a89e9573ba749e7968bec7dd04bb8f6f6ebf6bb563c252

https://explorer.cardano.org/en/transaction?id=9b57402505441ae392a89e9573ba749e7968bec7dd04bb8f6f6ebf6bb563c252

-}

{-------------------------------------------------------------------------------
Parser combinators, no backtracking, distinct type for incremental vs full
-------------------------------------------------------------------------------}

-- TODO Which existing library offers these four types and similar combinators?
data    T' o a   = Bad [String] | Ok a o
newtype T  i o a = T {unT :: i -> T' o a}
type P  s a = T s s  a   -- parses part of the input
type P0 i a = T i () a   -- parses all  of the input

runP0 :: P0 i a -> i -> Either [String] a
runP0 p s = case unT p s of
  Bad msgs -> Left  msgs
  Ok a ()  -> Right a

flattenToP0 :: P s (P0 s a) -> P0 s a
flattenToP0 p = T $ \s -> case unT p s of
  Bad msgs -> Bad msgs
  Ok p0 s'  -> unT p0 s'

embedToP :: i -> P0 i a -> P s a
embedToP s p = case runP0 p s of
  Left msgs -> T $ \_ -> Bad msgs
  Right a   -> pure a

instance (i ~ o) => Monad (T i o) where
  T f >>= k = T $ \s0 -> case f s0 of
    Bad msgs -> Bad msgs
    Ok a s1  -> k a `unT` s1
instance (i ~ o) => Applicative (T i o) where
  pure  = T . Ok
  (<*>) = M.ap
instance            Functor (T i o) where
  fmap f (T h) = T $ \i -> case h i of
    Bad msgs -> Bad msgs
    Ok a o   -> Ok (f a) o

failT :: String -> T i o a
failT msg = T $ \_ -> Bad [msg]

popP :: P [a] a
popP = T $ \case
  []   -> Bad ["popP"]
  x:xs -> Ok x xs

_toP :: (s -> (next, s)) -> P0 next a -> P s a
{-# INLINE _toP #-}
_toP uncons p = T $ \s ->
  let !(next, s') = uncons s in case runP0 p next of
    Left msgs -> Bad msgs
    Right a   -> Ok a s'

nullP0 :: P0 [i] ()
nullP0 = T $ \is ->
  if null is then Ok () () else Bad ["nullP0"]

nullToP0 :: P [i] a -> P0 [i] a
nullToP0 = flattenToP0 . fmap (\a -> a <$ nullP0)

mapP0 :: (i -> i') -> P0 i' a -> P0 i a
{-# INLINE mapP0 #-}
mapP0 prj p = T $ \i -> unT p (prj i)

sequenceP0 :: P0 i a -> P0 [i] [a]
sequenceP0 p = T $ \is -> case traverse (runP0 p) is of
  Left  msgs -> Bad msgs
  Right xs   -> Ok xs ()

guardP :: String -> Bool -> P s ()
guardP msg b = if b then pure () else failT msg

mapMaybeP0 :: String -> (a -> Maybe b) -> P0 i a -> P0 i b
mapMaybeP0 msg f p = T $ \i -> case unT p i of
  Bad msgs -> Bad msgs
  Ok a ()  -> case f a of
    Nothing -> Bad [msg]
    Just b  -> Ok b ()

intP :: P [Char8.ByteString] Int
intP = do
  bs <- popP
  case Char8.readInt bs of
    Just (i, bs') -> i <$ guardP "intP partial" (Char8.null bs')
    Nothing       -> failT "intP fail"

contextT :: String -> T i o a -> T i o a
contextT msg (T f) = T $ \i -> case f i of
  Ok a o  -> Ok a o
  Bad msgs -> Bad (msg : msgs)

{-------------------------------------------------------------------------------
Parsing a row from the timeline file
-------------------------------------------------------------------------------}

data Row = Row {
    rBlockNumber :: {-# UNPACK #-} !Word64
  , rSlotNumber  :: {-# UNPACK #-} !Word64
  , rNumTx       :: {-# UNPACK #-} !Int
  , rConsumed    :: {-# UNPACK #-} !(V.Vector TxIn)
  , rCreated     :: {-# UNPACK #-} !(V.Vector TxOutputIds)
  , rCache       :: RowCache
  }

data RowCache = RowCache {
    -- | The `TxIn` this row consumes, including those it created
    rcConsumed      :: Set TxIn
    -- | The `TxIn` this row created, including those it consumed
  , rcCreated       :: Map TxIn Word64
    -- | The `TxIn` this row consumes excluding those it created
  , rcConsumedOther :: Set TxIn
    -- | The `TxIn` this row created, excluding those it consumed
  , rcCreatedOther  :: Map TxIn Word64
  }

-- | Parse either a @\@@ item or the first part of a @#@ item, from the
-- variable length portion of each line
p0Item :: Char -> (ShortByteString -> Word32 -> ans) -> P0 Char8.ByteString ans
p0Item sep f = mapP0 (Char8.split sep) $ contextT ("p0Item " <> [sep]) $ nullToP0 $ do
  h <- popP
  h' <- either (failT . show) pure $ decodeBase64 (Char8.toStrict h)
  i' <- intP
  pure $ f (Short.toShort h') (toEnum i')

p0Output :: P0 Char8.ByteString TxOutputIds
p0Output = mapP0 (Char8.split ',') $ contextT "p0Output" $ nullToP0 $ do
  (h, n) <- popP >>= \item -> embedToP item $ p0Item '#' (,)
  sizes <- M.replicateM (fromEnum n) (toEnum <$> intP)
  pure $ TxOutputIds h $ toVec (fromEnum n) sizes

-- | Parse a line from the timeline file
parseRow :: Char8.ByteString -> Row
parseRow bs0 = run $ flattenToP0 $ do
    bn          <- intP
    sn          <- intP
    numTx       <- intP
    numConsumed <- intP
    numCreated  <- intP
    consumed    <- M.replicateM numConsumed (popP >>= \bs -> embedToP bs $ p0Item '@' TxIn)
    -- the rest of the line is parsed by the resulting P0
    pure
      $ flip (mapMaybeP0 "mismatch in number of created txins")
          (sequenceP0 p0Output)
      $ \created -> do
        M.guard $ toEnum numCreated == sum [ V.length sizes | TxOutputIds _ sizes <- created ]
        let setco      = Set.fromList $                                    consumed
            setcr      = Map.fromList $ concatMap (V.toList . outputTxIns) created
            setcoOther = Set.difference  setco (Map.keysSet setcr)
            setcrOther = Map.withoutKeys setcr setco
        pure Row {
            rBlockNumber = toEnum bn
          , rSlotNumber  = toEnum sn
          , rNumTx       = numTx
          , rConsumed    = toVec numConsumed      consumed
          , rCreated     = toVec (length created) created
          , rCache       = RowCache {
              rcConsumed      = setco
            , rcCreated       = setcr
            , rcConsumedOther = setcoOther
            , rcCreatedOther  = setcrOther
            }
          }
  where
    run :: P0 [Char8.ByteString] Row -> Row
    run =
        either (error . show) id
      . flip runP0 (Char8.words bs0) 
      . contextT (Char8.unpack bs0)

-- make a vector with exact size
toVec :: Int -> [a] -> V.Vector a
toVec n =
  V.unfoldrExactN n $ \case
    x:xs -> (x, xs)
    []   -> error "impossible! parseRow toVec"

{-------------------------------------------------------------------------------
In-memory simulation
-------------------------------------------------------------------------------}

newtype InMem = InMem (Set TxIn)

inMemSim :: [Row] -> IO ()
inMemSim =
    \rows -> do
      t0 <- getMonotonicTimeNSec
      InMem _utxo <- M.foldM (snoc t0) (InMem genesisUTxO) rows
      pure ()
  where
    snoc :: Word64 -> InMem -> Row -> IO InMem
    snoc t0 (InMem utxo) row = do
      t <- getMonotonicTimeNSec
      let reltime_milliseconds = (t - t0) `div` 1_000_000
--      M.when (0 == mod n 10000) $ do
--        putStrLn $ show n <> "\t" <> show reltime_milliseconds <> "\t" <> show (Set.size utxo)

      let blkConsumed = rcConsumedOther (rCache row)
          blkCreated  = rcCreatedOther  (rCache row)

      -- fail if a txin this block consumes is not in the utxo
      do
        let missing = blkConsumed `Set.difference` utxo
        M.unless (Set.null missing) $ error $ unlines [
            unwords ["ERROR: missing TxIn", show (rBlockNumber row), show (Set.size blkConsumed), show (Set.size missing)]
          , unwords $ map showTxIn64 $ Set.toList missing
          , unwords $ map showTxIn16 $ Set.toList missing
          ]

      let utxo' = Set.union (Map.keysSet blkCreated) $ utxo `Set.difference` blkConsumed

      putStrLn $   show (rBlockNumber row)
        <> "\t" <> show (rSlotNumber  row)
        <> "\t" <> show reltime_milliseconds
        <> "\t" <> show (Set.size blkConsumed)
        <> "\t" <> show (Map.size blkCreated)
        <> "\t" <> show (Set.size utxo')

      pure $ InMem utxo'

{-------------------------------------------------------------------------------
Measuring age
-------------------------------------------------------------------------------}

data AgeEntry = AgeEntry {
    _birthBlock :: {-# UNPACK #-} !Word64
  , _survivors  :: {-# UNPACK #-} !Word32
  }

data HASH = HASH
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Show)

txidHASH :: ShortByteString -> HASH
txidHASH h =
    if 32 /= Short.length h then error "bad hash length" else
    HASH (f 0) (f 1) (f 2) (f 3)
  where
    {-# INLINE f #-}
    f wordIndex =
          g base 0 + g base 1 + g base 2 + g base 3
        + g base 4 + g base 5 + g base 6 + g base 7
      where
        base = wordIndex * bytesPerWord64

    bytesPerWord64 = 8
    bitsPerByte    = 8

    g :: Int -> Int -> Word64
    {-# INLINE g #-}
    g byteBase byteOffset =
        (`shiftL` (bitsPerByte * (bytesPerWord64 - byteOffset - 1)))
      $ toEnum . fromEnum
      $ Short.index h (byteBase + byteOffset)

txid :: TxIn -> HASH
txid (TxIn h _) = txidHASH h

data AgeMeasures = AgeMeasures
  !(Map HASH AgeEntry)
  !(IntMap Word64)   -- staleness histogram: key is log of age of reference

measureAge :: [Row] -> IO ()
measureAge =
    \rows -> do
      t0 <- getMonotonicTimeNSec
      AgeMeasures _utxo histo <- M.foldM (snoc t0) (AgeMeasures (prep genesisUTxO) IntMap.empty) rows
      flip mapM_ (IntMap.toList histo) $ \(logAge, count) -> putStrLn $ show logAge <> "\t" <> show count
      pure ()
  where
    prep utxo0 =
        (\f -> Map.fromAscList $ map f $ Set.toAscList utxo0)
      $ \txin -> (txid txin, AgeEntry 0 1)   -- the 0 is off-by-one, but that's ignorable here

    snoc :: Word64 -> AgeMeasures -> Row -> IO AgeMeasures
    snoc t0 (AgeMeasures utxo histo) row = do
      t <- getMonotonicTimeNSec
      let reltime_milliseconds = (t - t0) `div` 1_000_000

      let blkConsumed = rcConsumedOther (rCache row)
          blkCreated  = rcCreatedOther  (rCache row)

      let updCreated :: Map HASH AgeEntry
          updCreated =   -- how many outputs this block creates for its own txs
              fmap (\c -> AgeEntry (rBlockNumber row) c)
            $ (\f -> Map.fromAscListWith (+) $ map (f . fst) $ Map.toAscList blkCreated)
            $ \txin -> (txid txin, 1)
      let updConsumed :: Map HASH Word32
          updConsumed =  -- how many outputs this block consumes from each old tx
              (\f -> Map.fromAscListWith (+) $ map f $ Set.toAscList blkConsumed)
            $ \txin -> (txid txin, 1)

      let consumptionAgeHisto :: IntMap Word64
          consumptionAgeHisto =
              IntMap.fromListWith (+)
            $ Map.elems
            $ Map.intersectionWith
                (\_c2 (AgeEntry birth _c1) -> (fromEnum (rBlockNumber row - birth), 1))
                updConsumed
                utxo

      let utxo' = MM.merge
            -- only in L
            MM.preserveMissing
            -- only in R
            (MM.mapMaybeMissing (\h _ -> error $ "impossible! " <> show (rBlockNumber row) <> " " <> show h))
            -- both
            ( MM.zipWithMaybeMatched $ \_h (AgeEntry b c1) c2 ->
                let c' = c1 - c2 in
                if 0 == c' then Nothing else Just $   -- remove totally consumed txs
                AgeEntry b c'
            )
            (Map.union utxo updCreated)
            updConsumed

      let histo' =
              IntMap.unionWith (+) histo
            $ IntMap.fromListWith (+)
            $ map (\(age, count) -> (ceiling $ logBase (2 :: Double) $ fromIntegral age, count))
            $ IntMap.toAscList consumptionAgeHisto

      putStrLn $   show (rBlockNumber row)
        <> "\t" <> show (rSlotNumber  row)
        <> "\t" <> show reltime_milliseconds
        <> ( if IntMap.null consumptionAgeHisto then "" else
                "\t" <> show (fst (IntMap.findMin consumptionAgeHisto))
             <> "\t" <> show (fst (IntMap.findMax consumptionAgeHisto))
           )

      pure $ AgeMeasures utxo' histo'

{-------------------------------------------------------------------------------
Simulating via LMDB
-------------------------------------------------------------------------------}

data LmdbSimEnv = LmdbSimEnv
  !(LMDB.Environment LMDB.ReadWrite)
  !(LMDB.Database () Word64)
  -- ^ we version by storing the successor of the slot number of the last block
  -- that was written
  !(LMDB.Database (LmdbBox TxIn) (LmdbBox TxOut))

-- | A newtype for being explicit and avoiding orphans
newtype LmdbBox a = LmdbBox a

-- TODO I think this has at least 2 bytes of CBOR overhead :(
instance S.Serialise (LmdbBox TxIn) where
  encode (LmdbBox (TxIn h i)) =                         S.encode h <>  S.encode i
  decode                      = fmap LmdbBox $ TxIn <$> S.decode   <*> S.decode

newtype TxOut = TxOut Word64   -- size

-- TODO I think this has at least 2 bytes of CBOR overhead :(
instance S.Serialise (LmdbBox TxOut) where
  encode (LmdbBox (TxOut size)) = S.encode $ BS.replicate (fromEnum size) 0
  decode                        = do
    bs <- S.decode
    pure $ LmdbBox $ TxOut $ toEnum $ BS.length bs

data LmdbAppExn =
    LmdbMissingSeqNo   Word64
  | LmdbMissingTxIn    Word64 Word64 TxIn
  | LmdbNotEmptyAtInit Word64
  deriving (Show)

instance Exception LmdbAppExn

lmdbDirName :: String
lmdbDirName = "utxo-lmdb"

lmdbSim :: [Row] -> IO ()
lmdbSim =
    \rows -> do
      mono0 <- getMonotonicTimeNSec
      t0    <- Time.getCurrentTime
      createDirectoryIfMissing False lmdbDirName
      env <- LMDB.openEnvironment
               lmdbDirName
               LMDB.defaultLimits {
                   LMDB.mapSize      = 6 * 1024 * 1024 * 1024 -- 6 gigabytes, for now
                 , LMDB.maxDatabases = 2
                 }
      dbSeqNo <- LMDB.readWriteTransaction env $ LMDB.getDatabase (Just "SeqNo")
      dbUTxO  <- LMDB.readWriteTransaction env $ LMDB.getDatabase (Just "UTxO")
      LMDB.readWriteTransaction env $ LMDB.get dbSeqNo () >>= \case
        Just seqNo -> throw $ LmdbNotEmptyAtInit seqNo
        Nothing    -> do
          LMDB.put dbSeqNo () (Just 0)
          flip mapM_ genesisUTxO $ \txin -> LMDB.put dbUTxO (LmdbBox txin) (Just (LmdbBox (TxOut 86)))
      _n <- mapM_ (each (LmdbSimEnv env dbSeqNo dbUTxO) mono0 t0) rows
      pure ()
  where
    each :: LmdbSimEnv -> Word64 -> Time.UTCTime -> Row -> IO ()
    each (LmdbSimEnv env dbSeqNo dbUTxO) mono0 _t0 row = do
      mono <- getMonotonicTimeNSec
--      t    <- Time.getCurrentTime
      let diffmono_microseconds a b =
            (a - b) `div` 1_000 --- nano to micro
{-
          difftime_microseconds a b =
            fromEnum (Time.nominalDiffTimeToSeconds (Time.diffUTCTime a b))
               `div` 1_000_000   -- pico to micro
-}

      let blkConsumed = rcConsumedOther (rCache row)
          blkCreated  = rcCreatedOther  (rCache row)

      sizeRead <- if Set.null blkConsumed then pure 0 else LMDB.readOnlyTransaction env $ do
        seqNo <- LMDB.get dbSeqNo () >>= \case
          Nothing    -> throw $ LmdbMissingSeqNo (rBlockNumber row)
          Just seqNo -> pure seqNo

        fmap sum $ flip mapM (Set.toList blkConsumed) $ \txin -> do
          LMDB.get dbUTxO (LmdbBox txin) >>= \case
            Nothing                     -> throw $ LmdbMissingTxIn (rBlockNumber row) seqNo txin
            Just (LmdbBox (TxOut size)) -> pure size

      monoRead <- getMonotonicTimeNSec
--      tRead    <- Time.getCurrentTime

      -- separate write transaction, to simulate actual patterns (TODO
      -- pipeline)
      LMDB.readWriteTransaction env $ do
        LMDB.put dbSeqNo () $ Just $ 1 + rSlotNumber row

        flip mapM_ blkConsumed $ \txin -> do
          LMDB.put dbUTxO (LmdbBox txin) Nothing
        flip mapM_ (Map.toList blkCreated) $ \(txin, sz) -> do
          LMDB.put dbUTxO (LmdbBox txin) $ Just (LmdbBox (TxOut sz))

      monoWrite <- getMonotonicTimeNSec
--      tWrite    <- Time.getCurrentTime

      putStrLn $   show (rBlockNumber row)
        <> "\t" <> show (rSlotNumber  row)
        <> "\t" <> show (diffmono_microseconds mono mono0)
--        <> "\t" <> show (difftime_microseconds t    t0)
        <> "\t" <> show (Set.size blkConsumed)
        <> "\t" <> show (Map.size blkCreated)
        <> "\t" <> show sizeRead
        <> "\t" <> show (sum blkCreated)
        <> "\t" <> show (diffmono_microseconds monoRead  mono)
--        <> "\t" <> show (difftime_microseconds tRead     t)
        <> "\t" <> show (diffmono_microseconds monoWrite monoRead)
--        <> "\t" <> show (difftime_microseconds tWrite    tRead)

-- TODO confirm that: monotonic does not hide system time (it agreed with
-- getCurrenTime whenever I used both, but I wasn't sleeping the process, eg)

{-

I made a ramfs and will be `xzcat`ing the input file from there.

I started `iostat -y 5`.

I invoked the program at 10:19:15.

Circa 350000

avg-cpu:  %user   %nice %system %iowait  %steal   %idle
           0.43    0.00   19.96    4.98    0.00   74.64

Device             tps    kB_read/s    kB_wrtn/s    kB_read    kB_wrtn
sda           19590.80         0.00     79423.20          0     397116

Circa 6460000

avg-cpu:  %user   %nice %system %iowait  %steal   %idle
           0.65    0.00   26.08    0.70    0.00   72.56

Device             tps    kB_read/s    kB_wrtn/s    kB_read    kB_wrtn
sda           24710.00         0.00     99226.40          0     496132

Circa 6710000

avg-cpu:  %user   %nice %system %iowait  %steal   %idle
           0.33    0.00   26.57    0.44    0.00   72.67

Device             tps    kB_read/s    kB_wrtn/s    kB_read    kB_wrtn
sda           25240.80         0.00    101375.20          0     506876

I wasn't watching, but the logged timestamps suggest it took around 2.5 hours.

-}

{-------------------------------------------------------------------------------
Simulating via RocksDB
-------------------------------------------------------------------------------}

data RocksDbSimEnv = RocksDbSimEnv
  !RocksDB.DB
  !(R.IOGenM R.StdGen)

data RocksDbAppExn =
    RocksDbMissingSeqNo           Word64
  | RocksDbMissingTxIn            Word64 Word64 TxIn
  | RocksDbDeserialiseFailed      Word64 Word64 TxIn S.DeserialiseFailure
  | RocksDbDeserialiseSeqNoFailed Word64 S.DeserialiseFailure
  deriving (Show)

instance Exception RocksDbAppExn

rocksDbDirName :: String
rocksDbDirName = "utxo-rocksdb"

rocksDbConfig :: RocksDB.Config
rocksDbConfig = RocksDB.Config {
    RocksDB.createIfMissing = True
  , RocksDB.errorIfExists   = True
  , RocksDB.paranoidChecks  = False   -- TODO ?
  , RocksDB.maxFiles        = Nothing
  , RocksDB.prefixLength    = Nothing
  , RocksDB.bloomFilter     = False
  }

rocksDbSim :: [Row] -> IO ()
rocksDbSim =
    \rows -> do
      mono0 <- getMonotonicTimeNSec
      prng <- R.newIOGenM (R.mkStdGen 20220119)
      RocksDB.withDB rocksDbDirName rocksDbConfig $ \db -> do
        RocksDB.put db seqNoKey (ser (0 :: Word64))
        flip mapM_ genesisUTxO $ \txin -> do
          bs <- R.uniformByteStringM 86 prng
          RocksDB.put db (ser (LmdbBox txin)) bs
        _n <- mapM_ (each (RocksDbSimEnv db prng) mono0) rows
        pure ()
  where
    ser :: S.Serialise a => a -> BS.ByteString
    ser = LBS.toStrict . S.serialise

    seqNoKey = BS.empty

    each :: RocksDbSimEnv -> Word64 -> Row -> IO ()
    each (RocksDbSimEnv db prng) mono0 row = do
      mono <- getMonotonicTimeNSec
      let diffmono_microseconds a b =
            (a - b) `div` 1_000 --- nano to micro

      let blkConsumed = rcConsumedOther (rCache row)
          blkCreated  = rcCreatedOther  (rCache row)

      sizeRead <- if Set.null blkConsumed then pure 0 else do
        seqNo <- RocksDB.get db seqNoKey >>= \case
          Nothing -> throw $ RocksDbMissingSeqNo (rBlockNumber row)
          Just bs -> case S.deserialiseOrFail (LBS.fromStrict bs) of
            Right seqNo -> pure seqNo
            Left err    -> throw $ RocksDbDeserialiseSeqNoFailed (rBlockNumber row) err

        fmap sum $ flip mapM (Set.toList blkConsumed) $ \txin -> do
          RocksDB.get db (ser (LmdbBox txin)) >>= \case
            Nothing -> throw $ RocksDbMissingTxIn (rBlockNumber row) seqNo txin
            Just bs -> pure (toEnum $ BS.length bs :: Word64)

      monoRead <- getMonotonicTimeNSec

      -- separate write transaction, to simulate actual patterns (TODO
      -- pipeline)
      do
        putsCreated <- flip mapM (Map.toList blkCreated) $ \(txin, sz) -> do
          -- use a random bytestring to defeat the RocksDB compression, so that
          -- it's comparable to the LMDB test
          --
          -- NOTE: this is pessimistic; the actual node use will likely benefit
          -- at least a little from RockDB's compression (maybe not enough to
          -- make it worthwhile, though; we have a lot of hashes!)
          --
          -- With replicated 0s instead of random payloads, the whole thing was
          -- 209M on disk at the end, in 15min (no mem constraint)
          --
          -- With random payloads, the whole thing was 707M on disk at the end,
          -- in ~15min (no mem constraint)
          bs <- R.uniformByteStringM (fromEnum sz) prng
          pure $ RocksDB.Put (ser (LmdbBox txin)) bs
        RocksDB.write db $ concat
          [ [RocksDB.Put seqNoKey $ ser $ 1 + rSlotNumber row]
          , flip map (Set.toList blkConsumed) $ \txin ->
              RocksDB.Del (ser (LmdbBox txin))
          , putsCreated
          ]

      monoWrite <- getMonotonicTimeNSec

      putStrLn $   show (rBlockNumber row)
        <> "\t" <> show (rSlotNumber  row)
        <> "\t" <> show (diffmono_microseconds mono mono0)
        <> "\t" <> show (Set.size blkConsumed)
        <> "\t" <> show (Map.size blkCreated)
        <> "\t" <> show sizeRead
        <> "\t" <> show (sum blkCreated)
        <> "\t" <> show (diffmono_microseconds monoRead  mono)
        <> "\t" <> show (diffmono_microseconds monoWrite monoRead)

rocksDbInspect :: [Row] -> IO ()
rocksDbInspect [row] = do
    RocksDB.withDB
      rocksDbDirName
      rocksDbConfig {
          RocksDB.createIfMissing = False
        , RocksDB.errorIfExists = False
        } $ \db -> do
      seqNo <- RocksDB.get db seqNoKey >>= \case
        Nothing -> throw $ RocksDbMissingSeqNo maxBound
        Just bs -> case S.deserialiseOrFail (LBS.fromStrict bs) of
          Right seqNo -> pure seqNo
          Left err    -> throw $ RocksDbDeserialiseSeqNoFailed maxBound err
      print (seqNo :: Word64)
      let txin = V.head (rConsumed row)
      sz <- RocksDB.get db (ser (LmdbBox txin)) >>= \case
        Nothing -> throw $ RocksDbMissingTxIn (rBlockNumber row) seqNo txin
        Just bs -> case S.deserialiseOrFail (LBS.fromStrict bs) of
          Right (LmdbBox (TxOut size)) -> pure size
          Left err                     -> throw $ RocksDbDeserialiseFailed (rBlockNumber row) seqNo txin err
      print sz
      pure ()
  where
    ser :: S.Serialise a => a -> BS.ByteString
    ser = LBS.toStrict . S.serialise

    seqNoKey = BS.empty
rocksDbInspect _ = fail "only expecting one faked-up input row!"

{-------------------------------------------------------------------------------
TODO more simulations/statistics etc
-------------------------------------------------------------------------------}

-- eg timings for various on-disk back-ends (at least when each block was read
-- and when each block was written, possibly also delays of the operations
-- themselves?)
