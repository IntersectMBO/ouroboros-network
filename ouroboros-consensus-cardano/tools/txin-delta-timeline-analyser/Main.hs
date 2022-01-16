{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import qualified Control.Monad as M
import           Data.Bits (shiftL)
import           Data.ByteString.Base64 (decodeBase64)
import           Data.ByteString.Short.Base16 (encodeBase16')
import           Data.ByteString.Short.Base64 (encodeBase64)
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
import qualified Data.Text.Short as TextShort
import           Data.Word (Word32, Word64)
import qualified Data.Vector as V
import           GHC.Clock (getMonotonicTimeNSec)
import qualified Options.Applicative as O

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
      InMemSim   -> pure inMemSim
      MeasureAge -> pure measureAge
  where
    opts = O.info (commandLineParser O.<**> O.helper) $
         O.fullDesc
      <> O.progDesc "Analyse a txin delta timeline file"
      <> O.header "txin-delta-timeline-analyser - a tool for empirical UTxO HD design"

data AnalysisName =
    InMemSim
  | MeasureAge
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
  }

-- | Parse either a @\@@ item or a @#@ item, from the variable length portion
-- of each line
p0Item :: Char -> (ShortByteString -> Word32 -> ans) -> P0 Char8.ByteString ans
p0Item sep f = mapP0 (Char8.split sep) $ contextT ("p0Item " <> [sep]) $ nullToP0 $ do
  h <- popP
  h' <- either (failT . show) pure $ decodeBase64 (Char8.toStrict h)
  i' <- intP
  pure $ f (Short.toShort h') (toEnum i')

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
          (sequenceP0 (p0Item '#' TxOutputIds))
      $ \created -> do
        M.guard $ toEnum numCreated == sum [ n | TxOutputIds _ n <- created ]
        pure Row {
            rBlockNumber = toEnum bn
          , rSlotNumber  = toEnum sn
          , rNumTx       = numTx
          , rConsumed    = toVec numConsumed      consumed
          , rCreated     = toVec (length created) created
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
      let consumed = Set.fromList $                         V.toList $ rConsumed row
          created  = Set.fromList $ concatMap outputTxIns $ V.toList $ rCreated  row
          blkCreated  = Set.difference created consumed
          blkConsumed = Set.difference consumed created

      -- fail if a txin this block consumes is not in the utxo
      do
        let missing = blkConsumed `Set.difference` utxo
            sho1 (TxIn h i) = TextShort.toString (encodeBase64 h) <> "@" <> show i
            -- encodeBase16 is bugged, so I need to convert through hoops to use encodeBase16'
            sho2 (TxIn h i) = Char8.unpack (Char8.fromStrict (Short.fromShort (encodeBase16' h))) <> "@" <> show i
        M.unless (Set.null missing) $ error $ unlines [
            unwords ["ERROR: missing TxIn", show (rBlockNumber row), show (Set.size blkConsumed), show (Set.size missing)]
          , unwords $ map sho1 $ Set.toList missing
          , unwords $ map sho2 $ Set.toList missing
          ]

      let utxo' = Set.union blkCreated $ utxo `Set.difference` blkConsumed

      putStrLn $   show (rBlockNumber row)
        <> "\t" <> show (rSlotNumber  row)
        <> "\t" <> show reltime_milliseconds
        <> "\t" <> show (Set.size blkConsumed)
        <> "\t" <> show (Set.size blkCreated)
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
      let consumed = Set.fromList $                         V.toList $ rConsumed row
          created  = Set.fromList $ concatMap outputTxIns $ V.toList $ rCreated  row
          blkCreated  :: Set TxIn
          blkCreated  = Set.difference created consumed
          blkConsumed :: Set TxIn
          blkConsumed = Set.difference consumed created

      let updCreated :: Map HASH AgeEntry
          updCreated =   -- how many outputs this block creates for its own txs
              fmap (\c -> AgeEntry (rBlockNumber row) c)
            $ (\f -> Map.fromAscListWith (+) $ map f $ Set.toAscList blkCreated)
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
TODO more simulations/statistics etc
-------------------------------------------------------------------------------}

-- eg histogram of how long between creation and consumption of each spent tx
-- output

-- eg timings for various on-disk back-ends (at least when each block was read
-- and when each block was written, possibly also delays of the operations
-- themselves?)
