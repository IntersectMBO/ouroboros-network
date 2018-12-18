{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Protocol.Codec.Coherent where

import Data.Either (partitionEithers)
import Data.List (intercalate, sortBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Type.Equality

import Test.QuickCheck

import Protocol.Codec
import Protocol.Path

-- | Test the coherence of a 'CodecTree' (use 'codecTreePaths' to get one).
--
-- This test checks 2 properties:
-- 
-- 1. Local coherence of each encoder/decoder. If the decoder fails to invert
--    the encoder, then it's locally incoherent.
-- 2. Global coherence of the codec itself. If any 2 paths disagree on the
--    encoding at the same spot in the path, then the codec is globally
--    incoherent.
--
prop_coherent
  :: (concrete -> concrete -> Bool)
     -- ^ Equality of encoding
  -> (forall from to . tr from to -> String)
     -- ^ Show transition, for counterexample reporting.
  -> (concrete -> String)
     -- ^ Show encoding, for counterexample reporting.
  -> (fail -> String)
     -- ^ Show decoding failure reason.
  -> CodecTree fail concrete tr (a ': b ': rest)
     -- ^ The tree of decodings. Use Test.Protocol.Codec.Coherent.codecTree
     -- to get this.
  -> Property
prop_coherent eq showTr showEncoding showFail tree =
  noLocalIncoherent .&&. noMutuallyIncoherent

  where

  paths = codecTreePaths tree

  (incoherent, coherent) = partitionEithers (classifyLocal <$> NE.toList paths)

  noLocalIncoherent :: Property
  noLocalIncoherent = case incoherent of
    [] -> property True
    _  ->
      let counterExampleStrings =
            fmap (showLocalIncoherence showTr showEncoding showFail) incoherent
          counterExampleString = intercalate "\n" counterExampleStrings
      in  counterexample counterExampleString (property False)

  noMutuallyIncoherent :: Property
  noMutuallyIncoherent = case mutuallyIncoherent of
    [] -> property True
    _  ->
      let counterExampleStrings =
            fmap (showNonAgreement showTr showEncoding) mutuallyIncoherent
          counterExampleString = mconcat
            [ show (length mutuallyIncoherent)
            , " counterexamples. Showing at most 10.\n"
            , "======================================\n"
            , intercalate "\n" (take 10 counterExampleStrings)
            , "\n======================================\n"
            ]
      in  counterexample counterExampleString (property False)

  -- To check for mutual incoherence, the idea is to take all pairs of all
  -- coherent paths in the tree and then check whether they agree on encodings
  -- ('agreement eq'). In fact, we don't need strictly _all_ pairs, since
  -- 'agreement eq' commutes, and 'agreement eq a a = Nothing' if 'eq' really
  -- is equality. So we can just take the unique pairs.

  {-
  uniquePairs :: [t] -> [(t, t)]
  uniquePairs [] = []
  uniquePairs (x : xs) = fmap ((,) x) xs ++ uniquePairs xs

  mutuallyIncoherent = mapMaybe (uncurry (agreement eq)) (uniquePairs coherent)
  -}

  -- But this isn't so great, because if path `a` disagrees at some point
  -- with path `b`, then it will also disagree with every path including `b`,
  -- so we'll get a bunch of duplicate counterexamples, unless `a` and `b`
  -- disagree only at the end of the path.

  -- So what's a better way? Simple option is to just look for the first
  -- mutually incoherent pair and then immediately stop.
  --
  -- When we get a mutual incoherence we want to eliminate all other paths
  -- containing that prefix from the rest of the list.
  -- Take the head of the list, and run it over the tail of the list.
  -- At each step, run `agreement eq x y` where `x` is the original head.
  -- If it's nothing, keep `y` in the output list; otherwise, filter the
  -- remainder to eliminate every path which contains the offending path, and
  -- recurse on what remains.

  -- Combine `t`s into matching pairs (witnessed by `r`), and filter
  -- other `t`s w.r.t. that matching pair.
  pairs :: (r -> t -> Bool) -> (t -> t -> Maybe r) -> [t] -> [r]
  pairs _p _f [] = []
  pairs p  f  (t:ts) =
    -- Run the matcher with `t` against the rest of the list to find
    -- an `[r]`, and also a shorter `[t]`: everything in `ts'` was false
    -- under `p r` for every `r` in `rs`.
    let (rs, ts') = go p (f t) ts
        rs' = pairs p f ts'
    in  rs ++ rs'
    where
    go :: (r -> t -> Bool) -> (t -> Maybe r) -> [t] -> ([r], [t])
    go _p' _f' [] = ([], [])
    go p'  f'  (t' : ts') = case f' t' of
      Nothing -> go p' f' ts'
      Just r  -> let (rs, ts'') = go p' f' (filter (not . p' r) ts') in (r:rs, ts'')

  -- True if the locally-coherent path contains the coherent part of the
  -- mutually-incoherent path: that is, the same "trace" of `Encode | Decode`
  -- from root to end.
  subPath
    :: MutuallyIncoherent concrete tr (a ': b ': rest)
    -> LocallyCoherent concrete tr (a ': b ': rest)
    -> Bool
  subPath m l = subPathOn const m l || subPathOn (flip const) m l

  subPathOn
    :: (Side -> Side -> Side)
       -- ^ Select a side to compare against the locally-coherent part's side.
    -> MutuallyIncoherent concrete tr (a ': b ': rest)
    -> LocallyCoherent concrete tr (a ': b ': rest)
    -> Bool
  subPathOn pick (PathCons m) (PathCons l) = case m of
    MutuallyIncoherentHere _ _ _                 -> True
    MutuallyIncoherentThere _ _ sideA sideB subM -> case l of
      CoherentThere _ side subL ->
        (pick sideA sideB == side) && subPathOn pick subM subL

  -- This is OK, but we really ought to get the shortest one first.
  -- In general, it's not possible to determine that a shorter
  -- MutuallyIncoherent characterizes a longer one.

  mutuallyIncoherent = sortBy compareLength (pairs subPath (agreement eq) coherent)

  compareLength
    :: MutuallyIncoherent concrete tr states
    -> MutuallyIncoherent concrete tr states
    -> Ordering
  compareLength (PathCons left) (PathCons right) = case left of
    MutuallyIncoherentHere _ _ _ -> case right of
      MutuallyIncoherentHere _ _ _ -> EQ
      MutuallyIncoherentThere _ _ _ _ _ -> LT
    MutuallyIncoherentThere _ _ _ _ subLeft -> case right of
      MutuallyIncoherentHere _ _ _ -> GT
      MutuallyIncoherentThere _ _ _ _ subRight ->
        compareLength subLeft subRight
  compareLength PathNil PathNil      = EQ


data Good concrete tr from to = Good
  { goodTransition :: tr from to
  , goodEncoding   :: concrete
  }

data Bad fail concrete tr from to = forall to' . Bad
  { badTransition :: tr from to
    -- ^ This transition ...
  , badEncoding   :: concrete
    -- ^ ... encodes to this ...
  , badDecoding   :: Either fail (tr from to')
    -- ^ ... but decodes to this, or 'Left' if the decoder failed.
  }

data Coherence fail concrete tr k states where
  Coherent
    :: Good concrete tr a b
    -> k -- Encoder path
    -> k -- Decoder path
    -> Coherence fail concrete tr k (a ': b ': rest)
  Incoherent
    :: Bad fail concrete tr a b
    -> Coherence fail concrete tr k (a ': b ': rest)

-- | The rather general 'Path' type here is used to define a tree where all
-- children of a node are constrained by the terminal vertex type.
type CodecTree fail concrete tr = Path (Coherence fail concrete tr)

-- | Useful for generating arbitrary paths: we want
--     Gen (exists b rest . TransitionPath tr (a ': b ': rest))
-- which is expressed by
--     Gen (SomeTransitionPath tr a)
data SomeTransitionPath tr begin where
  SomeTransitionPath :: TransitionPath tr (begin ': next ': rest) -> SomeTransitionPath tr begin

showSomeTransitionPath
  :: forall tr st. 
     (forall from to. tr from to -> String)
  -> SomeTransitionPath tr st
  -> String
showSomeTransitionPath showTransition (SomeTransitionPath path) = foldPath fn "" path
 where
  fn :: (k -> String)
      -> Transition tr k states
      -> String
  fn g (Transition tr next) = showTransition tr ++ " : " ++ g next

codecTree
  :: forall m tr concreteEnc concreteDec a b states fail .
     ( Monad m )
  => (forall x . concreteEnc -> Decoder fail concreteDec m x -> m (Either fail x))
     -- ^ How to decode from a concrete encoded type.
     -- Useful that this is abstract. In the case of a CBOR decoder, the encoded
     -- thing can be sliced up as you like, perhaps even at random (put the
     -- decoder into IO).
  -> (forall from to to' . tr from to -> tr from to' -> Maybe (to :~: to'))
     -- ^ Equality test on transitions at a given initial state, which also
     -- produces the equality of the terminal states. These are not equivalent
     -- notions: two different transitions can have the same initial and
     -- terminal states.
  -> Codec m fail concreteEnc concreteDec tr a
  -> TransitionPath tr (a ': b ': states)
  -> m (CodecTree fail concreteEnc tr (a ': b ': states))
codecTree unencode match codec (PathCons (Transition tr next)) = do
  let Encoded concrete codecE = runEncoder (encode codec) tr
  result <- unencode concrete (decode codec)
  case result of
    Left fail_ -> pure $ PathCons (Incoherent (Bad tr concrete (Left fail_)))
    Right (Decoded tr' codecD) -> case match tr tr' of
      Nothing -> pure $ PathCons (Incoherent (Bad tr concrete (Right tr')))
      Just Refl -> case next of
        PathNil -> pure $ PathCons (Coherent (Good tr concrete) PathNil PathNil)
        -- After matching on PathCons, we know that the type of next
        -- has, in its final parameter is of the form (a ': b ': states), so
        -- we can recurse.
        PathCons _ -> do
          byEncoder <- codecTree unencode match codecE next
          byDecoder <- codecTree unencode match codecD next
          pure $ PathCons (Coherent (Good tr concrete) byEncoder byDecoder)

data Side where
  Encode :: Side
  Decode :: Side

deriving instance Eq Side
deriving instance Show Side

-- | Defined for use in the 'CodecPath' type.
data CodecResult fail concrete tr k states where
  -- | A good result, with more following results. This one includes a
  -- 'Side' so we know that the next results are relevant to either the
  -- encoder or decoder path.
  GoodResult :: Good concrete tr a b
             -> Side
             -> k
             -> CodecResult fail concrete tr k (a ': b ': c ': rest)
  -- | A bad result. Immediately ends the path (no use of `k`).
  BadResult  :: Bad fail concrete tr a b
             -> CodecResult fail concrete tr k (a ': b ': rest)
  -- | A final good result. No 'Side' is included.
  AllGood    :: Good concrete tr a b
             -> CodecResult fail concrete tr k '[a, b]

-- | The definition of 'Result' is tailored for this type: for n+2 states,
-- it's either n+1 good results, with n 'Side's (one between each). Or
-- it's m < n+1 good results, terminating in a bad result.
type CodecPath fail concrete tr = Path (CodecResult fail concrete tr)

-- | All paths in the codec tree. All paths are at most the length of the
-- path used to construct the codec tree by 'codecTree'. Some paths are
-- shorter just in case they terminate in 'Incoherent'.
--
-- Beware: there are as many as 2^(n-1) paths in a binary tree of height n.
codecTreePaths
  :: CodecTree fail concrete tr states
  -> NonEmpty (CodecPath fail concrete tr states)
codecTreePaths path = case path of
  PathNil -> PathNil NE.:| []
  PathCons cons -> case cons of
    Incoherent bad -> PathCons (BadResult bad) NE.:| []
    Coherent good PathNil PathNil -> PathCons (AllGood good) NE.:| []
    Coherent good byEncoder@(PathCons _) byDecoder@(PathCons _) ->
      let encoderPaths = codecTreePaths byEncoder
          decoderPaths = codecTreePaths byDecoder
          prependGood tag path' = PathCons (GoodResult good tag path')
      in  (prependGood Encode <$> encoderPaths) <> (prependGood Decode <$> decoderPaths)

-- What we'll do is partition the list of CodecPaths into those which are all
-- 'GoodResult', and those which end in a 'BadResult'.

-- A local incoherence is a path where every cons is a transition, between
-- every 2 cons's is a Tag, and at the end is a 'Bad'.

data LocalIncoherence fail concrete tr k states where
  -- | Prefix of a path that ends in an incoherence. The choice of
  -- a ': b ': c ': rest for the final parameter ensures that this is never
  -- the last in the path.
  IncoherentThere :: Good concrete tr a b
                  -> Side
                  -> k
                  -> LocalIncoherence fail concrete tr k (a ': b ': c ': rest)
  -- | Final spot in a path that ends in an incoherence.
  IncoherentHere  :: Bad fail concrete tr from to
                  -> LocalIncoherence fail concrete tr k (a ': b ': rest)

-- The issue with this type is that there's no guarantee that a 'Bad' is in
-- there. It could just be a bunch of 'IncoherentThere' constructors.
type LocallyIncoherent fail concrete tr = Path (LocalIncoherence fail concrete tr)

data LocalCoherence concrete tr k states where
  -- | A 'Side' is included only if there's more to come, ensured by the
  -- 3 cons's in the final type parameter.
  CoherentThere :: Good concrete tr a b
                -> Side
                -> k
                -> LocalCoherence concrete tr k (a ': b ': c ': rest)
  -- | The end of a locally-coherent path. No 'Side' is included.
  CoherentHere :: Good concrete tr a b
               -> LocalCoherence concrete tr k '[a, b]

type LocallyCoherent concrete tr = Path (LocalCoherence concrete tr)

-- | Classify a 'CodecPath' according to local coherence.
classifyLocal
  :: CodecPath fail concrete tr (a ': b ': rest)
  -> Either (LocallyIncoherent fail concrete tr (a ': b ': rest))
            (LocallyCoherent concrete tr (a ': b ': rest))
classifyLocal (PathCons cons) = case cons of
  BadResult bad -> Left $ PathCons (IncoherentHere bad)
  AllGood good -> Right $ PathCons (CoherentHere good)
  GoodResult good side sub@(PathCons _) -> case classifyLocal sub of
    Left  incoh -> Left  $ PathCons (IncoherentThere good side incoh)
    Right coh   -> Right $ PathCons (CoherentThere good side coh)

showLocalIncoherence
  :: forall fail concrete tr states .
     (forall from to . tr from to -> String)
  -> (concrete -> String)
  -> (fail -> String)
  -> LocallyIncoherent fail concrete tr states
  -> String
showLocalIncoherence showTr _showEncoding showFail = foldPath showOne mempty
  where
  showOne :: (forall k states' . (k -> String) -> LocalIncoherence fail concrete tr k states' -> String)
  showOne recurse it = case it of
    IncoherentThere good side k -> mconcat
      [ showTr (goodTransition good)
      , " : "
      , show side
      , " : "
      , recurse k
      ]
    IncoherentHere bad -> mconcat
      [ "incoherent codec at "
      , showTr (badTransition bad)
      , " : "
        -- Can't pattern match on (badDecoding bad) because of the existential
        -- type variable.
      , case bad of
          Bad _ _ (Left fail_) -> mconcat
            [ "failed to decode with reason "
            , showFail fail_
            ]
          Bad _ _ (Right tr) -> mconcat
            [ "decoded a different transition "
            , showTr tr
            ]
      ]


data MutualIncoherence concrete tr k states where
  MutuallyIncoherentThere
    :: tr a b
    -> concrete
    -> Side
    -> Side
    -> k
    -> MutualIncoherence concrete tr k (a ': b ': c ': rest)
  MutuallyIncoherentHere
    :: tr a b
    -> concrete
    -> concrete
    -> MutualIncoherence concrete tr k (a ': b ': rest)

-- | Stops at the first mutual incoherence.
--
type MutuallyIncoherent concrete tr = Path (MutualIncoherence concrete tr)

-- | Determine whether 2 locally-coherent paths agree on corresponding
-- concrete representations.
agreement
  :: (concrete -> concrete -> Bool)
  -> LocallyCoherent concrete tr (a ': b ': rest)
  -> LocallyCoherent concrete tr (a ': b ': rest)
  -> Maybe (MutuallyIncoherent concrete tr (a ': b ': rest))
agreement eq (PathCons left) (PathCons right) = case (left, right) of
  (CoherentHere goodL, CoherentHere goodR) ->
    if encodingL `eq` encodingR
    then Nothing
    else Just (PathCons (MutuallyIncoherentHere tr encodingL encodingR))
    where
    encodingL = goodEncoding goodL
    encodingR = goodEncoding goodR
    tr = goodTransition goodL -- = goodTransition goodR by assumption.
  (CoherentThere goodL sideL subL, CoherentThere goodR sideR subR) ->
    if encodingL `eq` encodingR
    then (\path -> PathCons (MutuallyIncoherentThere tr encodingL sideL sideR path)) <$> sub
    else Just (PathCons (MutuallyIncoherentHere tr encodingL encodingR))
    where
    sub = agreement eq subL subR
    encodingL = goodEncoding goodL
    encodingR = goodEncoding goodR
    tr = goodTransition goodL

showNonAgreement
  :: forall concrete tr states .
     (forall from to . tr from to -> String)
  -> (concrete -> String)
  -> MutuallyIncoherent concrete tr states
  -> String
showNonAgreement showTr showEncoding = foldPath showOne mempty
  where
  showOne :: (forall k states' . (k -> String) -> MutualIncoherence concrete tr k states' -> String)
  showOne recurse it = case it of
    MutuallyIncoherentThere tr _ sideA sideB k -> mconcat
      [ showTr tr
      , " : "
      , show (sideA, sideB)
      , " : "
      , recurse k
      ]
    MutuallyIncoherentHere tr encodingA encodingB -> mconcat
      [ "encoding disagreement at "
      , showTr tr
      , " : ("
      , showEncoding encodingA
      , ") /= ("
      , showEncoding encodingB
      , ")"
      ]
