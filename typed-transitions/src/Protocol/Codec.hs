-- | Definition of codecs for typed transitions, permitting a representation in
-- a monomorphic type such as @ByteString@.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Protocol.Codec where

-- | Monadic fold over an input stream with the notions of end of stream
-- and end of fold.
--
-- The 'Choice' type allows the fold to express its end, and the
-- 'Response' type forces the fold to deal with the input stream end.
--
-- A decoder is a @Fold concrete m (Either fail t)@.
-- This is a sharper representation compared to 'IDecode' from cborg: if the
-- input is exhausted (`Nothing` is given to the 'IDecode') then the decoder
-- cannot possibly give another `Partial`; it must choose a result.
--
newtype Fold input m r = Fold
  { runFold :: m (Choice input m r)
  }

instance Functor m => Functor (Fold input m) where
  fmap f fold = Fold $ fmap recurse (runFold fold)
    where
    recurse choice = case choice of
      Complete leftover term -> Complete leftover $ fmap f term
      Partial response       -> Partial $ response
        { more = fmap f . more response
        , end  = fmap f (end response)
        }

data Choice input m r where
  Complete :: input -> m r -> Choice input m r
  Partial  :: Response input (Fold input m r) (m r) -> Choice input m r

-- | Morally an additive conjunction: only one of 'more' or 'end' is allowed
-- to be used.
data Response input more end = Response
  { more :: input -> more
    -- ^ If there is more input.
  , end  :: end
    -- ^ If there's no more input.
  }

hoistFold
  :: forall input m n r .
     ( Functor n )
  => (forall x . m x -> n x)
  -> Fold input m r
  -> Fold input n r
hoistFold nat fold = Fold $ fmap recurse (nat (runFold fold))
  where
  recurse :: Choice input m r -> Choice input n r
  recurse choice = case choice of
    Complete leftover term -> Complete leftover $ nat term
    Partial response       -> Partial $ response
      { more = hoistFold nat . more response
      , end  = nat (end response)
      }

newtype Input input m = Input
  { runInput :: m (Maybe (input, Input input m))
  }

drainInput :: Monad m => Input input m -> m [input]
drainInput inp = runInput inp >>= \it -> case it of
  Nothing        -> pure []
  Just (i, inp') -> fmap ((:) i) (drainInput inp')

noInput :: Applicative m => Input input m
noInput = Input $ pure Nothing

singletonInput :: Applicative m => input -> Input input m
singletonInput inp = Input $ pure $ Just $ (inp, noInput)

prependInput :: Applicative m => [input] -> Input input m -> Input input m
prependInput inp tail_ = case inp of
  []      -> tail_
  (i : is) -> Input $ pure (Just (i, prependInput is tail_))

-- | Use an 'Input' to run a 'Fold'.
-- Notice how, no matter what, the output of the fold is given. That's
-- a great thing to have: it eliminates the case in which a decoder gives
-- a partial parse even after the end of the input stream.
--
-- Any _unconsumed_ input is given back. Unconsumed means the monadic thing
-- which defines it was never run. It could still be exhausted.
-- In other words: it's 'Just' whenever the fold finished before the input
-- was exhausted. It's 'Nothing' whenever the input was exhausted before
-- the fold chose to finish ('Complete').
foldOverInput
  :: ( Monad m )
  => Fold input m r
  -> Input input m
  -> m (r, Maybe (Input input m))
foldOverInput fold input = runFold fold >>= \choice -> case choice of
  Complete leftovers it -> flip (,) (Just (prependInput [leftovers] input)) <$> it
  Partial step -> runInput input >>= \it -> case it of
    Nothing -> flip (,) Nothing <$> end step
    Just (i, input') -> foldOverInput (more step i) input'

-- Why bother with all this? Well, it makes using a decoder easier: we can
-- just pass all of the input and we know we'll get a result, no possibility
-- of a partial!

type Decoder fail input m r = Fold [input] m (Either fail r)

feedInput
  :: ( Monad m )
  => Decoder fail input m r
  -> Input [input] m
  -> m (Either fail r, Maybe (Input [input] m))
feedInput = foldOverInput

data Codec m fail concreteTo concreteFrom tr from = Codec
  { encode :: Encoder tr from
                      (EncodedCodec m fail concreteTo concreteFrom tr)
  , decode :: Decoder fail concreteFrom m
                      (DecodedCodec m fail concreteTo concreteFrom tr from)
  }

-- | Type alias just to make the type expressions shorter
type EncodedCodec m fail concreteTo concreteFrom tr =
       Encoded concreteTo (Codec m fail concreteTo concreteFrom tr)

-- | Type alias just to make the type expressions shorter
type DecodedCodec m fail concreteTo concreteFrom tr from =
       Decoded tr from (Codec m fail concreteTo concreteFrom tr)

data Encoded concrete codec to = Encoded
  { representation :: concrete
  , encCodec       :: codec to
  }

data Decoded tr from codec = forall to . Decoded
  { transition :: tr from to
  , decCodec   :: codec to
  }

newtype Encoder tr from encoded = Encoder
  { runEncoder :: forall to . tr from to -> encoded to
  }

hoistCodec
  :: ( Functor n )
  => (forall x . m x -> n x)
  -> Codec m fail cto cfrom tr st
  -> Codec n fail cto cfrom tr st
hoistCodec nat codec = codec
  { encode = hoistEncoder nat (encode codec)
  , decode = hoistDecoder nat (decode codec)
  }

hoistEncoder
  :: ( Functor n )
  => (forall x . m x -> n x)
  -> Encoder tr from (Encoded cto (Codec m fail cto cfrom tr))
  -> Encoder tr from (Encoded cto (Codec n fail cto cfrom tr))
hoistEncoder nat encoder = Encoder $ \tr ->
  let encoded = runEncoder encoder tr
  in  encoded { encCodec = hoistCodec nat (encCodec encoded) }

hoistDecoder
  :: forall m n fail cfrom cto tr from .
     ( Functor n )
  => (forall x . m x -> n x)
  -> Decoder fail cfrom m (Decoded tr from (Codec m fail cto cfrom tr))
  -> Decoder fail cfrom n (Decoded tr from (Codec n fail cto cfrom tr))
hoistDecoder nat = (fmap . fmap) hoistDecoded . hoistFold nat
  where
  -- fmap'd twice because
  --   Decoder fail input m r = Fold input m (Either fail r)
  -- the `Decoded` is inside an `Either fail`.
  hoistDecoded (Decoded it codec) = Decoded it (hoistCodec nat codec)

-- | @'Encoded'@ is functor with respect to the @'cto'@ (concrete encoding)
-- parameter.
--
mapEncoded
  :: Functor m
  => (cto -> cto')
  -> Encoded cto  (Codec m fail cto  cfrom tr) to
  -> Encoded cto' (Codec m fail cto' cfrom tr) to
mapEncoded f Encoded {representation, encCodec} = Encoded
  { representation = f representation
  , encCodec       = mapCodec f encCodec
  }

-- | @'Decoded'@ is functor with respect to the @'coded'@
-- parameter.
--
mapDecoded
  :: (forall to. codec to -> codec' to)
  -> Decoded tr from codec
  -> Decoded tr from codec'
mapDecoded f Decoded {transition, decCodec}= Decoded
  { transition = transition
  , decCodec   = f decCodec
  }

-- | @'Codec'@ is a functor with respoect to the @'cto'@ (concrete encoding)
-- parameter.
--
mapCodec
  :: Functor m
  => (cto -> cto')
  -> Codec m fail cto  cfrom tr from
  -> Codec m fail cto' cfrom tr from
mapCodec f Codec {encode, decode} = Codec
  { encode = mapEncoder f encode
  , decode = mapDecoder f decode
  }

-- | @'Encoder'@ is a functor with respect to the @'cto'@ (concrete encoding)
-- parameter.
--
mapEncoder
  :: Functor m
  => (cto -> cto')
  -> Encoder tr from (Encoded cto  (Codec m fail cto  cfrom tr))
  -> Encoder tr from (Encoded cto' (Codec m fail cto' cfrom tr))
mapEncoder f (Encoder encoder) = Encoder (mapEncoded f . encoder)

-- | @'Decoder'@ is a functor with respect to the @'cto'@ (concrete encoding)
-- parameter.
--
mapDecoder
  :: Functor m
  => (cto -> cto')
  -> Decoder fail cfrom m (Decoded tr from (Codec m fail cto  cfrom tr))
  -> Decoder fail cfrom m (Decoded tr from (Codec m fail cto' cfrom tr))
mapDecoder f = (fmap . fmap) (mapDecoded (mapCodec f))
