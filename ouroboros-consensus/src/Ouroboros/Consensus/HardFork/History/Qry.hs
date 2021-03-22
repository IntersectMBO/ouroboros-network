{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.HardFork.History.Qry (
    -- * Qry
    Expr (..)
  , PastHorizonException (..)
  , qryFromExpr
  , runQuery
  , runQueryPure
  , runQueryThrow
    -- ** opaque
  , Qry
    -- * Interpreter
  , interpretQuery
  , mkInterpreter
  , unsafeExtendSafeZone
    -- ** opaque
  , Interpreter
    -- * Specific queries
  , epochToSize
  , epochToSlot
  , epochToSlot'
  , slotToEpoch
  , slotToEpoch'
  , slotToWallclock
  , wallclockToSlot
  ) where

import           Codec.Serialise (Serialise (..))
import           Control.Exception (throw)
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Fixed (divMod')
import           Data.Foldable (toList)
import           Data.Functor.Identity
import           Data.Kind (Type)
import           Data.SOP.Strict (SListI)
import           Data.Time hiding (UTCTime)
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Show (showSpace)
import           GHC.Stack
import           Quiet

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Util (Some (..))
import           Ouroboros.Consensus.Util.Counting (NonEmpty (..))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.HardFork.History.EraParams
import           Ouroboros.Consensus.HardFork.History.Summary
import           Ouroboros.Consensus.HardFork.History.Util

{-------------------------------------------------------------------------------
  Internal: reified queries

  NOTE. The lower bound of every era is inclusive, while the upper bound is
  really exclusive, making the upper bound of every era equal to the lower
  bound of the next.

  >         era A         era B         era C
  >        [.....) [...............) [..........)
  > epoch         e                 e'
  > slot          s                 s'
  > time          t                 t'

  Now let's consider what happens when we do translations of the values at
  the boundary.

   1. Slot-to-epoch translation. Using era C, we get

      > e' + ((s' - s') / epochSizeC) == e'

      Using era B (technically the wrong era to be using, since the upper bound
      is exclusive), we get

      > e + ((s' - s) / epochSizeB)

      These are equal by (INV-1a).

   2. Epoch-to-slot translation. Using era C, we get

      > s' + ((e' - e') * epochSizeC) == s'

      Using era B, we'd get

      > s + ((e' - e) * epochSizeB

      These are equal by (INV-1b).

   3. Slot to time translation. Using era C, we get

      > t' + ((s' - s') * slotLenC) == t'

      Using era C, we get

      > t + ((s' - s) * slotLenB)

      These are equal by (INV-2b)

   4. Time to slot translation. Using era C, we get

      > s' + ((t' - t') / slotLenC) == s'

      Using era B, we get

      > s + ((t' - t) / slotLenB)

      These are equal by (INV-2a).

  This means that for values at that boundary, it does not matter if we use
  this era or the next era for the translation. However, this is only true for
  these 4 translations. If we are returning the era parameters directly, then
  of course we can't use the era parameters from the wrong era.

  There is however a benefit to using the current era: there might not /be/
  a next era, and so if we use the current era, we extend the period for which
  we do calculations just that tiny bit more. This might be important for
  ledger implementations. For example, suppose we want to know if a particular
  slot @s@ is far enough away from the next epoch boundary (e.g., to determine
  if an update proposal should take effect in this epoch or the next). One
  natural way to write this would be to translate @s@ to the corresponding
  epoch @e@, then translate @e + 1@ back to a slot @s'@, and check the
  distance @s' - s@. However, it is conceivable that the safe zone stops at
  that epoch boundary; if it does, this computation would result in a
  'PastHorizonException', even if a different way to write the same computation
  (translating @s + delta@ to an epoch number, and then comparing that to @e@)
  might succeed. Rather than imposing an unnecessary limitation on the ledger,
  we therefore treat the upper bound as inclusive, so that both ways to do the
  check would succeed.
-------------------------------------------------------------------------------}

-- | Query
--
-- 'Qry' adds a monadic interface on top of 'Expr'. Although means that 'Qry'
-- itself is not showable, the 'PastHorizonException' can nonetheless show the
-- offending expression alongside the 'Summary' against which it was evaluated.
data Qry :: Type -> Type where
  QPure :: a -> Qry a
  QExpr :: ClosedExpr a -> (a -> Qry b) -> Qry b

instance Functor Qry where
  fmap = liftM

instance Applicative Qry where
  pure  = QPure
  (<*>) = ap

instance Monad Qry where
  return = pure
  QPure a   >>= k = k a
  QExpr e f >>= k = QExpr e (f >=> k)

-- | Construct a 'Qry' from a closed 'Expr'
qryFromExpr :: (forall f. Expr f a) -> Qry a
qryFromExpr e = QExpr (ClosedExpr e) QPure

{-------------------------------------------------------------------------------
  Clarifying newtypes
-------------------------------------------------------------------------------}

newtype TimeInEra   = TimeInEra   { getTimeInEra   :: NominalDiffTime } deriving (Generic)
newtype TimeInSlot  = TimeInSlot  { getTimeInSlot  :: NominalDiffTime } deriving (Generic)
newtype SlotInEra   = SlotInEra   { getSlotInEra   :: Word64 }          deriving (Generic)
newtype SlotInEpoch = SlotInEpoch { getSlotInEpoch :: Word64 }          deriving (Generic)
newtype EpochInEra  = EpochInEra  { getEpochInEra  :: Word64 }          deriving (Generic)

deriving via Quiet TimeInEra   instance Show TimeInEra
deriving via Quiet TimeInSlot  instance Show TimeInSlot
deriving via Quiet SlotInEra   instance Show SlotInEra
deriving via Quiet SlotInEpoch instance Show SlotInEpoch
deriving via Quiet EpochInEra  instance Show EpochInEra

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

data ClosedExpr a = ClosedExpr (forall f. Expr f a)

-- | Query expressions in PHOAS
data Expr (f :: Type -> Type) :: Type -> Type where
  -- PHOAS infrastructure

  EVar  :: f a -> Expr f a
  ELit  :: Show a => a -> Expr f a
  ELet  :: Expr f a -> (f a -> Expr f b) -> Expr f b

  -- Support for pairs makes expressions more easily composable

  EPair :: Expr f a -> Expr f b -> Expr f (a, b)
  EFst  :: Expr f (a, b) -> Expr f a
  ESnd  :: Expr f (a, b) -> Expr f b

  -- Convert from absolute to era-relative

  EAbsToRelTime  :: Expr f RelativeTime -> Expr f TimeInEra
  EAbsToRelSlot  :: Expr f SlotNo       -> Expr f SlotInEra
  EAbsToRelEpoch :: Expr f EpochNo      -> Expr f EpochInEra

  -- Convert from era-relative to absolute

  ERelToAbsTime  :: Expr f TimeInEra                 -> Expr f RelativeTime
  ERelToAbsSlot  :: Expr f (SlotInEra, TimeInSlot)   -> Expr f SlotNo
  ERelToAbsEpoch :: Expr f (EpochInEra, SlotInEpoch) -> Expr f EpochNo

  -- Convert between relative values

  ERelTimeToSlot  :: Expr f TimeInEra  -> Expr f (SlotInEra, TimeInSlot)
  ERelSlotToTime  :: Expr f SlotInEra  -> Expr f TimeInEra
  ERelSlotToEpoch :: Expr f SlotInEra  -> Expr f (EpochInEra, SlotInEpoch)
  ERelEpochToSlot :: Expr f EpochInEra -> Expr f SlotInEra

  -- Get era parameters
  -- The arguments are used for bound checks

  ESlotLength :: Expr f SlotNo  -> Expr f SlotLength
  EEpochSize  :: Expr f EpochNo -> Expr f EpochSize

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

evalExprInEra :: EraSummary -> ClosedExpr a -> Maybe a
evalExprInEra EraSummary{..} = \(ClosedExpr e) -> go e
  where
    EraParams{..} = eraParams
    slotLen   = getSlotLength eraSlotLength
    epochSize = unEpochSize   eraEpochSize

    guardEnd :: (Bound -> Bool) -> Maybe ()
    guardEnd p =
        case eraEnd of
          EraUnbounded -> return ()
          EraEnd b     -> guard $ p b

    go :: Expr Identity a -> Maybe a
    go (EVar a) =
        return $ runIdentity a
    go (ELet e f) =
        go e >>= go . f . Identity

    -- Literals and pairs
    go (ELit i) =
        return i
    go (EPair e e') = do
        x <- go e
        y <- go e'
        return (x, y)
    go (EFst e) =
       fst <$> go e
    go (ESnd e) =
       snd <$> go e

    -- Convert absolute to relative
    --
    -- The guards here justify the subtractions.

    go (EAbsToRelTime expr) = do
        t <- go expr
        guard (t >= boundTime eraStart)
        return $ TimeInEra (t `diffRelTime` boundTime eraStart)
    go (EAbsToRelSlot expr) = do
        s <- go expr
        guard (s >= boundSlot eraStart)
        return $ SlotInEra (countSlots s (boundSlot eraStart))
    go (EAbsToRelEpoch expr) = do
        e <- go expr
        guard (e >= boundEpoch eraStart)
        return $ EpochInEra (countEpochs e (boundEpoch eraStart))

    -- Convert relative to absolute
    --
    -- As justified by the proof above, the guards treat the upper bound
    -- as inclusive.

    go (ERelToAbsTime expr) = do
        t <- go expr
        let absTime = getTimeInEra t `addRelTime` boundTime eraStart
        guardEnd $ \end -> absTime <= boundTime end
        return absTime
    go (ERelToAbsSlot expr) = do
        (s, t) <- go expr
        let absSlot = addSlots (getSlotInEra s) (boundSlot eraStart)
        guardEnd $ \end -> absSlot <  boundSlot end
                        || absSlot == boundSlot end && getTimeInSlot t == 0
        return absSlot
    go (ERelToAbsEpoch expr) = do
        (e, s) <- go expr
        let absEpoch = addEpochs (getEpochInEra e) (boundEpoch eraStart)
        guardEnd $ \end -> absEpoch <  boundEpoch end
                        || absEpoch == boundEpoch end && getSlotInEpoch s == 0
        return absEpoch

    -- Convert between relative values
    --
    -- No guards necessary

    go (ERelTimeToSlot expr) = do
        t <- go expr
        return $ bimap SlotInEra TimeInSlot (getTimeInEra t `divMod'` slotLen)
    go (ERelSlotToTime expr) = do
        s <- go expr
        return $ TimeInEra (fromIntegral (getSlotInEra s) * slotLen)
    go (ERelSlotToEpoch expr) = do
        s <- go expr
        return $ bimap EpochInEra SlotInEpoch $ getSlotInEra s `divMod` epochSize
    go (ERelEpochToSlot expr) = do
        e <- go expr
        return $ SlotInEra (getEpochInEra e * epochSize)

    -- Get era parameters
    --
    -- Here the upper bound must definitely be exclusive, or we'd return the
    -- era parameters from the wrong era.

    go (ESlotLength expr) = do
        s <- go expr
        guard    $ s >= boundSlot eraStart
        guardEnd $ \end -> s < boundSlot end
        return eraSlotLength
    go (EEpochSize expr) = do
        e <- go expr
        guard    $ e >= boundEpoch eraStart
        guardEnd $ \end -> e < boundEpoch end
        return eraEpochSize

{-------------------------------------------------------------------------------
  PastHorizonException
-------------------------------------------------------------------------------}

-- | We tried to convert something that is past the horizon
--
-- That is, we tried to convert something that is past the point in time
-- beyond which we lack information due to uncertainty about the next
-- hard fork.
data PastHorizonException = PastHorizon {
      -- | Callstack to the call to 'runQuery'
      pastHorizonCallStack  :: CallStack

      -- | The 'Expr' we tried to evaluate
    , pastHorizonExpression :: Some ClosedExpr

      -- | The 'EraSummary's that we tried to evaluate the 'Expr' against
    , pastHorizonSummary    :: [EraSummary]
    }

deriving instance Show PastHorizonException
instance Exception PastHorizonException

{-------------------------------------------------------------------------------
  Running queries
-------------------------------------------------------------------------------}

-- | Run a query
--
-- Unlike an 'Expr', which is evaluated in a single era, a 'Qry' is evaluated
-- against /all/ eras. Only if all 'Expr's embedded in the 'Qry' can be
-- evaluated in the /same/ era (we don't want to mix properties of different
-- eras in one query) do we return the result. If there is no era in which we
-- can evaluate all 'Expr's in the 'Qry', we report a 'PastHorizonException'.
--
-- NOTE: this means that queries about separate eras have to be run separately,
-- they should not be composed into a single query. How could we know to which
-- era which relative slot/time refers?
runQuery ::
     forall a xs. HasCallStack
  => Qry a -> Summary xs -> Either PastHorizonException a
runQuery qry (Summary summary) = go summary
  where
    go :: NonEmpty xs' EraSummary -> Either PastHorizonException a
    go (NonEmptyOne era)       = tryEra era qry
    go (NonEmptyCons era eras) = case tryEra era qry of
        Left  _ -> go eras
        Right x -> Right x

    tryEra :: forall b. EraSummary -> Qry b -> Either PastHorizonException b
    tryEra era = \case
        QPure x   -> Right x
        QExpr e k ->
          case evalExprInEra era e of
            Just x  ->
              tryEra era (k x)
            Nothing ->
              Left $ PastHorizon callStack (Some e) (toList summary)

runQueryThrow :: (HasCallStack, MonadThrow m )=> Qry a -> Summary xs -> m a
runQueryThrow q = either throwIO return . runQuery q

runQueryPure :: HasCallStack => Qry a -> Summary xs -> a
runQueryPure q = either throw id . runQuery q

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

-- | Can be sent across the LocalStateQuery protocol to interpret queries in
-- the wallet.
--
-- The 'Summary' should be considered internal.
newtype Interpreter xs = Interpreter (Summary xs)
  deriving (Eq)

deriving instance SListI xs => Serialise (Interpreter xs)

instance Show (Interpreter xs) where
  show _ = "<Interpreter>"

mkInterpreter :: Summary xs -> Interpreter xs
mkInterpreter = Interpreter

interpretQuery ::
     HasCallStack
  => Interpreter xs
  -> Qry a
  -> Either PastHorizonException a
interpretQuery (Interpreter summary) qry = runQuery qry summary

-- | UNSAFE: extend the safe zone of the current era of the given 'Interpreter'
-- to be /unbounded/, ignoring any future hard forks.
--
-- This only has effect when the 'Interpreter' was obtained in an era that was
-- /not the final one/ (in the final era, this is a no-op). The 'Interpreter'
-- will be made to believe that the current era is the final era, making its
-- horizon unbounded, and thus never returning a 'PastHorizonException'.
--
-- Use of this function is /strongly discouraged/, as it will ignore any future
-- hard forks, and the results produced by the 'Interpreter' can thus be
-- incorrect.
unsafeExtendSafeZone :: Interpreter xs -> Interpreter xs
unsafeExtendSafeZone (Interpreter (Summary eraSummaries)) =
    Interpreter (Summary (go eraSummaries))
  where
    go :: NonEmpty xs' EraSummary -> NonEmpty xs' EraSummary
    go (NonEmptyCons e es) = NonEmptyCons e (go es)
    go (NonEmptyOne  e)    = NonEmptyOne  e { eraEnd = EraUnbounded }

{-------------------------------------------------------------------------------
  Specific queries

  The primed forms are the ones used in the 'EpochInfo' construction.
  Critically, they do not ask for any of the era parameters. This means that
  their valid range /includes/ the end bound.

  All of these queries are constructed so that they contain a single 'Expr'
  (which is therefore evaluated against a single era).
-------------------------------------------------------------------------------}

-- | Translate 'UTCTime' to 'SlotNo'
--
-- Additionally returns the time spent and time left in this slot.
wallclockToSlot :: RelativeTime -> Qry (SlotNo, NominalDiffTime, NominalDiffTime)
wallclockToSlot absTime =
    aux <$> qryFromExpr (wallclockToSlotExpr absTime)
  where
    aux :: (TimeInSlot, (SlotNo, SlotLength))
        -> (SlotNo, NominalDiffTime, NominalDiffTime)
    aux (TimeInSlot timeInSlot, (absSlot, slotLen)) = (
          absSlot
        , timeInSlot
        , getSlotLength slotLen - timeInSlot
        )

-- | Translate 'SlotNo' to the 'UTCTime' at the start of that slot
--
-- Additionally returns the length of the slot.
slotToWallclock :: SlotNo -> Qry (RelativeTime, SlotLength)
slotToWallclock absSlot =
    qryFromExpr (slotToWallclockExpr absSlot)

-- | Convert 'SlotNo' to 'EpochNo' and the relative slot within the epoch
slotToEpoch' :: SlotNo -> Qry (EpochNo, Word64)
slotToEpoch' absSlot =
    second getSlotInEpoch <$> qryFromExpr (slotToEpochExpr' absSlot)

-- | Translate 'SlotNo' to its corresponding 'EpochNo'
--
-- Additionally returns the relative slot within this epoch and how many
-- slots are left in this slot.
slotToEpoch :: SlotNo -> Qry (EpochNo, Word64, Word64)
slotToEpoch absSlot =
    aux <$> qryFromExpr (slotToEpochExpr absSlot)
  where
    aux :: ((EpochNo, SlotInEpoch), EpochSize)
        -> (EpochNo, Word64, Word64)
    aux ((absEpoch, SlotInEpoch slotInEpoch), epochSize) = (
          absEpoch
        , slotInEpoch
        , unEpochSize epochSize - slotInEpoch
        )

epochToSlot' :: EpochNo -> Qry SlotNo
epochToSlot' absEpoch =
    qryFromExpr (epochToSlotExpr' absEpoch)

-- | Translate 'EpochNo' to the 'SlotNo' of the first slot in that epoch
--
-- Additionally returns the size of the epoch.
epochToSlot :: EpochNo -> Qry (SlotNo, EpochSize)
epochToSlot absEpoch =
    qryFromExpr (epochToSlotExpr absEpoch)

epochToSize :: EpochNo -> Qry EpochSize
epochToSize absEpoch =
    qryFromExpr (epochToSizeExpr absEpoch)

{-------------------------------------------------------------------------------
  Supporting expressions for the queries above
-------------------------------------------------------------------------------}

wallclockToSlotExpr :: RelativeTime -> Expr f (TimeInSlot, (SlotNo, SlotLength))
wallclockToSlotExpr absTime =
    ELet (ERelTimeToSlot (EAbsToRelTime (ELit absTime))) $ \relSlot ->
    ELet (ERelToAbsSlot (EVar relSlot)) $ \absSlot ->
    EPair (ESnd (EVar relSlot))
          (EPair (EVar absSlot) (ESlotLength (EVar absSlot)))

slotToWallclockExpr :: SlotNo -> Expr f (RelativeTime, SlotLength)
slotToWallclockExpr absSlot =
    EPair
      (ERelToAbsTime (ERelSlotToTime (EAbsToRelSlot (ELit absSlot))))
      (ESlotLength (ELit absSlot))

slotToEpochExpr' :: SlotNo -> Expr f (EpochNo, SlotInEpoch)
slotToEpochExpr' absSlot =
    ELet (ERelSlotToEpoch (EAbsToRelSlot (ELit absSlot))) $ \epochSlot ->
    EPair (ERelToAbsEpoch (EVar epochSlot)) (ESnd (EVar epochSlot))

slotToEpochExpr ::
     SlotNo
  -> Expr f ((EpochNo, SlotInEpoch), EpochSize)
slotToEpochExpr absSlot =
    ELet (slotToEpochExpr' absSlot) $ \x ->
    EPair (EVar x) (EEpochSize (EFst (EVar x)))

epochToSlotExpr' :: EpochNo -> Expr f SlotNo
epochToSlotExpr' absEpoch =
    ERelToAbsSlot (EPair (ERelEpochToSlot (EAbsToRelEpoch (ELit absEpoch)))
                         (ELit (TimeInSlot 0)))

epochToSlotExpr :: EpochNo -> Expr f (SlotNo, EpochSize)
epochToSlotExpr absEpoch =
    EPair (epochToSlotExpr' absEpoch) (epochToSizeExpr absEpoch)

epochToSizeExpr :: EpochNo -> Expr f EpochSize
epochToSizeExpr absEpoch =
    EEpochSize (ELit absEpoch)

{-------------------------------------------------------------------------------
  'Show' instances
-------------------------------------------------------------------------------}

newtype Var a = Var String
  deriving (Show)

deriving instance Show (Some ClosedExpr)

instance Show (ClosedExpr a) where
  showsPrec = \d (ClosedExpr e) -> go 0 d e
    where
      go :: Int  -- How many variables are already in scope?
         -> Int  -- Precedence
         -> Expr Var b -> ShowS
      go n d = showParen (d >= 11) . \case

          -- Variables and let-binding
          --
          -- We recover Haskell syntax here, e.g.
          --
          -- > ELet .. (\x -> .... x ....)

          EVar (Var x) -> showString "EVar " . showString x
          ELet e f     -> let x = "x" ++ show n in
                          showString "ELet "
                        . go n 11 e
                        . showString " (\\"
                        . showString x
                        . showString " -> "
                        . go (n + 1) 0 (f (Var x))
                        . showString ")"

          -- Literals

          ELit i -> showString "ELit " . showsPrec 11 i

          -- Pairs

          EPair e e' -> showString "EPair " . go n 11 e . showSpace . go n 11 e'
          EFst  e    -> showString "EFst "  . go n 11 e
          ESnd  e    -> showString "ESnd "  . go n 11 e

          -- Domain specific

          EAbsToRelTime   e -> showString "EAbsToRelTime "   . go n 11 e
          EAbsToRelSlot   e -> showString "EAbsToRelSlot "   . go n 11 e
          EAbsToRelEpoch  e -> showString "EAbsToRelEpoch "  . go n 11 e
          ERelToAbsTime   e -> showString "ERelToAbsTime "   . go n 11 e
          ERelToAbsSlot   e -> showString "ERelToAbsSlot "   . go n 11 e
          ERelToAbsEpoch  e -> showString "ERelToAbsEpoch "  . go n 11 e
          ERelTimeToSlot  e -> showString "ERelTimeToSlot "  . go n 11 e
          ERelSlotToTime  e -> showString "ERelSlotToTime "  . go n 11 e
          ERelSlotToEpoch e -> showString "ERelSlotToEpoch " . go n 11 e
          ERelEpochToSlot e -> showString "ERelEpochToSlot " . go n 11 e
          ESlotLength     e -> showString "ESlotLength "     . go n 11 e
          EEpochSize      e -> showString "EEpochSize "      . go n 11 e
