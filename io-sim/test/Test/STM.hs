{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | A reference implementation of the STM operational semantics.
--
-- It is based on the paper /Composable Memory Transactions/, which gives the
-- operational semantics of STM Haskell in Figures 2--4.
--
-- <https://research.microsoft.com/en-us/um/people/simonpj/papers/stm/stm.pdf>
--
module Test.STM where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Type.Equality
import           Prelude hiding (exp)

import           Control.Monad.Class.MonadSTM as STM
import           Control.Monad.Class.MonadThrow

import           Test.QuickCheck


-- | The type level structure of types in our STM 'Term's. This is kept simple,
-- just unit and ints as base types, and the type of STM variables.
--
data Type where
    TyUnit :: Type
    TyInt  :: Type
    TyVar  :: Type -> Type


-- | A value level representation of the types of STM 'Term's.
--
data TyRep (t :: Type) where
    TyRepUnit ::            TyRep TyUnit
    TyRepInt  ::            TyRep TyInt
    TyRepVar  :: TyRep t -> TyRep (TyVar t)

deriving instance Show (TyRep t)


-- | Figure 2 in the paper gives the syntax of STM terms. It does not
-- distinguish between STM action terms and other terms. We make such a
-- distinction here because it makes the encoding and generation of terms
-- easier, and the restriction is not fundamental for the STM semantics.
--
-- Note that we choose not to implement @catch@ as we do not need it. But it
-- should be straightforward to add if it becomes necessary.
--
data Term (t :: Type) where

    Return    :: Expr t -> Term t
    Throw     :: Expr a -> Term t
    Retry     :: Term t

    ReadTVar  :: Name (TyVar t) -> Term t
    WriteTVar :: Name (TyVar t) -> Expr t -> Term TyUnit
    NewTVar   :: Expr t -> Term (TyVar t)

    -- | This is the ordinary monad bind for STM terms.
    Bind      :: Term a -> Name a -> Term t -> Term t
    OrElse    :: Term t -> Term t -> Term t

deriving instance Show (Term t)


-- | Expressions that can appear within 'Term's.
--
data Expr (t :: Type) where

    ExprUnit ::           Expr TyUnit
    ExprInt  :: Int    -> Expr TyInt
    ExprName :: Name t -> Expr t

deriving instance Show (Expr t)


-- | Normal form values that occur during evaluation.
--
data Value (t :: Type) where

    ValUnit ::          Value TyUnit
    ValInt  :: Int   -> Value TyInt
    ValVar  :: Var t -> Value (TyVar t)

deriving instance Show (Value t)


-- | We have both names and STM variables, and it is important to keep the two
-- concepts distinct. We need names because we have bind, and in particular the
-- same name may end up referring to different variables during execution,
-- depending on runtime conditions.
--
-- The bound variable scheme here is just a simple fresh name supply. The
-- variable bindings are held in the 'Env'.
--
-- The names are typed and carry a representation of their type.
--
data Name (t :: Type) = Name !NameId (TyRep t)
deriving instance Show (Name t)

newtype NameId = NameId Int
  deriving (Eq, Ord, Enum, Show)

-- | An STM variable. The value is held in the 'Heap'. A simple fresh name
-- supply scheme is used.
--
-- The variables are typed and carry a representation of their type.
--
data Var (t :: Type) = Var !VarId (TyRep t)
deriving instance Show (Var t)

newtype VarId = VarId Int
  deriving (Eq, Ord, Enum, Show)



--
-- Type rep utils
--

eqTyRep :: TyRep a -> TyRep b -> Maybe (a :~: b)
eqTyRep  TyRepUnit    TyRepUnit   = Just Refl
eqTyRep  TyRepInt     TyRepInt    = Just Refl
eqTyRep (TyRepVar a) (TyRepVar b) = case eqTyRep a b of
                                      Nothing   -> Nothing
                                      Just Refl -> Just Refl
eqTyRep  _            _           = Nothing

nameTyRep :: Name t -> TyRep t
nameTyRep (Name _ tyrep) = tyrep

varTyRep :: Var t -> TyRep t
varTyRep (Var _ tyrep) = tyrep

tyRepExpr :: Expr t -> TyRep t
tyRepExpr (ExprName n) = nameTyRep n
tyRepExpr  ExprUnit    = TyRepUnit
tyRepExpr (ExprInt _)  = TyRepInt

tyRepValue :: Value t -> TyRep t
tyRepValue  ValUnit   = TyRepUnit
tyRepValue (ValInt _) = TyRepInt
tyRepValue (ValVar v) = TyRepVar (varTyRep v)


--
-- Evaluation environments
--

data SomeName where
     SomeName :: Name t -> SomeName

data SomeValue where
     SomeValue :: Value t -> SomeValue

deriving instance Show SomeName
deriving instance Show SomeValue


-- | The environment is a mapping of 'Name's to their values.
--
newtype Env = Env (Map NameId SomeValue)
  deriving Show

-- | Lookup a name in the environment. This dynamically checks the types.
--
lookupEnv :: Env -> Name t -> Value t
lookupEnv (Env env) (Name name tyrep) =
    fromMaybe (error "lookupEnv: no such var") $ do
    SomeValue v <- Map.lookup name env
    Refl        <- tyrep `eqTyRep` tyRepValue v
    return v

extendEnv :: Name t -> Value t -> Env -> Env
extendEnv (Name name _tyrep) v (Env env) =
    Env (Map.insert name (SomeValue v) env)


--
-- Heaps for mutable variable
--

data SomeVar where
     SomeVar :: Var t -> SomeVar

-- | The heap is a mapping of 'Var's to their current values.
--
newtype Heap = Heap (Map VarId SomeValue)
  deriving (Show, Semigroup, Monoid)

-- | The STM semantics uses two heaps, the other one is called the allocations.
type Allocs = Heap


readVar :: Heap -> Var t -> Value t
readVar (Heap heap) (Var n tyrep) =
    fromMaybe (error "readVar: no such var") $ do
    SomeValue v <- Map.lookup n heap
    Refl        <- tyrep `eqTyRep` tyRepValue v
    return v

writeVar :: Heap -> Var t -> Value t -> Heap
writeVar (Heap heap) (Var n tyrep) v' =
    fromMaybe (error "writeVar: no such var") $ do
    SomeValue v <- Map.lookup n heap
    Refl        <- tyrep `eqTyRep` tyRepValue v
    let heap' = Heap (Map.insert n (SomeValue v') heap)
    return heap'

-- | Extend the heap and allocs with a fresh variable.
extendHeap :: (Heap, Allocs) -> Value t -> (Var t, Heap, Allocs)
extendHeap (Heap heap, Heap allocs) v =
    (var, Heap heap', Heap allocs')
  where
    var     = Var n' (tyRepValue v)
    heap'   = Map.insert n' (SomeValue v) heap
    allocs' = Map.insert n' (SomeValue v) allocs
    n'     :: VarId
    n'      = case Map.maxViewWithKey heap of
                Nothing          -> VarId 0
                Just ((n, _), _) -> succ n


--
-- Top level results
--

-- | The overall result of an STM transaction.
--
-- This is used for both the reference evaluator 'evalAtomically' and the
-- conversion into the implementation STM via 'execAtomically'.
--
data TxResult =
       TxComitted ImmValue
     | TxBlocked
     | TxAborted  ImmValue
  deriving (Eq, Show)

-- | An immutable snapshot of a 'Value' where the current values of the mutable
-- variables are captured and included.
--
-- 'ImmValVar' is an evidence that it was the value within in a mutable
-- variable; the identity of the variable is forgotten.
--
data ImmValue where

    ImmValUnit ::             ImmValue
    ImmValInt  :: Int      -> ImmValue
    ImmValVar  :: ImmValue -> ImmValue
  deriving (Eq, Show)

-- | In the execution in real STM transactions are aborted by throwing an
-- exception.
--
instance Exception ImmValue



--
-- Evaluation
--

evalExpr :: Env -> Expr t -> Value t
evalExpr  env  (ExprName n) = lookupEnv env n
evalExpr _env   ExprUnit    = ValUnit
evalExpr _env  (ExprInt n)  = ValInt n

-- | The normal form for a 'Term' after execution.
--
data NfTerm (t :: Type) where

    NfReturn :: Value t -> NfTerm t
    NfThrow  :: Value a -> NfTerm t
    NfRetry  ::            NfTerm t

deriving instance Show (NfTerm t)


-- | The STM transition rules. They reduce a 'Term' to a normal-form 'NfTerm'.
--
-- Compare the implementation of this against the operational semantics in
-- Figure 4 in the paper. Note that @catch@ is not included.
--
evalTerm :: Env -> Heap -> Allocs -> Term t -> (NfTerm t, Heap, Allocs)
evalTerm !env !heap !allocs term = case term of

    Return e -> (NfReturn e', heap, allocs)
      where
        e' = evalExpr env e

    Throw  e -> (NfThrow e', heap, allocs)
      where
        e'  = evalExpr env e

    Retry    -> (NfRetry,                   heap, allocs)

    -- Rule READ
    ReadTVar nvar -> (NfReturn (readVar heap var), heap,  allocs)
      where
        ValVar var = lookupEnv env nvar

    -- Rule WRITE
    WriteTVar nvar exp -> (NfReturn ValUnit, heap', allocs)
      where
        heap'             = writeVar heap var val
        (ValVar var)      = lookupEnv env nvar
        val               = evalExpr env exp

    -- Rule NEW
    NewTVar exp ->
      let val                   = evalExpr env exp
          (var, heap', allocs') = extendHeap (heap, allocs) val
      in (NfReturn (ValVar var), heap', allocs')

    Bind t1 name t2 ->
      let (nf1, heap', allocs') = evalTerm env heap allocs t1 in
      case nf1 of

        -- Rule BIND
        NfReturn v -> evalTerm env' heap' allocs' t2
          where
            env' = extendEnv name v env

        -- Rule THROW
        NfThrow v  -> (NfThrow v, heap', allocs')

        -- Rule RETRY
        NfRetry    -> (NfRetry,   heap', allocs')

    OrElse t1 t2 ->
      let (nft1, heap', allocs') = evalTerm env heap allocs t1 in
      case nft1 of

        -- Rule OR1
        NfReturn v -> (NfReturn v, heap', allocs')

        -- Rule OR2
        NfThrow  v -> (NfThrow  v, heap', allocs')

        -- Rule OR3
        NfRetry    -> evalTerm env heap allocs t2

-- | The top level rule for STM transitions (on closed terms).
--
evalAtomically :: Term t -> (TxResult, Heap)
evalAtomically t =
    let env                  = Env mempty
        heap                 = mempty
        allocs               = mempty
        (t', heap', allocs') = evalTerm env heap allocs t in
    case t' of

      -- Rule ARET
      NfReturn v -> (TxComitted v', heap')
                      where v' = snapshotValue heap' v

      -- Rule ATHROW
      NfThrow  v -> (TxAborted  v', heap <> allocs')
                      where v' = snapshotValue heap' v

      -- There is no rule in the paper for atomic retry because the lack of
      -- that case means the system has to progress by picking a different
      -- thread which is exactly what one wants for retry.
      --
      -- But we have to have a total result. So we have a blocked result
      -- with the heap unchanged.
      NfRetry    -> (TxBlocked, heap)

-- | Capture an immutable snapshot of a value, given the current value of the
-- mutable variable heap.
--
snapshotValue :: Heap -> Value t -> ImmValue
snapshotValue _  ValUnit   = ImmValUnit
snapshotValue _ (ValInt x) = ImmValInt x
snapshotValue h (ValVar n) = ImmValVar (snapshotValue h (readVar h n))


--
-- Execution in an STM monad (real or sim)
--

data ExecValue m (t :: Type) where

    ExecValUnit ::                           ExecValue m TyUnit
    ExecValInt  :: Int                    -> ExecValue m TyInt
    ExecValVar  :: TVar m (ExecValue m t)
                -> TyRep t                -> ExecValue m (TyVar t)

instance Show (ExecValue m t) where
  show  ExecValUnit         = "ExecValUnit"
  show (ExecValInt x)       = "ExecValInt " ++ show x
  show (ExecValVar _ tyrep) = "ExecValVar (<tvar> :: " ++ show tyrep ++ ")"


data SomeExecValue m where
     SomeExecValue :: ExecValue m t -> SomeExecValue m

deriving instance Show (SomeExecValue m)


newtype ExecEnv m = ExecEnv (Map NameId (SomeExecValue m))
  deriving (Semigroup, Monoid)

tyRepExecValue :: ExecValue m t -> TyRep t
tyRepExecValue  ExecValUnit         = TyRepUnit
tyRepExecValue (ExecValInt _)       = TyRepInt
tyRepExecValue (ExecValVar _ tyrep) = TyRepVar tyrep

lookupExecEnv :: ExecEnv m -> Name t -> ExecValue m t
lookupExecEnv (ExecEnv env) (Name name tyrep) =
    fromMaybe (error "lookupExecEnv: no such var") $ do
    SomeExecValue v <- Map.lookup name env
    Refl            <- tyrep `eqTyRep` tyRepExecValue v
    return v

extendExecEnv :: Name t -> ExecValue m t -> ExecEnv m -> ExecEnv m
extendExecEnv (Name name _tyrep) v (ExecEnv env) =
    ExecEnv (Map.insert name (SomeExecValue v) env)


-- | Execute an STM 'Term' in the 'STM' monad.
--
execTerm :: (MonadSTM m, MonadThrow (STM m))
         => ExecEnv m
         -> Term t
         -> STM m (ExecValue m t)
execTerm env t =
    case t of
      Return e -> do
        let e' = execExpr env e
        return e'

      Throw e -> do
        let e' = execExpr env e
        throwM =<< snapshotExecValue e'

      Retry -> retry

      ReadTVar n -> do
        let tv = case lookupExecEnv env n of
                   ExecValVar v _ -> v
        readTVar tv

      WriteTVar n e -> do
        let tv = case lookupExecEnv env n of
                   ExecValVar v _ -> v
            e' = execExpr env e
        writeTVar tv e'
        return ExecValUnit

      NewTVar e -> do
        let e'    = execExpr env e
            tyrep = tyRepExecValue e'
        tv <- newTVar e'
        return (ExecValVar tv tyrep)


      Bind t1 n1 t2 -> do
        v1 <- execTerm env t1
        let env' = extendExecEnv n1 v1 env
        execTerm env' t2

      OrElse t1 t2 -> execTerm env t1
             `orElse` execTerm env t2

execExpr :: forall m t. ExecEnv m -> Expr t -> ExecValue m t
execExpr _    ExprUnit    = ExecValUnit
execExpr _   (ExprInt x)  = ExecValInt x
execExpr env (ExprName n) = lookupExecEnv env n

snapshotExecValue :: MonadSTM m => ExecValue m t -> STM m ImmValue
snapshotExecValue  ExecValUnit     = return  ImmValUnit
snapshotExecValue (ExecValInt x)   = return (ImmValInt x)
snapshotExecValue (ExecValVar v _) = fmap ImmValVar
                                          (snapshotExecValue =<< readTVar v)

execAtomically :: forall m t. (MonadSTM m, MonadThrow (STM m), MonadCatch m)
               => Term t -> m TxResult
execAtomically t =
    toTxResult <$> try (atomically action')
  where
    action  = snapshotExecValue =<< execTerm (mempty :: ExecEnv m) t

    action' = fmap Just action `orElse` return Nothing
    -- We want to observe if the transaction would block. If we trust the STM
    -- implementation then we can just use 'orElse' to observe the blocking.

    toTxResult (Right (Just x)) = TxComitted x
    toTxResult (Left e)         = TxAborted  e
    toTxResult (Right Nothing)  = TxBlocked


--
-- QuickCheck generators
--

instance Arbitrary SomeTerm where
  arbitrary = genSomeTerm emptyGenEnv

  shrink (SomeTerm tyrep t) = [ SomeTerm tyrep t' | t' <- shrinkTerm t ]


data SomeTerm where
     SomeTerm :: TyRep t -> Term t -> SomeTerm

data SomeExpr where
     SomeExpr :: Expr t -> SomeExpr

deriving instance Show SomeTerm
deriving instance Show SomeExpr


-- | The generator environment, used to keep track of what names are in scope
-- in the terms and expressions we generate.
--
data GenEnv = GenEnv {
       -- | The sets of names, grouped by type
       envNames    :: TyTrie NameId,

       -- | For managing the fresh name supply
       envNextName :: NameId
     }

data TyTrie a =
     TyTrieEmpty
   | TyTrieNode {
       trieUnit :: [a],
       trieInt  :: [a],
       trieVar  :: TyTrie a
     }
  deriving Show

lookupTyTrie :: TyTrie a -> TyRep t -> [a]
lookupTyTrie TyTrieNode{trieUnit}  TyRepUnit       = trieUnit
lookupTyTrie TyTrieNode{trieInt}   TyRepInt        = trieInt
lookupTyTrie TyTrieNode{trieVar}  (TyRepVar tyrep) = lookupTyTrie trieVar tyrep
lookupTyTrie _                     _               = []

insertTyTrie :: TyTrie a -> TyRep t -> a -> TyTrie a
insertTyTrie TyTrieEmpty tyrep x =
    case tyrep of
      TyRepUnit       -> TyTrieNode [x] [] TyTrieEmpty
      TyRepInt        -> TyTrieNode [] [x] TyTrieEmpty
      TyRepVar tyrep' -> TyTrieNode [] [] (insertTyTrie TyTrieEmpty tyrep' x)

insertTyTrie node@TyTrieNode{trieUnit = us, trieInt = ns, trieVar} tyrep x =
    case tyrep of
      TyRepUnit       -> node { trieUnit = x : us }
      TyRepInt        -> node { trieInt  = x : ns }
      TyRepVar tyrep' -> node { trieVar  = insertTyTrie trieVar tyrep' x }

emptyGenEnv :: GenEnv
emptyGenEnv = GenEnv TyTrieEmpty (NameId 0)

lookupNames :: GenEnv -> TyRep t -> Maybe [Name t]
lookupNames GenEnv{envNames} tyrep =
    case lookupTyTrie envNames tyrep of
      [] -> Nothing
      ns -> Just [ Name n tyrep | n <- ns ]

freshName :: GenEnv -> TyRep t -> (Name t, GenEnv)
freshName GenEnv {envNames, envNextName} tyrep =
    (name, env')
  where
    name = Name envNextName tyrep
    env' = GenEnv {
             envNames    = insertTyTrie envNames tyrep envNextName,
             envNextName = succ envNextName
           }

pickName :: GenEnv -> TyRep t -> Maybe (Gen (Name t))
pickName env tyrep =
    elements <$> lookupNames env tyrep

data SomeVarName where
     SomeVarName :: Name (TyVar t) -> SomeVarName
deriving instance Show SomeVarName

lookupVarNames :: GenEnv -> [SomeVarName]
lookupVarNames GenEnv{envNames = TyTrieEmpty} = []
lookupVarNames GenEnv{envNames = TyTrieNode{trieVar = trieVar0}} =
    go 0 trieVar0
  where
    go :: Int -> TyTrie NameId -> [SomeVarName]
    go _ TyTrieEmpty = []
    go d TyTrieNode{trieUnit = us, trieInt = ns, trieVar} =
         [ deep n TyRepUnit d | n <- us ]
      ++ [ deep n TyRepInt  d | n <- ns ]
      ++ go (succ d) trieVar

deep :: NameId -> TyRep t -> Int -> SomeVarName
deep nid tyrep 0 = SomeVarName (Name nid (TyRepVar tyrep))
deep nid tyrep d = deep nid (TyRepVar tyrep) (pred d)


-- | Generate a 'Term' of some type.
--
genSomeTerm :: GenEnv -> Gen SomeTerm
genSomeTerm env =
  oneof
    [ SomeTerm          TyRepUnit
        <$> genTerm env TyRepUnit
    , SomeTerm          TyRepInt
        <$> genTerm env TyRepInt
    , SomeTerm          (TyRepVar TyRepInt)
        <$> genTerm env (TyRepVar TyRepInt)
    , SomeTerm          (TyRepVar (TyRepVar TyRepInt))
        <$> genTerm env (TyRepVar (TyRepVar TyRepInt))
      -- vars of vars is probably deep enough.
    ]

-- | Generate a 'Term' of a given type.
--
genTerm :: GenEnv -> TyRep t -> Gen (Term t)
genTerm env tyrep =
    sized $ \sz ->
      if sz <= 1
        then leafTerm
        else frequency [ (1, leafTerm), (2, binTerm) ]
  where
    leafTerm =
      frequency' $
        [ (2, fmap Return <$> genExpr env tyrep)
        , (1, Just ((\(SomeExpr e) -> Throw e) <$> genSomeExpr env))
        , (1, Just (pure Retry))
        , (3, do genvarname <- pickName env (TyRepVar tyrep)
                 return (ReadTVar <$> genvarname))
        , (3, case tyrep of
                TyRepUnit ->
                  case [ WriteTVar varname <$> genexpr
                       | SomeVarName varname <- lookupVarNames env
                       , let TyRepVar valtyrep = nameTyRep varname
                       , genexpr <- maybeToList $ genExpr env valtyrep
                       ]
                  of [] -> Nothing
                     ws -> Just (oneof ws)
                TyRepVar vartyrep ->
                  fmap NewTVar <$> genExpr env vartyrep
                TyRepInt ->
                  Nothing)
        ]

    binTerm = frequency [ (2, bindTerm), (1, orElseTerm)]

    bindTerm =
      sized $ \sz -> do
        let sz1 = sz     `div` 3   -- 1/3
            sz2 = sz * 2 `div` 3   -- 2/3
            -- To right bias it a bit

        SomeTerm t1ty t1 <- resize sz1 (genSomeTerm env)
        let (name, env') = freshName env t1ty
        t2 <- resize sz2 (genTerm env' tyrep)
        return (Bind t1 name t2)

    orElseTerm =
      sized $ \sz -> resize (sz `div` 2) $
        OrElse <$> genTerm env tyrep
               <*> genTerm env tyrep

genSomeExpr :: GenEnv -> Gen SomeExpr
genSomeExpr env =
    oneof'
      [ fmap SomeExpr <$> genExpr env TyRepUnit
      , fmap SomeExpr <$> genExpr env TyRepInt
      , fmap SomeExpr <$> genExpr env (TyRepVar TyRepInt)
      , fmap SomeExpr <$> genExpr env (TyRepVar (TyRepVar TyRepInt))
      ]

genExpr :: GenEnv -> TyRep t -> Maybe (Gen (Expr t))
genExpr env tyrep@TyRepUnit =
    Just $ oneof'
      [ Just (pure ExprUnit)
      , fmap ExprName <$> pickName env tyrep
      ]
genExpr env tyrep@TyRepInt  =
    Just $ oneof'
      [ Just (ExprInt <$> arbitrary)
      , fmap ExprName <$> pickName env tyrep
      ]
genExpr env tyrep@TyRepVar{} =
    fmap ExprName <$> pickName env tyrep


elements' :: [Maybe a] -> Gen a
elements' xs = elements [ g | Just g <- xs ]

oneof' :: [Maybe (Gen a)] -> Gen a
oneof' xs = oneof [ g | Just g <- xs ]

frequency' :: [(Int, Maybe (Gen a))] -> Gen a
frequency' xs = frequency [ (n, g) | (n, Just g) <- xs ]

shrinkTerm :: Term t -> [Term t]
shrinkTerm t =
    case t of
      Return e      -> [Return e' | e' <- shrinkExpr e]
      Throw e       -> [Throw  e' | e' <- shrinkExpr e]
      Retry         -> []
      ReadTVar _    -> []

      WriteTVar _ _ -> [Return ExprUnit] --TODO: there are other less drastic shrinks possible here

      NewTVar e     -> [NewTVar e' | e' <- shrinkExpr e]

      Bind t1 n t2  -> [ t2 | nameId n `Set.notMember` freeNamesTerm t2 ]
                    ++ [ Bind t1' n t2  | t1' <- shrinkTerm t1 ]
                    ++ [ Bind t1  n t2' | t2' <- shrinkTerm t2 ]

      OrElse t1 t2  -> [t1, t2]
                    ++ [ OrElse t1' t2  | t1' <- shrinkTerm t1 ]
                    ++ [ OrElse t1  t2' | t2' <- shrinkTerm t2 ]

shrinkExpr :: Expr t -> [Expr t]
shrinkExpr  ExprUnit                        = []
shrinkExpr (ExprInt n)                      = [ExprInt n' | n' <- shrink n]
shrinkExpr (ExprName (Name _ TyRepUnit))    = [ExprUnit]
shrinkExpr (ExprName (Name _ TyRepInt))     = [ExprInt 0]
shrinkExpr (ExprName (Name _ (TyRepVar _))) = []

freeNamesTerm :: Term t -> Set NameId
freeNamesTerm (Return e)      = freeNamesExpr e
freeNamesTerm (Throw  e)      = freeNamesExpr e
freeNamesTerm  Retry          = Set.empty
freeNamesTerm (ReadTVar  n)   = Set.singleton (nameId n)
freeNamesTerm (WriteTVar n e) = Set.singleton (nameId n) <> freeNamesExpr e
freeNamesTerm (NewTVar e)     = freeNamesExpr e
freeNamesTerm (Bind t1 n t2)  = freeNamesTerm t1 <> Set.delete (nameId n)
                                                               (freeNamesTerm t2)
freeNamesTerm (OrElse t1 t2)  = freeNamesTerm t1 <> freeNamesTerm t2

freeNamesExpr :: Expr t -> Set NameId
freeNamesExpr  ExprUnit    = Set.empty
freeNamesExpr (ExprInt _)  = Set.empty
freeNamesExpr (ExprName n) = Set.singleton (nameId n)

nameId :: Name t -> NameId
nameId (Name nid _) = nid

prop_genSomeTerm :: SomeTerm -> Property
prop_genSomeTerm (SomeTerm tyrep term) =
    tabulate "1. Term type"  [show tyrep] $
    tabulate "2. Term size"  [show (sizeBucket (termSize term))] $
    tabulate "3. Term depth" [show (termDepth term)] $
    case evalAtomically term of
      (!_val, !_heap') -> True
  where
    sizeBucket s = ((s-1) `div` 10 + 1) * 10


termSize :: Term a -> Int
termSize Return{}     = 1
termSize Throw{}      = 1
termSize Retry{}      = 1
termSize ReadTVar{}   = 1
termSize WriteTVar{}  = 1
termSize NewTVar{}    = 1
termSize (Bind a _ b) = 1 + termSize a + termSize b
termSize (OrElse a b) = 1 + termSize a + termSize b

termDepth :: Term a -> Int
termDepth Return{}     = 1
termDepth Throw{}      = 1
termDepth Retry{}      = 1
termDepth ReadTVar{}   = 1
termDepth WriteTVar{}  = 1
termDepth NewTVar{}    = 1
termDepth (Bind a _ b) = 1 + max (termDepth a) (termDepth b)
termDepth (OrElse a b) = 1 + max (termDepth a) (termDepth b)

showTerm :: Int -> Term t -> ShowS
showTerm p (Return e)      = showParen (p > 10) $
                               showString "return " . showExpr 11 e
showTerm p (Throw  e)      = showParen (p > 10) $
                               showString "throwSTM " . showExpr 11 e
showTerm _  Retry          = showString "retry"
showTerm p (ReadTVar  n)   = showParen (p > 10) $
                               showString "readTVar " . showName n
showTerm p (WriteTVar n e) = showParen (p > 10) $
                               showString "writeTVar " . showName n
                                        . showChar ' ' . showExpr 11 e
showTerm p (NewTVar e)     = showParen (p > 10) $
                               showString "newTVar " . showExpr 11 e
showTerm p (Bind t1 n t2)  = showParen (p > 1) $
                               showTerm 2 t1 . showString " >>= \\"
                             . showNameTyped n . showString " -> "
                             . showTerm 1 t2
showTerm p (OrElse t1 t2)  = showParen (p > 9) $
                               showTerm 10 t1 . showString " `orElse` "
                             . showTerm 10 t2

showExpr :: Int -> Expr t -> ShowS
showExpr _ ExprUnit     = showString "()"
showExpr p (ExprInt n)  = showsPrec p n
showExpr _ (ExprName n) = showName n

showName :: Name t -> ShowS
showName (Name (NameId nid) _) = showChar 'v' . shows nid

showNameTyped :: Name t -> ShowS
showNameTyped (Name (NameId nid) tyrep) =
    showChar 'v' . shows nid
  . showString " :: " . showTyRep 0 tyrep

showTyRep :: Int -> TyRep t -> ShowS
showTyRep _  TyRepUnit   = showString "()"
showTyRep _  TyRepInt    = showString "Int"
showTyRep p (TyRepVar t) = showParen (p > 10) $
                             showString "TVar " . showTyRep 11 t
