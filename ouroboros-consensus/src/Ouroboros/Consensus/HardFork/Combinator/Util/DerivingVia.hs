{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Ouroboros.Consensus.HardFork.Combinator.Util.DerivingVia (
    LiftMismatch (..)
  , LiftNP (..)
  , LiftNS (..)
  , LiftNamedMismatch (..)
  , LiftNamedNP (..)
  , LiftNamedNS (..)
  , LiftNamedTelescope (..)
  , LiftOptNP (..)
  , LiftTelescope (..)
  ) where

import           Data.List (intercalate)
import           Data.Proxy
import           Data.SOP.Dict
import           Data.SOP.Strict
import           Data.Typeable
import           GHC.TypeLits
import           NoThunks.Class (NoThunks (..))

import           Ouroboros.Consensus.Util.OptNP (OptNP (..))

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match (Mismatch)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

proofAll :: SListI xs
         => (forall x . Dict c x -> Dict d x)
         -> Dict (All c) xs -> Dict (All d) xs
proofAll f dict = all_NP (hmap f (unAll_NP dict))

proofLift :: (SingleEraBlock x => c (f x))
          => Dict SingleEraBlock x -> Dict (Compose c f) x
proofLift Dict = Dict

liftEras :: (All SingleEraBlock xs, forall x. SingleEraBlock x => c (f x))
         => Proxy xs -> Proxy c -> Proxy f -> Dict (All (Compose c f)) xs
liftEras _ _ _ = proofAll proofLift Dict

{-------------------------------------------------------------------------------
  LiftNS
-------------------------------------------------------------------------------}

newtype LiftNS f xs = LiftNS (NS f xs)

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Eq (f x))
      => Eq (LiftNS f xs) where
  LiftNS x == LiftNS y =
      case liftEras (Proxy @xs) (Proxy @Eq) (Proxy @f) of { Dict ->
          x == y
        }

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Ord (f x))
      => Ord (LiftNS f xs) where
  LiftNS x `compare` LiftNS y =
      case liftEras (Proxy @xs) (Proxy @Eq)  (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Ord) (Proxy @f) of { Dict ->
          x `compare` y
        }}

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Show (f x))
      => Show (LiftNS f xs) where
  show (LiftNS x) =
      case liftEras (Proxy @xs) (Proxy @Show) (Proxy @f) of { Dict ->
          show x
        }

{-------------------------------------------------------------------------------
  LiftNP
-------------------------------------------------------------------------------}

newtype LiftNP f xs = LiftNP (NP f xs)

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Eq (f x))
      => Eq (LiftNP f xs) where
  LiftNP x == LiftNP y =
      case liftEras (Proxy @xs) (Proxy @Eq) (Proxy @f) of { Dict ->
          x == y
        }

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Ord (f x))
      => Ord (LiftNP f xs) where
  LiftNP x `compare` LiftNP y =
      case liftEras (Proxy @xs) (Proxy @Eq)  (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Ord) (Proxy @f) of { Dict ->
          x `compare` y
        }}

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Show (f x))
      => Show (LiftNP f xs) where
  show (LiftNP x) =
      case liftEras (Proxy @xs) (Proxy @Show) (Proxy @f) of { Dict ->
          show x
        }

{-------------------------------------------------------------------------------
  LiftOptNP
-------------------------------------------------------------------------------}

newtype LiftOptNP empty f xs = LiftOptNP (OptNP empty f xs)

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Eq (f x))
      => Eq (LiftOptNP empty f xs) where
  LiftOptNP x == LiftOptNP y =
      case liftEras (Proxy @xs) (Proxy @Eq) (Proxy @f) of { Dict ->
          x == y
        }

instance (All SingleEraBlock xs, forall x. SingleEraBlock x => Show (f x))
      => Show (LiftOptNP empty f xs) where
  show (LiftOptNP x) =
      case liftEras (Proxy @xs) (Proxy @Show) (Proxy @f) of { Dict ->
          show x
        }

{-------------------------------------------------------------------------------
  LiftTelescope
-------------------------------------------------------------------------------}

newtype LiftTelescope g f xs = LiftTelescope (Telescope g f xs)

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => Eq (g x)
         , forall x. SingleEraBlock x => Eq (f x)
         ) => Eq (LiftTelescope g f xs) where
  LiftTelescope x == LiftTelescope y =
      case liftEras (Proxy @xs) (Proxy @Eq) (Proxy @g) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Eq) (Proxy @f) of { Dict ->
          x == y
        }}

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => Ord (f x)
         , forall x. SingleEraBlock x => Ord (g x)
         ) => Ord (LiftTelescope g f xs) where
  compare (LiftTelescope x) (LiftTelescope y) =
      case liftEras (Proxy @xs) (Proxy @Eq)  (Proxy @g) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Ord) (Proxy @g) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Eq)  (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Ord) (Proxy @f) of { Dict ->
          compare x y
        }}}}

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => Show (g x)
         , forall x. SingleEraBlock x => Show (f x)
         ) => Show (LiftTelescope g f xs) where
  show (LiftTelescope x) =
      case liftEras (Proxy @xs) (Proxy @Show) (Proxy @g) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Show) (Proxy @f) of { Dict ->
          show x
        }}

{-------------------------------------------------------------------------------
  LiftMismatch
-------------------------------------------------------------------------------}

newtype LiftMismatch f g xs = LiftMismatch (Mismatch f g xs)

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => Eq (f x)
         , forall x. SingleEraBlock x => Eq (g x)
         ) => Eq (LiftMismatch f g xs) where
  LiftMismatch x == LiftMismatch y =
      case liftEras (Proxy @xs) (Proxy @Eq) (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Eq) (Proxy @g) of { Dict ->
          x == y
        }}

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => Ord (f x)
         , forall x. SingleEraBlock x => Ord (g x)
         ) => Ord (LiftMismatch f g xs) where
  compare (LiftMismatch x) (LiftMismatch y) =
      case liftEras (Proxy @xs) (Proxy @Eq)  (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Ord) (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Eq)  (Proxy @g) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Ord) (Proxy @g) of { Dict ->
          compare x y
        }}}}

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => Show (f x)
         , forall x. SingleEraBlock x => Show (g x)
         ) => Show (LiftMismatch f g xs) where
  show (LiftMismatch x) =
      case liftEras (Proxy @xs) (Proxy @Show) (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @Show) (Proxy @g) of { Dict ->
          show x
        }}

{-------------------------------------------------------------------------------
  LiftNamedNS
-------------------------------------------------------------------------------}

newtype LiftNamedNS (name :: Symbol) f xs = LiftNamedNS (NS f xs)

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => NoThunks (f x)
         , KnownSymbol name
         ) => NoThunks (LiftNamedNS name f xs) where
  showTypeOf _ = symbolVal (Proxy @name) ++ " " ++ showBlockTypes (sList :: SList xs)

  wNoThunks ctxt (LiftNamedNS x) =
      case liftEras (Proxy @xs) (Proxy @NoThunks) (Proxy @f) of { Dict ->
          wNoThunks ctxt x
        }

{-------------------------------------------------------------------------------
  LiftNamedNP
-------------------------------------------------------------------------------}

newtype LiftNamedNP (name :: Symbol) f xs = LiftNamedNP (NP f xs)

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => NoThunks (f x)
         , KnownSymbol name
         ) => NoThunks (LiftNamedNP name f xs) where
  showTypeOf _ = symbolVal (Proxy @name) ++ " " ++ showBlockTypes (sList :: SList xs)

  wNoThunks ctxt (LiftNamedNP x) =
      case liftEras (Proxy @xs) (Proxy @NoThunks) (Proxy @f) of { Dict ->
          wNoThunks ctxt x
        }

{-------------------------------------------------------------------------------
  LiftNamedTelescope
-------------------------------------------------------------------------------}

newtype LiftNamedTelescope (name :: Symbol) f g xs = LiftNamedTelescope (Telescope f g xs)

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => NoThunks (f x)
         , forall x. SingleEraBlock x => NoThunks (g x)
         , KnownSymbol name
         ) => NoThunks (LiftNamedTelescope name f g xs) where
  showTypeOf _ = symbolVal (Proxy @name) ++ " " ++ showBlockTypes (sList :: SList xs)

  wNoThunks ctxt (LiftNamedTelescope x) =
      case liftEras (Proxy @xs) (Proxy @NoThunks) (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @NoThunks) (Proxy @g) of { Dict ->
          wNoThunks ctxt x
        }}

{-------------------------------------------------------------------------------
  LiftNamedTelescope
-------------------------------------------------------------------------------}

newtype LiftNamedMismatch (name :: Symbol) f g xs = LiftNamedMismatch (Mismatch f g xs)

instance ( All SingleEraBlock xs
         , forall x. SingleEraBlock x => NoThunks (f x)
         , forall x. SingleEraBlock x => NoThunks (g x)
         , KnownSymbol name
         ) => NoThunks (LiftNamedMismatch name f g xs) where
  showTypeOf _ = symbolVal (Proxy @name) ++ " " ++ showBlockTypes (sList :: SList xs)

  wNoThunks ctxt (LiftNamedMismatch x) =
      case liftEras (Proxy @xs) (Proxy @NoThunks) (Proxy @f) of { Dict ->
      case liftEras (Proxy @xs) (Proxy @NoThunks) (Proxy @g) of { Dict ->
          wNoThunks ctxt x
        }}

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

showBlockTypes :: All SingleEraBlock xs => SList xs -> String
showBlockTypes =
    (\names -> "[" ++ intercalate "," names ++ "]") . hcollapse . go
  where
    go :: All SingleEraBlock xs' => SList xs' -> NP (K String) xs'
    go SNil  = Nil
    go SCons = typeRep' :* go sList

    typeRep' :: forall blk. SingleEraBlock blk => K String blk
    typeRep' = K . show $ typeRep (Proxy @blk)
