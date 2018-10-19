{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.GlobalState (
    GlobalState(..)
  , forLeaders
    -- * QuickCheck support
  , GenSt
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import Test.QuickCheck
import qualified Data.Map.Strict as Map

import Ouroboros

{-------------------------------------------------------------------------------
  Global state
-------------------------------------------------------------------------------}

data GlobalState p = GlobalState (Map NodeId (OuroborosState p))

instance Arbitrary (GlobalState p) where
  arbitrary = return (GlobalState mempty)

stateFor :: NodeId -> GlobalState p -> OuroborosState p
stateFor nid (GlobalState ss) = ss Map.! nid

knownNodes :: GlobalState p -> [NodeId]
knownNodes (GlobalState ss) = Map.keys ss

putStateFor :: NodeId -> OuroborosState p -> GlobalState p -> GlobalState p
putStateFor nid s (GlobalState ss) = GlobalState (Map.insert nid s ss)

{-------------------------------------------------------------------------------
  Choose leaders
-------------------------------------------------------------------------------}

forLeaders :: ( KnownOuroborosProtocol p
              , MonadState (GlobalState p) m
              , Traversable t
              )
           => Slot
           -> ([(NodeId, ProofIsLeader p)] -> t (NodeId, ProofIsLeader p))
           -> (forall n. MonadOuroborosState p n => ProofIsLeader p -> n a)
           -> m (t a)
forLeaders slot pickLeaders k = do
    nodes   <- gets knownNodes
    leaders <- fmap catMaybes $ forM nodes $ \nid ->
                 fmap (nid, ) <$> (chooseNode nid $ checkIsLeader slot)
    for (pickLeaders leaders) $ \(nid, isLeader) ->
      chooseNode nid $ k isLeader

{-------------------------------------------------------------------------------
  Internal: run a specific node
-------------------------------------------------------------------------------}

newtype ChooseNode m a = ChooseNode { chooseNode' :: ReaderT NodeId m a }
  deriving (Functor, Applicative, Monad)

chooseNode :: (KnownOuroborosProtocol p, MonadState (GlobalState p) m)
           => NodeId -> (forall n. MonadOuroborosState p n => n a) -> m a
chooseNode = flip (runReaderT . chooseNode')

instance (KnownOuroborosProtocol p, MonadState (GlobalState p) m)
      => MonadOuroborosState p (ChooseNode m) where
  getOuroborosState   = ChooseNode $ do
                          nid <- ask
                          lift $ gets (stateFor nid)
  putOuroborosState s = ChooseNode $ do
                          nid <- ask
                          lift $ modify (putStateFor nid s)

{-------------------------------------------------------------------------------
  Support for dealing with state and 'Arbitrary'
-------------------------------------------------------------------------------}

type GenSt p a = StateT (GlobalState p) Gen a
