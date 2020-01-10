{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ouroboros.Consensus.Util.Orphans () where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.Serialise (Serialise (..))
import           Control.Concurrent.STM (readTVarIO)
import           Control.Monad.Identity
import           Control.Monad.Trans
import           Crypto.Random
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as PSQ

import           Control.Tracer (Tracer)

import           Control.Monad.Class.MonadTime (Time (..))

import           Ouroboros.Consensus.Util.MonadSTM.NormalForm

import           Cardano.Crypto.Hash (Hash)
import           Cardano.Prelude (NoUnexpectedThunks (..), OnlyCheckIsWHNF (..),
                     allNoUnexpectedThunks, noUnexpectedThunksInKeysAndValues)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader, HeaderHash, Point (..))
import           Ouroboros.Network.MockChain.Chain (Chain (..))
import           Ouroboros.Network.Point (WithOrigin (..), blockPointHash,
                     blockPointSlot)

import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance Condense (HeaderHash block) => Condense (Point block) where
    condense (Point Origin)        = "Origin"
    condense (Point (At blk)) =
      "(Point " <> condense ptSlot <> ", " <> condense ptHash <> ")"
      where
      ptSlot = blockPointSlot blk
      ptHash = blockPointHash blk

instance Condense block => Condense (Chain block) where
    condense Genesis   = "Genesis"
    condense (cs :> b) = condense cs <> " :> " <> condense b

instance (Condense block, HasHeader block, Condense (HeaderHash block))
    => Condense (AnchoredFragment block) where
    condense (AF.Empty pt) = "EmptyAnchor " <> condense pt
    condense (cs AF.:> b)  = condense cs <> " :> " <> condense b

{-------------------------------------------------------------------------------
  MonadRandom
-------------------------------------------------------------------------------}

instance MonadRandom m => MonadRandom (IdentityT m) where
     getRandomBytes = lift . getRandomBytes

{-------------------------------------------------------------------------------
  Serialise
-------------------------------------------------------------------------------}

instance Serialise (Hash h a) where

{-------------------------------------------------------------------------------
  NoUnexpectedThunks
-------------------------------------------------------------------------------}

instance NoUnexpectedThunks a => NoUnexpectedThunks (StrictTVar IO a) where
  showTypeOf _ = "StrictTVar IO"
  whnfNoUnexpectedThunks ctxt tv = do
      -- We can't use @atomically $ readTVar ..@ here, as that will lead to a
      -- "Control.Concurrent.STM.atomically was nested" exception.
      a <- readTVarIO (toLazyTVar tv)
      noUnexpectedThunks ctxt a

instance (NoUnexpectedThunks k, NoUnexpectedThunks v)
      => NoUnexpectedThunks (Bimap k v) where
  whnfNoUnexpectedThunks ctxt = noUnexpectedThunksInKeysAndValues ctxt
                              . Bimap.toList

instance ( NoUnexpectedThunks p
         , NoUnexpectedThunks v
         , Ord p
         ) => NoUnexpectedThunks (IntPSQ p v) where
  showTypeOf _ = "IntPSQ"
  whnfNoUnexpectedThunks ctxt = allNoUnexpectedThunks
                              . concatMap (\(k, p, v) ->
                                [ noUnexpectedThunks ctxt k
                                , noUnexpectedThunks ctxt p
                                , noUnexpectedThunks ctxt v])
                              . PSQ.toList

deriving via OnlyCheckIsWHNF "Decoder" (Decoder s a) instance NoUnexpectedThunks (Decoder s a)

deriving via OnlyCheckIsWHNF "Tracer" (Tracer m ev) instance NoUnexpectedThunks (Tracer m ev)

deriving newtype instance NoUnexpectedThunks Time
