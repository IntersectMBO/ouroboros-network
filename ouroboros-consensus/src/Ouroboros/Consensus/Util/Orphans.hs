{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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
import           Data.SOP.Strict
import           Data.Void (Void)

import           Control.Tracer (Tracer)

import           Control.Monad.Class.MonadTime (Time (..))

import           Ouroboros.Consensus.Util.MonadSTM.NormalForm

import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Crypto.Hash (Hash)
import           Cardano.Prelude (NoUnexpectedThunks (..), OnlyCheckIsWHNF (..),
                     allNoUnexpectedThunks, noUnexpectedThunksInKeysAndValues)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.MockChain.Chain (Chain (..))

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance Condense (HeaderHash block) => Condense (Point block) where
    condense GenesisPoint     = "Origin"
    condense (BlockPoint s h) = "(Point " <> condense s <> ", " <> condense h <> ")"

instance Condense block => Condense (Chain block) where
    condense Genesis   = "Genesis"
    condense (cs :> b) = condense cs <> " :> " <> condense b

instance (Condense block, HasHeader block, Condense (HeaderHash block))
    => Condense (AnchoredFragment block) where
    condense (AF.Empty pt) = "EmptyAnchor " <> condense (AF.anchorToPoint pt)
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

instance Serialise (VerKeyDSIGN MockDSIGN) where
  encode = encodeVerKeyDSIGN
  decode = decodeVerKeyDSIGN

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

-- TODO move to cardano-prelude
deriving anyclass instance NoUnexpectedThunks Void

instance NoUnexpectedThunks a => NoUnexpectedThunks (K a b) where
  showTypeOf _ = showTypeOf (Proxy @a)
  whnfNoUnexpectedThunks ctxt (K a) = whnfNoUnexpectedThunks ("K":ctxt) a
