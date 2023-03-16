{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Ouroboros.Network.Mux
  ( MuxMode (..)
  , ProtocolTemperature (..)
  , SingProtocolTemperature (..)
  , SomeTokProtocolTemperature (..)
  , WithProtocolTemperature (..)
  , withoutProtocolTemperature
  , WithSomeProtocolTemperature (..)
  , withoutSomeProtocolTemperature
  , TemperatureBundle (..)
  , projectBundle
  , OuroborosBundle
  , OuroborosBundleWithExpandedCtx
  , MiniProtocol (..)
  , MiniProtocolWithExpandedCtx
  , MiniProtocolWithMinimalCtx
  , MiniProtocolNum (..)
  , MiniProtocolLimits (..)
  , RunMiniProtocol (..)
  , RunMiniProtocolWithExpandedCtx
  , RunMiniProtocolWithMinimalCtx
  , MuxPeer (..)
  , runMuxPeer
  , toApplication
  , mkMiniProtocolBundle
    -- * Non-P2P API
  , OuroborosApplication (..)
  , OuroborosApplicationWithMinimalCtx
  , fromOuroborosBundle
  , contramapInitiatorCtx
    -- * Re-exports
    -- | from "Network.Mux"
  , MuxError (..)
  , MuxErrorType (..)
  , HasInitiator
  , HasResponder
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer)

import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (fold)
import           Data.Void (Void)

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import           Network.Mux (HasInitiator, HasResponder,
                     MiniProtocolBundle (..), MiniProtocolInfo,
                     MiniProtocolLimits (..), MiniProtocolNum, MuxError (..),
                     MuxErrorType (..), MuxMode (..))
import qualified Network.Mux.Compat as Mux.Compat
import qualified Network.Mux.Types as Mux

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Context (ExpandedInitiatorContext,
                     MinimalInitiatorContext, ResponderContext)
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)




-- |  There are three kinds of applications: warm, hot and established (ones
-- that run in both warm and hot peers).
--
data ProtocolTemperature = Established | Warm | Hot
  deriving (Eq, Ord, Show)

-- | Singletons for 'ProtocolTemperature'.
--
data SingProtocolTemperature (pt :: ProtocolTemperature) where
    SingHot         :: SingProtocolTemperature Hot
    SingWarm        :: SingProtocolTemperature Warm
    SingEstablished :: SingProtocolTemperature Established

data SomeTokProtocolTemperature where
    SomeTokProtocolTemperature :: SingProtocolTemperature pt
                               -> SomeTokProtocolTemperature


-- | We keep hot, warm and established application (or their context) distinct.
-- It's only needed for a handy 'projectBundle' map.
--
data WithProtocolTemperature (pt :: ProtocolTemperature) a where
    WithHot         :: !a -> WithProtocolTemperature Hot  a
    WithWarm        :: !a -> WithProtocolTemperature Warm a
    WithEstablished :: !a -> WithProtocolTemperature Established a

deriving instance Eq a => Eq (WithProtocolTemperature pt a)
deriving instance Show a => Show (WithProtocolTemperature pt a)
deriving instance Functor     (WithProtocolTemperature pt)
deriving instance Foldable    (WithProtocolTemperature pt)
deriving instance Traversable (WithProtocolTemperature pt)

instance Applicative (WithProtocolTemperature Hot) where
    pure = WithHot
    (<*>) (WithHot f) = fmap f
instance Applicative (WithProtocolTemperature Warm) where
    pure = WithWarm
    (<*>) (WithWarm f) = fmap f
instance Applicative (WithProtocolTemperature Established) where
    pure = WithEstablished
    (<*>) (WithEstablished f) = fmap f

instance Semigroup a => Semigroup (WithProtocolTemperature Hot a) where
    WithHot a <> WithHot b                 = WithHot (a <> b)
instance Semigroup a => Semigroup (WithProtocolTemperature Warm a) where
    WithWarm a <> WithWarm b               = WithWarm (a <> b)
instance Semigroup a => Semigroup (WithProtocolTemperature Established a) where
    WithEstablished a <> WithEstablished b = WithEstablished (a <> b)

instance Monoid a => Monoid (WithProtocolTemperature Hot a) where
    mempty = WithHot mempty

instance Monoid a => Monoid (WithProtocolTemperature Warm a) where
    mempty = WithWarm mempty

instance Monoid a => Monoid (WithProtocolTemperature Established a) where
    mempty = WithEstablished mempty


withoutProtocolTemperature :: WithProtocolTemperature pt a -> a
withoutProtocolTemperature (WithHot a)         = a
withoutProtocolTemperature (WithWarm a)        = a
withoutProtocolTemperature (WithEstablished a) = a


data WithSomeProtocolTemperature a where
    WithSomeProtocolTemperature :: WithProtocolTemperature pt a -> WithSomeProtocolTemperature a

deriving instance Show a => Show (WithSomeProtocolTemperature a)
deriving instance Functor WithSomeProtocolTemperature

withoutSomeProtocolTemperature :: WithSomeProtocolTemperature a -> a
withoutSomeProtocolTemperature (WithSomeProtocolTemperature a) = withoutProtocolTemperature a

-- | A bundle of 'HotApp', 'WarmApp' and 'EstablishedApp'.
--

data TemperatureBundle a =
      TemperatureBundle {
          -- | hot mini-protocols
          --
          withHot
            :: !(WithProtocolTemperature Hot a),

          -- | warm mini-protocols
          --
          withWarm
            :: !(WithProtocolTemperature Warm a),

          -- | established mini-protocols
          --
          withEstablished
            :: !(WithProtocolTemperature Established a)
        }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (TemperatureBundle a) where
    TemperatureBundle hot warm established <> TemperatureBundle hot' warm' established' =
      TemperatureBundle (hot <> hot')
                        (warm <> warm')
                        (established <> established')

instance Monoid a => Monoid (TemperatureBundle a) where
    mempty = TemperatureBundle mempty mempty mempty

projectBundle :: SingProtocolTemperature pt -> TemperatureBundle a -> a
projectBundle SingHot         = withoutProtocolTemperature . withHot
projectBundle SingWarm        = withoutProtocolTemperature . withWarm
projectBundle SingEstablished = withoutProtocolTemperature . withEstablished


instance Applicative TemperatureBundle where
    pure a = TemperatureBundle (WithHot a) (WithWarm a) (WithEstablished a)
    TemperatureBundle hotFn
                      warmFn
                      establishedFn
      <*> TemperatureBundle hot
                            warm
                            established =
          TemperatureBundle (hotFn <*> hot)
                            (warmFn <*> warm)
                            (establishedFn <*> established)

--
-- Useful type synonyms
--

type OuroborosBundle   (mode :: MuxMode) initiatorCtx responderCtx bytes m a b =
    TemperatureBundle [MiniProtocol mode initiatorCtx responderCtx bytes m a b]

-- | 'OuroborosBundle' used in P2P.
--
type OuroborosBundleWithExpandedCtx (mode :: MuxMode) peerAddr bytes m a b =
     OuroborosBundle mode
                     (ExpandedInitiatorContext peerAddr m)
                     (ResponderContext peerAddr)
                     bytes m a b


-- | Each mini-protocol is represented by its
--
-- * mini-protocol number,
-- * ingress size limit, and
-- * callbacks.
--
data MiniProtocol (mode :: MuxMode) initiatorCtx responderCtx bytes m a b =
     MiniProtocol {
       miniProtocolNum    :: !MiniProtocolNum,
       miniProtocolLimits :: !MiniProtocolLimits,
       miniProtocolRun    :: !(RunMiniProtocol mode initiatorCtx responderCtx bytes m a b)
     }


-- | 'MiniProtocol' type used in P2P.
--
type MiniProtocolWithExpandedCtx mode peerAddr bytes m a b =
     MiniProtocol mode (ExpandedInitiatorContext peerAddr m)
                       (ResponderContext peerAddr)
                       bytes m a b

-- | 'MiniProtocol' type used in non-P2P.
--
type MiniProtocolWithMinimalCtx mode peerAddr bytes m a b =
     MiniProtocol mode (MinimalInitiatorContext peerAddr)
                       (ResponderContext peerAddr)
                       bytes m a b


-- | 'RunMiniProtocol'.  It also capture context (the `IsBigLedgerPeer`) which
-- is passed to the mini-protocol when a mini-protocol is started.
--
data RunMiniProtocol (mode :: MuxMode) initiatorCtx responderCtx bytes m a b where
     InitiatorProtocolOnly
       :: (initiatorCtx -> MuxPeer bytes m a)
       -> RunMiniProtocol InitiatorMode initiatorCtx responderCtx bytes m a Void

     ResponderProtocolOnly
       :: (responderCtx -> MuxPeer bytes m b)
       -> RunMiniProtocol ResponderMode initiatorCtx responderCtx bytes m Void b

     InitiatorAndResponderProtocol
       :: (initiatorCtx -> MuxPeer bytes m a)
       -> (responderCtx -> MuxPeer bytes m b)
       -> RunMiniProtocol InitiatorResponderMode initiatorCtx responderCtx bytes m a b


-- | 'RunMiniProtocol' with 'ExpandedInitiatorContext' and 'ResponderContext'.
--
-- Used to run P2P node-to-node applications.
--
type RunMiniProtocolWithExpandedCtx mode peerAddr bytes m a b =
     RunMiniProtocol mode
                     (ExpandedInitiatorContext peerAddr m)
                     (ResponderContext peerAddr)
                     bytes m a b


-- | 'RunMiniProtocol' with 'MinimalInitiatorContext' and 'ResponderContext'.
--
-- Use to run node-to-client application as well as in some non p2p contexts.
--
type RunMiniProtocolWithMinimalCtx mode peerAddr bytes m a b =
     RunMiniProtocol mode
                     (MinimalInitiatorContext peerAddr)
                     (ResponderContext peerAddr)
                     bytes m a b

-- TODO: do we need this type? Don't we only use 'MuxPeerRaw'.
--
data MuxPeer bytes m a where
    MuxPeer :: forall (pr :: PeerRole) ps (st :: ps) failure bytes m a.
               ( Show failure
               , forall (st' :: ps). Show (ClientHasAgency st')
               , forall (st' :: ps). Show (ServerHasAgency st')
               , ShowProxy ps
               )
            => Tracer m (TraceSendRecv ps)
            -> Codec ps failure m bytes
            -> Peer ps pr st m a
            -> MuxPeer bytes m a

    MuxPeerPipelined
             :: forall (pr :: PeerRole) ps (st :: ps) failure bytes m a.
               ( Show failure
               , forall (st' :: ps). Show (ClientHasAgency st')
               , forall (st' :: ps). Show (ServerHasAgency st')
               , ShowProxy ps
               )
            => Tracer m (TraceSendRecv ps)
            -> Codec ps failure m bytes
            -> PeerPipelined ps pr st m a
            -> MuxPeer bytes m a

    MuxPeerRaw
           :: (Channel m bytes -> m (a, Maybe bytes))
           -> MuxPeer bytes m a


-- |  Like 'MuxApplication' but using a 'MuxPeer' rather than a raw
-- @Channel -> m a@ action.
--
-- Note: Only used in some non-P2P contexts.
newtype OuroborosApplication  (mode :: MuxMode) initiatorCtx responderCtx bytes m a b =
        OuroborosApplication [MiniProtocol mode initiatorCtx responderCtx bytes m a b]

-- | 'OuroborosApplication' used in NonP2P mode.
--
type OuroborosApplicationWithMinimalCtx mode peerAddr bytes m a b =
     OuroborosApplication mode
                          (MinimalInitiatorContext peerAddr)
                          (ResponderContext peerAddr)
                          bytes m a b

fromOuroborosBundle :: OuroborosBundle      mode initiatorCtx responderCtx bytes m a b
                    -> OuroborosApplication mode initiatorCtx responderCtx bytes m a b
fromOuroborosBundle = OuroborosApplication . fold


contramapInitiatorCtx :: (initiatorCtx' -> initiatorCtx)
                      -> OuroborosApplication mode initiatorCtx  responderCtx bytes m a b
                      -> OuroborosApplication mode initiatorCtx' responderCtx bytes m a b
contramapInitiatorCtx f (OuroborosApplication ptcls) = OuroborosApplication
  [ ptcl { miniProtocolRun =
             case miniProtocolRun ptcl of
               InitiatorProtocolOnly initiator ->
                 InitiatorProtocolOnly (initiator . f)
               ResponderProtocolOnly responder ->
                 ResponderProtocolOnly responder
               InitiatorAndResponderProtocol initiator responder ->
                 InitiatorAndResponderProtocol (initiator . f) responder
         }
  | ptcl <- ptcls
  ]


-- | Create non p2p mux application.
--
-- Note that callbacks will always receive `IsNotBigLedgerPeer`.
toApplication :: forall mode initiatorCtx responderCtx m a b.
                 (MonadCatch m, MonadAsync m)
              => initiatorCtx
              -> responderCtx
              -> OuroborosApplication mode initiatorCtx responderCtx LBS.ByteString m a b
              -> Mux.Compat.MuxApplication mode m a b
toApplication initiatorContext responderContext (OuroborosApplication ptcls) =
    Mux.Compat.MuxApplication
      [ Mux.Compat.MuxMiniProtocol {
          Mux.Compat.miniProtocolNum    = miniProtocolNum ptcl,
          Mux.Compat.miniProtocolLimits = miniProtocolLimits ptcl,
          Mux.Compat.miniProtocolRun    = toMuxRunMiniProtocol (miniProtocolRun ptcl)
        }
      | ptcl <- ptcls ]
  where
    toMuxRunMiniProtocol :: RunMiniProtocol mode initiatorCtx responderCtx LBS.ByteString m a b
                         -> Mux.Compat.RunMiniProtocol mode m a b
    toMuxRunMiniProtocol (InitiatorProtocolOnly i) =
      Mux.Compat.InitiatorProtocolOnly
        (runMuxPeer (i initiatorContext) . fromChannel)
    toMuxRunMiniProtocol (ResponderProtocolOnly r) =
      Mux.Compat.ResponderProtocolOnly
        (runMuxPeer (r responderContext) . fromChannel)
    toMuxRunMiniProtocol (InitiatorAndResponderProtocol i r) =
      Mux.Compat.InitiatorAndResponderProtocol
        (runMuxPeer (i initiatorContext) . fromChannel)
        (runMuxPeer (r responderContext)  . fromChannel)


-- | Make 'MiniProtocolBundle', which is used to create a mux interface with
-- 'newMux'.
--
mkMiniProtocolBundle :: OuroborosBundle mode initiatorCtx responderCtx bytes m a b
                     -> MiniProtocolBundle mode
mkMiniProtocolBundle = MiniProtocolBundle . foldMap fn
  where
    fn :: [MiniProtocol mode initiatorCtx responderCtx bytes m a b] -> [MiniProtocolInfo mode]
    fn ptcls = [ Mux.MiniProtocolInfo
                   { Mux.miniProtocolNum
                   , Mux.miniProtocolDir = dir
                   , Mux.miniProtocolLimits
                   }
               | MiniProtocol { miniProtocolNum
                              , miniProtocolLimits
                              , miniProtocolRun
                              }
                   <- ptcls
               , dir <- case miniProtocolRun of
                   InitiatorProtocolOnly{}         -> [ Mux.InitiatorDirectionOnly ]
                   ResponderProtocolOnly{}         -> [ Mux.ResponderDirectionOnly ]
                   InitiatorAndResponderProtocol{} -> [ Mux.InitiatorDirection
                                                      , Mux.ResponderDirection ]
               ]

-- | Run a @'MuxPeer'@ using either @'runPeer'@ or @'runPipelinedPeer'@.
--
runMuxPeer
  :: ( MonadCatch m
     , MonadAsync m
     )
  => MuxPeer bytes m a
  -> Channel m bytes
  -> m (a, Maybe bytes)
runMuxPeer (MuxPeer tracer codec peer) channel =
    runPeer tracer codec channel peer

runMuxPeer (MuxPeerPipelined tracer codec peer) channel =
    runPipelinedPeer tracer codec channel peer

runMuxPeer (MuxPeerRaw action) channel =
    action channel
