{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

module Ouroboros.Network.Mux
  ( -- * Basic notions
    ProtocolTemperature (..)
  , SingProtocolTemperature (..)
  , SomeTokProtocolTemperature (..)
  , WithProtocolTemperature (..)
  , withoutProtocolTemperature
  , WithSomeProtocolTemperature (..)
  , withoutSomeProtocolTemperature
  , TemperatureBundle (..)
  , projectBundle
    -- * Mux mini-protocol callback
  , MiniProtocolCb (..)
  , mkMiniProtocolCbFromPeer
  , mkMiniProtocolCbFromPeerPipelined
  , mkMiniProtocolCbFromPeerSt
    -- * Mux mini-protocol callback in MuxMode
  , RunMiniProtocol (..)
  , RunMiniProtocolWithExpandedCtx
  , RunMiniProtocolWithMinimalCtx
    -- * MiniProtocol description
  , MiniProtocol (..)
  , MiniProtocolWithExpandedCtx
  , MiniProtocolWithMinimalCtx
  , MiniProtocolNum (..)
  , MiniProtocolLimits (..)
    -- * MiniProtocol bundle
  , OuroborosBundle
  , OuroborosBundleWithExpandedCtx
  , OuroborosBundleWithMinimalCtx
    -- * Non-P2P API
  , OuroborosApplication (..)
  , OuroborosApplicationWithMinimalCtx
  , mkMiniProtocolInfos
  , fromOuroborosBundle
  , toMiniProtocolInfos
  , contramapInitiatorCtx
    -- * Re-exports
    -- | from "Network.Mux"
  , Mux.HasInitiator
  , Mux.HasResponder
  , Mux.StartOnDemandOrEagerly (..)
  ) where

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer)

import Data.Foldable (fold)
import Data.Kind (Type)
import Data.Void (Void)

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer
import Network.TypedProtocol.Stateful.Codec qualified as Stateful
import Network.TypedProtocol.Stateful.Peer qualified as Stateful

import Network.Mux qualified as Mux
import Network.Mux.Types (MiniProtocolInfo, MiniProtocolLimits,
           MiniProtocolNum (..))

import Ouroboros.Network.Channel
import Ouroboros.Network.Context (ExpandedInitiatorContext,
           MinimalInitiatorContext, ResponderContext)
import Ouroboros.Network.Driver
import Ouroboros.Network.Driver.Stateful qualified as Stateful
import Ouroboros.Network.Util.ShowProxy (ShowProxy)


-- |  There are three kinds of applications: warm, hot and established (ones
-- that run in both warm and hot states).
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

type OuroborosBundle   (mode :: Mux.Mode) initiatorCtx responderCtx bytes m a b =
    TemperatureBundle [MiniProtocol mode initiatorCtx responderCtx bytes m a b]

-- | 'OuroborosBundle' used in P2P.
--
type OuroborosBundleWithExpandedCtx (mode :: Mux.Mode) peerAddr bytes m a b =
     OuroborosBundle mode
                     (ExpandedInitiatorContext peerAddr m)
                     (ResponderContext peerAddr)
                     bytes m a b

type OuroborosBundleWithMinimalCtx (mode :: Mux.Mode) peerAddr bytes m a b =
     OuroborosBundle mode
                     (MinimalInitiatorContext peerAddr)
                     (ResponderContext peerAddr)
                     bytes m a b


-- | Each mini-protocol is represented by its
--
-- * mini-protocol number,
-- * ingress size limit, and
-- * callbacks.
--
data MiniProtocol (mode :: Mux.Mode) initiatorCtx responderCtx bytes m a b =
     MiniProtocol {
       miniProtocolNum    :: !MiniProtocolNum,
       -- ^ mini-protocol number
       miniProtocolStart  :: !Mux.StartOnDemandOrEagerly,
       -- ^ strategy for starting responder side; initiator side is always
       -- started using `StartEagerly`.
       miniProtocolLimits :: !MiniProtocolLimits,
       -- ^ mini-protocol limits
       miniProtocolRun    :: !(RunMiniProtocol mode initiatorCtx responderCtx bytes m a b)
       -- ^ mini-protocol callback(s)
     }

mkMiniProtocolInfo :: MiniProtocol mode initiatorCtx responderCtx bytes m a b -> [MiniProtocolInfo mode]
mkMiniProtocolInfo MiniProtocol {
     miniProtocolNum,
     miniProtocolLimits,
     miniProtocolRun
   }
  =
  [ Mux.MiniProtocolInfo {
      Mux.miniProtocolNum,
      Mux.miniProtocolDir = dir,
      Mux.miniProtocolLimits
    }
  | dir <- case miniProtocolRun of
             InitiatorProtocolOnly{}         -> [ Mux.InitiatorDirectionOnly ]
             ResponderProtocolOnly{}         -> [ Mux.ResponderDirectionOnly ]
             InitiatorAndResponderProtocol{} -> [ Mux.InitiatorDirection
                                                , Mux.ResponderDirection ]
  ]


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
data RunMiniProtocol (mode :: Mux.Mode) initiatorCtx responderCtx bytes m a b where
     InitiatorProtocolOnly
       :: (MiniProtocolCb initiatorCtx bytes m a)
       -> RunMiniProtocol Mux.InitiatorMode initiatorCtx responderCtx bytes m a Void

     ResponderProtocolOnly
       :: (MiniProtocolCb responderCtx bytes m b)
       -> RunMiniProtocol Mux.ResponderMode initiatorCtx responderCtx bytes m Void b

     InitiatorAndResponderProtocol
       :: (MiniProtocolCb initiatorCtx bytes m a)
       -> (MiniProtocolCb responderCtx bytes m b)
       -> RunMiniProtocol Mux.InitiatorResponderMode initiatorCtx responderCtx bytes m a b


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


--
-- MiniProtocol callback
--

-- | A callback executed by each muxed mini-protocol.
--
newtype MiniProtocolCb ctx bytes m a =
    MiniProtocolCb {
      runMiniProtocolCb ::
        ctx -> Channel m bytes -> m (a, Maybe bytes)
    }


-- | Create a 'MuxPeer' from a tracer, codec and 'Peer'.
--
mkMiniProtocolCbFromPeer
  :: forall (pr :: PeerRole) ps (st :: ps) failure bytes ctx m a.
     ( MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , Show failure
     )
  => (ctx -> ( Tracer m (TraceSendRecv ps)
             , Codec ps failure m bytes
             , Peer ps pr NonPipelined st m a
             )
     )
  -> MiniProtocolCb ctx bytes m a
mkMiniProtocolCbFromPeer fn =
    MiniProtocolCb $ \ctx channel ->
      case fn ctx of
        (tracer, codec, peer) ->
          runPeer tracer codec channel peer

-- | Create a 'MuxPeer' from a tracer, codec and 'Stateful.Peer'.
--
mkMiniProtocolCbFromPeerSt
  :: forall (pr :: PeerRole) ps (f :: ps -> Type) (st :: ps) failure bytes ctx m a.
     ( MonadAsync m
     , MonadMask  m
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , Show failure
     )
  => (ctx -> ( Tracer m (Stateful.TraceSendRecv ps f)
             , Stateful.Codec ps failure f m bytes
             , f st
             , Stateful.Peer ps pr st f m a
             )
     )
  -> MiniProtocolCb ctx bytes m a
mkMiniProtocolCbFromPeerSt fn =
    MiniProtocolCb $ \ctx channel ->
      case fn ctx of
        (tracer, codec, f, peer) ->
          Stateful.runPeer tracer codec channel f peer


-- | Create a 'MuxPeer' from a tracer, codec and 'PeerPipelined'.
--
mkMiniProtocolCbFromPeerPipelined
  :: forall (pr :: PeerRole) ps (st :: ps) failure ctx bytes m a.
     ( MonadAsync m
     , MonadThrow m
     , ShowProxy ps
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , Show failure
     )
  => (ctx -> ( Tracer m (TraceSendRecv ps)
             , Codec ps failure m bytes
             , PeerPipelined ps pr st m a
             )
     )
  -> MiniProtocolCb ctx bytes m a
mkMiniProtocolCbFromPeerPipelined fn =
    MiniProtocolCb $ \ctx channel ->
      case fn ctx of
        (tracer, codec, peer) ->
          runPipelinedPeer tracer codec channel peer


contramapMiniProtocolCbCtx :: (ctx -> ctx')
                           -> MiniProtocolCb ctx' bytes m a
                           -> MiniProtocolCb ctx  bytes m a
contramapMiniProtocolCbCtx f (MiniProtocolCb cb) = MiniProtocolCb (cb . f)


-- |  Like 'MuxApplication' but using a 'MuxPeer' rather than a raw
-- @Channel -> m a@ action.
--
-- Note: Only used in some non-P2P contexts.
newtype OuroborosApplication  (mode :: Mux.Mode) initiatorCtx responderCtx bytes m a b =
  OuroborosApplication {
    getOuroborosApplication
      :: [MiniProtocol mode initiatorCtx responderCtx bytes m a b]
  }

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


toMiniProtocolInfos :: OuroborosApplication mode initiatorCtx responderCtx bytes m a b
                    -> [MiniProtocolInfo mode]
toMiniProtocolInfos =
    foldMap mkMiniProtocolInfo . getOuroborosApplication


contramapInitiatorCtx :: (initiatorCtx' -> initiatorCtx)
                      -> OuroborosApplication mode initiatorCtx  responderCtx bytes m a b
                      -> OuroborosApplication mode initiatorCtx' responderCtx bytes m a b
contramapInitiatorCtx f (OuroborosApplication ptcls) = OuroborosApplication
  [ ptcl { miniProtocolRun =
             case miniProtocolRun ptcl of
               InitiatorProtocolOnly initiator ->
                 InitiatorProtocolOnly (contramapMiniProtocolCbCtx f initiator)
               ResponderProtocolOnly responder ->
                 ResponderProtocolOnly responder
               InitiatorAndResponderProtocol initiator responder ->
                 InitiatorAndResponderProtocol (contramapMiniProtocolCbCtx f initiator) responder
         }
  | ptcl <- ptcls
  ]


-- | Make 'MiniProtocolBundle', which is used to create a mux interface with
-- 'newMux'.
--
mkMiniProtocolInfos :: OuroborosBundle mode initiatorCtx responderCtx bytes m a b
                    -> [MiniProtocolInfo mode]
mkMiniProtocolInfos = foldMap (foldMap mkMiniProtocolInfo)
