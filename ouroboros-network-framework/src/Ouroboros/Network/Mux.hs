{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Ouroboros.Network.Mux
  ( MuxMode (..)
  , OuroborosApplication (..)
  , MuxProtocolBundle
  , ProtocolTemperature (..)
  , TokProtocolTemperature (..)
  , SomeTokProtocolTemperature (..)
  , WithProtocolTemperature (..)
  , withoutProtocolTemperature
  , WithSomeProtocolTemperature (..)
  , withoutSomeProtocolTemperature
  , Bundle (..)
  , projectBundle
  , OuroborosBundle
  , MuxBundle
  , MiniProtocol (..)
  , MiniProtocolNum (..)
  , MiniProtocolLimits (..)
  , RunMiniProtocol (..)
  , MuxPeer (..)
  , runMuxPeer
  , toApplication
  , mkMuxApplicationBundle
  , ControlMessage (..)
  , ControlMessageSTM
  , continueForever
  , timeoutWithControlMessage

    -- * Re-exports
    -- | from "Network.Mux"
  , MuxError(..)
  , MuxErrorType(..)
  , HasInitiator
  , HasResponder
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer)

import           Data.Void (Void)
import qualified Data.ByteString.Lazy as LBS

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import qualified Network.Mux.Compat as Mux
import           Network.Mux
                   ( MuxMode(..), HasInitiator, HasResponder
                   , MiniProtocolNum, MiniProtocolLimits(..)
                   , MuxError(..), MuxErrorType(..) )

import           Ouroboros.Network.Channel
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)


-- | Control signal sent to a mini-protocol.  expected to exit, on 'Continue' it
-- should continue its operation
--
data ControlMessage =
    -- | Continue operation.
      Continue

    -- | Hold on, e.g. do not sent messages until resumed.  This is not used for
    -- any hot protocol.
    --
    | Quiesce

    -- | The client is expected to terminate as soon as possible.
    --
    | Terminate
  deriving (Eq, Show)

-- |  'ControlMessageSTM' should depend on `muxMode` (we only need to shedule
-- stop for intiator side).  This is not done only because this would break
-- tests, but once the old api is removed it should be possible.
--
type ControlMessageSTM m = STM m ControlMessage

continueForever :: Applicative (STM m)
          => proxy m
          -> ControlMessageSTM m
continueForever _ = pure Continue


-- | First to finish synchronisation between 'Terminate' state of
-- 'ControlMessage' and an stm action.
--
-- This should return @STM m (Maybe a)@ but 'STM' is a non-injective type
-- family, and we would need to pass @Proxy m@ to fix an ambiuous type (or use
-- 'AllowAmbiguousTypes' extension).
--
timeoutWithControlMessage :: MonadSTM m
                          => ControlMessageSTM m
                          -> STM m a
                          -> m (Maybe a)
timeoutWithControlMessage controlMessageSTM stm =
    atomically $
      do
        cntrlMsg <- controlMessageSTM
        case cntrlMsg of
          Terminate -> return Nothing
          Continue  -> retry
          Quiesce   -> retry
      `orElse` (Just <$> stm)


-- |  Like 'MuxApplication' but using a 'MuxPeer' rather than a raw
-- @Channel -> m a@ action.
--
newtype OuroborosApplication (mode :: MuxMode) addr bytes m a b =
        OuroborosApplication
          (ConnectionId addr -> ControlMessageSTM m -> [MiniProtocol mode bytes m a b])


-- |  There are three kinds of applications: warm, hot and established (ones
-- that run in for both warm and hot peers).
--
data ProtocolTemperature = Established | Warm | Hot
  deriving (Eq, Ord, Show)


-- | Singletons for 'AppKind'
--
data TokProtocolTemperature (pt :: ProtocolTemperature) where
    TokHot         :: TokProtocolTemperature Hot
    TokWarm        :: TokProtocolTemperature Warm
    TokEstablished :: TokProtocolTemperature Established


data SomeTokProtocolTemperature where
    SomeTokProtocolTemperature :: TokProtocolTemperature pt
                               -> SomeTokProtocolTemperature


-- | We keep hot and warm application (or their context) distinct.  It's only
-- needed for a handly 'projectBundle' map.
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

instance Semigroup a => Semigroup (WithProtocolTemperature pt a) where
    WithHot a <> WithHot b = WithHot (a <> b)
    WithWarm a <> WithWarm b = WithWarm (a <> b)
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
data Bundle a =
      Bundle {
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

instance Semigroup a => Semigroup (Bundle a) where
    Bundle hot warm established <> Bundle hot' warm' established' =
      Bundle (hot <> hot')
             (warm <> warm')
             (established <> established')

instance Monoid a => Monoid (Bundle a) where
    mempty = Bundle mempty mempty mempty

projectBundle :: TokProtocolTemperature pt -> Bundle a -> a
projectBundle TokHot         = withoutProtocolTemperature . withHot
projectBundle TokWarm        = withoutProtocolTemperature . withWarm
projectBundle TokEstablished = withoutProtocolTemperature . withEstablished


instance Applicative Bundle where
    pure a = Bundle (WithHot a) (WithWarm a) (WithEstablished a)
    Bundle hotFn
           warmFn
           establishedFn
      <*> Bundle hot
                 warm
                 established =
          Bundle (hotFn <*> hot)
                 (warmFn <*> warm)
                 (establishedFn <*> established)

--
-- Useful type synonyms
--

type MuxProtocolBundle (mode :: MuxMode) addr bytes m a b
       = ConnectionId addr
      -> ControlMessageSTM m
      -> [MiniProtocol mode bytes m a b]

type OuroborosBundle (mode :: MuxMode) addr bytes m a b =
    Bundle (MuxProtocolBundle mode addr bytes m a b)

data MiniProtocol (mode :: MuxMode) bytes m a b =
     MiniProtocol {
       miniProtocolNum    :: !MiniProtocolNum,
       miniProtocolLimits :: !MiniProtocolLimits,
       miniProtocolRun    :: !(RunMiniProtocol mode bytes m a b)
     }

type MuxBundle (mode :: MuxMode) bytes m a b =
    Bundle [MiniProtocol mode bytes m a b]


data RunMiniProtocol (mode :: MuxMode) bytes m a b where
     InitiatorProtocolOnly
       :: MuxPeer bytes m a
       -> RunMiniProtocol InitiatorMode bytes m a Void

     ResponderProtocolOnly
       :: MuxPeer bytes m b
       -> RunMiniProtocol ResponderMode bytes m Void b

     InitiatorAndResponderProtocol
       :: MuxPeer bytes m a
       -> MuxPeer bytes m b
       -> RunMiniProtocol InitiatorResponderMode bytes m a b

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

toApplication :: (MonadCatch m, MonadAsync m)
              => ConnectionId addr
              -> ControlMessageSTM m
              -> OuroborosApplication mode addr LBS.ByteString m a b
              -> Mux.MuxApplication mode m a b
toApplication connectionId controlMessageSTM (OuroborosApplication ptcls) =
  Mux.MuxApplication
    [ Mux.MuxMiniProtocol {
        Mux.miniProtocolNum    = miniProtocolNum ptcl,
        Mux.miniProtocolLimits = miniProtocolLimits ptcl,
        Mux.miniProtocolRun    = toMuxRunMiniProtocol (miniProtocolRun ptcl)
      }
    | ptcl <- ptcls connectionId controlMessageSTM ]


mkMuxApplicationBundle
    :: forall mode addr bytes m a b.
       ConnectionId addr
    -> Bundle (ControlMessageSTM m)
    -> OuroborosBundle mode addr bytes m a b
    -> MuxBundle       mode      bytes m a b
mkMuxApplicationBundle connectionId controlMessageBundle appBundle =
    mkApplication <$> controlMessageBundle <*> appBundle
  where
    mkApplication :: (ControlMessageSTM m)
                  -> (MuxProtocolBundle mode addr bytes m a b)
                  -> [MiniProtocol mode bytes m a b]
    mkApplication controlMessageSTM app = app connectionId controlMessageSTM


toMuxRunMiniProtocol :: forall mode m a b.
                        (MonadCatch m, MonadAsync m)
                     => RunMiniProtocol mode LBS.ByteString m a b
                     -> Mux.RunMiniProtocol mode m a b
toMuxRunMiniProtocol (InitiatorProtocolOnly i) =
  Mux.InitiatorProtocolOnly (runMuxPeer i . fromChannel)
toMuxRunMiniProtocol (ResponderProtocolOnly r) =
  Mux.ResponderProtocolOnly (runMuxPeer r . fromChannel)
toMuxRunMiniProtocol (InitiatorAndResponderProtocol i r) =
  Mux.InitiatorAndResponderProtocol (runMuxPeer i . fromChannel)
                                    (runMuxPeer r . fromChannel)

-- |
-- Run a @'MuxPeer'@ using either @'runPeer'@ or @'runPipelinedPeer'@.
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
