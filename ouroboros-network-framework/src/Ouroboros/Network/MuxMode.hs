{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

-- 'withInitiatorMode' and 'withResponderMode' are using redundant constraints.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Singletons to work with 'MuxMode' kind.
--
module Ouroboros.Network.MuxMode
  ( SingMuxMode (..)
  , SingHasInitiator (..)
  , hasInitiatorMode
  , WithMuxMode (..)
  , WithMuxTuple
  , withInitiatorMode
  , withResponderMode
  , InResponderMode (..)
  ) where

import           Network.Mux.Types


-- | Singletons for matching the 'MuxMode' at term level.
--
data SingMuxMode (mode :: MuxMode) where
    SingInitiatorMode          :: SingMuxMode InitiatorMode
    SingResponderMode          :: SingMuxMode ResponderMode
    SingInitiatorResponderMode :: SingMuxMode InitiatorResponderMode


-- | Singleton for to match the @'HasInitiator' mode ~ True@ constraint.
--
data SingHasInitiator (mode :: MuxMode) where
    SingHasInitiator :: HasInitiator mode ~ True
                  => SingHasInitiator mode

    SingNoInitiator  :: HasInitiator mode ~ False
                  => SingHasInitiator mode

hasInitiatorMode :: SingMuxMode mode
                 -> SingHasInitiator mode
hasInitiatorMode SingInitiatorMode          = SingHasInitiator
hasInitiatorMode SingInitiatorResponderMode = SingHasInitiator
hasInitiatorMode SingResponderMode          = SingNoInitiator

data WithMuxMode (mode :: MuxMode) a b where
    WithInitiatorMode          :: a -> WithMuxMode InitiatorMode a b
    WithResponderMode          :: b -> WithMuxMode ResponderMode a b
    WithInitiatorResponderMode :: a -> b -> WithMuxMode InitiatorResponderMode a b


type WithMuxTuple mode a = WithMuxMode mode a a

withInitiatorMode :: HasInitiator mode ~ True
                  => WithMuxMode mode a b
                  -> a
withInitiatorMode (WithInitiatorMode          a  ) = a
withInitiatorMode (WithInitiatorResponderMode a _) = a

withResponderMode :: HasResponder mode ~ True
                  => WithMuxMode mode a b
                  -> b
withResponderMode (WithResponderMode            b) = b
withResponderMode (WithInitiatorResponderMode _ b) = b


data InResponderMode (mode :: MuxMode) a where
    InResponderMode    :: HasResponder mode ~ True
                       => a
                       -> InResponderMode mode a

    NotInResponderMode :: InResponderMode mode a
