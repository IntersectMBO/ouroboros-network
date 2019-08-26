{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Process.Core
    ( Prob
    , Time
    , ProcId
    , ProcessF (..)
    , Process
    , coin
    , delay
    , getProcId
    , fork
    , send
    , receive
    , logMsg
    , failProcess
    , coinM
    ) where

import Control.Monad (void)
import Control.Monad.Free

import PreDQAlgebra.Class

type Prob = Double
type Time = Double
type ProcId = Int

data ProcessF a where
    Coin      :: Prob -> (Bool -> a) -> ProcessF a
    Delay     :: Time -> a -> ProcessF a
    GetProcId :: (ProcId -> a) -> ProcessF a
    Fork      :: Process () -> (ProcId -> a) -> ProcessF a
    Send      :: ProcId -> String -> a -> ProcessF a
    Receive   :: (String -> a) -> ProcessF a
    LogMsg    :: String -> a -> ProcessF a
    Fail      :: ProcessF a

deriving instance Functor ProcessF

type Process = Free ProcessF

coin :: Prob -> Process Bool
coin p = liftF $ Coin p id

delay :: Time -> Process ()
delay t = liftF $ Delay t ()

getProcId :: Process ProcId
getProcId = liftF $ GetProcId id

fork :: Process () -> Process ProcId
fork x = liftF $ Fork x id

send :: ProcId -> String -> Process ()
send pid msg = liftF $ Send pid msg ()

receive :: Process String
receive = liftF $ Receive id

logMsg :: String -> Process ()
logMsg msg = liftF $ LogMsg msg ()

failProcess :: Process a
failProcess = liftF Fail

coinM :: Prob -> Process a -> Process a -> Process a
coinM p x y = do
    b <- coin p
    if b then x else y

instance Semigroup (Process ()) where
    (<>) = (>>)

instance Monoid (Process ()) where
    mempty = return ()

instance PreDQAlgebra (Process ()) where

    firstToFinish x y = do
        fork2 x y
        void receive

    lastToFinish x y = do
        fork2 x y
        void receive
        void receive

    bottom = failProcess

fork2 :: Process () -> Process () -> Process ()
fork2 x y = do
    pid <- getProcId
    void $ fork $ x >> send pid ""
    void $ fork $ y >> send pid ""
