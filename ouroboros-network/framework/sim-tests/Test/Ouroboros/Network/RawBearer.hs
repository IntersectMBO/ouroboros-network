{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.RawBearer where


import Control.Monad.Class.MonadSay
import Control.Monad.IOSim hiding (liftST)
import Control.Tracer (Tracer (..), nullTracer)

import Ouroboros.Network.Snocket
import Simulation.Network.Snocket as SimSnocket

import Test.Ouroboros.Network.Data.AbsBearerInfo
import Test.Ouroboros.Network.RawBearer.Utils
import Test.Simulation.Network.Snocket (toBearerInfo)
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Ouroboros.Network.RawBearer"
  [ testProperty "raw bearer send receive simulated socket" prop_raw_bearer_send_and_receive_iosim
  ]

iosimTracer :: forall s. Tracer (IOSim s) String
iosimTracer = Tracer say

ioTracer :: Tracer IO String
ioTracer = nullTracer

onlyIf :: Bool -> a -> Maybe a
onlyIf False = const Nothing
onlyIf True  = Just

prop_raw_bearer_send_and_receive_iosim :: Int -> Int -> Message -> Property
prop_raw_bearer_send_and_receive_iosim serverInt clientInt msg =
  iosimProperty $
    SimSnocket.withSnocket
      nullTracer
      (toBearerInfo absNoAttenuation)
      mempty $ \snocket _observe -> do
        rawBearerSendAndReceive
          iosimTracer
          snocket
          (makeFDRawBearer nullTracer)
          (TestAddress serverInt)
          (Just $ TestAddress clientInt)
          msg


iosimProperty :: (forall s . IOSim s Property)
              -> Property
iosimProperty sim =
  let tr = runSimTrace sim
   in case traceResult True tr of
     Left e -> counterexample
                (unlines
                  [ "=== Say Events ==="
                  , unlines (selectTraceEventsSay' tr)
                  , "=== Trace Events ==="
                  , unlines (show `map` traceEvents tr)
                  , "=== Error ==="
                  , show e ++ "\n"
                  ])
                False
     Right prop -> prop

