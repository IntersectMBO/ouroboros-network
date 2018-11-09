-- {-# LANGUAGE DataKinds     #-}
-- {-# LANGUAGE GADTs         #-}
-- {-# LANGUAGE PolyKinds     #-}
-- {-# LANGUAGE RankNTypes    #-}
-- {-# LANGUAGE TypeOperators #-}

-- | Helper functions for defining tests that abstract over the
-- choice of ouroboros protocol
module Test.Ouroboros () where

{-
    (:->)
  , simpleProp
  ) where

import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.DepFn
import           Ouroboros.Consensus.Util.Singletons (Dict (..))

-- TODO: We may wish to make this a type family so that we can write stuff like
-- @a :-> b :-> c@ to mean @DepFn '[a, b] c@. For now this will do though.
type (:->) (a :: (k -> *)) b = DepFn '[a] b

simpleProp :: (forall p. KnownOuroborosProtocol p => Sing p -> a p -> b) -> (a :-> b)
simpleProp k = DepFn $ \protocol (a :* Nil) ->
                 case dictKnownOuroborosProtocol protocol of
                      Dict -> k protocol a
-}
